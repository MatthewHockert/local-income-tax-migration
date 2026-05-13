# q3_unified_honestdid.R
# Rambachan-Roth (2023) honest-DiD sensitivity bounds on the
# headline realized-dose Sun-Abraham event study at all three
# estimating units (cousub, school district, census block) and all
# four residence-side outcomes per unit.
#
# Spec mirrors the existing ES scripts exactly (Inc-bind cell only):
#
#   - Cousub level: q3_unified_headline_es.R
#       y_{it} = sum_{k != -1} beta^M_k * 1{rel=k} * realized_dose_muni_i
#              + sum_{c'} beta^SD_{c'} * Delta tau_SD_{it} * 1{cell_SD_i = c'}
#              + sum_{c'} beta^nr_{c'} * Delta tau_nr_{it} * 1{cell_nr_i = c'}
#              + X_{it} phi + alpha_{stack,muni} + gamma_{stack,year}
#       Subset: {NC, Inc-bind}. Cluster: ~muni.
#
#   - SD level: q3_unified_sd_es.R
#       Same structure, replacing muni with SD as the estimating unit.
#       FE: sd_id^stack + year^stack. Cluster: ~sd_id.
#
#   - Block level: q3_unified_block_es_full.R
#       Same structure, block-by-stack and year-by-stack FE,
#       two-way cluster ~muni + sd_id.
#
# For each (unit, outcome) the script:
#   1. Fits the regression on the {NC, Inc-bind} subset
#   2. Extracts beta_hat for the (rel::k:realized_dose_muni) coefficients
#      at k in {-5,-4,-3,-2, 0,1,2,3,4,5} (k = -1 omitted as reference)
#   3. Extracts the cluster-robust vcov for those coefficients
#   4. Calls HonestDiD::createSensitivityResults_relativeMagnitudes() with
#      Mbar in {0, 0.5, 1, 1.5, 2} on the average post-period effect
#      (l_vec = (1/6, 1/6, ..., 1/6) over six post-periods)
#
# Output:
#   data/q3_unified/honestdid_sensitivity.csv  (long format)
#   figures/honestdid_<unit>_<outcome>.png    (sensitivity plot per cell)

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(HonestDiD)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
fig_dir     <- file.path(project_dir, "figures")
dir.create(out_dir, showWarnings = FALSE); dir.create(fig_dir, showWarnings = FALSE)

K_stack    <- 5L
cell_focal <- "Inc-bind"
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
Mbarvec    <- c(0, 0.5, 1, 1.5, 2)

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
sd_ctrls   <- c("share_state_rev_sd","share_local_rev_sd",
                "share_federal_rev_sd","share_property_tax_sd",
                "share_instructional_sd","share_support_sd",
                "share_capital_sd","share_debt_int_sd")

# ---------------------------------------------------------------------------
# Build cousub-level panel via the existing prep (q3_unified_full_table.R)
# ---------------------------------------------------------------------------
cat("Sourcing cousub data prep (q3_unified_full_table.R)...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
cousub_panel <- e$panel
cousub_meta  <- unique(cousub_panel[, .(muni, year,
                                        cell_sd, cell_nr,
                                        dose_sd, dose_nr,
                                        c_muni, cell_muni,
                                        share_police, share_streets, share_debt_svc,
                                        share_eit, share_re, share_intergov)])

# Realized muni dose: cumulative Delta tau between c_muni-1 and c_muni+5
build_realized_dose <- function(p) {
  mu_year <- unique(p[, .(muni, year, resident_eit, c_muni)])
  setorder(mu_year, muni, year)
  rd <- mu_year[!is.na(c_muni), {
    pre <- resident_eit[year == c_muni - 1L]
    yr_max <- min(c_muni + K_stack, max(year, na.rm = TRUE))
    end <- resident_eit[year == yr_max]
    pre_v <- if (length(pre) == 1) pre else NA_real_
    end_v <- if (length(end) == 1) end else NA_real_
    list(realized_dose_muni = end_v - pre_v)
  }, by = muni]
  rd[is.na(realized_dose_muni), realized_dose_muni := 0]
  nc <- unique(p[cell_muni == "NC", muni])
  rd <- rbind(rd, data.table(muni = nc, realized_dose_muni = 0))
  rd[, .(muni = muni[1], realized_dose_muni = realized_dose_muni[1]), by = muni][, -1]
}
rd <- build_realized_dose(cousub_panel)
cousub_panel <- merge(cousub_panel, rd, by = "muni", all.x = TRUE)
cousub_panel[is.na(realized_dose_muni), realized_dose_muni := 0]

# Stack constructor (cousub or SD aggregation); takes p with 'muni' col
build_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC" & !is.na(cell_muni),
                       .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_m <- treated[cohort == cy]
    if (nrow(t_m) == 0) return(NULL)
    d_t <- p[muni %in% t_m$muni & year %in% win]
    d_t[, treat_cell_muni := cell_muni]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell_muni := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }), fill = TRUE)
}

# ---------------------------------------------------------------------------
# Helper: fit the Inc-bind ES, extract beta + vcov on rel::k:dose coefs,
# call HonestDiD::createSensitivityResults_relativeMagnitudes(),
# return long-format data.table with per-Mbar CIs + breakdown Mbar.
# ---------------------------------------------------------------------------
run_honestdid <- function(model, unit_label, outcome_label,
                          unit_n_label) {
  ct <- as.data.table(model$coeftable, keep.rownames = "term")
  rel_terms <- ct[grepl("^rel::", term)]
  rel_terms[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
  setorder(rel_terms, rel)

  # Identify pre and post periods (k = -1 omitted by ref = -1)
  pre_idx  <- which(rel_terms$rel <= -2)
  post_idx <- which(rel_terms$rel >= 0)
  if (length(pre_idx) == 0 || length(post_idx) == 0) {
    cat(sprintf("  [%s, %s] missing pre/post coefs; skipping HonestDiD\n",
                unit_label, outcome_label))
    return(NULL)
  }

  betahat <- rel_terms$Estimate
  vc      <- vcov(model)
  vc_idx  <- match(rel_terms$term, rownames(vc))
  sigma   <- vc[vc_idx, vc_idx]

  numPrePeriods  <- length(pre_idx)
  numPostPeriods <- length(post_idx)
  # Average of post-period coefs (the "ATT-summary" the paper reports)
  l_vec <- rep(1 / numPostPeriods, numPostPeriods)

  # Original (no-restriction) CI for the post-period average
  orig <- tryCatch(constructOriginalCS(betahat = betahat, sigma = sigma,
                                       numPrePeriods = numPrePeriods,
                                       numPostPeriods = numPostPeriods,
                                       l_vec = l_vec),
                    error = function(ee) {
                      cat(sprintf("  [%s, %s] constructOriginalCS error: %s\n",
                                  unit_label, outcome_label, conditionMessage(ee)))
                      NULL })

  if (is.null(orig)) return(NULL)

  sens <- tryCatch(createSensitivityResults_relativeMagnitudes(
                     betahat = betahat, sigma = sigma,
                     numPrePeriods = numPrePeriods,
                     numPostPeriods = numPostPeriods,
                     l_vec = l_vec,
                     Mbarvec = Mbarvec,
                     bound = "deviation from parallel trends"),
                    error = function(ee) {
                      cat(sprintf("  [%s, %s] sensitivity error: %s\n",
                                  unit_label, outcome_label, conditionMessage(ee)))
                      NULL })
  if (is.null(sens)) return(NULL)

  sens_dt <- as.data.table(sens)
  setnames(sens_dt, c("Mbar","lb","ub"), c("Mbar","ci_lo","ci_hi"),
           skip_absent = TRUE)
  sens_dt[, unit    := unit_label]
  sens_dt[, outcome := outcome_label]
  sens_dt[, n_label := unit_n_label]

  # Append the "original" (no restriction) CI as Mbar=NA row for reference
  ref_row <- data.table(Mbar = NA_real_,
                         ci_lo = orig$lb, ci_hi = orig$ub,
                         method = "Original",
                         Delta = "Original",
                         unit = unit_label, outcome = outcome_label,
                         n_label = unit_n_label)
  sens_dt <- rbind(ref_row, sens_dt, fill = TRUE)
  sens_dt[, beta_post := sum(l_vec * betahat[post_idx])]

  cat(sprintf("  [%s, %s] beta_post = %.3f; original CI = [%.3f, %.3f]; sensitivity Mbar grid done\n",
              unit_label, outcome_label,
              sum(l_vec * betahat[post_idx]), orig$lb, orig$ub))
  sens_dt
}

results <- list()

# ===========================================================================
# COUSUB (municipality level), 4 outcomes
# ===========================================================================
cat("\n=== COUSUB ===\n")
stk_c <- build_stack(cousub_panel)
stk_c[, stack_muni := paste0(stack, "_", muni)]
stk_c[, stack_year := paste0(stack, "_", year)]
stk_c[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk_c[, cell_sd := factor(cell_sd, levels = cell_levels)]
stk_c[, cell_nr := factor(cell_nr, levels = cell_levels)]
stk_c[is.na(dose_sd), dose_sd := 0]; stk_c[is.na(dose_nr), dose_nr := 0]

ctrls_c <- c(muni_ctrls)
ctrls_str_c <- paste(ctrls_c, collapse = " + ")

cousub_outcomes <- c("log_live_and_work","log_in_commuters",
                     "log_total_residents","share_live_in")
for (o in cousub_outcomes) {
  dd <- stk_c[treat_cell_muni %in% c("NC", cell_focal)]
  cols <- c("muni","year","stack_muni","stack_year",
            "treat_cell_muni","realized_dose_muni","rel",
            "cell_sd","dose_sd","cell_nr","dose_nr",
            o, ctrls_c)
  cols <- intersect(cols, names(dd))
  dd <- dd[, ..cols]; dd <- dd[complete.cases(dd)]
  if (nrow(dd) == 0) next
  f <- as.formula(paste0(o,
       " ~ i(rel, realized_dose_muni, ref = -1) ",
       "+ i(cell_sd, dose_sd, ref = 'NC') ",
       "+ i(cell_nr, dose_nr, ref = 'NC') + ",
       ctrls_str_c, " | stack_muni + stack_year"))
  m <- feols(f, data = dd, cluster = ~muni)
  r <- run_honestdid(m, unit_label = "cousub",
                     outcome_label = o,
                     unit_n_label = sprintf("N = %s", format(nobs(m), big.mark=",")))
  if (!is.null(r)) results[[length(results)+1L]] <- r
}

# ===========================================================================
# SCHOOL DISTRICT level, 4 outcomes
# ===========================================================================
cat("\n=== SD ===\n")
# Source q3_unified_sd_es.R to get its prep (sd_panel, stk_sd, controls).
# This script in turn sources q3_unified_sd_level.R for the SD panel.
e_sd <- new.env()
sd_es_script <- file.path(project_dir, "q3_unified_sd_es.R")
if (!file.exists(sd_es_script)) {
  cat("WARNING: q3_unified_sd_es.R not found; skipping SD level.\n")
} else {
  cat("Sourcing SD-level ES prep...\n")
  sys.source(sd_es_script, envir = e_sd)
  stk_sd <- e_sd$stk_sd
  ctrls_es_sd <- e_sd$ctrls_es
  if (is.null(stk_sd) || !"rel" %in% names(stk_sd)) {
    cat("WARNING: SD stacked panel not found in env; skipping.\n")
  } else {
    sd_outcomes <- c("log_live_and_work","log_in_commuters",
                     "log_total_residents","share_live_in")
    sd_outcomes <- intersect(sd_outcomes, names(stk_sd))
    ctrls_str_sd <- paste(ctrls_es_sd, collapse = " + ")
    for (o in sd_outcomes) {
      dd <- stk_sd[treat_cell %in% c("NC", cell_focal)]
      cols <- c("sd_id","year","stack_sd","stack_year",
                "treat_cell","realized_dose_sd","rel",
                o, ctrls_es_sd)
      cols <- intersect(cols, names(dd))
      dd <- dd[, ..cols]; dd <- dd[complete.cases(dd)]
      if (nrow(dd) == 0) next
      f <- as.formula(paste0(o,
           " ~ i(rel, realized_dose_sd, ref = -1) + ",
           ctrls_str_sd, " | stack_sd + stack_year"))
      m <- tryCatch(feols(f, data = dd, cluster = ~sd_id),
                    error = function(ee) {
                      cat(sprintf("  [sd, %s] error: %s\n", o, conditionMessage(ee))); NULL })
      if (is.null(m)) next
      r <- run_honestdid(m, unit_label = "sd",
                         outcome_label = o,
                         unit_n_label = sprintf("N = %s", format(nobs(m), big.mark=",")))
      if (!is.null(r)) results[[length(results)+1L]] <- r
    }
  }
}

# ===========================================================================
# BLOCK level, 4 outcomes (the slow one - block panel is ~30M rows)
# ===========================================================================
cat("\n=== BLOCK ===\n")
block_panel_path <- file.path(data_dir, "pa_block_panel.rds")
if (!file.exists(block_panel_path)) {
  cat("WARNING: pa_block_panel.rds not found; skipping block level.\n")
} else {
  b <- as.data.table(readRDS(block_panel_path))
  drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                            "share_eit","share_re","share_intergov"),
                          names(b))
  if (length(drop_cols)) b[, (drop_cols) := NULL]
  b <- merge(b, cousub_meta, by = c("muni","year"), all.x = TRUE)
  b <- merge(b, rd, by = "muni", all.x = TRUE)

  sd_ctrls_path <- file.path(data_dir, "pa_sd_controls.rds")
  if (file.exists(sd_ctrls_path)) {
    sd_ctrl <- as.data.table(readRDS(sd_ctrls_path))
    keep <- intersect(c("sd_id","year", sd_ctrls), names(sd_ctrl))
    sd_ctrl <- sd_ctrl[, ..keep]
    sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                         if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                       by = .(sd_id, year)]
    for (cc in sd_ctrls) {
      set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
    }
    b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)
  }
  b[is.na(realized_dose_muni), realized_dose_muni := 0]

  cat("Building block-level stacked panel (~30M rows)...\n")
  stk_b <- build_stack(b)
  stk_b[, stack_year := paste0(stack, "_", year)]
  stk_b[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
  stk_b[, cell_sd := factor(cell_sd, levels = cell_levels)]
  stk_b[, cell_nr := factor(cell_nr, levels = cell_levels)]
  stk_b[is.na(dose_sd), dose_sd := 0]; stk_b[is.na(dose_nr), dose_nr := 0]

  ctrls_b <- c(muni_ctrls, sd_ctrls)
  ctrls_str_b <- paste(ctrls_b, collapse = " + ")
  block_outcomes <- c("log_home_count","log_work_count",
                      "home_in_muni_share","share_work_outside")
  for (o in block_outcomes) {
    dd <- stk_b[treat_cell_muni %in% c("NC", cell_focal)]
    cols <- c("block_geoid","muni","sd_id","year","stack",
              "treat_cell_muni","realized_dose_muni","rel",
              "cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls_b)
    cols <- intersect(cols, names(dd))
    dd <- dd[, ..cols]; dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0) next
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1) ",
         "+ i(cell_sd, dose_sd, ref = 'NC') ",
         "+ i(cell_nr, dose_nr, ref = 'NC') + ",
         ctrls_str_b, " | block_geoid^stack + year^stack"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni + sd_id),
                  error = function(ee) {
                    cat(sprintf("  [block, %s] error: %s\n", o, conditionMessage(ee))); NULL })
    if (is.null(m)) next
    r <- run_honestdid(m, unit_label = "block",
                       outcome_label = o,
                       unit_n_label = sprintf("N = %s", format(nobs(m), big.mark=",")))
    if (!is.null(r)) results[[length(results)+1L]] <- r
  }
}

# ---------------------------------------------------------------------------
# Save long-format results
# ---------------------------------------------------------------------------
out <- rbindlist(results, fill = TRUE)
fwrite(out, file.path(out_dir, "honestdid_sensitivity.csv"))
cat(sprintf("\nSaved %d sensitivity rows to honestdid_sensitivity.csv\n", nrow(out)))

# Breakdown Mbar per (unit, outcome): largest Mbar at which CI excludes 0
out[, signif := !(ci_lo <= 0 & ci_hi >= 0)]
breakdown <- out[!is.na(Mbar) & signif == TRUE,
                  .(breakdown_Mbar = max(Mbar)),
                  by = .(unit, outcome)]
breakdown <- merge(unique(out[, .(unit, outcome, beta_post, n_label)]),
                   breakdown, by = c("unit","outcome"), all.x = TRUE)
breakdown[is.na(breakdown_Mbar), breakdown_Mbar := 0]
fwrite(breakdown, file.path(out_dir, "honestdid_breakdown.csv"))
cat("\n=== Breakdown Mbar (largest Mbar at which post-effect stays stat-sig) ===\n")
print(breakdown)

# Sensitivity plots per (unit, outcome)
for (rr in seq_len(nrow(unique(out[, .(unit, outcome)])))) {
  u <- unique(out[, .(unit, outcome)])[rr]
  dd <- out[unit == u$unit & outcome == u$outcome]
  if (nrow(dd) == 0) next
  dd[, Mbar_lab := ifelse(is.na(Mbar), "Original", sprintf("%.2f", Mbar))]
  dd[, Mbar_lab := factor(Mbar_lab, levels = c("Original", sprintf("%.2f", Mbarvec)))]
  p <- ggplot(dd, aes(x = Mbar_lab, ymin = ci_lo, ymax = ci_hi)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(width = 0.2, color = "steelblue4", linewidth = 0.8) +
    geom_point(aes(y = (ci_lo + ci_hi) / 2), color = "steelblue4", size = 2) +
    labs(title = sprintf("HonestDiD sensitivity: %s level, %s (Inc-bind)",
                          u$unit, u$outcome),
         subtitle = "95% CI for average post-period effect under relative-magnitudes Mbar restriction",
         x = expression(bar(M)),
         y = expression(beta[post])) +
    theme_bw(base_size = 11)
  ggsave(file.path(fig_dir, sprintf("honestdid_%s_%s.png", u$unit, u$outcome)),
         p, width = 7, height = 4.5, dpi = 300)
}

cat("\nDone.\n")
