# q3_unified_compare.R
# Comparison run: same unified panel + cell logic as q3_unified.R, but
# tests two alternative resident-side treatment specifications:
#
#   MODE A "homestack": one resident-side treatment = Δhome_stack
#                       (worker-bill perspective). Plus Δnonres on the
#                       work side.
#   MODE B "decomp":    THREE treatments separately decomposed by cells:
#                       Δmuni_res, Δsd, Δnonres. Each has its own cell
#                       classification keyed off the relevant first event,
#                       all binding indicators using home_stack vs nonres.
#
# Drops sample = drop (largest N). Estimators = TWFE pooled + stacked
# continuous (skip DCDH for speed; comparison to existing muni-only
# unified covers the DCDH dimension).
#
# Output: data/q3_unified/compare_<mode>_<estimator>.csv with same
# (outcome x cell x side) layout as the master q3_unified.R outputs.

suppressMessages({
  library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
dir.create(out_dir, showWarnings = FALSE)
max_year <- 2019
K_stack  <- 5L

# ---------------------------------------------------------------------------
# 1. Load and merge (same as q3_unified.R)
# ---------------------------------------------------------------------------
wp <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
rp <- as.data.table(readRDS(file.path(data_dir, "pa_residence_panel.rds")))[year <= max_year]
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

setnames(wp, "work_city", "muni", skip_absent = TRUE)
setnames(rp, "home_city", "muni", skip_absent = TRUE)

panel <- merge(
  wp[, .(muni, year, live_and_work, live_outside, total_jobs, share_live_in)],
  rp[, .(muni, year, residents_employed, residents_work_in_own,
         residents_work_outside, share_work_outside)],
  by = c("muni", "year"), all = TRUE
)
rate_keep <- rate[!is.na(cousub_fips) & year <= max_year,
                  .(muni = cousub_fips, year, resident_eit, sd_eit, muni_nonres_eit)]
rate_keep <- rate_keep[, .(resident_eit = mean(resident_eit, na.rm = TRUE),
                           sd_eit       = max(sd_eit, na.rm = TRUE),
                           muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                       by = .(muni, year)]
for (c in c("resident_eit","sd_eit","muni_nonres_eit"))
  rate_keep[!is.finite(get(c)), (c) := NA_real_]
panel <- merge(panel, rate_keep, by = c("muni", "year"), all.x = TRUE)
panel[, home_stack := resident_eit + sd_eit]

panel[, in_commuters    := live_outside]
panel[, out_commuters   := residents_work_outside]
panel[, total_residents := residents_employed]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
panel[, log_live_and_work   := log_pos(live_and_work)]
panel[, log_in_commuters    := log_pos(in_commuters)]
panel[, log_out_commuters   := log_pos(out_commuters)]
panel[, log_total_jobs      := log_pos(total_jobs)]
panel[, log_total_residents := log_pos(total_residents)]
panel[, share_nonres := ifelse(total_jobs > 0, in_commuters / total_jobs, NA_real_)]

# Fiscal controls (same as q3_unified.R, with share_intergov)
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
fc <- afr[!is.na(cousub_fips), {
  total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
  eit_rev <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                               earned_income_tax_revenues, earned_income_tax))
  re_tax  <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                               real_estate_tax_revenues, real_estate_tax))
  fed_ig <- as.numeric(`intergovernmental_revenues-federal_government`)
  sta_ig <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                               `intergovernmental_revenues-state_government`, state_govt))
  loc_ig <- as.numeric(`intergovernmental_revenues-local_government`)
  intergov <- rowSums(cbind(fed_ig, sta_ig, loc_ig), na.rm = TRUE)
  intergov[is.na(fed_ig) & is.na(sta_ig) & is.na(loc_ig)] <- NA_real_
  list(muni = cousub_fips, year = yr,
       log_police    = log_pos(ifelse(!is.na(police_expenditures),
                                       police_expenditures, police)),
       log_streets   = log_pos(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                       `public_works-highways_and_streets_expenditures`,
                                       streets_and_roads)),
       log_debt      = log_pos(as.numeric(total_debt)),
       log_state_aid = log_pos(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                       `intergovernmental_revenues-state_government`,
                                       state_govt)),
       share_eit       = ifelse(total_rev > 0, eit_rev  / total_rev, NA_real_),
       share_re        = ifelse(total_rev > 0, re_tax   / total_rev, NA_real_),
       share_intergov  = ifelse(total_rev > 0, intergov / total_rev, NA_real_))
}]
fc <- fc[!is.na(muni) & !is.na(year)]
fc <- fc[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
         by = .(muni, year)]
panel <- merge(panel, fc, by = c("muni", "year"), all.x = TRUE)

# Dedupe (muni, year)
panel <- panel[, lapply(.SD, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)
}), by = .(muni, year)]
for (col in names(panel)) {
  if (is.numeric(panel[[col]])) {
    set(panel, which(!is.finite(panel[[col]])), col, NA_real_)
  }
}

outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
              "log_total_jobs", "log_total_residents",
              "share_live_in", "share_work_outside", "share_nonres")
ctrls <- c("log_police", "log_streets", "log_debt", "log_state_aid",
           "share_eit", "share_re", "share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

# Use drop sample
panel_drop <- panel

# ---------------------------------------------------------------------------
# 2. Helper: classify events for any rate variable, build cell + dose
# ---------------------------------------------------------------------------
# rate_var: column name to track (e.g., "home_stack", "resident_eit", "sd_eit")
# bind_ref: column name used to test binding (= muni_nonres_eit usually)
# tag: short string to prefix cell/dose vars (e.g., "home", "muni", "sd", "nr")
classify_treatment <- function(p, rate_var, bind_ref, tag) {
  setorder(p, muni, year)
  # Binding indicator: home_stack > nonres (always uses full home_stack)
  p[, home_binds := as.integer(home_stack > muni_nonres_eit)]
  # Lag and event
  lag_col <- paste0("lag_", tag)
  p[, (lag_col) := shift(get(rate_var)), by = muni]
  ev_col <- paste0("ev_", tag)
  p[, (ev_col) := fifelse(is.na(get(lag_col)), NA_character_,
                  fifelse(get(rate_var) > get(lag_col) + 1e-6, "Inc",
                  fifelse(get(rate_var) < get(lag_col) - 1e-6, "Dec", NA_character_)))]
  # First event per muni
  ev <- p[!is.na(get(ev_col)), .SD[1], by = muni,
          .SDcols = c("year", ev_col, lag_col, rate_var, "home_binds")]
  setnames(ev, c("year", ev_col, lag_col, rate_var, "home_binds"),
           c("c_var","ev_dir","baseline_var","first_post","postbind"))
  # Pre-event binding state (lag of home_binds at year c_var)
  pbind <- p[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
  ev <- merge(ev, pbind[, .(muni, year, lag_home_binds)],
              by.x = c("muni","c_var"), by.y = c("muni","year"), all.x = TRUE)
  ev[, prebind := lag_home_binds]
  ev[, lag_home_binds := NULL]
  # Cell label
  ev[, cell := fcase(
    ev_dir == "Inc" & prebind == 1 & postbind == 1, "Inc-bind",
    ev_dir == "Inc" & prebind == 0 & postbind == 1, "Inc-becomes",
    ev_dir == "Inc" & prebind == 0 & postbind == 0, "Inc-nobind",
    ev_dir == "Inc" & prebind == 1 & postbind == 0, "Inc-loses",
    ev_dir == "Dec" & prebind == 1 & postbind == 1, "Dec-bind",
    ev_dir == "Dec" & prebind == 1 & postbind == 0, "Dec-becomes-non",
    ev_dir == "Dec" & prebind == 0 & postbind == 0, "Dec-nobind",
    ev_dir == "Dec" & prebind == 0 & postbind == 1, "Dec-gains",
    default = NA_character_
  )]
  setnames(ev, c("c_var","baseline_var","cell"),
           c(paste0("c_", tag), paste0("baseline_", tag), paste0("cell_", tag)))
  ev <- ev[, .SD, .SDcols = c("muni",
                              paste0("c_", tag),
                              paste0("baseline_", tag),
                              paste0("cell_", tag))]
  p2 <- merge(p, ev, by = "muni", all.x = TRUE)
  cell_v <- paste0("cell_", tag)
  c_v    <- paste0("c_", tag)
  base_v <- paste0("baseline_", tag)
  dose_v <- paste0("dose_", tag)
  p2[is.na(get(cell_v)), (cell_v) := "NC"]
  p2[, (dose_v) := fifelse(!is.na(get(c_v)) & year >= get(c_v),
                           get(rate_var) - get(base_v), 0)]
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind",
                   "Inc-loses", "Dec-gains")
  p2[, (cell_v) := factor(get(cell_v), levels = cell_levels)]
  return(p2)
}

# ---------------------------------------------------------------------------
# 3. Helpers for estimators
# ---------------------------------------------------------------------------
extract_cell_coefs <- function(model, side_label) {
  ct <- as.data.table(model$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  pat <- paste0("cell_", side_label, "::")
  ct <- ct[grepl(pat, term)]
  ct[, cell := sub(paste0("^.*", pat, "([^:]+):.*$"), "\\1", term)]
  ct[, side := side_label]
  ct[, n := nobs(model)]
  ct[, .(side, cell, est, se, p, n)]
}

clean_for <- function(p, outcome, extra_cols) {
  needed <- unique(c(outcome, ctrls, extra_cols, "muni", "year"))
  p2 <- p[, ..needed]
  p2[complete.cases(p2)]
}

# ---------------------------------------------------------------------------
# 4. MODE A: homestack (single resident-side treatment + nonres)
# ---------------------------------------------------------------------------
cat("\n========== MODE A: homestack ==========\n")
pA <- copy(panel_drop)
pA <- classify_treatment(pA, "home_stack", "muni_nonres_eit", "home")
pA <- classify_treatment(pA, "muni_nonres_eit", "home_stack", "nr")
cat("cell_home counts:\n"); print(unique(pA[, .(muni, cell_home)])[, .N, by = cell_home][order(cell_home)])
cat("cell_nr counts:\n");   print(unique(pA[, .(muni, cell_nr)])[, .N, by = cell_nr][order(cell_nr)])

run_twfe_pooled_A <- function(p, outcome) {
  d <- clean_for(p, outcome, c("cell_home", "cell_nr", "dose_home", "dose_nr"))
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(cell_home, dose_home, ref='NC') + i(cell_nr, dose_nr, ref='NC') + ",
       ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}

# Stacked continuous on home side (one set of stacks for "home" first events)
build_stack_one_side_v2 <- function(p, side_tag, dose_col) {
  cell_v <- paste0("cell_", side_tag)
  c_v    <- paste0("c_", side_tag)
  treated_munis <- unique(p[!is.na(get(c_v)) & get(cell_v) != "NC",
                            .(muni, cohort = get(c_v), cell = get(cell_v))])
  nc_munis <- unique(p[get(cell_v) == "NC", muni])
  cohort_years <- sort(unique(treated_munis$cohort))
  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated_munis[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]
    d[, rel   := year - cy]
    d[, treat := treat_cell != "NC"]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  if (nrow(stk) == 0) return(NULL)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(get(dose_col)[rel < 0], na.rm = TRUE),
      by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := get(dose_col) - base_rate]
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind",
                   "Inc-loses", "Dec-gains")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  stk
}

run_stack_cont <- function(p, outcome, side_tag, dose_col) {
  stk <- build_stack_one_side_v2(p, side_tag, dose_col)
  if (is.null(stk) || nrow(stk) == 0) return(NULL)
  stk2 <- stk[, c("muni","year","treat_cell","dose_stack","stack_muni",
                  "stack_year", outcome, ctrls), with = FALSE]
  stk2 <- stk2[complete.cases(stk2)]
  if (nrow(stk2) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_stack, ref='NC') + ", ctrls_str,
       " | stack_muni + stack_year"))
  feols(f, data = stk2, cluster = ~muni)
}

extract_treat_cell <- function(m, side_tag) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("treat_cell::", term)]
  ct[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]
  ct[, side := side_tag]; ct[, n := nobs(m)]
  ct[, .(side, cell, est, se, p, n)]
}

resA_long <- list()

cat("\n-- Mode A TWFE pooled --\n")
for (o in outcomes) {
  m <- run_twfe_pooled_A(pA, o)
  if (is.null(m)) next
  rr <- rbind(extract_cell_coefs(m, "home"), extract_cell_coefs(m, "nr"))
  rr[, `:=`(estimator = "twfe_pooled", mode = "homestack", outcome = o)]
  resA_long[[length(resA_long) + 1L]] <- rr
}

cat("\n-- Mode A Stacked continuous (home side) --\n")
for (o in outcomes) {
  m <- run_stack_cont(pA, o, "home", "dose_home")
  if (is.null(m)) next
  rr <- extract_treat_cell(m, "home")
  rr[, `:=`(estimator = "stacked_continuous", mode = "homestack", outcome = o)]
  resA_long[[length(resA_long) + 1L]] <- rr
}
cat("\n-- Mode A Stacked continuous (nr side) --\n")
for (o in outcomes) {
  m <- run_stack_cont(pA, o, "nr", "dose_nr")
  if (is.null(m)) next
  rr <- extract_treat_cell(m, "nr")
  rr[, `:=`(estimator = "stacked_continuous", mode = "homestack", outcome = o)]
  resA_long[[length(resA_long) + 1L]] <- rr
}

# ---------------------------------------------------------------------------
# 5. MODE B: muni + sd + nr separately decomposed
# ---------------------------------------------------------------------------
cat("\n\n========== MODE B: muni + sd + nr separately ==========\n")
pB <- copy(panel_drop)
pB <- classify_treatment(pB, "resident_eit",   "muni_nonres_eit", "muni")
pB <- classify_treatment(pB, "sd_eit",          "muni_nonres_eit", "sd")
pB <- classify_treatment(pB, "muni_nonres_eit", "home_stack",       "nr")
cat("cell_muni counts:\n"); print(unique(pB[, .(muni, cell_muni)])[, .N, by = cell_muni][order(cell_muni)])
cat("cell_sd counts:\n");   print(unique(pB[, .(muni, cell_sd)])[, .N, by = cell_sd][order(cell_sd)])
cat("cell_nr counts:\n");   print(unique(pB[, .(muni, cell_nr)])[, .N, by = cell_nr][order(cell_nr)])

run_twfe_pooled_B <- function(p, outcome) {
  d <- clean_for(p, outcome, c("cell_muni","cell_sd","cell_nr",
                                "dose_muni","dose_sd","dose_nr"))
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(cell_muni, dose_muni, ref='NC') + ",
             "i(cell_sd, dose_sd, ref='NC') + ",
             "i(cell_nr, dose_nr, ref='NC') + ",
             ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}

resB_long <- list()

cat("\n-- Mode B TWFE pooled --\n")
for (o in outcomes) {
  m <- run_twfe_pooled_B(pB, o)
  if (is.null(m)) next
  rr <- rbind(extract_cell_coefs(m, "muni"),
              extract_cell_coefs(m, "sd"),
              extract_cell_coefs(m, "nr"))
  rr[, `:=`(estimator = "twfe_pooled", mode = "decomp", outcome = o)]
  resB_long[[length(resB_long) + 1L]] <- rr
}

cat("\n-- Mode B Stacked continuous (each side) --\n")
for (sd_tag in c("muni","sd","nr")) {
  dose_col <- paste0("dose_", sd_tag)
  for (o in outcomes) {
    m <- run_stack_cont(pB, o, sd_tag, dose_col)
    if (is.null(m)) next
    rr <- extract_treat_cell(m, sd_tag)
    rr[, `:=`(estimator = "stacked_continuous", mode = "decomp", outcome = o)]
    resB_long[[length(resB_long) + 1L]] <- rr
  }
}

# ---------------------------------------------------------------------------
# 6. Save outputs
# ---------------------------------------------------------------------------
results <- rbindlist(c(resA_long, resB_long), fill = TRUE)
results[, sig := fifelse(p < 0.01, "***",
                  fifelse(p < 0.05, "**",
                  fifelse(p < 0.10, "*", "")))]
results[, est_str := sprintf("%.3f%s", est, ifelse(is.na(sig), "", sig))]
fwrite(results, file.path(out_dir, "compare_results_long.csv"))
cat(sprintf("\nSaved compare_results_long.csv with %s rows\n",
            format(nrow(results), big.mark = ",")))

# Wide tables per (mode, estimator)
for (m in unique(results$mode)) {
  for (e in unique(results$estimator)) {
    sub <- results[mode == m & estimator == e]
    if (nrow(sub) == 0) next
    sub[, side_cell := paste0(side, "_", cell)]
    wide <- dcast(sub, outcome ~ side_cell, value.var = "est_str")
    fwrite(wide, file.path(out_dir, sprintf("compare_table_%s_%s.csv", m, e)))
    cat("Saved:", sprintf("compare_table_%s_%s.csv", m, e), "\n")
  }
}

cat("\nDone.\n")
