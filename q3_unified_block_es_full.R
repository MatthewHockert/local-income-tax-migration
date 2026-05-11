# q3_unified_block_es_full.R
# Block-level Sun-Abraham ES with realized dose on the MUNI side,
# fully parameterized: SD-side and nonres-side cell x dose interactions
# kept exactly as in the headline equation. Apples-to-apples block-level
# analog of q3_unified_headline_es.R.
#
# Spec, run per muni-side cell c (subset {NC, c} on muni cell):
#   y_{bt} = sum_{k != -1} beta^M_k * 1{rel = k} * realized_dose_muni_b
#          + sum_{c'} beta^S_{c'} * Delta tau_SD_{bt} * 1{cell_SD = c'}
#          + sum_{c'} beta^N_{c'} * Delta tau_nr_{bt} * 1{cell_nr = c'}
#          + X_{mt} phi
#          + alpha_{stack,b} + lambda_{stack,t} + epsilon
# Cluster: ~ muni + sd_id

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
sd_ctrls   <- c("share_state_rev_sd","share_local_rev_sd",
                "share_federal_rev_sd","share_property_tax_sd",
                "share_instructional_sd","share_support_sd",
                "share_capital_sd","share_debt_int_sd")
ctrls <- c(muni_ctrls, sd_ctrls)
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

# 1. Source the cousub-level data prep so we can borrow the SD-side and
#    nonres-side cell labels + doses (computed at cousub level there).
cat("Sourcing cousub-level full table prep for SD/nonres cells...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
cousub_panel <- e$panel
# Keep just the columns we need to attach to blocks.
cousub_meta <- unique(cousub_panel[, .(muni, year,
                                        cell_sd, cell_nr,
                                        dose_sd, dose_nr,
                                        c_muni, cell_muni,
                                        share_police, share_streets, share_debt_svc,
                                        share_eit, share_re, share_intergov)])

# 2. Load block panel (uses pa_block_panel.rds; built in build_block_panel.R)
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
# Drop block panel's share_* / cell_* columns since we'll use the cousub-level
# definitions (consistent with headline).
drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                          "share_eit","share_re","share_intergov"),
                        names(b))
b[, (drop_cols) := NULL]

# 3. Compute realized muni dose per muni (constant within muni)
mu_year <- unique(cousub_panel[, .(muni, year, resident_eit, c_muni)])
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
nc_munis <- unique(cousub_panel[cell_muni == "NC", muni])
rd <- rbind(rd, data.table(muni = nc_munis, realized_dose_muni = 0))

# 4. Merge cousub-level metadata onto block panel
b <- merge(b, cousub_meta, by = c("muni","year"), all.x = TRUE)
b <- merge(b, rd, by = "muni", all.x = TRUE)

# Merge SD-level fiscal share controls (Census F-33; built by
# build_sd_controls.R). Each block inherits its SD's fiscal posture.
sd_ctrls_path <- file.path(data_dir, "pa_sd_controls.rds")
if (file.exists(sd_ctrls_path)) {
  sd_ctrl <- as.data.table(readRDS(sd_ctrls_path))
  sd_ctrl_cols <- c("sd_id","year", sd_ctrls)
  sd_ctrl_cols <- intersect(sd_ctrl_cols, names(sd_ctrl))
  sd_ctrl <- sd_ctrl[, ..sd_ctrl_cols]
  # Collapse multiple (sd_id, year) rows (SCHLEV variants etc.) to one
  sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                       if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                     by = .(sd_id, year)]
  for (cc in sd_ctrls) {
    set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
  }
  b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)
  cat(sprintf("\nMerged SD fiscal controls: %d / %d block-years matched\n",
              sum(!is.na(b$share_state_rev_sd)),
              nrow(b)))
} else {
  cat("\nWARNING: data/pa_sd_controls.rds not found; SD fiscal controls absent.\n")
  for (cc in sd_ctrls) b[, (cc) := NA_real_]
}
b[is.na(realized_dose_muni), realized_dose_muni := 0]

cat("\nMuni cell distribution (block-year):\n")
print(b[, .N, keyby = cell_muni])

# 5. Build stacked panel on muni cohorts (mirroring headline construction)
build_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC" & !is.na(cell_muni),
                       .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t[, treat_cell_muni := cell_muni]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell_muni := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }), fill = TRUE)
}

cat("\nBuilding block-level stacked panel (this is large)...\n")
stk <- build_stack(b)
stk[, stack_muni  := paste0(stack, "_", muni)]
stk[, stack_sd    := paste0(stack, "_", sd_id)]
stk[, stack_year  := paste0(stack, "_", year)]
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk[, cell_sd := factor(cell_sd, levels = cell_levels)]
stk[, cell_nr := factor(cell_nr, levels = cell_levels)]
stk[is.na(dose_sd), dose_sd := 0]
stk[is.na(dose_nr), dose_nr := 0]

cat(sprintf("Stack: %s rows; treated blocks: %s; NC blocks: %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell_muni != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell_muni == "NC", block_geoid]), big.mark=",")))

# 6. Per-cell ES with full SD + nonres controls
es_results <- list(); att_results <- list()
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- stk[treat_cell_muni %in% c("NC", cl)]
    cols <- c("block_geoid","year","muni","sd_id","stack",
               "treat_cell_muni","realized_dose_muni","rel",
               "cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls)
    cols <- intersect(cols, names(dd))
    dd <- dd[, ..cols]
    dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0 || sum(dd$treat_cell_muni == cl) == 0) {
      cat(sprintf("  %s skip\n", cl)); next
    }
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1) ",
         "+ i(cell_sd, dose_sd, ref = 'NC') ",
         "+ i(cell_nr, dose_nr, ref = 'NC') + ",
         ctrls_str, " | block_geoid^stack + year^stack"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni + sd_id),
                   error = function(ee) {
                     cat(sprintf("  %s error: %s\n", cl, conditionMessage(ee))); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) next
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic
    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    wp <- wald_pretrend(m, "^rel::-[2-9]")
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, cell = cl,
      att_post_per_pp = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se     = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se      = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      wald_pre_stat = round(wp$stat, 3),
      wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4),
      n = nobs(m))
    cat(sprintf("  %-16s post=%6.3f (SE %.3f)  pre=%6.3f (SE %.3f)  N=%s\n",
                cl, mean(post$est, na.rm=TRUE),
                sqrt(mean(post$se^2, na.rm=TRUE) / max(nrow(post),1)),
                mean(pre$est, na.rm=TRUE),
                sqrt(mean(pre$se^2, na.rm=TRUE) / max(nrow(pre),1)),
                format(nobs(m), big.mark=",")))
  }
}
es_dt  <- rbindlist(es_results, fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "block_es_full_coefs.csv"))
fwrite(att_dt, file.path(out_dir, "block_es_full_att.csv"))

# Figures
es_plot <- copy(es_dt)
es_plot <- rbind(es_plot,
                  unique(es_plot[, .(outcome, cell)])[, .(outcome, cell, rel = -1L,
                                                          est = 0, se = 0, p = NA, n = NA)],
                  fill = TRUE)
es_plot[, lo := est - 1.96 * se]
es_plot[, hi := est + 1.96 * se]
for (o in outcomes) {
  dd <- es_plot[outcome == o]
  if (nrow(dd) == 0) next
  p <- ggplot(dd, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) +
    geom_line() +
    facet_wrap(~ cell, ncol = 3, scales = "free_y") +
    labs(title = paste0("Block-level fully-parameterized SA ES (muni side): ", o),
         subtitle = "Per-pp realized-dose effect at event-time k; SD+nonres cell x dose controls retained",
         x = "Event time", y = expression(beta[k])) +
    theme_bw(base_size = 9)
  ggsave(file.path(fig_dir, paste0("block_es_full_", o, ".png")),
         p, width = 8.5, height = 5, dpi = 300)
}
cat("\n--- Block-level fully-parameterized ES summary (muni side) ---\n")
print(att_dt)
