# q3_unified_robustness_passes.R
# Two robustness passes for the stacked-continuous DiD headline:
#
#   (1) Dose-magnitude tercile split: split Inc-bind munis by total dose
#       (small / medium / large) and re-estimate. Tests linearity of the
#       per-pp coefficient across dose magnitudes.
#
#   (2) Alternative control group: "no-events-in-window" controls
#       (Cengiz-style). Allows munis that have events outside the
#       (c - K, c + K) window to serve as controls in any cohort whose
#       window does not contain their event. Should give similar results
#       since NC is a strict subset.

suppressMessages({
  library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
max_year <- 2019
K_stack <- 5L

# Reuse data prep + classification from q3_unified_full_table.R via source.
# That script already builds `panel` with cell labels and `stk_muni` with
# stacks for the muni-rate side under Mode B (NC fix applied).
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel    <- e$panel
ctrls    <- e$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

# ---------------------------------------------------------------------------
# (1) Dose-magnitude tercile split
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("(1) Dose-magnitude tercile split (Inc-bind cell only)\n")
cat("=========================================================\n")

# Compute "total dose" per Inc-bind muni: cumulative Δτ at year +5 post-event,
# or maximum observed if year +5 is past panel end
incbind_munis <- unique(panel[cell_muni == "Inc-bind", muni])
incbind_dose <- panel[muni %in% incbind_munis & year >= c_muni & year <= c_muni + 5,
                       .(total_dose = max(dose_muni, na.rm = TRUE)),
                       by = muni]
incbind_dose[, dose_tercile := cut(total_dose, breaks = quantile(total_dose, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                     include.lowest = TRUE,
                                     labels = c("small", "medium", "large"))]
cat("\nInc-bind munis by dose tercile:\n")
print(incbind_dose[, .(N = .N, mean_dose = round(mean(total_dose, na.rm = TRUE), 3),
                       min_dose = round(min(total_dose, na.rm = TRUE), 3),
                       max_dose = round(max(total_dose, na.rm = TRUE), 3)),
                   by = dose_tercile])

# Build sub-experiment per cohort, restricted to one tercile + all NC munis
build_stack_tercile <- function(p, target_munis) {
  treated <- unique(p[muni %in% target_munis & cell_muni == "Inc-bind",
                       .(muni, cohort = c_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t[, treat_cell := "Inc-bind"]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }), fill = TRUE)
}

# For each tercile, run the stacked-continuous spec on log(live_and_work)
tercile_results <- list()
for (t in c("small", "medium", "large")) {
  munis_t <- incbind_dose[dose_tercile == t, muni]
  cat(sprintf("\nTercile = %s: %d munis, dose range [%.3f, %.3f]\n",
              t, length(munis_t),
              min(incbind_dose[dose_tercile == t, total_dose]),
              max(incbind_dose[dose_tercile == t, total_dose])))
  stk_t <- build_stack_tercile(panel, munis_t)
  stk_t[, stack_muni := paste0(stack, "_", muni)]
  stk_t[, stack_year := paste0(stack, "_", year)]
  stk_t[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE),
        by = .(stack, muni)]
  stk_t[!is.finite(base_rate), base_rate := 0]
  stk_t[, dose_stack := dose_muni - base_rate]
  stk_t[, treat_cell := factor(treat_cell, levels = c("NC", "Inc-bind"))]

  d <- stk_t[, c("muni","year","treat_cell","dose_stack","stack_muni",
                  "stack_year","log_live_and_work", ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0("log_live_and_work ~ ",
       "i(treat_cell, dose_stack, ref='NC') + ",
       paste(ctrls, collapse = " + "),
       " | stack_muni + stack_year"))
  m <- feols(f, data = d, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("Inc-bind:dose_stack", term)]
  cat(sprintf("  Inc-bind coef: %.3f (SE %.3f, p=%.4f)  N=%s\n",
              ic$Estimate, ic$`Std. Error`, ic$`Pr(>|t|)`,
              format(nobs(m), big.mark = ",")))
  tercile_results[[t]] <- data.table(
    tercile = t, n_munis = length(munis_t),
    est = round(as.numeric(ic$Estimate), 3),
    se = round(as.numeric(ic$`Std. Error`), 3),
    p = round(as.numeric(ic$`Pr(>|t|)`), 4),
    n = nobs(m)
  )
}
tercile_dt <- rbindlist(tercile_results, fill = TRUE)
fwrite(tercile_dt, file.path(out_dir, "robustness_tercile.csv"))
cat("\n--- Summary: dose-tercile robustness ---\n")
print(tercile_dt)

# ---------------------------------------------------------------------------
# (2) Alternative control group: "no events in 8-yr window" (Cengiz-style)
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("(2) Alternative controls: no events in 8-yr window\n")
cat("=========================================================\n")

# For each cohort year cy, control munis = munis with NO muni-rate event
# in [cy - 4, cy + 4] (8-year window). This includes our NC pool plus munis
# whose events are outside this window.
build_stack_clean8yr <- function(p) {
  treated <- unique(p[!is.na(c_muni) & cell_muni != "NC",
                       .(muni, cohort = c_muni, cell = cell_muni)])
  cohort_years <- sort(unique(treated$cohort))

  # All event years per muni (need this to test "no event in [cy - K, cy + K]")
  rate_changes_by_muni <- p[!is.na(resident_eit), {
    tab <- copy(.SD)
    setorder(tab, year)
    chg <- which(abs(diff(tab$resident_eit)) > 1e-6 & !is.na(diff(tab$resident_eit)))
    list(event_years = list(tab$year[chg + 1]))
  }, by = muni, .SDcols = c("year","resident_eit")]
  setDT(rate_changes_by_muni)

  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    # Treated for this cohort
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    # Controls: any muni whose events fall OUTSIDE [cy - K, cy + K]
    is_clean_in_window <- sapply(rate_changes_by_muni$event_years, function(yrs) {
      length(intersect(yrs, win)) == 0
    })
    clean_munis <- rate_changes_by_muni$muni[is_clean_in_window]
    # Drop the treated cohort itself
    clean_munis <- setdiff(clean_munis, t_munis$muni)
    d_c <- p[muni %in% clean_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }
  rbindlist(lapply(cohort_years, build_one), fill = TRUE)
}

cat("\nBuilding 'no events in window' stack...\n")
stk_clean <- build_stack_clean8yr(panel)
stk_clean[, stack_muni := paste0(stack, "_", muni)]
stk_clean[, stack_year := paste0(stack, "_", year)]
stk_clean[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE), by = .(stack, muni)]
stk_clean[!is.finite(base_rate), base_rate := 0]
stk_clean[, dose_stack := dose_muni - base_rate]
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")
stk_clean[, treat_cell := factor(treat_cell, levels = cell_levels)]

cat(sprintf("Stacked panel size: %s rows; treated munis: %s; control munis (clean-in-window): %s\n",
            format(nrow(stk_clean), big.mark = ","),
            format(uniqueN(stk_clean[treat_cell != "NC", muni]), big.mark = ","),
            format(uniqueN(stk_clean[treat_cell == "NC", muni]), big.mark = ",")))

# Run headline regression with clean-in-window controls
outcomes <- c("log_live_and_work","log_in_commuters","log_total_residents",
              "share_live_in","share_work_outside")
clean_results <- list()
for (o in outcomes) {
  d <- stk_clean[, c("muni","year","treat_cell","dose_stack","stack_muni",
                      "stack_year", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                          paste(ctrls, collapse = " + "),
                          " | stack_muni + stack_year"))
  m <- feols(f, data = d, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("Inc-bind:dose_stack|Inc-becomes:dose_stack|Inc-nobind:dose_stack", term)]
  ct[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]
  clean_results[[o]] <- ct[, .(outcome, cell, est = round(est, 3),
                                se = round(se, 3), p = round(p, 4), n)]
}
clean_dt <- rbindlist(clean_results, fill = TRUE)
fwrite(clean_dt, file.path(out_dir, "robustness_clean_controls.csv"))

cat("\n--- Summary: alternative-controls (no events in 8-yr window) ---\n")
print(clean_dt)

cat("\n=== Compare to NC-only headline (from data/q3_unified/results_long.csv) ===\n")
hdl <- fread(file.path(out_dir, "results_long.csv"))
hdl_inc <- hdl[sample == "drop" & estimator == "stacked_continuous" & side == "res" &
                cell %in% c("Inc-bind","Inc-becomes","Inc-nobind") &
                outcome %in% outcomes]
hdl_inc <- hdl_inc[, .(outcome, cell, est_NC = round(est, 3), se_NC = round(se, 3))]
compare <- merge(clean_dt[, .(outcome, cell, est_clean = est, se_clean = se)],
                  hdl_inc, by = c("outcome","cell"))
print(compare)
cat("\nDone.\n")
