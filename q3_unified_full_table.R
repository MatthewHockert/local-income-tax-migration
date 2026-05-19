# q3_unified_full_table.R
# Re-run the headline stacked-continuous DiD for the eight outcomes on
# the muni-rate side (with SD and nr cells in the same regression as in
# Mode B), and produce a publication-ready table with SEs, R^2, Within
# R^2, N, FE info, and control-coefficient estimates.
#
# Output: data/q3_unified/headline_full_table.csv (one row per coefficient,
# with all diagnostics) and data/q3_unified/headline_diagnostics.csv
# (one row per outcome, with R^2, N, etc.).

suppressMessages({
  library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
max_year <- 2019
K_stack <- 5L

# ---------------------------------------------------------------------------
# Load + setup (mirrors q3_unified.R / compare; uses Mode B)
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
  by = c("muni", "year"), all = TRUE)
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
panel[, in_commuters := live_outside]
panel[, out_commuters := residents_work_outside]
panel[, total_residents := residents_employed]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
panel[, log_live_and_work := log_pos(live_and_work)]
panel[, log_in_commuters  := log_pos(in_commuters)]
panel[, log_out_commuters := log_pos(out_commuters)]
panel[, log_total_jobs    := log_pos(total_jobs)]
panel[, log_total_residents := log_pos(total_residents)]
panel[, share_nonres := ifelse(total_jobs > 0, in_commuters / total_jobs, NA_real_)]

afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
fc <- afr[!is.na(cousub_fips), {
  total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
  total_exp <- as.numeric(total_expenditures)
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
  police_exp <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
  streets_exp <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                    `public_works-highways_and_streets_expenditures`,
                                    streets_and_roads))
  debt_svc <- as.numeric(debt_service_expenditures)
  list(muni = cousub_fips, year = yr,
       share_police    = ifelse(total_exp > 0, police_exp  / total_exp, NA_real_),
       share_streets   = ifelse(total_exp > 0, streets_exp / total_exp, NA_real_),
       share_debt_svc  = ifelse(total_exp > 0, debt_svc    / total_exp, NA_real_),
       share_eit       = ifelse(total_rev > 0, eit_rev  / total_rev, NA_real_),
       share_re        = ifelse(total_rev > 0, re_tax   / total_rev, NA_real_),
       share_intergov  = ifelse(total_rev > 0, intergov / total_rev, NA_real_))
}]
fc <- fc[!is.na(muni) & !is.na(year)]
fc <- fc[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
         by = .(muni, year)]
panel <- merge(panel, fc, by = c("muni", "year"), all.x = TRUE)
panel <- panel[, lapply(.SD, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)
}), by = .(muni, year)]
for (col in names(panel)) {
  if (is.numeric(panel[[col]])) {
    set(panel, which(!is.finite(panel[[col]])), col, NA_real_)
  }
}

ctrls <- c("share_police", "share_streets", "share_debt_svc",
           "share_eit", "share_re", "share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

# Cell classification (NC fix applied)
classify_treatment <- function(p, rate_var, bind_ref, tag) {
  setorder(p, muni, year)
  p[, home_binds := as.integer(home_stack > muni_nonres_eit)]
  lag_col <- paste0("lag_", tag)
  p[, (lag_col) := shift(get(rate_var)), by = muni]
  ev_col <- paste0("ev_", tag)
  p[, (ev_col) := fifelse(is.na(get(lag_col)), NA_character_,
                  fifelse(get(rate_var) > get(lag_col) + 1e-6, "Inc",
                  fifelse(get(rate_var) < get(lag_col) - 1e-6, "Dec", NA_character_)))]
  ev <- p[!is.na(get(ev_col)), .SD[1], by = muni,
          .SDcols = c("year", ev_col, lag_col, rate_var, "home_binds")]
  setnames(ev, c("year", ev_col, lag_col, rate_var, "home_binds"),
           c("c_var","ev_dir","baseline_var","first_post","postbind"))
  pbind <- p[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
  ev <- merge(ev, pbind[, .(muni, year, lag_home_binds)],
              by.x = c("muni","c_var"), by.y = c("muni","year"), all.x = TRUE)
  ev[, prebind := lag_home_binds]; ev[, lag_home_binds := NULL]
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
  rate_changes <- p2[!is.na(get(rate_var)), .(
    n = sum(abs(diff(get(rate_var))) > 1e-6, na.rm = TRUE)
  ), by = muni]
  truly_stable <- rate_changes[n == 0, muni]
  p2[muni %in% truly_stable, (cell_v) := "NC"]
  p2[!muni %in% truly_stable & is.na(get(cell_v)), (cell_v) := "Uncertain"]
  # Flip-flopper filter: after the first event, all subsequent rate changes
  # in the same direction. Any opposite-direction reversal -> Uncertain.
  ff <- p2[!is.na(get(rate_var)), .(year, rate = get(rate_var)), by = muni][
    order(muni, year)]
  ff[, d := c(NA_real_, diff(rate)), by = muni]
  ff_summary <- ff[!is.na(d) & abs(d) > 1e-6, .(
    n_inc = sum(d > 0), n_dec = sum(d < 0)), by = muni]
  flip <- ff_summary[n_inc > 0 & n_dec > 0, muni]
  p2[muni %in% flip, (cell_v) := "Uncertain"]
  p2[, (dose_v) := fifelse(!is.na(get(c_v)) & year >= get(c_v),
                           get(rate_var) - get(base_v), 0)]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                   "Dec-bind","Dec-becomes-non","Dec-nobind",
                   "Inc-loses","Dec-gains","Uncertain")
  p2[, (cell_v) := factor(get(cell_v), levels = cell_levels)]
  p2
}

panel <- classify_treatment(panel, "resident_eit",   "muni_nonres_eit", "muni")
panel <- classify_treatment(panel, "sd_eit",          "muni_nonres_eit", "sd")
panel <- classify_treatment(panel, "muni_nonres_eit", "home_stack",       "nr")
u <- Reduce(union, list(unique(panel[cell_muni == "Uncertain", muni]),
                         unique(panel[cell_sd   == "Uncertain", muni]),
                         unique(panel[cell_nr   == "Uncertain", muni])))
panel <- panel[!muni %in% u]

# Drop Dec munis entirely (paper scope: Inc cells only).
# Removes any muni whose muni-side OR SD-side classification is in the Dec set;
# Dec munis are not treated, not controls.
dec_cells_drop <- c("Dec-bind","Dec-becomes-non","Dec-nobind","Dec-gains")
dec_munis <- unique(c(panel[cell_muni %in% dec_cells_drop, muni],
                       panel[cell_sd   %in% dec_cells_drop, muni],
                       panel[cell_nr   %in% dec_cells_drop, muni]))
cat(sprintf("Dropping %d Dec munis (%.1f%% of panel).\n",
            length(dec_munis),
            100 * length(dec_munis) / uniqueN(panel$muni)))
panel <- panel[!muni %in% dec_munis]
panel[, cell_muni := droplevels(cell_muni)]
panel[, cell_sd   := droplevels(cell_sd)]
panel[, cell_nr   := droplevels(cell_nr)]

# Build stack for muni side (Mode B)
build_stack <- function(p, side_tag, dose_col, K = K_stack) {
  cell_v <- paste0("cell_", side_tag)
  c_v    <- paste0("c_", side_tag)
  treated_munis <- unique(p[!is.na(get(c_v)) & get(cell_v) != "NC",
                             .(muni, cohort = get(c_v), cell = get(cell_v))])
  nc_munis <- unique(p[get(cell_v) == "NC", muni])
  cohort_years <- sort(unique(treated_munis$cohort))
  build_one <- function(cy) {
    win <- (cy - K):(cy + K)
    t_munis <- treated_munis[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(get(dose_col)[rel < 0], na.rm = TRUE),
      by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := get(dose_col) - base_rate]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                   "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  stk
}
stk_muni <- build_stack(panel, "muni", "dose_muni")

# ---------------------------------------------------------------------------
# Run headline regression for each outcome and collect full diagnostics
# ---------------------------------------------------------------------------
outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
              "log_total_jobs", "log_total_residents",
              "share_live_in", "share_work_outside", "share_nonres")

run_headline <- function(stk, outcome) {
  # Unified resident-side spec: muni AND SD cell x dose in one regression.
  # cell_sd / dose_sd are computed in the panel build above; bring them in.
  cols <- c("muni","year","treat_cell","dose_stack",
             "cell_sd","dose_sd","stack_muni","stack_year",
             outcome, ctrls)
  cols <- intersect(cols, names(stk))
  d <- stk[, ..cols]
  if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  # Resident-side unified: muni + SD cell x dose interactions.
  # NOTE: nonresident side intentionally excluded per current paper scope.
  # NOTE: Cousub-level model intentionally uses only muni AFR shares; the
  #       SD F-33 shares enter at the SD- and block-level specifications.
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_stack, ref='NC') ",
       "+ i(cell_sd, dose_sd, ref='NC') + ",
       paste(ctrls, collapse=" + "),
       " | stack_muni + stack_year"))
  feols(f, data = d, cluster = ~muni)
}

cat("\nRunning headline regressions for each outcome...\n")
models <- list()
for (o in outcomes) {
  cat("  ", o, "...\n")
  m <- run_headline(stk_muni, o)
  if (!is.null(m)) models[[o]] <- m
}

# Extract full coefficient tables + diagnostics
all_coefs <- list()
diagnostics <- list()

for (o in names(models)) {
  m <- models[[o]]
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct[, outcome := o]
  ct[, n := nobs(m)]
  all_coefs[[o]] <- ct

  # Diagnostics (R2, within R2, mean of dep var)
  diag_row <- data.table(
    outcome    = o,
    n          = nobs(m),
    R2         = round(fitstat(m, "r2", simplify = TRUE), 4),
    Within_R2  = round(fitstat(m, "wr2", simplify = TRUE), 4),
    dep_mean   = round(mean(m$residuals + m$fitted.values, na.rm = TRUE), 4),
    dep_sd     = round(sd(m$residuals + m$fitted.values, na.rm = TRUE), 4),
    cluster_n  = m$nobs_origin %/% 1L,  # rough
    fe         = "stack_muni + stack_year",
    se_type    = "cluster: muni"
  )
  diagnostics[[o]] <- diag_row
}

coefs <- rbindlist(all_coefs, fill = TRUE)
coefs[, sig := fifelse(p < 0.01, "***",
                fifelse(p < 0.05, "**",
                fifelse(p < 0.10, "*", "")))]
fwrite(coefs, file.path(out_dir, "headline_full_table.csv"))

diag_dt <- rbindlist(diagnostics, fill = TRUE)
fwrite(diag_dt, file.path(out_dir, "headline_diagnostics.csv"))

cat("\n--- Headline regression diagnostics ---\n")
print(diag_dt)

cat("\n--- Inc-bind coefficient with SE per outcome ---\n")
print(coefs[grepl("Inc-bind:dose_stack", term),
            .(outcome, term, est = round(est, 3), se = round(se, 3), p = round(p, 4),
              n)])

cat("\n--- Control coefficients on log_live_and_work ---\n")
ctrl_lw <- coefs[outcome == "log_live_and_work" & term %in% ctrls]
print(ctrl_lw[, .(term, est = round(est, 4), se = round(se, 4),
                   p = round(p, 4), sig)])

cat("\nSaved data/q3_unified/headline_full_table.csv (all coefficients)\n")
cat("Saved data/q3_unified/headline_diagnostics.csv (one row per outcome)\n")
