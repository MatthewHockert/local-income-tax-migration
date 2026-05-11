# q3_unified_sd_level.R
# Flip the unit of observation: aggregate from cousub level to school
# district (SD) level. Treatment = SD rate; controls = average muni
# rate in the SD plus share-based fiscal controls. SD FE + year FE.
# Stacked-continuous DiD on SD-rate cell decomposition.
#
# Sample: 500 PA SDs over 2002-2019.
# 214 SDs change rate at some point; 286 never change (NC pool).

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
# 1. Load data
# ---------------------------------------------------------------------------
wp <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
rp <- as.data.table(readRDS(file.path(data_dir, "pa_residence_panel.rds")))[year <= max_year]
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
eit <- fread(file.path(data_dir, "pa_eit_rate_panel.csv"))

setnames(wp, "work_city", "muni", skip_absent = TRUE)
setnames(rp, "home_city", "muni", skip_absent = TRUE)

# Cousub × year outcome panel
panel_cousub <- merge(
  wp[, .(muni, year, live_and_work, live_outside, total_jobs)],
  rp[, .(muni, year, residents_employed, residents_work_outside)],
  by = c("muni", "year"), all = TRUE
)
panel_cousub[, in_commuters := live_outside]
panel_cousub[, out_commuters := residents_work_outside]
panel_cousub[, total_residents := residents_employed]

# Cousub → SD assignment from EIT rate panel
# For multi-SD munis, take the SD with the maximum sd_eit_rate (consistent with
# pa_rate_panel.rds construction)
eit_resid <- eit[is_resident == 1 & !is.na(sd_id) & !is.na(sd_eit_rate) & year <= max_year]
muni_sd_assign <- eit_resid[, .(sd_id = sd_id[which.max(sd_eit_rate)],
                                  sd_eit_rate = max(sd_eit_rate, na.rm = TRUE),
                                  muni_eit_rate = first(muni_eit_rate)),
                            by = .(municipality_id, year)]
# Add cousub_fips
xw <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
muni_sd_assign <- merge(muni_sd_assign, xw, by = "municipality_id", all.x = TRUE)
muni_sd_assign <- muni_sd_assign[!is.na(cousub_fips)]
setnames(muni_sd_assign, "cousub_fips", "muni")

cat(sprintf("Cousub-year-SD assignments: %s\n",
            format(nrow(muni_sd_assign), big.mark = ",")))
cat(sprintf("Unique SDs: %d, unique munis: %d\n",
            uniqueN(muni_sd_assign$sd_id),
            uniqueN(muni_sd_assign$muni)))

# Merge SD assignment to cousub panel
panel_cousub <- merge(panel_cousub, muni_sd_assign[, .(muni, year, sd_id,
                                                          sd_eit_rate, muni_eit_rate)],
                      by = c("muni","year"), all.x = TRUE)
panel_cousub <- panel_cousub[!is.na(sd_id)]

# Merge in muni_nonres_eit from the canonical rate panel (the actual work-side
# nonresident rate paid by an out-of-muni worker, distinct from the muni
# resident rate). We aggregate this to the SD level below.
rate_nr <- rate[!is.na(cousub_fips) & year <= max_year,
                .(muni = cousub_fips, year,
                  muni_nonres_eit = as.numeric(muni_nonres_eit))]
rate_nr <- rate_nr[, .(muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                   by = .(muni, year)]
rate_nr[!is.finite(muni_nonres_eit), muni_nonres_eit := NA_real_]
panel_cousub <- merge(panel_cousub, rate_nr, by = c("muni","year"), all.x = TRUE)

# ---------------------------------------------------------------------------
# 2. Aggregate cousub → SD
# ---------------------------------------------------------------------------
# Outcomes: sum across cousubs in same SD
sd_panel <- panel_cousub[, .(
  live_and_work    = sum(live_and_work, na.rm = TRUE),
  in_commuters     = sum(in_commuters, na.rm = TRUE),
  out_commuters    = sum(out_commuters, na.rm = TRUE),
  total_jobs       = sum(total_jobs, na.rm = TRUE),
  total_residents  = sum(total_residents, na.rm = TRUE),
  sd_eit_rate      = first(sd_eit_rate),                          # one SD rate per SD-year
  mean_muni_rate   = mean(muni_eit_rate, na.rm = TRUE),           # avg muni resident rate in SD (control)
  pop_weighted_muni_rate = weighted.mean(muni_eit_rate, total_residents, na.rm = TRUE),
  mean_muni_nonres = mean(muni_nonres_eit, na.rm = TRUE),         # avg work-side nonres rate in SD
  max_muni_nonres  = max(muni_nonres_eit, na.rm = TRUE),          # peak nonres rate in SD
  n_munis_in_sd    = uniqueN(muni)
), by = .(sd_id, year)]

log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
sd_panel[, log_live_and_work    := log_pos(live_and_work)]
sd_panel[, log_in_commuters     := log_pos(in_commuters)]
sd_panel[, log_out_commuters    := log_pos(out_commuters)]
sd_panel[, log_total_jobs       := log_pos(total_jobs)]
sd_panel[, log_total_residents  := log_pos(total_residents)]
sd_panel[, share_live_in        := ifelse(total_jobs > 0, live_and_work / total_jobs, NA_real_)]
sd_panel[, share_work_outside   := ifelse(total_residents > 0, out_commuters / total_residents, NA_real_)]
sd_panel[, share_nonres         := ifelse(total_jobs > 0, in_commuters / total_jobs, NA_real_)]

cat(sprintf("\nSD panel: %s SD-years, %d unique SDs\n",
            format(nrow(sd_panel), big.mark = ","),
            uniqueN(sd_panel$sd_id)))
cat(sprintf("Mean munis per SD: %.1f\n",
            mean(sd_panel$n_munis_in_sd, na.rm = TRUE)))

# Merge in SD-level fiscal share controls (Census F-33; built by
# build_sd_controls.R). Parallel to the muni AFR shares used in the
# cousub-level model but for school districts.
sd_ctrls_path <- file.path(data_dir, "pa_sd_controls.rds")
if (file.exists(sd_ctrls_path)) {
  sd_ctrl <- as.data.table(readRDS(sd_ctrls_path))
  sd_ctrl_cols <- c("sd_id","year",
                    "share_state_rev_sd","share_local_rev_sd",
                    "share_federal_rev_sd","share_property_tax_sd",
                    "share_instructional_sd","share_support_sd",
                    "share_capital_sd","share_debt_int_sd")
  sd_ctrl_cols <- intersect(sd_ctrl_cols, names(sd_ctrl))
  sd_ctrl <- unique(sd_ctrl[, ..sd_ctrl_cols])
  sd_panel <- merge(sd_panel, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)
  cat(sprintf("Merged SD fiscal controls: %d / %d SD-years matched\n",
              sum(!is.na(sd_panel$share_state_rev_sd)),
              nrow(sd_panel)))
} else {
  cat("WARNING: data/pa_sd_controls.rds not found; SD fiscal controls skipped.\n")
}

# ---------------------------------------------------------------------------
# 3. SD treatment cells (binding-state classification)
# ---------------------------------------------------------------------------
# SD-level binding-state classification.
# Home stack for an SD's typical resident = (mean muni resident rate within
# the SD) + (SD resident rate). Work-side nonresident comparator = within-SD
# mean of muni_nonres_eit (the actual nonresident-rate field, aggregated over
# the SD's constituent munis). The earlier version of this script used
# mean_muni_rate (i.e., the resident rate) as a proxy for the nonres rate;
# that proxy is now retired -- we aggregate the institutional nonres field
# directly. Most PA workers commute within or close to their home SD, so a
# within-SD mean of muni_nonres_eit is a sensible approximation of the
# "typical worker's" work-side nonres rate at the SD aggregation level.
sd_panel[, home_stack := mean_muni_rate + sd_eit_rate]
sd_panel[, work_nonres_avg := mean_muni_nonres]
sd_panel[, sd_home_binds := as.integer(home_stack > work_nonres_avg)]

setorder(sd_panel, sd_id, year)
sd_panel[, lag_sd := shift(sd_eit_rate), by = sd_id]
sd_panel[, sd_event := fifelse(is.na(lag_sd), NA_character_,
                        fifelse(sd_eit_rate > lag_sd + 1e-6, "Inc",
                        fifelse(sd_eit_rate < lag_sd - 1e-6, "Dec", NA_character_)))]
ev <- sd_panel[!is.na(sd_event), .SD[1], by = sd_id,
               .SDcols = c("year","sd_event","lag_sd","sd_eit_rate","sd_home_binds")]
setnames(ev, c("year","sd_event","lag_sd","sd_eit_rate","sd_home_binds"),
         c("c_sd","ev_dir","baseline_sd","first_post","postbind"))
pbind <- sd_panel[, .(sd_id, year, lag_home_binds = shift(sd_home_binds)), by = sd_id]
ev <- merge(ev, pbind[, .(sd_id, year, lag_home_binds)],
            by.x = c("sd_id","c_sd"), by.y = c("sd_id","year"), all.x = TRUE)
ev[, prebind := lag_home_binds]; ev[, lag_home_binds := NULL]
ev[, cell_sd := fcase(
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
ev <- ev[, .(sd_id, c_sd, baseline_sd, cell_sd)]
sd_panel <- merge(sd_panel, ev, by = "sd_id", all.x = TRUE)

# NC fix: truly stable SDs (no rate change ever)
rate_changes_sd <- sd_panel[!is.na(sd_eit_rate),
                              .(n_changes = sum(abs(diff(sd_eit_rate)) > 1e-6, na.rm = TRUE)),
                              by = sd_id]
truly_stable_sd <- rate_changes_sd[n_changes == 0, sd_id]
sd_panel[sd_id %in% truly_stable_sd, cell_sd := "NC"]
sd_panel[!sd_id %in% truly_stable_sd & is.na(cell_sd), cell_sd := "Uncertain"]
sd_panel[, dose_sd := fifelse(!is.na(c_sd) & year >= c_sd, sd_eit_rate - baseline_sd, 0)]

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind",
                 "Inc-loses","Dec-gains","Uncertain")
sd_panel[, cell_sd := factor(cell_sd, levels = cell_levels)]

# Drop Uncertain
ucs <- unique(sd_panel[cell_sd == "Uncertain", sd_id])
cat(sprintf("\nDropping %d uncertain SDs\n", length(ucs)))
sd_panel <- sd_panel[!sd_id %in% ucs]
sd_panel[, cell_sd := droplevels(cell_sd)]

cat("\nSD cell composition (unique SDs):\n")
print(unique(sd_panel[, .(sd_id, cell_sd)])[, .N, by = cell_sd][order(cell_sd)])

# ---------------------------------------------------------------------------
# 4. Build stacked panel for SD side
# ---------------------------------------------------------------------------
build_stack_sd <- function(p, K = K_stack) {
  treated <- unique(p[!is.na(c_sd) & cell_sd != "NC",
                       .(sd_id, cohort = c_sd, cell = cell_sd)])
  nc_sds <- unique(p[cell_sd == "NC", sd_id])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_sds <- treated[cohort == cy]
    if (nrow(t_sds) == 0) return(NULL)
    d_t <- p[sd_id %in% t_sds$sd_id & year %in% win]
    d_t <- merge(d_t, t_sds[, .(sd_id, treat_cell = cell)], by = "sd_id", all.x = TRUE)
    d_c <- p[sd_id %in% nc_sds & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }), fill = TRUE)
}
stk_sd <- build_stack_sd(sd_panel)
stk_sd[, stack_sd := paste0(stack, "_", sd_id)]
stk_sd[, stack_year := paste0(stack, "_", year)]
stk_sd[, base_rate := mean(dose_sd[rel < 0], na.rm = TRUE), by = .(stack, sd_id)]
stk_sd[!is.finite(base_rate), base_rate := 0]
stk_sd[, dose_stack := dose_sd - base_rate]
stk_sd[, treat_cell := factor(treat_cell, levels = cell_levels)]

cat(sprintf("\nStacked panel: %s SD-stack-year obs, %d unique SDs\n",
            format(nrow(stk_sd), big.mark = ","),
            uniqueN(stk_sd$sd_id)))

# ---------------------------------------------------------------------------
# 5. Run stacked-continuous DiD with mean_muni_rate as a control
# ---------------------------------------------------------------------------
outcomes <- c("log_live_and_work","log_in_commuters","log_out_commuters",
              "log_total_jobs","log_total_residents",
              "share_live_in","share_work_outside","share_nonres")

sd_share_ctrls <- intersect(c("share_state_rev_sd","share_local_rev_sd",
                                "share_federal_rev_sd","share_property_tax_sd",
                                "share_instructional_sd","share_support_sd",
                                "share_capital_sd","share_debt_int_sd"),
                             names(sd_panel))

run_sd_headline <- function(stk, outcome, ctrls_str) {
  needed_cols <- unique(c("sd_id","year","treat_cell","dose_stack","stack_sd",
                "stack_year","mean_muni_rate","pop_weighted_muni_rate",
                outcome, sd_share_ctrls))
  needed_cols <- intersect(needed_cols, names(stk))
  d <- stk[, ..needed_cols]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_stack, ref='NC') + ",
       ctrls_str,
       " | stack_sd + stack_year"))
  feols(f, data = d, cluster = ~sd_id)
}

# Headline controls: mean muni rate (within-SD aggregate of muni rates,
# the SD-level analog of the cousub model's SD-rate-as-control) plus
# F-33 SD-level fiscal share controls.
ctrls_sd <- paste(c("mean_muni_rate", sd_share_ctrls), collapse = " + ")
ctrls_sd_pop <- paste(c("pop_weighted_muni_rate", sd_share_ctrls), collapse = " + ")

cat("\n--- SD-level stacked-continuous DiD (control: mean muni rate) ---\n")
results <- list()
diags <- list()
for (o in outcomes) {
  m <- run_sd_headline(stk_sd, o, ctrls_sd)
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("treat_cell::", term)]
  ct[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]
  results[[o]] <- ct[, .(outcome, cell, est = round(est, 3),
                          se = round(se, 3), p = round(p, 4), n)]
  diags[[o]] <- data.table(
    outcome = o, n = nobs(m),
    R2 = round(fitstat(m, "r2", simplify = TRUE), 4),
    Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
    dep_mean = round(mean(m$residuals + m$fitted.values, na.rm = TRUE), 4),
    dep_sd = round(sd(m$residuals + m$fitted.values, na.rm = TRUE), 4)
  )
}
res_dt <- rbindlist(results, fill = TRUE)
res_dt[, sig := fifelse(p < 0.01, "***",
                fifelse(p < 0.05, "**",
                fifelse(p < 0.10, "*", "")))]
fwrite(res_dt, file.path(out_dir, "sd_level_results.csv"))
diag_dt <- rbindlist(diags, fill = TRUE)
fwrite(diag_dt, file.path(out_dir, "sd_level_diagnostics.csv"))

cat("\n--- SD-level Inc cells, all outcomes ---\n")
print(res_dt[cell %in% c("Inc-bind","Inc-becomes","Inc-nobind")])

cat("\n--- Diagnostics ---\n")
print(diag_dt)

cat("\nSaved data/q3_unified/sd_level_results.csv\n")
cat("Saved data/q3_unified/sd_level_diagnostics.csv\n")
