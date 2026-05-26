# q3_unified_industry.R
# Industry-decomposed unified model.
#
# Loads the industry panel (cousub x year x industry-bucket from pa_industry_panel.rds)
# Reuses cell classification + treatment design from q3_unified_compare.R Mode B
# (muni + SD + nonres separately decomposed by binding-state cells).
# Runs stacked-continuous + Q2 TWFE pooled on industry-bucket outcomes:
#   - log_jobs_total, log_jobs_consumer, log_jobs_tradeable
#   - log_jobs_health_edu, log_jobs_public, log_jobs_other
#   - share_consumer, share_tradeable
#
# Predicted: muni Inc-bind cell hits log_jobs_consumer harder than
# log_jobs_tradeable (local-services-demand spillover signature).

suppressMessages({
  library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
dir.create(out_dir, showWarnings = FALSE)
max_year <- 2019
K_stack <- 5L

# ---------------------------------------------------------------------------
# 1. Load: industry panel + rate panel + fiscal controls
# ---------------------------------------------------------------------------
ind  <- as.data.table(readRDS(file.path(data_dir, "pa_industry_panel.rds")))
ind  <- ind[year <= max_year]
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

rate_keep <- rate[!is.na(cousub_fips) & year <= max_year,
                  .(muni = cousub_fips, year, resident_eit, sd_eit, muni_nonres_eit)]
rate_keep <- rate_keep[, .(resident_eit = mean(resident_eit, na.rm = TRUE),
                           sd_eit       = max(sd_eit, na.rm = TRUE),
                           muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                       by = .(muni, year)]
for (c in c("resident_eit","sd_eit","muni_nonres_eit"))
  rate_keep[!is.finite(get(c)), (c) := NA_real_]
panel <- merge(ind, rate_keep, by = c("muni", "year"), all.x = TRUE)
panel[, home_stack := resident_eit + sd_eit]

afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
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
  police_exp <- as.numeric(ifelse(!is.na(police_expenditures),
                                   police_expenditures, police))
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

outcomes <- c("log_jobs_total", "log_jobs_consumer", "log_jobs_tradeable",
              "log_jobs_health_edu", "log_jobs_public", "log_jobs_other",
              "share_consumer", "share_tradeable")

# ---------------------------------------------------------------------------
# 2. Cell classification (replicates q3_unified_compare.R Mode B)
# ---------------------------------------------------------------------------
classify_treatment <- function(p, rate_var, tag) {
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
  # FIX: True NC = no change in rate_var; Uncertain = had changes but cell unclassifiable
  rate_changes <- p2[!is.na(get(rate_var)), .(
    n = sum(abs(diff(get(rate_var))) > 1e-6, na.rm = TRUE)
  ), by = muni]
  truly_stable <- rate_changes[n == 0, muni]
  p2[muni %in% truly_stable,                            (cell_v) := "NC"]
  p2[!muni %in% truly_stable & is.na(get(cell_v)),     (cell_v) := "Uncertain"]
  p2[, (dose_v) := fifelse(!is.na(get(c_v)) & year >= get(c_v),
                           get(rate_var) - get(base_v), 0)]
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind",
                   "Inc-loses", "Dec-gains", "Uncertain")
  p2[, (cell_v) := factor(get(cell_v), levels = cell_levels)]
  return(p2)
}

panel <- classify_treatment(panel, "resident_eit",   "muni")
panel <- classify_treatment(panel, "sd_eit",          "sd")
panel <- classify_treatment(panel, "muni_nonres_eit", "nr")
# Drop Uncertain munis
u <- Reduce(union, list(unique(panel[cell_muni == "Uncertain", muni]),
                         unique(panel[cell_sd   == "Uncertain", muni]),
                         unique(panel[cell_nr   == "Uncertain", muni])))
cat(sprintf("Industry: dropping %d uncertain munis\n", length(u)))
panel <- panel[!muni %in% u]
panel[, cell_muni := droplevels(cell_muni)]
panel[, cell_sd   := droplevels(cell_sd)]
panel[, cell_nr   := droplevels(cell_nr)]

# Strip Dec munis (paper-wide policy: Dec cells excluded).
dec_cell_set <- c("Dec-bind","Dec-becomes-non","Dec-nobind","Dec-gains")
dec_munis <- Reduce(union, list(
  unique(panel[as.character(cell_muni) %in% dec_cell_set, muni]),
  unique(panel[as.character(cell_sd)   %in% dec_cell_set, muni]),
  unique(panel[as.character(cell_nr)   %in% dec_cell_set, muni])
))
cat(sprintf("Industry: dropping %d Dec munis (paper excludes Dec cells)\n", length(dec_munis)))
panel <- panel[!muni %in% dec_munis]
panel[, cell_muni := droplevels(cell_muni)]
panel[, cell_sd   := droplevels(cell_sd)]
panel[, cell_nr   := droplevels(cell_nr)]

cat("\ncell_muni counts:\n"); print(unique(panel[, .(muni, cell_muni)])[, .N, by = cell_muni][order(cell_muni)])
cat("cell_sd counts:\n");   print(unique(panel[, .(muni, cell_sd)])[, .N, by = cell_sd][order(cell_sd)])
cat("cell_nr counts:\n");   print(unique(panel[, .(muni, cell_nr)])[, .N, by = cell_nr][order(cell_nr)])

# ---------------------------------------------------------------------------
# 3. Estimators
# ---------------------------------------------------------------------------
clean_for <- function(p, outcome, extra_cols) {
  needed <- unique(c(outcome, ctrls, extra_cols, "muni", "year"))
  p2 <- p[, ..needed]
  p2[complete.cases(p2)]
}

extract_cell_coefs <- function(model, side_label) {
  ct <- as.data.table(model$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  pat <- paste0("cell_", side_label, "::")
  ct <- ct[grepl(pat, term)]
  ct[, cell := sub(paste0("^.*", pat, "([^:]+):.*$"), "\\1", term)]
  ct[, side := side_label]; ct[, n := nobs(model)]
  ct[, .(side, cell, est, se, p, n)]
}

run_twfe_pooled <- function(p, outcome) {
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

build_stack_one <- function(p, side_tag, dose_col) {
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
    d[, stack := cy]; d[, rel := year - cy]
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
  stk <- build_stack_one(p, side_tag, dose_col)
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

# ---------------------------------------------------------------------------
# 4. Run
# ---------------------------------------------------------------------------
results_long <- list()

cat("\n-- Q2 TWFE pooled (industry outcomes) --\n")
for (o in outcomes) {
  m <- run_twfe_pooled(panel, o)
  if (is.null(m)) next
  rr <- rbind(extract_cell_coefs(m, "muni"),
              extract_cell_coefs(m, "sd"),
              extract_cell_coefs(m, "nr"))
  rr[, `:=`(estimator = "twfe_pooled", outcome = o)]
  results_long[[length(results_long) + 1L]] <- rr
}

cat("\n-- Stacked continuous (industry outcomes) --\n")
diag_list <- list()
for (sd_tag in c("muni","sd","nr")) {
  dose_col <- paste0("dose_", sd_tag)
  for (o in outcomes) {
    m <- run_stack_cont(panel, o, sd_tag, dose_col)
    if (is.null(m)) next
    rr <- extract_treat_cell(m, sd_tag)
    rr[, `:=`(estimator = "stacked_continuous", outcome = o)]
    results_long[[length(results_long) + 1L]] <- rr
    if (sd_tag == "muni") {
      dep_vec <- m$residuals + m$fitted.values
      diag_list[[o]] <- data.table(
        outcome   = o, n = nobs(m),
        R2        = round(fitstat(m, "r2",  simplify = TRUE), 4),
        Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
        dep_mean  = round(mean(dep_vec, na.rm = TRUE), 4),
        dep_sd    = round(sd(dep_vec,   na.rm = TRUE), 4))
    }
  }
}
if (length(diag_list)) {
  fwrite(rbindlist(diag_list, fill = TRUE),
         file.path(out_dir, "industry_cousub_diag.csv"))
}

results <- rbindlist(results_long, fill = TRUE)
results[, sig := fifelse(p < 0.01, "***",
                  fifelse(p < 0.05, "**",
                  fifelse(p < 0.10, "*", "")))]
results[, est_str := sprintf("%.3f%s", est, ifelse(is.na(sig), "", sig))]
fwrite(results, file.path(out_dir, "industry_results_long.csv"))

for (e in unique(results$estimator)) {
  sub <- results[estimator == e]
  if (nrow(sub) == 0) next
  sub[, side_cell := paste0(side, "_", cell)]
  wide <- dcast(sub, outcome ~ side_cell, value.var = "est_str")
  fwrite(wide, file.path(out_dir, sprintf("industry_table_%s.csv", e)))
  cat("Saved:", sprintf("industry_table_%s.csv", e), "\n")
}

cat("\nDone.\n")
