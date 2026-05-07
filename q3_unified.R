# q3_unified.R
# Unified muni-level decomposition.
#
# ONE panel: muni x year.
# TWO treatments in each regression: cumulative resident-rate change and
#   cumulative nonres-rate change, each interacted with a 7-cell classification
#   {NC, Inc-bind, Inc-becomes, Inc-nobind, Dec-bind, Dec-becomes-non, Dec-nobind}.
#   Two rare residual cells (Inc-loses, Dec-gains) reported but expected thin.
# EIGHT outcomes per regression: log(live_and_work), log(in_commuters),
#   log(out_commuters), log(total_jobs), log(total_residents),
#   share_live_in, share_work_outside, share_nonres.
# FIVE estimators: Q2 TWFE pooled, single-event TWFE, DCDH, stacked binary,
#   stacked continuous.
# THREE samples: strict (drop NA nonres), filled (impute nonres = resident_eit),
#   drop-incomplete (all years, drop rows missing any needed var at run time).
#
# Output: data/q3_unified/<estimator>_<sample>.csv
# Each CSV: rows = outcomes, columns = (cell_res x stat) | (cell_nr x stat).

suppressMessages({
  library(data.table)
  library(fixest)
  library(DIDmultiplegtDYN)
})

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
dir.create(out_dir, showWarnings = FALSE)

max_year <- 2019
K_stack  <- 5L  # stacked-DiD half-window

# ---------------------------------------------------------------------------
# 1. Load and merge into the unified panel
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
                  .(muni = cousub_fips, year,
                    resident_eit, sd_eit, muni_nonres_eit)]
# Some munis have multiple rate rows per (muni, year) due to multi-SD; collapse
rate_keep <- rate_keep[, .(resident_eit = mean(resident_eit, na.rm = TRUE),
                           sd_eit       = max(sd_eit, na.rm = TRUE),
                           muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                       by = .(muni, year)]
rate_keep[!is.finite(resident_eit),    resident_eit    := NA_real_]
rate_keep[!is.finite(sd_eit),          sd_eit          := NA_real_]
rate_keep[!is.finite(muni_nonres_eit), muni_nonres_eit := NA_real_]
panel <- merge(panel, rate_keep, by = c("muni", "year"), all.x = TRUE)
panel[, home_stack := resident_eit + sd_eit]

# Outcomes (rename for clarity, build logs and shares)
panel[, in_commuters    := live_outside]                # workers from elsewhere
panel[, out_commuters   := residents_work_outside]      # residents commuting out
panel[, total_residents := residents_employed]

log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
panel[, log_live_and_work   := log_pos(live_and_work)]
panel[, log_in_commuters    := log_pos(in_commuters)]
panel[, log_out_commuters   := log_pos(out_commuters)]
panel[, log_total_jobs      := log_pos(total_jobs)]
panel[, log_total_residents := log_pos(total_residents)]
panel[, share_nonres := ifelse(total_jobs > 0, in_commuters / total_jobs, NA_real_)]

# Fiscal controls
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
                               `intergovernmental_revenues-state_government`,
                               state_govt))
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
       # Expenditure shares (denominator = total expenditures)
       share_police    = ifelse(total_exp > 0, police_exp  / total_exp, NA_real_),
       share_streets   = ifelse(total_exp > 0, streets_exp / total_exp, NA_real_),
       share_debt_svc  = ifelse(total_exp > 0, debt_svc    / total_exp, NA_real_),
       # Revenue shares (denominator = total revenues)
       share_eit       = ifelse(total_rev > 0, eit_rev  / total_rev, NA_real_),
       share_re        = ifelse(total_rev > 0, re_tax   / total_rev, NA_real_),
       share_intergov  = ifelse(total_rev > 0, intergov / total_rev, NA_real_))
}]
fc <- fc[!is.na(muni) & !is.na(year)]
fc <- fc[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
         by = .(muni, year)]
panel <- merge(panel, fc, by = c("muni", "year"), all.x = TRUE)

# Dedupe (muni, year) — pa_panel and pa_residence_panel have a handful of dup
# rows from upstream multi-SD joins; collapse by mean for numerics, first for chars
panel <- panel[, lapply(.SD, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)
}), by = .(muni, year)]
for (col in names(panel)) {
  if (is.numeric(panel[[col]])) {
    set(panel, which(!is.finite(panel[[col]])), col, NA_real_)
  }
}

# Useful constants
outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
              "log_total_jobs", "log_total_residents",
              "share_live_in", "share_work_outside", "share_nonres")
ctrls <- c("share_police", "share_streets", "share_debt_svc",
           "share_eit", "share_re", "share_intergov")

# ---------------------------------------------------------------------------
# 2. Build the three sample variants
# ---------------------------------------------------------------------------
panel_strict <- panel[!is.na(muni_nonres_eit)]
panel_filled <- copy(panel)
panel_filled[is.na(muni_nonres_eit), muni_nonres_eit := resident_eit]
panel_drop   <- copy(panel)  # drop missing rows at regression time per outcome

samples <- list(strict = panel_strict, filled = panel_filled, drop = panel_drop)

# ---------------------------------------------------------------------------
# 3. Classify binding states + first event per side, build cell labels and dose
# ---------------------------------------------------------------------------
classify_events <- function(p) {
  p <- copy(p)
  setorder(p, muni, year)
  # Binding state per row
  p[, home_binds := as.integer(home_stack    > muni_nonres_eit)]
  p[, nr_binds   := as.integer(muni_nonres_eit > home_stack)]

  # First resident-rate event per muni
  p[, lag_res   := shift(resident_eit), by = muni]
  p[, res_event := fifelse(is.na(lag_res), NA_character_,
                  fifelse(resident_eit > lag_res + 1e-6, "Inc",
                  fifelse(resident_eit < lag_res - 1e-6, "Dec", NA_character_)))]
  ev_res <- p[!is.na(res_event), .SD[1],
              by = muni,
              .SDcols = c("year", "res_event", "lag_res", "resident_eit",
                          "home_binds", "nr_binds")]
  setnames(ev_res,
           c("year","lag_res","resident_eit","home_binds","nr_binds"),
           c("c_res","baseline_res","first_post_res","postbind_res_home","postbind_res_nr"))
  # Pre-event binding state: look up the row at year c_res - 1
  prev_bind_res <- p[!is.na(lag_res),
                     .(muni, year, prebind_res_home = shift(home_binds, type="lag"))][
                     , .SD[1], by = muni]
  # Simpler: just merge lagged home_binds into ev_res
  pbind <- p[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
  ev_res <- merge(ev_res, pbind[, .(muni, year, lag_home_binds)],
                  by.x = c("muni", "c_res"), by.y = c("muni", "year"), all.x = TRUE)
  ev_res[, prebind_res_home := lag_home_binds]
  ev_res[, lag_home_binds := NULL]
  ev_res[, cell_res := fcase(
    res_event == "Inc" & prebind_res_home == 1 & postbind_res_home == 1, "Inc-bind",
    res_event == "Inc" & prebind_res_home == 0 & postbind_res_home == 1, "Inc-becomes",
    res_event == "Inc" & prebind_res_home == 0 & postbind_res_home == 0, "Inc-nobind",
    res_event == "Inc" & prebind_res_home == 1 & postbind_res_home == 0, "Inc-loses",
    res_event == "Dec" & prebind_res_home == 1 & postbind_res_home == 1, "Dec-bind",
    res_event == "Dec" & prebind_res_home == 1 & postbind_res_home == 0, "Dec-becomes-non",
    res_event == "Dec" & prebind_res_home == 0 & postbind_res_home == 0, "Dec-nobind",
    res_event == "Dec" & prebind_res_home == 0 & postbind_res_home == 1, "Dec-gains",
    default = NA_character_
  )]

  # First nonres-rate event per muni
  p[, lag_nr := shift(muni_nonres_eit), by = muni]
  p[, nr_event := fifelse(is.na(lag_nr), NA_character_,
                  fifelse(muni_nonres_eit > lag_nr + 1e-6, "Inc",
                  fifelse(muni_nonres_eit < lag_nr - 1e-6, "Dec", NA_character_)))]
  ev_nr <- p[!is.na(nr_event), .SD[1],
             by = muni,
             .SDcols = c("year","nr_event","lag_nr","muni_nonres_eit",
                         "home_binds","nr_binds")]
  setnames(ev_nr,
           c("year","lag_nr","muni_nonres_eit","home_binds","nr_binds"),
           c("c_nr","baseline_nr","first_post_nr","postbind_nr_home","postbind_nr_nr"))
  pbind_nr <- p[, .(muni, year, lag_nr_binds = shift(nr_binds)), by = muni]
  ev_nr <- merge(ev_nr, pbind_nr[, .(muni, year, lag_nr_binds)],
                 by.x = c("muni","c_nr"), by.y = c("muni","year"), all.x = TRUE)
  ev_nr[, prebind_nr := lag_nr_binds]; ev_nr[, lag_nr_binds := NULL]
  ev_nr[, postbind_nr := postbind_nr_nr]
  ev_nr[, cell_nr := fcase(
    nr_event == "Inc" & prebind_nr == 1 & postbind_nr == 1, "Inc-bind",
    nr_event == "Inc" & prebind_nr == 0 & postbind_nr == 1, "Inc-becomes",
    nr_event == "Inc" & prebind_nr == 0 & postbind_nr == 0, "Inc-nobind",
    nr_event == "Inc" & prebind_nr == 1 & postbind_nr == 0, "Inc-loses",
    nr_event == "Dec" & prebind_nr == 1 & postbind_nr == 1, "Dec-bind",
    nr_event == "Dec" & prebind_nr == 1 & postbind_nr == 0, "Dec-becomes-non",
    nr_event == "Dec" & prebind_nr == 0 & postbind_nr == 0, "Dec-nobind",
    nr_event == "Dec" & prebind_nr == 0 & postbind_nr == 1, "Dec-gains",
    default = NA_character_
  )]

  # Merge cell labels back to panel
  p2 <- merge(p, ev_res[, .(muni, c_res, baseline_res, cell_res)],
              by = "muni", all.x = TRUE)
  p2 <- merge(p2, ev_nr[, .(muni, c_nr, baseline_nr, cell_nr)],
              by = "muni", all.x = TRUE)
  p2[is.na(cell_res), cell_res := "NC"]
  p2[is.na(cell_nr),  cell_nr  := "NC"]

  # Cumulative dose (option b): post-event signed change from baseline
  p2[, dose_res := fifelse(!is.na(c_res) & year >= c_res,
                           resident_eit - baseline_res, 0)]
  p2[, dose_nr  := fifelse(!is.na(c_nr)  & year >= c_nr,
                           muni_nonres_eit - baseline_nr, 0)]
  p2[, post_res := as.integer(!is.na(c_res) & year >= c_res)]
  p2[, post_nr  := as.integer(!is.na(c_nr)  & year >= c_nr)]
  p2[, rel_res  := fifelse(!is.na(c_res), year - c_res, NA_integer_)]
  p2[, rel_nr   := fifelse(!is.na(c_nr),  year - c_nr,  NA_integer_)]
  p2
}

samples <- lapply(samples, classify_events)

# Cell levels (factor with NC reference)
cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                 "Dec-bind", "Dec-becomes-non", "Dec-nobind",
                 "Inc-loses", "Dec-gains")
for (s in names(samples)) {
  samples[[s]][, cell_res := factor(cell_res, levels = cell_levels)]
  samples[[s]][, cell_nr  := factor(cell_nr,  levels = cell_levels)]
}

# Print cell counts
for (s in names(samples)) {
  cat(sprintf("\n=== Sample: %s ===\n", s))
  cat("cell_res counts (unique munis):\n")
  print(unique(samples[[s]][, .(muni, cell_res)])[, .N, by = cell_res][order(cell_res)])
  cat("cell_nr counts (unique munis):\n")
  print(unique(samples[[s]][, .(muni, cell_nr)])[, .N, by = cell_nr][order(cell_nr)])
}

# ---------------------------------------------------------------------------
# 4. Estimator helpers
# ---------------------------------------------------------------------------
# All estimators output a tidy data.table:
# outcome | side | cell | est | se | p | n
extract_cell_coefs <- function(model, side_label) {
  ct <- as.data.table(model$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"), c("est", "se", "p"),
           skip_absent = TRUE)
  pat <- if (side_label == "res") "cell_res::" else "cell_nr::"
  ct <- ct[grepl(pat, term)]
  ct[, cell := sub(paste0("^.*", pat, "([^:]+):.*$"), "\\1", term)]
  ct[, side := side_label]
  ct[, n := nobs(model)]
  ct[, .(side, cell, est, se, p, n)]
}

ctrls_str <- paste(ctrls, collapse = " + ")

# Drop rows missing controls or outcome at run time per outcome
clean_for <- function(p, outcome) {
  needed <- c(outcome, ctrls, "cell_res", "cell_nr",
              "dose_res", "dose_nr", "post_res", "post_nr",
              "muni", "year")
  p2 <- p[, ..needed]
  p2 <- p2[complete.cases(p2)]
  p2
}

run_twfe_pooled <- function(p, outcome) {
  d <- clean_for(p, outcome)
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome, " ~ i(cell_res, dose_res, ref='NC') + ",
                                "i(cell_nr, dose_nr, ref='NC') + ",
                                ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}

run_twfe_single <- function(p, outcome) {
  # Restrict to munis with at most one event on each side
  ev_counts <- p[, .(n_res_ev = uniqueN(year[!is.na(c_res) & year >= c_res &
                                       abs(resident_eit - shift(resident_eit, fill = first(resident_eit))) > 1e-6]),
                     n_nr_ev  = uniqueN(year[!is.na(c_nr)  & year >= c_nr  &
                                       abs(muni_nonres_eit - shift(muni_nonres_eit, fill = first(muni_nonres_eit))) > 1e-6])),
                 by = muni]
  # Simpler restriction: flag muni if it has more than 1 distinct change on a side
  count_changes <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) return(0L)
    sum(abs(diff(x)) > 1e-6)
  }
  ec <- p[, .(n_res = count_changes(resident_eit),
              n_nr  = count_changes(muni_nonres_eit)), by = muni]
  # Loosen to <=2 events per side: <=1 leaves Inc-nobind effectively unidentified
  keep <- ec[n_res <= 2 & n_nr <= 2, muni]
  d <- clean_for(p[muni %in% keep], outcome)
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome, " ~ i(cell_res, dose_res, ref='NC') + ",
                                "i(cell_nr, dose_nr, ref='NC') + ",
                                ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}

# Stacked-DiD: build sub-experiments per (cell, cohort year) on each side.
# We do TWO stacked panels (one per side) because mixing two treatments in
# a stacked design gets messy. Each side returns its own cell coefficients.
build_stack_one_side <- function(p, side) {
  cell_col   <- if (side == "res") "cell_res" else "cell_nr"
  cohort_col <- if (side == "res") "c_res"    else "c_nr"
  dose_col   <- if (side == "res") "dose_res" else "dose_nr"
  rel_col    <- if (side == "res") "rel_res"  else "rel_nr"
  treated_munis <- unique(p[!is.na(get(cohort_col)) & get(cell_col) != "NC",
                            .(muni, cohort = get(cohort_col), cell = get(cell_col))])
  nc_munis <- unique(p[get(cell_col) == "NC", muni])
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
    d[, stack    := cy]
    d[, rel      := year - cy]
    d[, treat    := treat_cell != "NC"]
    d[, post     := as.integer(rel >= 0)]
    d[, treat_post := as.integer(treat & post == 1)]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  if (nrow(stk) == 0) return(NULL)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  # Per-stack-muni dose: post-period continuous Δτ relative to pre-window
  stk[, base_rate_stack := mean(get(dose_col)[rel < 0], na.rm = TRUE),
      by = .(stack, muni)]
  stk[!is.finite(base_rate_stack), base_rate_stack := 0]
  stk[, dose_stack := get(dose_col) - base_rate_stack]
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  stk
}

run_stacked_binary <- function(p, outcome, side) {
  stk <- build_stack_one_side(p, side)
  if (is.null(stk) || nrow(stk) == 0) return(NULL)
  stk2 <- stk[, c("muni", "year", "treat_cell", "treat_post", "stack_muni",
                  "stack_year", outcome, ctrls), with = FALSE]
  stk2 <- stk2[complete.cases(stk2)]
  if (nrow(stk2) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, treat_post, ref='NC') + ", ctrls_str,
       " | stack_muni + stack_year"))
  feols(f, data = stk2, cluster = ~muni)
}

run_stacked_continuous <- function(p, outcome, side) {
  stk <- build_stack_one_side(p, side)
  if (is.null(stk) || nrow(stk) == 0) return(NULL)
  stk2 <- stk[, c("muni", "year", "treat_cell", "dose_stack", "stack_muni",
                  "stack_year", outcome, ctrls), with = FALSE]
  stk2 <- stk2[complete.cases(stk2)]
  if (nrow(stk2) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_stack, ref='NC') + ", ctrls_str,
       " | stack_muni + stack_year"))
  feols(f, data = stk2, cluster = ~muni)
}

# DCDH cell-by-cell: for each non-NC cell on a side, restrict treated to that
# cell's munis and controls to NC, run did_multiplegt_dyn, extract ATE.
run_dcdh_cell <- function(p, outcome, side, target_cell) {
  cell_col <- if (side == "res") "cell_res" else "cell_nr"
  treat_col <- if (side == "res") "resident_eit" else "muni_nonres_eit"
  d <- p[get(cell_col) %in% c("NC", target_cell)]
  # Save cell labels BEFORE subsetting columns
  cell_vec <- as.character(d[[cell_col]])
  needed <- c(outcome, treat_col, "muni", "year", ctrls)
  d <- d[, ..needed]
  d[, cell_lbl := cell_vec]
  d <- d[complete.cases(d[, .SD, .SDcols = c(outcome, treat_col, "muni", "year")])]
  if (nrow(d) == 0 || uniqueN(d[cell_lbl != "NC", muni]) < 2) return(NULL)
  d[, muni_int := as.integer(as.factor(muni))]
  res <- tryCatch(suppressMessages(did_multiplegt_dyn(
    df = as.data.frame(d), outcome = outcome,
    group = "muni_int", time = "year",
    treatment = treat_col,
    effects = 3, placebo = 2, continuous = 1,
    cluster = "muni_int", graph_off = TRUE
  )), error = function(e) {message("DCDH error: ", conditionMessage(e)); NULL})
  if (is.null(res) || is.null(res$results$ATE)) return(NULL)
  ate <- res$results$ATE
  data.table(side = side, cell = target_cell,
             est = as.numeric(ate[1, "Estimate"]),
             se  = as.numeric(ate[1, "SE"]),
             p = NA_real_, n = as.numeric(ate[1, "N"]))
}

# ---------------------------------------------------------------------------
# 5. Run estimators across (sample, estimator, outcome)
# ---------------------------------------------------------------------------
non_nc_cells <- setdiff(cell_levels, "NC")

results_long <- list()

for (s_name in names(samples)) {
  cat(sprintf("\n\n========== SAMPLE: %s ==========\n", s_name))
  p <- samples[[s_name]]

  # --- Q2 TWFE pooled ---
  cat("\n-- Q2 TWFE pooled --\n")
  for (o in outcomes) {
    m <- run_twfe_pooled(p, o)
    if (is.null(m)) next
    rr <- rbind(extract_cell_coefs(m, "res"), extract_cell_coefs(m, "nr"))
    rr[, `:=`(estimator = "twfe_pooled", sample = s_name, outcome = o)]
    results_long[[length(results_long) + 1L]] <- rr
  }

  # --- Single-event TWFE ---
  cat("\n-- Single-event TWFE --\n")
  for (o in outcomes) {
    m <- run_twfe_single(p, o)
    if (is.null(m)) next
    rr <- rbind(extract_cell_coefs(m, "res"), extract_cell_coefs(m, "nr"))
    rr[, `:=`(estimator = "twfe_single", sample = s_name, outcome = o)]
    results_long[[length(results_long) + 1L]] <- rr
  }

  # --- Stacked binary, per side ---
  cat("\n-- Stacked binary --\n")
  for (o in outcomes) {
    for (sd in c("res", "nr")) {
      m <- run_stacked_binary(p, o, sd)
      if (is.null(m)) next
      rr <- extract_cell_coefs(m, sd)
      # The factor name in stacked is treat_cell, not cell_res/cell_nr; fix
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
               skip_absent = TRUE)
      ct <- ct[grepl("treat_cell::", term)]
      ct[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]
      ct[, side := sd]; ct[, n := nobs(m)]
      ct <- ct[, .(side, cell, est, se, p, n)]
      ct[, `:=`(estimator = "stacked_binary", sample = s_name, outcome = o)]
      results_long[[length(results_long) + 1L]] <- ct
    }
  }

  # --- Stacked continuous, per side ---
  cat("\n-- Stacked continuous --\n")
  for (o in outcomes) {
    for (sd in c("res", "nr")) {
      m <- run_stacked_continuous(p, o, sd)
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
               skip_absent = TRUE)
      ct <- ct[grepl("treat_cell::", term)]
      ct[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]
      ct[, side := sd]; ct[, n := nobs(m)]
      ct <- ct[, .(side, cell, est, se, p, n)]
      ct[, `:=`(estimator = "stacked_continuous", sample = s_name, outcome = o)]
      results_long[[length(results_long) + 1L]] <- ct
    }
  }

  # --- DCDH cell-by-cell (drop sample only, 2 outcomes, 5 cells) ---
  if (s_name != "drop") next
  cat("\n-- DCDH (drop sample only, headline cells) --\n")
  dcdh_outcomes <- c("log_live_and_work", "log_in_commuters")
  dcdh_cells <- list(
    res = c("Inc-bind", "Inc-becomes", "Inc-nobind"),
    nr  = c("Inc-nobind", "Dec-nobind")
  )
  for (o in dcdh_outcomes) {
    for (sd in c("res", "nr")) {
      for (cell in dcdh_cells[[sd]]) {
        cat(sprintf("  DCDH: %s %s %s ... ", sd, cell, o))
        rr <- run_dcdh_cell(p, o, sd, cell)
        if (is.null(rr)) { cat("skipped\n"); next }
        cat(sprintf("est=%.3f se=%.3f n=%d\n", rr$est, rr$se, rr$n))
        rr[, `:=`(estimator = "dcdh", sample = s_name, outcome = o)]
        results_long[[length(results_long) + 1L]] <- rr
      }
    }
  }
}

results <- rbindlist(results_long, fill = TRUE)
fwrite(results, file.path(out_dir, "results_long.csv"))
cat(sprintf("\nSaved long results: %s rows -> %s\n",
            format(nrow(results), big.mark = ","),
            file.path(out_dir, "results_long.csv")))

# ---------------------------------------------------------------------------
# 6. Build per-estimator wide tables (rows = outcomes, cols = side x cell)
# ---------------------------------------------------------------------------
results[, sig := fifelse(p < 0.01, "***",
                  fifelse(p < 0.05, "**",
                  fifelse(p < 0.10, "*", "")))]
results[, est_str := sprintf("%.3f%s", est, ifelse(is.na(sig), "", sig))]

for (est_name in unique(results$estimator)) {
  for (s_name in unique(results$sample)) {
    sub <- results[estimator == est_name & sample == s_name]
    if (nrow(sub) == 0) next
    sub[, side_cell := paste0(side, "_", cell)]
    wide <- dcast(sub, outcome ~ side_cell, value.var = "est_str")
    fwrite(wide, file.path(out_dir, sprintf("table_%s_%s.csv", est_name, s_name)))
  }
}


# ---------------------------------------------------------------------------
# 7. Event studies (Sun-Abraham per cell on the headline sample = drop)
# ---------------------------------------------------------------------------
cat("\n\n========== EVENT STUDIES (Sun-Abraham per cell) ==========\n")

es_outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs",
                 "log_total_residents", "share_live_in", "share_work_outside")
es_cells_res <- c("Inc-bind", "Inc-becomes", "Inc-nobind",
                  "Dec-bind", "Dec-nobind")
es_cells_nr  <- c("Inc-nobind", "Dec-nobind")

run_sa_es <- function(p, outcome, side, target_cell, K = 5) {
  cell_col   <- if (side == "res") "cell_res" else "cell_nr"
  cohort_col <- if (side == "res") "c_res"    else "c_nr"
  d <- p[get(cell_col) %in% c("NC", target_cell)]
  d[, cohort := as.integer(get(cohort_col))]
  d[is.na(cohort), cohort := 10000L]
  needed <- c(outcome, "muni", "year", "cohort", ctrls)
  d <- d[, ..needed]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0 || sum(d$cohort != 10000L) < 5) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ sunab(cohort, year) + ", ctrls_str,
       " | muni + year"))
  m <- tryCatch(feols(f, data = d, cluster = ~muni),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  is_sa <- grepl("^year::-?[[:digit:]]+$", ct$term)
  ct <- ct[is_sa]
  ct[, rel_year := as.integer(sub("^year::(-?[[:digit:]]+)$", "\\1", term))]
  ct <- ct[abs(rel_year) <= K]
  # Add reference period
  ct <- rbind(ct,
              data.table(term="ref", est=0, se=0, p=NA_real_,
                         rel_year=-1L), fill=TRUE)
  ct[, `:=`(side = side, cell = target_cell, outcome = outcome)]
  ct[order(rel_year)]
}

es_results <- list()
p_drop <- samples[["drop"]]
for (o in es_outcomes) {
  for (cell in es_cells_res) {
    rr <- run_sa_es(p_drop, o, "res", cell)
    if (!is.null(rr)) es_results[[length(es_results) + 1L]] <- rr
  }
  for (cell in es_cells_nr) {
    rr <- run_sa_es(p_drop, o, "nr", cell)
    if (!is.null(rr)) es_results[[length(es_results) + 1L]] <- rr
  }
}
es_dt <- rbindlist(es_results, fill = TRUE)
fwrite(es_dt, file.path(out_dir, "event_studies.csv"))
cat(sprintf("Saved event studies: %s rows -> %s\n",
            format(nrow(es_dt), big.mark = ","),
            file.path(out_dir, "event_studies.csv")))

# --- Plot event studies (resident-side, core outcomes) ---
suppressMessages({library(ggplot2)})
fig_dir <- file.path(project_dir, "figures")
muni_red <- "#C0392B"; muni_blue <- "#2C7FB8"; muni_grey <- "#7F8C8D"

cell_colors <- c("Inc-bind"    = muni_red,
                 "Inc-becomes" = "#E67E22",
                 "Inc-nobind"  = muni_grey,
                 "Dec-bind"    = muni_blue,
                 "Dec-nobind"  = "#85C1E9")

es_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

plot_es <- function(out, side_label, cells_use, fname) {
  d <- es_dt[outcome == out & side == "res" & cell %in% cells_use]
  if (nrow(d) == 0) return(invisible(NULL))
  d[, cell := factor(cell, levels = cells_use)]
  p <- ggplot(d, aes(rel_year, est, color = cell, group = cell)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
    geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se),
                  width = 0, linewidth = 0.4) +
    geom_line(linewidth = 0.5) + geom_point(size = 1.6) +
    scale_color_manual(values = cell_colors) +
    labs(title = sprintf("Event study: %s by binding cell", out),
         subtitle = sprintf("Resident-side cells (Sun-Abraham, drop sample); 95%% CI"),
         x = "Years relative to first event",
         y = "Coefficient", color = NULL) +
    es_theme
  ggsave(file.path(fig_dir, fname), p, width = 9, height = 4.5, dpi = 300)
  cat("Saved:", fname, "\n")
}

inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
dec_cells <- c("Dec-bind", "Dec-nobind")

# Resident-side ES figures: Inc cells
for (o in es_outcomes) {
  plot_es(o, "res", inc_cells, sprintf("unified_es_res_inc_%s.png", o))
}
# Resident-side ES figures: Dec cells
for (o in es_outcomes) {
  plot_es(o, "res", dec_cells, sprintf("unified_es_res_dec_%s.png", o))
}

cat("\nDone. Per-estimator tables and ES figures in", out_dir, "and", fig_dir, "\n")
