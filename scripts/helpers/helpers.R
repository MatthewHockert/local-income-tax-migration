# helpers.R
# Code every model and output script shares: it builds the estimation panel,
# and holds the pre-trend tests and the table and figure formatting.

suppressMessages({
  library(dplyr)   # before data.table so data.table's first/last/between win
  library(data.table)
  library(fixest)
  library(ggplot2)
})

# Config and paths ----
source(here::here("scripts", "helpers", "config.R"))

# Merge and filter logging ----
# Wrappers that print what a join or filter did, so no rows vanish without
# a message. Set VERBOSE_MERGES <- FALSE to turn the printing off.
if (!exists("VERBOSE_MERGES")) VERBOSE_MERGES <- TRUE

merge_check <- function(left, right, by, label, all.x = TRUE, expect_min_match = NULL,
                        show_unmatched = 5L, ...) {
  n_rows_before <- nrow(left)
  left_keys  <- unique(left[, ..by])
  right_keys <- unique(right[, ..by])
  n_keys_matched <- nrow(merge(left_keys, right_keys, by = by))
  merged <- merge(left, right, by = by, all.x = all.x, ...)
  match_rate <- if (nrow(left_keys) > 0) n_keys_matched / nrow(left_keys) else NA_real_

  if (isTRUE(VERBOSE_MERGES)) {
    message(sprintf(
      "[merge] %-34s rows %s -> %s | keys %s, matched %s (%.1f%%)",
      label, format(n_rows_before, big.mark = ","), format(nrow(merged), big.mark = ","),
      format(nrow(left_keys), big.mark = ","),
      format(n_keys_matched, big.mark = ","), 100 * match_rate))
    if (nrow(merged) > n_rows_before)
      message(sprintf("[merge]   NOTE: row count GREW by %s -- `by` is not unique in y",
                      format(nrow(merged) - n_rows_before, big.mark = ",")))
    if (n_keys_matched < nrow(left_keys) && show_unmatched > 0) {
      unmatched_keys <- left_keys[!right_keys, on = by]
      examples <- utils::head(unmatched_keys, show_unmatched)
      message(sprintf("[merge]   unmatched keys (first %d of %s): %s",
                      nrow(examples), format(nrow(unmatched_keys), big.mark = ","),
                      paste(apply(examples, 1, paste, collapse = "/"), collapse = ", ")))
    }
  }
  if (!is.null(expect_min_match) && !is.na(match_rate) && match_rate < expect_min_match)
    warning(sprintf("[merge] %s: match rate %.1f%% below expected floor %.1f%%",
                    label, 100 * match_rate, 100 * expect_min_match), call. = FALSE)
  merged
}

filter_check <- function(data, keep, label, unit = NULL) {
  n_rows_before  <- nrow(data)
  n_units_before <- if (!is.null(unit)) uniqueN(data[[unit]]) else NA_integer_
  kept <- data[keep]
  if (isTRUE(VERBOSE_MERGES)) {
    n_units_after <- if (!is.null(unit)) uniqueN(kept[[unit]]) else NA_integer_
    message(sprintf("[filter] %-33s rows %s -> %s (-%s)%s",
      label, format(n_rows_before, big.mark = ","), format(nrow(kept), big.mark = ","),
      format(n_rows_before - nrow(kept), big.mark = ","),
      if (!is.null(unit)) sprintf(" | %s %s -> %s", unit,
        format(n_units_before, big.mark = ","), format(n_units_after, big.mark = ",")) else ""))
  }
  kept
}

# Headline environment ----
# Builds the town-year panel, sorts towns into binding cells and stacks it.
# Every model script starts from this.
build_headline_env <- function(max_year = 2019, K_stack = 5L) {
  work_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_panel.rds")))[year <= max_year]
  home_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_residence_panel.rds")))[year <= max_year]
  rate_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  setnames(work_panel, "work_city", "muni", skip_absent = TRUE)
  setnames(home_panel, "home_city", "muni", skip_absent = TRUE)

  panel <- merge(
    work_panel[, .(muni, year, live_and_work, live_outside, total_jobs, share_live_in)],
    home_panel[, .(muni, year, residents_employed, residents_work_in_own,
                   residents_work_outside, share_work_outside)],
    by = c("muni", "year"), all = TRUE)
  # Tax rates. A Census town can hold more than one DCED town, so their resident
  # and nonresident rates are averaged and the higher school-district rate is kept.
  rates <- rate_panel[!is.na(cousub_fips) & year <= max_year,
                      .(muni = cousub_fips, year, resident_eit, sd_eit, muni_nonres_eit)]
  rates <- rates[, .(resident_eit    = mean(resident_eit, na.rm = TRUE),
                     sd_eit          = max(sd_eit, na.rm = TRUE),
                     muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                 by = .(muni, year)]
  for (rate_col in c("resident_eit", "sd_eit", "muni_nonres_eit"))
    rates[!is.finite(get(rate_col)), (rate_col) := NA_real_]
  panel <- merge_check(panel, rates, by = c("muni", "year"),
                       label = "LODES panel <- DCED EIT rates",
                       expect_min_match = 0.90)
  panel[, home_stack := resident_eit + sd_eit]
  panel[, in_commuters := live_outside]
  panel[, out_commuters := residents_work_outside]
  panel[, total_residents := residents_employed]
  log_pos <- function(value) ifelse(as.numeric(value) > 0, log(as.numeric(value)), NA_real_)
  panel[, log_live_and_work := log_pos(live_and_work)]
  panel[, log_in_commuters  := log_pos(in_commuters)]
  panel[, log_out_commuters := log_pos(out_commuters)]
  panel[, log_total_jobs    := log_pos(total_jobs)]
  panel[, log_total_residents := log_pos(total_residents)]
  panel[, share_nonres := ifelse(total_jobs > 0, in_commuters / total_jobs, NA_real_)]

  # Budget-share controls from the towns' annual financial reports. DCED renamed
  # the columns in 2006, so each line uses the new name and falls back to the old one.
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  muni_xwalk <- unique(rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  setnames(muni_xwalk, "municipality_id", "muni_id")
  afr <- merge_check(afr, muni_xwalk, by = "muni_id",
                     label = "AFR <- DCED muni/cousub xwalk",
                     expect_min_match = 0.95)
  setnames(muni_xwalk, "muni_id", "municipality_id")
  afr[, afr_year := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
  fiscal <- afr[!is.na(cousub_fips), {
    total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
    total_exp <- as.numeric(total_expenditures)
    eit_revenue_amount <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                                            earned_income_tax_revenues, earned_income_tax))
    re_tax_revenue <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                                        real_estate_tax_revenues, real_estate_tax))
    intergov_federal <- as.numeric(`intergovernmental_revenues-federal_government`)
    intergov_state   <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                          `intergovernmental_revenues-state_government`, state_govt))
    intergov_local   <- as.numeric(`intergovernmental_revenues-local_government`)
    intergov <- rowSums(cbind(intergov_federal, intergov_state, intergov_local), na.rm = TRUE)
    intergov[is.na(intergov_federal) & is.na(intergov_state) & is.na(intergov_local)] <- NA_real_
    police_exp <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
    streets_exp <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                      `public_works-highways_and_streets_expenditures`,
                                      streets_and_roads))
    # debt service has no pre-2006 column, so this share is missing for 2002-2005,
    # which is why the headline sample really starts in 2006
    debt_service <- as.numeric(debt_service_expenditures)
    list(muni = cousub_fips, year = afr_year,
         # EIT revenue in dollars, only used by the parked revenue section
         eit_revenue     = eit_revenue_amount,
         share_police    = ifelse(total_exp > 0, police_exp   / total_exp, NA_real_),
         share_streets   = ifelse(total_exp > 0, streets_exp  / total_exp, NA_real_),
         share_debt_svc  = ifelse(total_exp > 0, debt_service / total_exp, NA_real_),
         share_eit       = ifelse(total_rev > 0, eit_revenue_amount / total_rev, NA_real_),
         share_re        = ifelse(total_rev > 0, re_tax_revenue     / total_rev, NA_real_),
         share_intergov  = ifelse(total_rev > 0, intergov           / total_rev, NA_real_))
  }]
  fiscal <- fiscal[!is.na(muni) & !is.na(year)]
  fiscal <- fiscal[, lapply(.SD, function(column) if (is.numeric(column)) mean(column, na.rm = TRUE) else first(column)),
                   by = .(muni, year)]
  panel <- merge_check(panel, fiscal, by = c("muni", "year"),
                       label = "panel <- AFR fiscal shares",
                       expect_min_match = 0.70)
  panel[, log_eit_revenue := log_pos(eit_revenue)]

  # Property-tax rates (town, school district, county). Only the repaired file
  # will do: the raw one still has the reassessment errors.
  burden_path <- file.path(DERIVED_DIR, "pa_property_burden_panel_repaired.csv")
  if (!file.exists(burden_path))
    stop("Missing ", burden_path, ": run scripts/2_build_data/2_build_property_rates.py ",
         "then scripts/2_build_data/3_repair_property_rates.R")
  burden <- fread(burden_path)
  burden <- merge_check(burden[, .(municipality_id = muni_id, year,
                                   muni_eff_rate, sd_eff_rate, county_eff_rate)],
                        muni_xwalk, by = "municipality_id",
                        label = "property burden <- cousub xwalk",
                        expect_min_match = 0.95)
  # times 100 so property rates are in percentage points, like the EIT rate
  burden <- burden[!is.na(cousub_fips),
                   .(muni_eff_rate   = 100 * mean(muni_eff_rate,   na.rm = TRUE),
                     sd_eff_rate     = 100 * mean(sd_eff_rate,     na.rm = TRUE),
                     county_eff_rate = 100 * mean(county_eff_rate, na.rm = TRUE)),
                   by = .(muni = cousub_fips, year)]
  panel <- merge_check(panel, burden, by = c("muni", "year"),
                       label = "panel <- property effective rates",
                       expect_min_match = 0.90)

  # collapse any town-year that appears twice, and turn Inf into NA
  panel <- panel[, lapply(.SD, function(column) {
    if (is.numeric(column)) mean(column, na.rm = TRUE) else first(column)
  }), by = .(muni, year)]
  for (col in names(panel)) {
    if (is.numeric(panel[[col]])) {
      set(panel, which(!is.finite(panel[[col]])), col, NA_real_)
    }
  }

  # Warn about property rates over 5 pp (real ones are 0.1-3 pp). These are
  # leftovers the repair missed; they are reported, not changed.
  if (isTRUE(VERBOSE_MERGES)) {
    for (rate_col in c("muni_eff_rate", "sd_eff_rate", "county_eff_rate")) {
      high_rows <- panel[is.finite(get(rate_col)) & get(rate_col) > 5]
      if (nrow(high_rows)) {
        message(sprintf(
          "[check]  %-15s %s muni-years above 5pp (max %.1f) across %d munis -- suspect millage/CLR basis mismatch",
          rate_col, format(nrow(high_rows), big.mark = ","), max(high_rows[[rate_col]]),
          uniqueN(high_rows$muni)))
        worst <- high_rows[, .N, by = muni][order(-N)][1]
        message(sprintf("[check]    worst offender: muni %s in %d years", worst$muni, worst$N))
      }
    }
  }

  # The control set. The EIT revenue share is left out because it moves with the
  # tax rate itself, and the property rates replace the property-tax revenue share.
  ctrls <- c("share_police", "share_streets", "share_debt_svc", "share_intergov",
             "muni_eff_rate", "sd_eff_rate", "county_eff_rate")

  # Cell classification ----
  # Sort each town by its first rate change: did it raise or cut, and did the
  # home rates set workers' bills before and after? Run once per rate setter.
  classify_treatment <- function(data, rate_var, tag) {
    setorder(data, muni, year)
    # 1 when the home rates (town + school district) beat the work-town rate,
    # i.e. the home rates are what the worker actually pays
    data[, home_binds := as.integer(home_stack > muni_nonres_eit)]
    lag_col <- paste0("lag_", tag)
    data[, (lag_col) := shift(get(rate_var)), by = muni]
    # mark each year the rate went up or down (ignoring rounding noise)
    event_col <- paste0("ev_", tag)
    data[, (event_col) := fifelse(is.na(get(lag_col)), NA_character_,
                          fifelse(get(rate_var) > get(lag_col) + 1e-6, "Inc",
                          fifelse(get(rate_var) < get(lag_col) - 1e-6, "Dec", NA_character_)))]
    # a town's first change sets its cohort year
    first_events <- data[!is.na(get(event_col)), .SD[1], by = muni,
                         .SDcols = c("year", event_col, lag_col, rate_var, "home_binds")]
    setnames(first_events, c("year", event_col, lag_col, rate_var, "home_binds"),
             c("cohort_year", "direction", "baseline_rate", "rate_at_event", "postbind"))
    # did the home rates set the bill the year before the change (prebind)? the year of it (postbind)?
    lagged_binds <- data[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
    first_events <- merge(first_events, lagged_binds[, .(muni, year, lag_home_binds)],
                          by.x = c("muni", "cohort_year"), by.y = c("muni", "year"), all.x = TRUE)
    first_events[, prebind := lag_home_binds]; first_events[, lag_home_binds := NULL]
    # Inc-bind: the home rate already set the bill, so the increase raised it.
    # Inc-becomes: the increase pushed the home rate past the work rate.
    # Inc-nobind: the rate rose on paper but no bill changed (the placebo).
    first_events[, cell := fcase(
      direction == "Inc" & prebind == 1 & postbind == 1, "Inc-bind",
      direction == "Inc" & prebind == 0 & postbind == 1, "Inc-becomes",
      direction == "Inc" & prebind == 0 & postbind == 0, "Inc-nobind",
      direction == "Inc" & prebind == 1 & postbind == 0, "Inc-loses",
      direction == "Dec" & prebind == 1 & postbind == 1, "Dec-bind",
      direction == "Dec" & prebind == 1 & postbind == 0, "Dec-becomes-non",
      direction == "Dec" & prebind == 0 & postbind == 0, "Dec-nobind",
      direction == "Dec" & prebind == 0 & postbind == 1, "Dec-gains",
      default = NA_character_
    )]
    cell_col     <- paste0("cell_", tag)
    cohort_col   <- paste0("c_", tag)
    baseline_col <- paste0("baseline_", tag)
    dose_col     <- paste0("dose_", tag)
    setnames(first_events, c("cohort_year", "baseline_rate", "cell"),
             c(cohort_col, baseline_col, cell_col))
    first_events <- first_events[, .SD, .SDcols = c("muni", cohort_col, baseline_col, cell_col)]
    classified <- merge(data, first_events, by = "muni", all.x = TRUE)
    # a control town (NC) is one whose rate never changes; a change we can't date
    # (say, a missing year) makes it Uncertain instead of a control
    change_counts <- classified[!is.na(get(rate_var)), .(
      n_changes = sum(abs(diff(get(rate_var))) > 1e-6, na.rm = TRUE)
    ), by = muni]
    never_changers <- change_counts[n_changes == 0, muni]
    classified[muni %in% never_changers, (cell_col) := "NC"]
    classified[!muni %in% never_changers & is.na(get(cell_col)), (cell_col) := "Uncertain"]
    # towns that both raised and cut can't be called treated either way: Uncertain
    rate_steps <- classified[!is.na(get(rate_var)), .(year, rate = get(rate_var)), by = muni][
      order(muni, year)]
    rate_steps[, step := c(NA_real_, diff(rate)), by = muni]
    step_counts <- rate_steps[!is.na(step) & abs(step) > 1e-6, .(
      n_inc = sum(step > 0), n_dec = sum(step < 0)), by = muni]
    flip_floppers <- step_counts[n_inc > 0 & n_dec > 0, muni]
    classified[muni %in% flip_floppers, (cell_col) := "Uncertain"]
    # dose: how far the rate has risen since just before the first change (0 before it, 0 for controls)
    classified[, (dose_col) := fifelse(!is.na(get(cohort_col)) & year >= get(cohort_col),
                                       get(rate_var) - get(baseline_col), 0)]
    cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                     "Dec-bind", "Dec-becomes-non", "Dec-nobind",
                     "Inc-loses", "Dec-gains", "Uncertain")
    classified[, (cell_col) := factor(get(cell_col), levels = cell_levels)]
    classified
  }

  panel <- classify_treatment(panel, "resident_eit",    "muni")
  panel <- classify_treatment(panel, "sd_eit",          "sd")
  panel <- classify_treatment(panel, "muni_nonres_eit", "nr")
  uncertain_munis <- Reduce(union, list(unique(panel[cell_muni == "Uncertain", muni]),
                                        unique(panel[cell_sd   == "Uncertain", muni]),
                                        unique(panel[cell_nr   == "Uncertain", muni])))
  panel <- filter_check(panel, !panel$muni %in% uncertain_munis,
                        "drop flip-floppers (Uncertain)", unit = "muni")

  # the paper is about increases, so a town with a decrease on any side is dropped
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind", "Dec-gains")
  dec_munis <- unique(c(panel[cell_muni %in% dec_cells, muni],
                        panel[cell_sd   %in% dec_cells, muni],
                        panel[cell_nr   %in% dec_cells, muni]))
  panel <- filter_check(panel, !panel$muni %in% dec_munis,
                        "drop decrease munis (Dec cells)", unit = "muni")
  panel[, cell_muni := droplevels(cell_muni)]
  panel[, cell_sd   := droplevels(cell_sd)]
  panel[, cell_nr   := droplevels(cell_nr)]

  # Stack builder ----
  # One mini-experiment per cohort year (Cengiz et al. 2019): that year's treated
  # towns, five years either side, plus a fresh copy of every control town.
  build_stack <- function(data, side_tag, dose_col, K = K_stack) {
    cell_col   <- paste0("cell_", side_tag)
    cohort_col <- paste0("c_", side_tag)
    treated_munis <- unique(data[!is.na(get(cohort_col)) & get(cell_col) != "NC",
                                 .(muni, cohort = get(cohort_col), cell = get(cell_col))])
    nc_munis <- unique(data[get(cell_col) == "NC", muni])
    cohort_years <- sort(unique(treated_munis$cohort))
    build_cohort_stack <- function(cohort_year) {
      window_years <- (cohort_year - K):(cohort_year + K)
      cohort_munis <- treated_munis[cohort == cohort_year]
      if (nrow(cohort_munis) == 0) return(NULL)
      treated_rows <- data[muni %in% cohort_munis$muni & year %in% window_years]
      treated_rows <- merge(treated_rows, cohort_munis[, .(muni, treat_cell = cell)],
                            by = "muni", all.x = TRUE)
      control_rows <- data[muni %in% nc_munis & year %in% window_years]
      control_rows[, treat_cell := "NC"]
      stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
      if (nrow(stack_rows) == 0) return(NULL)
      stack_rows[, stack := cohort_year]; stack_rows[, rel := year - cohort_year]
      stack_rows
    }
    stacked <- rbindlist(lapply(cohort_years, build_cohort_stack), fill = TRUE)
    stacked[, stack_muni := paste0(stack, "_", muni)]
    stacked[, stack_year := paste0(stack, "_", year)]
    # dose_change: the dose measured from the town's own pre-change average in this stack
    stacked[, base_rate := mean(get(dose_col)[rel < 0], na.rm = TRUE),
            by = .(stack, muni)]
    stacked[!is.finite(base_rate), base_rate := 0]
    stacked[, dose_change := get(dose_col) - base_rate]

    # The model uses the rate level. With town fixed effects that gives the same
    # coefficients as the change (checked in RUN_LEVELS_COMPARE), but the level is
    # missing on about 1,300 rows where the change is known, so those rows drop.
    level_col <- c(muni = "resident_eit", sd = "sd_eit",
                   nr = "muni_nonres_eit")[[side_tag]]
    stacked[, dose_stack := get(level_col)]
    cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                     "Dec-bind", "Dec-becomes-non", "Dec-nobind", "Inc-loses", "Dec-gains")
    stacked[, treat_cell := factor(treat_cell, levels = cell_levels)]
    stacked
  }
  stk_muni <- build_stack(panel, "muni", "dose_muni")
  # the school-district rate goes in as a level too
  stk_muni[, dose_sd_change := dose_sd]
  stk_muni[, dose_sd := sd_eit]

  list(panel = panel, stk_muni = stk_muni, ctrls = ctrls,
       ctrls_str = paste(ctrls, collapse = " + "))
}

# Realized dose ----
# For the event studies: one number per town, how much its rate ended up rising
# five years after its first change. 0 for controls.
add_realized_dose <- function(panel, K = 5L) {
  rate_in <- function(rates, years, target_year) {
    matched <- rates[years == target_year]
    if (length(matched) == 1) matched else NA_real_
  }
  realized <- panel %>%
    distinct(muni, year, resident_eit, c_muni) %>%
    filter(!is.na(c_muni)) %>%
    group_by(muni) %>%
    summarise(realized_dose_muni =
                rate_in(resident_eit, year, min(first(c_muni) + K, max(year, na.rm = TRUE))) -
                rate_in(resident_eit, year, first(c_muni) - 1L),
              .groups = "drop")
  panel %>%
    left_join(realized, by = "muni") %>%
    mutate(realized_dose_muni = coalesce(realized_dose_muni, 0)) %>%
    as.data.table()
}

# Pre-trend tests ----
# Runs the pre-trend checks on one event-study model. The joint Wald test is
# unreliable with only 14-24 treated towns, so it is printed but the verdict
# (robust_clean) rests on the individual leads and the pre-trend slope.
pretrend_bucket <- function(model, lin_model = NULL, lin_term = "dose_rel") {
  leads <- tibble(term = rownames(model$coeftable), pval = model$coeftable[, "Pr(>|t|)"]) %>%
    filter(grepl("^rel::", term)) %>%
    mutate(event_time = as.integer(sub("rel::(-?\\d+):.*", "\\1", term))) %>%
    filter(event_time <= -2)
  wald_p  <- tryCatch(wald_pretrend(model, "^rel::-[2-9]")$p, error = function(e) NA_real_)
  has_lin <- !is.null(lin_model) && lin_term %in% names(coef(lin_model))
  lin_slope_est  <- if (has_lin) coef(lin_model)[lin_term] else NA_real_
  lin_slope_pval <- if (has_lin) lin_model$coeftable[lin_term, "Pr(>|t|)"] else NA_real_
  n_leads_sig <- sum(leads$pval < 0.05, na.rm = TRUE)
  data.table(
    wald_joint_p = round(wald_p, 4),
    n_leads      = nrow(leads),
    n_leads_sig5 = n_leads_sig,
    min_lead_p   = round(suppressWarnings(min(leads$pval)), 4),
    lin_slope    = round(lin_slope_est, 4),
    lin_slope_p  = round(lin_slope_pval, 4),
    robust_clean = n_leads_sig == 0 & (is.na(lin_slope_pval) | lin_slope_pval > 0.10))
}

# Labels ----
# Variable names to the labels printed in tables and figures.
OUTCOME_LABELS <- c(
  log_live_and_work    = "Log(residents working in own municipality)",
  log_in_commuters     = "Log(in-commuters)",
  log_out_commuters    = "Log(out-commuters)",
  log_total_jobs       = "Log(total jobs)",
  log_total_residents  = "Log(employed residents)",
  share_live_in        = "Share of workers living in own muni",
  share_work_outside   = "Share of residents working outside",
  share_nonres         = "Share of workers who are nonresidents",
  log_home_count       = "Log(workers living in block)",
  log_work_count       = "Log(workers at block)",
  home_in_muni_share   = "Share of block residents in own muni"
)

OUTCOME_LABELS_SHORT <- c(
  log_live_and_work    = "Log(work in own muni)",
  log_in_commuters     = "Log(in-commuters)",
  log_out_commuters    = "Log(out-commuters)",
  log_total_jobs       = "Log(total jobs)",
  log_total_residents  = "Log(employed residents)",
  share_live_in        = "Share live in muni",
  share_work_outside   = "Share work outside",
  share_nonres         = "Share nonres workers",
  log_home_count       = "Log(workers live)",
  log_work_count       = "Log(workers work)",
  home_in_muni_share   = "Share live+work in muni"
)

# a name with no label prints as itself (underscores escaped for LaTeX)
outcome_label <- function(outcomes, short = FALSE) {
  lookup <- if (short) OUTCOME_LABELS_SHORT else OUTCOME_LABELS
  labels <- lookup[outcomes]
  labels[is.na(labels)] <- gsub("_", "\\\\_", outcomes[is.na(labels)])
  unname(labels)
}

# Theme and colors ----
# red for the town rate, blue for the school-district rate
PAPER_RED  <- "#C0392B"
PAPER_BLUE <- "#1F4E79"

theme_paper <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text       = element_text(face = "bold", size = base_size - 1),
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(1.4, "lines"),
      axis.title       = element_text(size = base_size - 1)
    )
}

# Joint Wald test on the leads ----
# Tests whether all the pre-change coefficients are zero together, using the
# clustered standard errors. NA if there are fewer than two of them.
wald_pretrend <- function(model, term_regex = "^rel::-[2-9]") {
  coef_names <- names(coef(model))
  lead_idx <- grep(term_regex, coef_names)
  if (length(lead_idx) < 2)
    return(list(stat = NA_real_, df = 0L, p = NA_real_,
                terms = coef_names[lead_idx]))
  lead_coefs <- coef(model)[lead_idx]
  lead_vcov  <- vcov(model)[lead_idx, lead_idx, drop = FALSE]
  vcov_inverse <- tryCatch(solve(lead_vcov), error = function(e) NULL)
  if (is.null(vcov_inverse))
    return(list(stat = NA_real_, df = length(lead_idx),
                p = NA_real_, terms = names(lead_coefs)))
  wald_stat <- as.numeric(t(lead_coefs) %*% vcov_inverse %*% lead_coefs)
  wald_p <- pchisq(wald_stat, df = length(lead_coefs), lower.tail = FALSE)
  list(stat = wald_stat, df = length(lead_coefs), p = wald_p, terms = names(lead_coefs))
}

# Number formatters ----
sig_stars <- function(pvals) {
  stars <- rep("", length(pvals))
  stars[!is.na(pvals) & pvals < 0.10] <- "*"
  stars[!is.na(pvals) & pvals < 0.05] <- "**"
  stars[!is.na(pvals) & pvals < 0.01] <- "***"
  stars
}
fmt_est <- function(estimates, digits = 3) {
  if (is.null(estimates) || length(estimates) == 0) return("")
  ifelse(is.na(estimates), "---", formatC(estimates, format = "f", digits = digits))
}
fmt_se <- function(ses, digits = 3) {
  if (is.null(ses) || length(ses) == 0) return("")
  ifelse(is.na(ses), "", sprintf("(%s)", formatC(ses, format = "f", digits = digits)))
}
fmt_n <- function(counts) {
  if (is.null(counts) || length(counts) == 0) return("")
  ifelse(is.na(counts), "---", formatC(as.numeric(counts), format = "d", big.mark = ","))
}
# estimate with significance stars, as printed in the tables
fmt_coef <- function(estimate, pval) {
  stars <- sig_stars(pval)
  if (nchar(stars) == 0) fmt_est(estimate) else paste0(fmt_est(estimate), "\\sym{", stars, "}")
}

# Table builder ----
# Writes a LaTeX table: one column per outcome, each cell's estimate with stars
# and its SE underneath, then optional pre-trend p, controls and diagnostics rows.
#   coefs_dt     outcome, cell, est, se, p (long)
#   diag_dt      one row per outcome: n, r2, wr2, pre_baseline, ...
#   pretrend_dt  cell, outcome, wald_pre_p
#   controls_dt  outcome, term, est, se, p
# Returns a character vector, one element per line.
build_pub_table <- function(coefs_dt, diag_dt, outcomes, cells,
                            caption, label, short_labels = TRUE,
                            notes = character(0), col_spec = NULL,
                            sideways = FALSE, pretrend_dt = NULL,
                            # dep_mean is not printed by default: for a logged outcome
                            # it is the mean of the log, which invites wrong conversions
                            diag_fields = c("n", "r2", "wr2", "pre_baseline"),
                            controls_dt = NULL, control_labels = NULL) {
  coefs_dt <- mutate(coefs_dt, outcome = as.character(outcome))
  if ("outcome" %in% names(diag_dt)) diag_dt <- mutate(diag_dt, outcome = as.character(outcome))
  if (is.null(col_spec)) col_spec <- paste0("l", strrep("c", length(outcomes)))

  header <- paste0("Outcome & ", paste(outcome_label(outcomes, short = short_labels),
                                       collapse = " & "), " \\\\")

  # Cell rows. A cell with no estimate in any column is dropped and named in the note.
  body <- character(0)
  dropped_cells <- character(0)
  for (cell_name in cells) {
    est_vec <- character(length(outcomes))
    se_vec  <- character(length(outcomes))
    any_present <- FALSE
    for (col_index in seq_along(outcomes)) {
      coef_row <- filter(coefs_dt, outcome == outcomes[col_index], cell == cell_name)
      if (nrow(coef_row) == 0 || is.na(coef_row$est[1]) || !is.finite(coef_row$est[1])) {
        est_vec[col_index] <- "---"; se_vec[col_index] <- ""
      } else {
        any_present <- TRUE
        est_vec[col_index] <- fmt_coef(coef_row$est[1], coef_row$p[1])
        se_vec[col_index]  <- fmt_se(coef_row$se[1])
      }
    }
    if (!any_present) {
      dropped_cells <- c(dropped_cells, cell_name)
      next
    }
    body <- c(body,
              paste0("\\textbf{", cell_name, "} & ", paste(est_vec, collapse = " & "), " \\\\"),
              paste0("                  & ", paste(se_vec,  collapse = " & "), " \\\\"))
    if (is.data.frame(pretrend_dt) && nrow(pretrend_dt) > 0) {
      pretrend_p <- vapply(outcomes, function(outcome_name) {
        pretrend_row <- filter(pretrend_dt, cell == cell_name, outcome == outcome_name)
        if (!nrow(pretrend_row) || is.na(pretrend_row$wald_pre_p[1])) "---"
        else formatC(pretrend_row$wald_pre_p[1], format = "f", digits = 3)
      }, character(1))
      body <- c(body,
                paste0("\\quad Pre-trend Wald $p$ & ", paste(pretrend_p, collapse = " & "), " \\\\"))
    }
  }
  if (length(dropped_cells) > 0) {
    notes <- c(notes, paste0("Rows omitted (no estimable coefficients): ",
                             paste(dropped_cells, collapse = ", "), "."))
  }

  # Fiscal controls block
  controls_block <- character(0)
  if (is.data.frame(controls_dt) && nrow(controls_dt) > 0) {
    ncols <- length(outcomes)
    controls_block <- c("\\midrule",
                        paste0("\\multicolumn{", ncols + 1, "}{l}{\\textit{Fiscal controls}} \\\\"))
    ctrl_terms <- unique(controls_dt$term)
    label_map <- if (!is.null(control_labels)) control_labels else setNames(ctrl_terms, ctrl_terms)
    for (control_term in ctrl_terms) {
      est_vec <- character(ncols); se_vec <- character(ncols)
      any_present <- FALSE
      for (col_index in seq_along(outcomes)) {
        control_row <- filter(controls_dt, outcome == outcomes[col_index], term == control_term)
        if (nrow(control_row) == 0 || is.na(control_row$est[1])) {
          est_vec[col_index] <- "---"; se_vec[col_index] <- ""; next
        }
        any_present <- TRUE
        est_vec[col_index] <- fmt_coef(control_row$est[1], control_row$p[1])
        se_vec[col_index]  <- fmt_se(control_row$se[1])
      }
      if (!any_present) next
      pretty <- if (control_term %in% names(label_map)) label_map[[control_term]] else gsub("_", "\\\\_", control_term)
      controls_block <- c(controls_block,
                          paste0(pretty, " & ", paste(est_vec, collapse = " & "), " \\\\"),
                          paste0("                  & ", paste(se_vec,  collapse = " & "), " \\\\"))
    }
  }

  # Diagnostics rows
  fmt_3dp <- function(value) formatC(value, format = "f", digits = 3)
  diag_lookup <- list(
    n            = list(label = "$N$",                        fmt = fmt_n),
    r2           = list(label = "$R^2$",                      fmt = fmt_3dp),
    wr2          = list(label = "Within $R^2$",               fmt = fmt_3dp),
    dep_mean     = list(label = "Dep.\\ mean",                fmt = fmt_3dp),
    dep_sd       = list(label = "Dep.\\ SD",                  fmt = fmt_3dp),
    pre_baseline = list(label = "Treated pre-period baseline",
                        fmt = function(value) formatC(value, format = "f", digits = if (abs(value) < 1) 3 else 0)),
    wald_p       = list(label = "Joint Wald $p$ (leads)",     fmt = fmt_3dp)
  )
  diag_row <- function(field) {
    diag_spec <- diag_lookup[[field]]
    values <- vapply(outcomes, function(outcome_name) {
      outcome_row <- if ("outcome" %in% names(diag_dt)) filter(diag_dt, outcome == outcome_name) else diag_dt[0, ]
      if (nrow(outcome_row) == 0 || !(field %in% names(outcome_row))) return("---")
      value <- outcome_row[[field]]
      if (length(value) == 0 || all(is.na(value))) "---" else diag_spec$fmt(value[1])
    }, character(1))
    paste0(diag_spec$label, " & ", paste(values, collapse = " & "), " \\\\")
  }
  diag_block <- vapply(intersect(diag_fields, names(diag_lookup)), diag_row, character(1),
                       USE.NAMES = FALSE)

  note_all <- paste(c(notes, "SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
                    collapse = " ")

  # tables with 3+ outcome columns are scaled to the text width
  tabular <- c(
    paste0("  \\begin{tabular}{", col_spec, "}"),
    "    \\toprule",
    paste0("    ", header),
    "    \\midrule",
    paste0("    ", body),
    if (length(controls_block)) paste0("    ", controls_block) else character(0),
    if (length(diag_block)) "    \\midrule" else character(0),
    if (length(diag_block)) paste0("    ", diag_block) else character(0),
    "    \\bottomrule",
    "  \\end{tabular}"
  )
  if (length(outcomes) >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
  c(
    if (sideways) "\\begin{sidewaystable}" else "\\begin{table}[H]",
    "  \\centering",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    "  \\footnotesize",
    "  \\renewcommand{\\arraystretch}{0.9}",
    tabular,
    "  \\vspace{0.3em}",
    "  \\begin{minipage}{0.95\\textwidth}",
    paste0("    \\footnotesize \\textit{Notes:} ", note_all),
    "  \\end{minipage}",
    if (sideways) "\\end{sidewaystable}" else "\\end{table}"
  )
}
