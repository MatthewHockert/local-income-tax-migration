#!/usr/bin/env Rscript
# 1_main_models.R
# The paper's main regressions: the headline binding decomposition, the pooled
# version, the EIT revenue response and the flexible-dose fits. Results go to
# data/results/. Turn sections on and off with the RUN_ flags; PARKED ones are
# not in the paper.

suppressMessages({
  library(dplyr)   # before data.table so data.table's first/last/between win
  library(data.table)
  library(fixest)
})

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
RUN_HEADLINE <- TRUE  # headline binding decomposition (8 outcomes) + validation gate
RUN_NOPROP   <- FALSE  # PARKED 2026-10-29, not in the paper: headline re-run WITHOUT the property effective-rate controls
RUN_POOLED   <- TRUE  # pooled Inc/Dec per-pp + binary benchmark (cousub)
RUN_REVENUE  <- TRUE  # log EIT revenue decomposition + ES + Laffer offset + splits
RUN_LEVELS_COMPARE <- FALSE  # PARKED 2026-10-29, not in the paper: headline + pooled re-run with the treatment in LEVELS
RUN_PRE_BASELINES <- TRUE  # treated pre-period baselines under the results tables
RUN_FLEX_DOSE <- TRUE  # headline + pooled re-run with a quadratic and a spline in the dose
RUN_REVENUE_TRADEOFF <- FALSE  # PARKED 2026-10-29, not in the paper: workers-vs-revenue tradeoff on ONE shared sample, and how it varies with the pre-existing rate

# Shared setup ----
# build the panel and stacks once; every section below uses them
env <- build_headline_env()
panel <- env$panel
stk_muni <- env$stk_muni
ctrls <- env$ctrls
ctrls_str <- env$ctrls_str

# Headline binding decomposition ----
# Table 3: the town and school-district rates, by binding cell, in one regression.
if (RUN_HEADLINE) local({

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
                "log_total_jobs", "log_total_residents",
                "share_live_in", "share_work_outside", "share_nonres")

  run_headline <- function(stack_data, outcome) {
    # no school-district change means a dose of 0, not missing; then drop rows
    # missing the outcome or a control
    model_data <- stack_data %>%
      select(any_of(c("muni", "year", "treat_cell", "dose_stack", "cell_sd", "dose_sd",
                      "stack_muni", "stack_year", outcome, ctrls))) %>%
      mutate(dose_sd = coalesce(dose_sd, 0)) %>%
      filter(complete.cases(.))
    if (nrow(model_data) == 0) return(NULL)
    formula <- as.formula(paste0(outcome,
         " ~ i(treat_cell, dose_stack, ref='NC') ",
         "+ i(cell_sd, dose_sd, ref='NC') + ",
         paste(ctrls, collapse = " + "),
         " | stack_muni + stack_year"))
    feols(formula, data = model_data, cluster = ~muni)
  }

  models <- list()
  for (outcome in outcomes) {
    model <- run_headline(stk_muni, outcome)
    if (!is.null(model)) models[[outcome]] <- model
  }

  coefs <- bind_rows(lapply(names(models), function(outcome) {
    model <- models[[outcome]]
    as_tibble(model$coeftable, rownames = "term") %>%
      rename(est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`) %>%
      mutate(outcome = outcome, n = nobs(model))
  })) %>%
    mutate(sig = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""))
  fwrite(coefs, file.path(RESULTS_DIR, "headline_full_table.csv"))

  # Validation gate ----
  # Stop if an Inc-bind number moves more than 0.005 away from the paper.
  # Only change these values when you mean to change the headline.
  inc_bind_rows <- filter(coefs, grepl("Inc-bind:dose_stack", term))
  expected <- c(log_live_and_work = -0.537, log_in_commuters = -0.345,
                log_total_jobs = -0.423, share_live_in = -0.058)
  for (outcome in names(expected)) {
    estimate <- inc_bind_rows$est[inc_bind_rows$outcome == outcome]
    if (length(estimate) != 1 || abs(estimate - expected[outcome]) > 0.005)
      stop(sprintf("VALIDATION FAIL: Inc-bind %s = %s, expected %.3f (tol 0.005)",
                   outcome, ifelse(length(estimate) == 1, sprintf("%.3f", estimate), "NA"),
                   expected[outcome]))
  }
  print(paste("[validation] headline Inc-bind matches spec:",
              "log_LW=-0.537 log_IC=-0.345 log_TJ=-0.423 share=-0.058"))

  diagnostics <- bind_rows(lapply(names(models), function(outcome) {
    model <- models[[outcome]]
    dep_var <- model$residuals + model$fitted.values
    tibble(
      outcome    = outcome,
      n          = nobs(model),
      R2         = round(fitstat(model, "r2", simplify = TRUE), 4),
      Within_R2  = round(fitstat(model, "wr2", simplify = TRUE), 4),
      dep_mean   = round(mean(dep_var, na.rm = TRUE), 4),
      dep_sd     = round(sd(dep_var, na.rm = TRUE), 4),
      cluster_n  = model$nobs_origin %/% 1L,  # NOT a cluster count: rows before fixest drops any. Nothing reads it.
      fe         = "stack_muni + stack_year",
      se_type    = "cluster: muni"
    )
  }))
  fwrite(diagnostics, file.path(RESULTS_DIR, "headline_diagnostics.csv"))
  print(diagnostics)

  inc_bind_rows %>%
    transmute(outcome, term, est = round(est, 3), se = round(se, 3), p = round(p, 4), n) %>%
    print()
  coefs %>%
    filter(outcome == "log_live_and_work", term %in% ctrls) %>%
    transmute(term, est = round(est, 4), se = round(se, 4), p = round(p, 4), sig) %>%
    print()
})

# Headline WITHOUT property controls ----
# Does conditioning on the property tax move the EIT estimate? Re-runs the
# headline spec verbatim except that muni/sd/county effective property rates
# are removed from the right-hand side. Everything else -- outcomes, muni and
# SD cell x dose interactions, AFR spending shares, stack-interacted FE,
# clustering on muni -- is identical.
#
# Two no-property variants, because dropping a control also drops it from
# complete.cases() and would otherwise change the SAMPLE at the same time:
#   fixed  = same estimation sample as the headline, property rates omitted
#            from the regression only. This isolates the control's effect.
#   free   = complete cases on the reduced control set, so munis with missing
#            property data re-enter. This is what the estimate would be if the
#            property panel had never been built.
# The `fixed` column is the answer to "does the control change the estimate".
# NOTE: this is NOT the same comparison as the pre-property headline recorded
# in the validation gate above (log_LW = -0.573), because that spec also
# carried share_re and share_eit, which were dropped when the property rates
# were added. Two things changed there at once; only one changes here.
# Outputs -> data/results/headline_noprop_compare.csv
# Lay explanation: the paper conditions on how heavily a town taxes property
# so that the income-tax effect is not just picking up "high-tax town." This
# section takes that control back out and checks whether the headline number
# actually depended on it.
if (RUN_NOPROP) local({

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
                "log_total_jobs", "log_total_residents",
                "share_live_in", "share_work_outside", "share_nonres")
  prop_ctrls <- c("muni_eff_rate", "sd_eff_rate", "county_eff_rate")
  keep_ctrls <- setdiff(ctrls, prop_ctrls)

  fit <- function(stk, outcome, use_ctrls, sample_ctrls) {
    cols <- c("muni", "year", "treat_cell", "dose_stack",
              "cell_sd", "dose_sd", "stack_muni", "stack_year",
              outcome, sample_ctrls)
    cols <- intersect(cols, names(stk))
    d <- stk[, ..cols]
    if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) return(NULL)
    f <- as.formula(paste0(outcome,
         " ~ i(treat_cell, dose_stack, ref='NC') ",
         "+ i(cell_sd, dose_sd, ref='NC') + ",
         paste(use_ctrls, collapse = " + "),
         " | stack_muni + stack_year"))
    feols(f, data = d, cluster = ~muni)
  }

  grab <- function(m, lab, o) {
    if (is.null(m)) return(NULL)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"), c("est", "se", "p"),
             skip_absent = TRUE)
    ct <- ct[grepl("^treat_cell::", term)]
    ct[, `:=`(cell = sub("treat_cell::(.*):dose_stack", "\\1", term),
              spec = lab, outcome = o, n = nobs(m))]
    ct[, .(outcome, cell, spec, est, se, p, n)]
  }

  res <- list()
  for (o in outcomes) {
    m_with  <- fit(stk_muni, o, ctrls,      ctrls)          # headline
    m_fixed <- fit(stk_muni, o, keep_ctrls, ctrls)          # same sample, no property
    m_free  <- fit(stk_muni, o, keep_ctrls, keep_ctrls)     # reduced-sample
    res[[o]] <- rbindlist(list(grab(m_with,  "with_property",   o),
                               grab(m_fixed, "no_property_fixed", o),
                               grab(m_free,  "no_property_free",  o)), fill = TRUE)
  }
  cmp <- rbindlist(res, fill = TRUE)
  fwrite(cmp, file.path(RESULTS_DIR, "headline_noprop_compare.csv"))

  wide <- dcast(cmp, outcome + cell ~ spec, value.var = c("est", "n"))
  wide[, delta_fixed := round(est_no_property_fixed - est_with_property, 4)]
  wide[, pct_shift   := round(100 * delta_fixed / abs(est_with_property), 1)]
  print(wide[cell %in% c("Inc-bind", "Inc-becomes", "Inc-nobind") &
             outcome %in% c("log_live_and_work", "log_in_commuters",
                            "log_total_jobs", "share_live_in"),
             .(outcome, cell,
               with_prop  = round(est_with_property, 3),
               no_prop    = round(est_no_property_fixed, 3),
               delta      = delta_fixed,
               pct_shift,
               n_with     = n_with_property,
               n_free     = n_no_property_free)])
})

# Pooled Inc/Dec benchmark ----
# What you'd estimate ignoring which rate binds: every increase lumped together.
# Same controls and fixed effects as the headline.
if (RUN_POOLED) local({

  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")
  to_direction <- function(cell) {
    cell <- as.character(cell)
    factor(case_when(cell %in% inc_cells ~ "Inc",
                     cell %in% dec_cells ~ "Dec",
                     cell == "NC"        ~ "NC"),
           levels = c("NC", "Inc", "Dec"))
  }
  pooled_data <- stk_muni %>%
    mutate(direction_muni     = to_direction(treat_cell),
           direction_sd       = to_direction(cell_sd),
           ever_muni_inc_post = as.integer(direction_muni == "Inc" & rel >= 0),
           ever_muni_dec_post = as.integer(direction_muni == "Dec" & rel >= 0),
           ever_sd_inc_post   = as.integer(direction_sd   == "Inc" & rel >= 0),
           ever_sd_dec_post   = as.integer(direction_sd   == "Dec" & rel >= 0))

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
                "log_total_jobs", "log_total_residents",
                "share_live_in", "share_work_outside", "share_nonres")
  fe_and_controls <- paste0(" + ", paste(ctrls, collapse = " + "), " | stack_muni + stack_year")

  fit_pooled <- function(outcome) {
    model_data <- pooled_data %>%
      select(any_of(c("muni", "year", "direction_muni", "dose_stack", "direction_sd", "dose_sd",
                      "ever_muni_inc_post", "ever_muni_dec_post",
                      "ever_sd_inc_post", "ever_sd_dec_post",
                      "stack_muni", "stack_year", outcome, ctrls))) %>%
      mutate(dose_sd = coalesce(dose_sd, 0)) %>%
      filter(complete.cases(.))
    if (nrow(model_data) == 0) return(NULL)
    per_pp <- feols(as.formula(paste0(outcome,
                " ~ i(direction_muni, dose_stack, ref='NC') + i(direction_sd, dose_sd, ref='NC')",
                fe_and_controls)), data = model_data, cluster = ~muni)
    binary <- feols(as.formula(paste0(outcome,
                " ~ ever_muni_inc_post + ever_muni_dec_post + ever_sd_inc_post + ever_sd_dec_post",
                fe_and_controls)), data = model_data, cluster = ~muni)
    list(outcome = outcome, per_pp = per_pp, binary = binary)
  }
  fits <- Filter(Negate(is.null), lapply(outcomes, fit_pooled))

  coef_table <- function(model) {
    as_tibble(model$coeftable, rownames = "term") %>%
      rename(est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)
  }

  # every coefficient of the per-pp models, for the appendix table
  full_table <- bind_rows(lapply(fits, function(fit) {
    coef_table(fit$per_pp) %>%
      mutate(level = "cousub", outcome = fit$outcome, n = nobs(fit$per_pp))
  }))

  # the pooled coefficients of interest and the model diagnostics
  key_terms <- c(per_pp = "direction_(muni|sd)::", binary = "ever_(muni|sd)_(inc|dec)_post")
  results <- list()
  diagnostics <- list()
  for (fit in fits) {
    for (mode in names(key_terms)) {
      model <- fit[[mode]]
      key <- filter(coef_table(model), grepl(key_terms[[mode]], term))
      if (nrow(key) == 0) next
      results[[length(results) + 1L]] <- key %>%
        transmute(level = "cousub", mode = mode, outcome = fit$outcome,
                  term, est, se, p, n = nobs(model))
      dep_var <- model$residuals + model$fitted.values
      diagnostics[[length(diagnostics) + 1L]] <- tibble(
        level = "cousub", mode = mode, outcome = fit$outcome,
        n        = nobs(model),
        r2       = round(fitstat(model, "r2", simplify = TRUE), 4),
        wr2      = round(fitstat(model, "wr2", simplify = TRUE), 4),
        dep_mean = round(mean(dep_var, na.rm = TRUE), 4),
        dep_sd   = round(sd(dep_var, na.rm = TRUE), 4))
    }
  }
  results <- bind_rows(results) %>%
    mutate(side = case_when(grepl("direction_muni::|ever_muni_", term) ~ "muni",
                            grepl("direction_sd::|ever_sd_", term)     ~ "sd"),
           direction = case_when(grepl("Inc", term, ignore.case = TRUE) ~ "Inc",
                                 grepl("Dec", term, ignore.case = TRUE) ~ "Dec"))

  fwrite(results, file.path(RESULTS_DIR, "pooled_results.csv"))
  fwrite(bind_rows(diagnostics), file.path(RESULTS_DIR, "pooled_diagnostics.csv"))
  fwrite(full_table, file.path(RESULTS_DIR, "pooled_full_table.csv"))

  results %>%
    transmute(level, mode, outcome, direction, est = round(est, 3), se = round(se, 3),
              p = round(p, 4), n = format(n, big.mark = ",")) %>%
    print(n = Inf)
})

# EIT revenue response ----
# Does raising the rate raise revenue, and how much of the gain do the people
# who leave take with them?
if (RUN_REVENUE) local({

  K_stack <- 5L
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind", "Inc-loses", "Dec-gains")

  # share_eit is EIT revenue over total revenue, the outcome itself, so it leaves the controls
  ctrls <- setdiff(ctrls, "share_eit")
  ctrls_str <- paste(ctrls, collapse = " + ")

  # EIT revenue from the financial reports: the 2006+ column, else the older one.
  # The Local Services Tax is its own line and never gets added in. Zero counts as missing.
  rate_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  muni_xwalk <- unique(rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  afr <- merge(afr, muni_xwalk, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  afr[, afr_year := as.integer(ifelse(!is.na(reporting_year), reporting_year, year))]
  afr[, eit_amount := suppressWarnings(as.numeric(
        ifelse(!is.na(earned_income_tax_revenues), earned_income_tax_revenues, earned_income_tax)))]
  eit_revenue <- afr[!is.na(cousub_fips) & !is.na(afr_year),
                     .(eit_rev = mean(eit_amount, na.rm = TRUE)),
                     by = .(muni = cousub_fips, year = afr_year)]
  eit_revenue[!is.finite(eit_rev) | eit_rev <= 0, eit_rev := NA_real_]
  panel <- merge(panel, eit_revenue, by = c("muni", "year"), all.x = TRUE)
  panel[, log_eit_revenue := ifelse(!is.na(eit_rev) & eit_rev > 0, log(eit_rev), NA_real_)]
  print(sprintf("EIT-revenue muni-years: %s of %s (%.0f%%); Inc-bind treated muni-years with EIT: %d",
                format(sum(!is.na(panel$log_eit_revenue)), big.mark = ","),
                format(nrow(panel), big.mark = ","),
                100 * mean(!is.na(panel$log_eit_revenue)),
                sum(panel$cell_muni == "Inc-bind" & !is.na(panel$log_eit_revenue))))

  # Same stacking as build_headline_env(), redone here because revenue has to be
  # on the panel before stacking.
  build_revenue_stack <- function(data, cell_col, levels) {
    treated_munis <- unique(data[get(cell_col) != "NC" & !is.na(c_muni),
                                 .(muni, cohort = c_muni, cell = get(cell_col))])
    nc_munis <- unique(data[get(cell_col) == "NC", muni])
    stacked <- rbindlist(lapply(sort(unique(treated_munis$cohort)), function(cohort_year) {
      window_years <- (cohort_year - K_stack):(cohort_year + K_stack)
      cohort_munis <- treated_munis[cohort == cohort_year]
      treated_rows <- data[muni %in% cohort_munis$muni & year %in% window_years]
      treated_rows <- merge(treated_rows, cohort_munis[, .(muni, treat_cell = cell)],
                            by = "muni", all.x = TRUE)
      control_rows <- data[muni %in% nc_munis & year %in% window_years]
      control_rows[, treat_cell := "NC"]
      stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
      stack_rows[, stack := cohort_year]; stack_rows[, rel := as.integer(year - cohort_year)]
      stack_rows
    }), fill = TRUE)
    stacked[, stack_muni := paste0(stack, "_", muni)]
    stacked[, stack_year := paste0(stack, "_", year)]
    # note: unlike the headline, the dose here is the change, not the level,
    # so this sample is slightly different
    stacked[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE), by = .(stack, muni)]
    stacked[!is.finite(base_rate), base_rate := 0]
    stacked[, dose_stack := dose_muni - base_rate]
    stacked[, treat_cell := factor(treat_cell, levels = levels)]
    stacked[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
    stacked[is.na(dose_sd), dose_sd := 0]
    stacked
  }

  model_vars <- c("log_eit_revenue", "treat_cell", "dose_stack", "cell_sd", "dose_sd",
                  "stack_muni", "stack_year", "muni", ctrls)
  revenue_formula <- as.formula(paste0("log_eit_revenue ~ i(treat_cell, dose_stack, ref='NC') ",
                      "+ i(cell_sd, dose_sd, ref='NC') + ", ctrls_str, " | stack_muni + stack_year"))

  # the headline regression with log EIT revenue as the outcome
  model_data <- build_revenue_stack(panel, "cell_muni", cell_levels) %>%
    select(all_of(model_vars)) %>%
    filter(complete.cases(.))
  print(sprintf("static EIT regression sample: %s rows; Inc-cell treated rows = %d",
                format(nrow(model_data), big.mark = ","), sum(model_data$treat_cell %in% inc_cells)))
  model <- feols(revenue_formula, data = model_data, cluster = ~muni)
  decomp <- as_tibble(model$coeftable, rownames = "term") %>%
    filter(grepl("^treat_cell::", term)) %>%
    mutate(cell = sub("treat_cell::([^:]+):.*", "\\1", term)) %>%
    filter(cell %in% inc_cells) %>%
    transmute(outcome = "log_eit_revenue", cell, beta = round(Estimate, 3),
              se = round(`Std. Error`, 3), p = round(`Pr(>|t|)`, 4), n = nobs(model)) %>%
    arrange(cell)
  print(decomp[match(inc_cells, decomp$cell), ])
  fwrite(decomp, file.path(RESULTS_DIR, "eit_revenue_decomp.csv"))

  # event study on revenue, to check for pre-trends
  panel <- add_realized_dose(panel, K = K_stack)
  treated_munis <- unique(panel[cell_muni != "NC" & !is.na(c_muni), .(muni, cohort = c_muni)])
  nc_munis <- unique(panel[cell_muni == "NC", muni])
  es_stack <- rbindlist(lapply(sort(unique(treated_munis$cohort)), function(cohort_year) {
    window_years <- (cohort_year - K_stack):(cohort_year + K_stack)
    treated_rows <- panel[muni %in% treated_munis[cohort == cohort_year, muni] & year %in% window_years]
    treated_rows[, tcell := cell_muni]
    control_rows <- panel[muni %in% nc_munis & year %in% window_years]
    control_rows[, tcell := "NC"]
    stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
    stack_rows[, stack := cohort_year]; stack_rows[, rel := as.integer(year - cohort_year)]
    stack_rows
  }), fill = TRUE)
  es_stack[, stack_muni := paste0(stack, "_", muni)]
  es_stack[, stack_year := paste0(stack, "_", year)]
  es_stack[, tcell := factor(tcell, levels = cell_levels)]
  es_stack[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
  es_stack[, cell_nr := factor(as.character(cell_nr), levels = cell_levels)]
  es_stack[is.na(dose_sd), dose_sd := 0]
  es_stack[is.na(dose_nr), dose_nr := 0]

  es_vars <- c("muni", "year", "stack_muni", "stack_year", "tcell", "realized_dose_muni", "rel",
               "cell_sd", "dose_sd", "log_eit_revenue", ctrls)
  es_formula <- as.formula(paste0("log_eit_revenue ~ i(rel, realized_dose_muni, ref=-1) + ",
                 "i(cell_sd, dose_sd, ref='NC') + ", ctrls_str, " | stack_muni + stack_year"))
  es_coefs <- list()
  es_summary <- list()
  for (cell_name in inc_cells) {
    model_data <- es_stack %>%
      filter(tcell %in% c("NC", cell_name)) %>%
      select(all_of(es_vars)) %>%
      filter(complete.cases(.))
    if (nrow(model_data) == 0 || sum(model_data$tcell == cell_name) == 0) next
    model <- tryCatch(feols(es_formula, data = model_data, cluster = ~muni), error = function(e) NULL)
    if (is.null(model)) next
    event_coefs <- as_tibble(model$coeftable, rownames = "term") %>%
      filter(grepl("^rel::", term)) %>%
      mutate(rel = as.integer(sub("rel::(-?\\d+):.*", "\\1", term)))
    es_coefs[[cell_name]] <- event_coefs %>%
      transmute(cell = cell_name, rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)
    es_summary[[cell_name]] <- tibble(
      cell       = cell_name,
      n_munis    = n_distinct(model_data$muni[model_data$tcell == cell_name]),
      es_post    = round(mean(event_coefs$Estimate[event_coefs$rel >= 0 & event_coefs$rel <= 5], na.rm = TRUE), 3),
      es_pre     = round(mean(event_coefs$Estimate[event_coefs$rel <= -2], na.rm = TRUE), 3),
      wald_pre_p = round(wald_pretrend(model, "^rel::-[2-9]")$p, 4))
  }
  fwrite(bind_rows(es_coefs), file.path(RESULTS_DIR, "eit_revenue_es.csv"))
  print(bind_rows(es_summary))

  # If nobody moved, a 1 pp increase would raise log revenue by 1 / (starting rate).
  # The offset is the share of that gain lost to people leaving: 1 - beta x starting rate.
  # It applies to these 24 Inc-bind towns, not to Pennsylvania as a whole.
  beta_net <- decomp$beta[decomp$cell == "Inc-bind"]
  pre_event_rates <- panel %>%
    filter(cell_muni == "Inc-bind") %>%
    group_by(muni) %>%
    summarise(tau = resident_eit[match(first(c_muni) - 1L, year)], .groups = "drop")
  tau_mean   <- mean(pre_event_rates$tau, na.rm = TRUE)
  tau_median <- median(pre_event_rates$tau, na.rm = TRUE)
  offset_at  <- function(tau_pre) 1 - beta_net * tau_pre
  offset <- tibble(beta_net = beta_net, tau_pre_mean = round(tau_mean, 3),
                   tau_pre_median = round(tau_median, 3),
                   offset_at_mean = round(offset_at(tau_mean), 3),
                   offset_at_median = round(offset_at(tau_median), 3))
  print(offset)
  fwrite(offset, file.path(RESULTS_DIR, "eit_revenue_offset.csv"))

  # Split each cell by starting rate, to separate the few high-rate cities from
  # towns under the usual 1 pp cap. Splits at 1 pp and at 0.5 pp (-lo below, -hi at or above).
  baselines <- panel %>%
    filter(cell_muni %in% inc_cells) %>%
    group_by(muni) %>%
    summarise(base_cell = as.character(first(cell_muni)),
              tau_pre   = resident_eit[match(first(c_muni) - 1L, year)], .groups = "drop")
  munis_in <- function(cell_name, threshold, high) {
    baselines %>%
      filter(base_cell == cell_name, if (high) tau_pre >= threshold else tau_pre < threshold)
  }

  run_split <- function(threshold, split_name) {
    split_panel <- mutate(panel, cell_split = as.character(cell_muni))
    for (cell_name in inc_cells) {
      split_panel <- split_panel %>%
        mutate(cell_split = case_when(
          muni %in% munis_in(cell_name, threshold, high = FALSE)$muni ~ paste0(cell_name, "-lo"),
          muni %in% munis_in(cell_name, threshold, high = TRUE)$muni  ~ paste0(cell_name, "-hi"),
          TRUE ~ cell_split))
    }
    groups <- paste0(rep(inc_cells, each = 2), c("-lo", "-hi"))
    model_data <- build_revenue_stack(as.data.table(split_panel), "cell_split", c("NC", groups)) %>%
      select(all_of(model_vars)) %>%
      filter(complete.cases(.))
    model <- feols(revenue_formula, data = model_data, cluster = ~muni)
    group_coefs <- as_tibble(model$coeftable, rownames = "term") %>%
      filter(grepl("^treat_cell::", term)) %>%
      mutate(group = sub("treat_cell::([^:]+):.*", "\\1", term))
    bind_rows(lapply(groups, function(group_name) {
      cell_name <- sub("-(lo|hi)$", "", group_name)
      group_taus <- munis_in(cell_name, threshold, high = grepl("-hi$", group_name))$tau_pre
      mean_tau <- mean(group_taus, na.rm = TRUE)
      coef_row <- filter(group_coefs, group == group_name)
      beta <- if (nrow(coef_row)) coef_row$Estimate[1] else NA_real_
      tibble(split = split_name, cell = group_name, n = length(group_taus),
             tau_pre  = round(mean_tau, 3),
             beta_net = round(beta, 3),
             se       = if (nrow(coef_row)) round(coef_row$`Std. Error`[1], 3) else NA_real_,
             p        = if (nrow(coef_row)) round(coef_row$`Pr(>|t|)`[1], 4) else NA_real_,
             offset   = round(1 - beta * mean_tau, 3))
    }))
  }
  splits <- bind_rows(run_split(1.0, "cap_1pp"), run_split(0.5001, "median_0.5pp"))
  fwrite(splits, file.path(RESULTS_DIR, "eit_revenue_split.csv"))
  print(splits)
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()

# ---------------------------------------------------------------------------
# Levels vs changes: treatment variable specification check
# ---------------------------------------------------------------------------
# The headline and pooled specifications regress on the cumulative rate CHANGE
# from a pre-event baseline (dose_stack, dose_sd). Because dose and level differ
# by a municipality-specific constant, and the FE are stack-interacted at the
# municipality, the two are equivalent up to that constant: subtracting a
# unit-specific constant from a regressor leaves every coefficient unchanged
# when unit FE are present. This block runs both side by side to document that,
# and to show the one place the choice does bite -- sample retention, since the
# rate LEVEL is missing on municipality-years where the CHANGE is known to be
# zero (never-changers and pre-event years).
#
# The sample is held fixed across the two specifications so any difference is
# specification and not composition. Output: data/results/levels_vs_changes.csv
if (RUN_LEVELS_COMPARE) local({

  outcomes <- c("log_live_and_work", "log_in_commuters",
                "log_total_jobs", "share_live_in")
  stk <- copy(stk_muni)
  if (!"direction_muni" %in% names(stk)) {
    inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
    dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")
    stk[, direction_muni := fcase(
      as.character(treat_cell) %in% inc_cells, "Inc",
      as.character(treat_cell) %in% dec_cells, "Dec",
      as.character(treat_cell) == "NC", "NC", default = NA_character_)]
    stk[, direction_sd := fcase(
      as.character(cell_sd) %in% inc_cells, "Inc",
      as.character(cell_sd) %in% dec_cells, "Dec",
      as.character(cell_sd) == "NC", "NC", default = NA_character_)]
    lv <- c("NC", "Inc", "Dec")
    stk[, direction_muni := factor(direction_muni, levels = lv)]
    stk[, direction_sd   := factor(direction_sd,   levels = lv)]
  }

  sd_counterpart <- c(treat_cell = "cell_sd", direction_muni = "direction_sd")

  fit <- function(o, group_var, muni_reg, sd_reg) {
    sd_group <- sd_counterpart[[group_var]]
    cols <- intersect(c("muni", "year", "stack_muni", "stack_year",
                        group_var, sd_group,
                        muni_reg, sd_reg, o, ctrls), names(stk))
    d <- stk[, ..cols]
    if (!is.null(d[[sd_reg]])) d[is.na(get(sd_reg)), (sd_reg) := 0]
    # Fix the sample across BOTH specifications so differences are not
    # composition: require every regressor either version needs.
    need <- intersect(c("dose_stack", "dose_sd", "resident_eit", "sd_eit",
                        o, ctrls), names(stk))
    keep <- complete.cases(stk[, ..need]) & complete.cases(d)
    d <- d[keep]
    if (nrow(d) == 0) return(NULL)
    f <- as.formula(paste0(o, " ~ i(", group_var, ", ", muni_reg, ", ref='NC')",
                           " + i(", sd_group, ", ", sd_reg, ", ref='NC') + ",
                           paste(ctrls, collapse = " + "),
                           " | stack_muni + stack_year"))
    m <- suppressMessages(feols(f, data = d, cluster = ~muni, notes = FALSE))
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"), c("est", "se", "p"),
             skip_absent = TRUE)
    # Keep EVERY coefficient, controls included, not just the treatment terms.
    ct[, `:=`(outcome = o, n = nobs(m))]
    ct[, label := fifelse(
      grepl("::", term),
      paste0(fifelse(grepl(paste0("^", group_var), term), "muni ", "SD "),
             sub("^[^:]+::([^:]+):.*$", "\\1", term)),
      term)]
    ct[, is_ctrl := !grepl("::", term)]
    ct[, .(outcome, label, is_ctrl, est, se, p, n)]
  }

  rows <- list()
  for (o in outcomes) {
    for (spec in c("decomposition", "pooled")) {
      gv <- if (spec == "decomposition") "treat_cell" else "direction_muni"
      a <- fit(o, gv, "dose_stack",   "dose_sd")   # as published
      b <- fit(o, gv, "resident_eit", "sd_eit")    # levels
      if (is.null(a) || is.null(b)) next
      a[, version := "change"]; b[, version := "level"]
      rows[[length(rows) + 1L]] <- rbind(a, b)[, spec := spec]
    }
  }
  out <- rbindlist(rows, fill = TRUE)
  wide <- dcast(out, spec + outcome + label + is_ctrl ~ version,
                value.var = c("est", "se", "n"))
  wide[, diff := round(est_level - est_change, 4)]
  for (cc in c("est_change", "est_level", "se_change", "se_level"))
    wide[, (cc) := round(get(cc), 4)]
  setorder(wide, spec, outcome, is_ctrl, label)
  fwrite(wide, file.path(RESULTS_DIR, "levels_vs_changes.csv"))
  message("Saved data/results/levels_vs_changes.csv")
  for (sp in unique(wide$spec)) for (oo in unique(wide[spec == sp]$outcome)) {
    x <- wide[spec == sp & outcome == oo]
    cat(sprintf("\n--- %s | %s | N = %s ---\n", sp, oo,
                format(x$n_change[1], big.mark = ",")))
    cat(sprintf("%-34s %14s %14s\n", "", "CHANGE", "LEVEL"))
    for (i in seq_len(nrow(x))) with(x[i], cat(sprintf(
      "%-34s %8.4f (%.4f) %8.4f (%.4f)\n", label, est_change, se_change,
      est_level, se_level)))
  }
})

# Treated pre-period baselines ----
# The "Treated pre-period baseline" row in the results tables: what treated
# towns looked like in the five years before their change.
if (RUN_PRE_BASELINES) local({
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  pre_period <- stk_muni %>%
    select(all_of(c("muni", "treat_cell", "rel", "live_and_work", "in_commuters",
                    "total_jobs", "share_live_in", ctrls))) %>%
    filter(if_all(all_of(ctrls), ~ !is.na(.x))) %>%   # same rows as the models' complete controls
    filter(as.character(treat_cell) %in% inc_cells, rel >= -5, rel <= -1)
  # for the logged outcomes: a geometric mean, labeled as such in the table
  geometric_mean <- function(values) {
    values <- values[is.finite(values) & values > 0]
    if (!length(values)) NA_real_ else exp(mean(log(values)))
  }
  baselines <- tibble(
    outcome  = c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in"),
    baseline = c(geometric_mean(pre_period$live_and_work), geometric_mean(pre_period$in_commuters),
                 geometric_mean(pre_period$total_jobs), mean(pre_period$share_live_in, na.rm = TRUE)),
    kind     = c("geometric mean, workers", "geometric mean, workers",
                 "geometric mean, jobs", "mean share, proportion"),
    n_munis      = n_distinct(pre_period$muni),
    n_muni_years = nrow(pre_period))
  fwrite(baselines, file.path(RESULTS_DIR, "treated_pre_baselines.csv"))
  message("Saved data/results/treated_pre_baselines.csv")
  print(baselines)

  # add the baseline to the diagnostics files the table builder reads
  for (file_name in c("headline_diagnostics.csv", "pooled_diagnostics.csv")) {
    path <- file.path(RESULTS_DIR, file_name)
    if (!file.exists(path)) next
    fread(path) %>%
      select(-any_of("pre_baseline")) %>%
      relocate(outcome) %>%
      left_join(transmute(baselines, outcome, pre_baseline = baseline), by = "outcome") %>%
      arrange(outcome) %>%
      fwrite(path)
    message("  merged pre_baseline into ", file_name)
  }
})

# Flexible dose ----
# Appendix A: the headline with a quadratic and a spline in the dose instead of
# a straight line, to see whether bigger increases bite differently.
if (RUN_FLEX_DOSE) local({
  suppressMessages(library(splines))
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  to_direction <- function(cell) {
    cell <- as.character(cell)
    factor(case_when(cell %in% inc_cells ~ "Inc", cell == "NC" ~ "NC"), levels = c("NC", "Inc"))
  }
  # the dose as a change here: for a curve, level and change are not interchangeable
  flex_data <- stk_muni %>%
    mutate(dose_sd = coalesce(dose_sd, 0),
           dose    = if_else(as.character(treat_cell) %in% inc_cells, dose_change, 0),
           dose2   = dose^2)
  treated_dose   <- flex_data$dose[flex_data$dose > 0]
  interior_knots <- quantile(treated_dose, c(1/3, 2/3), na.rm = TRUE)   # spline bends at the dose terciles
  boundary_knots <- c(0, max(treated_dose, na.rm = TRUE))
  spline_basis   <- ns(flex_data$dose, knots = interior_knots, Boundary.knots = boundary_knots)
  basis_cols     <- paste0("sp", seq_len(ncol(spline_basis)))
  flex_data <- flex_data %>%
    bind_cols(setNames(as_tibble(unclass(spline_basis)), basis_cols)) %>%
    mutate(direction_muni = to_direction(treat_cell),
           direction_sd   = to_direction(cell_sd)) %>%
    as.data.table()

  outcomes <- c("log_live_and_work", "share_live_in", "log_in_commuters", "log_total_jobs")
  # omega(d): one weight per treated muni, at its dose (kept in data.table so the
  # munis stay in first-appearance order, which the sums below depend on to the last digit)
  treated_doses <- function(cells) {
    doses <- unique(flex_data[as.character(treat_cell) %in% cells & dose > 0, .(muni, dose)])
    doses[, .(dose = max(dose)), by = muni]$dose
  }
  # slope of each spline basis column at the given doses (central difference)
  basis_slope <- function(doses, h = 1e-5) {
    upper <- pmin(doses + h, boundary_knots[2]); lower <- pmax(doses - h, 0)
    (predict(spline_basis, upper) - predict(spline_basis, lower)) / (upper - lower)
  }
  # the ACR is a weighted sum of coefficients w'b, so its SE is sqrt(w'Vw);
  # the p-value uses the model's t degrees of freedom (the cluster count)
  lincom <- function(model, terms, weights) {
    if (!all(terms %in% names(coef(model)))) return(c(NA_real_, NA_real_, NA_real_))
    coefs <- coef(model)[terms]
    term_vcov <- vcov(model)[terms, terms, drop = FALSE]
    estimate <- as.numeric(weights %*% coefs)
    se <- sqrt(as.numeric(t(weights) %*% term_vcov %*% weights))
    df_t <- tryCatch(fixest::degrees_freedom(model, "t"), error = function(e) Inf)
    c(estimate, se, 2 * stats::pt(-abs(estimate / se), df = df_t))
  }

  rows <- list()
  for (spec in c("decomp", "pooled")) {
    cell_var    <- if (spec == "decomp") "treat_cell" else "direction_muni"
    sd_cell_var <- if (spec == "decomp") "cell_sd"    else "direction_sd"
    groups      <- if (spec == "decomp") inc_cells    else "Inc"
    for (outcome in outcomes) {
      # dose_stack (the headline's level regressor) is in the complete-cases set
      # though no formula uses it: it keeps this sample equal to the headline's
      model_data <- flex_data %>%
        select(all_of(c("muni", cell_var, sd_cell_var, "dose", "dose2", basis_cols, "dose_sd",
                        "dose_stack", "stack_muni", "stack_year", outcome, ctrls))) %>%
        filter(complete.cases(.))
      make_formula <- function(rhs) {
        as.formula(paste0(outcome, " ~ ", rhs, " + ",
                          sprintf("i(%s, dose_sd, ref='NC')", sd_cell_var), " + ",
                          paste(ctrls, collapse = " + "), " | stack_muni + stack_year"))
      }
      linear_model <- feols(make_formula(sprintf("i(%s, dose, ref='NC')", cell_var)),
                            model_data, cluster = ~muni, notes = FALSE)
      quad_model   <- feols(make_formula(sprintf("i(%s, dose, ref='NC') + i(%s, dose2, ref='NC')",
                                                 cell_var, cell_var)),
                            model_data, cluster = ~muni, notes = FALSE)
      spline_model <- feols(make_formula(paste(sprintf("i(%s, %s, ref='NC')", cell_var, basis_cols),
                                               collapse = " + ")),
                            model_data, cluster = ~muni, notes = FALSE)
      for (group in groups) {
        doses <- if (spec == "decomp") treated_doses(group) else treated_doses(inc_cells)
        if (!length(doses)) next
        # each fit_* is c(estimate, se, p). Not named acr_*: tibble() would read
        # the new acr_* column instead of the local
        fit_linear <- lincom(linear_model, sprintf("%s::%s:dose", cell_var, group), 1)
        fit_quad   <- lincom(quad_model, sprintf("%s::%s:%s", cell_var, group, c("dose", "dose2")),
                             c(1, 2 * mean(doses)))
        fit_spline <- lincom(spline_model, sprintf("%s::%s:%s", cell_var, group, basis_cols),
                             colMeans(basis_slope(doses)))
        rows[[length(rows) + 1L]] <- tibble(
          spec = spec, cell = group, outcome = outcome,
          acr_linear = fit_linear[1], se_linear = fit_linear[2], p_linear = fit_linear[3],
          acr_quad   = fit_quad[1],   se_quad   = fit_quad[2],   p_quad   = fit_quad[3],
          acr_spline = fit_spline[1], se_spline = fit_spline[2], p_spline = fit_spline[3],
          n = nobs(linear_model), n_treated = length(doses), mean_dose = mean(doses))
      }
    }
  }
  acr_table <- bind_rows(rows)
  fwrite(acr_table, file.path(RESULTS_DIR, "flex_dose_acr.csv"))
  message("[flex-dose] ACR under three dose specifications:")
  acr_table %>%
    filter(outcome == "log_live_and_work") %>%
    transmute(spec, cell, linear = round(acr_linear, 3), quad = round(acr_quad, 3),
              spline = round(acr_spline, 3), n_treated, mean_dose = round(mean_dose, 3)) %>%
    print()

  # Gate: the linear column must reproduce the headline, otherwise the other two
  # columns are not the same object under a looser functional form
  check <- acr_table %>%
    filter(spec == "decomp") %>%
    select(cell, outcome, acr_linear) %>%
    inner_join(fread(file.path(RESULTS_DIR, "headline_full_table.csv")) %>%
                 filter(grepl("^treat_cell::", term)) %>%
                 transmute(cell = sub(".*::(.*):dose_stack", "\\1", term), outcome, headline = est),
               by = c("cell", "outcome")) %>%
    mutate(gap = abs(acr_linear - headline))
  if (nrow(check)) message(sprintf("[flex-dose] max |linear - headline| = %.6f across %d rows",
                                   max(check$gap), nrow(check)))
  stopifnot("flex-dose linear column does not reproduce the headline" = all(check$gap < 1e-6))
  message("Saved flex_dose_acr.csv")
})


# Revenue tradeoff (RUN_REVENUE_TRADEOFF) ----
# The separate RUN_REVENUE section above builds its own stack with the dose in
# CHANGES, so its sample (190,760) is not the headline's (171,451) and its
# coefficients cannot be divided into the worker coefficients. This section
# estimates BOTH outcomes on the shared headline stack, one complete-cases set,
# identical controls / FE / clustering, so the ratio is well defined.
#   rho = beta_workers / beta_revenue  = log workers lost per log revenue gained.
# beta_revenue is mechanical PLUS behavioural (revenue = rate x base), so rho is
# workers per unit of REALIZED revenue, not a behavioural elasticity ratio.
# Inference on the ratio uses a municipality cluster bootstrap, because a
# delta-method interval misbehaves exactly where the denominator is small.
if (RUN_REVENUE_TRADEOFF) local({
  ACT32_YEAR <- 2012L
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  outs <- c("log_live_and_work", "log_eit_revenue")
  stk <- copy(stk_muni)
  stk[is.na(dose_sd), dose_sd := 0]
  stk[, direction_muni := factor(fcase(as.character(treat_cell) %in% inc_cells, "Inc",
                                       as.character(treat_cell) == "NC", "NC",
                                       default = NA_character_), levels = c("NC", "Inc"))]
  stk[, direction_sd := factor(fcase(as.character(cell_sd) %in% inc_cells, "Inc",
                                     as.character(cell_sd) == "NC", "NC",
                                     default = NA_character_), levels = c("NC", "Inc"))]
  # tau0 = the municipality's own pre-event RATE LEVEL inside its stack window.
  # NOT base_rate: that is the pre-event mean of the dose, which is zero by
  # construction. dose_stack carries the resident rate level, so average it
  # over the pre-event years of each stack.
  stk[, tau0 := mean(dose_stack[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(tau0), tau0 := NA_real_]
  stk[, dose_x_tau0 := dose_stack * tau0]
  # share_eit is EIT revenue / total revenue, mechanically the outcome; drop it.
  ctrls_r <- setdiff(ctrls, "share_eit")

  cols <- c("muni", "year", "stack", "treat_cell", "cell_sd", "direction_muni", "direction_sd",
            "dose_stack", "dose_sd", "tau0", "dose_x_tau0",
            "stack_muni", "stack_year", outs, ctrls_r)
  d <- stk[, ..cols]; d <- d[complete.cases(d)]
  message(sprintf("[tradeoff] shared complete-cases sample: %s rows, %d munis",
                  format(nrow(d), big.mark = ","), uniqueN(d$muni)))

  fml <- function(y, cellv, sdv, extra = "") as.formula(paste0(
    y, " ~ i(", cellv, ", dose_stack, ref='NC')", extra,
    " + i(", sdv, ", dose_sd, ref='NC') + ", paste(ctrls_r, collapse = " + "),
    " | stack_muni + stack_year"))

  fit <- function(dd, y, spec, extra = "") {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    sdv   <- if (spec == "decomp") "cell_sd"    else "direction_sd"
    feols(fml(y, cellv, sdv, extra), dd, cluster = ~muni, notes = FALSE)
  }
  cf <- function(m, cellv, g) {
    nm <- sprintf("%s::%s:dose_stack", cellv, g)
    if (nm %in% names(coef(m))) unname(coef(m)[nm]) else NA_real_
  }

  rows <- list()
  for (spec in c("pooled", "decomp")) {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    grps  <- if (spec == "decomp") inc_cells    else "Inc"
    mW <- fit(d, outs[1], spec); mR <- fit(d, outs[2], spec)
    for (g in grps) {
      bW <- cf(mW, cellv, g); bR <- cf(mR, cellv, g)
      seW <- unname(se(mW)[sprintf("%s::%s:dose_stack", cellv, g)])
      seR <- unname(se(mR)[sprintf("%s::%s:dose_stack", cellv, g)])
      tau_bar <- mean(d[get(cellv) == g & dose_stack > 0, tau0], na.rm = TRUE)
      rows[[length(rows) + 1]] <- data.table(
        spec = spec, cell = g, beta_workers = bW, se_workers = seW,
        beta_revenue = bR, se_revenue = seR, rho = bW / bR,
        tau0_mean = tau_bar, mechanical = 1 / tau_bar,
        offset = 1 - bR * tau_bar, n = nobs(mW))
    }
  }
  res <- rbindlist(rows)

  # Cluster bootstrap for the ratio: resample municipalities with replacement,
  # refit BOTH equations on the same draw so the ratio inherits the covariance.
  set.seed(20260814)
  B <- 200L; munis <- unique(d$muni)
  boot <- vector("list", B)
  for (b in seq_len(B)) {
    pick <- data.table(muni = sample(munis, length(munis), replace = TRUE))
    pick[, rep_id := seq_len(.N)]
    db <- merge(d, pick, by = "muni", allow.cartesian = TRUE)
    # Only the UNIT fixed effect is replicated. stack_year is a time effect and
    # must stay shared across draws, otherwise every cell becomes a singleton.
    db[, stack_muni := paste0(stack_muni, "_", rep_id)]
    o <- lapply(c("pooled", "decomp"), function(spec) {
      cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
      grps  <- if (spec == "decomp") inc_cells    else "Inc"
      mW <- tryCatch(fit(db, outs[1], spec), error = function(e) {
              message("[tradeoff] bootstrap fit failed: ", conditionMessage(e)); NULL })
      mR <- tryCatch(fit(db, outs[2], spec), error = function(e) NULL)
      if (is.null(mW) || is.null(mR)) return(NULL)
      rbindlist(lapply(grps, function(g) data.table(spec = spec, cell = g,
        rho = cf(mW, cellv, g) / cf(mR, cellv, g))))
    })
    boot[[b]] <- rbindlist(o[!sapply(o, is.null)])
    if (b %% 50 == 0) message(sprintf("[tradeoff] bootstrap %d/%d", b, B))
  }
  bt <- rbindlist(boot)
  ci <- bt[is.finite(rho), .(rho_lo = quantile(rho, .025, na.rm = TRUE),
                             rho_hi = quantile(rho, .975, na.rm = TRUE),
                             B_ok = .N), by = .(spec, cell)]
  res <- merge(res, ci, by = c("spec", "cell"), all.x = TRUE)
  fwrite(res, file.path(RESULTS_DIR, "revenue_tradeoff.csv"))

  # How the tradeoff moves with the rate already levied: interact the dose with
  # tau0 in BOTH equations, so beta(tau0) = b_main + b_int * tau0 for each.
  extra <- " + i(treat_cell, dose_x_tau0, ref='NC')"
  gW <- fit(d, outs[1], "decomp", extra); gR <- fit(d, outs[2], "decomp", extra)
  grad <- rbindlist(lapply(inc_cells, function(g) {
    nm1 <- sprintf("treat_cell::%s:dose_stack", g)
    nm2 <- sprintf("treat_cell::%s:dose_x_tau0", g)
    ok <- all(c(nm1, nm2) %in% names(coef(gW))) && all(c(nm1, nm2) %in% names(coef(gR)))
    if (!ok) return(NULL)
    tg <- seq(quantile(d[treat_cell == g & dose_stack > 0, tau0], .05, na.rm = TRUE),
              quantile(d[treat_cell == g & dose_stack > 0, tau0], .95, na.rm = TRUE),
              length.out = 40)
    # beta(tau0) = b_main + b_int * tau0 is a linear combination, so its
    # variance is w'Vw with w = (1, tau0).
    lc <- function(m) {
      b <- coef(m)[c(nm1, nm2)]; V <- vcov(m)[c(nm1, nm2), c(nm1, nm2)]
      est <- b[1] + b[2] * tg
      sev <- sqrt(pmax(V[1,1] + 2 * tg * V[1,2] + tg^2 * V[2,2], 0))
      list(est = est, se = sev)
    }
    lw <- lc(gW); lr <- lc(gR)
    data.table(cell = g, tau0 = tg,
               beta_workers = lw$est, se_workers = lw$se,
               beta_revenue = lr$est, se_revenue = lr$se,
               mechanical = 1 / tg)
  }))
  if (nrow(grad)) {
    grad[, `:=`(rho = beta_workers / beta_revenue,
                w_lo = beta_workers - 1.96 * se_workers,
                w_hi = beta_workers + 1.96 * se_workers,
                r_lo = beta_revenue - 1.96 * se_revenue,
                r_hi = beta_revenue + 1.96 * se_revenue)]
    fwrite(grad, file.path(RESULTS_DIR, "revenue_tradeoff_gradient.csv"))
    # Where the revenue response crosses zero is the implied Laffer peak.
    peak <- grad[, {
      i <- which(diff(sign(beta_revenue)) != 0)[1]
      .(tau_peak = if (is.na(i)) NA_real_ else tau0[i], beta_r_min = min(beta_revenue))
    }, by = cell]
    print(peak)
  }
  # Spline dose-response for both outcomes ------------------------------------
  # The linear block above uses the rate LEVEL, matching the headline. A spline
  # cannot: level and change differ by a municipality constant, which unit FE
  # absorb for a linear term but not for a nonlinear one (see Appendix A). So
  # the flexible fit enters the cumulative CHANGE, exactly as RUN_FLEX_DOSE does.
  suppressMessages(library(splines))
  stk[, dose_ch := fifelse(as.character(treat_cell) %in% inc_cells, dose_change, 0)]
  kk <- quantile(stk[dose_ch > 0, dose_ch], c(1/3, 2/3), na.rm = TRUE)
  bb <- c(0, max(stk[dose_ch > 0, dose_ch], na.rm = TRUE))
  Bs <- ns(stk$dose_ch, knots = kk, Boundary.knots = bb)
  bn <- paste0("sp", seq_len(ncol(Bs)))
  stk[, (bn) := as.data.table(unclass(Bs))]
  cols_s <- c(cols, "dose_ch", bn)
  ds <- stk[, ..cols_s]; ds <- ds[complete.cases(ds)]
  message(sprintf("[tradeoff] spline sample: %s rows (linear sample %s)",
                  format(nrow(ds), big.mark = ","), format(nrow(d), big.mark = ",")))
  fml_s <- function(y, cellv, sdv) as.formula(paste0(
    y, " ~ ", paste(sprintf("i(%s, %s, ref='NC')", cellv, bn), collapse = " + "),
    " + i(", sdv, ", dose_sd, ref='NC') + ", paste(ctrls_r, collapse = " + "),
    " | stack_muni + stack_year"))
  # ATT(d) = (B(d) - B(0))'b, variance w'Vw.
  att_spline <- function(m, cellv, g, dgrid) {
    nm <- sprintf("%s::%s:%s", cellv, g, bn)
    if (!all(nm %in% names(coef(m)))) return(NULL)
    b <- coef(m)[nm]; V <- vcov(m)[nm, nm, drop = FALSE]
    W <- predict(Bs, dgrid)
    W <- sweep(W, 2, as.numeric(predict(Bs, 0)), "-")
    est <- as.numeric(W %*% b)
    sev <- sqrt(pmax(rowSums((W %*% V) * W), 0))
    data.table(dose = dgrid, est = est, lo = est - 1.96 * sev, hi = est + 1.96 * sev)
  }
  spl <- rbindlist(lapply(c("pooled", "decomp"), function(spec) {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    sdv   <- if (spec == "decomp") "cell_sd"    else "direction_sd"
    grps  <- if (spec == "decomp") inc_cells    else "Inc"
    mW <- feols(fml_s(outs[1], cellv, sdv), ds, cluster = ~muni, notes = FALSE)
    mR <- feols(fml_s(outs[2], cellv, sdv), ds, cluster = ~muni, notes = FALSE)
    rbindlist(lapply(grps, function(g) {
      # Evaluate each cell only over the doses it actually contains. The basis
      # is fitted on all treated doses, which run to the Inc-becomes maximum,
      # so a common grid would extrapolate Inc-bind far past its own support.
      dmax_g <- if (spec == "decomp") ds[treat_cell == g, max(dose_ch, na.rm = TRUE)]
                else ds[direction_muni == "Inc", max(dose_ch, na.rm = TRUE)]
      if (!is.finite(dmax_g) || dmax_g <= 0) return(NULL)
      dgrid <- seq(0, dmax_g, length.out = 60)
      aW <- att_spline(mW, cellv, g, dgrid); aR <- att_spline(mR, cellv, g, dgrid)
      if (is.null(aW) || is.null(aR)) return(NULL)
      rbind(cbind(spec = spec, cell = g, outcome = "workers", aW),
            cbind(spec = spec, cell = g, outcome = "revenue", aR))
    }), fill = TRUE)
  }), fill = TRUE)
  if (nrow(spl)) {
    fwrite(spl, file.path(RESULTS_DIR, "revenue_tradeoff_spline.csv"))
    # rho(d) = ATT_workers(d) / ATT_revenue(d): under linearity this is constant,
    # under the spline it is allowed to move with the dose.
    wide <- dcast(spl, spec + cell + dose ~ outcome, value.var = "est")
    wide[, rho_d := workers / revenue]
    fwrite(wide, file.path(RESULTS_DIR, "revenue_tradeoff_spline_rho.csv"))
  }

  # Dose-response curves for both outcomes, for the figures.
  dr <- rbindlist(lapply(seq_len(nrow(res)), function(i) {
    r <- res[i]; dmax <- max(d[[ "dose_stack" ]], na.rm = TRUE)
    dd <- seq(0, dmax, length.out = 60)
    rbind(data.table(spec = r$spec, cell = r$cell, outcome = "workers", dose = dd,
                     y = r$beta_workers * dd,
                     lo = (r$beta_workers - 1.96 * r$se_workers) * dd,
                     hi = (r$beta_workers + 1.96 * r$se_workers) * dd),
          data.table(spec = r$spec, cell = r$cell, outcome = "revenue", dose = dd,
                     y = r$beta_revenue * dd,
                     lo = (r$beta_revenue - 1.96 * r$se_revenue) * dd,
                     hi = (r$beta_revenue + 1.96 * r$se_revenue) * dd))
  }))
  fwrite(dr, file.path(RESULTS_DIR, "revenue_tradeoff_doseresponse.csv"))
  # Gates 3 and 4: is the revenue result an artefact of the AFR schema break or
  # of the Act 32 compliance shock? Re-estimate both outcomes on restricted
  # windows and compare. No new specification, only the sample moves.
  #   schema : AFR switches column names and ID at 2005/2006. Post-2006 only
  #            uses one vintage, so agreement means the stitch is not driving it.
  #   act32  : Bagchi finds EIT collections jump ~13% at 2012 from compliance
  #            alone, heterogeneous across districts. Pre/post and a 2013+ window
  #            check whether the revenue response survives away from the break.
  gate <- function(lab, dd) {
    if (!nrow(dd)) return(NULL)
    rbindlist(lapply(c("pooled", "decomp"), function(spec) {
      cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
      grps  <- if (spec == "decomp") inc_cells    else "Inc"
      mW <- tryCatch(fit(dd, outs[1], spec), error = function(e) NULL)
      mR <- tryCatch(fit(dd, outs[2], spec), error = function(e) NULL)
      if (is.null(mW) || is.null(mR)) return(NULL)
      rbindlist(lapply(grps, function(g) {
        bW <- cf(mW, cellv, g); bR <- cf(mR, cellv, g)
        data.table(sample = lab, spec = spec, cell = g,
                   beta_workers = bW,
                   se_workers = unname(se(mW)[sprintf("%s::%s:dose_stack", cellv, g)]),
                   beta_revenue = bR,
                   se_revenue = unname(se(mR)[sprintf("%s::%s:dose_stack", cellv, g)]),
                   rho = bW / bR, n = nobs(mW))
      }))
    }), fill = TRUE)
  }
  # Gate 4 splits by COHORT, keeping every never-changer in both periods, which
  # is how RUN_ACT32 in 3_models/3_robustness.R does it. Splitting on calendar year
  # instead truncates each stack's own pre-period and destroys the within-stack
  # comparison, which produced sign flips on a first pass.
  gates <- rbindlist(list(
    gate("full",                    d),
    gate("gate3 schema 2006-2019",  d[year >= 2006]),
    gate("gate3 schema 2010-2019",  d[year >= 2010]),
    gate("gate4 pre-Act32 cohorts",  d[treat_cell == "NC" | stack <  ACT32_YEAR]),
    gate("gate4 post-Act32 cohorts", d[treat_cell == "NC" | stack >= ACT32_YEAR])),
    fill = TRUE)
  fwrite(gates, file.path(RESULTS_DIR, "revenue_tradeoff_gates.csv"))
  cat("\n---- gates 3 and 4: sample restrictions ----\n")
  print(gates[spec == "decomp" & cell == "Inc-bind" |
              spec == "pooled",
              .(sample, spec, cell, beta_workers, beta_revenue, rho, n)])

  print(res[, .(spec, cell, beta_workers, beta_revenue, rho, rho_lo, rho_hi,
                tau0_mean, offset, n)])
  message("Saved revenue_tradeoff.csv / _gradient.csv / _doseresponse.csv / _spline.csv")
})
