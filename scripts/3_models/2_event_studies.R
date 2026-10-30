#!/usr/bin/env Rscript
# 2_event_studies.R -- realized-dose event studies, by cell and pooled.
# For each muni-side cell c, on the stack restricted to {NC, c}:
#   y = alpha_{stack,muni} + gamma_{stack,year}
#       + sum_{k != -1} beta_k 1{rel = k} x realized_dose_muni
#       + SD cell x dose + controls
# realized_dose_muni is each treated muni's rate change from c-1 to c+5, a
# constant per muni, so the leads test strong parallel trends: eventual dose
# size should not predict outcomes before the event.
# Writes data/results/: headline_es_coefs, headline_es_att,
#   pooled_es_cousub_coefs, pooled_es_cousub_att (.csv)
# Run: Rscript scripts/3_models/2_event_studies.R

suppressMessages({
  library(dplyr)   # before data.table so data.table's first/last/between win
  library(data.table)
  library(fixest)
})

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
RUN_HEADLINE_ES <- TRUE
RUN_POOLED_ES   <- TRUE

# Shared setup ----
env <- build_headline_env()
panel <- env$panel
ctrls <- env$ctrls
ctrls_str <- env$ctrls_str
K_stack <- 5L

cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                 "Dec-bind", "Dec-becomes-non", "Dec-nobind")

# Event-study stack ----
# Same stacking as the headline, on the panel with realized_dose_muni added.
if (RUN_HEADLINE_ES || RUN_POOLED_ES) {
  es_panel <- add_realized_dose(copy(panel), K = K_stack)

  build_es_stack <- function(data, K = K_stack) {
    treated_munis <- unique(data[cell_muni != "NC", .(muni, cohort = c_muni, cell = cell_muni)])
    nc_munis <- unique(data[cell_muni == "NC", muni])
    rbindlist(lapply(sort(unique(treated_munis$cohort)), function(cohort_year) {
      window_years <- (cohort_year - K):(cohort_year + K)
      cohort_munis <- treated_munis[cohort == cohort_year]
      if (nrow(cohort_munis) == 0) return(NULL)
      treated_rows <- data[muni %in% cohort_munis$muni & year %in% window_years]
      treated_rows[, treat_cell_muni := cell_muni]
      control_rows <- data[muni %in% nc_munis & year %in% window_years]
      control_rows[, treat_cell_muni := "NC"]
      stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
      stack_rows[, stack := cohort_year]; stack_rows[, rel := as.integer(year - cohort_year)]
      stack_rows
    }), fill = TRUE)
  }

  es_stack <- build_es_stack(es_panel)
  es_stack[, stack_muni := paste0(stack, "_", muni)]
  es_stack[, stack_year := paste0(stack, "_", year)]
  es_stack[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
  es_stack[, cell_sd := factor(cell_sd, levels = cell_levels)]
  es_stack[, cell_nr := factor(cell_nr, levels = cell_levels)]
  # SD and nonresident doses are already 0 before each event; this only fills rows with no cell tag
  es_stack[is.na(dose_sd), dose_sd := 0]
  es_stack[is.na(dose_nr), dose_nr := 0]
}

# event-time coefficients from one model
event_coefs <- function(model) {
  as_tibble(model$coeftable, rownames = "term") %>%
    filter(grepl("^rel::", term)) %>%
    transmute(rel = as.integer(sub("rel::(-?\\d+):.*", "\\1", term)),
              est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)
}
# SE of an average of event-time coefficients, treating them as independent
# (it ignores their covariances, so it is an approximation)
mean_se <- function(ses) sqrt(mean(ses^2, na.rm = TRUE) / max(length(ses), 1))
es_formula <- function(outcome) {
  as.formula(paste0(outcome, " ~ i(rel, realized_dose_muni, ref = -1) ",
                    "+ i(cell_sd, dose_sd, ref = 'NC') + ",
                    ctrls_str, " | stack_muni + stack_year"))
}

# Headline realized-dose ES ----
# Same sample and right-hand side as the headline, so the pre-trend test is
# about the regression it supports: dose_stack is in the complete-cases set
# (the headline needs it non-missing), and the nonresident terms are left out.
if (RUN_HEADLINE_ES) local({
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs",
                "share_live_in", "share_work_outside")
  coef_rows <- list()
  summary_rows <- list()
  for (outcome in outcomes) {
    for (cell_name in setdiff(cell_levels, "NC")) {
      model_data <- es_stack %>%
        filter(treat_cell_muni %in% c("NC", cell_name)) %>%
        select(any_of(c("muni", "year", "stack_muni", "stack_year", "treat_cell_muni",
                        "realized_dose_muni", "dose_stack", "rel",
                        "cell_sd", "dose_sd", outcome, ctrls))) %>%
        filter(complete.cases(.))
      if (nrow(model_data) == 0 || sum(model_data$treat_cell_muni == cell_name) == 0) next
      model <- tryCatch(feols(es_formula(outcome), data = model_data, cluster = ~muni),
                        error = function(e) NULL)
      if (is.null(model)) next
      coefs <- event_coefs(model)
      if (nrow(coefs) == 0) next
      coef_rows[[length(coef_rows) + 1L]] <- coefs %>%
        mutate(outcome = outcome, cell = cell_name, n = nobs(model))
      post <- filter(coefs, rel >= 0, rel <= 5)
      pre  <- filter(coefs, rel <= -2)
      wald <- wald_pretrend(model, "^rel::-[2-9]")
      summary_rows[[length(summary_rows) + 1L]] <- tibble(
        outcome = outcome, cell = cell_name,
        att_post_per_pp = round(mean(post$est, na.rm = TRUE), 3),
        att_post_se     = round(mean_se(post$se), 3),
        att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
        att_pre_se      = round(mean_se(pre$se), 3),
        wald_pre_stat   = round(wald$stat, 3),
        wald_pre_df     = wald$df,
        wald_pre_p      = round(wald$p, 4),
        n               = nobs(model),
        n_munis         = n_distinct(model_data$muni),
        r2              = round(fitstat(model, "r2",  simplify = TRUE), 4),
        wr2             = round(fitstat(model, "wr2", simplify = TRUE), 4))
    }
  }
  es_summary <- bind_rows(summary_rows)
  fwrite(bind_rows(coef_rows), file.path(RESULTS_DIR, "headline_es_coefs.csv"))
  fwrite(es_summary, file.path(RESULTS_DIR, "headline_es_att.csv"))
  print(es_summary)
})

# Pooled realized-dose ES ----
# All increases (or all decreases) together against NC, same stack and
# controls. Unlike the headline ES, dose_stack is not in the complete-cases
# set, so this sample is about 1,300 rows larger than the pooled regression's.
if (RUN_POOLED_ES) local({
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs",
                "share_live_in", "share_work_outside")

  run_pooled_es <- function(direction) {
    pooled_cells <- if (direction == "Inc") inc_cells else dec_cells
    direction_data <- filter(es_stack, as.character(treat_cell_muni) %in% c(pooled_cells, "NC"))
    coef_rows <- list()
    summary_rows <- list()
    for (outcome in outcomes) {
      model_data <- direction_data %>%
        select(any_of(c("muni", "year", "stack", "stack_muni", "stack_year", "rel",
                        "realized_dose_muni", "cell_sd", "dose_sd", outcome, ctrls))) %>%
        filter(complete.cases(.))
      if (nrow(model_data) == 0) next
      model <- tryCatch(feols(es_formula(outcome), data = model_data, cluster = ~muni),
                        error = function(e) NULL)
      if (is.null(model)) next
      coefs <- event_coefs(model)
      if (nrow(coefs) == 0) next
      coef_rows[[length(coef_rows) + 1L]] <- coefs %>%
        mutate(outcome = outcome, direction = direction, n = nobs(model))
      post <- filter(coefs, rel >= 0, rel <= 5)
      pre  <- filter(coefs, rel <= -2)
      wald <- wald_pretrend(model, "^rel::-[2-9]")
      dep_var <- model$fitted.values + model$residuals
      summary_rows[[length(summary_rows) + 1L]] <- tibble(
        outcome = outcome, direction = direction,
        att_post      = round(mean(post$est, na.rm = TRUE), 3),
        att_post_se   = round(mean_se(post$se), 3),
        att_pre       = round(mean(pre$est, na.rm = TRUE), 3),
        att_pre_se    = round(mean_se(pre$se), 3),
        wald_pre_stat = round(wald$stat, 3),
        wald_pre_df   = wald$df,
        wald_pre_p    = round(wald$p, 4),
        n             = nobs(model),
        r2            = round(fitstat(model, "r2", simplify = TRUE), 4),
        wr2           = round(fitstat(model, "wr2", simplify = TRUE), 4),
        dep_mean      = round(mean(dep_var, na.rm = TRUE), 3),
        dep_sd        = round(sd(dep_var, na.rm = TRUE), 3))
    }
    list(coefs = bind_rows(coef_rows), summary = bind_rows(summary_rows))
  }

  increases <- run_pooled_es("Inc")
  decreases <- run_pooled_es("Dec")   # empty: decrease munis are dropped in build_headline_env()
  fwrite(bind_rows(increases$coefs, decreases$coefs),
         file.path(RESULTS_DIR, "pooled_es_cousub_coefs.csv"))
  pooled_summary <- bind_rows(increases$summary, decreases$summary)
  fwrite(pooled_summary, file.path(RESULTS_DIR, "pooled_es_cousub_att.csv"))
  print(pooled_summary)
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
