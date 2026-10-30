#!/usr/bin/env Rscript
# 3_robustness.R
# The robustness checks in Section 6 and their pooled versions in the appendix.
# Results go to data/results/. Takes about 45 minutes, almost all of it the two
# dCDH sections. PARKED sections are not in the paper.

suppressMessages({
  library(dplyr)   # before data.table so data.table's first/last/between win
  library(data.table)
  library(fixest)
  library(ggplot2)
})
# the dCDH package is slow to load, so only the dCDH sections load it

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
RUN_DCDH_POOLED <- TRUE   # ~5 min
RUN_DCDH_BYCELL <- TRUE   # ~30 min
RUN_ACT32       <- TRUE
RUN_ACT47       <- TRUE
RUN_PRETREND_BATTERY <- TRUE   # 4-test battery per cell (moved from stage 03)
# DROPPED FROM THE PAPER 2026-07-30. Section 6.5 (dose-permutation
# randomization inference) was cut, so this block no longer feeds anything.
# The code is left in place, gated off, rather than deleted.
RUN_DOSE_PERMUTATION <- FALSE  # randomization inference: shuffle dose magnitudes (~15 min)
RUN_FAKE_TIMING      <- TRUE   # placebo cohorts 4 years early, pre-event data only
RUN_POOLED_ROBUSTNESS <- TRUE  # the same battery, run on the pooled Inc specification
RUN_EVENT_BY_EVENT   <- FALSE  # PARKED 2026-10-29, not in the paper: one ATT per treated municipality (Cengiz et al. 2019, Online Appendix C)
RUN_LEVEL_ADJUST     <- FALSE  # PARKED 2026-10-29, not in the paper: baseline-rate trend adjustment: tau0 x time (conditional parallel trends)
RUN_STACK_QWEIGHTS   <- FALSE  # PARKED 2026-10-29, not in the paper: Wing-Freedman-Hollingsworth corrective Q-weights on the stacked design

# Shared setup ----
env <- build_headline_env()
panel <- env$panel
stk_muni <- env$stk_muni
ctrls <- env$ctrls
ctrls_str <- env$ctrls_str
cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                 "Dec-bind", "Dec-becomes-non", "Dec-nobind")

# Pennsylvania's financially distressed towns (Act 47) and the year each was
# designated, from DCED's register (dced.pa.gov, retrieved 2026-07-30).
act47 <- tibble(
  name = c("NEWVILLE", "CHESTER", "HARRISBURG",
           "SHENANDOAH", "AMBRIDGE", "WILKINSBURG", "EAST PITTSBURGH",
           "NORTH BRADDOCK", "HOMESTEAD", "MILLBOURNE", "WESTFALL",
           "WEST HAZLETON", "NANTICOKE", "CLAIRTON", "PLYMOUTH", "ALTOONA",
           "PITTSBURGH", "FARRELL", "SCRANTON", "COLWYN", "READING",
           "FRANKLIN", "MAHANOY CITY", "JOHNSTOWN", "HAZLETON", "RANKIN",
           "BRADDOCK", "DUQUESNE", "GREENVILLE", "ALIQUIPPA", "NEW CASTLE",
           "SHAMOKIN"),
  designated = c(2014L, 1995L, 2010L,
                 1988L, 1990L, 1988L, 1992L,
                 1995L, 1993L, 1993L, 2009L,
                 2003L, 2006L, 1988L, 2004L, 2012L,
                 2003L, 1987L, 1992L, 2015L, 2009L,
                 1988L, 2016L, 1992L, 2017L, 1989L,
                 1988L, 1991L, 2002L, 1987L, 2007L,
                 2014L))
# DCED spells names with different punctuation in different files, so compare them stripped down
normalize_name <- function(names) trimws(gsub("\\s+", " ", gsub("[^A-Z0-9 ]", " ", toupper(names))))
act47$name_norm <- normalize_name(act47$name)
# TRUE for each town name that contains an Act 47 name
matches_act47 <- function(names_norm) {
  vapply(names_norm, function(name) any(vapply(act47$name_norm, function(a47)
    grepl(a47, name, fixed = TRUE), logical(1))), logical(1), USE.NAMES = FALSE)
}

# Turn the dCDH output into one row per year: the pre-change years (placebos),
# a zero at the change, then the effects. These are effects of the whole increase
# each town got, not per point: multiply the headline by the mean dose (~0.33) to compare.
extract_dcdh <- function(model) {
  results <- model$results
  horizon_rows <- function(estimates, sign = 1L) {
    estimates <- as.data.frame(estimates)
    rows <- tibble(rel = sign * seq_len(nrow(estimates)), est = estimates$Estimate,
                   se = estimates$SE, ci_lo = estimates$`LB CI`, ci_hi = estimates$`UB CI`)
    if ("N" %in% names(estimates))         rows$n_obs       <- estimates$N
    if ("Switchers" %in% names(estimates)) rows$n_switchers <- estimates$Switchers
    rows
  }
  placebos <- if (!is.null(results$Placebos) && results$N_Placebos > 0)
                horizon_rows(results$Placebos, -1L) else NULL
  reference <- tibble(rel = 0L, est = 0, se = 0, ci_lo = 0, ci_hi = 0)
  bind_rows(placebos, reference, horizon_rows(results$Effects)) %>% arrange(rel)
}

# dCDH settings. These must be plain numbers, not integers like 5L: the package
# rejects integers. Set DCDH_BOOTSTRAP=50 for bootstrapped SEs; the default SEs can
# be too small with a continuous treatment, but the estimates don't change.
dcdh_window <- 5
dcdh_boot   <- as.numeric(Sys.getenv("DCDH_BOOTSTRAP", "0"))
dcdh_labels <- c(log_live_and_work = "Log(work in own muni)", log_in_commuters = "Log(in-commuters)",
                 log_total_jobs = "Log(total jobs)", share_live_in = "Share live in muni")
fit_dcdh <- function(data, outcome) {
  # pass only the columns it needs: an extra text column confuses the package
  model_data <- data %>%
    select(all_of(c("muni_int", "year", "resident_eit", outcome, ctrls))) %>%
    filter(complete.cases(.)) %>%
    rename(Y = all_of(outcome))
  if (!nrow(model_data)) return(NULL)
  message(sprintf("[%s] N=%s munis=%s -- fitting dCDH", outcome,
                  format(nrow(model_data), big.mark = ","), n_distinct(model_data$muni_int)))
  args <- list(df = as.data.frame(model_data), outcome = "Y", group = "muni_int", time = "year",
               treatment = "resident_eit", effects = dcdh_window, placebo = dcdh_window,
               continuous = 1, controls = ctrls, cluster = "muni_int", graph_off = TRUE)
  if (dcdh_boot > 0) args$bootstrap <- dcdh_boot
  tryCatch(do.call(did_multiplegt_dyn, args),
           error = function(e) { message("ERROR: ", conditionMessage(e)); NULL })
}
joint_placebo_p <- function(model) {
  p <- tryCatch(model$results$p_jointplacebo, error = function(e) NA_real_)
  if (is.null(p)) NA_real_ else p
}
dcdh_se_type <- if (dcdh_boot > 0) sprintf("bootstrap(%d)", dcdh_boot) else "analytical"

# dCDH pooled ----
# A second estimator (de Chaisemartin and D'Haultfoeuille 2024) that can't put
# negative weight on any town. If its path matches the headline's, the headline's
# straight-line average is not hiding anything. No school-district term here.
if (RUN_DCDH_POOLED) local({
  suppressMessages(library(DIDmultiplegtDYN))
  dcdh_panel <- panel %>%
    filter(as.character(cell_muni) %in% c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"),
           !is.na(resident_eit)) %>%
    mutate(muni_int = as.integer(factor(muni)))

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  estimates <- list()
  for (outcome in outcomes) {
    model <- fit_dcdh(dcdh_panel, outcome)
    if (is.null(model)) next
    estimates[[outcome]] <- extract_dcdh(model) %>%
      mutate(outcome = outcome, joint_placebo_p = joint_placebo_p(model), se_type = dcdh_se_type)
    message(sprintf("  done; effect@+%d = %+.3f", dcdh_window,
                    with(estimates[[outcome]], est[rel == max(rel)])))
  }
  estimates <- bind_rows(estimates)
  fwrite(estimates, file.path(RESULTS_DIR, "dcdh_robustness.csv"))

  plot <- ggplot(mutate(estimates, outcome_label = dcdh_labels[outcome]), aes(rel, est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166ac") +
    geom_line(color = "#2166ac") + geom_point(color = "#2166ac", size = 1) +
    facet_wrap(~ outcome_label, scales = "free_y") +
    labs(title = "dCDH (de Chaisemartin-D'Haultfoeuille) event study -- robustness to linear feols",
         subtitle = "continuous treatment (resident rate level), non-negative weights; same headline sample",
         x = "periods since rate change", y = "estimated effect (per unit of rate)") +
    theme_bw(base_size = 10)
  ggsave(file.path(OUT_DIR, "dcdh_vs_feols_es.png"), plot, width = 9, height = 6, dpi = 300)
  message("Saved data/results/dcdh_robustness.csv + figures/dcdh_vs_feols_es.png")
  estimates %>% transmute(outcome, rel, est = round(est, 3), se = round(se, 3)) %>%
    arrange(outcome, rel) %>% print(n = Inf)
})

# dCDH by cell ----
# The same estimator for all increases, Inc-bind and Inc-becomes separately.
if (RUN_DCDH_BYCELL) local({
  suppressMessages(library(DIDmultiplegtDYN))
  dcdh_panel <- panel %>%
    filter(as.character(cell_muni) %in% c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"),
           !is.na(resident_eit))
  groups <- list(
    pooled     = c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"),
    incbind    = c("NC", "Inc-bind"),
    incbecomes = c("NC", "Inc-becomes"))
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")

  estimates <- list()
  for (group_name in names(groups)) {
    group_panel <- dcdh_panel %>%
      filter(as.character(cell_muni) %in% groups[[group_name]]) %>%
      mutate(muni_int = as.integer(factor(muni)))
    for (outcome in outcomes) {
      model <- fit_dcdh(group_panel, outcome)
      if (is.null(model)) next
      # the package's effect per percentage point, comparable to the headline coefficient
      # ($ATE comes back as a one-row matrix, so convert it first)
      ate <- tryCatch({
        ate_matrix <- model$results$ATE
        if (is.null(ate_matrix)) NA_real_ else as.numeric(as.data.frame(ate_matrix)$Estimate[1])
      }, error = function(e) NA_real_)
      estimates[[paste(group_name, outcome)]] <- extract_dcdh(model) %>%
        mutate(group = group_name, outcome = outcome, joint_placebo_p = joint_placebo_p(model),
               ate = ate, se_type = dcdh_se_type)
      message(sprintf("  done; effect@+%d = %+.3f", dcdh_window,
                      with(estimates[[paste(group_name, outcome)]], est[rel == max(rel)])))
    }
  }
  estimates <- bind_rows(estimates)
  fwrite(estimates, file.path(RESULTS_DIR, "dcdh_bycell.csv"))

  group_labels <- c(pooled = "Pooled Increase", incbind = "Inc-bind", incbecomes = "Inc-becomes")
  group_colors <- c("Pooled Increase" = "#4d4d4d", "Inc-bind" = "#b2182b", "Inc-becomes" = "#2166ac")
  plot_data <- mutate(estimates, outcome_label = dcdh_labels[outcome],
                      group_label = factor(group_labels[group], levels = group_labels))
  plot <- ggplot(plot_data, aes(rel, est, color = group_label, fill = group_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~ outcome_label, scales = "free_y") +
    scale_color_manual(values = group_colors, name = NULL) +
    scale_fill_manual(values = group_colors, name = NULL) +
    labs(title = "dCDH event study by group -- non-negative-weight check on the decomposition",
         subtitle = "continuous treatment (resident rate level); pooled vs Inc-bind vs Inc-becomes; same headline sample",
         x = "periods since rate change", y = "estimated effect (per unit of rate)") +
    theme_bw(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "dcdh_bycell_es.png"), plot, width = 9, height = 6, dpi = 300)
  message("Saved data/results/dcdh_bycell.csv + figures/dcdh_bycell_es.png")
  estimates %>% transmute(group, outcome, rel, est = round(est, 3), se = round(se, 3)) %>%
    arrange(group, outcome, rel) %>% print(n = Inf)
})

# Act 32 split ----
# The headline run separately for towns that raised rates before 2012 and from
# 2012 on, when Act 32 made employers withhold the tax. Control towns are in both.
if (RUN_ACT32) local({
  act32_year <- 2012L
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs",
                "share_live_in", "share_work_outside")

  # how many towns per cell before and after 2012 (a cell with none can't be estimated)
  cohort_counts <- stk_muni %>%
    filter(!is.na(c_muni), treat_cell != "NC") %>%
    distinct(muni, cell = droplevels(treat_cell), c_muni) %>%
    mutate(period = if_else(c_muni < act32_year, "pre", "post"))
  print(table(cell = cohort_counts$cell, period = cohort_counts$period))

  results <- list()
  full_tables <- list()
  diagnostics <- list()
  for (period in c("pre", "post")) {
    period_stack <- stk_muni %>%
      filter(is.na(c_muni) | (if (period == "pre") c_muni < act32_year else c_muni >= act32_year)) %>%
      mutate(treat_cell = factor(treat_cell, levels = cell_levels))
    for (outcome in outcomes) {
      model_data <- period_stack %>%
        select(any_of(c(outcome, "dose_stack", "treat_cell", ctrls, "muni",
                        "stack_muni", "stack_year"))) %>%
        filter(complete.cases(.))
      if (nrow(model_data) == 0) next
      model <- tryCatch(
        # note: no school-district term here, unlike the headline and the pooled split
        feols(as.formula(paste0(outcome, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                                ctrls_str, " | stack_muni + stack_year")),
              data = model_data, cluster = ~muni),
        error = function(e) {
          message(sprintf("  %s %s err: %s", period, outcome, conditionMessage(e))); NULL })
      if (is.null(model)) next
      coefs <- as_tibble(model$coeftable, rownames = "term") %>%
        rename(est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)
      dep_var <- model$residuals + model$fitted.values
      results[[length(results) + 1L]] <- coefs %>%
        filter(grepl("treat_cell::", term)) %>%
        transmute(period = period, outcome = outcome,
                  cell = sub("^.*::([^:]+):.*$", "\\1", term), est, se, p, n = nobs(model))
      full_tables[[length(full_tables) + 1L]] <- coefs %>%
        mutate(period = period, outcome = outcome, n = nobs(model),
               r2 = round(fitstat(model, "r2", simplify = TRUE), 4),
               wr2 = round(fitstat(model, "wr2", simplify = TRUE), 4),
               dep_mean = round(mean(dep_var, na.rm = TRUE), 4))
      diagnostics[[length(diagnostics) + 1L]] <- tibble(
        period = period, outcome = outcome, n = nobs(model),
        r2       = round(fitstat(model, "r2",  simplify = TRUE), 4),
        wr2      = round(fitstat(model, "wr2", simplify = TRUE), 4),
        dep_mean = round(mean(dep_var, na.rm = TRUE), 4),
        dep_sd   = round(sd(dep_var,   na.rm = TRUE), 4))
    }
  }
  fwrite(bind_rows(results), file.path(RESULTS_DIR, "act32_split_cousub.csv"))
  fwrite(bind_rows(full_tables), file.path(RESULTS_DIR, "act32_full_cousub.csv"))
  fwrite(bind_rows(diagnostics), file.path(RESULTS_DIR, "act32_split_cousub_diag.csv"))
  message(sprintf("[cousub] Act 32 split saved (%d coef rows, %d diag rows).",
                  nrow(bind_rows(results)), nrow(bind_rows(diagnostics))))
})

# Act 47 exclusion ----
# The headline without the Inc-bind towns that were ever financially distressed.
if (RUN_ACT47) local({
  rate_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  muni_names <- unique(rate_panel[!is.na(cousub_fips),
                                  .(muni = cousub_fips, name = toupper(municipality))])
  named_panel <- merge(panel, muni_names, by = "muni", all.x = TRUE)

  # each Inc-bind town's earliest Act 47 year. "Ever designated" is the cautious rule:
  # none of them was actually in recovery when it raised its rate
  inc_bind_munis <- named_panel %>%
    filter(cell_muni == "Inc-bind") %>%
    distinct(muni, name, c_muni) %>%
    mutate(name_norm = normalize_name(name),
           act47_year = vapply(name_norm, function(name) {
             hits <- act47$designated[vapply(act47$name_norm, function(a47)
                       grepl(a47, name, fixed = TRUE), logical(1))]
             if (length(hits)) min(hits) else NA_integer_
           }, integer(1)),
           is_act47 = !is.na(act47_year),
           in_recovery_at_event = !is.na(act47_year) & c_muni >= act47_year)
  message(sprintf("[act47] Inc-bind munis: %d total", nrow(inc_bind_munis)))
  message(sprintf("[act47] ever designated: %d | already in recovery at their rate change: %d",
                  sum(inc_bind_munis$is_act47), sum(inc_bind_munis$in_recovery_at_event)))
  inc_bind_munis %>% filter(is_act47) %>%
    transmute(name, cohort = c_muni, designated = act47_year, in_recovery_at_event) %>% print()

  # list names that match no town are outside the sample (Pittsburgh, Altoona, ...) or misspelled
  panel_names <- normalize_name(unique(named_panel$name))
  unmatched <- act47$name[!vapply(act47$name_norm, function(a47)
    any(grepl(a47, panel_names, fixed = TRUE)), logical(1))]
  if (length(unmatched))
    message(sprintf("[act47] WARNING: %d list entries match no municipality in the panel: %s",
                    length(unmatched), paste(unmatched, collapse = ", ")))

  act47_munis <- inc_bind_munis$muni[inc_bind_munis$is_act47]
  # blank out their cell so they drop out of the treated group
  named_panel[, cell_muni_a47drop := cell_muni]
  named_panel[muni %in% act47_munis & cell_muni == "Inc-bind", cell_muni_a47drop := NA_character_]

  # rebuild the stacks the same way as build_headline_env()
  K_stack <- 5L
  build_act47_stack <- function(data, cell_col) {
    treated_munis <- unique(data[get(cell_col) != "NC" & !is.na(get(cell_col)),
                                 .(muni, cohort = c_muni, cell = get(cell_col))])
    nc_munis <- unique(data[get(cell_col) == "NC", muni])
    stacked <- rbindlist(lapply(sort(unique(treated_munis$cohort)), function(cohort_year) {
      window_years <- (cohort_year - K_stack):(cohort_year + K_stack)
      cohort_munis <- unique(treated_munis[cohort == cohort_year, .(muni)])
      if (nrow(cohort_munis) == 0) return(NULL)
      treated_rows <- data[muni %in% cohort_munis$muni & year %in% window_years]
      treated_rows[, treat_cell := get(cell_col)]
      control_rows <- data[muni %in% nc_munis & year %in% window_years]
      control_rows[, treat_cell := "NC"]
      rbind(treated_rows, control_rows, fill = TRUE)[, c(list(
        muni = muni, year = year, treat_cell = treat_cell,
        stack = cohort_year, rel = year - cohort_year,
        dose_muni = dose_muni, dose_sd = dose_sd, resident_eit = resident_eit,
        dose_nr = dose_nr, cell_sd = cell_sd, cell_nr = cell_nr,
        log_live_and_work = log_live_and_work, log_in_commuters = log_in_commuters,
        log_total_jobs = log_total_jobs, share_live_in = share_live_in),
        mget(ctrls))]
    }), fill = TRUE)
    stacked[, stack_muni := paste0(stack, "_", muni)]
    stacked[, stack_year := paste0(stack, "_", year)]
    stacked[, treat_cell := factor(treat_cell, levels = cell_levels)]
    stacked[is.na(dose_sd), dose_sd := 0]
    stacked[is.na(dose_nr), dose_nr := 0]
    stacked[, dose_stack := resident_eit]
    stacked
  }
  message("Building stacks (full headline + Act 47 excluded)...")
  full_stack <- build_act47_stack(named_panel, "cell_muni")
  drop_stack <- build_act47_stack(named_panel, "cell_muni_a47drop")

  # returns the Inc-bind row for the summary and every coefficient for the appendix table
  fit_headline <- function(stack, outcome) {
    model_data <- stack %>%
      select(all_of(c("muni", "year", "stack_muni", "stack_year", "treat_cell",
                      "dose_stack", "cell_sd", "dose_sd", outcome, ctrls))) %>%
      filter(complete.cases(.))
    model <- feols(as.formula(paste0(outcome, " ~ i(treat_cell, dose_stack, ref = 'NC') ",
                                     "+ i(cell_sd, dose_sd, ref = 'NC') + ",
                                     ctrls_str, " | stack_muni + stack_year")),
                   data = model_data, cluster = ~muni)
    coefs <- as_tibble(model$coeftable, rownames = "term") %>%
      rename(est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)
    inc_bind <- filter(coefs, grepl("^treat_cell::Inc-bind:", term))
    list(summary = tibble(outcome = outcome, est = round(inc_bind$est, 4),
                          se = round(inc_bind$se, 4), p = round(inc_bind$p, 4), n = nobs(model)),
         full = coefs %>%
           mutate(outcome = outcome, n = nobs(model),
                  r2 = round(fitstat(model, "r2", simplify = TRUE), 4),
                  wr2 = round(fitstat(model, "wr2", simplify = TRUE), 4),
                  dep_mean = round(mean(model$residuals + model$fitted.values, na.rm = TRUE), 4)))
  }

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  message(sprintf("Cousub headline (all %d Inc-bind munis)", nrow(inc_bind_munis)))
  full_fits <- lapply(outcomes, function(outcome) fit_headline(full_stack, outcome))
  message("Cousub headline (excluding Act 47 Inc-bind munis)")
  drop_fits <- lapply(outcomes, function(outcome) fit_headline(drop_stack, outcome))
  summary_table <- bind_rows(
    bind_rows(lapply(full_fits, `[[`, "summary")) %>%
      mutate(spec = sprintf("All %d Inc-bind munis", nrow(inc_bind_munis))),
    bind_rows(lapply(drop_fits, `[[`, "summary")) %>%
      mutate(spec = sprintf("Drop %d Act 47 Inc-bind", length(act47_munis))))
  print(summary_table)
  fwrite(summary_table, file.path(RESULTS_DIR, "act47_robustness_cousub.csv"))
  fwrite(bind_rows(lapply(drop_fits, `[[`, "full")), file.path(RESULTS_DIR, "act47_full_table.csv"))
  message("Saved data/results/act47_robustness_cousub.csv + act47_full_table.csv")
})

# Pre-trend battery ----
# Several pre-trend tests per cell (see pretrend_bucket() in helpers.R). The
# pooled group is expected to fail, since it mixes cells that behave differently.
if (RUN_PRETREND_BATTERY) local({
  K <- 5L; set.seed(42)
  battery_panel <- add_realized_dose(copy(panel), K = K)

  treated_munis <- unique(battery_panel[cell_muni != "NC", .(muni, cohort = c_muni)])
  nc_munis <- unique(battery_panel[cell_muni == "NC", muni])
  battery_stack <- rbindlist(lapply(sort(unique(treated_munis$cohort)), function(cohort_year) {
    window_years <- (cohort_year - K):(cohort_year + K)
    cohort_munis <- treated_munis[cohort == cohort_year]
    if (!nrow(cohort_munis)) return(NULL)
    treated_rows <- battery_panel[muni %in% cohort_munis$muni & year %in% window_years]
    treated_rows[, treat_cell_muni := as.character(cell_muni)]
    control_rows <- battery_panel[muni %in% nc_munis & year %in% window_years]
    control_rows[, treat_cell_muni := "NC"]
    stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
    stack_rows[, rel := as.integer(year - cohort_year)]; stack_rows[, stack := cohort_year]
    stack_rows
  }), fill = TRUE)
  battery_stack[, stack_muni := paste0(stack, "_", muni)]
  battery_stack[, stack_year := paste0(stack, "_", year)]
  battery_stack[, cell_sd := factor(cell_sd, levels = cell_levels)]
  battery_stack[, cell_nr := factor(cell_nr, levels = cell_levels)]
  battery_stack[is.na(dose_sd), dose_sd := 0]
  battery_stack[is.na(dose_nr), dose_nr := 0]
  battery_stack[, dose_rel := realized_dose_muni * rel]   # for the straight-line pre-trend test

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  battery_groups <- list(
    "Inc-bind"    = "Inc-bind",
    "Inc-becomes" = "Inc-becomes",
    "Inc-nobind"  = "Inc-nobind",
    "Pooled-Inc"  = c("Inc-bind", "Inc-becomes", "Inc-nobind"))
  rows <- list()
  for (outcome in outcomes) for (group_name in names(battery_groups)) {
    group_stack <- filter(battery_stack, treat_cell_muni %in% c("NC", battery_groups[[group_name]]))
    model_data <- group_stack %>%
      select(all_of(c("muni", "rel", "stack_muni", "stack_year", "realized_dose_muni",
                      "cell_sd", "dose_sd", outcome, ctrls))) %>%
      filter(complete.cases(.))
    if (!nrow(model_data)) next
    es_model <- tryCatch(feols(as.formula(paste0(outcome,
                  " ~ i(rel, realized_dose_muni, ref=-1) + i(cell_sd, dose_sd, ref='NC') + ",
                  ctrls_str, " | stack_muni + stack_year")), model_data, cluster = ~muni),
                  error = function(e) NULL)
    if (is.null(es_model)) next
    pre_data <- group_stack %>%
      filter(rel < 0) %>%
      select(all_of(c("muni", "dose_rel", "stack_muni", "stack_year",
                      "cell_sd", "dose_sd", outcome, ctrls))) %>%
      filter(complete.cases(.))
    slope_model <- tryCatch(feols(as.formula(paste0(outcome,
                     " ~ dose_rel + i(cell_sd,dose_sd,ref='NC') + ",
                     ctrls_str, " | stack_muni + stack_year")), pre_data, cluster = ~muni),
                     error = function(e) NULL)
    rows[[length(rows) + 1L]] <- cbind(
      data.table(outcome = outcome, cell = group_name,
                 n_treated = uniqueN(model_data$muni[model_data$realized_dose_muni != 0])),
      pretrend_bucket(es_model, lin_model = slope_model, lin_term = "dose_rel"))
  }
  battery <- rbindlist(rows, fill = TRUE)
  fwrite(battery, file.path(RESULTS_DIR, "pretrend_battery.csv"))
  print(battery)
})

# Dose-permutation randomization inference ----
# Strong parallel trends is fundamentally about DOSE SELECTION: did the
# municipalities that chose big increases differ in trend from those that
# chose small ones? This test shuffles the dose MAGNITUDES across treated
# municipalities (keeping each muni's timing and binding cell fixed),
# re-estimates the headline decomposition, and asks where the true
# coefficient falls in the distribution of shuffled ones.
# Lay explanation: imagine every treated town keeps the YEAR it raised taxes
# but swaps the SIZE of its hike with another town at random. If the real
# estimate only stands out because big hikes went to towns already losing
# residents, the shuffled estimates would often look just as extreme. A true
# estimate in the far tail of the shuffled distribution says the size of the
# hike, not the kind of town, drives the result.
# Output: data/results/perm_inference_draws.csv, perm_inference_summary.csv,
#         figures/perm_inference_hist.png
if (RUN_DOSE_PERMUTATION) local({
  set.seed(20260707)
  n_perm <- as.integer(Sys.getenv("N_PERM", "500"))
  o <- "log_live_and_work"
  cols <- c("muni", "year", "treat_cell", "dose_stack", "cell_sd", "dose_sd",
            "stack_muni", "stack_year", o, ctrls)
  d0 <- stk_muni[, ..cols]
  d0[is.na(dose_sd), dose_sd := 0]
  d0 <- d0[complete.cases(d0)]
  f <- as.formula(paste0(o, " ~ i(treat_cell, dose_perm, ref = 'NC')",
       " + i(cell_sd, dose_sd, ref = 'NC') + ", ctrls_str,
       " | stack_muni + stack_year"))
  cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  get_betas <- function(m) {
    b <- coef(m)
    sapply(cells, function(cl) {
      tm <- sprintf("treat_cell::%s:dose_perm", cl)
      if (tm %in% names(b)) unname(b[tm]) else NA_real_
    })
  }
  d0[, dose_perm := dose_stack]
  m0 <- feols(f, d0, cluster = ~muni)
  b_true <- get_betas(m0)
  # realized magnitude per treated muni = its max cumulative dose in-window
  mags <- d0[as.character(treat_cell) != "NC",
             .(mag = max(dose_stack, na.rm = TRUE)), by = muni]
  draws <- vector("list", n_perm)
  for (r in seq_len(n_perm)) {
    mags[, mag_perm := sample(mag)]
    dd <- merge(d0, mags[, .(muni, mag, mag_perm)], by = "muni", all.x = TRUE)
    dd[, sc := fifelse(!is.na(mag) & mag > 0, mag_perm / mag, 1)]
    dd[, dose_perm := dose_stack * sc]
    mm <- tryCatch(feols(f, dd, cluster = ~muni), error = function(e) NULL)
    if (is.null(mm)) next
    draws[[r]] <- as.data.table(as.list(get_betas(mm)))[, rep := r]
    if (r %% 50 == 0) message(sprintf("  permutation %d / %d", r, n_perm))
  }
  pd <- rbindlist(draws, fill = TRUE)
  setnames(pd, cells, c("incbind", "incbecomes", "incnobind"), skip_absent = TRUE)
  fwrite(pd, file.path(RESULTS_DIR, "perm_inference_draws.csv"))
  summ <- data.table(
    cell = cells,
    beta_true = round(b_true, 4),
    perm_mean = round(sapply(c("incbind", "incbecomes", "incnobind"),
                             function(x) mean(pd[[x]], na.rm = TRUE)), 4),
    perm_p = round(sapply(seq_along(cells), function(j) {
      x <- pd[[c("incbind", "incbecomes", "incnobind")[j]]]
      mean(abs(x) >= abs(b_true[j]), na.rm = TRUE)
    }), 4),
    n_perm = nrow(pd))
  fwrite(summ, file.path(RESULTS_DIR, "perm_inference_summary.csv"))
  print(summ)
  ph <- melt(pd, id.vars = "rep",
             measure.vars = c("incbind", "incbecomes", "incnobind"),
             variable.name = "cell", value.name = "beta")
  vl <- data.table(cell = factor(c("incbind", "incbecomes", "incnobind")),
                   beta_true = b_true)
  gp <- ggplot(ph, aes(beta)) +
    geom_histogram(bins = 40, fill = "grey65", color = "white") +
    geom_vline(data = vl, aes(xintercept = beta_true),
               color = "#C5283D", linewidth = 0.7) +
    facet_wrap(~ cell, ncol = 3, scales = "free",
               labeller = as_labeller(c(incbind = "Inc-bind",
                                        incbecomes = "Inc-becomes",
                                        incnobind = "Inc-nobind"))) +
    labs(title = "Dose-permutation randomization inference (log live-and-work)",
         subtitle = sprintf("%d permutations of dose magnitudes across treated munis; red line = true estimate", nrow(pd)),
         x = "permuted coefficient", y = "count") +
    theme_bw(base_size = 10)
  ggsave(file.path(OUT_DIR, "perm_inference_hist.png"), gp,
         width = 10, height = 3.8, dpi = 300)
})

# Fake-timing placebo cohorts ----
# Pretend each town changed its rate 3, 4 or 5 years early and use only data from
# before the real change. Nothing happened at the fake date, so the effects should be zero.
if (RUN_FAKE_TIMING) local({
  shifts <- c(3L, 4L, 5L); K <- 5L
  treated_all <- unique(panel[!is.na(c_muni) & as.character(cell_muni) != "NC",
                              .(muni, c_true = c_muni, cell = as.character(cell_muni))])
  dose_tags <- unique(add_realized_dose(copy(panel), K = K)[!is.na(c_muni),
                                                           .(muni, dose_tag = realized_dose_muni)])
  nc_munis <- unique(panel[cell_muni == "NC", muni])
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  rows <- list()
  for (shift in shifts) {
    treated <- treated_all[c_true - shift - 2L >= 2002L]   # need two years of data before the fake date
    message(sprintf("fake-timing shift -%d: %d of %d treated munis eligible",
                    shift, nrow(treated), nrow(treated_all)))
    treated <- merge(treated, dose_tags, by = "muni", all.x = TRUE)
    fake_stack <- rbindlist(lapply(sort(unique(treated$c_true - shift)), function(cohort_year) {
      window_years <- (cohort_year - K):(cohort_year + K)
      cohort_munis <- treated[c_true - shift == cohort_year]
      if (!nrow(cohort_munis)) return(NULL)
      treated_rows <- merge(panel[muni %in% cohort_munis$muni & year %in% window_years],
                            cohort_munis[, .(muni, c_true, cell_fake = cell, dose_tag)], by = "muni")
      treated_rows <- treated_rows[year < c_true]          # before the real event only
      control_rows <- panel[muni %in% nc_munis & year %in% window_years]
      control_rows[, `:=`(cell_fake = "NC", dose_tag = 0, c_true = NA_integer_)]
      stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
      stack_rows[, stack := cohort_year]; stack_rows[, rel := as.integer(year - cohort_year)]
      stack_rows
    }), fill = TRUE)
    fake_stack[, stack_muni := paste0(stack, "_", muni)]
    fake_stack[, stack_year := paste0(stack, "_", year)]
    fake_stack[, cell_fake := factor(cell_fake, levels = c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"))]
    fake_stack[, dose_fake := fifelse(rel >= 0, dose_tag, 0)]   # the town's real eventual increase, switched on at the fake date
    fake_stack[is.na(dose_sd), dose_sd := 0]
    for (outcome in outcomes) {
      model_data <- fake_stack %>%
        select(all_of(c("muni", "rel", "stack_muni", "stack_year", "cell_fake",
                        "dose_fake", "cell_sd", "dose_sd", outcome, ctrls))) %>%
        filter(complete.cases(.))
      if (!nrow(model_data)) next
      model <- tryCatch(feols(as.formula(paste0(outcome,
                 " ~ i(cell_fake, dose_fake, ref = 'NC') + i(cell_sd, dose_sd, ref = 'NC') + ",
                 ctrls_str, " | stack_muni + stack_year")), model_data, cluster = ~muni),
                 error = function(e) NULL)
      if (is.null(model)) next
      rows[[length(rows) + 1L]] <- as_tibble(model$coeftable, rownames = "term") %>%
        filter(grepl("^cell_fake::", term)) %>%
        transmute(shift = shift, outcome = outcome,
                  cell = sub("^.*::([^:]+):.*$", "\\1", term),
                  est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nobs(model),
                  n_treated = n_distinct(model_data$muni[model_data$dose_fake != 0]))
    }
  }
  placebo <- bind_rows(rows)
  fwrite(placebo, file.path(RESULTS_DIR, "fake_timing_placebo.csv"))
  print(placebo, n = Inf)
})

# Pooled robustness ----
# The same checks on the pooled version (all increases as one group), for the
# appendix. A few are built differently from the versions above; each is noted
# where it happens.
if (RUN_POOLED_ROBUSTNESS) local({

  act32_year <- 2012L; shifts <- c(3L, 4L, 5L); K <- 5L
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")
  to_direction <- function(cell) {
    cell <- as.character(cell)
    factor(case_when(cell %in% inc_cells ~ "Inc", cell %in% dec_cells ~ "Dec", cell == "NC" ~ "NC"),
           levels = c("NC", "Inc", "Dec"))
  }
  pooled_stack <- stk_muni %>%
    mutate(direction_muni = to_direction(treat_cell),
           direction_sd   = to_direction(cell_sd),
           dose_sd        = coalesce(dose_sd, 0)) %>%
    as.data.table()

  # every coefficient of one pooled model, with fit statistics
  fit_pooled <- function(data, outcome, dose_col = "dose_stack") {
    model_data <- data %>%
      select(any_of(c("muni", "direction_muni", dose_col, "direction_sd", "dose_sd",
                      "stack_muni", "stack_year", outcome, ctrls))) %>%
      filter(complete.cases(.))
    if (!nrow(model_data) || n_distinct(model_data$direction_muni) < 2) return(NULL)
    model <- tryCatch(suppressMessages(feols(as.formula(paste0(outcome,
               " ~ i(direction_muni, ", dose_col, ", ref='NC')",
               " + i(direction_sd, dose_sd, ref='NC') + ",
               paste(ctrls, collapse = " + "), " | stack_muni + stack_year")),
               data = model_data, cluster = ~muni, notes = FALSE)),
             error = function(e) NULL)
    if (is.null(model)) return(NULL)
    as_tibble(model$coeftable, rownames = "term") %>%
      rename(est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`) %>%
      mutate(outcome = outcome, n = nobs(model),
             r2  = round(fitstat(model, "r2",  simplify = TRUE), 4),
             wr2 = round(fitstat(model, "wr2", simplify = TRUE), 4),
             dep_mean  = round(mean(model$residuals + model$fitted.values, na.rm = TRUE), 4),
             n_treated = n_distinct(model_data$muni[model_data$direction_muni == "Inc"]))
  }
  fit_all_outcomes <- function(data, dose_col = "dose_stack") {
    bind_rows(lapply(outcomes, function(outcome) fit_pooled(data, outcome, dose_col)))
  }

  # (1) Act 32 split (note: this one does include the school-district term)
  bind_rows(
    fit_all_outcomes(filter(pooled_stack, is.na(c_muni) | c_muni <  act32_year)) %>% mutate(period = "pre"),
    fit_all_outcomes(filter(pooled_stack, is.na(c_muni) | c_muni >= act32_year)) %>% mutate(period = "post")) %>%
    fwrite(file.path(RESULTS_DIR, "pooled_act32.csv"))

  # (2) Act 47 exclusion: drop every pooled-Inc muni whose name matches the list
  rate_names <- unique(as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))[
    !is.na(cousub_fips), .(muni = cousub_fips, name_norm = normalize_name(municipality))])
  act47_munis <- rate_names$muni[matches_act47(rate_names$name_norm)]
  n_act47 <- n_distinct(pooled_stack$muni[pooled_stack$muni %in% act47_munis &
                                            pooled_stack$direction_muni == "Inc"])
  message(sprintf("[pooled-rob] Act 47 munis in the pooled Inc group: %d", n_act47))
  bind_rows(
    fit_all_outcomes(pooled_stack) %>% mutate(spec = "All Inc munis"),
    fit_all_outcomes(filter(pooled_stack, !(direction_muni == "Inc" & muni %in% act47_munis))) %>%
      mutate(spec = sprintf("Drop %d Act 47 munis", n_act47))) %>%
    fwrite(file.path(RESULTS_DIR, "pooled_act47.csv"))

  # (3) Fake timing at three shifts
  direction_panel <- panel %>%
    mutate(direction_muni = to_direction(cell_muni), direction_sd = to_direction(cell_sd)) %>%
    as.data.table()
  nc_munis <- unique(direction_panel[direction_muni == "NC", muni])
  treated_all <- unique(pooled_stack[direction_muni == "Inc" & !is.na(c_muni), .(muni, c_true = c_muni)])
  fake_fits <- list()
  for (shift in shifts) {
    treated <- treated_all[c_true - shift - 2L >= 2002L]
    message(sprintf("[pooled-rob] fake timing -%d: %d of %d treated munis eligible",
                    shift, nrow(treated), nrow(treated_all)))
    fake_stack <- rbindlist(lapply(sort(unique(treated$c_true - shift)), function(cohort_year) {
      window_years <- (cohort_year - K):(cohort_year + K)
      cohort_munis <- treated[c_true - shift == cohort_year]
      treated_rows <- direction_panel[muni %in% cohort_munis$muni & year %in% window_years &
                                        year < (cohort_year + shift)]
      control_rows <- direction_panel[muni %in% nc_munis & year %in% window_years]
      if (!nrow(treated_rows)) return(NULL)
      treated_rows[, direction_fake := "Inc"]; control_rows[, direction_fake := "NC"]
      rbind(treated_rows, control_rows, fill = TRUE)[, stack := cohort_year][]
    }), fill = TRUE)
    if (!nrow(fake_stack)) next
    fake_stack[, `:=`(rel = year - stack, stack_muni = paste0(stack, "_", muni),
                      stack_year = paste0(stack, "_", year))]
    fake_stack[, direction_muni := factor(direction_fake, levels = c("NC", "Inc", "Dec"))]
    # note: the fake dose here is the rate level, not the realized increase used above
    fake_stack[, dose_fake := fifelse(rel >= 0 & direction_fake == "Inc", resident_eit, 0)]
    if (!"dose_sd" %in% names(fake_stack)) fake_stack[, dose_sd := 0]
    fake_stack[is.na(dose_sd), dose_sd := 0]
    fits <- fit_all_outcomes(fake_stack, "dose_fake")
    if (nrow(fits)) fake_fits[[as.character(shift)]] <- mutate(fits, shift = shift)
  }
  if (length(fake_fits)) fwrite(bind_rows(fake_fits), file.path(RESULTS_DIR, "pooled_fake_timing.csv"))

  # (4) Pre-trend battery from the pooled event study in 2_event_studies.R
  coefs_path   <- file.path(RESULTS_DIR, "pooled_es_cousub_coefs.csv")
  summary_path <- file.path(RESULTS_DIR, "pooled_es_cousub_att.csv")
  if (file.exists(coefs_path) && file.exists(summary_path)) {
    es_coefs   <- fread(coefs_path) %>% filter(direction == "Inc", outcome %in% outcomes)
    es_summary <- fread(summary_path) %>% filter(outcome %in% outcomes)
    battery <- bind_rows(lapply(outcomes, function(outcome_name) {
      leads <- filter(es_coefs, outcome == outcome_name, rel <= -2)
      summary_row <- filter(es_summary, outcome == outcome_name)
      if (!nrow(leads) || !nrow(summary_row)) return(NULL)
      # note: a slope fitted through the published lead estimates, not a regression
      # on the data like the battery above
      slope <- tryCatch(summary(lm(est ~ rel, data = leads))$coefficients["rel", c(1, 4)],
                        error = function(e) c(NA_real_, NA_real_))
      n_sig <- sum(leads$p < 0.05, na.rm = TRUE)
      tibble(outcome = outcome_name, n_leads = nrow(leads),
             wald_joint_p = summary_row$wald_pre_p[1],
             n_leads_sig5 = n_sig,
             min_lead_p   = min(leads$p, na.rm = TRUE),
             lin_slope    = unname(slope[1]), lin_slope_p = unname(slope[2]),
             # note: unlike the battery above, "clean" here requires the Wald test to pass
             robust_clean = summary_row$wald_pre_p[1] >= 0.10 & n_sig == 0)
    }))
    fwrite(battery, file.path(RESULTS_DIR, "pooled_pretrend_battery.csv"))
    message("[pooled-rob] pre-trend battery:"); print(battery)
  }
  message("Saved pooled_act32 / pooled_act47 / pooled_fake_timing / pooled_pretrend_battery")
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()


# Cohort-by-cohort ATTs (RUN_EVENT_BY_EVENT) ----
# Cengiz, Dube, Lindner & Zipperer (2019, Online Appendix C) estimate a separate
# effect for each of their events and show the distribution, so a reader can see
# whether the pooled average hides a mix of large positives and negatives. The
# analog here is one ATT per STACK: every municipality whose first rate change
# fell in year c is one sub-experiment, estimated on that stack alone.
#
# For each cohort c:
#   sample = stk_muni[stack == c]  (that cohort's treated munis + all NC),
#            already windowed to [c-5, c+5] and dose re-baselined by build_stack
#   Y_it   = alpha_i + gamma_t + sum_k beta_kc * dose_it * 1{cell_i = k}
#            + SD co-treatment + X'phi + e
# Plain muni + year FE, because a single stack has no stack dimension left.
#
# INFERENCE CAVEAT, repeated in the figure note: several cohorts contain only one
# or two treated municipalities, so the cluster-robust SE on those is unreliable
# (Conley & Taber 2011). Cengiz et al. use Ferman & Pinto for this reason. Read
# the DISTRIBUTION of point estimates; the SEs order them, they do not test them.
if (RUN_EVENT_BY_EVENT) local({
  OUT <- "log_live_and_work"
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  stk <- copy(stk_muni)
  stk[is.na(dose_sd), dose_sd := 0]
  ctrls_str <- paste(ctrls, collapse = " + ")

  one <- function(cy) {
    d <- stk[stack == cy]
    cells_here <- intersect(inc_cells, unique(as.character(d$treat_cell)))
    if (!length(cells_here)) return(NULL)
    cols <- c("muni","year","treat_cell","cell_sd","dose_stack","dose_sd", OUT, ctrls)
    d <- d[, ..cols]; d <- d[complete.cases(d)]
    if (!nrow(d)) return(NULL)
    f <- as.formula(paste0(OUT, " ~ i(treat_cell, dose_stack, ref='NC')",
                           " + i(cell_sd, dose_sd, ref='NC') + ", ctrls_str,
                           " | muni + year"))
    m <- tryCatch(feols(f, d, cluster = ~muni, notes = FALSE), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    rbindlist(lapply(cells_here, function(k) {
      nm <- sprintf("treat_cell::%s:dose_stack", k)
      if (!(nm %in% names(coef(m)))) return(NULL)
      data.table(cohort = cy, cell = k,
                 n_treated = d[treat_cell == k, uniqueN(muni)],
                 est = unname(coef(m)[nm]), se = unname(se(m)[nm]),
                 p = unname(pvalue(m)[nm]), n = nobs(m))
    }))
  }
  res <- rbindlist(lapply(sort(unique(stk$stack)), one), fill = TRUE)
  if (!nrow(res)) { message("[cohort-ATT] nothing estimated"); return(invisible(NULL)) }
  res[, `:=`(lo = est - 1.96*se, hi = est + 1.96*se, sig = p < 0.05)]
  fwrite(res, file.path(RESULTS_DIR, "cohort_by_cohort_att.csv"))

  cat("\n---- cohort-by-cohort ATTs,", OUT, "----\n")
  print(res[, .(cohorts = .N, munis = sum(n_treated),
                negative = sum(est < 0), sig5 = sum(sig, na.rm = TRUE),
                median = round(median(est), 3), mean = round(mean(est), 3),
                min = round(min(est), 2), max = round(max(est), 2)), by = cell])
  print(res[order(cell, cohort), .(cohort, cell, n_treated,
            est = round(est,3), se = round(se,3), sig)], nrows = 60)
  message("Saved cohort_by_cohort_att.csv")
})


# Baseline-level trend adjustment (RUN_LEVEL_ADJUST) ----
# The control pool is nearly a point mass at a 0.5pp resident rate (mean 0.510,
# SD 0.089), while Inc-bind municipalities start at 0.777 on average. Tested on
# NEVER-CHANGERS ONLY, higher-baseline municipalities are already losing
# live-and-work workers faster: -0.0171 log points per year per pp of baseline
# (p = 0.016). Level therefore correlates with the counterfactual trend, so
# unconditional parallel trends is the wrong assumption and the comparison has
# to be made within baseline level.
#
# This is conditional parallel trends (Abadie 2005; Callaway & Sant'Anna 2021).
# It is also the assumption Callaway, Goodman-Bacon & Sant'Anna (2024) name as
# "strong parallel trends" for continuous treatment: ATT(d) across d confounds
# the causal dose response with selection INTO dose, and the baseline rate is
# the clearest observable proxy for that selection.
#
# Two adjustments, both added to the headline and changing nothing else:
#   A1  + tau0:year      one linear differential trend scaled by baseline rate
#   A2  + i(year, tau0)  a differential effect per calendar year
# Cost: one parameter (A1) rather than a free trend per unit, which is what the
# Wolfers (2006) / Meer & West (2016) objection to unit-specific trends targets.
# Internal check: Inc-becomes has essentially no level imbalance (0.446 vs 0.510)
# and should therefore NOT move if this is removing confounding rather than
# absorbing treatment.
if (RUN_LEVEL_ADJUST) local({
  outs <- c("log_live_and_work", "share_live_in", "log_in_commuters", "log_total_jobs")
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  stk <- copy(stk_muni)
  stk[is.na(dose_sd), dose_sd := 0]
  # tau0 = the municipality's own pre-event RATE LEVEL inside its stack window.
  stk[, tau0 := mean(dose_stack[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(tau0), tau0 := NA_real_]
  stk[, direction_muni := factor(fcase(as.character(treat_cell) %in% inc_cells, "Inc",
                                       as.character(treat_cell) == "NC", "NC",
                                       default = NA_character_), levels = c("NC", "Inc"))]
  stk[, direction_sd := factor(fcase(as.character(cell_sd) %in% inc_cells, "Inc",
                                     as.character(cell_sd) == "NC", "NC",
                                     default = NA_character_), levels = c("NC", "Inc"))]
  ctrls_str <- paste(ctrls, collapse = " + ")

  fit <- function(o, spec, extra) {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    sdv   <- if (spec == "decomp") "cell_sd"    else "direction_sd"
    f <- as.formula(paste0(o, " ~ i(", cellv, ", dose_stack, ref='NC') + i(",
                           sdv, ", dose_sd, ref='NC') + ", ctrls_str, extra,
                           " | stack_muni + stack_year"))
    tryCatch(feols(f, stk, cluster = ~muni, notes = FALSE), error = function(e) NULL)
  }
  grab <- function(m, spec, o, lab) {
    if (is.null(m)) return(NULL)
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    grps  <- if (spec == "decomp") inc_cells    else "Inc"
    rbindlist(lapply(grps, function(k) {
      nm <- sprintf("%s::%s:dose_stack", cellv, k)
      if (!(nm %in% names(coef(m)))) return(NULL)
      data.table(adjustment = lab, spec = spec, outcome = o, cell = k,
                 est = unname(coef(m)[nm]), se = unname(se(m)[nm]),
                 p = unname(pvalue(m)[nm]), n = nobs(m))
    }))
  }
  specs <- list(c("none", ""), c("A1 tau0 x year", " + tau0:year"),
                c("A2 tau0 x calendar year", " + i(year, tau0)"))
  res <- rbindlist(lapply(outs, function(o)
    rbindlist(lapply(c("pooled", "decomp"), function(sp)
      rbindlist(lapply(specs, function(x) grab(fit(o, sp, x[2]), sp, o, x[1])), fill = TRUE)),
      fill = TRUE)), fill = TRUE)
  fwrite(res, file.path(RESULTS_DIR, "level_adjusted_headline.csv"))
  cat("\n---- baseline-level trend adjustment ----\n")
  print(dcast(res, spec + cell + outcome ~ adjustment, value.var = "est"))

  # Event study under the A1 adjustment, per cell and outcome, with the
  # unadjusted path alongside so the correction is visible frame by frame.
  # realized_dose_muni is muni-level and time-invariant (add_realized_dose
  # merges it by muni), so it can be joined straight onto the stack.
  rd <- unique(add_realized_dose(copy(panel))[, .(muni, realized_dose_muni)], by = "muni")
  stk <- merge(stk, rd, by = "muni", all.x = TRUE)
  stk[is.na(realized_dose_muni), realized_dose_muni := 0]
  es_one <- function(dat, o, cl, extra, lab) {
    keep <- c(cl, "NC")
    dd <- dat[as.character(treat_cell) %in% keep]
    cols <- c("muni","year","rel","treat_cell","realized_dose_muni","tau0",
              "cell_sd","dose_sd","stack_muni","stack_year", o, ctrls)
    cols <- intersect(cols, names(dd)); dd <- dd[, ..cols]; dd <- dd[complete.cases(dd)]
    if (!nrow(dd) || !sum(dd$treat_cell == cl)) return(NULL)
    f <- as.formula(paste0(o, " ~ i(rel, realized_dose_muni, ref = -1) + i(cell_sd, dose_sd, ref='NC') + ",
                           ctrls_str, extra, " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, dd, cluster = ~muni, notes = FALSE), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
    if (!nrow(ct)) return(NULL)
    ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    data.table(adjustment = lab, cell = cl, outcome = o, rel = ct$rel,
               est = ct$Estimate, se = ct$`Std. Error`)
  }
  grid <- CJ(o = outs, cl = c("Inc", "Inc-bind", "Inc-becomes"), sorted = FALSE)
  # "Inc" = pooled: relabel every increaser cell into one group first.
  stk_pooled <- copy(stk)[, treat_cell := fifelse(as.character(treat_cell) %in% inc_cells,
                                                  "Inc", as.character(treat_cell))]
  es <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
    o <- grid$o[i]; cl <- grid$cl[i]
    dat <- if (cl == "Inc") stk_pooled else stk
    rbindlist(list(es_one(dat, o, cl, "", "none"),
                   es_one(dat, o, cl, " + tau0:year", "A1 tau0 x year")), fill = TRUE)
  }), fill = TRUE)
  if (nrow(es)) {
    es[, `:=`(lo = est - 1.96*se, hi = est + 1.96*se)]
    fwrite(es, file.path(RESULTS_DIR, "level_adjusted_es.csv"))
    message("Saved level_adjusted_headline.csv / level_adjusted_es.csv")
  }
})


# Corrective stack weights (RUN_STACK_QWEIGHTS) ----
# Wing, Freedman & Hollingsworth, "Stacked Difference-in-Differences". The
# unweighted stacked regression does not identify the target ATT aggregate,
# because it weights each sub-experiment's TREATMENT and CONTROL trends
# differently. Their fix is a sample weight
#
#     Q_sa = 1                                       if unit s is treated in stack a
#     Q_sa = (N^D_a / N^D_Omega) / (N^C_a / N^C_Omega)   if unit s is a control
#
# counted in UNITS (municipalities), not observations. Under Q, treatment and
# control trends enter each sub-experiment with equal weight, they cancel under
# common trends, and the coefficient identifies the treated-count-weighted
# average of the sub-experiment ATTs.
#
# WHAT THIS CHANGES HERE. Every never-changer appears in every stack, so
# N^C_a / N^C_Omega is essentially 1/(number of stacks) and the weight reduces
# to scaling each stack's control arm by that stack's SHARE OF TREATED
# MUNICIPALITIES. Unweighted OLS instead weights each stack by its share of
# residual dose variance -- which is why bacon_cont_decomp.R finds 2010 and 2012
# carrying 74% of Inc-bind. Q-weights replace that precision rule with a
# treated-count rule, so the two answer different questions and both are valid.
#
# Q is defined from the TOTAL treated count per stack (any increaser cell),
# not per cell, because the control pool is shared across cells and the weight
# is meant to balance the treatment and control arms of each sub-experiment.
#
# The baseline-rate level control (tau0 x year, see RUN_LEVEL_ADJUST) is kept
# throughout and reported as a 2x2 against the weighting.
if (RUN_STACK_QWEIGHTS) local({
  outs <- c("log_live_and_work", "share_live_in", "log_in_commuters", "log_total_jobs")
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  stk <- copy(stk_muni)
  stk[is.na(dose_sd), dose_sd := 0]
  stk[, tau0 := mean(dose_stack[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(tau0), tau0 := NA_real_]
  stk[, direction_muni := factor(fcase(as.character(treat_cell) %in% inc_cells, "Inc",
                                       as.character(treat_cell) == "NC", "NC",
                                       default = NA_character_), levels = c("NC", "Inc"))]
  stk[, direction_sd := factor(fcase(as.character(cell_sd) %in% inc_cells, "Inc",
                                     as.character(cell_sd) == "NC", "NC",
                                     default = NA_character_), levels = c("NC", "Inc"))]

  cols <- c("muni","stack","year","rel","treat_cell","cell_sd","direction_muni",
            "direction_sd","dose_stack","dose_sd","tau0","stack_muni","stack_year",
            outs, ctrls)
  d <- stk[, ..cols]; d <- d[complete.cases(d)]

  # Q-weights, counted in municipalities per stack.
  d[, treated_unit := as.character(treat_cell) %in% inc_cells]
  cnt <- d[, .(ND_a = uniqueN(muni[treated_unit]),
               NC_a = uniqueN(muni[!treated_unit])), by = stack]
  cnt[, `:=`(ND_O = sum(ND_a), NC_O = sum(NC_a))]
  cnt[, q_ctrl := (ND_a / ND_O) / (NC_a / NC_O)]
  d <- merge(d, cnt[, .(stack, ND_a, NC_a, q_ctrl)], by = "stack")
  d[, Q := fifelse(treated_unit, 1, q_ctrl)]
  message(sprintf("[qweights] %d stacks; treated munis per stack %d-%d; control Q range %.3f-%.3f",
                  nrow(cnt), min(cnt$ND_a), max(cnt$ND_a), min(cnt$q_ctrl), max(cnt$q_ctrl)))
  print(cnt[order(stack), .(stack, ND_a, NC_a, q_ctrl = round(q_ctrl, 4))])

  ctrls_str <- paste(ctrls, collapse = " + ")
  fit <- function(o, spec, extra, wt) {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    sdv   <- if (spec == "decomp") "cell_sd"    else "direction_sd"
    f <- as.formula(paste0(o, " ~ i(", cellv, ", dose_stack, ref='NC') + i(",
                           sdv, ", dose_sd, ref='NC') + ", ctrls_str, extra,
                           " | stack_muni + stack_year"))
    if (is.null(wt)) feols(f, d, cluster = ~muni, notes = FALSE)
    else             feols(f, d, cluster = ~muni, weights = ~Q, notes = FALSE)
  }
  grab <- function(m, spec, o, lab) {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    grps  <- if (spec == "decomp") inc_cells    else "Inc"
    rbindlist(lapply(grps, function(k) {
      nm <- sprintf("%s::%s:dose_stack", cellv, k)
      if (!(nm %in% names(coef(m)))) return(NULL)
      data.table(spec = spec, variant = lab, outcome = o, cell = k,
                 est = unname(coef(m)[nm]), se = unname(se(m)[nm]),
                 p = unname(pvalue(m)[nm]), n = nobs(m))
    }))
  }
  grid <- list(c("unweighted",            "",            "no"),
               c("unweighted + tau0",     " + tau0:year", "no"),
               c("Q-weighted",            "",            "yes"),
               c("Q-weighted + tau0",     " + tau0:year", "yes"))
  res <- rbindlist(lapply(outs, function(o)
    rbindlist(lapply(c("pooled","decomp"), function(sp)
      rbindlist(lapply(grid, function(g)
        grab(fit(o, sp, g[2], if (g[3] == "yes") TRUE else NULL), sp, o, g[1])),
        fill = TRUE)), fill = TRUE)), fill = TRUE)
  fwrite(res, file.path(RESULTS_DIR, "stack_qweights.csv"))
  cat("\n---- corrective Q-weights vs unweighted (est) ----\n")
  print(dcast(res, spec + cell + outcome ~ variant, value.var = "est"))
  cat("\n---- standard errors ----\n")
  print(dcast(res, spec + cell + outcome ~ variant, value.var = "se"))
  message("Saved stack_qweights.csv")
})
