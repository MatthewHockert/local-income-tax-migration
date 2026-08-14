#!/usr/bin/env Rscript
# 02_main_models.R -- Main models stage (self-contained, flag-gated).
#
# Consolidates the former q3_unified_full_table.R (regression/output section),
# q3_unified_pooled.R, and q3_unified_eit_revenue.R into one script. The shared
# data prep (panel build + cell classification + muni stack) lives in
# _helpers.R::build_headline_env(), extracted verbatim from the headline script.
#
# Sections (each gated by a RUN_* flag):
#   1. Headline binding decomposition -- stacked continuous DiD, muni + SD
#      cell x dose, eight outcomes, with the paper-value validation gate.
#   2. Pooled Inc/Dec benchmark -- cells collapsed to {NC, Inc, Dec} per side;
#      per-pp dose-response and binary post-indicator analog.
#   3. EIT revenue response -- log EIT revenue through the binding
#      decomposition, realized-dose ES pre-trends, Laffer-style offset, and
#      baseline-rate splits.
#
# Inputs  (data/derived/, data/raw/ via build_headline_env and this script):
#   derived/pa_panel.rds, derived/pa_residence_panel.rds, derived/pa_rate_panel.rds,
#   raw/pa_afr_panel.csv, raw/multitax/pa_property_burden_panel.csv
# Outputs (data/results/):
#   headline_full_table.csv, headline_diagnostics.csv,
#   pooled_results.csv, pooled_diagnostics.csv,
#   eit_revenue_decomp.csv, eit_revenue_es.csv, eit_revenue_offset.csv,
#   eit_revenue_split.csv
#
# Run from package root:  Rscript scripts/02_main_models.R

suppressMessages({
  library(data.table)
  library(fixest)
})

.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_helpers.R"))

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
RUN_HEADLINE <- TRUE  # headline binding decomposition (8 outcomes) + validation gate
RUN_NOPROP   <- TRUE  # headline re-run WITHOUT the property effective-rate controls
RUN_POOLED   <- TRUE  # pooled Inc/Dec per-pp + binary benchmark (cousub)
RUN_REVENUE  <- TRUE  # log EIT revenue decomposition + ES + Laffer offset + splits
RUN_LEVELS_COMPARE <- TRUE  # headline + pooled re-run with the treatment in LEVELS
RUN_FLEX_DOSE <- TRUE  # headline + pooled re-run with a quadratic and a spline in the dose
RUN_REVENUE_TRADEOFF <- TRUE  # workers-vs-revenue tradeoff on ONE shared sample, and how it varies with the pre-existing rate

# Shared setup ----
# One panel build for the whole stage: classified cousub panel + muni stack.
# Lay explanation: the dataset is assembled once and every model below eats
# from the same plate -- the classified town panel and the stacked
# mini-experiments are identical across the headline, pooled, and revenue
# sections, so no two sections can quietly drift apart.
env <- build_headline_env()
panel <- env$panel
stk_muni <- env$stk_muni
ctrls <- env$ctrls
ctrls_str <- env$ctrls_str

# Headline binding decomposition ----
# Re-run the headline stacked-continuous DiD for the eight outcomes on the
# muni-rate side (with SD cells in the same regression as in Mode B), and
# produce a publication-ready table with SEs, R^2, Within R^2, N, FE info,
# and control-coefficient estimates.
if (RUN_HEADLINE) local({

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
                "log_total_jobs", "log_total_residents",
                "share_live_in", "share_work_outside", "share_nonres")

  run_headline <- function(stk, outcome) {
    # Unified resident-side spec: muni AND SD cell x dose in one regression.
    # cell_sd / dose_sd are computed in the panel build; bring them in.
    cols <- c("muni", "year", "treat_cell", "dose_stack",
              "cell_sd", "dose_sd", "stack_muni", "stack_year",
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
    # Lay explanation: town and school-district rate changes go into ONE
    # regression, so the town-rate effect is measured net of whatever the
    # school district was doing to its own rate at the same time -- both
    # governments tax the same paycheck and often move together.
    f <- as.formula(paste0(outcome,
         " ~ i(treat_cell, dose_stack, ref='NC') ",
         "+ i(cell_sd, dose_sd, ref='NC') + ",
         paste(ctrls, collapse = " + "),
         " | stack_muni + stack_year"))
    feols(f, data = d, cluster = ~muni)
  }

  models <- list()
  for (o in outcomes) {
    m <- run_headline(stk_muni, o)
    if (!is.null(m)) models[[o]] <- m
  }

  # Extract full coefficient tables + diagnostics
  all_coefs <- list()
  diagnostics <- list()

  for (o in names(models)) {
    m <- models[[o]]
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"), c("est", "se", "p"),
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
  fwrite(coefs, file.path(RESULTS_DIR, "headline_full_table.csv"))

  # Validation gate ----
  # Headline Inc-bind coefficients must match the paper. Halts loudly if a
  # future change makes the headline drift from the current values (Table:
  # headline_muni_combined; Inc-bind row, muni resident rate). Values updated
  # 2026-07-30 for the assessment-basis-repaired property panel (see
  # RUN_PROPERTY_REBASE in 01_data_prep.R). Lineage of this gate:
  #   pre-property           log_LW=-0.573 log_IC=-0.412 log_TJ=-0.490 share=-0.058
  #   property, raw register log_LW=-0.437 log_IC=-0.317 log_TJ=-0.391 share=-0.054
  #   property, repaired     log_LW=-0.539 log_IC=-0.349 log_TJ=-0.428 share=-0.058
  #   treatment in LEVELS    current values below
  # The last step is a description change, not a specification change: level and
  # baseline-differenced change are identical after stack_muni FE. The small
  # movement is sample composition -- the rate LEVEL is missing on muni-years
  # where the CHANGE is known to be zero, so listwise deletion drops ~1,300
  # stacked rows (172,795 -> 171,451 for log_live_and_work).
  # The move from the raw to the repaired register is a measurement-error
  # correction in a CONTROL, not a specification change: Wilkes-Barre (an
  # Inc-bind unit) carried a municipal effective rate wrong by a factor of
  # roughly 25 across 2009-2019.
  # Lay explanation: a tripwire. If any future edit to the data or code
  # nudges the headline numbers away from the ones printed in the paper,
  # the script halts with an error instead of quietly writing new results
  # that no longer match the published tables.
  .chk <- coefs[grepl("Inc-bind:dose_stack", term)]
  .exp <- c(log_live_and_work = -0.537, log_in_commuters = -0.345,
            log_total_jobs = -0.423, share_live_in = -0.058)
  for (.o in names(.exp)) {
    .got <- .chk[outcome == .o, est]
    if (length(.got) != 1 || abs(.got - .exp[.o]) > 0.005)
      stop(sprintf("VALIDATION FAIL: Inc-bind %s = %s, expected %.3f (tol 0.005)",
                   .o, ifelse(length(.got) == 1, sprintf("%.3f", .got), "NA"), .exp[.o]))
  }
  print(paste("[validation] headline Inc-bind matches spec:",
              "log_LW=-0.537 log_IC=-0.345 log_TJ=-0.423 share=-0.058"))

  diag_dt <- rbindlist(diagnostics, fill = TRUE)
  fwrite(diag_dt, file.path(RESULTS_DIR, "headline_diagnostics.csv"))

  print(diag_dt)

  # Inc-bind coefficient with SE per outcome
  print(coefs[grepl("Inc-bind:dose_stack", term),
              .(outcome, term, est = round(est, 3), se = round(se, 3),
                p = round(p, 4), n)])

  # Control coefficients on log_live_and_work
  ctrl_lw <- coefs[outcome == "log_live_and_work" & term %in% ctrls]
  print(ctrl_lw[, .(term, est = round(est, 4), se = round(se, 4),
                    p = round(p, 4), sig)])
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
# Collapses the six binding-state cells into two aggregate treatments ("any
# Inc event", "any Dec event") to produce a literature-comparable per-pp
# dose-response and a binary event-indicator analog. SD-side cell x dose
# interactions retained from the headline spec; controls and fixed effects
# identical to the headline.
#
# COUSUB-ONLY (repo-cleanup, 2026-05): the SD-unit and block-unit pooled
# regressions were trimmed out when those estimators were retired. The kept
# papers (UEA_draft.tex, muni_tax_sorting.tex) report only the cousub
# pooled benchmark. Prior 3-level version is in git history.
#
# Notation:
#   direction in {NC, Inc, Dec}      for the per-pp spec (interacted with dose)
#   ever_inc_post, ever_dec_post     for the binary spec (post-treatment 0/1)
if (RUN_POOLED) local({

  # copy(): this section adds columns to the stack by reference (direction_*,
  # rel, ever_*); work on a private copy so the shared stk_muni stays clean.
  stk_muni <- copy(stk_muni)
  ctrls_muni <- ctrls

  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")

  direction_levels <- c("NC", "Inc", "Dec")

  muni_outcomes  <- c("log_live_and_work", "log_in_commuters",
                      "log_out_commuters", "log_total_jobs",
                      "log_total_residents",
                      "share_live_in", "share_work_outside", "share_nonres")
  block_outcomes <- c("log_home_count", "log_work_count",
                      "home_in_muni_share", "share_work_outside")

  # Collect results across all (level, mode, outcome) combinations
  results <- list()
  diagnostics <- list()
  full_tables <- list()   # unfiltered coeftables (per-pp) for the full paper table
  save_result <- function(level_tag, mode, outcome, m, term_filter = "direction::|ever_inc_post|ever_dec_post") {
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"), c("est", "se", "p"), skip_absent = TRUE)
    if (mode == "per_pp") {
      full_tables[[length(full_tables) + 1L]] <<-
        copy(ct)[, `:=`(level = level_tag, outcome = outcome, n = nobs(m))]
    }
    ic <- ct[grepl(term_filter, term)]
    if (nrow(ic) == 0) return(invisible())
    ic[, level := level_tag]
    ic[, mode  := mode]
    ic[, outcome := outcome]
    ic[, n := nobs(m)]
    results[[length(results) + 1L]] <<- ic[, .(level, mode, outcome, term,
                                               est, se, p, n)]
    dep_vec <- m$residuals + m$fitted.values
    diagnostics[[length(diagnostics) + 1L]] <<- data.table(
      level = level_tag, mode = mode, outcome = outcome,
      n = nobs(m),
      r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
      wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
      dep_sd   = round(sd(dep_vec, na.rm = TRUE), 4))
    invisible()
  }

  # Direction tags pooled per rate-setter side: muni side (from treat_cell)
  # and SD side (from cell_sd). Each side is collapsed to {NC, Inc, Dec}.
  # Pooled per-pp regression is:
  #   y = FE + i(direction_muni, dose_muni) + i(direction_sd, dose_sd) + X
  # producing four coefficients of interest: muni-Inc, muni-Dec, SD-Inc, SD-Dec.
  # Lay explanation: here we deliberately throw away the binding detail and
  # keep only the direction of each change (Inc / Dec / never). This mimics
  # what a standard design that ignores the max-rule would estimate, giving
  # a benchmark to hold the cell-by-cell results against.
  stk_muni[, direction_muni := fcase(
    as.character(treat_cell) %in% inc_cells, "Inc",
    as.character(treat_cell) %in% dec_cells, "Dec",
    as.character(treat_cell) == "NC", "NC",
    default = NA_character_)]
  stk_muni[, direction_sd := fcase(
    as.character(cell_sd) %in% inc_cells, "Inc",
    as.character(cell_sd) %in% dec_cells, "Dec",
    as.character(cell_sd) == "NC", "NC",
    default = NA_character_)]
  stk_muni[, direction_muni := factor(direction_muni, levels = direction_levels)]
  stk_muni[, direction_sd   := factor(direction_sd,   levels = direction_levels)]
  # Binary post-treatment indicators per side
  stk_muni[, rel := year - stack]
  stk_muni[, ever_muni_inc_post := as.integer(direction_muni == "Inc" & rel >= 0)]
  stk_muni[, ever_muni_dec_post := as.integer(direction_muni == "Dec" & rel >= 0)]
  stk_muni[, ever_sd_inc_post   := as.integer(direction_sd   == "Inc" & rel >= 0)]
  stk_muni[, ever_sd_dec_post   := as.integer(direction_sd   == "Dec" & rel >= 0)]

  run_muni_pooled <- function(outcome) {
    cols <- c("muni", "year",
              "direction_muni", "dose_stack", "direction_sd", "dose_sd",
              "ever_muni_inc_post", "ever_muni_dec_post",
              "ever_sd_inc_post", "ever_sd_dec_post",
              "stack_muni", "stack_year",
              outcome, ctrls_muni)
    cols <- intersect(cols, names(stk_muni))
    d <- stk_muni[, ..cols]
    if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) return(invisible())
    # per-pp pooled: both sides pooled to Inc/Dec/NC
    f_pp <- as.formula(paste0(outcome,
         " ~ i(direction_muni, dose_stack, ref='NC')",
         " + i(direction_sd,   dose_sd,    ref='NC') + ",
         paste(ctrls_muni, collapse = " + "),
         " | stack_muni + stack_year"))
    m_pp <- feols(f_pp, data = d, cluster = ~muni)
    save_result("cousub", "per_pp", outcome, m_pp,
                term_filter = "direction_(muni|sd)::")
    # binary pooled: 4 post-treatment indicators
    f_bin <- as.formula(paste0(outcome,
         " ~ ever_muni_inc_post + ever_muni_dec_post",
         " + ever_sd_inc_post + ever_sd_dec_post + ",
         paste(ctrls_muni, collapse = " + "),
         " | stack_muni + stack_year"))
    m_bin <- feols(f_bin, data = d, cluster = ~muni)
    save_result("cousub", "binary", outcome, m_bin,
                term_filter = "ever_(muni|sd)_(inc|dec)_post")
  }
  for (o in muni_outcomes) run_muni_pooled(o)

  res_dt <- rbindlist(results, fill = TRUE)
  # Extract clean "side" and "direction" columns from the term names.
  # Each pooled regression contains up to four coefficients of interest:
  #   muni-Inc, muni-Dec, SD-Inc, SD-Dec
  res_dt[, side := fcase(
    grepl("direction_muni::",        term), "muni",
    grepl("direction_sd::",          term), "sd",
    grepl("ever_muni_(inc|dec)_post", term), "muni",
    grepl("ever_sd_(inc|dec)_post",   term), "sd",
    # SD-only level (no muni side in regression): the lone "direction::"
    grepl("direction::",             term), "sd",
    grepl("ever_inc_post|ever_dec_post", term), "sd",
    default = NA_character_)]
  res_dt[, direction := fcase(
    grepl("Inc",   term, ignore.case = TRUE), "Inc",
    grepl("Dec",   term, ignore.case = TRUE), "Dec",
    default = NA_character_)]
  fwrite(res_dt, file.path(RESULTS_DIR, "pooled_results.csv"))
  fwrite(rbindlist(diagnostics, fill = TRUE),
         file.path(RESULTS_DIR, "pooled_diagnostics.csv"))
  fwrite(rbindlist(full_tables, fill = TRUE),
         file.path(RESULTS_DIR, "pooled_full_table.csv"))

  # Pooled Inc/Dec summary
  print(res_dt[, .(level, mode, outcome, direction,
                   est = round(est, 3), se = round(se, 3),
                   p = round(p, 4), n = format(n, big.mark = ","))])
})

# EIT revenue response ----
# Task 3: run LOG EIT REVENUE through the binding decomposition (the same
#   stacked continuous-DiD as the headline, Inc-bind/Inc-becomes/Inc-nobind
#   cells vs NC). The Inc-bind coefficient is the NET per-pp revenue response
#   (mechanical + behavioral combined) -- the cleanest "returns" result.
#
# Task 4: Laffer-style behavioral offset. The mechanical-only benchmark --
#   what a 1pp increase raises if the base does not move -- is, in log terms,
#   d log R / d tau = 1/tau_pre (since R = tau * B, log R = log tau + log B,
#   and B is held fixed). Comparing the net per-pp response beta_net to this:
#       offset = 1 - beta_net / beta_mech = 1 - beta_net * tau_pre
#   = the share of the mechanically projected revenue gain offset by the
#   sorting response. This is a WITHIN-PA LOCAL-DISPLACEMENT offset estimated
#   on the 24-muni, Home-Rule-heavy Inc-bind cohort -- NOT a state-level
#   fiscal multiplier.
#
# EIT revenue series: the AFR EIT line, coalesced across DCED's two reporting
#   eras -- `earned_income_tax_revenues` (2006+) and `earned_income_tax`
#   (2002-2005), which are disjoint by year. In the 2006+ family the Local
#   Services Tax is a SEPARATE column (`local_services_tax_revenues`), so EIT
#   is not folded in with LST (no LST fill-in). This is the same EIT measure
#   the fiscal-control share `share_eit` uses, so the outcome aligns with the
#   controls; the pre-2006 years drop out because the control denominators
#   (total_revenues) begin in 2006.
if (RUN_REVENUE) local({

  # copy(): this section merges new columns into the panel and then mutates it
  # by reference (log_eit_revenue, realized_dose_muni); work on a private copy
  # so the shared panel stays clean for other sections.
  panel <- copy(panel)

  K_stack <- 5L
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind", "Inc-loses", "Dec-gains")

  # Drop share_eit (= EIT revenue / total revenue) from the controls: with log
  # EIT revenue as the OUTCOME it is mechanically the same object, so including
  # it would absorb the revenue response and bias the coefficient toward zero.
  # Keep the other fiscal shares (none contains EIT revenue).
  ctrls <- setdiff(ctrls, "share_eit")
  ctrls_str <- paste(ctrls, collapse = " + ")
  rate <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))

  # Stack builder (same Cengiz-style builder as build_headline_env; inlined
  # here because the shared stk_muni predates the log_eit_revenue merge --
  # the outcome must be on the panel BEFORE stacking).
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
    stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
    stk
  }

  # Clean EIT revenue (AFR earned_income_tax, directly reported; NO LST fill-in)
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  afr[, yr := as.integer(ifelse(!is.na(reporting_year), reporting_year, year))]
  # Coalesce the two reporting eras; LST is a separate line so EIT excludes it.
  afr[, eit_raw := suppressWarnings(as.numeric(
        ifelse(!is.na(earned_income_tax_revenues), earned_income_tax_revenues, earned_income_tax)))]
  eit <- afr[!is.na(cousub_fips) & !is.na(yr),
             .(eit_rev = mean(eit_raw, na.rm = TRUE)),
             by = .(muni = cousub_fips, year = yr)]
  eit[!is.finite(eit_rev) | eit_rev <= 0, eit_rev := NA_real_]   # drop missing/zero; never fill with LST
  panel <- merge(panel, eit, by = c("muni", "year"), all.x = TRUE)
  panel[, log_eit_revenue := ifelse(!is.na(eit_rev) & eit_rev > 0, log(eit_rev), NA_real_)]
  print(sprintf("EIT-revenue muni-years: %s of %s (%.0f%%); Inc-bind treated muni-years with EIT: %d",
                format(sum(!is.na(panel$log_eit_revenue)), big.mark = ","),
                format(nrow(panel), big.mark = ","),
                100 * mean(!is.na(panel$log_eit_revenue)),
                panel[cell_muni == "Inc-bind" & !is.na(log_eit_revenue), .N]))

  # Static binding decomposition on log EIT revenue (headline spec)
  stk <- build_stack(panel, "muni", "dose_muni")
  stk[, treat_cell := factor(as.character(treat_cell), levels = cell_levels)]
  stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
  stk[is.na(dose_sd), dose_sd := 0]
  scols <- c("log_eit_revenue", "treat_cell", "dose_stack", "cell_sd", "dose_sd",
             "stack_muni", "stack_year", "muni", ctrls)
  dd <- stk[, ..scols][complete.cases(stk[, ..scols])]
  print(sprintf("static EIT regression sample: %s rows; Inc-cell treated rows = %d",
                format(nrow(dd), big.mark = ","), dd[treat_cell %in% inc_cells, .N]))
  f_static <- as.formula(paste0("log_eit_revenue ~ i(treat_cell, dose_stack, ref='NC') ",
              "+ i(cell_sd, dose_sd, ref='NC') + ", ctrls_str, " | stack_muni + stack_year"))
  m <- feols(f_static, data = dd, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^treat_cell::", term)]
  ct[, cell := sub("treat_cell::([^:]+):.*", "\\1", term)]
  decomp <- ct[cell %in% inc_cells,
               .(outcome = "log_eit_revenue", cell, beta = round(Estimate, 3),
                 se = round(`Std. Error`, 3), p = round(`Pr(>|t|)`, 4), n = nobs(m))]
  setkey(decomp, cell)
  # Task 3: net per-pp revenue response (log EIT revenue, Inc cells vs NC)
  print(decomp[c("Inc-bind", "Inc-becomes", "Inc-nobind")])
  fwrite(decomp, file.path(RESULTS_DIR, "eit_revenue_decomp.csv"))

  # Realized-dose ES (pre-trend test) on log EIT revenue
  mu <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu, muni, year)
  rd <- mu[!is.na(c_muni), {
    pre <- resident_eit[year == c_muni - 1L]
    yend <- min(c_muni + K_stack, max(year, na.rm = TRUE)); endv <- resident_eit[year == yend]
    list(realized_dose_muni = (if (length(endv) == 1) endv else NA_real_) -
                              (if (length(pre) == 1) pre else NA_real_))
  }, by = muni]
  rd[is.na(realized_dose_muni), realized_dose_muni := 0]
  rd <- rbind(rd, data.table(muni = unique(panel[cell_muni == "NC", muni]), realized_dose_muni = 0))
  panel <- merge(panel, unique(rd, by = "muni"), by = "muni", all.x = TRUE)
  panel[is.na(realized_dose_muni), realized_dose_muni := 0]

  treated <- unique(panel[cell_muni != "NC" & !is.na(c_muni), .(muni, cohort = c_muni)])
  ncm <- unique(panel[cell_muni == "NC", muni])
  es_stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    d_t <- panel[muni %in% treated[cohort == cy, muni] & year %in% win]; d_t[, tcell := cell_muni]
    d_c <- panel[muni %in% ncm & year %in% win]; d_c[, tcell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE); if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
  es_stk[, stack_muni := paste0(stack, "_", muni)]; es_stk[, stack_year := paste0(stack, "_", year)]
  es_stk[, tcell := factor(tcell, levels = cell_levels)]
  es_stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
  es_stk[, cell_nr := factor(as.character(cell_nr), levels = cell_levels)]
  es_stk[is.na(dose_sd), dose_sd := 0]; es_stk[is.na(dose_nr), dose_nr := 0]

  es_res <- list(); att_res <- list()
  for (cl in inc_cells) {
    cols <- c("muni", "year", "stack_muni", "stack_year", "tcell", "realized_dose_muni", "rel",
              "cell_sd", "dose_sd", "log_eit_revenue", ctrls)
    dd <- es_stk[tcell %in% c("NC", cl), ..cols][complete.cases(es_stk[tcell %in% c("NC", cl), ..cols])]
    if (nrow(dd) == 0 || sum(dd$tcell == cl) == 0) next
    f <- as.formula(paste0("log_eit_revenue ~ i(rel, realized_dose_muni, ref=-1) + ",
          "i(cell_sd, dose_sd, ref='NC') + ", ctrls_str,
          " | stack_muni + stack_year"))
    mm <- tryCatch(feols(f, data = dd, cluster = ~muni), error = function(ee) NULL)
    if (is.null(mm)) next
    ic <- as.data.table(mm$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    es_res[[cl]] <- ic[, .(cell = cl, rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    wp <- wald_pretrend(mm, "^rel::-[2-9]")
    att_res[[cl]] <- data.table(cell = cl, n_munis = uniqueN(dd[tcell == cl, muni]),
      es_post = round(mean(ic[rel >= 0 & rel <= 5, Estimate], na.rm = TRUE), 3),
      es_pre  = round(mean(ic[rel <= -2, Estimate], na.rm = TRUE), 3),
      wald_pre_p = round(wp$p, 4))
  }
  att_dt <- rbindlist(att_res, fill = TRUE)
  fwrite(rbindlist(es_res, fill = TRUE), file.path(RESULTS_DIR, "eit_revenue_es.csv"))
  # Realized-dose ES pre-trends (log EIT revenue)
  print(att_dt)

  # Task 4: Laffer-style behavioral offset (Inc-bind).
  # beta_mech = 1/tau_pre; offset = 1 - beta_net * tau_pre. Within-PA
  # local-displacement offset on the Home-Rule-heavy Inc-bind cohort;
  # not a state-level fiscal multiplier.
  # Lay explanation: if nobody moved, raising the rate would lift revenue
  # mechanically -- rate up, same tax base, more money. The offset is the
  # share of that promised gain eaten away because the base shrinks as
  # workers sort away: 0 = the town keeps it all, 1 = fully eaten.
  beta_net <- decomp["Inc-bind", beta]
  base <- panel[cell_muni == "Inc-bind", {cm <- c_muni[1L]; .(tau = resident_eit[match(cm - 1L, year)])}, by = muni]
  tau_mean <- mean(base$tau, na.rm = TRUE); tau_med <- median(base$tau, na.rm = TRUE)
  offset_at <- function(tp) 1 - beta_net * tp
  offset_dt <- data.table(beta_net = beta_net, tau_pre_mean = round(tau_mean, 3),
                          tau_pre_median = round(tau_med, 3),
                          offset_at_mean = round(offset_at(tau_mean), 3),
                          offset_at_median = round(offset_at(tau_med), 3))
  print(offset_dt)
  fwrite(offset_dt, file.path(RESULTS_DIR, "eit_revenue_offset.csv"))

  # Split EACH Inc cell (bind / becomes / nobind) by pre-event baseline rate,
  # to break the high-rate Home Rule / Act 47 cities out of the standard
  # (sub-1pp Act 511 cap) municipalities within every cell.
  base_all <- panel[cell_muni %in% inc_cells,
    {cm <- c_muni[1L]; .(cell0 = as.character(cell_muni[1L]),
                         tau_pre = resident_eit[match(cm - 1L, year)])}, by = muni]

  build_stack_split <- function(p, levels, K = K_stack) {
    treated <- unique(p[cell_split != "NC" & !is.na(c_muni), .(muni, cohort = c_muni, cell = cell_split)])
    ncm <- unique(p[cell_split == "NC", muni])
    stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
      win <- (cy - K):(cy + K)
      d_t <- p[muni %in% treated[cohort == cy, muni] & year %in% win]
      d_t <- merge(d_t, treated[cohort == cy, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
      d_c <- p[muni %in% ncm & year %in% win]; d_c[, treat_cell := "NC"]
      d <- rbind(d_t, d_c, fill = TRUE); if (nrow(d) == 0) return(NULL)
      d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
    }), fill = TRUE)
    stk[, stack_muni := paste0(stack, "_", muni)]; stk[, stack_year := paste0(stack, "_", year)]
    stk[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE), by = .(stack, muni)]
    stk[!is.finite(base_rate), base_rate := 0]; stk[, dose_stack := dose_muni - base_rate]
    stk[, treat_cell := factor(treat_cell, levels = levels)]
    stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]; stk[is.na(dose_sd), dose_sd := 0]
    stk
  }

  run_split <- function(thresh, split_name) {
    pn <- copy(panel); pn[, cell_split := as.character(cell_muni)]
    for (cl in inc_cells) {
      pn[muni %in% base_all[cell0 == cl & tau_pre <  thresh, muni], cell_split := paste0(cl, "-lo")]
      pn[muni %in% base_all[cell0 == cl & tau_pre >= thresh, muni], cell_split := paste0(cl, "-hi")]
    }
    groups <- paste0(rep(inc_cells, each = 2), c("-lo", "-hi"))
    stk <- build_stack_split(pn, c("NC", groups))
    cols <- c("log_eit_revenue", "treat_cell", "dose_stack", "cell_sd", "dose_sd",
              "stack_muni", "stack_year", "muni", ctrls)
    dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
    m <- feols(as.formula(paste0("log_eit_revenue ~ i(treat_cell, dose_stack, ref='NC') ",
          "+ i(cell_sd, dose_sd, ref='NC') + ", ctrls_str, " | stack_muni + stack_year")),
          data = dd, cluster = ~muni)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^treat_cell::", term)]
    ct[, grp := sub("treat_cell::([^:]+):.*", "\\1", term)]
    rbindlist(lapply(groups, function(gg) {
      cl <- sub("-(lo|hi)$", "", gg); hi <- grepl("-hi$", gg)
      bg <- if (hi) base_all[cell0 == cl & tau_pre >= thresh, tau_pre] else base_all[cell0 == cl & tau_pre < thresh, tau_pre]
      tp <- mean(bg, na.rm = TRUE); row <- ct[grp == gg]
      b  <- if (nrow(row)) row$Estimate[1] else NA_real_
      data.table(split = split_name, cell = gg, n = length(bg), tau_pre = round(tp, 3),
                 beta_net = round(b, 3),
                 se = if (nrow(row)) round(row$`Std. Error`[1], 3) else NA_real_,
                 p  = if (nrow(row)) round(row$`Pr(>|t|)`[1], 4) else NA_real_,
                 offset = round(1 - b * tp, 3))
    }), fill = TRUE)
  }

  # Baseline-rate split of all three Inc cells (log EIT revenue).
  # Thresholds: cap = 1pp Act 511 combined cap; median = 0.5pp.
  # -lo = below, -hi = at/above.
  split_res <- rbind(run_split(1.0, "cap_1pp"), run_split(0.5001, "median_0.5pp"))
  fwrite(split_res, file.path(RESULTS_DIR, "eit_revenue_split.csv"))
  print(split_res)
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

# ---------------------------------------------------------------------------
# Treated pre-period baselines for the results tables
# ---------------------------------------------------------------------------
# The "Dep. mean" convention breaks for this design in two ways, so the two
# outcome types get different baselines and different labels.
#
# LOGGED outcomes: mean(log Y) is meaningless to print and exponentiating it is
# a trap, since exp(mean(log Y)) is the GEOMETRIC mean, not the arithmetic one
# (374 against 999 here). The magnitude conversion is already handled by
# exp(beta)-1, so the baseline's job is sample transparency: it tells a reader
# these are municipalities of a few hundred resident workers, not a few dozen
# and not a metro. Reported in levels and labelled as a geometric mean.
#
# SHARE outcome: not logged, so beta is uninterpretable without a denominator.
# -0.021 against a baseline of 0.15 is a 14 percent proportional shift; against
# the full-sample 0.09 it would read as 23 percent. The relevant baseline is the
# TREATED units before treatment, not the sample, which is dominated by
# never-changers with much lower shares.
#
# Both are computed over t in [c-5, c-1] for treated municipalities in the
# estimation sample. Output -> data/results/treated_pre_baselines.csv
local({
  inc  <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  cols <- c("muni","treat_cell","rel","live_and_work","in_commuters",
            "total_jobs","share_live_in", ctrls)
  d <- stk_muni[, ..cols]
  d <- d[complete.cases(d[, ..ctrls])]
  pre <- d[as.character(treat_cell) %in% inc & rel >= -5 & rel <= -1]
  gm <- function(x) { x <- x[is.finite(x) & x > 0]; if (!length(x)) NA_real_ else exp(mean(log(x))) }
  out <- rbind(
    data.table(outcome = "log_live_and_work", baseline = gm(pre$live_and_work),
               kind = "geometric mean, workers"),
    data.table(outcome = "log_in_commuters",  baseline = gm(pre$in_commuters),
               kind = "geometric mean, workers"),
    data.table(outcome = "log_total_jobs",    baseline = gm(pre$total_jobs),
               kind = "geometric mean, jobs"),
    data.table(outcome = "share_live_in",     baseline = mean(pre$share_live_in, na.rm = TRUE),
               kind = "mean share, proportion"))
  out[, `:=`(n_munis = uniqueN(pre$muni), n_muni_years = nrow(pre))]
  fwrite(out, file.path(RESULTS_DIR, "treated_pre_baselines.csv"))
  message("Saved data/results/treated_pre_baselines.csv")
  print(out)
  # Attach to the diagnostics the paper tables read, so the row prints
  # alongside N and R-squared without a second lookup.
  for (f in c("headline_diagnostics.csv", "pooled_diagnostics.csv")) {
    p <- file.path(RESULTS_DIR, f)
    if (!file.exists(p)) next
    dg <- fread(p)
    dg[, pre_baseline := NULL]
    dg <- merge(dg, out[, .(outcome, pre_baseline = baseline)], by = "outcome", all.x = TRUE)
    fwrite(dg, p)
    message("  merged pre_baseline into ", f)
  }
})

#==========================================================
# Flexible dose: the headline specification with a quadratic and a spline
#----------------------------------------------------------
# The headline enters the dose linearly, so its coefficient IS the ACR by
# construction: with ACRT(d) constant the integral of ACRT against omega
# returns beta for any omega. This block relaxes that and computes the ACR the
# way it is defined, as the dose-weighted average of ACRT(d).
#
# Everything except the dose term is the headline. Same stacked panel, same
# {NC + Inc cells} sample, same fiscal controls, same SD co-treatment, same
# muni x stack and year x stack fixed effects, same clustering at the
# municipality. All three models share one complete-cases column set so the
# samples are identical rather than merely similar.
#
# One deliberate departure. The headline regressor is the rate LEVEL. Under
# unit fixed effects a linear term in the level equals a linear term in the
# change, since level = baseline + change and the baseline is absorbed, which
# is why the linear column below reproduces the headline exactly. That
# equivalence fails for anything nonlinear: d^2 = b^2 + 2bc + c^2 leaves a
# baseline-by-change cross term the fixed effects cannot remove, and fitting
# the quadratic in levels flips both coefficients and turns the d^2 p-value
# from 0.73 to 0.06. All three columns therefore use the CHANGE.
# Outputs: data/results/flex_dose_acr.csv
if (RUN_FLEX_DOSE) local({
  suppressMessages(library(splines))
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  stk <- copy(stk_muni)
  stk[is.na(dose_sd), dose_sd := 0]
  stk[, dose := fifelse(as.character(treat_cell) %in% inc_cells, dose_change, 0)]
  stk[, dose2 := dose^2]
  kk <- quantile(stk[dose > 0, dose], c(1/3, 2/3), na.rm = TRUE)
  bb <- c(0, max(stk[dose > 0, dose], na.rm = TRUE))
  B  <- ns(stk$dose, knots = kk, Boundary.knots = bb)
  bn <- paste0("sp", seq_len(ncol(B)))
  stk[, (bn) := as.data.table(unclass(B))]
  stk[, direction_muni := factor(fcase(as.character(treat_cell) %in% inc_cells, "Inc",
                                       as.character(treat_cell) == "NC", "NC",
                                       default = NA_character_), levels = c("NC", "Inc"))]
  stk[, direction_sd := factor(fcase(as.character(cell_sd) %in% inc_cells, "Inc",
                                     as.character(cell_sd) == "NC", "NC",
                                     default = NA_character_), levels = c("NC", "Inc"))]

  outs <- c("log_live_and_work", "share_live_in", "log_in_commuters", "log_total_jobs")
  # omega(d): one weight per treated municipality, at the dose it took.
  wts <- function(cells) {
    u <- unique(stk[as.character(treat_cell) %in% cells & dose > 0, .(muni, dose)])
    u[, .(dose = max(dose)), by = muni]$dose
  }
  # Basis derivative at a vector of doses, for the spline ACR.
  Bprime <- function(x, h = 1e-5) {
    hi <- pmin(x + h, bb[2]); lo <- pmax(x - h, 0)
    (predict(B, hi) - predict(B, lo)) / (hi - lo)
  }
  # ACR is a linear combination w'b in every case, so the SE is sqrt(w'Vw).
  # Returns estimate, SE and a two-sided p-value. The ACR is a linear
  # combination of the fitted coefficients, so its variance is w'Vw and the
  # test is the usual one on est/SE, referred to the cluster count.
  lincom <- function(m, nm, w) {
    if (!all(nm %in% names(coef(m)))) return(c(NA_real_, NA_real_, NA_real_))
    b <- coef(m)[nm]; V <- vcov(m)[nm, nm, drop = FALSE]
    est <- as.numeric(w %*% b); se <- sqrt(as.numeric(t(w) %*% V %*% w))
    dfree <- tryCatch(fixest::degrees_freedom(m, "t"), error = function(e) Inf)
    c(est, se, 2 * stats::pt(-abs(est / se), df = dfree))
  }

  rows <- list()
  for (spec in c("decomp", "pooled")) {
    cellv <- if (spec == "decomp") "treat_cell" else "direction_muni"
    sdv   <- if (spec == "decomp") "cell_sd"    else "direction_sd"
    grps  <- if (spec == "decomp") inc_cells    else "Inc"
    for (o in outs) {
      # dose_stack is carried into complete.cases but NEVER enters a formula.
      # It is the headline's regressor, and dose_change is non-missing on rows
      # where the rate level is not, so without it this block would silently run
      # on a larger sample than the headline and the linear column would not
      # reproduce it. Same trap the Act 47 block fell into.
      cols <- c("muni", cellv, sdv, "dose", "dose2", bn, "dose_sd", "dose_stack",
                "stack_muni", "stack_year", o, ctrls)
      d <- stk[, ..cols]; d <- d[complete.cases(d)]
      sd_term <- sprintf("i(%s, dose_sd, ref='NC')", sdv)
      fml <- function(rhs) as.formula(paste0(o, " ~ ", rhs, " + ", sd_term, " + ",
                                             paste(ctrls, collapse = " + "),
                                             " | stack_muni + stack_year"))
      mL <- feols(fml(sprintf("i(%s, dose, ref='NC')", cellv)), d, cluster = ~muni, notes = FALSE)
      mQ <- feols(fml(sprintf("i(%s, dose, ref='NC') + i(%s, dose2, ref='NC')", cellv, cellv)),
                  d, cluster = ~muni, notes = FALSE)
      mS <- feols(fml(paste(sprintf("i(%s, %s, ref='NC')", cellv, bn), collapse = " + ")),
                  d, cluster = ~muni, notes = FALSE)
      for (g in grps) {
        wd <- if (spec == "decomp") wts(g) else wts(inc_cells)
        if (!length(wd)) next
        lin <- lincom(mL, sprintf("%s::%s:dose", cellv, g), 1)
        qd  <- lincom(mQ, sprintf("%s::%s:%s", cellv, g, c("dose", "dose2")),
                      c(1, 2 * mean(wd)))
        sp  <- lincom(mS, sprintf("%s::%s:%s", cellv, g, bn), colMeans(Bprime(wd)))
        rows[[length(rows) + 1L]] <- data.table(
          spec = spec, cell = g, outcome = o,
          acr_linear = lin[1], se_linear = lin[2], p_linear = lin[3],
          acr_quad   = qd[1],  se_quad   = qd[2],  p_quad   = qd[3],
          acr_spline = sp[1],  se_spline = sp[2],  p_spline = sp[3],
          n = nobs(mL), n_treated = length(wd), mean_dose = mean(wd))
      }
    }
  }
  out <- rbindlist(rows)
  fwrite(out, file.path(RESULTS_DIR, "flex_dose_acr.csv"))
  message("[flex-dose] ACR under three dose specifications:")
  print(out[outcome == "log_live_and_work",
            .(spec, cell, linear = round(acr_linear, 3), quad = round(acr_quad, 3),
              spline = round(acr_spline, 3), n_treated, mean_dose = round(mean_dose, 3))])
  # Gate: the linear column must reproduce the headline, which is the whole
  # basis for reading the other two as the same object under a looser form.
  hl <- fread(file.path(RESULTS_DIR, "headline_full_table.csv"))
  chk <- merge(out[spec == "decomp", .(cell, outcome, acr_linear)],
               hl[grepl("^treat_cell::", term),
                  .(cell = sub(".*::(.*):dose_stack", "\\1", term), outcome, hl = est)],
               by = c("cell", "outcome"))
  chk[, gap := abs(acr_linear - hl)]
  if (nrow(chk)) message(sprintf("[flex-dose] max |linear - headline| = %.6f across %d rows",
                                 max(chk$gap), nrow(chk)))
  stopifnot("flex-dose linear column does not reproduce the headline" = all(chk$gap < 1e-6))
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

  cols <- c("muni", "treat_cell", "cell_sd", "direction_muni", "direction_sd",
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
  print(res[, .(spec, cell, beta_workers, beta_revenue, rho, rho_lo, rho_hi,
                tau0_mean, offset, n)])
  message("Saved revenue_tradeoff.csv / _gradient.csv / _doseresponse.csv")
})
