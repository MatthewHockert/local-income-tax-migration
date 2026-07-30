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
  #   property, repaired     current values below
  # The move from the raw to the repaired register is a measurement-error
  # correction in a CONTROL, not a specification change: Wilkes-Barre (an
  # Inc-bind unit) carried a municipal effective rate wrong by a factor of
  # roughly 25 across 2009-2019.
  # Lay explanation: a tripwire. If any future edit to the data or code
  # nudges the headline numbers away from the ones printed in the paper,
  # the script halts with an error instead of quietly writing new results
  # that no longer match the published tables.
  .chk <- coefs[grepl("Inc-bind:dose_stack", term)]
  .exp <- c(log_live_and_work = -0.539, log_in_commuters = -0.349,
            log_total_jobs = -0.428, share_live_in = -0.058)
  for (.o in names(.exp)) {
    .got <- .chk[outcome == .o, est]
    if (length(.got) != 1 || abs(.got - .exp[.o]) > 0.005)
      stop(sprintf("VALIDATION FAIL: Inc-bind %s = %s, expected %.3f (tol 0.005)",
                   .o, ifelse(length(.got) == 1, sprintf("%.3f", .got), "NA"), .exp[.o]))
  }
  print(paste("[validation] headline Inc-bind matches spec:",
              "log_LW=-0.539 log_IC=-0.349 log_TJ=-0.428 share=-0.058"))

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
              "cell_sd", "dose_sd", "cell_nr", "dose_nr", "log_eit_revenue", ctrls)
    dd <- es_stk[tcell %in% c("NC", cl), ..cols][complete.cases(es_stk[tcell %in% c("NC", cl), ..cols])]
    if (nrow(dd) == 0 || sum(dd$tcell == cl) == 0) next
    f <- as.formula(paste0("log_eit_revenue ~ i(rel, realized_dose_muni, ref=-1) + ",
          "i(cell_sd, dose_sd, ref='NC') + i(cell_nr, dose_nr, ref='NC') + ", ctrls_str,
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
