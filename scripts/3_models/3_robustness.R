#!/usr/bin/env Rscript
# 3_models/3_robustness.R -- Every robustness / alternative-specification model,
# consolidated from the former standalone scripts:
#   q3_unified_dcdh_robustness.R      -> RUN_DCDH_POOLED
#   q3_unified_dcdh_bycell.R          -> RUN_DCDH_BYCELL
#   q3_unified_act32_3level.R         -> RUN_ACT32
#   q3_unified_act47_robustness.R     -> RUN_ACT47
#   q3_unified_curvature_robustness.R -> RUN_CURVATURE
#   pre-trend battery (moved in from stage 03, 2026-07-07)
#
# All sections share ONE headline environment (build_headline_env() in
# helpers/helpers.R): the cousub panel with binding-cell classification and the
# muni-side Cengiz stack -- identical to what q3_unified_full_table.R built.
#
# Outputs -> data/results/*.csv, figures/*.png, tables/act47_robustness.tex.
# Run from package root:  Rscript scripts/3_models/3_robustness.R

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})
# DIDmultiplegtDYN is heavy to load; it is attached inside the sections that
# need it (dCDH pooled / dCDH by cell / curvature Part B) rather than here.

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
# The two dCDH sections are slow: RUN_DCDH_POOLED ~5 min, RUN_DCDH_BYCELL ~30 min.
RUN_DCDH_POOLED <- TRUE
RUN_DCDH_BYCELL <- TRUE
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

# Pull the estimates out of a did_multiplegt_dyn object into a tidy table:
# Effects = post-event rows (rel 1..K), Placebos = pre-event rows (rel -1..-K,
# these are the pre-trend test), plus a rel=0 reference row at exactly 0.
# NOTE the scale: these are per-event LEVEL effects at the realized dose,
# NOT per-pp slopes -- compare to feols only after multiplying the feols ACR
# by the group's mean dose (~0.33 pp). Used by both dCDH sections.
# Lay explanation: the two estimators answer in different units. The headline
# reports a price-per-mile ("effect per percentage point of tax"); dCDH
# reports the whole trip ("effect of the hike each town actually took").
# Since a typical hike is about a third of a point, the headline number must
# be multiplied by that trip length before the two can be compared.
extract_dcdh <- function(m) {
  res <- m$results
  # Keep N (observations) and Switchers (treated units) per horizon when the
  # package reports them -- they feed the dCDH table diagnostics block.
  grab <- function(x, sgn = 1L) {
    d <- as.data.table(x)
    out <- d[, .(rel = sgn * seq_len(.N), est = Estimate, se = SE,
                 ci_lo = `LB CI`, ci_hi = `UB CI`)]
    if ("N" %in% names(d))         out[, n_obs := d$N]
    if ("Switchers" %in% names(d)) out[, n_switchers := d$Switchers]
    out
  }
  eff <- grab(res$Effects)
  pl <- if (!is.null(res$Placebos) && res$N_Placebos > 0) grab(res$Placebos, -1L) else NULL
  ref <- data.table(rel = 0L, est = 0, se = 0, ci_lo = 0, ci_hi = 0)
  rbindlist(list(pl, ref, eff), fill = TRUE)[order(rel)]
}

# dCDH pooled (non-negative weights) ----
# de Chaisemartin & D'Haultfoeuille (2024) DIDmultiplegtDYN as a robustness
# cross-check to the linear stacked-feols dose-response. The linear feols beta
# is an ACR only if the dose-response is ~linear; if the marginal effect bends
# with dose size, beta weights heterogeneous dose-specific responses with
# possibly-perverse weights. did_multiplegt_dyn re-estimates the dose-specific
# pieces and aggregates them with NON-NEGATIVE weights, so a matching event-
# study path validates the linear aggregation.
#
# SAME SAMPLE as the headline: never-changers + the three Inc cells; Dec and
# flip-floppers/Uncertain already dropped; same muni fiscal controls.
# Treatment = resident rate LEVEL (continuous = 1), group = muni, time = year,
# cluster = muni.
#
# Window: effects = 5, placebo = 5, matching the by-cell dCDH and the
# stacked exhibits.
#
# SEs: analytical by default (NBOOT = 0). The package notes analytical SEs can
# be LIBERAL with the continuous option; set NBOOT >= 50 for the recommended
# (but slow, ~3 min/outcome) bootstrap. Point estimates are identical either way.
#
# Output: data/results/dcdh_robustness.csv ; figures/dcdh_vs_feols_es.png
if (RUN_DCDH_POOLED) local({
  suppressMessages(library(DIDmultiplegtDYN))
  # NB: did_multiplegt_dyn's effects/placebo must be DOUBLE, not integer (3L
  # throws "Positive integer required" -- a package quirk).
  # +-5 to match the by-cell dCDH and the stacked exhibits (aligned 2026-07-06;
  # the old +-3/+-2 cap predated the property-controls spec and was stale).
  K_post <- 5; K_pre <- 5
  # as.numeric, NOT as.integer: `bootstrap` has the same package quirk as
  # `effects`/`placebo` above and rejects an integer with "Syntax error in
  # bootstrap option. Positive integer required."
  NBOOT  <- as.numeric(Sys.getenv("DCDH_BOOTSTRAP", "0"))   # set DCDH_BOOTSTRAP=50 to bootstrap

  panel <- panel[as.character(cell_muni) %in% c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind") &
                   !is.na(resident_eit)]
  panel[, muni_int := as.integer(factor(muni))]

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  all <- list()
  for (o in outcomes) {
    # only the columns the estimator needs (an extra character id column makes
    # did_multiplegt_dyn mis-read the effects option)
    cols <- c("muni_int", "year", "resident_eit", o, ctrls)
    d <- panel[, ..cols][complete.cases(panel[, ..cols])]; if (!nrow(d)) next
    setnames(d, o, "Y")
    message(sprintf("[%s] N=%s munis=%s -- fitting dCDH", o,
                    format(nrow(d), big.mark = ","), uniqueN(d$muni_int)))
    args <- list(df = as.data.frame(d), outcome = "Y", group = "muni_int", time = "year",
                 treatment = "resident_eit", effects = K_post, placebo = K_pre,
                 continuous = 1, controls = ctrls, cluster = "muni_int", graph_off = TRUE)
    if (NBOOT > 0) args$bootstrap <- NBOOT
    m <- tryCatch(do.call(did_multiplegt_dyn, args),
                  error = function(err) { message("ERROR: ", conditionMessage(err)); NULL })
    if (is.null(m)) next
    co <- extract_dcdh(m); co[, outcome := o]
    pj <- tryCatch(m$results$p_jointplacebo, error = function(e) NA_real_)
    co[, joint_placebo_p := if (is.null(pj)) NA_real_ else pj]
    co[, se_type := if (NBOOT > 0) sprintf("bootstrap(%d)", NBOOT) else "analytical"]
    all[[o]] <- co
    message(sprintf("  done; effect@+%d = %+.3f", max(co$rel), co[rel == max(rel), est]))
  }
  res <- rbindlist(all, fill = TRUE)
  fwrite(res, file.path(RESULTS_DIR, "dcdh_robustness.csv"))

  lab <- c(log_live_and_work = "Log(work in own muni)", log_in_commuters = "Log(in-commuters)",
           log_total_jobs = "Log(total jobs)", share_live_in = "Share live in muni")
  res[, ol := lab[outcome]]
  g <- ggplot(res, aes(rel, est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166ac") +
    geom_line(color = "#2166ac") + geom_point(color = "#2166ac", size = 1) +
    facet_wrap(~ ol, scales = "free_y") +
    labs(title = "dCDH (de Chaisemartin-D'Haultfoeuille) event study -- robustness to linear feols",
         subtitle = "continuous treatment (resident rate level), non-negative weights; same headline sample",
         x = "periods since rate change", y = "estimated effect (per unit of rate)") +
    theme_bw(base_size = 10)
  ggsave(file.path(OUT_DIR, "dcdh_vs_feols_es.png"), g, width = 9, height = 6, dpi = 300)
  message("Saved data/results/dcdh_robustness.csv + figures/dcdh_vs_feols_es.png")
  print(res[, .(outcome, rel, est = round(est, 3), se = round(se, 3))][order(outcome, rel)])
})

# dCDH by cell ----
# DIDmultiplegtDYN event study run SEPARATELY for the three groups that the
# headline reports -- the POOLED Increase aggregate, the Inc-bind cell, and the
# Inc-becomes cell -- across all four outcomes. The pooled run reproduces the
# section above; the two cell runs give the binding decomposition its OWN
# non-negative-weight check, so the "single ACRT curve" claim does not rest on
# the aggregate dCDH alone.
#
# Each group restricts the headline cousub-year panel to {its treated cell + NC}.
# Treatment = resident rate LEVEL (continuous = 1); group = muni; time = year;
# muni fiscal-share controls; cluster = muni. (Like the aggregate dCDH, this nets
# out the muni controls but NOT the SD rate via the cell structure -- it is a
# treatment-level cross-check, not the SD-controlled headline spec.)
#
# Window: effects = 5, placebo = 5. SEs analytical (NBOOT=0; set DCDH_BOOTSTRAP=50).
# Output: data/results/dcdh_bycell.csv ; figures/dcdh_bycell_es.png
if (RUN_DCDH_BYCELL) local({
  suppressMessages(library(DIDmultiplegtDYN))
  K_post <- 5; K_pre <- 5
  NBOOT  <- as.numeric(Sys.getenv("DCDH_BOOTSTRAP", "0"))  # numeric: see note in the pooled block

  panel <- panel[as.character(cell_muni) %in% c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind") &
                   !is.na(resident_eit)]

  groups <- list(
    pooled     = c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"),
    incbind    = c("NC", "Inc-bind"),
    incbecomes = c("NC", "Inc-becomes"))
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")

  all <- list()
  # Lay explanation: run the alternative estimator three times on three
  # slices of towns -- everyone pooled, then only towns whose bills actually
  # moved (Inc-bind), then only towns newly pulled above the workplace floor
  # (Inc-becomes). Each slice keeps the never-changers as its yardstick, so
  # the bind/becomes split gets its own independent second opinion.
  for (gn in names(groups)) {
    pg <- panel[as.character(cell_muni) %in% groups[[gn]]]
    pg[, muni_int := as.integer(factor(muni))]
    for (o in outcomes) {
      cols <- c("muni_int", "year", "resident_eit", o, ctrls)
      d <- pg[, ..cols][complete.cases(pg[, ..cols])]; if (!nrow(d)) next
      setnames(d, o, "Y")
      message(sprintf("[%s | %s] N=%s munis=%s -- fitting dCDH", gn, o,
                      format(nrow(d), big.mark = ","), uniqueN(d$muni_int)))
      args <- list(df = as.data.frame(d), outcome = "Y", group = "muni_int", time = "year",
                   treatment = "resident_eit", effects = K_post, placebo = K_pre,
                   continuous = 1, controls = ctrls, cluster = "muni_int", graph_off = TRUE)
      if (NBOOT > 0) args$bootstrap <- NBOOT
      m <- tryCatch(do.call(did_multiplegt_dyn, args),
                    error = function(err) { message("ERROR: ", conditionMessage(err)); NULL })
      if (is.null(m)) next
      co <- extract_dcdh(m); co[, `:=`(group = gn, outcome = o)]
      pj <- tryCatch(m$results$p_jointplacebo, error = function(e) NA_real_)
      co[, joint_placebo_p := if (is.null(pj)) NA_real_ else pj]
      # The package's "ATE" is its Average Total Effect PER UNIT OF TREATMENT
      # (dose-normalized) -- the dCDH analog of the ACR, directly comparable to
      # the feols per-pp coefficient. It is a 1-row matrix, so $Estimate fails;
      # convert first (the old $-access returned NULL -> NA silently).
      # Lay explanation: unlike the year-by-year effects above (which are in
      # "whole trip" units), this ATE is already back in price-per-mile
      # terms -- effect per unit of tax. It is the ONE number from this
      # estimator that can sit directly next to the headline per-pp estimate
      # with no rescaling.
      co[, ate := tryCatch({
        a <- m$results$ATE
        if (is.null(a)) NA_real_ else as.numeric(as.data.table(a)$Estimate[1])
      }, error = function(e) NA_real_)]
      co[, se_type := if (NBOOT > 0) sprintf("bootstrap(%d)", NBOOT) else "analytical"]
      all[[paste(gn, o)]] <- co
      message(sprintf("  done; effect@+%d = %+.3f", max(co$rel), co[rel == max(rel), est]))
    }
  }
  res <- rbindlist(all, fill = TRUE)
  fwrite(res, file.path(RESULTS_DIR, "dcdh_bycell.csv"))

  lab <- c(log_live_and_work = "Log(work in own muni)", log_in_commuters = "Log(in-commuters)",
           log_total_jobs = "Log(total jobs)", share_live_in = "Share live in muni")
  glab <- c(pooled = "Pooled Increase", incbind = "Inc-bind", incbecomes = "Inc-becomes")
  res[, `:=`(ol = lab[outcome], gl = factor(glab[group], levels = glab))]
  g <- ggplot(res, aes(rel, est, color = gl, fill = gl)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~ ol, scales = "free_y") +
    scale_color_manual(values = c("Pooled Increase" = "#4d4d4d", "Inc-bind" = "#b2182b",
                                  "Inc-becomes" = "#2166ac"), name = NULL) +
    scale_fill_manual(values = c("Pooled Increase" = "#4d4d4d", "Inc-bind" = "#b2182b",
                                 "Inc-becomes" = "#2166ac"), name = NULL) +
    labs(title = "dCDH event study by group -- non-negative-weight check on the decomposition",
         subtitle = "continuous treatment (resident rate level); pooled vs Inc-bind vs Inc-becomes; same headline sample",
         x = "periods since rate change", y = "estimated effect (per unit of rate)") +
    theme_bw(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "dcdh_bycell_es.png"), g, width = 9, height = 6, dpi = 300)
  message("Saved data/results/dcdh_bycell.csv + figures/dcdh_bycell_es.png")
  print(res[, .(group, outcome, rel, est = round(est, 3), se = round(se, 3))][order(group, outcome, rel)])
})

# Act 32 split ----
# Pre-/post-Act-32 split of the headline cell x dose response. COUSUB-ONLY
# (repo-cleanup, 2026-05: the SD-unit and block-unit splits were trimmed when
# those estimators were retired; the kept papers report only the cousub split).
#
# Approach: split treated munis by whether their first event year falls before
# 2012 or in/after 2012, run two separate stacked-continuous regressions on
# each subset, and compare the Inc-cell coefficients.
#
# Output: data/results/act32_split_cousub.csv (+ _diag.csv)
if (RUN_ACT32) local({
  # Lay explanation: before 2012, paying the local income tax was partly an
  # honor system -- many workers simply never filed. Act 32 (effective 2012)
  # made every employer withhold the tax from paychecks, so the bill became
  # unavoidable. Splitting events at 2012 tests a sharp prediction: if
  # people respond to taxes they actually pay, effects should be stronger
  # after withholding made the tax impossible to dodge.
  ACT32_YEAR <- 2012L

  run_act32_split <- function(stk, c_col, cell_col, dose_col,
                               outcomes, ctrls, fe_str, cluster_var, level_tag) {
    ctrls_str <- paste(ctrls, collapse = " + ")
    results <- list()
    diagnostics <- list()
    fulls <- list()
    cohort <- stk[, get(c_col)]
    # Cohort counts per cell x period: documents WHY a cell can be absent from
    # a period's table (e.g. no Inc-nobind first events before 2012).
    cc <- unique(stk[!is.na(get(c_col)) & get(cell_col) != "NC",
                     .(muni, cell = get(cell_col), co = get(c_col))])
    cc[, period := fifelse(co < ACT32_YEAR, "pre", "post")]
    # Lay explanation: this printout is a head count -- how many towns of
    # each binding type first changed rates before vs after 2012. If a cell
    # shows zero in one column, that cell simply has no towns to estimate in
    # that period, which is why a row can vanish from one side of the split.
    print(dcast(cc[, .N, by = .(cell, period)], cell ~ period, value.var = "N", fill = 0))
    for (label in c("pre", "post")) {
      if (label == "pre") {
        sub <- stk[is.na(get(c_col)) | get(c_col) < ACT32_YEAR]
      } else {
        sub <- stk[is.na(get(c_col)) | get(c_col) >= ACT32_YEAR]
      }
      sub[, (cell_col) := factor(get(cell_col),
                                  levels = c("NC", "Inc-bind", "Inc-becomes",
                                              "Inc-nobind", "Dec-bind",
                                              "Dec-becomes-non", "Dec-nobind"))]
      fe_cols <- trimws(unlist(strsplit(fe_str, "[+^]")))
      fe_cols <- fe_cols[fe_cols != ""]
      for (o in outcomes) {
        cols <- intersect(c(o, dose_col, cell_col, ctrls,
                             all.vars(cluster_var), fe_cols),
                           names(sub))
        d <- sub[, ..cols]
        d <- d[complete.cases(d)]
        if (nrow(d) == 0) next
        f <- as.formula(paste0(o,
             " ~ i(", cell_col, ", ", dose_col, ", ref='NC') + ",
             ctrls_str, " | ", fe_str))
        m <- tryCatch(feols(f, data = d, cluster = cluster_var),
                       error = function(e) {
                         message(sprintf("  %s %s err: %s", label, o, conditionMessage(e))); NULL })
        if (is.null(m)) next
        ct <- as.data.table(m$coeftable, keep.rownames = "term")
        setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"),
                 c("est", "se", "p"), skip_absent = TRUE)
        ic <- ct[grepl(paste0(cell_col, "::"), term)]
        ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
        ic[, period := label]
        ic[, outcome := o]
        ic[, n := nobs(m)]
        results[[length(results) + 1L]] <- ic[, .(period, outcome, cell,
                                                  est, se, p, n)]
        fulls[[length(fulls) + 1L]] <- copy(ct)[, `:=`(period = label,
          outcome = o, n = nobs(m),
          r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
          wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
          dep_mean = round(mean(m$residuals + m$fitted.values, na.rm = TRUE), 4))]
        dep_vec <- m$residuals + m$fitted.values
        diagnostics[[length(diagnostics) + 1L]] <- data.table(
          period   = label, outcome = o,
          n        = nobs(m),
          r2       = round(fitstat(m, "r2",  simplify = TRUE), 4),
          wr2      = round(fitstat(m, "wr2", simplify = TRUE), 4),
          dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
          dep_sd   = round(sd(dep_vec,   na.rm = TRUE), 4))
      }
    }
    out <- rbindlist(results, fill = TRUE)
    fwrite(out, file.path(RESULTS_DIR, paste0("act32_split_", level_tag, ".csv")))
    fwrite(rbindlist(fulls, fill = TRUE),
           file.path(RESULTS_DIR, paste0("act32_full_", level_tag, ".csv")))
    diag_dt <- rbindlist(diagnostics, fill = TRUE)
    fwrite(diag_dt, file.path(RESULTS_DIR, paste0("act32_split_", level_tag, "_diag.csv")))
    message(sprintf("[%s] Act 32 split saved (%d coef rows, %d diag rows).",
                    level_tag, nrow(out), nrow(diag_dt)))
    invisible(out)
  }

  message("Cousub-level Act 32 split")
  run_act32_split(stk_muni, "c_muni", "treat_cell", "dose_stack",
                  c("log_live_and_work", "log_in_commuters",
                    "log_total_jobs", "share_live_in", "share_work_outside"),
                  ctrls, "stack_muni + stack_year", ~muni, "cousub")
  message("Done. Output in data/results/act32_split_cousub.csv")
})

# Act 47 exclusion ----
# Robustness check: drop Act 47 distressed municipalities from the treated
# cohort and re-estimate the headline binding-state decomposition. Identifies
# Act 47 munis in the Inc-bind cell by name matching the standard PA DCED list
# of Act 47 designations.
#
# Outputs: data/results/act47_robustness_cousub.csv ;
#          tables/act47_robustness.tex (concise summary table)
if (RUN_ACT47) local({
  # Act 47 distressed-municipality list (DCED designations 1987-2019).
  # Source: PA DCED Act 47 active and rescinded designation list.
  # Names normalized to match DCED's municipality names.
  # Lay explanation: Act 47 cities are Pennsylvania's officially "financially
  # distressed" places -- and distressed cities are exactly the ones that
  # raise income taxes hardest while also losing residents for reasons that
  # have nothing to do with the tax. The check drops any treated town whose
  # name matches this list and re-runs the headline: if the result survives,
  # it is not just a story about a couple of collapsing cities.
  # Act 47 distressed municipalities, transcribed from DCED's own register
  # (https://dced.pa.gov/local-government/act-47-financial-distress/,
  # retrieved 2026-07-30): three active determinations plus every rescission
  # DCED lists. Cross-checked against the Wikipedia table for the
  # Financially Distressed Municipalities Act.
  #
  # The previous hardcoded list was wrong in BOTH directions. It carried ten
  # municipalities that were never designated (Carbondale, Jeannette, Sharon,
  # Turtle Creek, Wilkes-Barre, Yeadon, Coatesville, DuBois, Ellport,
  # Glassport) and omitted six that were (Newville, Shenandoah, Ambridge,
  # North Braddock, Westfall Twp, Colwyn). `designated` is the determination
  # year, which matters because the exclusion is only meaningful for a
  # municipality that was ALREADY in recovery when it changed its rate.
  act47 <- data.table::data.table(
    name = toupper(c(
      "NEWVILLE", "CHESTER", "HARRISBURG",
      "SHENANDOAH", "AMBRIDGE", "WILKINSBURG", "EAST PITTSBURGH",
      "NORTH BRADDOCK", "HOMESTEAD", "MILLBOURNE", "WESTFALL",
      "WEST HAZLETON", "NANTICOKE", "CLAIRTON", "PLYMOUTH", "ALTOONA",
      "PITTSBURGH", "FARRELL", "SCRANTON", "COLWYN", "READING",
      "FRANKLIN", "MAHANOY CITY", "JOHNSTOWN", "HAZLETON", "RANKIN",
      "BRADDOCK", "DUQUESNE", "GREENVILLE", "ALIQUIPPA", "NEW CASTLE",
      "SHAMOKIN")),
    designated = c(
      2014L, 1995L, 2010L,
      1988L, 1990L, 1988L, 1992L,
      1995L, 1993L, 1993L, 2009L,
      2003L, 2006L, 1988L, 2004L, 2012L,
      2003L, 1987L, 1992L, 2015L, 2009L,
      1988L, 2016L, 1992L, 2017L, 1989L,
      1988L, 1991L, 2002L, 1987L, 2007L,
      2014L))
  act47_names <- act47$name

  # 1. Pull municipality names onto the shared headline panel
  rate <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  muni_names <- unique(rate[!is.na(cousub_fips),
                              .(muni = cousub_fips,
                                name = toupper(municipality))])
  panel <- merge(panel, muni_names, by = "muni", all.x = TRUE)

  # Flag Act 47 munis in the Inc-bind cell. Names are normalized on both
  # sides (punctuation to space, whitespace collapsed) because DCED's rate
  # register and the distressed register do not agree on hyphenation.
  norm_name <- function(x) {
    x <- toupper(x)
    x <- gsub("[^A-Z0-9 ]", " ", x)   # hyphens, periods, apostrophes -> space
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  act47[, name_norm := norm_name(name)]
  treated_incbind <- unique(panel[cell_muni == "Inc-bind", .(muni, name, c_muni)])
  treated_incbind[, name_norm := norm_name(name)]
  # Carry the determination year through, so "distressed" can be evaluated
  # AT the rate-change year rather than ever-in-the-panel.
  treated_incbind[, act47_year := vapply(name_norm, function(nm) {
    hit <- act47$designated[vapply(act47$name_norm,
             function(a47) grepl(a47, nm, fixed = TRUE), logical(1))]
    if (length(hit)) min(hit) else NA_integer_
  }, integer(1))]
  treated_incbind[, is_act47 := !is.na(act47_year)]
  treated_incbind[, in_recovery_at_event := !is.na(act47_year) & c_muni >= act47_year]

  message(sprintf("[act47] Inc-bind munis: %d total", nrow(treated_incbind)))
  message(sprintf("[act47] ever designated: %d | already in recovery at their rate change: %d",
                  sum(treated_incbind$is_act47),
                  sum(treated_incbind$in_recovery_at_event)))
  if (any(treated_incbind$is_act47))
    print(treated_incbind[is_act47 == TRUE,
          .(name, cohort = c_muni, designated = act47_year, in_recovery_at_event)])
  # The exclusion is a blunt "ever designated" one, which is the conservative
  # choice. Note in the paper that no Inc-bind municipality was actually in
  # recovery when it raised its rate, so the concern this check addresses
  # (rate increases mandated by a state-supervised recovery plan) does not
  # arise in this sample.

  # Guard: report any distressed-list entry that matches nothing anywhere in
  # the panel. A name on the list that never matches is either a municipality
  # outside the estimation sample or a spelling that has drifted from DCED's.
  all_norm <- norm_name(unique(panel$name))
  unmatched <- act47$name[!vapply(act47$name_norm, function(a47)
    any(grepl(a47, all_norm, fixed = TRUE)), logical(1))]
  if (length(unmatched))
    message(sprintf("[act47] WARNING: %d list entries match no municipality in the panel: %s",
                    length(unmatched), paste(unmatched, collapse = ", ")))

  # 2. Re-estimate headline cousub regression excluding Act 47 munis
  act47_munis_in_panel <- treated_incbind[is_act47 == TRUE, muni]

  panel_no_a47 <- panel[!(cell_muni == "Inc-bind" & muni %in% act47_munis_in_panel)]
  # Treat the Act 47 Inc-bind munis as if they were dropped from the regression
  # by setting their cell to NA (so they're not in any treated cohort).
  panel[, cell_muni_a47drop := cell_muni]
  panel[muni %in% act47_munis_in_panel & cell_muni == "Inc-bind",
         cell_muni_a47drop := NA_character_]

  # Stack on muni cohorts (mirror headline)
  K_stack <- 5L
  build_stack <- function(p, cell_col, K = K_stack) {
    treated <- unique(p[get(cell_col) != "NC" & !is.na(get(cell_col)),
                         .(muni, cohort = c_muni, cell = get(cell_col))])
    nc_munis <- unique(p[get(cell_col) == "NC", muni])
    cohort_years <- sort(unique(treated$cohort))
    rbindlist(lapply(cohort_years, function(cy) {
      win <- (cy - K):(cy + K)
      t_m <- unique(treated[cohort == cy, .(muni)])
      if (nrow(t_m) == 0) return(NULL)
      d_t <- p[muni %in% t_m$muni & year %in% win]
      d_t[, treat_cell := get(cell_col)]
      d_c <- p[muni %in% nc_munis & year %in% win]
      d_c[, treat_cell := "NC"]
      # Carry the CURRENT headline control columns (env$ctrls) through the stack
      # dynamically, so control-set changes in the headline propagate here
      # without editing this script (the old hard-coded share list silently
      # kept Act 47 on the retired controls).
      rbind(d_t, d_c, fill = TRUE)[, c(list(muni = muni, year = year,
                                        treat_cell = treat_cell,
                                        stack = cy, rel = year - cy,
                                        dose_muni = dose_muni, dose_sd = dose_sd,
                                        resident_eit = resident_eit,
                                        dose_nr = dose_nr,
                                        cell_sd = cell_sd, cell_nr = cell_nr,
                                        log_live_and_work = log_live_and_work,
                                        log_in_commuters = log_in_commuters,
                                        log_total_jobs = log_total_jobs,
                                        share_live_in = share_live_in),
                                        mget(muni_ctrls))]
    }), fill = TRUE)
  }

  # Inherit the headline control set (must be defined BEFORE build_stack runs --
  # the stack selection above references muni_ctrls via closure).
  muni_ctrls <- ctrls
  ctrls_str <- paste(muni_ctrls, collapse = " + ")

  message("Building stacks (full headline + Act 47 excluded)...")
  stk_full <- build_stack(panel, "cell_muni")
  stk_a47drop <- build_stack(panel, "cell_muni_a47drop")
  for (s in list(stk_full, stk_a47drop)) {
    s[, stack_muni := paste0(stack, "_", muni)]
    s[, stack_year := paste0(stack, "_", year)]
    s[, treat_cell := factor(treat_cell,
         levels = c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                    "Dec-bind", "Dec-becomes-non", "Dec-nobind"))]
    s[is.na(dose_sd), dose_sd := 0]
    s[is.na(dose_nr), dose_nr := 0]
    # Take the regressor from the SAME source column build_stack() in
    # helpers/helpers.R uses, which is the resident rate level. Building it from
    # dose_muni instead admitted 1,972 stacked rows where resident_eit is
    # missing but dose_muni is not, roughly 1,350 of them surviving listwise
    # deletion. The cells with real data barely moved, but Inc-nobind has 14
    # municipalities and SEs near 0.3, so its in-commuter coefficient read
    # +0.357 against the headline's +0.145 (fixed 2026-08-07).
    s[, dose_stack := resident_eit]
  }

  # Returns TWO tables per model: the Inc-bind summary row (concise CSV, as
  # before) and the FULL coeftable + diagnostics, so 4_output/2_tables.R can build a
  # main-results-format Act 47 table (all cells + controls + diagnostics).
  run_one <- function(stk, outcome) {
    f <- as.formula(paste0(outcome,
         " ~ i(treat_cell, dose_stack, ref = 'NC') ",
         "+ i(cell_sd, dose_sd, ref = 'NC') + ",
         ctrls_str, " | stack_muni + stack_year"))
    dd <- stk[, c("muni", "year", "stack_muni", "stack_year", "treat_cell",
                  "dose_stack", "cell_sd", "dose_sd",
                  outcome, muni_ctrls), with = FALSE]
    dd <- dd[complete.cases(dd)]
    m <- feols(f, data = dd, cluster = ~muni)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"),
             c("est", "se", "p"), skip_absent = TRUE)
    full <- copy(ct)[, `:=`(outcome = outcome, n = nobs(m),
      r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
      wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean = round(mean(m$residuals + m$fitted.values, na.rm = TRUE), 4))]
    ic <- ct[grepl("^treat_cell::Inc-bind:", term)]
    list(summary = data.table(outcome = outcome,
                              est = round(ic$est, 4),
                              se = round(ic$se, 4),
                              p = round(ic$p, 4),
                              n = nobs(m)),
         full = full)
  }

  outcomes <- c("log_live_and_work", "log_in_commuters",
                "log_total_jobs", "share_live_in")
  n_inc_bind_total <- nrow(treated_incbind)
  message(sprintf("Cousub headline (all %d Inc-bind munis)", n_inc_bind_total))
  runs_full <- lapply(outcomes, function(o) run_one(stk_full, o))
  res_full <- rbindlist(lapply(runs_full, `[[`, "summary"))
  res_full[, spec := sprintf("All %d Inc-bind munis", n_inc_bind_total)]
  print(res_full)

  message("Cousub headline (excluding Act 47 Inc-bind munis)")
  runs_drop <- lapply(outcomes, function(o) run_one(stk_a47drop, o))
  res_a47drop <- rbindlist(lapply(runs_drop, `[[`, "summary"))
  res_a47drop[, spec := sprintf("Drop %d Act 47 Inc-bind", length(act47_munis_in_panel))]
  print(res_a47drop)

  results <- rbind(res_full, res_a47drop)
  fwrite(results, file.path(RESULTS_DIR, "act47_robustness_cousub.csv"))
  # full coeftable on the Act-47-dropped sample, main-results format input
  act47_full <- rbindlist(lapply(runs_drop, `[[`, "full"))
  fwrite(act47_full, file.path(RESULTS_DIR, "act47_full_table.csv"))
  message("Saved data/results/act47_robustness_cousub.csv + act47_full_table.csv")

  # 3. LaTeX summary table for the paper
  fmt_coef <- function(est, se, p) {
    stars <- if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
    sprintf("$%+.2f^{%s}$ (%.2f)", est, stars, se)
  }

  tex <- sprintf("\\begin{table}[H]
  \\centering
  \\caption{Act~47 robustness: dropping distressed Inc-bind municipalities from the cousub headline}
  \\label{tab:act47_robustness}
  \\small
  \\begin{tabular}{lcc}
    \\toprule
    Outcome & All %d Inc-bind & Drop %d Act~47 \\\\
    \\midrule
", n_inc_bind_total, length(act47_munis_in_panel))
  pretty_outcome <- function(o) switch(o,
    log_live_and_work   = "log live-and-work",
    log_in_commuters    = "log in-commuters",
    log_total_jobs      = "log total jobs",
    share_live_in       = "live-in-muni share",
    gsub("_", " ", o))
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    r_full <- res_full[outcome == o]
    r_drop <- res_a47drop[outcome == o]
    tex <- paste0(tex,
      sprintf("    %s & %s & %s \\\\\n",
              pretty_outcome(o),
              fmt_coef(r_full$est, r_full$se, r_full$p),
              fmt_coef(r_drop$est, r_drop$se, r_drop$p)))
  }
  tex <- paste0(tex,
    "    \\bottomrule\n  \\end{tabular}\n",
    "  \\vspace{0.3em}\n",
    "  \\begin{minipage}{0.85\\textwidth}\n",
    sprintf("    \\footnotesize \\textit{Notes:} Muni-level Inc-bind coefficient on each residence-side outcome, with same FE / cluster / controls as the headline. Column 1 is the headline (all %d Inc-bind municipalities). Column 2 drops %d Act 47 distressed-designation municipalities from the Inc-bind cohort; the remaining %d Inc-bind municipalities are mostly Home Rule and Boroughs. Cluster-robust SEs in parentheses. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.\n",
            n_inc_bind_total, length(act47_munis_in_panel), n_inc_bind_total - length(act47_munis_in_panel)),
    "  \\end{minipage}\n",
    "\\end{table}\n")

  writeLines(tex, file.path(TABLE_DIR, "act47_robustness.tex"))
  message("Saved tables/act47_robustness.tex")
})

# Done ----
# Pre-trend battery ----
# (from q3_unified_pretrend_battery.R) Multiple pre-trend tests on the
# realized-dose ES, per muni-side cell x outcome. The single joint Wald
# over-rejects on the thin cohorts (14-24 treated clusters), so we report a
# battery of four:
#   1. joint Wald on leads (asymptotic)             -- the standard test
#   2. individual leads: # significant at 5%, min p
#   3. linear pre-trend slope (asymptotic, 1 df)    -- more stable than joint
#        violation (relative to the max pre-period deviation) is needed before
#        the post-effect CI includes zero; bigger = more robust.
# Per-model tests now come from pretrend_bucket() in
# helpers/helpers.R (extracted verbatim). robust_clean = passes the
# few-clusters-robust frequentist tests (individual leads + linear slope);
# the joint Wald is reported but does not drive the
# verdict.
# (A wild-cluster bootstrap was attempted but fwildclusterboot is incompatible
#  small-sample-robustness purpose and is the standard tool.)
if (RUN_PRETREND_BATTERY) local({
  K <- 5L; set.seed(42)
  # Grids must resolve below 0.5: the breakdown on this design sits near 0.3,
  # so the old c(0.5, 1, 1.5, 2) grid returned its own minimum for every row.
  cs <- ctrls_str
  panel_bat <- copy(panel)

  mu <- unique(panel_bat[, .(muni, year, resident_eit, c_muni)])
  setorder(mu, muni, year)
  rd <- mu[!is.na(c_muni), {
    pre <- resident_eit[year == c_muni - 1L]
    ymx <- min(c_muni + K, max(year, na.rm = TRUE)); end <- resident_eit[year == ymx]
    list(rdose = (if (length(end) == 1) end else NA) - (if (length(pre) == 1) pre else NA))
  }, by = muni]
  rd[is.na(rdose), rdose := 0]
  rd <- rbind(rd, data.table(muni = unique(panel_bat[cell_muni == "NC", muni]), rdose = 0))
  panel_bat <- merge(panel_bat, rd, by = "muni", all.x = TRUE)
  panel_bat[is.na(rdose), rdose := 0]

  build_stack <- function(p) {
    tr <- unique(p[cell_muni != "NC", .(muni, co = c_muni)])
    nc <- unique(p[cell_muni == "NC", muni])
    rbindlist(lapply(sort(unique(tr$co)), function(cy) {
      w <- (cy - K):(cy + K); tm <- tr[co == cy]; if (!nrow(tm)) return(NULL)
      dt <- p[muni %in% tm$muni & year %in% w]; dt[, tc := as.character(cell_muni)]
      dc <- p[muni %in% nc & year %in% w];      dc[, tc := "NC"]
      d <- rbind(dt, dc, fill = TRUE); d[, rel := as.integer(year - cy)]; d[, stk := cy]; d
    }), fill = TRUE)
  }
  stk <- build_stack(panel_bat)
  stk[, smuni := paste0(stk, "_", muni)]; stk[, syear := paste0(stk, "_", year)]
  stk[, cell_sd := factor(cell_sd, levels = cell_levels)]
  stk[, cell_nr := factor(cell_nr, levels = cell_levels)]
  stk[is.na(dose_sd), dose_sd := 0]; stk[is.na(dose_nr), dose_nr := 0]
  stk[, dose_rel := rdose * rel]

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  # Battery groups: the three decomposition cells PLUS the pooled-Increase
  # aggregate (all Inc cells together), so the pooled model gets the same
  # four-test documentation. Pooled is expected to FAIL the leads -- the
  # Inc-becomes/Inc-nobind selection is the reason the decomposition exists.
  # Lay explanation: the four groups tested are the three flavors of increase
  # plus all of them mixed together. The mixed group SHOULD flunk the
  # pre-trend test: it blends towns whose bills actually moved with towns
  # already drifting for other reasons. If Inc-bind alone passes while the
  # blend fails, that is the whole argument for splitting into cells.
  battery_groups <- list(
    "Inc-bind"    = "Inc-bind",
    "Inc-becomes" = "Inc-becomes",
    "Inc-nobind"  = "Inc-nobind",
    "Pooled-Inc"  = c("Inc-bind", "Inc-becomes", "Inc-nobind"))
  rows <- list()
  for (o in outcomes) for (cl in names(battery_groups)) {
    d <- stk[tc %in% c("NC", battery_groups[[cl]])]
    cols <- c("muni", "rel", "smuni", "syear", "rdose",
              "cell_sd", "dose_sd", o, ctrls)
    dd <- d[, ..cols][complete.cases(d[, ..cols])]; if (!nrow(dd)) next
    f <- as.formula(paste0(o, " ~ i(rel, rdose, ref=-1) + i(cell_sd, dose_sd, ref='NC') + ",
                           cs, " | smuni + syear"))
    m <- tryCatch(feols(f, dd, cluster = ~muni), error = function(e) NULL)
    if (is.null(m)) next
    # linear pre-trend slope on the pre-event window
    # Lay explanation: dose_rel = eventual dose x years-until-change, so one
    # coefficient asks a single question -- were bigger-hike towns drifting
    # along a straight line before the change? Estimating one slope instead
    # of five separate lead coefficients spends far less data, which keeps
    # the test stable when only 14-24 treated towns are in the cell.
    dp <- d[rel < 0]
    cols2 <- c("muni", "dose_rel", "smuni", "syear",
               "cell_sd", "dose_sd", o, ctrls)
    dp <- dp[, ..cols2][complete.cases(d[rel < 0, ..cols2])]
    ml <- tryCatch(feols(as.formula(paste0(o, " ~ dose_rel + i(cell_sd,dose_sd,ref='NC') + ",
                    cs, " | smuni + syear")),
                    dp, cluster = ~muni), error = function(e) NULL)
    rows[[length(rows) + 1L]] <- cbind(
      data.table(outcome = o, cell = cl, n_treated = uniqueN(dd$muni[dd$rdose != 0])),
      pretrend_bucket(m, lin_model = ml, lin_term = "dose_rel"))
  }
  bat <- rbindlist(rows, fill = TRUE)
  fwrite(bat, file.path(RESULTS_DIR, "pretrend_battery.csv"))
  print(bat)
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
# Assign every treated municipality a PLACEBO event 4 years before its real
# one, keep only data from BEFORE the real event, and re-run the headline
# decomposition on the placebo timing. Nothing real happened at the placebo
# date, so the coefficients should be null; a non-zero "effect" would mean
# treated munis were already drifting before their actual rate change.
# Cohorts with real events before 2008 are dropped (no room for a pre-window;
# the count is reported). Underpowered by construction -- treat as a
# direction check alongside the permutation test.
# Output: data/results/fake_timing_placebo.csv
if (RUN_FAKE_TIMING) local({
  # Several placebo dates (Huntington-Klein: "pick a few different periods and
  # pretend"): a real pre-existing drift shows up consistently across shifts;
  # a one-off fluke does not. The three samples share munis and overlapping
  # windows, so they are consistency checks, not independent tests.
  SHIFTS <- c(3L, 4L, 5L); K <- 5L
  treated_all <- unique(panel[!is.na(c_muni) & as.character(cell_muni) != "NC",
                              .(muni, c_true = c_muni, cell = as.character(cell_muni))])
  n_all <- nrow(treated_all)
  rd <- add_realized_dose(copy(panel), K = K)
  tag <- unique(rd[!is.na(c_muni), .(muni, dtag = realized_dose_muni)])
  nc <- unique(panel[cell_muni == "NC", muni])
  outcomes <- c("log_live_and_work", "log_in_commuters",
                "log_total_jobs", "share_live_in")
  rows <- list()
  for (SHIFT in SHIFTS) {
    treated <- treated_all[c_true - SHIFT - 2L >= 2002L]   # >= 2 placebo pre-years
    message(sprintf("fake-timing shift -%d: %d of %d treated munis eligible",
                    SHIFT, nrow(treated), n_all))
    treated <- merge(treated, tag, by = "muni", all.x = TRUE)
    stk_fake <- rbindlist(lapply(sort(unique(treated$c_true - SHIFT)), function(cy) {
      win <- (cy - K):(cy + K)
      tm <- treated[c_true - SHIFT == cy]
      if (!nrow(tm)) return(NULL)
      dt <- merge(panel[muni %in% tm$muni & year %in% win],
                  tm[, .(muni, c_true, cell_fake = cell, dtag)], by = "muni")
      dt <- dt[year < c_true]                        # pre-real-event data only
      dc <- panel[muni %in% nc & year %in% win]
      dc[, `:=`(cell_fake = "NC", dtag = 0, c_true = NA_integer_)]
      d <- rbind(dt, dc, fill = TRUE)
      d[, stack := cy]; d[, rel := as.integer(year - cy)]
      d
    }), fill = TRUE)
    stk_fake[, stack_muni := paste0(stack, "_", muni)]
    stk_fake[, stack_year := paste0(stack, "_", year)]
    stk_fake[, cell_fake := factor(cell_fake,
        levels = c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"))]
    # placebo dose: the muni's TRUE realized magnitude, switched on at the
    # FAKE event date
    stk_fake[, dose_fake := fifelse(rel >= 0, dtag, 0)]
    stk_fake[is.na(dose_sd), dose_sd := 0]
    for (o in outcomes) {
      cols <- c("muni", "rel", "stack_muni", "stack_year", "cell_fake",
                "dose_fake", "cell_sd", "dose_sd", o, ctrls)
      dd <- stk_fake[, ..cols][complete.cases(stk_fake[, ..cols])]
      if (!nrow(dd)) next
      m <- tryCatch(feols(as.formula(paste0(o,
            " ~ i(cell_fake, dose_fake, ref = 'NC') + i(cell_sd, dose_sd, ref = 'NC') + ", ctrls_str,
            " | stack_muni + stack_year")), dd, cluster = ~muni),
            error = function(e) NULL)
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"),
               c("est", "se", "p"), skip_absent = TRUE)
      ct <- ct[grepl("^cell_fake::", term)]
      ct[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
      ct[, `:=`(outcome = o, shift = SHIFT, n = nobs(m),
                n_treated = uniqueN(dd$muni[dd$dose_fake != 0]))]
      rows[[length(rows) + 1L]] <- ct[, .(shift, outcome, cell, est, se, p, n, n_treated)]
    }
  }
  out <- rbindlist(rows, fill = TRUE)
  fwrite(out, file.path(RESULTS_DIR, "fake_timing_placebo.csv"))
  print(out)
})



# ---------------------------------------------------------------------------
# Pooled specification: the full robustness battery
# ---------------------------------------------------------------------------
# Every check above is run on the binding decomposition. This block mirrors
# each of them on the pooled Inc grouping of Equation (eq:pooled), so the
# benchmark is held to the same standard as the headline estimator. Outputs
# parallel their decomposition counterparts one for one:
#   pooled_act32.csv            <- act32_full_cousub.csv
#   pooled_act47.csv            <- act47_full_table.csv
#   pooled_fake_timing.csv      <- fake_timing_placebo.csv (shifts 3, 4, 5)
#   pooled_pretrend_battery.csv <- pretrend_battery.csv
# The pooled dCDH already exists in dcdh_robustness.csv (RUN_DCDH_POOLED).
if (RUN_POOLED_ROBUSTNESS) local({

  ACT32_YEAR <- 2012L; SHIFTS <- c(3L, 4L, 5L); K <- 5L
  outcomes <- c("log_live_and_work", "log_in_commuters",
                "log_total_jobs", "share_live_in")
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")
  mk_dir <- function(x) fcase(as.character(x) %in% inc_cells, "Inc",
                              as.character(x) %in% dec_cells, "Dec",
                              as.character(x) == "NC", "NC",
                              default = NA_character_)
  stk <- copy(stk_muni)
  stk[, direction_muni := factor(mk_dir(treat_cell), levels = c("NC","Inc","Dec"))]
  stk[, direction_sd   := factor(mk_dir(cell_sd),    levels = c("NC","Inc","Dec"))]
  stk[is.na(dose_sd), dose_sd := 0]

  fit_full <- function(d, o, dose_col = "dose_stack") {
    cols <- intersect(c("muni","direction_muni",dose_col,"direction_sd","dose_sd",
                        "stack_muni","stack_year", o, ctrls), names(d))
    x <- d[, ..cols]; x <- x[complete.cases(x)]
    if (!nrow(x) || uniqueN(x$direction_muni) < 2) return(NULL)
    f <- as.formula(paste0(o, " ~ i(direction_muni, ", dose_col, ", ref='NC')",
                           " + i(direction_sd, dose_sd, ref='NC') + ",
                           paste(ctrls, collapse = " + "),
                           " | stack_muni + stack_year"))
    m <- tryCatch(suppressMessages(feols(f, data = x, cluster = ~muni, notes = FALSE)),
                  error = function(e) NULL)
    if (is.null(m)) return(NULL)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"), skip_absent = TRUE)
    ct[, `:=`(outcome = o, n = nobs(m),
              r2  = round(fitstat(m, "r2",  simplify = TRUE), 4),
              wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
              dep_mean  = round(mean(m$residuals + m$fitted.values, na.rm = TRUE), 4),
              n_treated = uniqueN(x[direction_muni == "Inc", muni]))][]
  }
  gather <- function(d, dose_col = "dose_stack")
    rbindlist(lapply(outcomes, function(o) fit_full(d, o, dose_col)), fill = TRUE)

  ## (1) Act 32 enforcement split
  pre  <- gather(stk[is.na(c_muni) | c_muni <  ACT32_YEAR]); if (nrow(pre))  pre[,  period := "pre"]
  post <- gather(stk[is.na(c_muni) | c_muni >= ACT32_YEAR]); if (nrow(post)) post[, period := "post"]
  fwrite(rbind(pre, post, fill = TRUE), file.path(RESULTS_DIR, "pooled_act32.csv"))

  ## (2) Act 47 distressed-municipality exclusion
  a47_names <- toupper(c("NEWVILLE","CHESTER","HARRISBURG","SHENANDOAH","AMBRIDGE",
    "WILKINSBURG","EAST PITTSBURGH","NORTH BRADDOCK","HOMESTEAD","MILLBOURNE",
    "WESTFALL","WEST HAZLETON","NANTICOKE","CLAIRTON","PLYMOUTH","ALTOONA",
    "PITTSBURGH","FARRELL","SCRANTON","COLWYN","READING","FRANKLIN","MAHANOY CITY",
    "JOHNSTOWN","HAZLETON","RANKIN","BRADDOCK","DUQUESNE","GREENVILLE","ALIQUIPPA",
    "NEW CASTLE","SHAMOKIN"))
  nrm <- function(x) trimws(gsub("\\s+"," ", gsub("[^A-Z0-9 ]"," ", toupper(x))))
  rate_nm <- unique(as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))[
    !is.na(cousub_fips), .(muni = cousub_fips, nm = nrm(municipality))])
  a47n <- nrm(a47_names)
  rate_nm[, is47 := vapply(nm, function(z) any(vapply(a47n, function(a)
    grepl(a, z, fixed = TRUE), logical(1))), logical(1))]
  drop47 <- rate_nm[is47 == TRUE, muni]
  n47 <- uniqueN(stk[muni %in% drop47 & direction_muni == "Inc", muni])
  message(sprintf("[pooled-rob] Act 47 munis in the pooled Inc group: %d", n47))
  aa <- gather(stk); if (nrow(aa)) aa[, spec := "All Inc munis"]
  bb <- gather(stk[!(direction_muni == "Inc" & muni %in% drop47)])
  if (nrow(bb)) bb[, spec := sprintf("Drop %d Act 47 munis", n47)]
  fwrite(rbind(aa, bb, fill = TRUE), file.path(RESULTS_DIR, "pooled_act47.csv"))

  ## (3) Fake-timing placebo at three shifts
  pan <- copy(panel)
  pan[, direction_muni := factor(mk_dir(cell_muni), levels = c("NC","Inc","Dec"))]
  pan[, direction_sd   := factor(mk_dir(cell_sd),   levels = c("NC","Inc","Dec"))]
  nc_munis <- unique(pan[direction_muni == "NC", muni])
  tr_all   <- unique(stk[direction_muni == "Inc" & !is.na(c_muni), .(muni, c_true = c_muni)])
  ft <- list()
  for (SH in SHIFTS) {
    tr <- tr_all[c_true - SH - 2L >= 2002L]
    message(sprintf("[pooled-rob] fake timing -%d: %d of %d treated munis eligible",
                    SH, nrow(tr), nrow(tr_all)))
    fake <- rbindlist(lapply(sort(unique(tr$c_true - SH)), function(cy) {
      win <- (cy - K):(cy + K); tm <- tr[c_true - SH == cy]
      dt <- pan[muni %in% tm$muni & year %in% win & year < (cy + SH)]
      dc <- pan[muni %in% nc_munis & year %in% win]
      if (!nrow(dt)) return(NULL)
      dt[, dir_fake := "Inc"]; dc[, dir_fake := "NC"]
      rbind(dt, dc, fill = TRUE)[, stack := cy][]
    }), fill = TRUE)
    if (!nrow(fake)) next
    fake[, `:=`(rel = year - stack, stack_muni = paste0(stack, "_", muni),
                stack_year = paste0(stack, "_", year))]
    fake[, direction_muni := factor(dir_fake, levels = c("NC","Inc","Dec"))]
    fake[, dose_fake := fifelse(rel >= 0 & dir_fake == "Inc", resident_eit, 0)]
    if (!"dose_sd" %in% names(fake)) fake[, dose_sd := 0]
    fake[is.na(dose_sd), dose_sd := 0]
    g <- gather(fake, "dose_fake")
    if (!is.null(g) && nrow(g)) { g[, shift := SH]; ft[[as.character(SH)]] <- g }
  }
  if (length(ft)) fwrite(rbindlist(ft, fill = TRUE),
                         file.path(RESULTS_DIR, "pooled_fake_timing.csv"))

  ## (4) Pre-trend battery from the pooled realized-dose event study
  esc <- file.path(RESULTS_DIR, "pooled_es_cousub_coefs.csv")
  esa <- file.path(RESULTS_DIR, "pooled_es_cousub_att.csv")
  if (file.exists(esc) && file.exists(esa)) {
    co <- fread(esc)[direction == "Inc" & outcome %in% outcomes]
    at <- fread(esa)[outcome %in% outcomes]
    bat <- rbindlist(lapply(outcomes, function(o) {
      cc <- co[outcome == o & rel <= -2]; ab <- at[outcome == o]
      if (!nrow(cc) || !nrow(ab)) return(NULL)
      ls <- tryCatch(summary(lm(est ~ rel, data = cc))$coefficients["rel", c(1, 4)],
                     error = function(e) c(NA_real_, NA_real_))
      data.table(outcome = o, n_leads = nrow(cc),
                 wald_joint_p = ab$wald_pre_p[1],
                 n_leads_sig5 = sum(cc$p < 0.05, na.rm = TRUE),
                 min_lead_p   = min(cc$p, na.rm = TRUE),
                 lin_slope    = unname(ls[1]), lin_slope_p = unname(ls[2]),
                 robust_clean = ab$wald_pre_p[1] >= 0.10 &
                                sum(cc$p < 0.05, na.rm = TRUE) == 0)
    }), fill = TRUE)
    fwrite(bat, file.path(RESULTS_DIR, "pooled_pretrend_battery.csv"))
    message("[pooled-rob] pre-trend battery:"); print(bat)
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
