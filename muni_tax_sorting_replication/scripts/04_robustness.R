#!/usr/bin/env Rscript
# 04_robustness.R -- Every robustness / alternative-specification model,
# consolidated from the former standalone scripts:
#   q3_unified_dcdh_robustness.R      -> RUN_DCDH_POOLED
#   q3_unified_dcdh_bycell.R          -> RUN_DCDH_BYCELL
#   q3_unified_act32_3level.R         -> RUN_ACT32
#   q3_unified_act47_robustness.R     -> RUN_ACT47
#   q3_unified_curvature_robustness.R -> RUN_CURVATURE
#   pre-trend battery (moved in from stage 03, 2026-07-07)
#
# All sections share ONE headline environment (build_headline_env() in
# _helpers.R): the cousub panel with binding-cell classification and the
# muni-side Cengiz stack -- identical to what q3_unified_full_table.R built.
#
# Outputs -> data/results/*.csv, figures/*.png, tables/act47_robustness.tex.
# Run from package root:  Rscript scripts/04_robustness.R

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})
# DIDmultiplegtDYN is heavy to load; it is attached inside the sections that
# need it (dCDH pooled / dCDH by cell / curvature Part B) rather than here.

.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_helpers.R"))

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
# The two dCDH sections are slow: RUN_DCDH_POOLED ~5 min, RUN_DCDH_BYCELL ~30 min.
RUN_DCDH_POOLED <- TRUE
RUN_DCDH_BYCELL <- TRUE
RUN_ACT32       <- TRUE
RUN_ACT47       <- TRUE
RUN_CURVATURE   <- TRUE
RUN_PRETREND_BATTERY <- TRUE   # 4-test battery per cell (moved from stage 03)
# DROPPED FROM THE PAPER 2026-07-30. Section 6.5 (dose-permutation
# randomization inference) was cut, so this block no longer feeds anything.
# The code is left in place, gated off, rather than deleted.
RUN_DOSE_PERMUTATION <- FALSE  # randomization inference: shuffle dose magnitudes (~15 min)
RUN_FAKE_TIMING      <- TRUE   # placebo cohorts 4 years early, pre-event data only
RUN_POOLED_ROBUSTNESS <- TRUE  # the same battery, run on the pooled Inc specification

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
    # Re-baseline the dose WITHIN each stack window, mirroring build_stack()
    # in _helpers.R. Without this the regressor is the global dose_muni and
    # the table is not the headline specification, which is why this table
    # used to report a different Inc-bind coefficient from Table
    # headline_muni_cell_results on an identical N (fixed 2026-07-30).
    s[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE), by = .(stack, muni)]
    s[!is.finite(base_rate), base_rate := 0]
    s[, dose_stack := dose_muni - base_rate]
  }

  # Returns TWO tables per model: the Inc-bind summary row (concise CSV, as
  # before) and the FULL coeftable + diagnostics, so 06_tables.R can build a
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

# Curvature (ACRT/ATT + dCDH lin vs quad) ----
# Does the per-point (marginal) effect bend with dose size? Two feasible
# parametric answers (the nonparametric contdid/CGS curve is not estimable on
# these thin cohorts -- CGS/contdid is infeasible on this design).
#
# PART A -- feols ACRT(d) CURVE.
#   Pooled-Inc quadratic dose-response on the SAME stacked headline sample:
#     y = FE + b1*dose + b2*dose^2 + X
#   ACRT(d) = b1 + 2*b2*d is the marginal effect at dose d; plotted across the
#   observed dose range with a delta-method CI band. A significant b2 => bend.
#
# PART B -- dCDH curvature DIAGNOSTIC.
#   did_multiplegt_dyn does NOT return a dose curve; instead we compare the
#   event-study effects under a LINEAR dose model (continuous=1) vs a QUADRATIC
#   one (continuous=2). Divergence => the linear dose model misses curvature.
#
# Output: data/results/curvature_acrt_feols.csv,
#         data/results/curvature_att_curve.csv,
#         data/results/curvature_acrt_cells.csv,
#         data/results/curvature_dcdh_lin_vs_quad.csv,
#         figures/curvature_acrt_feols.png, figures/curvature_att_curve.png,
#         figures/curvature_acrt_matrix.png, figures/curvature_dcdh_lin_vs_quad.png
if (RUN_CURVATURE) local({
  suppressMessages(library(DIDmultiplegtDYN))
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  lab <- c(log_live_and_work = "Log(work in own muni)", log_in_commuters = "Log(in-commuters)",
           log_total_jobs = "Log(total jobs)", share_live_in = "Share live in muni")

  # copy(): this section adds columns (inc/dose/dose2/ia*/cc*) by reference and
  # must not mutate the shared stacked panel.
  stk <- copy(stk_muni); cs <- ctrls_str
  stk[, inc := as.integer(as.character(treat_cell) %in% c("Inc-bind", "Inc-becomes", "Inc-nobind"))]
  # dose_change, NOT dose_stack -- see the note in 05_figures.R. The curvature
  # test needs the actual dose magnitude, not the rate level.
  stk[, dose := fifelse(inc == 1, dose_change, 0)]; stk[, dose2 := dose^2]
  dmax <- quantile(stk[inc == 1 & dose > 0, dose], 0.95, na.rm = TRUE)   # plot range
  dgrid <- seq(0, dmax, length.out = 60)

  ## PART A: feols ACRT(d) curve
  acrt <- list(); qsig <- list(); att <- list()
  for (o in outcomes) {
    cols <- c("muni", "dose", "dose2", "stack_muni", "stack_year", o, ctrls)
    d <- stk[, ..cols][complete.cases(stk[, ..cols])]
    m <- feols(as.formula(paste0(o, " ~ dose + dose2 + ", cs, " | stack_muni + stack_year")),
               d, cluster = ~muni)
    b <- coef(m)[c("dose", "dose2")]; V <- vcov(m)[c("dose", "dose2"), c("dose", "dose2")]
    # Lay explanation: with a squared-dose term in the model, the damage done
    # by one MORE bit of tax depends on how much tax is already in place:
    # that marginal effect is b1 + 2*b2*dose. Tracing it across doses asks
    # whether the tenth notch of a hike stings as much as the first --
    # a downward slope means each extra bit hurts less than the last.
    acrt_d <- b["dose"] + 2 * b["dose2"] * dgrid
    var_d  <- V["dose", "dose"] + 4 * dgrid^2 * V["dose2", "dose2"] + 4 * dgrid * V["dose", "dose2"]
    se_d   <- sqrt(pmax(var_d, 0))
    acrt[[o]] <- data.table(outcome = o, dose = dgrid, acrt = acrt_d,
                            lo = acrt_d - 1.96 * se_d, hi = acrt_d + 1.96 * se_d)
    att_d  <- b["dose"] * dgrid + b["dose2"] * dgrid^2                    # ATT(d) parabola
    vart_d <- dgrid^2 * V["dose", "dose"] + dgrid^4 * V["dose2", "dose2"] + 2 * dgrid^3 * V["dose", "dose2"]
    set_d  <- sqrt(pmax(vart_d, 0))
    att[[o]] <- data.table(outcome = o, dose = dgrid, att = att_d,
                           lo = att_d - 1.96 * set_d, hi = att_d + 1.96 * set_d,
                           d2_p = m$coeftable["dose2", "Pr(>|t|)"])
    qsig[[o]] <- data.table(outcome = o, b1 = b["dose"], b2 = b["dose2"],
                            b2_p = m$coeftable["dose2", "Pr(>|t|)"],
                            bends = m$coeftable["dose2", "Pr(>|t|)"] < 0.05)
  }
  acrt_dt <- rbindlist(acrt); qsig_dt <- rbindlist(qsig)
  fwrite(acrt_dt, file.path(RESULTS_DIR, "curvature_acrt_feols.csv"))
  acrt_dt[, ol := lab[outcome]]
  gA <- ggplot(acrt_dt, aes(dose, acrt)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, fill = "#1b7837") +
    geom_line(color = "#1b7837") + facet_wrap(~ ol, scales = "free_y") +
    labs(title = "Parametric ACRT(d): marginal effect at each dose (pooled Inc, headline sample)",
         subtitle = "feols quadratic; flat => linear ACR fine, sloped => per-pp effect bends with dose size",
         x = "dose (cumulative rate change, pp)", y = "ACRT(d) = marginal effect at dose d") +
    theme_bw(base_size = 10)
  ggsave(file.path(OUT_DIR, "curvature_acrt_feols.png"), gA, width = 9, height = 6, dpi = 300)

  ## PART A2: ATT(d) dose-response CURVE (the parabola b1*d + b2*d^2)
  # The *function* (height vs never-treated), not its slope. Curves when b2 != 0
  # (significant: log_live_and_work p=.007, share_live_in p=.024; the other two flat).
  att_dt <- rbindlist(att); fwrite(att_dt, file.path(RESULTS_DIR, "curvature_att_curve.csv"))
  att_dt[, ol := sprintf("%s  (d^2 p=%.3f)", lab[outcome], d2_p)]
  gA2 <- ggplot(att_dt, aes(dose, att)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, fill = "#1b7837") +
    geom_line(color = "#1b7837") + facet_wrap(~ ol, scales = "free_y") +
    labs(title = "ATT(d) dose-response: total effect at each dose (pooled Inc, headline sample)",
         subtitle = "quadratic b1*d + b2*d^2, effect vs never-treated; curvature where the dose^2 term is significant",
         x = "dose (cumulative rate change, pp)", y = "ATT(d) = total effect vs never-treated") +
    theme_bw(base_size = 10)
  ggsave(file.path(OUT_DIR, "curvature_att_curve.png"), gA2, width = 9, height = 6, dpi = 300)
  message("feols quadratic curvature (b2 = dose^2):"); print(qsig_dt)

  ## PART C: ACRT(d) MATRIX -- outcome x decomposition group, quadratic vs cubic
  # Cell-interacted polynomial: ACRT_group(d) = sum_k k*b_k*d^(k-1). Two findings:
  #  (1) the AGGREGATE curvature is robust to polynomial order (deg2 ~ deg3);
  #  (2) within the thin cells the CUBIC overfits wildly, so curvature is NOT
  #      identified per-cell -- the linear ACR is fine for the headline Inc-bind.
  # Dose capped at 0.5 pp (the data-dense range; beyond it cubics explode).
  stk[, id1 := fifelse(inc == 1, dose, 0)]
  dg <- seq(0, 0.5, length.out = 50); pgroups <- c("Aggregate", "Inc-bind", "Inc-becomes")
  acrt_poly <- function(bb, V, deg, d) {            # ACRT(d) and delta-method CI
    est <- rep(0, length(d)); for (k in 1:deg) est <- est + k * bb[k] * d^(k - 1)
    se <- sapply(d, function(x) { g <- sapply(1:deg, function(k) k * x^(k - 1))
                                  sqrt(max(as.numeric(t(g) %*% V %*% g), 0)) })
    data.table(dose = d, acrt = est, lo = est - 1.96 * se, hi = est + 1.96 * se)
  }
  mat <- list()
  for (o in outcomes) for (deg in c(2, 3)) {
    for (k in 1:deg) stk[[paste0("ia", k)]] <- stk$id1^k
    tn <- paste0("ia", 1:deg); cols <- c("muni", tn, "stack_muni", "stack_year", o, ctrls)
    da <- stk[, ..cols][complete.cases(stk[, ..cols])]
    ma <- feols(as.formula(paste0(o, " ~ ", paste(tn, collapse = " + "), " + ", cs, " | stack_muni + stack_year")), da, cluster = ~muni)
    ca <- acrt_poly(coef(ma)[tn], vcov(ma)[tn, tn], deg, dg)
    ca[, `:=`(outcome = o, group = "Aggregate", degree = paste0("deg ", deg))]; mat[[length(mat) + 1L]] <- ca
    for (k in 1:deg) stk[[paste0("cc", k)]] <- stk$dose^k
    tn2 <- paste0("cc", 1:deg); cols <- c("muni", "treat_cell", tn2, "stack_muni", "stack_year", o, ctrls)
    dc <- stk[, ..cols][complete.cases(stk[, ..cols])]
    fc <- paste0(o, " ~ ", paste(sprintf("i(treat_cell,%s,ref='NC')", tn2), collapse = " + "), " + ", cs, " | stack_muni + stack_year")
    mc <- feols(as.formula(fc), dc, cluster = ~muni); V <- vcov(mc); cn <- names(coef(mc))
    for (g in c("Inc-bind", "Inc-becomes")) {
      nm <- paste0("treat_cell::", g, ":", tn2); if (!all(nm %in% cn)) next
      cc <- acrt_poly(coef(mc)[nm], V[nm, nm], deg, dg)
      cc[, `:=`(outcome = o, group = g, degree = paste0("deg ", deg))]; mat[[length(mat) + 1L]] <- cc
    }
  }
  mat_dt <- rbindlist(mat, fill = TRUE); fwrite(mat_dt, file.path(RESULTS_DIR, "curvature_acrt_cells.csv"))
  mat_dt[, ol := lab[outcome]]; mat_dt[, group := factor(group, levels = pgroups)]
  gC <- ggplot(mat_dt, aes(dose, acrt, color = degree, fill = degree)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.08, color = NA) + geom_line() +
    facet_grid(ol ~ group, scales = "free_y") +
    scale_color_manual(values = c("deg 2" = "#1b7837", "deg 3" = "#b35806")) +
    scale_fill_manual(values = c("deg 2" = "#1b7837", "deg 3" = "#b35806")) +
    labs(title = "ACRT(d) matrix: outcome x decomposition group, quadratic vs cubic dose",
         subtitle = "feols polynomial; dose capped at 0.5 pp (data-dense range). Aggregate robust to order; cubic overfits the thin cells. Inc-nobind omitted (14 munis).",
         x = "dose (cumulative rate change, pp)", y = "ACRT(d): marginal effect at dose d",
         color = "poly degree", fill = "poly degree") +
    theme_bw(base_size = 9) + theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "curvature_acrt_matrix.png"), gC, width = 10, height = 8.5, dpi = 300)

  ## PART B: dCDH continuous=1 vs continuous=2
  # Diagnostic is about the dose model, not the controls; fiscal controls make
  # did_multiplegt_dyn(continuous=2) ~5x slower, so we drop them here (the linear-
  # vs-quadratic dose comparison is unaffected in substance).
  # Lay explanation: same curvature question as Part A, but asked through the
  # alternative estimator, which never lets any town count negatively in the
  # average. It is run twice -- once assuming effects scale in a straight
  # line with dose, once allowing a bend -- and the two event-study paths are
  # overlaid. If they sit on top of each other, the straight-line assumption
  # was not hiding anything.
  panel <- panel[as.character(cell_muni) %in% c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind") &
                   !is.na(resident_eit)]
  panel[, muni_int := as.integer(factor(muni))]
  dcdh <- list()
  # continuous=2 is very slow; run the diagnostic on the two outcomes that show
  # significant feols curvature (the rest are statistically linear -- Part A).
  dcdh_outcomes <- c("log_live_and_work", "share_live_in")
  for (o in dcdh_outcomes) {
    cols <- c("muni_int", "year", "resident_eit", o)
    dd <- panel[, ..cols][complete.cases(panel[, ..cols])]; setnames(dd, o, "Y")
    for (cv in c(1, 2)) {
      m <- tryCatch(did_multiplegt_dyn(df = as.data.frame(dd), outcome = "Y", group = "muni_int",
              time = "year", treatment = "resident_eit", effects = 3, placebo = 2,  # NB: doubles
              continuous = cv, cluster = "muni_int", graph_off = TRUE),
            error = function(err) NULL)
      if (is.null(m)) next
      ef <- as.data.table(m$results$Effects)[, .(rel = seq_len(.N), est = Estimate, se = SE)]
      ef[, `:=`(outcome = o, dose_model = if (cv == 1) "linear" else "quadratic")]
      dcdh[[length(dcdh) + 1L]] <- ef
    }
    message(sprintf("  dCDH %s done", o))
  }
  dcdh_dt <- rbindlist(dcdh, fill = TRUE)
  fwrite(dcdh_dt, file.path(RESULTS_DIR, "curvature_dcdh_lin_vs_quad.csv"))
  dcdh_dt[, ol := lab[outcome]]
  gB <- ggplot(dcdh_dt, aes(rel, est, color = dose_model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~ ol, scales = "free_y") +
    scale_color_manual(values = c(linear = "#2166ac", quadratic = "#b2182b")) +
    labs(title = "dCDH curvature diagnostic: event study under linear vs quadratic dose model",
         subtitle = "did_multiplegt_dyn continuous=1 vs continuous=2; divergence => the linear dose model misses curvature",
         x = "periods since rate change", y = "estimated effect", color = "dose model") +
    theme_bw(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "curvature_dcdh_lin_vs_quad.png"), gB, width = 9, height = 6, dpi = 300)
  message("Saved curvature_acrt_feols.csv + curvature_dcdh_lin_vs_quad.csv + 2 figures")
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
#   4. HonestDiD breakdown Mbar (Rambachan-Roth)    -- how large a pre-trend
#        violation (relative to the max pre-period deviation) is needed before
#        the post-effect CI includes zero; bigger = more robust.
# Per-model tests + hdid_breakdown() now come from pretrend_bucket() in
# _helpers.R (extracted verbatim). robust_clean = passes the
# few-clusters-robust frequentist tests (individual leads + linear slope);
# the joint Wald and HonestDiD breakdown are reported but do not drive the
# verdict.
# (A wild-cluster bootstrap was attempted but fwildclusterboot is incompatible
#  with the stacked design's singleton fixed effects; HonestDiD serves the same
#  small-sample-robustness purpose and is the standard tool.)
if (RUN_PRETREND_BATTERY) local({
  K <- 5L; set.seed(42); Mbarvec <- c(0.5, 1, 1.5, 2)
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
      pretrend_bucket(m, lin_model = ml, lin_term = "dose_rel", Mbarvec = Mbarvec))
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
    for (o in outcomes) {
      cols <- c("muni", "rel", "stack_muni", "stack_year", "cell_fake",
                "dose_fake", o, ctrls)
      dd <- stk_fake[, ..cols][complete.cases(stk_fake[, ..cols])]
      if (!nrow(dd)) next
      m <- tryCatch(feols(as.formula(paste0(o,
            " ~ i(cell_fake, dose_fake, ref = 'NC') + ", ctrls_str,
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
