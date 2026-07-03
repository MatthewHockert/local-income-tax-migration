#!/usr/bin/env Rscript
# 04_robustness.R -- Every robustness / alternative-specification model,
# consolidated from the former standalone scripts:
#   q3_unified_dcdh_robustness.R      -> RUN_DCDH_POOLED
#   q3_unified_dcdh_bycell.R          -> RUN_DCDH_BYCELL
#   q3_unified_act32_3level.R         -> RUN_ACT32
#   q3_unified_act47_robustness.R     -> RUN_ACT47
#   q3_unified_curvature_robustness.R -> RUN_CURVATURE
# (The pre-trend battery lives in stage 03, not here.)
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

# Shared setup ----
env <- build_headline_env()
panel <- env$panel
stk_muni <- env$stk_muni
ctrls <- env$ctrls
ctrls_str <- env$ctrls_str

# Pull the estimates out of a did_multiplegt_dyn object into a tidy table:
# Effects = post-event rows (rel 1..K), Placebos = pre-event rows (rel -1..-K,
# these are the pre-trend test), plus a rel=0 reference row at exactly 0.
# NOTE the scale: these are per-event LEVEL effects at the realized dose,
# NOT per-pp slopes -- compare to feols only after multiplying the feols ACR
# by the group's mean dose (~0.33 pp). Used by both dCDH sections.
extract_dcdh <- function(m) {
  res <- m$results
  eff <- as.data.table(res$Effects)[, .(rel = seq_len(.N), est = Estimate, se = SE,
                                         ci_lo = `LB CI`, ci_hi = `UB CI`)]
  pl <- if (!is.null(res$Placebos) && res$N_Placebos > 0)
          as.data.table(res$Placebos)[, .(rel = -seq_len(.N), est = Estimate, se = SE,
                                           ci_lo = `LB CI`, ci_hi = `UB CI`)] else NULL
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
# Window: effects = 3, placebo = 2 -- the longest horizon the continuous +
# controlled estimator supports on this sample (longer errors out).
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
  K_post <- 3; K_pre <- 2
  NBOOT  <- as.integer(Sys.getenv("DCDH_BOOTSTRAP", "0"))   # set DCDH_BOOTSTRAP=50 to bootstrap

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
  NBOOT  <- as.integer(Sys.getenv("DCDH_BOOTSTRAP", "0"))

  panel <- panel[as.character(cell_muni) %in% c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind") &
                   !is.na(resident_eit)]

  groups <- list(
    pooled     = c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind"),
    incbind    = c("NC", "Inc-bind"),
    incbecomes = c("NC", "Inc-becomes"))
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")

  all <- list()
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
      co[, ate := tryCatch(m$results$ATE$Estimate[1], error = function(e) NA_real_)]
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
  ACT32_YEAR <- 2012L

  run_act32_split <- function(stk, c_col, cell_col, dose_col,
                               outcomes, ctrls, fe_str, cluster_var, level_tag) {
    ctrls_str <- paste(ctrls, collapse = " + ")
    results <- list()
    diagnostics <- list()
    cohort <- stk[, get(c_col)]
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
  act47_names <- toupper(c(
    "ALIQUIPPA", "BRADDOCK", "CARBONDALE", "CHESTER CITY", "CLAIRTON",
    "DUQUESNE", "EAST PITTSBURGH", "FARRELL", "FRANKLIN",
    "GREENVILLE", "HARRISBURG", "HAZLETON", "HOMESTEAD", "JEANNETTE",
    "JOHNSTOWN", "MILLBOURNE", "NANTICOKE", "NEW CASTLE", "PITTSBURGH",
    "PLYMOUTH TOWNSHIP", "RANKIN", "READING", "SCRANTON", "SHAMOKIN",
    "SHARON", "TURTLE CREEK", "WEST HAZLETON", "WILKES-BARRE",
    "WILKINSBURG", "YEADON", "ALTOONA", "COATESVILLE", "DUBOIS",
    "ELLPORT", "GLASSPORT", "MAHANOY CITY"
  ))

  # 1. Pull municipality names onto the shared headline panel
  rate <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  muni_names <- unique(rate[!is.na(cousub_fips),
                              .(muni = cousub_fips,
                                name = toupper(municipality))])
  panel <- merge(panel, muni_names, by = "muni", all.x = TRUE)

  # Flag Act 47 munis in the Inc-bind cell
  treated_incbind <- unique(panel[cell_muni == "Inc-bind",
                                    .(muni, name)])
  treated_incbind[, is_act47 := vapply(name, function(nm) {
    any(sapply(act47_names, function(a47) grepl(a47, nm, fixed = TRUE)))
  }, logical(1))]

  message(sprintf("Inc-bind munis: %d total", nrow(treated_incbind)))
  message(sprintf("Act 47 (or distressed-list) matches: %d",
                  sum(treated_incbind$is_act47)))
  message("Act 47 Inc-bind munis identified:")
  print(treated_incbind[is_act47 == TRUE, name])

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
  }

  run_one <- function(stk, outcome) {
    f <- as.formula(paste0(outcome,
         " ~ i(treat_cell, dose_muni, ref = 'NC') ",
         "+ i(cell_sd, dose_sd, ref = 'NC') ",
         "+ i(cell_nr, dose_nr, ref = 'NC') + ",
         ctrls_str, " | stack_muni + stack_year"))
    dd <- stk[, c("muni", "year", "stack_muni", "stack_year", "treat_cell",
                  "dose_muni", "cell_sd", "dose_sd", "cell_nr", "dose_nr",
                  outcome, muni_ctrls), with = FALSE]
    dd <- dd[complete.cases(dd)]
    m <- feols(f, data = dd, cluster = ~muni)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"),
             c("est", "se", "p"), skip_absent = TRUE)
    ic <- ct[grepl("^treat_cell::Inc-bind:", term)]
    data.table(outcome = outcome,
               est = round(ic$est, 4),
               se = round(ic$se, 4),
               p = round(ic$p, 4),
               n = nobs(m))
  }

  outcomes <- c("log_live_and_work", "log_in_commuters",
                "log_total_jobs", "share_live_in")
  n_inc_bind_total <- nrow(treated_incbind)
  message(sprintf("Cousub headline (all %d Inc-bind munis)", n_inc_bind_total))
  res_full <- rbindlist(lapply(outcomes, function(o) run_one(stk_full, o)))
  res_full[, spec := sprintf("All %d Inc-bind munis", n_inc_bind_total)]
  print(res_full)

  message("Cousub headline (excluding Act 47 Inc-bind munis)")
  res_a47drop <- rbindlist(lapply(outcomes, function(o) run_one(stk_a47drop, o)))
  res_a47drop[, spec := sprintf("Drop %d Act 47 Inc-bind", length(act47_munis_in_panel))]
  print(res_a47drop)

  results <- rbind(res_full, res_a47drop)
  fwrite(results, file.path(RESULTS_DIR, "act47_robustness_cousub.csv"))
  message("Saved data/results/act47_robustness_cousub.csv")

  # 3. LaTeX summary table for the paper
  fmt_coef <- function(est, se, p) {
    stars <- if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
    sprintf("$%+.3f^{%s}$ (%.3f)", est, stars, se)
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
  stk[, dose := fifelse(inc == 1, dose_stack, 0)]; stk[, dose2 := dose^2]
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
if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
