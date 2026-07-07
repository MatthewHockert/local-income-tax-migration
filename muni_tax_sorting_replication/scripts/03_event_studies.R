#!/usr/bin/env Rscript
# 03_event_studies.R -- Event studies: realized-dose ES + pooled ES + pre-trend battery.
# Consolidates (verbatim model code, shared data build via _helpers.R):
#   q3_unified_headline_es.R       -> Headline realized-dose ES section
#   q3_unified_pooled_es.R         -> Pooled realized-dose ES section
#   q3_unified_pretrend_battery.R  -> Pre-trend battery section
# Spec (per muni-side cell c, run on subset {NC, c}):
#   y_{it} = alpha_{stack,muni} + gamma_{stack,year}
#          + sum_{k != -1} beta^muni_k * 1{rel=k} * realized_dose_muni_i
#          + sum_{c'} beta^SD_{c'} * Delta tau_SD_{it} * 1{cell_SD_i = c'}
#          + sum_{c'} beta^nr_{c'} * Delta tau_nr_{it} * 1{cell_nr_i = c'}
#          + X_{it} phi + epsilon
# The realized dose is each treated muni's cumulative rate change from c-1 to
# c+5 -- a CONSTANT per-muni magnitude tag, so the leads test STRONG parallel
# trends: before the event, eventual-dose size should not predict outcomes.
# Outputs -> data/results/: headline_es_coefs.csv, headline_es_att.csv,
#   pooled_es_cousub_coefs.csv, pooled_es_cousub_att.csv, pretrend_battery.csv
# Run from package root:  Rscript scripts/03_event_studies.R

suppressMessages({
  library(data.table)
  library(fixest)
  library(HonestDiD)
})

.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_helpers.R"))

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# Flags ----
RUN_HEADLINE_ES      <- TRUE
RUN_POOLED_ES        <- TRUE
# RUN_PRETREND_BATTERY moved to 04_robustness.R (2026-07-07)

# Shared setup ----
env <- build_headline_env()
panel <- env$panel
ctrls <- env$ctrls
ctrls_str <- env$ctrls_str
K_stack <- 5L

cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                 "Dec-bind", "Dec-becomes-non", "Dec-nobind")

# Shared realized-dose stack ----
# The pooled-ES section originally sys.source()d q3_unified_headline_es.R just
# to reuse its stacked panel (`stk`) and controls; both sections now share this
# one build. Realized dose comes from add_realized_dose() in _helpers.R (same
# column name, `realized_dose_muni`, as the original inline block). Stack is on
# muni cohorts, identical to the headline construction.
if (RUN_HEADLINE_ES || RUN_POOLED_ES) {
  # Lay explanation: tag each treated town with one number -- the total rate
  # change it will have racked up five years after its first change. It is a
  # size label, like knowing in advance which towns are headed for a big hike.
  # The event study then asks: were the big-hike towns already drifting away
  # from the controls BEFORE the hike? If yes, the comparison is suspect.
  panel_es <- add_realized_dose(copy(panel), K = K_stack)

  build_stack <- function(p, K = K_stack) {
    treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni, cell = cell_muni)])
    nc_munis <- unique(p[cell_muni == "NC", muni])
    cohort_years <- sort(unique(treated$cohort))
    rbindlist(lapply(cohort_years, function(cy) {
      win <- (cy - K):(cy + K)
      t_munis <- treated[cohort == cy]
      if (nrow(t_munis) == 0) return(NULL)
      d_t <- p[muni %in% t_munis$muni & year %in% win]
      d_t[, treat_cell_muni := cell_muni]
      d_c <- p[muni %in% nc_munis & year %in% win]
      d_c[, treat_cell_muni := "NC"]
      d <- rbind(d_t, d_c, fill = TRUE)
      d[, stack := cy]; d[, rel := as.integer(year - cy)]
      d
    }), fill = TRUE)
  }

  stk_es <- build_stack(panel_es)
  stk_es[, stack_muni := paste0(stack, "_", muni)]
  stk_es[, stack_year := paste0(stack, "_", year)]
  stk_es[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
  stk_es[, cell_sd := factor(cell_sd, levels = cell_levels)]
  stk_es[, cell_nr := factor(cell_nr, levels = cell_levels)]
  # Pre-event SD and nonres dose are already 0 pre-event by construction in
  # build_headline_env(); the fill below only covers rows missing a cell tag.
  stk_es[is.na(dose_sd), dose_sd := 0]
  stk_es[is.na(dose_nr), dose_nr := 0]
}

# Headline realized-dose ES ----
# (from q3_unified_headline_es.R) Per-cell ES with full SD-side and
# nonres-side cell x dose controls; otherwise identical to eq. headline.
# NOTE: figure output is superseded by the figures stage, which reads
# headline_es_coefs.csv and emits the ggplot ribbon-style figures.
if (RUN_HEADLINE_ES) local({
  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs",
                "share_live_in", "share_work_outside")
  es_results <- list(); att_results <- list()
  for (o in outcomes) {
    for (cl in setdiff(cell_levels, "NC")) {
      dd <- stk_es[treat_cell_muni %in% c("NC", cl)]
      cols <- c("muni", "year", "stack_muni", "stack_year", "treat_cell_muni",
                "realized_dose_muni", "rel",
                "cell_sd", "dose_sd", "cell_nr", "dose_nr", o, ctrls)
      dd <- dd[, ..cols]
      dd <- dd[complete.cases(dd)]
      if (nrow(dd) == 0 || sum(dd$treat_cell_muni == cl) == 0) next
      # Lay explanation: i(rel, realized_dose, ref=-1) estimates a separate
      # per-percentage-point effect for EACH year relative to the rate change
      # (-5, -4, ..., +5), like a film strip around the event. The year just
      # before the change is pinned at zero, so every frame reads as "how far
      # from where we stood the year before it happened."
      f <- as.formula(paste0(o,
        " ~ i(rel, realized_dose_muni, ref = -1) ",
        "+ i(cell_sd, dose_sd, ref = 'NC') ",
        "+ i(cell_nr, dose_nr, ref = 'NC') + ",
        ctrls_str, " | stack_muni + stack_year"))
      m <- tryCatch(feols(f, data = dd, cluster = ~muni),
                    error = function(ee) NULL)
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      ic <- ct[grepl("^rel::", term)]
      if (nrow(ic) == 0) next
      ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
      ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
      ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
      es_results[[length(es_results) + 1L]] <- ic

      post <- ic[rel >= 0 & rel <= 5]
      pre  <- ic[rel <= -2]
      # Lay explanation: the Wald test asks whether all the BEFORE-the-change
      # coefficients are jointly zero. If treated and control towns really
      # were moving in parallel before the tax change, nothing should show up
      # in those years -- a big test statistic (small p) means the two groups
      # were already on different paths and the comparison is contaminated.
      wp <- wald_pretrend(m, "^rel::-[2-9]")
      att_results[[length(att_results) + 1L]] <- data.table(
        outcome = o, cell = cl,
        att_post_per_pp = round(mean(post$est, na.rm = TRUE), 3),
        att_post_se     = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post), 1)), 3),
        att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
        att_pre_se      = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre), 1)), 3),
        wald_pre_stat = round(wp$stat, 3),
        wald_pre_df = wp$df,
        wald_pre_p = round(wp$p, 4),
        n = nobs(m))
    }
  }
  es_dt  <- rbindlist(es_results, fill = TRUE)
  att_dt <- rbindlist(att_results, fill = TRUE)
  fwrite(es_dt,  file.path(RESULTS_DIR, "headline_es_coefs.csv"))
  fwrite(att_dt, file.path(RESULTS_DIR, "headline_es_att.csv"))
  print(att_dt)
})

# Pooled realized-dose ES ----
# (from q3_unified_pooled_es.R) Pool-Inc / Pool-Dec: all Inc-* (resp. Dec-*)
# treated munis versus NC controls, same stacked panel and controls as the
# headline ES. Block-level pooled ES is out of scope for this package (it ran
# from standalone scripts against the ~30M-row block stack).
if (RUN_POOLED_ES) local({
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  dec_cells <- c("Dec-bind", "Dec-becomes-non", "Dec-nobind")
  muni_outcomes <- c("log_live_and_work", "log_in_commuters",
                     "log_total_jobs",
                     "share_live_in", "share_work_outside")

  stk_muni <- copy(stk_es)
  ctrls_muni <- ctrls
  # Lay explanation: instead of studying each binding cell on its own, lump
  # every rate INCREASE together (and, separately, every DECREASE) and compare
  # that whole pile to the never-changers. This is the naive "all hikes vs no
  # hikes" view -- useful as a benchmark for what the cell split adds.
  stk_muni[, pool_inc := as.integer(as.character(treat_cell_muni) %in% inc_cells)]
  stk_muni[, pool_dec := as.integer(as.character(treat_cell_muni) %in% dec_cells)]
  stk_muni[, is_nc    := as.integer(as.character(treat_cell_muni) == "NC")]

  run_pooled_es_muni <- function(direction) {
    dd <- if (direction == "Inc") stk_muni[pool_inc == 1 | is_nc == 1]
          else                    stk_muni[pool_dec == 1 | is_nc == 1]
    results <- list(); att <- list()
    for (o in muni_outcomes) {
      cols <- c("muni", "year", "stack", "stack_muni", "stack_year", "rel",
                "realized_dose_muni", "cell_sd", "dose_sd", "cell_nr", "dose_nr",
                o, ctrls_muni)
      cols <- intersect(cols, names(dd))
      d <- dd[, ..cols]
      if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
      if (!is.null(d$dose_nr)) d[is.na(dose_nr), dose_nr := 0]
      d <- d[complete.cases(d)]
      if (nrow(d) == 0) next
      f <- as.formula(paste0(o,
        " ~ i(rel, realized_dose_muni, ref = -1)",
        " + i(cell_sd, dose_sd, ref = 'NC')",
        " + i(cell_nr, dose_nr, ref = 'NC') + ",
        paste(ctrls_muni, collapse = " + "),
        " | stack_muni + stack_year"))
      m <- tryCatch(feols(f, data = d, cluster = ~muni),
                    error = function(e) NULL)
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      ic <- ct[grepl("^rel::", term)]
      if (nrow(ic) == 0) next
      ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
      ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
      ic[, outcome := o]; ic[, direction := direction]; ic[, n := nobs(m)]
      results[[length(results) + 1L]] <- ic
      post <- ic[rel >= 0 & rel <= 5]
      pre  <- ic[rel <= -2]
      wp <- wald_pretrend(m, "^rel::-[2-9]")
      dep_vec <- m$fitted.values + m$residuals
      att[[length(att) + 1L]] <- data.table(
        outcome = o, direction = direction,
        att_post = round(mean(post$est, na.rm = TRUE), 3),
        att_post_se = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post), 1)), 3),
        att_pre = round(mean(pre$est, na.rm = TRUE), 3),
        att_pre_se  = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre), 1)), 3),
        wald_pre_stat = round(wp$stat, 3),
        wald_pre_df = wp$df,
        wald_pre_p = round(wp$p, 4),
        n = nobs(m),
        r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
        wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
        dep_mean = round(mean(dep_vec, na.rm = TRUE), 3),
        dep_sd = round(sd(dep_vec, na.rm = TRUE), 3))
    }
    list(coefs = rbindlist(results, fill = TRUE),
         att   = rbindlist(att, fill = TRUE))
  }

  r_muni_inc <- run_pooled_es_muni("Inc")
  r_muni_dec <- run_pooled_es_muni("Dec")

  fwrite(rbind(r_muni_inc$coefs, r_muni_dec$coefs, fill = TRUE),
         file.path(RESULTS_DIR, "pooled_es_cousub_coefs.csv"))
  fwrite(rbind(r_muni_inc$att, r_muni_dec$att, fill = TRUE),
         file.path(RESULTS_DIR, "pooled_es_cousub_att.csv"))
  print(rbind(r_muni_inc$att, r_muni_dec$att, fill = TRUE))
})

# (Pre-trend battery moved to 04_robustness.R on 2026-07-07 -- it is a
#  robustness test, and the user wants all robustness in one script.)

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
