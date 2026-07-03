# q3_unified_headline_es.R
# Pre-trend event study for the muni-side of the HEADLINE stacked-continuous
# DiD (eq. headline in paper/main.tex). Replaces the muni-side
# i(cell_muni, dose_muni) interaction with a Sun-Abraham-style continuous ES
# using realized dose (constant per muni). Keeps SD-side and nonres-side
# cell x dose interactions as in the headline so the spec is otherwise
# identical to eq. headline.
#
# Spec (per muni-side cell c, run on subset {NC, c}):
#   y_{it} = alpha_{stack,muni} + gamma_{stack,year}
#          + sum_{k != -1} beta^muni_k * 1{rel=k} * realized_dose_muni_i
#          + sum_{c'} beta^SD_{c'} * Delta tau_SD_{it} * 1{cell_SD_i = c'}
#          + sum_{c'} beta^nr_{c'} * Delta tau_nr_{it} * 1{cell_nr_i = c'}
#          + X_{it} phi + epsilon
#
# Outputs:
#   data/q3_unified/headline_es_coefs.csv  (lead-by-lead per outcome,cell)
#   data/q3_unified/headline_es_att.csv    (post-mean and pre-mean)
#   figures/headline_es_<outcome>.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
source(file.path(SCRIPT_DIR, "paper_helpers.R"))
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

# Reuse data prep from the headline
e <- new.env()
sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
panel    <- e$panel
ctrls    <- e$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs",
              "share_live_in","share_work_outside")

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

# Realized dose ----
# The D-tilde_i in eq. realized-dose-es: each treated muni's cumulative rate
# change from the year BEFORE its event (c-1) to c+5 — a CONSTANT per-muni
# magnitude tag, not time-varying. The ES interacts event-time dummies with
# this constant, so beta_k reads "per-pp effect at event time k" and the
# leads test STRONG parallel trends: before the event, eventual-dose size
# should not predict outcome trajectories.
mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)])
setorder(mu_year, muni, year)
rd <- mu_year[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  yr_max <- min(c_muni + K_stack, max(year, na.rm = TRUE))
  end <- resident_eit[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose_muni = end_v - pre_v)
}, by = muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
nc_munis <- unique(panel[cell_muni == "NC", muni])
rd <- rbind(rd, data.table(muni = nc_munis, realized_dose_muni = 0))
panel <- merge(panel, rd, by = "muni", all.x = TRUE)
panel[is.na(realized_dose_muni), realized_dose_muni := 0]

# Stack on muni cohorts (same as headline)
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

cat("Building stacked panel (headline structure)...\n")
stk <- build_stack(panel)
stk[, stack_muni := paste0(stack, "_", muni)]
stk[, stack_year := paste0(stack, "_", year)]
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk[, cell_sd := factor(cell_sd,   levels = cell_levels)]
stk[, cell_nr := factor(cell_nr,   levels = cell_levels)]
# Pre-event SD and nonres dose should be 0 for non-NC sides too in the
# headline construction, but that's already handled by the panel build in
# q3_unified_full_table.R (dose_sd and dose_nr are zero pre-event).
stk[is.na(dose_sd), dose_sd := 0]
stk[is.na(dose_nr), dose_nr := 0]

cat(sprintf("Stack: %s rows; %s munis (treated); %s munis (NC)\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell_muni != "NC", muni]), big.mark=","),
            format(uniqueN(stk[treat_cell_muni == "NC", muni]), big.mark=",")))

# ---------------------------------------------------------------------------
# Run per-cell ES with full SD-side and nonres-side cell x dose controls
# ---------------------------------------------------------------------------
es_results <- list(); att_results <- list()
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- stk[treat_cell_muni %in% c("NC", cl)]
    cols <- c("muni","year","stack_muni","stack_year","treat_cell_muni",
               "realized_dose_muni","rel",
               "cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls)
    dd <- dd[, ..cols]
    dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0 || sum(dd$treat_cell_muni == cl) == 0) {
      cat(sprintf("  %s skip (no treated rows)\n", cl)); next
    }
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1) ",
         "+ i(cell_sd, dose_sd, ref = 'NC') ",
         "+ i(cell_nr, dose_nr, ref = 'NC') + ",
         ctrls_str, " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni),
                   error = function(ee) {
                     cat(sprintf("  %s error: %s\n", cl, conditionMessage(ee))); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) next
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic

    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    wp <- wald_pretrend(m, "^rel::-[2-9]")
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, cell = cl,
      att_post_per_pp = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se     = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se      = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      wald_pre_stat = round(wp$stat, 3),
      wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4),
      n = nobs(m))
    cat(sprintf("  %-16s post=%6.3f (SE %.3f)  pre=%6.3f (SE %.3f)  N=%s\n",
                cl, mean(post$est, na.rm=TRUE),
                sqrt(mean(post$se^2, na.rm=TRUE) / max(nrow(post),1)),
                mean(pre$est, na.rm=TRUE),
                sqrt(mean(pre$se^2, na.rm=TRUE) / max(nrow(pre),1)),
                format(nobs(m), big.mark=",")))
  }
}
es_dt  <- rbindlist(es_results, fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "headline_es_coefs.csv"))
fwrite(att_dt, file.path(out_dir, "headline_es_att.csv"))

# Figures
# NOTE: figure output is superseded by build_appendix_es_figures.R, which
# reads headline_es_coefs.csv and emits the ggplot ribbon-style figures.
# Gated off by default to prevent clobbering on model re-runs.
if (FALSE) {
es_plot <- copy(es_dt)
es_plot <- rbind(es_plot,
                  unique(es_plot[, .(outcome, cell)])[, .(outcome, cell, rel = -1L,
                                                          est = 0, se = 0, p = NA, n = NA)],
                  fill = TRUE)
es_plot[, lo := est - 1.96 * se]
es_plot[, hi := est + 1.96 * se]
for (o in outcomes) {
  dd <- es_plot[outcome == o]
  if (nrow(dd) == 0) next
  p <- ggplot(dd, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) +
    geom_line() +
    facet_wrap(~ cell, ncol = 3, scales = "free_y") +
    labs(title = paste0("Headline-spec Sun-Abraham ES (muni side, w/ SD+nonres controls): ", o),
         subtitle = "Per-pp realized-dose effect at event-time k; k=-1 omitted",
         x = "Event time", y = expression(beta[k])) +
    theme_bw(base_size = 9)
  ggsave(file.path(fig_dir, paste0("headline_es_", o, ".png")),
         p, width = 8.5, height = 5, dpi = 300)
}
}  # end if(FALSE) figure gate
cat("\n--- Headline-spec ES summary (muni side) ---\n")
print(att_dt)
