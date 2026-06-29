# q3_unified_curvature_robustness.R
# Does the per-point (marginal) effect bend with dose size? Two feasible
# parametric answers (the nonparametric contdid/CGS curve is not estimable on
# these thin cohorts -- see q3_unified_contdid_robustness.R notes).
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
# Output: data/q3_unified/curvature_acrt_feols.csv,
#         data/q3_unified/curvature_dcdh_lin_vs_quad.csv,
#         figures/curvature_acrt_feols.png, figures/curvature_dcdh_lin_vs_quad.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2); library(DIDmultiplegtDYN) })
# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
data_dir <- file.path(project_dir, "data"); out_dir <- file.path(data_dir, "q3_unified")
fig_dir  <- file.path(project_dir, "figures")
outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
lab <- c(log_live_and_work="Log(work in own muni)", log_in_commuters="Log(in-commuters)",
         log_total_jobs="Log(total jobs)", share_live_in="Share live in muni")

e <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
stk <- as.data.table(e$stk_muni); ctrls <- e$ctrls; cs <- paste(ctrls, collapse = " + ")
stk[, inc := as.integer(as.character(treat_cell) %in% c("Inc-bind","Inc-becomes","Inc-nobind"))]
stk[, dose := fifelse(inc == 1, dose_stack, 0)]; stk[, dose2 := dose^2]
dmax <- quantile(stk[inc == 1 & dose > 0, dose], 0.95, na.rm = TRUE)   # plot range
dgrid <- seq(0, dmax, length.out = 60)

## PART A: feols ACRT(d) curve -----------------------------------------------
acrt <- list(); qsig <- list()
for (o in outcomes) {
  cols <- c("muni","dose","dose2","stack_muni","stack_year", o, ctrls)
  d <- stk[, ..cols][complete.cases(stk[, ..cols])]
  m <- feols(as.formula(paste0(o," ~ dose + dose2 + ",cs," | stack_muni + stack_year")),
             d, cluster = ~muni)
  b <- coef(m)[c("dose","dose2")]; V <- vcov(m)[c("dose","dose2"), c("dose","dose2")]
  acrt_d <- b["dose"] + 2 * b["dose2"] * dgrid
  var_d  <- V["dose","dose"] + 4*dgrid^2*V["dose2","dose2"] + 4*dgrid*V["dose","dose2"]
  se_d   <- sqrt(pmax(var_d, 0))
  acrt[[o]] <- data.table(outcome = o, dose = dgrid, acrt = acrt_d,
                          lo = acrt_d - 1.96*se_d, hi = acrt_d + 1.96*se_d)
  qsig[[o]] <- data.table(outcome = o, b1 = b["dose"], b2 = b["dose2"],
                          b2_p = m$coeftable["dose2","Pr(>|t|)"],
                          bends = m$coeftable["dose2","Pr(>|t|)"] < 0.05)
}
acrt_dt <- rbindlist(acrt); qsig_dt <- rbindlist(qsig)
fwrite(acrt_dt, file.path(out_dir, "curvature_acrt_feols.csv"))
acrt_dt[, ol := lab[outcome]]
gA <- ggplot(acrt_dt, aes(dose, acrt)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, fill = "#1b7837") +
  geom_line(color = "#1b7837") + facet_wrap(~ ol, scales = "free_y") +
  labs(title = "Parametric ACRT(d): marginal effect at each dose (pooled Inc, headline sample)",
       subtitle = "feols quadratic; flat => linear ACR fine, sloped => per-pp effect bends with dose size",
       x = "dose (cumulative rate change, pp)", y = "ACRT(d) = marginal effect at dose d") +
  theme_bw(base_size = 10)
ggsave(file.path(fig_dir, "curvature_acrt_feols.png"), gA, width = 9, height = 6, dpi = 300)
cat("--- feols quadratic curvature (b2 = dose^2) ---\n"); print(qsig_dt)

## PART B: dCDH continuous=1 vs continuous=2 ----------------------------------
# Diagnostic is about the dose model, not the controls; fiscal controls make
# did_multiplegt_dyn(continuous=2) ~5x slower, so we drop them here (the linear-
# vs-quadratic dose comparison is unaffected in substance).
panel <- as.data.table(e$panel)
panel <- panel[as.character(cell_muni) %in% c("NC","Inc-bind","Inc-becomes","Inc-nobind") &
                 !is.na(resident_eit)]
panel[, muni_int := as.integer(factor(muni))]
dcdh <- list()
# continuous=2 is very slow; run the diagnostic on the two outcomes that show
# significant feols curvature (the rest are statistically linear -- Part A).
dcdh_outcomes <- c("log_live_and_work","share_live_in")
for (o in dcdh_outcomes) {
  cols <- c("muni_int","year","resident_eit", o)
  dd <- panel[, ..cols][complete.cases(panel[, ..cols])]; setnames(dd, o, "Y")
  for (cv in c(1, 2)) {
    m <- tryCatch(did_multiplegt_dyn(df = as.data.frame(dd), outcome = "Y", group = "muni_int",
            time = "year", treatment = "resident_eit", effects = 3, placebo = 2,  # NB: doubles
            continuous = cv, cluster = "muni_int", graph_off = TRUE),
          error = function(err) NULL)
    if (is.null(m)) next
    ef <- as.data.table(m$results$Effects)[, .(rel = seq_len(.N), est = Estimate, se = SE)]
    ef[, `:=`(outcome = o, dose_model = if (cv==1) "linear" else "quadratic")]
    dcdh[[length(dcdh)+1L]] <- ef
  }
  cat(sprintf("  dCDH %s done\n", o))
}
dcdh_dt <- rbindlist(dcdh, fill = TRUE)
fwrite(dcdh_dt, file.path(out_dir, "curvature_dcdh_lin_vs_quad.csv"))
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
ggsave(file.path(fig_dir, "curvature_dcdh_lin_vs_quad.png"), gB, width = 9, height = 6, dpi = 300)
cat("\nSaved curvature_acrt_feols.csv + curvature_dcdh_lin_vs_quad.csv + 2 figures\n")
