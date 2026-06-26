# q3_proptax_compare.R
# Common-framework, MUNICIPALITY-LEVEL comparison of SEPARATE estimates
# (NOT a joint "identify both in one model" spec -- the property-tax residence
# margin is contaminated and cannot be co-identified; see PROPERTY_TAX_SPEC.md
# section 11). Assembles:
#   (a) EIT Inc-bind elasticity (clean) -- read from canonical headline table.
#   (b) Property-tax CLEAN margins (composition share_live_in; workplace
#       log_total_jobs, log_in_commuters) -- static dose-response point estimates.
#   (c) Property-tax RESIDENCE counts (log_live_and_work, log_total_residents)
#       -- Rambachan-Roth honest-DiD bounds, NOT point estimates.
#
# Property-tax dose is per unit effective rate (fraction of market value);
# per-pp = beta * 0.01. EIT is per pp of income. Bases differ (see spec section 0).
#
# Output: data/q3_unified/proptax_eit_comparison.csv

suppressMessages({ library(data.table); library(fixest); library(HonestDiD) })
# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
out_dir <- file.path(project_dir, "data", "q3_unified")

# reuse the property-tax ES sample/treatment (also (re)writes ES outputs)
e2 <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_proptax_es.R"), envir = e2)
stk <- e2$stk; ctrls <- e2$ctrls; ctrls_str <- paste(ctrls, collapse = " + ")
stk[, post := as.integer(rel >= 0)]
stk[, dose_post := realized_dose_P * post]

clean_margins <- c("share_live_in", "log_total_jobs", "log_in_commuters")
residence_cts <- c("log_live_and_work", "log_total_residents")

# Per-muni dollar basis from ACS (real, not hand-picked); see
# multitax_acs_dollar_base.py. EIT 1pp on earned income; property 1pp effective
# on market value. per-$1000 = (per-pp coef) * 1e5 / base. Use the estimation
# munis; per-muni panel-mean base; sample median for central conversion, p10/p90
# (and the home/income ratio spread) for the cross-muni sensitivity band.
acs <- fread(file.path(project_dir, "data", "multitax", "pa_acs_dollar_base.csv"))
acs[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
acsm <- acs[, .(medinc = mean(medinc, na.rm = TRUE),
                medhome = mean(medhome, na.rm = TRUE)), by = cousub_fips]
bdf <- acsm[cousub_fips %in% unique(stk$muni) & is.finite(medinc) & is.finite(medhome)]
MED_INCOME <- median(bdf$medinc); MED_HOME <- median(bdf$medhome)
ratio <- bdf$medhome / bdf$medinc
RATIO_P10 <- quantile(ratio, .1); RATIO_MED <- median(ratio); RATIO_P90 <- quantile(ratio, .9)
cat(sprintf("\nACS dollar basis (n=%d estimation munis): income median $%s; home median $%s;\n  home/income ratio  p10 %.2f / median %.2f / p90 %.2f\n",
    nrow(bdf), format(round(MED_INCOME), big.mark=","), format(round(MED_HOME), big.mark=","),
    RATIO_P10, RATIO_MED, RATIO_P90))

# (b) static dose-response point estimates (per unit dose) + 95% CI for ALL outcomes
static <- list()
for (o in c(clean_margins, residence_cts)) {
  cols <- c("muni","year","dose_post","eit_dose", o, ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  f <- as.formula(paste0(o, " ~ dose_post + eit_dose + ", ctrls_str, " | muni + year"))
  m <- feols(f, dd, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")[term == "dose_post"]
  pp  <- ct$Estimate * 0.01; pp_se <- ct$`Std. Error` * 0.01
  static[[o]] <- data.table(outcome = o, prop_per_pp = pp, prop_pp_se = pp_se,
                            prop_p = ct$`Pr(>|t|)`,
                            prop_pp_lo = pp - 1.96*pp_se, prop_pp_hi = pp + 1.96*pp_se)
}
static <- rbindlist(static)

# (c) honest bounds on the contaminated residence-count outcomes
honest <- list()
for (o in residence_cts) {
  cols <- c("muni","year","rel","realized_dose_P","eit_dose", o, ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  f <- as.formula(paste0(o, " ~ i(rel, realized_dose_P, ref=-1) + eit_dose + ",
                         ctrls_str, " | muni + year"))
  m <- feols(f, dd, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ct <- ct[grepl("^rel::", term)]
  ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
  setorder(ct, rel)
  pre_i  <- which(ct$rel <= -2); post_i <- which(ct$rel >= 0)
  ord <- c(pre_i, post_i)
  betahat <- ct$Estimate[ord]
  vc <- vcov(m); idx <- match(ct$term[ord], colnames(vc))
  sigma <- as.matrix(vc[idx, idx])
  nPre <- length(pre_i); nPost <- length(post_i); l_vec <- rep(1/nPost, nPost)
  orig <- tryCatch(constructOriginalCS(betahat = betahat, sigma = sigma,
            numPrePeriods = nPre, numPostPeriods = nPost, l_vec = l_vec),
            error = function(e) NULL)
  sens <- tryCatch(createSensitivityResults_relativeMagnitudes(
            betahat = betahat, sigma = sigma, numPrePeriods = nPre,
            numPostPeriods = nPost, l_vec = l_vec, Mbarvec = c(0.5, 1)),
            error = function(e) { cat("  HonestDiD err:", conditionMessage(e), "\n"); NULL })
  if (!is.null(sens)) sens <- as.data.frame(sens)
  beta_post <- sum(l_vec * betahat[(nPre+1):(nPre+nPost)])
  rb <- if (!is.null(sens)) sens[sens$Mbar == 1, ] else NULL
  # honest bounds are per UNIT of realized policy dose (fraction of market value);
  # convert to per-$1,000-of-tax (same units as the comparison table):
  # per_$1k = per_unit * 0.01 (->per pp) * 1e5/MED_HOME (->per $1k) = per_unit * 1000/MED_HOME
  to1k <- 1000 / MED_HOME
  honest[[o]] <- data.table(outcome = o,
    prop_post_per_unit = round(beta_post, 3),
    orig_lb = if (!is.null(orig)) round(orig$lb,3) else NA,
    orig_ub = if (!is.null(orig)) round(orig$ub,3) else NA,
    robust_lb_Mbar1 = if (!is.null(rb)) round(rb$lb,3) else NA,
    robust_ub_Mbar1 = if (!is.null(rb)) round(rb$ub,3) else NA,
    robust_lb_per_1k = if (!is.null(rb)) round(rb$lb * to1k, 4) else NA,
    robust_ub_per_1k = if (!is.null(rb)) round(rb$ub * to1k, 4) else NA,
    robust_includes_0 = if (!is.null(rb)) (rb$lb <= 0 & rb$ub >= 0) else NA)
}
honest <- rbindlist(honest)

# (a) EIT Inc-bind canonical (per pp income)
eit <- fread(file.path(out_dir, "headline_full_table.csv"))
eit <- eit[grepl("Inc-bind:dose_stack", term),
           .(outcome, eit_incbind = round(est,3), eit_se = round(se,3))]

cmp <- merge(static, eit, by = "outcome", all.x = TRUE)
cmp[, margin := fifelse(outcome %in% residence_cts, "residence (honest bounds)",
                 fifelse(grepl("^share", outcome), "composition (clean)",
                         "workplace (clean)"))]
cmp <- merge(cmp, honest, by = "outcome", all.x = TRUE)

# common DOLLAR basis: % change per $1,000 of annual tax bill.
# per-$1000 = (per-pp coef) * 1e5 / base; base = income (EIT) or home value (prop).
cmp[, eit_per_1k     := eit_incbind * 1e5 / MED_INCOME]
cmp[, prop_per_1k    := prop_per_pp * 1e5 / MED_HOME]
cmp[, prop_per_1k_lo := prop_pp_lo  * 1e5 / MED_HOME]
cmp[, prop_per_1k_hi := prop_pp_hi  * 1e5 / MED_HOME]
# Does the property 95% CI (per $1k) EXCLUDE the EIT effect? (clean null vs underpowered)
cmp[, CI_excludes_EIT := eit_per_1k < prop_per_1k_lo | eit_per_1k > prop_per_1k_hi]
fwrite(cmp, file.path(out_dir, "proptax_eit_comparison.csv"))

cat("\n==== MUNICIPALITY-LEVEL: EIT vs PROPERTY TAX (separate estimates) ====\n")
cat(sprintf("Dollar basis: EIT 1pp on median income $%s; property 1pp eff. on median home $%s.\n\n",
            format(MED_INCOME, big.mark=","), format(MED_HOME, big.mark=",")))
cat("Per pp of own base (EIT=income, Prop=market value), with property 95% CI:\n")
print(cmp[, .(outcome, margin, EIT_per_pp = eit_incbind, Prop_per_pp = round(prop_per_pp,4),
              Prop95 = sprintf("[%.4f, %.4f]", prop_pp_lo, prop_pp_hi), Prop_p = round(prop_p,3))])

cat("\nCommon DOLLAR basis -- % change per $1,000 of annual tax bill:\n")
print(cmp[, .(outcome, margin, EIT_per_1k = round(eit_per_1k,3), Prop_per_1k = round(prop_per_1k,4),
              Prop95_per_1k = sprintf("[%.4f, %.4f]", prop_per_1k_lo, prop_per_1k_hi),
              CI_excl_EIT = CI_excludes_EIT)])

cat("\n--- CLEAN-NULL vs UNDERPOWERED (clean margins, per $1k basis) ---\n")
for (o in clean_margins) {
  r <- cmp[outcome == o]
  cat(sprintf("  %-18s prop 95%% CI [%.4f, %.4f]; EIT=%.3f -> CI %s EIT effect\n",
      o, r$prop_per_1k_lo, r$prop_per_1k_hi, r$eit_per_1k,
      ifelse(r$CI_excludes_EIT, "EXCLUDES (clean null)", "INCLUDES (underpowered)")))
}
cat("\n--- Sensitivity band: EIT-vs-property per-$ multiple across cross-muni home/income ratio ---\n")
cat(sprintf("(home/income ratio p10 %.2f / median %.2f / p90 %.2f; the multiple scales with this ratio)\n",
            RATIO_P10, RATIO_MED, RATIO_P90))
for (o in clean_margins) {
  r <- cmp[outcome == o]
  mult_med <- abs(r$eit_per_1k / r$prop_per_1k)
  cat(sprintf("  %-18s EIT moves ~%.0fx more per $ (median ratio); band ~%.0fx-%.0fx across p10-p90 ratio\n",
      o, mult_med, mult_med * RATIO_P10 / RATIO_MED, mult_med * RATIO_P90 / RATIO_MED))
}

cat("\n--- Property-tax residence counts: honest-DiD bounds (Mbar=1) ---\n")
hh <- merge(honest, cmp[, .(outcome, eit_per_1k)], by = "outcome")
print(hh[, .(outcome,
             honest_per_unit = sprintf("[%.1f, %.1f]", robust_lb_Mbar1, robust_ub_Mbar1),
             honest_per_1k   = sprintf("[%.3f, %.3f]", robust_lb_per_1k, robust_ub_per_1k),
             EIT_per_1k = round(eit_per_1k, 3),
             excl_EIT = robust_ub_per_1k < eit_per_1k | robust_lb_per_1k > eit_per_1k,
             incl_0 = robust_includes_0)])
