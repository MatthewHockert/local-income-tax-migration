# q3_proptax_es.R
# PROPERTY-TAX LEG: realized-dose Sun-Abraham continuous event study (the
# make-or-break identification check; built BEFORE the headline per the
# 2026-05-24 decision). Mirror of q3_unified_headline_es.R, with the treatment
# replaced by the POLICY-ONLY property-tax dose.
#
# Treatment (policy-only, CLR-drift stripped; reassessment rollbacks handled):
#   total statutory mills_it = muni + SD + county RE millage (cousub-year)
#   discretionary increment:
#     non-reassessment year: d_mills_it = mills_it - mills_{i,t-1}
#     reassessment year:     d_mills_it = mills_it - mills_{i,t-1} * CLR_{t-1}/CLR_t
#                            (= deviation from revenue-neutral; 0 if pure rollback)
#   cum_disc_it = cumsum(d_mills); event = first |d_mills|>=0.5; cohort c.
#   realized policy dose_i = CLR_{c-1} * (cum_disc_{c+5} - cum_disc_{c-1}) / 1000
#                            (constant per muni; 0 for NC).
# Concurrent-EIT dose control: home_eit = resident_eit + sd_eit, entered as the
#   cumulative change since c-1 (dose form, not level).
#
# ES spec (subset {NC, Inc}, window +/-5):
#   y_it = a_{stack,muni} + g_{stack,year}
#        + sum_{k != -1} beta_k * 1{rel=k} * realized_dose_P_i
#        + lambda * eit_dose_it + X_it phi + e_it ,  cluster ~ muni
# Pre-period betas (k<=-2) must be ~0: no within-design placebo exists for this
# leg, so this pre-trend test is the sole identification check.
#
# Outputs: data/q3_unified/proptax_es_coefs.csv, proptax_es_att.csv

suppressMessages({ library(data.table); library(fixest) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
mt_dir   <- file.path(data_dir, "multitax")
out_dir  <- file.path(data_dir, "q3_unified")
K_stack  <- 5L

# --- reuse EIT cousub panel (outcomes + AFR controls + EIT rates) ----------
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- e$panel              # muni = cousub_fips
ctrls <- e$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

# --- property-tax treatment, cousub-year -----------------------------------
burden <- fread(file.path(mt_dir, "pa_property_burden_panel.csv"))
mp <- fread(file.path(mt_dir, "pa_muni_multitax_panel.csv"))[, .(muni_id, year, cousub_fips)]
mp[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
burden <- merge(burden, mp, by = c("muni_id", "year"), all.x = TRUE)
burden <- burden[!is.na(cousub_fips) & !is.na(total_statutory_mills) & !is.na(clr)]
# cousub-year: average across muni_ids mapping to the same cousub
bt <- burden[, .(mills = mean(total_statutory_mills, na.rm = TRUE),
                 clr   = mean(clr, na.rm = TRUE),
                 county = first(county)), by = .(muni = cousub_fips, year)]
setorder(bt, muni, year)

# reassessment years per county (CLR-footnote dates -- the precise record)
reass <- fread(file.path(mt_dir, "pa_reassessment_from_clr.csv"))
reass[, county := toupper(trimws(county))]
bt[, county := toupper(trimws(county))]
bt[, reassess := paste(county, year) %in% paste(reass$county, reass$reassessment_year)]

# discretionary millage increment with explicit reassessment handling
bt[, lag_mills := shift(mills), by = muni]
bt[, lag_clr   := shift(clr),   by = muni]
bt[, consec    := year == shift(year) + 1L, by = muni]
bt[, d_disc := fifelse(!consec | is.na(lag_mills), NA_real_,
               fifelse(reassess,
                       mills - lag_mills * (lag_clr / clr),   # deviation from revenue-neutral
                       mills - lag_mills))]                    # discretionary policy move
bt[is.na(d_disc), d_disc := 0]
bt[, cum_disc := cumsum(d_disc), by = muni]

# event = year of the muni's LARGEST discretionary millage INCREASE (the salient
# policy action). First-material-change is degenerate (property millage churns
# every year, so "first change" is always 2003 with no pre-period); the largest
# discretionary hike spreads cohorts across years and restores a pre-window.
ev <- bt[d_disc > 0][order(muni, -d_disc), .SD[1], by = muni,
                     .SDcols = c("year", "d_disc")]
setnames(ev, c("year", "d_disc"), c("c_prop", "first_disc"))
ev <- ev[first_disc >= 0.5]            # material increase
ev[, dir := "Inc"]
bt <- merge(bt, ev, by = "muni", all.x = TRUE)

# munis with no material discretionary INCREASE -> low/zero-dose comparison
nc_munis <- setdiff(unique(bt$muni), unique(ev$muni))

# realized policy dose at +5 (constant per muni): CLR_{c-1} * cum_disc change /1000
rd <- bt[!is.na(c_prop), {
  base_clr <- clr[year == c_prop - 1L]
  cum_b    <- cum_disc[year == c_prop - 1L]
  yr_max   <- min(c_prop + K_stack, max(year))
  cum_e    <- cum_disc[year == yr_max]
  bc <- if (length(base_clr) == 1) base_clr else NA_real_
  cb <- if (length(cum_b) == 1) cum_b else NA_real_
  ce <- if (length(cum_e) == 1) cum_e else NA_real_
  list(realized_dose_P = bc * (ce - cb) / 1000)
}, by = muni]
rd[is.na(realized_dose_P), realized_dose_P := 0]
rd <- rbind(rd, data.table(muni = nc_munis, realized_dose_P = 0))

# home-EIT level for the concurrent-EIT dose control
bt2 <- bt[, .(muni, year, c_prop, dir, home_eit = NA_real_)]

# --- merge treatment onto the EIT outcome panel ----------------------------
panel[, muni := sprintf("%05d", as.integer(muni))]
panel[, home_eit := resident_eit + sd_eit]
prop <- bt[, .(muni, year, c_prop, dir)]
prop <- unique(prop, by = c("muni", "year"))
P <- merge(panel, prop, by = c("muni", "year"), all.x = TRUE)
P <- merge(P, rd, by = "muni", all.x = TRUE)
P[is.na(realized_dose_P), realized_dose_P := 0]
# property cell: Inc treated vs NC; drop Dec (paper scope Inc)
P[, prop_cell := fifelse(muni %in% nc_munis, "NC",
                  fifelse(!is.na(dir) & dir == "Inc", "Inc", NA_character_))]
# cohort per muni (first event year)
coh <- unique(P[!is.na(c_prop), .(muni, cohort = c_prop)])
coh <- coh[, .(cohort = min(cohort)), by = muni]
P <- merge(P, coh, by = "muni", all.x = TRUE)

# --- continuous realized-dose ES (muni + year FE) --------------------------
# NOTE: property millage changes are high-frequency (SD Act-1 index churn), so
# there is NO never-treated control group (NC = 0). Identification therefore
# comes from DOSE MAGNITUDE, not a never-treated pool: realized_dose_P x event
# time with muni + year FE ("strong parallel trends across dose magnitudes",
# Callaway-Goodman-Bacon-Sant'Anna 2024). Low-dose munis anchor high-dose ones.
P[, rel := fifelse(!is.na(c_prop), as.integer(year - c_prop), NA_integer_)]
stk <- P[prop_cell == "Inc" & !is.na(rel) & rel >= -K_stack & rel <= K_stack]
# concurrent-EIT dose: cumulative home_eit change since own event baseline (c-1)
stk[, base_eit := mean(home_eit[rel < 0], na.rm = TRUE), by = muni]
stk[!is.finite(base_eit), base_eit := 0]
stk[, eit_dose := fifelse(is.finite(home_eit), home_eit - base_eit, 0)]
stk[is.na(eit_dose), eit_dose := 0]
stk[, stack_muni := muni]; stk[, stack_year := year]

cat(sprintf("\nProperty-tax ES sample: %s rows; %s munis; cohort (first-event) years:\n",
            format(nrow(stk), big.mark=","), uniqueN(stk$muni)))
print(table(unique(stk[, .(muni, c_prop)])$c_prop))
cat(sprintf("realized policy dose (pp of mkt value): median %.3f, p90 %.3f\n",
            median(unique(stk[,.(muni,realized_dose_P)])$realized_dose_P, na.rm=TRUE),
            quantile(unique(stk[,.(muni,realized_dose_P)])$realized_dose_P, .9, na.rm=TRUE)))

# --- realized-dose ES per outcome ------------------------------------------
# scale outcomes (whole-muni totals) vs composition outcomes (within-muni shares)
# -- to locate the pre-trend contamination: muni-scale trajectory vs composition.
outcomes <- c("log_live_and_work", "log_total_residents", "log_total_jobs",
              "log_in_commuters", "share_live_in", "share_work_outside")
es_results <- list(); att_results <- list()
for (o in outcomes) {
  cols <- c("muni","year","stack_muni","stack_year","rel",
            "realized_dose_P","eit_dose", o, ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (!nrow(dd)) { cat("skip", o, "\n"); next }
  f <- as.formula(paste0(o, " ~ i(rel, realized_dose_P, ref = -1) + eit_dose + ",
                         ctrls_str, " | stack_muni + stack_year"))
  m <- tryCatch(feols(f, data = dd, cluster = ~muni),
                error = function(ee){cat("err",o,conditionMessage(ee),"\n");NULL})
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("^rel::", term)]
  ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
  ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
  ic[, outcome := o]; ic[, n := nobs(m)]
  es_results[[length(es_results)+1L]] <- ic
  post <- ic[rel >= 0 & rel <= 5]; pre <- ic[rel <= -2]
  wp <- tryCatch(wald_pretrend(m, "^rel::-[2-9]"), error=function(e) list(stat=NA,df=NA,p=NA))
  att_results[[length(att_results)+1L]] <- data.table(
    outcome=o, att_post=round(mean(post$est),3),
    att_post_se=round(sqrt(mean(post$se^2)/max(nrow(post),1)),3),
    att_pre=round(mean(pre$est),3),
    att_pre_se=round(sqrt(mean(pre$se^2)/max(nrow(pre),1)),3),
    pre_t=round(mean(pre$est)/sqrt(mean(pre$se^2)/max(nrow(pre),1)),2),
    wald_pre_p=round(wp$p,4), n=nobs(m))
  cat(sprintf("  %-20s post=%6.3f (SE %.3f)  pre=%6.3f (SE %.3f) pre|t|=%.2f  N=%s\n",
              o, mean(post$est), sqrt(mean(post$se^2)/max(nrow(post),1)),
              mean(pre$est), sqrt(mean(pre$se^2)/max(nrow(pre),1)),
              abs(mean(pre$est)/sqrt(mean(pre$se^2)/max(nrow(pre),1))),
              format(nobs(m), big.mark=",")))
}
es_dt <- rbindlist(es_results, fill=TRUE)
fwrite(es_dt, file.path(out_dir, "proptax_es_coefs.csv"))
att <- rbindlist(att_results, fill=TRUE)
fwrite(att, file.path(out_dir, "proptax_es_att.csv"))

# --- event-study plots (scale vs composition panels) ----------------------
suppressMessages(library(ggplot2))
es_p <- rbind(es_dt, data.table(rel=-1L, est=0, se=0, p=NA,
              outcome=unique(es_dt$outcome), n=NA), fill=TRUE)
es_p[, `:=`(lo=est-1.96*se, hi=est+1.96*se)]
es_p[, kind := fifelse(grepl("^share", outcome), "composition (share)", "scale (log count)")]
es_p[, outcome := factor(outcome, levels=outcomes)]
g <- ggplot(es_p, aes(rel, est)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
  geom_vline(xintercept=-0.5, linetype="dotted", colour="grey60") +
  geom_pointrange(aes(ymin=lo, ymax=hi), size=.3) + geom_line(alpha=.5) +
  facet_wrap(~outcome, scales="free_y", ncol=3) +
  labs(title="Property-tax realized-dose ES (per unit policy dose, ref rel=-1)",
       subtitle="Scale (log counts) vs composition (shares); pre-leads test parallel trends",
       x="event time (yrs from largest discretionary millage increase)",
       y=expression(beta[k])) + theme_bw(base_size=9)
ggsave(file.path(project_dir,"figures","proptax_es_scale_vs_comp.png"), g,
       width=10, height=5.5, dpi=300)

cat("\n--- PROPERTY-TAX realized-dose ES: pre-trend verdict ---\n"); print(att)
cat("\nScale outcomes (log counts) vs composition (shares):\n")
print(att[, .(outcome, att_pre, pre_t, wald_pre_p,
              kind=fifelse(grepl("^share",outcome),"composition","scale"))])
cat("\nClean pre-trend (identification holds) if pre|t| <~1.5 and Wald p not small.\n")
