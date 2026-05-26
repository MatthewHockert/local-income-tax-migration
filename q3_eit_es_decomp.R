# q3_eit_es_decomp.R
# EIT realized-dose event study, decomposed into SCALE (log counts) vs
# COMPOSITION (shares) outcomes, measured IDENTICALLY to the property-tax ES
# (q3_proptax_es.R): same estimator (i(rel, realized_dose, ref=-1) with
# muni + year FE), same six outcomes, same per-lead + Wald pre-trend reporting,
# same largest-discretionary-increase event definition, symmetric concurrent
# cross-tax dose control.
#
# Purpose: put the two decompositions side by side to test the asymmetry claim
# -- EIT contamination is COMPOSITION (block-absorbable), property-tax is
# whole-muni RESIDENCE SCALE (not). Only convincing if both halves are measured
# the same way.
#
# Treatment: home_eit = resident_eit + sd_eit (resident's local income-tax rate).
#   event = year of the muni's largest discretionary home_eit INCREASE (>=0.05pp).
#   realized_dose_eit = home_eit_{c+5} - home_eit_{c-1} (constant per muni).
#   concurrent control = mill_dose (effective property rate change since baseline).
#
# Outputs: data/q3_unified/eit_es_decomp_coefs.csv, eit_es_decomp_att.csv

suppressMessages({ library(data.table); library(fixest) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data"); mt_dir <- file.path(data_dir, "multitax")
out_dir  <- file.path(data_dir, "q3_unified"); K_stack <- 5L

e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- e$panel; ctrls <- e$ctrls; ctrls_str <- paste(ctrls, collapse = " + ")
panel[, muni := sprintf("%05d", as.integer(muni))]
panel[, home_eit := resident_eit + sd_eit]

# concurrent property-tax dose (effective rate), cousub-year
burden <- fread(file.path(mt_dir, "pa_property_burden_panel.csv"))
mp <- fread(file.path(mt_dir, "pa_muni_multitax_panel.csv"))[, .(muni_id, year, cousub_fips)]
mp[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
burden <- merge(burden, mp, by = c("muni_id","year"), all.x = TRUE)
eff <- burden[!is.na(cousub_fips), .(eff_rate = mean(eff_property_tax_rate, na.rm=TRUE)),
              by = .(muni = cousub_fips, year)]
panel <- merge(panel, eff, by = c("muni","year"), all.x = TRUE)

# EIT event = largest discretionary home_eit increase (mirror property-tax framing)
setorder(panel, muni, year)
panel[, consec := year == shift(year) + 1L, by = muni]
panel[, d_eit := fifelse(consec, home_eit - shift(home_eit), NA_real_), by = muni]
ev <- panel[d_eit > 0][order(muni, -d_eit), .SD[1], by = muni, .SDcols = c("year","d_eit")]
setnames(ev, c("year","d_eit"), c("c_eit","max_inc"))
ev <- ev[max_inc >= 0.05]
panel <- merge(panel, ev[, .(muni, c_eit)], by = "muni", all.x = TRUE)

rd <- panel[!is.na(c_eit), {
  pre <- home_eit[year == c_eit - 1L]; yrmax <- min(c_eit + K_stack, max(year))
  end <- home_eit[year == yrmax]
  list(realized_dose_eit = (if (length(end)==1) end else NA) - (if (length(pre)==1) pre else NA))
}, by = muni]
rd[is.na(realized_dose_eit), realized_dose_eit := 0]
panel <- merge(panel, rd, by = "muni", all.x = TRUE)
panel[is.na(realized_dose_eit), realized_dose_eit := 0]

panel[, rel := fifelse(!is.na(c_eit), as.integer(year - c_eit), NA_integer_)]
stk <- panel[!is.na(rel) & rel >= -K_stack & rel <= K_stack]
stk[, base_eff := mean(eff_rate[rel < 0], na.rm = TRUE), by = muni]
stk[!is.finite(base_eff), base_eff := 0]
stk[, mill_dose := fifelse(is.finite(eff_rate), eff_rate - base_eff, 0)]
stk[is.na(mill_dose), mill_dose := 0]

cat(sprintf("EIT ES sample: %s rows; %s munis; cohort years:\n",
            format(nrow(stk), big.mark=","), uniqueN(stk$muni)))
print(table(unique(stk[, .(muni, c_eit)])$c_eit))

outcomes <- c("log_live_and_work","log_total_residents","log_total_jobs",
              "log_in_commuters","share_live_in","share_work_outside")
es_results <- list(); att_results <- list()
for (o in outcomes) {
  cols <- c("muni","year","rel","realized_dose_eit","mill_dose", o, ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (!nrow(dd)) next
  f <- as.formula(paste0(o, " ~ i(rel, realized_dose_eit, ref=-1) + mill_dose + ",
                         ctrls_str, " | muni + year"))
  m <- tryCatch(feols(f, data = dd, cluster = ~muni), error=function(ee) NULL)
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames="term")
  ic <- ct[grepl("^rel::", term)]
  ic[, rel := as.integer(sub("rel::(-?\\d+):.*","\\1",term))]
  ic <- ic[, .(rel, est=Estimate, se=`Std. Error`, p=`Pr(>|t|)`)][, `:=`(outcome=o, n=nobs(m))]
  es_results[[length(es_results)+1L]] <- ic
  post <- ic[rel>=0 & rel<=5]; pre <- ic[rel<=-2]
  wp <- tryCatch(wald_pretrend(m, "^rel::-[2-9]"), error=function(e) list(stat=NA,df=NA,p=NA))
  att_results[[length(att_results)+1L]] <- data.table(
    outcome=o, att_post=round(mean(post$est),3), att_pre=round(mean(pre$est),3),
    pre_t=round(mean(pre$est)/sqrt(mean(pre$se^2)/max(nrow(pre),1)),2),
    wald_pre_p=round(wp$p,4),
    kind=fifelse(grepl("^share",o),"composition","scale"), n=nobs(m))
  cat(sprintf("  %-20s post=%7.3f pre=%7.3f pre|t|=%.2f wald_p=%.3f\n",
              o, mean(post$est), mean(pre$est),
              abs(mean(pre$est)/sqrt(mean(pre$se^2)/max(nrow(pre),1))), wp$p))
}
fwrite(rbindlist(es_results, fill=TRUE), file.path(out_dir,"eit_es_decomp_coefs.csv"))
att <- rbindlist(att_results, fill=TRUE)
fwrite(att, file.path(out_dir,"eit_es_decomp_att.csv"))
cat("\n--- EIT realized-dose ES: scale vs composition pre-trend verdict ---\n"); print(att)
