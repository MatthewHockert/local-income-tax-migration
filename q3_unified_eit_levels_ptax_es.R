# q3_unified_eit_levels_ptax_es.R
# Realized-dose Sun-Abraham ES for the EIT decomposition under the NEW control
# set (drop share_eit + share_re; add city/SD/county property-tax effective-rate
# levels; keep police/streets/debt/intergov). Pre-trend diagnostic for the model
# in q3_unified_eit_levels_ptax_controls.R. Inc-bind (causal) + Inc-nobind (placebo).
#
# Output: data/q3_unified/eit_levels_ptax_es_coefs.csv, ..._att.csv,
#         figures/eit_levels_ptax_es.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data"); mt_dir <- file.path(data_dir, "multitax")
out_dir  <- file.path(data_dir, "q3_unified"); fig_dir <- file.path(project_dir, "figures")
K_stack <- 5L

e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

# property-tax effective-rate levels per cousub-year
b  <- fread(file.path(mt_dir, "pa_property_burden_panel.csv"))
mp <- fread(file.path(mt_dir, "pa_muni_multitax_panel.csv"))[, .(muni_id, year, cousub_fips)]
mp[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
b <- merge(b, mp, by = c("muni_id","year"), all.x = TRUE)
ptax <- b[!is.na(cousub_fips), .(ptax_muni = mean(muni_eff_rate, na.rm=TRUE),
                                 ptax_sd   = mean(sd_eff_rate, na.rm=TRUE),
                                 ptax_cty  = mean(county_eff_rate, na.rm=TRUE)),
          by = .(muni = cousub_fips, year)]
for (c in c("ptax_muni","ptax_sd","ptax_cty")) ptax[!is.finite(get(c)), (c) := NA_real_]
panel <- merge(panel, ptax, by = c("muni","year"), all.x = TRUE)

new_ctrls <- c("share_police","share_streets","share_debt_svc","share_intergov",
               "ptax_muni","ptax_sd","ptax_cty")
ctrls_str <- paste(new_ctrls, collapse = " + ")

# realized muni dose (cumulative Delta tau c-1 -> c+5), constant per muni
mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu_year, muni, year)
rd <- mu_year[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  ymx <- min(c_muni + K_stack, max(year, na.rm = TRUE))
  end <- resident_eit[year == ymx]
  list(realized_dose_muni = (if(length(end)==1)end else NA) - (if(length(pre)==1)pre else NA))
}, by = muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
rd <- rbind(rd, data.table(muni = unique(panel[cell_muni=="NC",muni]), realized_dose_muni = 0))
panel <- merge(panel, rd, by = "muni", all.x = TRUE)
panel[is.na(realized_dose_muni), realized_dose_muni := 0]

cell_levels <- levels(panel$cell_muni)
build_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni, cell = cell_muni)])
  nc <- unique(p[cell_muni == "NC", muni])
  rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy-K):(cy+K); tm <- treated[cohort==cy]
    if (!nrow(tm)) return(NULL)
    d_t <- p[muni %in% tm$muni & year %in% win]; d_t[, tcell := as.character(cell_muni)]
    d_c <- p[muni %in% nc & year %in% win];      d_c[, tcell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE); d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
}
stk <- build_stack(panel)
stk[, stack_muni := paste0(stack,"_",muni)]; stk[, stack_year := paste0(stack,"_",year)]
stk[, cell_sd := factor(cell_sd, levels = cell_levels)]
stk[, cell_nr := factor(cell_nr, levels = cell_levels)]
stk[is.na(dose_sd), dose_sd := 0]; stk[is.na(dose_nr), dose_nr := 0]

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
es_results <- list(); att_results <- list()
for (o in outcomes) {
  for (cl in c("Inc-bind","Inc-nobind")) {
    dd <- stk[tcell %in% c("NC", cl)]
    cols <- c("muni","stack_muni","stack_year","tcell","realized_dose_muni","rel",
              "cell_sd","dose_sd","cell_nr","dose_nr", o, new_ctrls)
    dd <- dd[, ..cols][complete.cases(dd[, ..cols])]
    if (!nrow(dd) || !sum(dd$tcell==cl)) next
    f <- as.formula(paste0(o," ~ i(rel, realized_dose_muni, ref=-1) + ",
         "i(cell_sd, dose_sd, ref='NC') + i(cell_nr, dose_nr, ref='NC') + ",
         ctrls_str, " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data=dd, cluster=~muni), error=function(e) NULL)
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames="term")[grepl("^rel::",term)]
    ct[, rel := as.integer(sub("rel::(-?\\d+):.*","\\1",term))]
    ic <- ct[, .(rel, est=Estimate, se=`Std. Error`, p=`Pr(>|t|)`, outcome=o, cell=cl)]
    es_results[[length(es_results)+1L]] <- ic
    post <- ic[rel>=0]; pre <- ic[rel<=-2]; wp <- wald_pretrend(m, "^rel::-[2-9]")
    att_results[[length(att_results)+1L]] <- data.table(outcome=o, cell=cl,
      att_post=round(mean(post$est),3), att_pre=round(mean(pre$est),3),
      wald_pre_p=round(wp$p,4), n=nobs(m))
    cat(sprintf("  %-18s %-11s post=%+.3f pre=%+.3f  wald_pre_p=%.3f\n",
                o, cl, mean(post$est), mean(pre$est), wp$p))
  }
}
es_dt <- rbindlist(es_results, fill=TRUE); att_dt <- rbindlist(att_results, fill=TRUE)
fwrite(es_dt, file.path(out_dir,"eit_levels_ptax_es_coefs.csv"))
fwrite(att_dt, file.path(out_dir,"eit_levels_ptax_es_att.csv"))

# figure: Inc-bind vs Inc-nobind, faceted by outcome
es_p <- rbind(es_dt, unique(es_dt[,.(outcome,cell)])[,.(outcome,cell,rel=-1L,est=0,se=0,p=NA)], fill=TRUE)
es_p[, `:=`(lo=est-1.96*se, hi=est+1.96*se)]
lab <- c(log_live_and_work="Log(work in own muni)", log_in_commuters="Log(in-commuters)",
         log_total_jobs="Log(total jobs)", share_live_in="Share live in muni")
es_p[, outcome_lab := lab[outcome]]
pg <- ggplot(es_p, aes(rel, est, color=cell, fill=cell)) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  geom_vline(xintercept=-0.5, linetype="dotted", color="grey60") +
  geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.15, color=NA) +
  geom_line() + geom_point(size=1) +
  facet_wrap(~outcome_lab, scales="free_y") +
  scale_color_manual(values=c("Inc-bind"="#b2182b","Inc-nobind"="#2166ac")) +
  scale_fill_manual(values=c("Inc-bind"="#b2182b","Inc-nobind"="#2166ac")) +
  labs(title="EIT realized-dose ES (levels spec, property-tax-level controls)",
       subtitle="Per-pp effect at event time k; k=-1 omitted. Controls: police/streets/debt/intergov shares + city/SD/county property-tax eff levels",
       x="Event time (years from first muni rate change)", y=expression(beta[k]),
       color="Cell", fill="Cell") +
  theme_bw(base_size=10) + theme(legend.position="bottom")
ggsave(file.path(fig_dir,"eit_levels_ptax_es.png"), pg, width=9, height=6, dpi=300)
cat("\nSaved figures/eit_levels_ptax_es.png\n")
print(att_dt)
