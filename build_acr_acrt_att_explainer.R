# build_acr_acrt_att_explainer.R
# Pedagogical figure: ATT(d) vs ACRT(d) vs ACR from ONE pooled-Inc quadratic
# dose-response, per outcome. Left column = ATT(d)=b1*d+b2*d^2 (total effect, the
# curve); right column = ACRT(d)=b1+2*b2*d (marginal effect, the slope of ATT) with
# the linear-model ACR overlaid as a flat line (= the average of the ACRT line).
# Output: figures/acr_acrt_att_explainer.png
#
# Run from the package root:  Rscript scripts/build_acr_acrt_att_explainer.R

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_dir <- file.path(project_dir, "figures")

e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
stk <- as.data.table(e$stk_muni); ctrls <- e$ctrls; cs <- paste(ctrls, collapse = " + ")
stk[, inc := as.integer(as.character(treat_cell) %in% c("Inc-bind","Inc-becomes","Inc-nobind"))]
stk[, dose := fifelse(inc == 1, dose_stack, 0)]; stk[, dose2 := dose^2]
lab <- c(log_live_and_work="Work in own muni", share_live_in="Share live in muni",
         log_total_jobs="Total jobs", log_in_commuters="In-commuters")
outs <- c("log_live_and_work","share_live_in","log_total_jobs","log_in_commuters")
dmax <- quantile(stk[inc == 1 & dose > 0, dose], 0.95, na.rm = TRUE)
dg <- seq(0, dmax, length.out = 60)

ATT <- list(); ACRT <- list(); ACRln <- list()
for (o in outs) {
  cols <- c("muni","dose","dose2","stack_muni","stack_year", o, ctrls)
  d <- stk[, ..cols][complete.cases(stk[, ..cols])]
  mQ <- feols(as.formula(paste0(o," ~ dose + dose2 + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
  mL <- feols(as.formula(paste0(o," ~ dose + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
  b <- coef(mQ)[c("dose","dose2")]; V <- vcov(mQ)[c("dose","dose2"),c("dose","dose2")]
  att  <- b[1]*dg + b[2]*dg^2
  vatt <- dg^2*V[1,1] + dg^4*V[2,2] + 2*dg^3*V[1,2]; satt <- sqrt(pmax(vatt,0))
  acrt <- b[1] + 2*b[2]*dg
  vacr <- V[1,1] + 4*dg^2*V[2,2] + 4*dg*V[1,2]; sacr <- sqrt(pmax(vacr,0))
  ATT[[o]]   <- data.table(outcome=lab[o], dose=dg, y=att,  lo=att-1.96*satt,  hi=att+1.96*satt)
  ACRT[[o]]  <- data.table(outcome=lab[o], dose=dg, y=acrt, lo=acrt-1.96*sacr, hi=acrt+1.96*sacr)
  ACRln[[o]] <- data.table(outcome=lab[o], acr=as.numeric(coef(mL)["dose"]))
}
P_ATT  <- "ATT(d): total effect (the curve)"
P_ACRT <- "ACRT(d): marginal effect (the slope) + ACR (flat avg)"
att_dt  <- rbindlist(ATT)[,  panel := P_ATT]
acrt_dt <- rbindlist(ACRT)[, panel := P_ACRT]
acr_dt  <- rbindlist(ACRln)[, panel := P_ACRT]
both <- rbind(att_dt, acrt_dt)
ord <- as.vector(t(outer(lab[outs], c(P_ATT, P_ACRT), paste, sep=" — ")))
both[,   fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
acr_dt[, fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
acr_dt[, ltxt := sprintf("ACR = %.3f", acr)]
cols2 <- setNames(c("#1b7837","#b35806"), c(P_ATT, P_ACRT))

g <- ggplot(both, aes(dose, y, color=panel, fill=panel)) +
  geom_hline(yintercept=0, linetype="dashed", color="grey60") +
  geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.13, color=NA) + geom_line(linewidth=0.7) +
  geom_hline(data=acr_dt, aes(yintercept=acr), color="#2166ac", linetype="longdash", linewidth=0.6) +
  geom_text(data=acr_dt, aes(x=dmax*0.5, y=acr, label=ltxt), color="#2166ac",
            vjust=-0.5, size=3, inherit.aes=FALSE) +
  facet_wrap(~ fc, scales="free_y", ncol=2) +
  scale_color_manual(values=cols2, guide="none") + scale_fill_manual(values=cols2, guide="none") +
  labs(title="ATT vs ACRT vs ACR (pooled Inc, quadratic dose-response)",
       subtitle=paste0("Left: ATT(d)=b1*d+b2*d^2 (total effect, curves). Right: ACRT(d)=b1+2*b2*d ",
                       "(slope of ATT, straight) with blue ACR = one-number average of ACRT."),
       x="dose (cumulative rate change, pp)", y=NULL) +
  theme_bw(base_size=9) + theme(plot.subtitle=element_text(size=8))
ggsave(file.path(fig_dir, "acr_acrt_att_explainer.png"), g, width=10, height=11, dpi=300)
cat("Saved figures/acr_acrt_att_explainer.png\n")
