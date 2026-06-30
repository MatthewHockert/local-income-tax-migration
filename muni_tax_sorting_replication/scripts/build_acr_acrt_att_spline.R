# build_acr_acrt_att_spline.R
# Natural-spline ns(dose) dose-response (ATT/ACRT) for the MUNI resident-rate dose,
# in the explainer layout, for THREE groups: the POOLED Increase aggregate, the
# Inc-bind cell, and the Inc-becomes cell. Each is the continuous-dose spline on
# {its treated cells + NC}, controlling for the SD rate changes (i(cell_sd, dose_sd))
# so the muni dose-response is net of SD -- matching the headline spec. The linear ACR
# is drawn as a flat reference and labeled. (Quadratic dropped.)
# Outputs: figures/acr_acrt_att_spline_{pooled,incbind,incbecomes}.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2); library(splines) })
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
fig_dir <- file.path(ROOT, "figures")

e <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
stk <- as.data.table(e$stk_muni); ctrls <- e$ctrls; cs <- paste(ctrls, collapse = " + ")
stk[, inc := as.integer(as.character(treat_cell) %in% c("Inc-bind","Inc-becomes","Inc-nobind"))]
stk[, dose := fifelse(inc == 1, dose_stack, 0)]
lab <- c(log_live_and_work="Work in own muni", share_live_in="Share live in muni",
         log_total_jobs="Total jobs", log_in_commuters="In-commuters")
outs <- c("log_live_and_work","share_live_in","log_total_jobs","log_in_commuters")
dmax <- quantile(stk[inc == 1 & dose > 0, dose], 0.95, na.rm = TRUE)
dg   <- seq(0, dmax, length.out = 80)
SDT  <- "i(cell_sd, dose_sd, ref='NC')"     # SD-side control (net out SD changes, per headline)

fit_group <- function(treated_cells) {
  ATT <- list(); ACRT <- list(); keep <- c(treated_cells, "NC")
  for (o in outs) {
    cols <- c("muni","dose","cell_sd","dose_sd","treat_cell","stack_muni","stack_year", o, ctrls)
    d <- stk[as.character(treat_cell) %in% keep, ..cols]; d <- d[complete.cases(d)]
    kk <- quantile(d[dose > 0, dose], c(1/3, 2/3), na.rm = TRUE)
    bb <- c(0, max(d[dose > 0, dose], na.rm = TRUE))
    B  <- ns(d$dose, knots = kk, Boundary.knots = bb); bn <- paste0("sp", seq_len(ncol(B)))
    d[, (bn) := as.data.table(unclass(B))]
    fS <- feols(as.formula(paste0(o," ~ ",paste(bn,collapse=" + ")," + ",SDT," + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
    fL <- feols(as.formula(paste0(o," ~ dose + ",SDT," + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
    csp <- coef(fS)[bn]; Vsp <- vcov(fS)[bn, bn]; acr <- as.numeric(coef(fL)["dose"])
    Bg <- predict(B, dg); B0 <- as.numeric(predict(B, 0)); G <- sweep(Bg, 2, B0)
    att_s <- as.numeric(G %*% csp); se_s <- sqrt(pmax(rowSums((G %*% Vsp) * G), 0))
    h <- 1e-4; Bp <- (predict(B, dg + h) - predict(B, dg - h)) / (2*h)
    acrt_s <- as.numeric(Bp %*% csp); se_a <- sqrt(pmax(rowSums((Bp %*% Vsp) * Bp), 0))
    ATT[[o]]  <- data.table(outcome=lab[o], dose=dg, y=att_s,  lo=att_s-1.96*se_s,  hi=att_s+1.96*se_s)
    ACRT[[o]] <- data.table(outcome=lab[o], dose=dg, y=acrt_s, lo=acrt_s-1.96*se_a, hi=acrt_s+1.96*se_a, acr=acr)
  }
  list(ATT = rbindlist(ATT), ACRT = rbindlist(ACRT))
}

plot_group <- function(res, ttl, outfile) {
  P_ATT  <- "ATT(d): total effect"
  P_ACRT <- "ACRT(d): marginal effect + ACR"
  both <- rbind(res$ATT[, .(outcome,dose,y,lo,hi)][, panel := P_ATT],
                res$ACRT[, .(outcome,dose,y,lo,hi)][, panel := P_ACRT])
  ord <- as.vector(t(outer(lab[outs], c(P_ATT, P_ACRT), paste, sep=" — ")))
  both[, fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
  acr <- unique(res$ACRT[, .(outcome, acr)])[, panel := P_ACRT]
  acr[, fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
  acr[, ltxt := sprintf("ACR = %.3f", acr)]
  g <- ggplot(both, aes(dose, y)) +
    geom_hline(yintercept=0, linetype="dotted", color="grey70") +
    geom_ribbon(aes(ymin=lo, ymax=hi), fill="#1b7837", alpha=0.13, color=NA) +
    geom_line(color="#1b7837", linewidth=0.7) +
    geom_hline(data=acr, aes(yintercept=acr), color="#2166ac", linetype="longdash", linewidth=0.5) +
    geom_text(data=acr, aes(x=dmax*0.45, y=acr, label=ltxt), color="#2166ac", vjust=-0.5, size=2.6, inherit.aes=FALSE) +
    facet_wrap(~ fc, scales="free_y", ncol=2) +
    labs(title=ttl,
         subtitle="Natural-spline ns(dose), net of SD rate changes. Left: ATT(d). Right: ACRT(d)=dATT/dd; blue dashed = linear ACR.",
         x="dose (cumulative muni rate change, pp)", y=NULL) +
    theme_bw(base_size=9) + theme(plot.subtitle=element_text(size=8))
  ggsave(file.path(fig_dir, outfile), g, width=10, height=11, dpi=300)
  cat(sprintf("Saved figures/%s\n", outfile))
}

plot_group(fit_group(c("Inc-bind","Inc-becomes","Inc-nobind")), "Muni dose-response (spline) — POOLED Increase",  "acr_acrt_att_spline_pooled.png")
plot_group(fit_group("Inc-bind"),    "Muni dose-response (spline) — Inc-bind cell",    "acr_acrt_att_spline_incbind.png")
plot_group(fit_group("Inc-becomes"), "Muni dose-response (spline) — Inc-becomes cell", "acr_acrt_att_spline_incbecomes.png")
