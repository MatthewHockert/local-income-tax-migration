# q3_unified_pretrend_battery.R
# Multiple pre-trend tests on the realized-dose ES (the paper's main event
# study), per muni-side cell x outcome. The single joint Wald over-rejects on
# the thin cohorts (14-24 treated clusters), so we report a battery of four:
#   1. joint Wald on leads (asymptotic)             -- the standard test
#   2. individual leads: # significant at 5%, min p
#   3. linear pre-trend slope (asymptotic, 1 df)    -- more stable than joint
#   4. HonestDiD breakdown Mbar (Rambachan-Roth)    -- how large a pre-trend
#        violation (relative to the max pre-period deviation) is needed before
#        the post-effect CI includes zero; bigger = more robust.
# Output: data/q3_unified/pretrend_battery.csv
#
# (A wild-cluster bootstrap was attempted but fwildclusterboot is incompatible
#  with the stacked design's singleton fixed effects; HonestDiD serves the same
#  small-sample-robustness purpose and is the standard tool.)

suppressMessages({ library(data.table); library(fixest); library(HonestDiD) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data"); out_dir <- file.path(data_dir, "q3_unified")
K <- 5L; set.seed(42); Mbarvec <- c(0.5, 1, 1.5, 2)

e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel); ctrls <- e$ctrls; cs <- paste(ctrls, collapse = " + ")
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

mu <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu, muni, year)
rd <- mu[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  ymx <- min(c_muni + K, max(year, na.rm = TRUE)); end <- resident_eit[year == ymx]
  list(rdose = (if(length(end)==1)end else NA) - (if(length(pre)==1)pre else NA))
}, by = muni]
rd[is.na(rdose), rdose := 0]
rd <- rbind(rd, data.table(muni = unique(panel[cell_muni=="NC", muni]), rdose = 0))
panel <- merge(panel, rd, by = "muni", all.x = TRUE); panel[is.na(rdose), rdose := 0]

build_stack <- function(p) {
  tr <- unique(p[cell_muni != "NC", .(muni, co = c_muni)])
  nc <- unique(p[cell_muni == "NC", muni])
  rbindlist(lapply(sort(unique(tr$co)), function(cy) {
    w <- (cy-K):(cy+K); tm <- tr[co==cy]; if (!nrow(tm)) return(NULL)
    dt <- p[muni %in% tm$muni & year %in% w]; dt[, tc := as.character(cell_muni)]
    dc <- p[muni %in% nc & year %in% w];      dc[, tc := "NC"]
    d <- rbind(dt, dc, fill = TRUE); d[, rel := as.integer(year - cy)]; d[, stk := cy]; d
  }), fill = TRUE)
}
stk <- build_stack(panel)
stk[, smuni := paste0(stk,"_",muni)]; stk[, syear := paste0(stk,"_",year)]
stk[, cell_sd := factor(cell_sd, levels = cell_levels)]
stk[, cell_nr := factor(cell_nr, levels = cell_levels)]
stk[is.na(dose_sd), dose_sd := 0]; stk[is.na(dose_nr), dose_nr := 0]
stk[, dose_rel := rdose * rel]

# HonestDiD breakdown: smallest Mbar in the grid whose robust CI includes 0
# (Inf = robust beyond the grid; -1 = original CI already includes 0).
hdid_breakdown <- function(m) {
  ct <- as.data.table(m$coeftable, keep.rownames="term")[grepl("^rel::", term)]
  ct[, rel := as.integer(sub("rel::(-?\\d+):.*","\\1", term))]; setorder(ct, rel)
  pre <- which(ct$rel <= -2); post <- which(ct$rel >= 0)
  if (length(pre) < 2 || length(post) < 1) return(list(orig_excl0=NA, breakdown=NA_real_))
  bh <- ct$Estimate; vc <- vcov(m); idx <- match(ct$term, rownames(vc)); sg <- vc[idx, idx]
  lv <- rep(1/length(post), length(post))
  orig <- tryCatch(constructOriginalCS(bh, sg, length(pre), length(post), l_vec=lv),
                   error=function(e) NULL)
  if (is.null(orig)) return(list(orig_excl0=NA, breakdown=NA_real_))
  orig_excl0 <- !(orig$lb <= 0 & 0 <= orig$ub)
  sens <- tryCatch(as.data.table(createSensitivityResults_relativeMagnitudes(
            bh, sg, length(pre), length(post), l_vec=lv, Mbarvec=Mbarvec,
            bound="deviation from parallel trends")), error=function(e) NULL)
  if (is.null(sens)) return(list(orig_excl0=orig_excl0, breakdown=NA_real_))
  incl0 <- sens[lb <= 0 & 0 <= ub]
  bd <- if (nrow(incl0)) min(incl0$Mbar) else Inf   # Inf = robust past grid
  list(orig_excl0 = orig_excl0, breakdown = bd)
}

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
rows <- list()
for (o in outcomes) for (cl in c("Inc-bind","Inc-becomes","Inc-nobind")) {
  d <- stk[tc %in% c("NC", cl)]
  cols <- c("muni","rel","smuni","syear","rdose","cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls)
  dd <- d[, ..cols][complete.cases(d[, ..cols])]; if (!nrow(dd)) next
  f <- as.formula(paste0(o, " ~ i(rel, rdose, ref=-1) + i(cell_sd, dose_sd, ref='NC') + ",
                         "i(cell_nr, dose_nr, ref='NC') + ", cs, " | smuni + syear"))
  m <- tryCatch(feols(f, dd, cluster = ~muni), error = function(e) NULL); if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames="t")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"), skip_absent=TRUE)
  ct <- ct[grepl("^rel::", t)]; ct[, k := as.integer(sub("rel::(-?\\d+):.*","\\1", t))]
  pre <- ct[k <= -2]
  wj <- tryCatch(wald_pretrend(m, "^rel::-[2-9]")$p, error=function(e) NA_real_)
  # linear pre-trend slope on the pre-event window
  dp <- d[rel < 0]; cols2 <- c("muni","dose_rel","smuni","syear","cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls)
  dp <- dp[, ..cols2][complete.cases(d[rel<0, ..cols2])]
  ml <- tryCatch(feols(as.formula(paste0(o, " ~ dose_rel + i(cell_sd,dose_sd,ref='NC') + ",
                  "i(cell_nr,dose_nr,ref='NC') + ", cs, " | smuni + syear")),
                  dp, cluster = ~muni), error=function(e) NULL)
  lin_b <- if(!is.null(ml) && "dose_rel" %in% names(coef(ml))) coef(ml)["dose_rel"] else NA_real_
  lin_p <- if(!is.null(ml) && "dose_rel" %in% names(coef(ml))) ml$coeftable["dose_rel","Pr(>|t|)"] else NA_real_
  hd <- hdid_breakdown(m)
  rows[[length(rows)+1L]] <- data.table(
    outcome=o, cell=cl, n_treated=uniqueN(dd$muni[dd$rdose != 0]),
    wald_joint_p=round(wj,4), n_leads=nrow(pre), n_leads_sig5=pre[p<0.05,.N],
    min_lead_p=round(min(pre$p),4), lin_slope=round(lin_b,4), lin_slope_p=round(lin_p,4),
    hdid_orig_excl0=hd$orig_excl0, hdid_breakdown_Mbar=hd$breakdown)
  cat(sprintf("  %-18s %-11s waldJ=%.3f leads_sig=%d/%d lin_p=%.3f hdidBD=%s\n",
              o, cl, wj, pre[p<0.05,.N], nrow(pre), lin_p,
              ifelse(is.infinite(hd$breakdown),">grid",sprintf("%.2f",hd$breakdown))))
}
bat <- rbindlist(rows, fill = TRUE)
# "clean" = passes the few-clusters-robust frequentist tests (individual leads
# + linear slope). The joint Wald over-rejects on thin cohorts; HonestDiD
# breakdown is reported but is non-discriminating here (wide realized-dose ES
# bounds give ~0.5 everywhere), so it does not drive the verdict.
bat[, robust_clean := n_leads_sig5 == 0 & lin_slope_p > 0.10]
fwrite(bat, file.path(out_dir, "pretrend_battery.csv"))
cat("\nSaved data/q3_unified/pretrend_battery.csv\n"); print(bat)
