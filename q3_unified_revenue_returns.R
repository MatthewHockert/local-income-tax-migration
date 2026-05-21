# q3_unified_revenue_returns.R
# Run the muni-side binding decomposition (Inc-bind/becomes/nobind vs NC) with a
# REVENUE outcome, for two revenue sources, on the IDENTICAL model:
#   (1) log EIT revenue       -- drop share_eit from controls (it IS the outcome)
#   (2) log property-tax rev  -- drop share_re  from controls; keep share_eit
# (2) is the substitution check: if cities that raise the EIT rate (Inc-bind) get
# a real bump in total revenue rather than offsetting other taxes, property-tax
# revenue should NOT fall when the EIT rate rises (flat/positive => additive).
#
# For each source we report, per cell:
#   * net per-pp response (static binding-decomposition coefficient)
#   * realized-dose pre-trend Wald test (PER CELL, pooled and baseline-split)
#   * Laffer-style offset (Inc-bind) = 1 - beta_net * tau_pre
#   * baseline-rate split (cap 1pp / median 0.5pp) of all three cells
#
# Revenue series: AFR EIT / real-estate-tax lines, coalesced across DCED's two
# eras (`*_revenues` 2006+, raw 2002-05; disjoint). LST is a separate column, so
# EIT excludes it. Directly reported only -- no LST/combined fill-in.
#
# Outputs (data/q3_unified/):
#   revenue_returns_decomp.csv  (source x cell: beta, se, p, wald pre-trend)
#   revenue_returns_offset.csv  (source: Inc-bind offset at mean/median baseline)
#   revenue_returns_split.csv   (source x cell x baseline group: beta, offset, wald)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified")
K_stack  <- 5L
inc_cells   <- c("Inc-bind","Inc-becomes","Inc-nobind")
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")

# --- panel + helpers --------------------------------------------------------
cat("Sourcing q3_unified_full_table.R...\n")
e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel); ctrls_full <- e$ctrls
rate  <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

# --- both revenue outcomes (coalesced across eras; LST excluded) ------------
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
afr[, yr := as.integer(ifelse(!is.na(reporting_year), reporting_year, year))]
afr[, eit_raw := suppressWarnings(as.numeric(
      ifelse(!is.na(earned_income_tax_revenues), earned_income_tax_revenues, earned_income_tax)))]
afr[, ptx_raw := suppressWarnings(as.numeric(
      ifelse(!is.na(real_estate_tax_revenues), real_estate_tax_revenues, real_estate_tax)))]
rev <- afr[!is.na(cousub_fips) & !is.na(yr),
           .(eit = mean(eit_raw, na.rm=TRUE), ptx = mean(ptx_raw, na.rm=TRUE)),
           by = .(muni = cousub_fips, year = yr)]
panel <- merge(panel, rev, by = c("muni","year"), all.x = TRUE)
panel[, log_eit_revenue     := ifelse(!is.na(eit) & eit > 0, log(eit), NA_real_)]
panel[, log_proptax_revenue := ifelse(!is.na(ptx) & ptx > 0, log(ptx), NA_real_)]

# baseline resident rate per Inc-cell muni
base_all <- panel[cell_muni %in% inc_cells,
  {cm <- c_muni[1L]; .(cell0 = as.character(cell_muni[1L]),
                       tau_pre = resident_eit[match(cm-1L, year)])}, by = muni]

# realized muni dose (constant per muni; for the ES)
mu <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu, muni, year)
rd <- mu[!is.na(c_muni), {pre <- resident_eit[year==c_muni-1L]
  yend <- min(c_muni+K_stack, max(year,na.rm=TRUE)); endv <- resident_eit[year==yend]
  list(realized_dose_muni = (if(length(endv)==1) endv else NA_real_)-(if(length(pre)==1) pre else NA_real_))}, by=muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
rd <- rbind(rd, data.table(muni=unique(panel[cell_muni=="NC",muni]), realized_dose_muni=0))
panel <- merge(panel, unique(rd, by="muni"), by="muni", all.x=TRUE)
panel[is.na(realized_dose_muni), realized_dose_muni := 0]
nc_munis <- unique(panel[cell_muni=="NC", muni])

# --- generic stacked-DiD on a cell-label column -----------------------------
build_stack_lbl <- function(p, cell_col, levels, K = K_stack) {
  treated <- unique(p[get(cell_col) != "NC" & !is.na(c_muni),
                      .(muni, cohort = c_muni, cell = get(cell_col))])
  stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy-K):(cy+K)
    d_t <- p[muni %in% treated[cohort==cy,muni] & year %in% win]
    d_t <- merge(d_t, treated[cohort==cy,.(muni,treat_cell=cell)], by="muni", all.x=TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]; d_c[, treat_cell:="NC"]
    d <- rbind(d_t,d_c,fill=TRUE); if(nrow(d)==0) return(NULL)
    d[, stack:=cy]; d[, rel:=as.integer(year-cy)]; d
  }), fill=TRUE)
  stk[, stack_muni:=paste0(stack,"_",muni)]; stk[, stack_year:=paste0(stack,"_",year)]
  stk[, base_rate:=mean(dose_muni[rel<0],na.rm=TRUE), by=.(stack,muni)]
  stk[!is.finite(base_rate), base_rate:=0]; stk[, dose_stack:=dose_muni-base_rate]
  stk[, treat_cell:=factor(treat_cell, levels=levels)]
  stk[, cell_sd:=factor(as.character(cell_sd), levels=cell_levels)]; stk[is.na(dose_sd),dose_sd:=0]
  stk
}

# static net response for each treated cell in `levels`
static_betas <- function(outcome, ctrls_use, cell_col, levels) {
  stk <- build_stack_lbl(panel, cell_col, levels)
  cs <- paste(ctrls_use, collapse=" + ")
  cols <- c(outcome,"treat_cell","dose_stack","cell_sd","dose_sd","stack_muni","stack_year","muni",ctrls_use)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  m <- feols(as.formula(paste0(outcome," ~ i(treat_cell,dose_stack,ref='NC') + i(cell_sd,dose_sd,ref='NC') + ",cs," | stack_muni + stack_year")),
             data=dd, cluster=~muni)
  ct <- as.data.table(m$coeftable, keep.rownames="term")[grepl("^treat_cell::",term)]
  ct[, grp := sub("treat_cell::([^:]+):.*","\\1",term)]
  ct[, .(grp, beta=Estimate, se=`Std. Error`, p=`Pr(>|t|)`)]
}

# realized-dose pre-trend Wald for one treated-muni subset vs NC
es_wald <- function(outcome, ctrls_use, treated_munis) {
  treated <- unique(panel[muni %in% treated_munis & !is.na(c_muni), .(muni, cohort=c_muni)])
  if (nrow(treated)==0) return(list(es_post=NA,es_pre=NA,wald_p=NA))
  stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy){
    win <- (cy-K_stack):(cy+K_stack)
    d <- rbind(panel[muni %in% treated[cohort==cy,muni] & year %in% win],
               panel[muni %in% nc_munis & year %in% win], fill=TRUE)
    if(nrow(d)==0) return(NULL); d[, stack:=cy]; d[, rel:=as.integer(year-cy)]; d
  }), fill=TRUE)
  stk[, stack_muni:=paste0(stack,"_",muni)]; stk[, stack_year:=paste0(stack,"_",year)]
  stk[, cell_sd:=factor(as.character(cell_sd),levels=cell_levels)]
  stk[, cell_nr:=factor(as.character(cell_nr),levels=cell_levels)]
  stk[is.na(dose_sd),dose_sd:=0]; stk[is.na(dose_nr),dose_nr:=0]
  cs <- paste(ctrls_use, collapse=" + ")
  cols <- c(outcome,"rel","realized_dose_muni","cell_sd","dose_sd","cell_nr","dose_nr","stack_muni","stack_year","muni",ctrls_use)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (nrow(dd)==0) return(list(es_post=NA,es_pre=NA,wald_p=NA))
  m <- tryCatch(feols(as.formula(paste0(outcome," ~ i(rel,realized_dose_muni,ref=-1) + i(cell_sd,dose_sd,ref='NC') + i(cell_nr,dose_nr,ref='NC') + ",cs," | stack_muni + stack_year")),
                      data=dd, cluster=~muni), error=function(z) NULL)
  if (is.null(m)) return(list(es_post=NA,es_pre=NA,wald_p=NA))
  ic <- as.data.table(m$coeftable, keep.rownames="term")[grepl("^rel::",term)]
  ic[, rel:=as.integer(sub("rel::(-?\\d+):.*","\\1",term))]
  wp <- wald_pretrend(m, "^rel::-[2-9]")
  list(es_post=round(mean(ic[rel>=0&rel<=5,Estimate],na.rm=TRUE),3),
       es_pre =round(mean(ic[rel<=-2,Estimate],na.rm=TRUE),3), wald_p=round(wp$p,4))
}

# --- main runner for one revenue source -------------------------------------
run_returns <- function(outcome, ctrls_use, label) {
  cat(sprintf("\n########## %s  (controls drop: %s) ##########\n",
              label, setdiff(ctrls_full, ctrls_use)))
  ## pooled 3-cell decomposition + per-cell pre-trend
  sb <- static_betas(outcome, ctrls_use, "cell_muni", c("NC", inc_cells))
  decomp <- rbindlist(lapply(inc_cells, function(cl){
    b <- sb[grp==cl]; w <- es_wald(outcome, ctrls_use, base_all[cell0==cl, muni])
    data.table(source=label, cell=cl, beta=round(b$beta,3), se=round(b$se,3), p=round(b$p,4),
               es_pre=w$es_pre, wald_pre_p=w$wald_p)
  }), fill=TRUE)
  print(decomp)
  ## Inc-bind offset
  b_ib <- decomp[cell=="Inc-bind", beta]
  tau_m <- mean(base_all[cell0=="Inc-bind", tau_pre], na.rm=TRUE)
  tau_md<- median(base_all[cell0=="Inc-bind", tau_pre], na.rm=TRUE)
  offset <- data.table(source=label, beta_net=b_ib, tau_mean=round(tau_m,3), tau_med=round(tau_md,3),
                       offset_mean=round(1-b_ib*tau_m,3), offset_med=round(1-b_ib*tau_md,3))
  ## baseline split (cap 1pp, median 0.5pp): static beta + offset + pre-trend per group
  split <- rbindlist(lapply(list(c(1.0,"cap_1pp"), c(0.5001,"median_0.5pp")), function(z){
    thr <- as.numeric(z[1]); sname <- z[2]
    pn <- copy(panel); pn[, cell_split := as.character(cell_muni)]
    for (cl in inc_cells){
      pn[muni %in% base_all[cell0==cl & tau_pre< thr, muni], cell_split:=paste0(cl,"-lo")]
      pn[muni %in% base_all[cell0==cl & tau_pre>=thr, muni], cell_split:=paste0(cl,"-hi")]
    }
    grps <- paste0(rep(inc_cells,each=2), c("-lo","-hi"))
    panel_bak <- panel; panel <<- pn       # static_betas reads `panel`
    sbs <- tryCatch(static_betas(outcome, ctrls_use, "cell_split", c("NC",grps)), error=function(z) NULL)
    panel <<- panel_bak
    rbindlist(lapply(grps, function(gg){
      cl <- sub("-(lo|hi)$","",gg); hi <- grepl("-hi$",gg)
      mset <- if(hi) base_all[cell0==cl & tau_pre>=thr, muni] else base_all[cell0==cl & tau_pre<thr, muni]
      tp <- mean(base_all[muni %in% mset, tau_pre], na.rm=TRUE)
      b <- if(!is.null(sbs) && nrow(sbs[grp==gg])) sbs[grp==gg, beta] else NA_real_
      w <- if(length(mset)>0) es_wald(outcome, ctrls_use, mset) else list(wald_p=NA)
      data.table(source=label, split=sname, cell=gg, n=length(mset), tau_pre=round(tp,3),
                 beta=round(b,3), offset=round(1-b*tp,3), wald_pre_p=w$wald_p)
    }), fill=TRUE)
  }), fill=TRUE)
  list(decomp=decomp, offset=offset, split=split)
}

eit <- run_returns("log_eit_revenue",     setdiff(ctrls_full,"share_eit"), "EIT")
ptx <- run_returns("log_proptax_revenue", setdiff(ctrls_full,"share_re"),  "property_tax")

fwrite(rbind(eit$decomp, ptx$decomp), file.path(out_dir,"revenue_returns_decomp.csv"))
fwrite(rbind(eit$offset, ptx$offset), file.path(out_dir,"revenue_returns_offset.csv"))
fwrite(rbind(eit$split,  ptx$split),  file.path(out_dir,"revenue_returns_split.csv"))
cat("\n=== OFFSET (Inc-bind) ===\n"); print(rbind(eit$offset, ptx$offset))
cat("\n=== SPLIT ===\n"); print(rbind(eit$split, ptx$split))
cat("\nDone. Outputs: revenue_returns_decomp.csv, revenue_returns_offset.csv, revenue_returns_split.csv\n")
