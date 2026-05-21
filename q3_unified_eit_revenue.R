# q3_unified_eit_revenue.R
# Task 3: run LOG EIT REVENUE through the binding decomposition (the same stacked
#   continuous-DiD as the headline, Inc-bind/Inc-becomes/Inc-nobind cells vs NC).
#   The Inc-bind coefficient is the NET per-pp revenue response (mechanical +
#   behavioral combined) -- the cleanest "returns" result, all from one design.
#
# Task 4: Laffer-style behavioral offset. The mechanical-only benchmark -- what a
#   1pp increase raises if the base does not move -- is, in log terms,
#   d log R / d tau = 1/tau_pre  (since R = tau * B, log R = log tau + log B,
#   and B is held fixed). Comparing the net per-pp response beta_net to this:
#       offset = 1 - beta_net / beta_mech = 1 - beta_net * tau_pre
#   = the share of the mechanically projected revenue gain offset by the sorting
#   response. This is a WITHIN-PA LOCAL-DISPLACEMENT offset estimated on the
#   24-muni, Home-Rule-heavy Inc-bind cohort -- NOT a state-level fiscal multiplier.
#
# EIT revenue series: the AFR EIT line, coalesced across DCED's two reporting
#   eras -- `earned_income_tax_revenues` (2006+) and `earned_income_tax`
#   (2002-2005), which are disjoint by year. In the 2006+ family the Local
#   Services Tax is a SEPARATE column (`local_services_tax_revenues`), so EIT is
#   not folded in with LST (no LST fill-in). This is the same EIT measure the
#   fiscal-control share `share_eit` uses, so the outcome aligns with the
#   controls; the pre-2006 years drop out because the control denominators
#   (total_revenues) begin in 2006.
#
# Outputs:
#   data/q3_unified/eit_revenue_decomp.csv  (static cell coefficients)
#   data/q3_unified/eit_revenue_es.csv      (realized-dose ES leads + Wald)
#   data/q3_unified/eit_revenue_offset.csv  (Laffer offset)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack  <- 5L
inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")

# ---------------------------------------------------------------------------
# 1. Classified panel + estimation helpers from the headline
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R...\n")
e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)
# Drop share_eit (= EIT revenue / total revenue) from the controls: with log EIT
# revenue as the OUTCOME it is mechanically the same object, so including it would
# absorb the revenue response and bias the coefficient toward zero. Keep the other
# five fiscal shares (none contains EIT revenue).
ctrls <- setdiff(e$ctrls, "share_eit"); ctrls_str <- paste(ctrls, collapse = " + ")
rate  <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

# ---------------------------------------------------------------------------
# 2. Clean EIT revenue (AFR earned_income_tax, directly reported; NO LST fill-in)
# ---------------------------------------------------------------------------
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
afr[, yr := as.integer(ifelse(!is.na(reporting_year), reporting_year, year))]
# Coalesce the two reporting eras; LST is a separate line so EIT excludes it.
afr[, eit_raw := suppressWarnings(as.numeric(
      ifelse(!is.na(earned_income_tax_revenues), earned_income_tax_revenues, earned_income_tax)))]
eit <- afr[!is.na(cousub_fips) & !is.na(yr),
           .(eit_rev = mean(eit_raw, na.rm = TRUE)),
           by = .(muni = cousub_fips, year = yr)]
eit[!is.finite(eit_rev) | eit_rev <= 0, eit_rev := NA_real_]   # drop missing/zero; never fill with LST
panel <- merge(panel, eit, by = c("muni", "year"), all.x = TRUE)
panel[, log_eit_revenue := ifelse(!is.na(eit_rev) & eit_rev > 0, log(eit_rev), NA_real_)]
cat(sprintf("EIT-revenue muni-years: %s of %s (%.0f%%); Inc-bind treated muni-years with EIT: %d\n",
            format(sum(!is.na(panel$log_eit_revenue)), big.mark = ","),
            format(nrow(panel), big.mark = ","),
            100 * mean(!is.na(panel$log_eit_revenue)),
            panel[cell_muni == "Inc-bind" & !is.na(log_eit_revenue), .N]))

# ---------------------------------------------------------------------------
# 3. Static binding decomposition on log EIT revenue (headline spec)
# ---------------------------------------------------------------------------
stk <- e$build_stack(panel, "muni", "dose_muni")
stk[, treat_cell := factor(as.character(treat_cell), levels = cell_levels)]
stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
stk[is.na(dose_sd), dose_sd := 0]
scols <- c("log_eit_revenue","treat_cell","dose_stack","cell_sd","dose_sd",
           "stack_muni","stack_year","muni", ctrls)
dd <- stk[, ..scols][complete.cases(stk[, ..scols])]
cat(sprintf("  static EIT regression sample: %s rows; Inc-cell treated rows = %d\n",
            format(nrow(dd), big.mark = ","), dd[treat_cell %in% inc_cells, .N]))
f_static <- as.formula(paste0("log_eit_revenue ~ i(treat_cell, dose_stack, ref='NC') ",
            "+ i(cell_sd, dose_sd, ref='NC') + ", ctrls_str, " | stack_muni + stack_year"))
m <- feols(f_static, data = dd, cluster = ~muni)
ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^treat_cell::", term)]
ct[, cell := sub("treat_cell::([^:]+):.*", "\\1", term)]
decomp <- ct[cell %in% inc_cells,
             .(outcome = "log_eit_revenue", cell, beta = round(Estimate, 3),
               se = round(`Std. Error`, 3), p = round(`Pr(>|t|)`, 4), n = nobs(m))]
setkey(decomp, cell)
cat("\n=== Task 3: net per-pp revenue response (log EIT revenue, Inc cells vs NC) ===\n")
print(decomp[c("Inc-bind","Inc-becomes","Inc-nobind")])
fwrite(decomp, file.path(out_dir, "eit_revenue_decomp.csv"))

# ---------------------------------------------------------------------------
# 4a. Realized-dose ES (pre-trend test) on log EIT revenue
# ---------------------------------------------------------------------------
mu <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu, muni, year)
rd <- mu[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  yend <- min(c_muni + K_stack, max(year, na.rm = TRUE)); endv <- resident_eit[year == yend]
  list(realized_dose_muni = (if (length(endv)==1) endv else NA_real_) -
                            (if (length(pre)==1) pre else NA_real_))
}, by = muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
rd <- rbind(rd, data.table(muni = unique(panel[cell_muni=="NC", muni]), realized_dose_muni = 0))
panel <- merge(panel, unique(rd, by = "muni"), by = "muni", all.x = TRUE)
panel[is.na(realized_dose_muni), realized_dose_muni := 0]

treated <- unique(panel[cell_muni != "NC" & !is.na(c_muni), .(muni, cohort = c_muni)])
ncm <- unique(panel[cell_muni == "NC", muni])
es_stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
  win <- (cy - K_stack):(cy + K_stack)
  d_t <- panel[muni %in% treated[cohort==cy, muni] & year %in% win]; d_t[, tcell := cell_muni]
  d_c <- panel[muni %in% ncm & year %in% win]; d_c[, tcell := "NC"]
  d <- rbind(d_t, d_c, fill = TRUE); if (nrow(d)==0) return(NULL)
  d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
}), fill = TRUE)
es_stk[, stack_muni := paste0(stack, "_", muni)]; es_stk[, stack_year := paste0(stack, "_", year)]
es_stk[, tcell := factor(tcell, levels = cell_levels)]
es_stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
es_stk[, cell_nr := factor(as.character(cell_nr), levels = cell_levels)]
es_stk[is.na(dose_sd), dose_sd := 0]; es_stk[is.na(dose_nr), dose_nr := 0]

es_res <- list(); att_res <- list()
for (cl in inc_cells) {
  cols <- c("muni","year","stack_muni","stack_year","tcell","realized_dose_muni","rel",
            "cell_sd","dose_sd","cell_nr","dose_nr","log_eit_revenue", ctrls)
  dd <- es_stk[tcell %in% c("NC", cl), ..cols][complete.cases(es_stk[tcell %in% c("NC", cl), ..cols])]
  if (nrow(dd)==0 || sum(dd$tcell==cl)==0) next
  f <- as.formula(paste0("log_eit_revenue ~ i(rel, realized_dose_muni, ref=-1) + ",
        "i(cell_sd, dose_sd, ref='NC') + i(cell_nr, dose_nr, ref='NC') + ", ctrls_str,
        " | stack_muni + stack_year"))
  mm <- tryCatch(feols(f, data = dd, cluster = ~muni), error = function(ee) NULL)
  if (is.null(mm)) next
  ic <- as.data.table(mm$coeftable, keep.rownames="term")[grepl("^rel::", term)]
  ic[, rel := as.integer(sub("rel::(-?\\d+):.*","\\1",term))]
  es_res[[cl]] <- ic[, .(cell = cl, rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
  wp <- wald_pretrend(mm, "^rel::-[2-9]")
  att_res[[cl]] <- data.table(cell = cl, n_munis = uniqueN(dd[tcell==cl, muni]),
    es_post = round(mean(ic[rel>=0 & rel<=5, Estimate], na.rm=TRUE), 3),
    es_pre  = round(mean(ic[rel<=-2, Estimate], na.rm=TRUE), 3),
    wald_pre_p = round(wp$p, 4))
}
att_dt <- rbindlist(att_res, fill = TRUE)
fwrite(rbindlist(es_res, fill = TRUE), file.path(out_dir, "eit_revenue_es.csv"))
cat("\n=== Realized-dose ES pre-trends (log EIT revenue) ===\n"); print(att_dt)

# ---------------------------------------------------------------------------
# 4b. Laffer-style behavioral offset (Inc-bind)
# ---------------------------------------------------------------------------
beta_net <- decomp["Inc-bind", beta]
base <- panel[cell_muni == "Inc-bind", {cm <- c_muni[1L]; .(tau = resident_eit[match(cm-1L, year)])}, by = muni]
tau_mean <- mean(base$tau, na.rm = TRUE); tau_med <- median(base$tau, na.rm = TRUE)
offset_at <- function(tp) 1 - beta_net * tp
cat("\n=== Task 4: Laffer-style behavioral offset (Inc-bind) ===\n")
cat(sprintf("  net per-pp log-revenue response  beta_net = %.3f\n", beta_net))
cat(sprintf("  mechanical benchmark beta_mech = 1/tau_pre\n"))
cat(sprintf("  at mean baseline tau_pre=%.3f pp:  beta_mech=%.3f  ->  offset = %.0f%%\n",
            tau_mean, 1/tau_mean, 100*offset_at(tau_mean)))
cat(sprintf("  at median baseline tau_pre=%.3f pp: beta_mech=%.3f  ->  offset = %.0f%%\n",
            tau_med, 1/tau_med, 100*offset_at(tau_med)))
cat("  (Within-PA local-displacement offset on the 24-muni Home-Rule-heavy Inc-bind\n")
cat("   cohort; not a state-level fiscal multiplier.)\n")
fwrite(data.table(beta_net = beta_net, tau_pre_mean = round(tau_mean,3),
                  tau_pre_median = round(tau_med,3),
                  offset_at_mean = round(offset_at(tau_mean),3),
                  offset_at_median = round(offset_at(tau_med),3)),
       file.path(out_dir, "eit_revenue_offset.csv"))
# ---------------------------------------------------------------------------
# 5. Split EACH Inc cell (bind / becomes / nobind) by pre-event baseline rate,
#    to break the high-rate Home Rule / Act 47 cities out of the standard
#    (sub-1pp Act 511 cap) municipalities within every cell.
# ---------------------------------------------------------------------------
base_all <- panel[cell_muni %in% inc_cells,
  {cm <- c_muni[1L]; .(cell0 = as.character(cell_muni[1L]),
                       tau_pre = resident_eit[match(cm-1L, year)])}, by = muni]

build_stack_split <- function(p, levels, K = K_stack) {
  treated <- unique(p[cell_split != "NC" & !is.na(c_muni), .(muni, cohort = c_muni, cell = cell_split)])
  ncm <- unique(p[cell_split == "NC", muni])
  stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy - K):(cy + K)
    d_t <- p[muni %in% treated[cohort==cy, muni] & year %in% win]
    d_t <- merge(d_t, treated[cohort==cy, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% ncm & year %in% win]; d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE); if (nrow(d)==0) return(NULL)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]; stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]; stk[, dose_stack := dose_muni - base_rate]
  stk[, treat_cell := factor(treat_cell, levels = levels)]
  stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]; stk[is.na(dose_sd), dose_sd := 0]
  stk
}

run_split <- function(thresh, split_name) {
  pn <- copy(panel); pn[, cell_split := as.character(cell_muni)]
  for (cl in inc_cells) {
    pn[muni %in% base_all[cell0==cl & tau_pre <  thresh, muni], cell_split := paste0(cl, "-lo")]
    pn[muni %in% base_all[cell0==cl & tau_pre >= thresh, muni], cell_split := paste0(cl, "-hi")]
  }
  groups <- paste0(rep(inc_cells, each = 2), c("-lo","-hi"))
  stk <- build_stack_split(pn, c("NC", groups))
  cols <- c("log_eit_revenue","treat_cell","dose_stack","cell_sd","dose_sd",
            "stack_muni","stack_year","muni", ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  m <- feols(as.formula(paste0("log_eit_revenue ~ i(treat_cell, dose_stack, ref='NC') ",
        "+ i(cell_sd, dose_sd, ref='NC') + ", ctrls_str, " | stack_muni + stack_year")),
        data = dd, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^treat_cell::", term)]
  ct[, grp := sub("treat_cell::([^:]+):.*", "\\1", term)]
  rbindlist(lapply(groups, function(gg) {
    cl <- sub("-(lo|hi)$", "", gg); hi <- grepl("-hi$", gg)
    bg <- if (hi) base_all[cell0==cl & tau_pre>=thresh, tau_pre] else base_all[cell0==cl & tau_pre<thresh, tau_pre]
    tp <- mean(bg, na.rm = TRUE); row <- ct[grp == gg]
    b  <- if (nrow(row)) row$Estimate[1] else NA_real_
    data.table(split = split_name, cell = gg, n = length(bg), tau_pre = round(tp,3),
               beta_net = round(b,3),
               se = if (nrow(row)) round(row$`Std. Error`[1],3) else NA_real_,
               p  = if (nrow(row)) round(row$`Pr(>|t|)`[1],4) else NA_real_,
               offset = round(1 - b*tp, 3))
  }), fill = TRUE)
}

cat("\n=== Baseline-rate split of all three Inc cells (log EIT revenue) ===\n")
cat("(thresholds: cap = 1pp Act 511 combined cap; median = 0.5pp.  -lo = below, -hi = at/above)\n")
split_res <- rbind(run_split(1.0, "cap_1pp"), run_split(0.5001, "median_0.5pp"))
fwrite(split_res, file.path(out_dir, "eit_revenue_split.csv"))
print(split_res)

cat("\nDone. Outputs: eit_revenue_decomp.csv, eit_revenue_es.csv, eit_revenue_offset.csv, eit_revenue_split.csv\n")
