# q3_unified_pooled_es_block_only.R
# Standalone: block-level pooled-Inc and pooled-Dec realized-dose
# event study. Builds its own stacked block panel (does not source
# block_es_full.R, which would re-run the per-cell ES as a side
# effect). Outputs:
#   data/q3_unified/pooled_es_block_coefs.csv
#   data/q3_unified/pooled_es_block_att.csv

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
K_stack <- 5L

inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
dec_cells <- c("Dec-bind","Dec-becomes-non","Dec-nobind")
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
sd_ctrls   <- c("share_state_rev_sd","share_local_rev_sd",
                "share_federal_rev_sd","share_property_tax_sd",
                "share_instructional_sd","share_support_sd",
                "share_capital_sd","share_debt_int_sd")
ctrls <- c(muni_ctrls, sd_ctrls)
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")

# 1. Source cousub-level prep for muni cell labels + doses + AFR shares.
cat("Sourcing cousub-level full table prep...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
cousub_panel <- e$panel
cousub_meta <- unique(cousub_panel[, .(muni, year,
                                        cell_muni, cell_sd, cell_nr,
                                        dose_muni, dose_sd, dose_nr,
                                        c_muni,
                                        share_police, share_streets, share_debt_svc,
                                        share_eit, share_re, share_intergov)])

# 2. Block panel
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                          "share_eit","share_re","share_intergov"),
                        names(b))
if (length(drop_cols)) b[, (drop_cols) := NULL]
b <- merge(b, cousub_meta, by = c("muni","year"), all.x = TRUE)

# 3. Realized muni-side dose per muni
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
mu_year <- unique(rate[!is.na(cousub_fips) & !is.na(resident_eit),
                       .(muni = cousub_fips, year, resident_eit)])
mu_year <- merge(mu_year, unique(cousub_meta[, .(muni, c_muni)]),
                  by = "muni", all.x = TRUE)
rd <- mu_year[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  yr_max <- min(c_muni + K_stack, max(year, na.rm = TRUE))
  end <- resident_eit[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose_muni = end_v - pre_v)
}, by = muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
nc_munis <- unique(cousub_meta[cell_muni == "NC", muni])
rd <- rbind(rd, data.table(muni = nc_munis, realized_dose_muni = 0))
rd <- unique(rd)
b <- merge(b, rd, by = "muni", all.x = TRUE)
b[is.na(realized_dose_muni), realized_dose_muni := 0]
b[is.na(dose_sd), dose_sd := 0]
b[is.na(dose_nr), dose_nr := 0]

# 4. SD share controls
sd_path <- file.path(data_dir, "pa_sd_controls.rds")
if (file.exists(sd_path)) {
  sd_ctrl <- as.data.table(readRDS(sd_path))
  keep <- intersect(c("sd_id","year", sd_ctrls), names(sd_ctrl))
  sd_ctrl <- sd_ctrl[, ..keep]
  sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                       if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                     by = .(sd_id, year)]
  for (cc in sd_ctrls)
    set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
  b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)
}

# 5. Stack on muni cohorts
b[, cell_muni := factor(cell_muni, levels = cell_levels)]
b[, cell_sd   := factor(cell_sd,   levels = cell_levels)]
b[, cell_nr   := factor(cell_nr,   levels = cell_levels)]

cat("\nBuilding stacked block panel...\n")
treated <- unique(b[cell_muni != "NC" & !is.na(cell_muni),
                     .(muni, cohort = c_muni, cell = cell_muni)])
nc_munis_b <- unique(b[cell_muni == "NC", muni])
cohort_years <- sort(unique(treated$cohort))
stk <- rbindlist(lapply(cohort_years, function(cy) {
  win <- (cy - K_stack):(cy + K_stack)
  t_munis <- treated[cohort == cy]
  if (nrow(t_munis) == 0) return(NULL)
  d_t <- b[muni %in% t_munis$muni & year %in% win]
  d_t[, treat_cell_muni := cell_muni]
  d_c <- b[muni %in% nc_munis_b & year %in% win]
  d_c[, treat_cell_muni := "NC"]
  d <- rbind(d_t, d_c, fill = TRUE)
  d[, stack := cy]; d[, rel := as.integer(year - cy)]
  d
}), fill = TRUE)
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk[, pool_inc := as.integer(as.character(treat_cell_muni) %in% inc_cells)]
stk[, pool_dec := as.integer(as.character(treat_cell_muni) %in% dec_cells)]
stk[, is_nc    := as.integer(as.character(treat_cell_muni) == "NC")]

cat(sprintf("Stack: %s rows; treated %s; NC %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell_muni != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell_muni == "NC", block_geoid]), big.mark=",")))

# 6. Pooled ES per direction (Inc / Dec) per outcome
es_results <- list(); att_results <- list()
for (direction in c("Inc")) {
  cat(sprintf("\n=== Pooled %s ===\n", direction))
  dd_base <- if (direction == "Inc") stk[pool_inc == 1 | is_nc == 1]
             else                    stk[pool_dec == 1 | is_nc == 1]
  for (o in outcomes) {
    cols <- c("block_geoid","year","muni","sd_id","stack","rel",
               "realized_dose_muni","cell_sd","dose_sd","cell_nr","dose_nr",
               o, ctrls)
    cols <- intersect(cols, names(dd_base))
    d <- dd_base[, ..cols]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    cat(sprintf("  [%s] N=%s ... ", o, format(nrow(d), big.mark=",")))
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1)",
         " + i(cell_sd, dose_sd, ref = 'NC')",
         " + i(cell_nr, dose_nr, ref = 'NC') + ",
         ctrls_str,
         " | block_geoid^stack + year^stack"))
    m <- tryCatch(feols(f, data = d, cluster = ~muni + sd_id),
                   error = function(e) { cat("err:",conditionMessage(e),"\n"); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) { cat("no rel terms\n"); next }
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, direction := direction]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic
    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    wp <- wald_pretrend(m, "^rel::-[2-9]")
    dep_vec <- m$fitted.values + m$residuals
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, direction = direction,
      att_post = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se  = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      wald_pre_stat = round(wp$stat, 3),
      wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4),
      n = nobs(m),
      r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
      wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean = round(mean(dep_vec, na.rm = TRUE), 3),
      dep_sd = round(sd(dep_vec, na.rm = TRUE), 3))
    cat(sprintf("post=%.3f pre=%.3f  Wald(%d)=%.2f p=%.4f\n",
                mean(post$est, na.rm = TRUE),
                mean(pre$est, na.rm = TRUE),
                wp$df, wp$stat, wp$p))
  }
}
fwrite(rbindlist(es_results,  fill = TRUE),
       file.path(out_dir, "pooled_es_block_coefs.csv"))
fwrite(rbindlist(att_results, fill = TRUE),
       file.path(out_dir, "pooled_es_block_att.csv"))
cat("\nDone.\n")
