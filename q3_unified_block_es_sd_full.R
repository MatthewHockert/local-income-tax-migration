# q3_unified_block_es_sd_full.R
# Block-level Sun-Abraham realized-dose event-study on the **SD-rate** side,
# fully parameterized: muni-side cell x dose and nonres-side cell x dose
# interactions kept exactly as in the headline equation. Analog of
# q3_unified_block_es_full.R but stacking on SD cohorts (c_sd) and using
# the SD-side realized dose. Used to build muni+SD overlay block-ES figures.
#
# Spec (per SD-side cell c, restrict to {NC, c} on cell_sd):
#   y_{bt} = sum_{k != -1} beta^S_k * 1{rel = k} * realized_dose_sd_b
#          + sum_{c'} beta^M_{c'} * Delta tau_M_{bt} * 1{cell_M = c'}
#          + sum_{c'} beta^N_{c'} * Delta tau_nr_{bt} * 1{cell_nr = c'}
#          + X phi
#          + alpha_{stack, b} + lambda_{stack, t} + epsilon
# Cluster: ~ muni + sd_id

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

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

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

# 1. Source cousub prep for muni/SD/nr cells + doses + muni AFR shares
cat("Sourcing cousub-level full table prep for cells/doses...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
cousub_panel <- e$panel
cousub_meta <- unique(cousub_panel[, .(muni, year,
                                        cell_muni, cell_sd, cell_nr,
                                        dose_muni, dose_sd, dose_nr,
                                        c_sd, c_muni,
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

# 3. Realized SD-side dose per (sd_id, muni). Use within-sd_id cohort first-event.
#    Compute pre-event and end-of-window rate from cousub_panel rate trail.
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
sd_year <- unique(rate[!is.na(cousub_fips) & !is.na(sd_eit),
                       .(muni = cousub_fips, year, sd_eit)])
sd_year <- merge(sd_year,
                 unique(cousub_meta[, .(muni, c_sd)]),
                 by = "muni", all.x = TRUE)
rd_sd <- sd_year[!is.na(c_sd), {
  pre  <- sd_eit[year == c_sd - 1L]
  yr_max <- min(c_sd + K_stack, max(year, na.rm = TRUE))
  end  <- sd_eit[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose_sd = end_v - pre_v)
}, by = muni]
rd_sd[is.na(realized_dose_sd), realized_dose_sd := 0]
nc_munis_sd <- unique(cousub_meta[cell_sd == "NC", muni])
rd_sd <- rbind(rd_sd, data.table(muni = nc_munis_sd, realized_dose_sd = 0))
rd_sd <- unique(rd_sd)
b <- merge(b, rd_sd, by = "muni", all.x = TRUE)

# 4. SD share controls onto block panel
sd_ctrls_path <- file.path(data_dir, "pa_sd_controls.rds")
if (file.exists(sd_ctrls_path)) {
  sd_ctrl <- as.data.table(readRDS(sd_ctrls_path))
  keep <- intersect(c("sd_id","year", sd_ctrls), names(sd_ctrl))
  sd_ctrl <- sd_ctrl[, ..keep]
  sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                       if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                     by = .(sd_id, year)]
  for (cc in sd_ctrls) {
    set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
  }
  b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)
}
b[is.na(realized_dose_sd), realized_dose_sd := 0]
b[is.na(dose_muni), dose_muni := 0]
b[is.na(dose_nr),   dose_nr := 0]

cat("\nSD cell distribution (block-year):\n"); print(b[, .N, keyby = cell_sd])

# 5. Stack on SD cohorts
build_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_sd != "NC" & !is.na(cell_sd),
                       .(muni, sd_id, cohort = c_sd, cell = cell_sd)])
  nc_munis <- unique(p[cell_sd == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_m <- unique(treated[cohort == cy, .(muni)])
    if (nrow(t_m) == 0) return(NULL)
    d_t <- p[muni %in% t_m$muni & year %in% win]
    d_t[, treat_cell_sd := cell_sd]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell_sd := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }), fill = TRUE)
}

cat("\nBuilding SD-cohort stacked block panel (this is large)...\n")
stk <- build_stack(b)
stk[, treat_cell_sd := factor(treat_cell_sd, levels = cell_levels)]
stk[, cell_muni := factor(cell_muni, levels = cell_levels)]
stk[, cell_nr   := factor(cell_nr,   levels = cell_levels)]

cat(sprintf("Stack: %s rows; treated blocks %s; NC blocks %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell_sd != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell_sd == "NC", block_geoid]), big.mark=",")))

# 6. Per-cell ES (SD side)
es_results <- list(); att_results <- list()
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- stk[treat_cell_sd %in% c("NC", cl)]
    cols <- c("block_geoid","year","muni","sd_id","stack",
               "treat_cell_sd","realized_dose_sd","rel",
               "cell_muni","dose_muni","cell_nr","dose_nr", o, ctrls)
    cols <- intersect(cols, names(dd))
    dd <- dd[, ..cols]
    dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0 || sum(dd$treat_cell_sd == cl) == 0) {
      cat(sprintf("  %s skip\n", cl)); next
    }
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_sd, ref = -1) ",
         "+ i(cell_muni, dose_muni, ref = 'NC') ",
         "+ i(cell_nr, dose_nr, ref = 'NC') + ",
         ctrls_str, " | block_geoid^stack + year^stack"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni + sd_id),
                   error = function(ee) {
                     cat(sprintf("  %s error: %s\n", cl, conditionMessage(ee))); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) next
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic
    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    wp <- wald_pretrend(m, "^rel::-[2-9]")
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, cell = cl,
      att_post_per_pp = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se     = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se      = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      wald_pre_stat = round(wp$stat, 3),
      wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4),
      n = nobs(m))
    cat(sprintf("  %-16s post=%6.3f (SE %.3f)  pre=%6.3f (SE %.3f)  N=%s\n",
                cl, mean(post$est, na.rm=TRUE),
                sqrt(mean(post$se^2, na.rm=TRUE) / max(nrow(post),1)),
                mean(pre$est, na.rm=TRUE),
                sqrt(mean(pre$se^2, na.rm=TRUE) / max(nrow(pre),1)),
                format(nobs(m), big.mark=",")))
  }
}
es_dt  <- rbindlist(es_results,  fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "block_es_sd_full_coefs.csv"))
fwrite(att_dt, file.path(out_dir, "block_es_sd_full_att.csv"))
cat("\nSaved data/q3_unified/block_es_sd_full_coefs.csv\n")
