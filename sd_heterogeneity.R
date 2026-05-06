# sd_heterogeneity.R
# Q3 of the SD-coefficient flip: split sample into cousubs that WERE in the
# prior tract-aggregated panel (likely larger, dense, host of a tract centroid)
# vs cousubs that were silently dropped (likely smaller, rural). Run the Q2
# decomposition on each subset and report SD coefficient. Also run by size
# tercile to confirm.

suppressMessages({library(data.table); library(fixest)})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

data_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"
max_year <- 2019

pa_panel        <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
pa_tract_xwalk  <- as.data.table(readRDS(file.path(data_dir, "pa_tract_xwalk.rds")))
old_cousubs     <- unique(pa_tract_xwalk$cousub_fips)

pa_panel[, in_old_panel := work_city %in% old_cousubs]

# --- Reattach fiscal controls (mirror run_slide_models.R) ---
pa_rate_panel <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
pa_afr_raw    <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
pa_afr_raw[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
pa_afr_raw <- merge(pa_afr_raw, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
pa_afr_raw[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)

fc <- pa_afr_raw[!is.na(cousub_fips), {
  total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
  eit_rev   <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                                 earned_income_tax_revenues, earned_income_tax))
  re_tax    <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                                 real_estate_tax_revenues, real_estate_tax))
  list(cousub_fips = cousub_fips, year = yr,
       log_police    = log_pos(ifelse(!is.na(police_expenditures), police_expenditures, police)),
       log_streets   = log_pos(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                      `public_works-highways_and_streets_expenditures`, streets_and_roads)),
       log_debt      = log_pos(as.numeric(total_debt)),
       log_state_aid = log_pos(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                      `intergovernmental_revenues-state_government`, state_govt)),
       share_eit     = ifelse(total_rev > 0, eit_rev / total_rev, NA_real_),
       share_re      = ifelse(total_rev > 0, re_tax / total_rev, NA_real_))
}]
pa_panel <- merge(pa_panel, fc,
                  by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"),
                  all.x = TRUE)

# --- Estimation sample ---
pa_samp <- pa_panel[total_jobs > 50 & !is.na(resident_eit) & !is.na(sd_eit) &
                    !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) &
                    !is.na(log_state_aid) & !is.na(share_eit) & !is.na(share_re)]

cat("Estimation sample:\n")
cat(sprintf("  Total: %s rows, %d cousubs\n",
            format(nrow(pa_samp), big.mark = ","), uniqueN(pa_samp$work_city)))
cat(sprintf("  In old (tract-centroid) panel: %s rows, %d cousubs\n",
            format(sum(pa_samp$in_old_panel), big.mark = ","),
            uniqueN(pa_samp[in_old_panel == TRUE, work_city])))
cat(sprintf("  Newly-recovered cousubs: %s rows, %d cousubs\n",
            format(sum(!pa_samp$in_old_panel), big.mark = ","),
            uniqueN(pa_samp[in_old_panel == FALSE, work_city])))

# --- Verify newly-recovered cousubs are smaller / less urban ---
size_stats <- pa_samp[year == 2019, .(
  median_total_jobs    = round(median(total_jobs, na.rm = TRUE)),
  mean_total_jobs      = round(mean(total_jobs, na.rm = TRUE)),
  median_share_live_in = round(median(share_live_in, na.rm = TRUE), 3),
  mean_share_live_in   = round(mean(share_live_in, na.rm = TRUE), 3),
  median_resident_eit  = round(median(resident_eit, na.rm = TRUE), 3),
  median_sd_eit        = round(median(sd_eit, na.rm = TRUE), 3),
  n_cousubs            = uniqueN(work_city)
), by = in_old_panel]
cat("\n--- 2019 size/composition by old/new sample ---\n")
print(size_stats)

# --- Q2 decomposition on each subset ---
ctrls_chr <- "log_police + log_streets + log_debt + log_state_aid + share_eit + share_re"
mk <- function(lhs, rhs) as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls_chr,
                                            " | work_city + year"))

fmt <- function(b, se) {
  stars <- ifelse(abs(b/se) > 2.576, "***",
           ifelse(abs(b/se) > 1.96,  "**",
           ifelse(abs(b/se) > 1.645, "*", "")))
  sprintf("%+.4f%s (%.4f)", b, stars, se)
}
get_row <- function(model, var) {
  ct <- model$coeftable
  if (!(var %in% rownames(ct))) return(c(NA, NA))
  ct[var, c("Estimate", "Std. Error")]
}
run_decomp <- function(d, label) {
  if (uniqueN(d$work_city) < 30) {
    cat(sprintf("  %s: too few cousubs (%d), skipped\n", label, uniqueN(d$work_city)))
    return(invisible(NULL))
  }
  m_sli <- feols(mk("share_live_in",  "resident_eit + sd_eit"), data = d, cluster = ~work_city)
  m_lr  <- feols(mk("log_live_in",    "resident_eit + sd_eit"), data = d, cluster = ~work_city)
  m_lj  <- feols(mk("log_total_jobs", "resident_eit + sd_eit"), data = d, cluster = ~work_city)
  cat(sprintf("\n========== %s (%d cousubs, %s rows) ==========\n",
              label, uniqueN(d$work_city), format(nrow(d), big.mark = ",")))
  for (m in list(share_live_in = m_sli, log_live_in = m_lr, log_total_jobs = m_lj)) NULL
  out <- data.table(
    outcome      = c("share_live_in", "log_live_in", "log_total_jobs"),
    resident_eit = c(fmt(get_row(m_sli,"resident_eit")[1], get_row(m_sli,"resident_eit")[2]),
                     fmt(get_row(m_lr ,"resident_eit")[1], get_row(m_lr ,"resident_eit")[2]),
                     fmt(get_row(m_lj ,"resident_eit")[1], get_row(m_lj ,"resident_eit")[2])),
    sd_eit       = c(fmt(get_row(m_sli,"sd_eit")[1], get_row(m_sli,"sd_eit")[2]),
                     fmt(get_row(m_lr ,"sd_eit")[1], get_row(m_lr ,"sd_eit")[2]),
                     fmt(get_row(m_lj ,"sd_eit")[1], get_row(m_lj ,"sd_eit")[2])),
    n            = c(nobs(m_sli), nobs(m_lr), nobs(m_lj))
  )
  print(out)
  invisible(NULL)
}

run_decomp(pa_samp, "FULL SAMPLE")
run_decomp(pa_samp[in_old_panel == TRUE],  "OLD (in tract-centroid panel)")
run_decomp(pa_samp[in_old_panel == FALSE], "NEW (recovered by block aggregation)")

# --- Heterogeneity by size: tercile of mean total_jobs over the panel ---
size_per_muni <- pa_samp[, .(mean_jobs = mean(total_jobs, na.rm = TRUE)), by = work_city]
cuts <- quantile(size_per_muni$mean_jobs, c(0, 1/3, 2/3, 1), na.rm = TRUE)
size_per_muni[, size_tercile := cut(mean_jobs, breaks = cuts, include.lowest = TRUE,
                                    labels = c("Small", "Mid", "Large"))]
pa_samp <- merge(pa_samp, size_per_muni[, .(work_city, size_tercile)], by = "work_city")

cat("\n\n--- SD coefficient by size tercile (mean total_jobs) ---\n")
for (g in c("Small", "Mid", "Large")) {
  run_decomp(pa_samp[size_tercile == g], paste0("Size = ", g))
}

cat("\nDone.\n")
