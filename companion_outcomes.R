# Companion Outcomes: Decomposing the share_live_in Effect
# Reviewer concern: share_live_in decline could reflect:
#   (a) resident workers leaving (sorting)
#   (b) nonresident commuter inflows rising (job growth)
#   (c) total employment changes (labor demand)
# This script runs the main specs on each component separately.
# ============================================================================

library(data.table)
library(fixest)
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

data_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"
max_year <- 2019

pa_panel <- readRDS(file.path(data_dir, "pa_panel.rds"))[year <= max_year]
pa_rate <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))
oh_panel <- readRDS(file.path(data_dir, "oh_panel.rds"))[year <= max_year]
oh_rate <- readRDS(file.path(data_dir, "oh_rate_panel.rds"))

# Log outcomes
pa_panel[, `:=`(
  log_live_work = fifelse(live_and_work > 0, log(live_and_work), NA_real_),
  log_live_out = fifelse(live_outside > 0, log(live_outside), NA_real_),
  log_total = fifelse(total_jobs > 0, log(total_jobs), NA_real_)
)]

oh_panel[, `:=`(
  log_live_work = fifelse(live_and_work > 0, log(live_and_work), NA_real_),
  log_live_out = fifelse(live_outside > 0, log(live_outside), NA_real_),
  log_total = fifelse(total_jobs > 0, log(total_jobs), NA_real_)
)]


# 1. PA TWFE: Full Outcome Family ----
# Three regressors per outcome:
#   muni only (resident_eit) -- back-compat with original results
#   total stack (total_eit) -- bundled home rate per Act 32 piecewise
#   decomposition (resident_eit + sd_eit) -- separates muni vs SD channels

pa_samp <- pa_panel[total_jobs > 50 & !is.na(resident_eit)]

# Muni only
m_share <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
m_lw <- feols(log_live_work ~ resident_eit | work_city + year,
  data = pa_samp[!is.na(log_live_work)], cluster = ~work_city)
m_lo <- feols(log_live_out ~ resident_eit | work_city + year,
  data = pa_samp[!is.na(log_live_out)], cluster = ~work_city)
m_tj <- feols(log_total ~ resident_eit | work_city + year,
  data = pa_samp[!is.na(log_total)], cluster = ~work_city)

# Total stack
m_share_t <- feols(share_live_in ~ total_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
m_lw_t <- feols(log_live_work ~ total_eit | work_city + year,
  data = pa_samp[!is.na(log_live_work)], cluster = ~work_city)
m_lo_t <- feols(log_live_out ~ total_eit | work_city + year,
  data = pa_samp[!is.na(log_live_out)], cluster = ~work_city)
m_tj_t <- feols(log_total ~ total_eit | work_city + year,
  data = pa_samp[!is.na(log_total)], cluster = ~work_city)

# Decomposition
m_share_d <- feols(share_live_in ~ resident_eit + sd_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
m_lw_d <- feols(log_live_work ~ resident_eit + sd_eit | work_city + year,
  data = pa_samp[!is.na(log_live_work)], cluster = ~work_city)
m_lo_d <- feols(log_live_out ~ resident_eit + sd_eit | work_city + year,
  data = pa_samp[!is.na(log_live_out)], cluster = ~work_city)
m_tj_d <- feols(log_total ~ resident_eit + sd_eit | work_city + year,
  data = pa_samp[!is.na(log_total)], cluster = ~work_city)

print("=== PA TWFE: OUTCOME FAMILY (muni only) ===")
etable(m_share, m_lw, m_lo, m_tj,
  headers = c("share_live_in", "log(Resident)", "log(Nonresident)", "log(Total Jobs)"))
print("=== PA TWFE: OUTCOME FAMILY (total stack) ===")
etable(m_share_t, m_lw_t, m_lo_t, m_tj_t,
  headers = c("share_live_in", "log(Resident)", "log(Nonresident)", "log(Total Jobs)"))
print("=== PA TWFE: OUTCOME FAMILY (muni + SD decomp) ===")
etable(m_share_d, m_lw_d, m_lo_d, m_tj_d,
  headers = c("share_live_in", "log(Resident)", "log(Nonresident)", "log(Total Jobs)"))


# 2. PA Dose-Response: Outcome Family (treated only) ----

pa_treated <- pa_rate[event_type == "increase" & !is.na(cousub_fips) &
  !is.na(rate_change) & rate_change < 5,
  .(first_inc_yr = min(year)), by = cousub_fips]

pa_tp <- merge(pa_samp, pa_treated, by.x = "work_city", by.y = "cousub_fips")
pa_tp[, post := as.integer(year >= first_inc_yr)]

d_share <- feols(share_live_in ~ resident_eit:post | work_city + year,
  data = pa_tp, cluster = ~work_city)
d_lw <- feols(log_live_work ~ resident_eit:post | work_city + year,
  data = pa_tp[!is.na(log_live_work)], cluster = ~work_city)
d_lo <- feols(log_live_out ~ resident_eit:post | work_city + year,
  data = pa_tp[!is.na(log_live_out)], cluster = ~work_city)
d_tj <- feols(log_total ~ resident_eit:post | work_city + year,
  data = pa_tp[!is.na(log_total)], cluster = ~work_city)

print("=== PA DOSE-RESPONSE: OUTCOME FAMILY (treated only) ===")
etable(d_share, d_lw, d_lo, d_tj,
  headers = c("share_live_in", "log(Resident Workers)", "log(Nonresident Workers)", "log(Total Jobs)"))

# Decomposed: post + rate x post
d2_share <- feols(share_live_in ~ post + resident_eit:post | work_city + year,
  data = pa_tp, cluster = ~work_city)
d2_lw <- feols(log_live_work ~ post + resident_eit:post | work_city + year,
  data = pa_tp[!is.na(log_live_work)], cluster = ~work_city)
d2_lo <- feols(log_live_out ~ post + resident_eit:post | work_city + year,
  data = pa_tp[!is.na(log_live_out)], cluster = ~work_city)
d2_tj <- feols(log_total ~ post + resident_eit:post | work_city + year,
  data = pa_tp[!is.na(log_total)], cluster = ~work_city)

print("=== PA DECOMPOSED DOSE-RESPONSE ===")
etable(d2_share, d2_lw, d2_lo, d2_tj,
  headers = c("share_live_in", "log(Resident)", "log(Nonresident)", "log(Total Jobs)"))


# 3. PA Sun-Abraham: Outcome Family ----

pa_samp[, cohort := fifelse(is.na(
  pa_rate[event_type == "increase" & !is.na(cousub_fips),
    .(fiy = min(year)), by = cousub_fips][
    match(pa_samp$work_city, cousub_fips), fiy]),
  10000L, as.integer(
  pa_rate[event_type == "increase" & !is.na(cousub_fips),
    .(fiy = min(year)), by = cousub_fips][
    match(pa_samp$work_city, cousub_fips), fiy]))]

sa_share <- feols(share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_samp, cluster = ~work_city)
sa_lw <- feols(log_live_work ~ sunab(cohort, year) | work_city + year,
  data = pa_samp[!is.na(log_live_work)], cluster = ~work_city)
sa_lo <- feols(log_live_out ~ sunab(cohort, year) | work_city + year,
  data = pa_samp[!is.na(log_live_out)], cluster = ~work_city)
sa_tj <- feols(log_total ~ sunab(cohort, year) | work_city + year,
  data = pa_samp[!is.na(log_total)], cluster = ~work_city)

extract_sa <- function(m, label) {
  ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
  setnames(ct, c("rn", "est", "se", "t", "p"))
  ct[, rel := as.integer(gsub("year::", "", rn))]
  ct[rel >= -3 & rel <= 5, .(label, rel, est = round(est, 5),
    sig = fifelse(p < 0.01, "**", fifelse(p < 0.05, "*", fifelse(p < 0.1, ".", ""))))]
}

print("=== PA SUN-ABRAHAM: OUTCOME FAMILY (t-3 to t+5) ===")
combined <- rbind(
  extract_sa(sa_share, "share"),
  extract_sa(sa_lw, "log(Resident)"),
  extract_sa(sa_lo, "log(Nonresid)"),
  extract_sa(sa_tj, "log(Total)"))
print(dcast(combined, rel ~ label, value.var = "est"))


# 4. OH: Same Outcome Family ----

oh_samp <- oh_panel[total_jobs > 50]

oh_m_share <- feols(share_live_in ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)
oh_m_lw <- feols(log_live_work ~ rate | work_city + year,
  data = oh_samp[!is.na(log_live_work)], cluster = ~work_city)
oh_m_lo <- feols(log_live_out ~ rate | work_city + year,
  data = oh_samp[!is.na(log_live_out)], cluster = ~work_city)
oh_m_tj <- feols(log_total ~ rate | work_city + year,
  data = oh_samp[!is.na(log_total)], cluster = ~work_city)

print("=== OH TWFE: OUTCOME FAMILY ===")
etable(oh_m_share, oh_m_lw, oh_m_lo, oh_m_tj,
  headers = c("share_live_in", "log(Resident Workers)", "log(Nonresident Workers)", "log(Total Jobs)"))


# 5. OH Cleaned Stacked DiD: Outcome Family ----

oh_neighbors <- readRDS(file.path(data_dir, "oh_neighbors.rds"))
oh_treatment_events <- unique(oh_rate[event_type == "increase",
  .(muni_fips, event_year = year, rate_change)])
oh_ctrl_no_tax <- setdiff(oh_neighbors[, unique(neighbor_fips)],
  oh_rate[rate > 0, unique(muni_fips)])

build_stacked <- function(panel, treatment_events, neighbors,
                          rate_panel = NULL, rate_id = "muni_fips") {
  stacked_list <- list()
  for (j in seq_len(nrow(treatment_events))) {
    tf <- treatment_events[[rate_id]][j]
    ey <- treatment_events$event_year[j]
    window <- seq(max(ey - 5L, 2002L), min(ey + 5L, max_year))
    if (length(window) < 6) next
    nbrs <- neighbors[city_fips == tf, neighbor_fips]
    if (length(nbrs) == 0) next
    if (!is.null(rate_panel)) {
      nbr_changes <- rate_panel[get(rate_id) %in% nbrs & event_type == "increase" &
        abs(year - ey) <= 2, unique(get(rate_id))]
    } else {
      nbr_changes <- character(0)
    }
    clean_controls <- setdiff(nbrs, nbr_changes)
    if (length(clean_controls) == 0) next
    cohort_data <- panel[work_city %in% c(tf, clean_controls) & year %in% window]
    if (nrow(cohort_data) == 0) next
    cohort_data[, `:=`(
      cohort_id = paste0(tf, "_", ey),
      treated = work_city == tf,
      rel_year = year - ey,
      cohort_city = paste0(tf, "_", ey, "_", work_city)
    )]
    stacked_list[[j]] <- cohort_data
  }
  rbindlist(stacked_list)
}

oh_stacked <- build_stacked(oh_samp, oh_treatment_events,
  oh_neighbors[neighbor_fips %in% oh_ctrl_no_tax],
  rate_panel = oh_rate, rate_id = "muni_fips")

if (nrow(oh_stacked) > 100) {
  st_share <- feols(share_live_in ~ i(rel_year, treated, ref = -1) | cohort_city + cohort_id^year,
    data = oh_stacked[total_jobs > 50], cluster = ~work_city)
  st_lw <- feols(log_live_work ~ i(rel_year, treated, ref = -1) | cohort_city + cohort_id^year,
    data = oh_stacked[total_jobs > 50 & !is.na(log_live_work)], cluster = ~work_city)
  st_lo <- feols(log_live_out ~ i(rel_year, treated, ref = -1) | cohort_city + cohort_id^year,
    data = oh_stacked[total_jobs > 50 & !is.na(log_live_out)], cluster = ~work_city)
  st_tj <- feols(log_total ~ i(rel_year, treated, ref = -1) | cohort_city + cohort_id^year,
    data = oh_stacked[total_jobs > 50 & !is.na(log_total)], cluster = ~work_city)

  extract_st <- function(m, label) {
    ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
    setnames(ct, c("rn", "est", "se", "t", "p"))
    ct[, rel := as.integer(str_extract(rn, "-?[0-9]+"))]
    ct[rel >= -3 & rel <= 5, .(label, rel, est = round(est, 5),
      sig = fifelse(p < 0.01, "**", fifelse(p < 0.05, "*", fifelse(p < 0.1, ".", ""))))]
  }

  print("=== OH STACKED DiD (NO-TAX CONTROLS): OUTCOME FAMILY ===")
  combined_oh <- rbind(
    extract_st(st_share, "share"),
    extract_st(st_lw, "log(Resident)"),
    extract_st(st_lo, "log(Nonresid)"),
    extract_st(st_tj, "log(Total)"))
  print(dcast(combined_oh, rel ~ label, value.var = "est"))
}

beepr::beep()
