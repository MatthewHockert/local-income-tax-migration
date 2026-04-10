# Robustness Checks
# 1. All PA models with fiscal controls (police, streets, debt, state aid)
# 2. All PA models excluding Philadelphia
# 3. Philadelphia effect on neighbors
# ============================================================================

library(data.table)
library(fixest)

data_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"

pa_panel <- readRDS(file.path(data_dir, "pa_panel.rds"))
pa_panel_high <- readRDS(file.path(data_dir, "pa_panel_high.rds"))
pa_rate_panel <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))
pa_neighbors <- readRDS(file.path(data_dir, "pa_neighbors.rds"))
oh_panel <- readRDS(file.path(data_dir, "oh_panel.rds"))
oh_rate_panel <- readRDS(file.path(data_dir, "oh_rate_panel.rds"))
oh_neighbors <- readRDS(file.path(data_dir, "oh_neighbors.rds"))

# Merge fiscal controls
pa_afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
pa_xwalk <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
pa_afr[, muni_id := fifelse(!is.na(municipality_id), municipality_id, municipal_code)]
pa_afr <- merge(pa_afr, pa_xwalk, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
pa_afr[, yr := fifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]

fiscal <- pa_afr[!is.na(cousub_fips), .(
  cousub_fips, year = yr,
  log_police = fifelse(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)) > 0,
    log(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police))), NA_real_),
  log_streets = fifelse(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
    `public_works-highways_and_streets_expenditures`, streets_and_roads)) > 0,
    log(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
    `public_works-highways_and_streets_expenditures`, streets_and_roads))), NA_real_),
  log_debt = fifelse(as.numeric(total_debt) > 0, log(as.numeric(total_debt)), NA_real_),
  log_state_aid = fifelse(as.numeric(fifelse(!is.na(`intergovernmental_revenues-state_government`),
    `intergovernmental_revenues-state_government`, state_govt)) > 0,
    log(as.numeric(fifelse(!is.na(`intergovernmental_revenues-state_government`),
    `intergovernmental_revenues-state_government`, state_govt))), NA_real_)
)]

pa_panel <- merge(pa_panel, fiscal,
  by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)
rm(pa_afr, fiscal)

# Identify Philadelphia (COUSUBFP = 60000)
philly_fips <- "60000"
philly_neighbors <- pa_neighbors[city_fips == philly_fips, neighbor_fips]

# Samples
pa_full <- pa_panel[total_jobs > 50 & !is.na(resident_eit)]
pa_ctrl <- pa_full[!is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) & !is.na(log_state_aid)]
pa_no_philly <- pa_full[work_city != philly_fips]
pa_no_philly_ctrl <- pa_ctrl[work_city != philly_fips]

# Treatment info
pa_treated <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips) &
  !is.na(rate_change) & rate_change < 5,
  .(first_inc_yr = min(year)), by = cousub_fips]

# Build treated-only samples
merge_treated <- function(panel) {
  tp <- merge(panel, pa_treated, by.x = "work_city", by.y = "cousub_fips")
  tp[, post := as.integer(year >= first_inc_yr)]
  tp
}

pa_tp_full <- merge_treated(pa_full)
pa_tp_ctrl <- merge_treated(pa_ctrl)
pa_tp_nophilly <- merge_treated(pa_no_philly)
pa_tp_nophilly_ctrl <- merge_treated(pa_no_philly_ctrl)

print(paste("Full:", nrow(pa_full), "| With controls:", nrow(pa_ctrl),
  "| No Philly:", nrow(pa_no_philly), "| No Philly + controls:", nrow(pa_no_philly_ctrl)))


# 1. TWFE with and without controls ----

cat("\n=== 1. PA TWFE: BASELINE vs CONTROLS vs NO PHILLY ===\n\n")

m_base <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_full, cluster = ~work_city)
m_ctrl <- feols(share_live_in ~ resident_eit + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_ctrl, cluster = ~work_city)
m_noph <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_no_philly, cluster = ~work_city)
m_noph_ctrl <- feols(share_live_in ~ resident_eit + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_no_philly_ctrl, cluster = ~work_city)

etable(m_base, m_ctrl, m_noph, m_noph_ctrl,
  headers = c("Baseline", "+ Controls", "No Philly", "No Philly + Controls"),
  keep = c("resident_eit"))


# 2. Dose-response with and without controls ----

cat("\n=== 2. PA DOSE-RESPONSE: TREATED ONLY ===\n\n")

d_base <- feols(share_live_in ~ resident_eit:post | work_city + year,
  data = pa_tp_full, cluster = ~work_city)
d_ctrl <- feols(share_live_in ~ resident_eit:post + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_tp_ctrl, cluster = ~work_city)
d_noph <- feols(share_live_in ~ resident_eit:post | work_city + year,
  data = pa_tp_nophilly, cluster = ~work_city)
d_noph_ctrl <- feols(share_live_in ~ resident_eit:post + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_tp_nophilly_ctrl, cluster = ~work_city)

# Decomposed: post + rate x post
d_decomp <- feols(share_live_in ~ post + resident_eit:post | work_city + year,
  data = pa_tp_full, cluster = ~work_city)
d_decomp_ctrl <- feols(share_live_in ~ post + resident_eit:post + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_tp_ctrl, cluster = ~work_city)
d_decomp_noph <- feols(share_live_in ~ post + resident_eit:post | work_city + year,
  data = pa_tp_nophilly, cluster = ~work_city)

etable(d_base, d_ctrl, d_noph, d_noph_ctrl,
  headers = c("Baseline", "+ Controls", "No Philly", "No Philly + Controls"),
  keep = c("resident_eit", "post"))

etable(d_decomp, d_decomp_ctrl, d_decomp_noph,
  headers = c("Post + Rate (base)", "Post + Rate (ctrl)", "Post + Rate (no Philly)"))


# 3. Asymmetry with controls ----

cat("\n=== 3. PA ASYMMETRY ===\n\n")

pa_full[, `:=`(
  has_increase = work_city %in% pa_rate_panel[event_type == "increase" & !is.na(cousub_fips), unique(cousub_fips)],
  has_decrease = work_city %in% pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips), unique(cousub_fips)]
)]
pa_full <- merge(pa_full,
  pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
    .(first_inc = min(year)), by = cousub_fips],
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_full <- merge(pa_full,
  pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
    .(first_dec = min(year)), by = cousub_fips],
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_full[, `:=`(
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

a_base <- feols(share_live_in ~ has_increase:post_inc + has_decrease:post_dec | work_city + year,
  data = pa_full, cluster = ~work_city)
a_ctrl <- feols(share_live_in ~ has_increase:post_inc + has_decrease:post_dec +
  log_police + log_streets + log_debt + log_state_aid | work_city + year,
  data = pa_full[!is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) & !is.na(log_state_aid)],
  cluster = ~work_city)
a_noph <- feols(share_live_in ~ has_increase:post_inc + has_decrease:post_dec | work_city + year,
  data = pa_full[work_city != philly_fips], cluster = ~work_city)

etable(a_base, a_ctrl, a_noph,
  headers = c("Asymmetry (base)", "Asymmetry (ctrl)", "Asymmetry (no Philly)"),
  keep = c("has_increase", "has_decrease"))


# 4. Sun-Abraham with controls ----

cat("\n=== 4. PA SUN-ABRAHAM ===\n\n")

pa_full[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

sa_base <- feols(share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_full, cluster = ~work_city)
sa_noph <- feols(share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_full[work_city != philly_fips], cluster = ~work_city)

# Extract t-3 to t+5
extract <- function(m, label) {
  ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
  setnames(ct, c("rn", "est", "se", "t", "p"))
  ct[, rel := as.integer(gsub("year::", "", rn))]
  ct[rel >= -3 & rel <= 5, .(label, rel, est = round(est, 5), se = round(se, 5),
    sig = fifelse(p < 0.01, "**", fifelse(p < 0.05, "*", fifelse(p < 0.1, ".", ""))))]
}

cat("Sun-Abraham (t-3 to t+5):\n")
combined <- rbind(extract(sa_base, "Full"), extract(sa_noph, "No Philly"))
print(dcast(combined, rel ~ label, value.var = "est"))


# 5. OH with controls ----
# OH doesn't have DCED AFR data, so controls are limited
# But we can run the main specs for comparison

cat("\n=== 5. OH RESULTS (for comparison) ===\n\n")

oh_samp <- oh_panel[total_jobs > 50]
oh_m1 <- feols(share_live_in ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)

# OH treated only
oh_treated <- oh_rate_panel[event_type == "increase",
  .(first_inc_yr = min(year)), by = muni_fips]
oh_tp <- merge(oh_samp, oh_treated, by.x = "work_city", by.y = "muni_fips")
oh_tp[, post := as.integer(year >= first_inc_yr)]

oh_d1 <- feols(share_live_in ~ rate:post | work_city + year,
  data = oh_tp, cluster = ~work_city)
oh_d2 <- feols(share_live_in ~ post + rate:post | work_city + year,
  data = oh_tp, cluster = ~work_city)

etable(oh_m1, oh_d1, oh_d2,
  headers = c("OH TWFE", "OH Rate x Post", "OH Post + Rate x Post"))


# 6. Summary table ----

cat("\n=== 6. COMBINED SUMMARY: MAIN COEFFICIENT ACROSS SPECIFICATIONS ===\n\n")

etable(m_base, m_ctrl, m_noph, d_base, d_ctrl, d_noph,
  headers = c("TWFE", "TWFE+Ctrl", "TWFE-Philly",
    "Dose", "Dose+Ctrl", "Dose-Philly"),
  keep = c("resident_eit"))

beepr::beep()
