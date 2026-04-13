# Robustness Checks
# 1. All PA models with fiscal controls (police, streets, debt, state aid)
# 2. All PA models excluding Philadelphia
# 3. Asymmetry with controls
# 4. Sun-Abraham with controls
# 5. OH comparison
# 6. Combined summary
# 7. Act 32 structural break on fiscal outcomes
# 8. Act 32 structural break on all Stage 2 models

library(data.table)
library(fixest)

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

data_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"

pa_panel <- readRDS(file.path(data_dir, "pa_panel.rds"))
pa_panel_high <- readRDS(file.path(data_dir, "pa_panel_high.rds"))
pa_rate_panel <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))
pa_neighbors <- readRDS(file.path(data_dir, "pa_neighbors.rds"))
oh_panel <- readRDS(file.path(data_dir, "oh_panel.rds"))
oh_rate_panel <- readRDS(file.path(data_dir, "oh_rate_panel.rds"))
oh_neighbors <- readRDS(file.path(data_dir, "oh_neighbors.rds"))

## Merge fiscal controls ----
pa_afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
pa_xwalk <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
pa_afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
pa_afr <- merge(pa_afr, pa_xwalk, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
pa_afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]

fiscal_controls <- pa_afr[!is.na(cousub_fips), .(
  cousub_fips, year = yr,
  log_police = ifelse(as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police)) > 0,
    log(as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))), NA_real_),
  log_streets = ifelse(as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
    `public_works-highways_and_streets_expenditures`, streets_and_roads)) > 0,
    log(as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
    `public_works-highways_and_streets_expenditures`, streets_and_roads))), NA_real_),
  log_debt = ifelse(as.numeric(total_debt) > 0, log(as.numeric(total_debt)), NA_real_),
  log_state_aid = ifelse(as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
    `intergovernmental_revenues-state_government`, state_govt)) > 0,
    log(as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
    `intergovernmental_revenues-state_government`, state_govt))), NA_real_)
)]

pa_panel <- merge(pa_panel, fiscal_controls,
  by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)
rm(pa_afr, fiscal_controls)

# Philadelphia identification
philly_fips <- "60000"
philly_neighbors <- pa_neighbors[city_fips == philly_fips, neighbor_fips]

## Sample construction ----
pa_full_sample <- pa_panel[total_jobs > 50 & !is.na(resident_eit)]
pa_ctrl_sample <- pa_full_sample[!is.na(log_police) & !is.na(log_streets) &
  !is.na(log_debt) & !is.na(log_state_aid)]
pa_nophilly_sample <- pa_full_sample[work_city != philly_fips]
pa_nophilly_ctrl_sample <- pa_ctrl_sample[work_city != philly_fips]

# Treatment timing for dose-response
pa_treated <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips) &
  !is.na(rate_change) & rate_change < 5,
  .(first_inc_yr = min(year)), by = cousub_fips]

merge_treated <- function(panel) {
  tp <- merge(panel, pa_treated, by.x = "work_city", by.y = "cousub_fips")
  tp[, post := as.integer(year >= first_inc_yr)]
  tp
}

pa_treated_full <- merge_treated(pa_full_sample)
pa_treated_ctrl <- merge_treated(pa_ctrl_sample)
pa_treated_nophilly <- merge_treated(pa_nophilly_sample)
pa_treated_nophilly_ctrl <- merge_treated(pa_nophilly_ctrl_sample)

print(paste("Full:", nrow(pa_full_sample), "| With controls:", nrow(pa_ctrl_sample),
  "| No Philly:", nrow(pa_nophilly_sample), "| No Philly + controls:", nrow(pa_nophilly_ctrl_sample)))


# 1. TWFE with and without controls ----

m_base <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_full_sample, cluster = ~work_city)
m_ctrl <- feols(share_live_in ~ resident_eit + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_ctrl_sample, cluster = ~work_city)
m_noph <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_nophilly_sample, cluster = ~work_city)
m_noph_ctrl <- feols(share_live_in ~ resident_eit + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_nophilly_ctrl_sample, cluster = ~work_city)

etable(m_base, m_ctrl, m_noph, m_noph_ctrl,
  headers = c("Baseline", "+ Controls", "No Philly", "No Philly + Controls"),
  keep = c("resident_eit"))


# 2. Dose-response with and without controls ----

d_base <- feols(share_live_in ~ resident_eit:post | work_city + year,
  data = pa_treated_full, cluster = ~work_city)
d_ctrl <- feols(share_live_in ~ resident_eit:post + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_treated_ctrl, cluster = ~work_city)
d_noph <- feols(share_live_in ~ resident_eit:post | work_city + year,
  data = pa_treated_nophilly, cluster = ~work_city)
d_noph_ctrl <- feols(share_live_in ~ resident_eit:post + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_treated_nophilly_ctrl, cluster = ~work_city)

# Decomposed: post + rate x post
d_decomp <- feols(share_live_in ~ post + resident_eit:post | work_city + year,
  data = pa_treated_full, cluster = ~work_city)
d_decomp_ctrl <- feols(share_live_in ~ post + resident_eit:post + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_treated_ctrl, cluster = ~work_city)
d_decomp_noph <- feols(share_live_in ~ post + resident_eit:post | work_city + year,
  data = pa_treated_nophilly, cluster = ~work_city)

etable(d_base, d_ctrl, d_noph, d_noph_ctrl,
  headers = c("Baseline", "+ Controls", "No Philly", "No Philly + Controls"),
  keep = c("resident_eit", "post"))

etable(d_decomp, d_decomp_ctrl, d_decomp_noph,
  headers = c("Post + Rate (base)", "Post + Rate (ctrl)", "Post + Rate (no Philly)"))


# 3. Asymmetry with controls ----

pa_full_sample[, `:=`(
  has_increase = work_city %in% pa_rate_panel[event_type == "increase" & !is.na(cousub_fips), unique(cousub_fips)],
  has_decrease = work_city %in% pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips), unique(cousub_fips)]
)]
pa_full_sample <- merge(pa_full_sample,
  pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
    .(first_inc = min(year)), by = cousub_fips],
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_full_sample <- merge(pa_full_sample,
  pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
    .(first_dec = min(year)), by = cousub_fips],
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_full_sample[, `:=`(
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

a_base <- feols(share_live_in ~ has_increase:post_inc + has_decrease:post_dec | work_city + year,
  data = pa_full_sample, cluster = ~work_city)
a_ctrl <- feols(share_live_in ~ has_increase:post_inc + has_decrease:post_dec +
  log_police + log_streets + log_debt + log_state_aid | work_city + year,
  data = pa_full_sample[!is.na(log_police) & !is.na(log_streets) &
    !is.na(log_debt) & !is.na(log_state_aid)],
  cluster = ~work_city)
a_noph <- feols(share_live_in ~ has_increase:post_inc + has_decrease:post_dec | work_city + year,
  data = pa_full_sample[work_city != philly_fips], cluster = ~work_city)

etable(a_base, a_ctrl, a_noph,
  headers = c("Asymmetry (base)", "Asymmetry (ctrl)", "Asymmetry (no Philly)"),
  keep = c("has_increase", "has_decrease"))


# 4. Sun-Abraham with controls ----

pa_full_sample[, cohort := ifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

sa_base <- feols(share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_full_sample, cluster = ~work_city)
sa_noph <- feols(share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_full_sample[work_city != philly_fips], cluster = ~work_city)

# Extract t-3 to t+5
extract_sa_coefs <- function(m, label) {
  ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
  setnames(ct, c("rn", "est", "se", "t", "p"))
  ct[, rel := as.integer(gsub("year::", "", rn))]
  ct[rel >= -3 & rel <= 5, .(label, rel, est = round(est, 5), se = round(se, 5),
    sig = ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", ""))))]
}

sa_combined <- rbind(
  extract_sa_coefs(sa_base, "Full"),
  extract_sa_coefs(sa_noph, "No Philly"))
print(dcast(sa_combined, rel ~ label, value.var = "est"))


# 5. OH comparison ----
# OH doesn't have DCED AFR data, so controls are limited

oh_full_sample <- oh_panel[total_jobs > 50]
oh_m1 <- feols(share_live_in ~ rate | work_city + year,
  data = oh_full_sample, cluster = ~work_city)

# OH treated only
oh_treated <- oh_rate_panel[event_type == "increase",
  .(first_inc_yr = min(year)), by = muni_fips]
oh_treated_sample <- merge(oh_full_sample, oh_treated, by.x = "work_city", by.y = "muni_fips")
oh_treated_sample[, post := as.integer(year >= first_inc_yr)]

oh_d1 <- feols(share_live_in ~ rate:post | work_city + year,
  data = oh_treated_sample, cluster = ~work_city)
oh_d2 <- feols(share_live_in ~ post + rate:post | work_city + year,
  data = oh_treated_sample, cluster = ~work_city)

etable(oh_m1, oh_d1, oh_d2,
  headers = c("OH TWFE", "OH Rate x Post", "OH Post + Rate x Post"))


# 6. Summary table ----

etable(m_base, m_ctrl, m_noph, d_base, d_ctrl, d_noph,
  headers = c("TWFE", "TWFE+Ctrl", "TWFE-Philly",
    "Dose", "Dose+Ctrl", "Dose-Philly"),
  keep = c("resident_eit"))


# 7. Act 32 structural break: fiscal outcomes ----
# Act 32 (effective 2012) consolidated EIT collection into county TCDs with
# mandatory employer withholding. If enforcement made the tax binding, the same
# statutory rate should generate more EIT revenue post-2012.
# Model: Y_it = alpha_i + gamma_t + beta*tau_it + delta*(tau_it * post2012) + eps_it
# beta = rate effect (whole panel), delta = additional enforcement effect post-2012

pa_afr_full <- fread(file.path(data_dir, "pa_afr_panel.csv"))
pa_afr_full[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
pa_afr_full <- merge(pa_afr_full, pa_xwalk, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
pa_afr_full[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]

pa_rate_fiscal <- unique(pa_rate_panel[!is.na(cousub_fips),
  .(cousub_fips, year, resident_eit, first_increase_year)])
pa_afr_full <- merge(pa_afr_full, pa_rate_fiscal,
  by.x = c("cousub_fips", "yr"), by.y = c("cousub_fips", "year"), all.x = TRUE)

pa_afr_full[, `:=`(
  eit_rev = as.numeric(ifelse(!is.na(earned_income_tax_revenues),
    earned_income_tax_revenues, earned_income_tax)),
  re_rev = as.numeric(ifelse(!is.na(real_estate_tax_revenues),
    real_estate_tax_revenues, real_estate_tax)),
  tot_rev = as.numeric(ifelse(!is.na(total_revenues), total_revenues, total_revenue)),
  tot_tax = as.numeric(ifelse(!is.na(total_taxes_revenues), total_taxes_revenues, total_taxes))
)]

pa_afr_full[, `:=`(
  log_eit = ifelse(eit_rev > 0, log(eit_rev), NA_real_),
  log_re  = ifelse(re_rev > 0, log(re_rev), NA_real_),
  log_rev = ifelse(tot_rev > 0, log(tot_rev), NA_real_),
  log_tax = ifelse(tot_tax > 0, log(tot_tax), NA_real_)
)]

fiscal_sample <- pa_afr_full[!is.na(cousub_fips) & !is.na(resident_eit) & tot_rev > 0 & yr <= 2019]
fiscal_sample[, post_act32 := as.integer(yr >= 2012)]

print(paste("Fiscal sample:", nrow(fiscal_sample), "municipality-years"))
print(paste("Pre-Act 32:", sum(fiscal_sample$post_act32 == 0),
  "| Post-Act 32:", sum(fiscal_sample$post_act32 == 1)))

# Interaction model: base rate effect + post-Act 32 increment
act32_eit <- feols(log_eit ~ resident_eit + resident_eit:post_act32 | cousub_fips + yr,
  data = fiscal_sample[!is.na(log_eit)], cluster = ~cousub_fips)
act32_tax <- feols(log_tax ~ resident_eit + resident_eit:post_act32 | cousub_fips + yr,
  data = fiscal_sample[!is.na(log_tax)], cluster = ~cousub_fips)
act32_rev <- feols(log_rev ~ resident_eit + resident_eit:post_act32 | cousub_fips + yr,
  data = fiscal_sample, cluster = ~cousub_fips)
act32_re <- feols(log_re ~ resident_eit + resident_eit:post_act32 | cousub_fips + yr,
  data = fiscal_sample[!is.na(log_re)], cluster = ~cousub_fips)

etable(act32_eit, act32_tax, act32_rev, act32_re,
  headers = c("log(EIT Rev)", "log(Total Tax)", "log(Total Rev)", "log(RE Tax)"),
  keep = c("resident_eit", "post_act32"))

rm(pa_afr_full, fiscal_sample)


# 8. Act 32 structural break: Stage 2 decomposition ----
# Unified test of whether Act 32 enforcement drives the tract-level results.
# Each model: Y = alpha + gamma_t + beta*tau + delta*(tau * post2012) + eps
# Requires intermediate panels saved by the master script (Section 21):
#   pa_city_pair_od.rds, pa_resident_panel.rds, pa_tract_panel.rds

#### 8a. Q4 resident worker panel (complete relocation) ----
# Does own rate reduce resident workers? Does neighbor exposure boost them?
# Channel 2 (complete relocation) should only appear post-Act 32 if tax-motivated.

pa_resident_panel <- readRDS(file.path(data_dir, "pa_resident_panel.rds"))
pa_resident_panel[, post_act32 := as.integer(year >= 2012)]
pa_resident_sample <- pa_resident_panel[total_jobs > 50 & !is.na(own_eit)]

print(paste("8a. Q4 resident workers:", nrow(pa_resident_sample), "city-years"))

# Baseline (no split)
q4_base <- feols(log_res_workers ~ own_eit + nbr_avg_rate | work_city + year,
  data = pa_resident_sample[!is.na(nbr_avg_rate)], cluster = ~work_city)

# Act 32 split: own rate
q4_own_act32 <- feols(log_res_workers ~ own_eit + own_eit:post_act32 +
  nbr_avg_rate | work_city + year,
  data = pa_resident_sample[!is.na(nbr_avg_rate)], cluster = ~work_city)

# Act 32 split: own rate + neighbor exposure
q4_full_act32 <- feols(log_res_workers ~ own_eit + own_eit:post_act32 +
  nbr_avg_rate + nbr_avg_rate:post_act32 | work_city + year,
  data = pa_resident_sample[!is.na(nbr_avg_rate)], cluster = ~work_city)

etable(q4_base, q4_own_act32, q4_full_act32,
  headers = c("Baseline", "Own Rate x Act 32", "Full x Act 32"),
  keep = c("own_eit", "nbr_avg_rate", "post_act32"))

rm(pa_resident_panel, pa_resident_sample)


#### 8b. Q4 commuter pair panel (destination choice) ----
# When A raises rate, do flows to B increase? Does home rate matter?
# Under credit system, home rate is irrelevant for stayers (Prediction 2).
# Post-Act 32, this should be cleanly enforced.

pa_pair <- readRDS(file.path(data_dir, "pa_city_pair_od.rds"))
pa_pair[, post_act32 := as.integer(year >= 2012)]

pa_commuter_sample <- pa_pair[is_resident == FALSE & !is.na(dist_km) & dist_km <= 80 &
  !is.na(w_eit) & !is.na(h_eit) & workers > 0]

print(paste("8b. Q4 commuter pairs:", nrow(pa_commuter_sample), "pair-years"))

# Baseline
q4_pair_base <- feols(log_workers ~ w_eit + h_eit | pair_id + year,
  data = pa_commuter_sample, cluster = ~work_city)

# Act 32 split: work + home rates
q4_pair_act32 <- feols(log_workers ~ w_eit + w_eit:post_act32 +
  h_eit + h_eit:post_act32 | pair_id + year,
  data = pa_commuter_sample, cluster = ~work_city)

# Act 32 split: rate gap interaction
q4_gap_act32 <- feols(log_workers ~ w_eit + w_eit:post_act32 +
  w_eit:I(w_eit - h_eit) + w_eit:I(w_eit - h_eit):post_act32 | pair_id + year,
  data = pa_commuter_sample, cluster = ~work_city)

etable(q4_pair_base, q4_pair_act32, q4_gap_act32,
  headers = c("Baseline", "Work + Home x Act 32", "Rate Gap x Act 32"),
  keep = c("w_eit", "h_eit", "post_act32", "w_eit - h_eit"))

rm(pa_commuter_sample)


#### 8c. Q5 school district rate decomposition x Act 32 ----
# Municipal and SD rates vary independently (corr = -0.12).
# Prediction 4: muni rates repel, SD rates attract (amenity signal).
# Post-Act 32, muni effect should strengthen; SD effect may be stable.

pa_resident_panel <- readRDS(file.path(data_dir, "pa_resident_panel.rds"))
pa_resident_panel[, post_act32 := as.integer(year >= 2012)]
pa_sd_sample <- pa_resident_panel[total_jobs > 50 & !is.na(own_eit) & !is.na(own_sd)]

print(paste("8c. Q5 school district:", nrow(pa_sd_sample), "city-years with SD rate data"))

# Baseline: muni + SD on resident workers
q5_base <- feols(log_res_workers ~ own_eit + own_sd | work_city + year,
  data = pa_sd_sample, cluster = ~work_city)

# Act 32 split: muni + SD
q5_act32 <- feols(log_res_workers ~ own_eit + own_eit:post_act32 +
  own_sd + own_sd:post_act32 | work_city + year,
  data = pa_sd_sample, cluster = ~work_city)

etable(q5_base, q5_act32,
  headers = c("Muni + SD (baseline)", "Muni + SD x Act 32"),
  keep = c("own_eit", "own_sd", "post_act32"))

# Commuter pairs: muni + SD decomposition with Act 32
pa_commuter_sd_sample <- pa_pair[is_resident == FALSE & !is.na(dist_km) & dist_km <= 80 &
  !is.na(w_eit) & !is.na(h_eit) & !is.na(w_sd) & !is.na(h_sd) & workers > 0]

q5_pair_base <- feols(log_workers ~ w_eit + w_sd + h_eit + h_sd | pair_id + year,
  data = pa_commuter_sd_sample, cluster = ~work_city)

q5_pair_act32 <- feols(log_workers ~ w_eit + w_eit:post_act32 +
  w_sd + w_sd:post_act32 +
  h_eit + h_eit:post_act32 +
  h_sd + h_sd:post_act32 | pair_id + year,
  data = pa_commuter_sd_sample, cluster = ~work_city)

etable(q5_pair_base, q5_pair_act32,
  headers = c("Full Decomp (baseline)", "Full Decomp x Act 32"),
  keep = c("w_eit", "w_sd", "h_eit", "h_sd", "post_act32"))

rm(pa_resident_panel, pa_sd_sample, pa_commuter_sd_sample, pa_pair)


#### 8d. Q6 boundary gradient x Act 32 ----
# Prediction 5: sorting concentrates at municipal boundaries.
# Post-Act 32, the boundary gradient should sharpen if sorting is tax-motivated.

pa_tract_panel <- readRDS(file.path(data_dir, "pa_tract_panel.rds"))
pa_tract_panel[, post_act32 := as.integer(year >= 2012)]
pa_tract_sample <- pa_tract_panel[total_workers > 10 & !is.na(muni_eit) & !is.na(is_boundary)]

print(paste("8d. Q6 boundary gradient:", nrow(pa_tract_sample), "tract-years"))
print(paste("Boundary tracts:", sum(pa_tract_sample$is_boundary, na.rm = TRUE),
  "| Interior:", sum(!pa_tract_sample$is_boundary, na.rm = TRUE)))

# Baseline: boundary interaction
q6_base <- feols(share_within_city ~ muni_eit + muni_eit:is_boundary | w_tract + year,
  data = pa_tract_sample, cluster = ~work_city)

# Act 32 split: rate + boundary + Act 32
q6_act32 <- feols(share_within_city ~ muni_eit + muni_eit:post_act32 +
  muni_eit:is_boundary + muni_eit:is_boundary:post_act32 | w_tract + year,
  data = pa_tract_sample, cluster = ~work_city)

etable(q6_base, q6_act32,
  headers = c("Boundary (baseline)", "Boundary x Act 32"),
  keep = c("muni_eit", "is_boundary", "post_act32"))

# Distance gradient: separate regressions by period
pa_tract_sample[, dist_bin := cut(dist_to_boundary_m / 1000,
  breaks = c(0, 0.5, 1, 2, 5, Inf),
  labels = c("0-0.5km", "0.5-1km", "1-2km", "2-5km", "5+km"))]

q6_gradient_pre <- feols(share_within_city ~ muni_eit:dist_bin | w_tract + year,
  data = pa_tract_sample[post_act32 == 0 & !is.na(dist_bin)], cluster = ~work_city)
q6_gradient_post <- feols(share_within_city ~ muni_eit:dist_bin | w_tract + year,
  data = pa_tract_sample[post_act32 == 1 & !is.na(dist_bin)], cluster = ~work_city)

etable(q6_gradient_pre, q6_gradient_post,
  headers = c("Pre-Act 32 (2002-2011)", "Post-Act 32 (2012-2019)"))

rm(pa_tract_panel, pa_tract_sample)

beepr::beep()
