# Local Income Tax Migration Analysis
# Do local income tax changes cause residential sorting at municipal borders?
# Uses LODES Origin-Destination data: share of workers living in their work city
# Setting: Ohio (incorporated places) and Pennsylvania (county subdivisions)
#
# Run order:
#   1. python3 scrape_pa_dced_eit.py
#   2. Rscript download_lodes.R
#   3. Rscript build_data.R
#   4. This script (loads pre-built RDS files)
# ============================================================================

library(sf)
library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)
library(tigris)
library(stringr)

options(tigris_use_cache = TRUE)

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")


# 1. Load Data ----

# Trim to 2019 to avoid COVID contamination in LODES 2020-2021
max_year <- 2019

oh_panel <- readRDS(file.path(data_dir, "oh_panel.rds"))[year <= max_year]
oh_panel_high <- readRDS(file.path(data_dir, "oh_panel_high.rds"))[year <= max_year]
pa_panel <- readRDS(file.path(data_dir, "pa_panel.rds"))[year <= max_year]
pa_panel_high <- readRDS(file.path(data_dir, "pa_panel_high.rds"))[year <= max_year]
oh_rate_panel <- readRDS(file.path(data_dir, "oh_rate_panel.rds"))
pa_rate_panel <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))
oh_neighbors <- readRDS(file.path(data_dir, "oh_neighbors.rds"))
pa_neighbors <- readRDS(file.path(data_dir, "pa_neighbors.rds"))
oh_places <- readRDS(file.path(data_dir, "oh_places.rds"))
pa_cousub <- readRDS(file.path(data_dir, "pa_cousub.rds"))
tf_rates <- readRDS(file.path(data_dir, "tf_rates.rds"))

## Merge fiscal controls into PA panel ----
# From DCED AFR: police, streets, debt, state aid expenditures
pa_afr_file <- file.path(data_dir, "pa_afr_panel.csv")
if (file.exists(pa_afr_file)) {
  pa_afr_raw <- fread(pa_afr_file)
  pa_xwalk_afr <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  pa_afr_raw[, muni_id := fifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  pa_afr_raw <- merge(pa_afr_raw, pa_xwalk_afr, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  pa_afr_raw[, yr := fifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]

  fiscal_controls <- pa_afr_raw[!is.na(cousub_fips), .(
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

  pa_panel <- merge(pa_panel, fiscal_controls,
    by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)
  pa_panel_high <- merge(pa_panel_high, fiscal_controls,
    by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

  print(paste("PA panel with fiscal controls:", sum(!is.na(pa_panel$log_police)), "of", nrow(pa_panel)))
  rm(pa_afr_raw, fiscal_controls)
}

## Summary stats ----
print(paste("OH panel:", nrow(oh_panel), "city-years,",
  uniqueN(oh_panel$work_city), "cities"))
print(paste("PA panel:", nrow(pa_panel), "city-years,",
  uniqueN(pa_panel$work_city), "municipalities"))
print(paste("OH mean share_live_in:", round(mean(oh_panel$share_live_in, na.rm = TRUE), 3)))
print(paste("PA mean share_live_in:", round(mean(pa_panel$share_live_in, na.rm = TRUE), 3)))
print(paste("OH rate increases:", oh_rate_panel[event_type == "increase", .N]))
print(paste("PA rate increases:", pa_rate_panel[event_type == "increase", .N]))


# 2. Maps ----

## Ohio: cities colored by 2021 tax rate, outlined if ever raised ----
oh_places_rates <- oh_places %>%
  left_join(
    oh_rate_panel[year == 2021, .(muni_fips, rate)],
    by = c("PLACEFP" = "muni_fips")
  ) %>%
  mutate(
    has_increase = PLACEFP %in% oh_rate_panel[event_type == "increase", unique(muni_fips)],
    rate_pct = rate * 100,
    city_type = case_when(
      is.na(rate) ~ "No tax data",
      has_increase ~ "Treated (raised rate)",
      TRUE ~ "Untreated"
    )
  )

oh_state <- states(year = 2020) %>% filter(STATEFP == "39") %>% st_transform(4326)

p_oh_map <- ggplot() +
  geom_sf(data = oh_state, fill = "grey95", color = "grey50") +
  geom_sf(data = oh_places_rates %>% filter(city_type == "No tax data"),
    fill = "grey80", color = "grey60", linewidth = 0.05) +
  geom_sf(data = oh_places_rates %>% filter(city_type == "Untreated"),
    aes(fill = rate_pct), color = "grey60", linewidth = 0.05) +
  geom_sf(data = oh_places_rates %>% filter(city_type == "Treated (raised rate)"),
    aes(fill = rate_pct), color = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Tax Rate (%)", na.value = "grey85") +
  labs(title = "Ohio Municipal Income Tax Rates (2021)",
    subtitle = "Bold outline = raised rates; thin outline = untreated; grey fill = no municipal tax") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
print(p_oh_map)

## PA: municipalities colored by whether they changed EIT rate ----
pa_ever_changed <- pa_rate_panel[!is.na(cousub_fips) & !is.na(event_type) & event_type != "no_change",
  .(
    n_increases = sum(event_type == "increase"),
    n_decreases = sum(event_type == "decrease"),
    total_change = sum(rate_change, na.rm = TRUE)
  ), by = cousub_fips]

pa_cousub_changes <- pa_cousub %>%
  left_join(pa_ever_changed, by = c("COUSUBFP" = "cousub_fips")) %>%
  mutate(
    change_type = case_when(
      n_increases > 0 & n_decreases > 0 ~ "Both",
      n_increases > 0 ~ "Increased",
      n_decreases > 0 ~ "Decreased",
      TRUE ~ "No change"
    )
  )

pa_state <- states(year = 2020) %>% filter(STATEFP == "42") %>% st_transform(4326)

p_pa_map <- ggplot() +
  geom_sf(data = pa_state, fill = "grey95", color = "grey50") +
  geom_sf(data = pa_cousub_changes %>% filter(change_type == "No change"),
    fill = "grey85", color = "grey70", linewidth = 0.02) +
  geom_sf(data = pa_cousub_changes %>% filter(change_type == "Increased"),
    fill = "firebrick", color = "grey50", linewidth = 0.05, alpha = 0.7) +
  geom_sf(data = pa_cousub_changes %>% filter(change_type == "Decreased"),
    fill = "steelblue", color = "grey50", linewidth = 0.05, alpha = 0.7) +
  geom_sf(data = pa_cousub_changes %>% filter(change_type == "Both"),
    fill = "darkorchid", color = "grey50", linewidth = 0.05, alpha = 0.7) +
  labs(title = "Pennsylvania Municipal EIT Rate Changes (2000-2021)",
    subtitle = "Red = increased, Blue = decreased, Purple = both, Grey = no change") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
print(p_pa_map)


# 3. Rate Change Distributions ----

## Histogram of rate changes: OH ----
oh_changes <- oh_rate_panel[!is.na(rate_change) & abs(rate_change) > 1e-6]
oh_changes[, rate_change_pp := rate_change * 100]

p_oh_hist <- ggplot(oh_changes, aes(x = rate_change_pp)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Rate Change (pp)", y = "Count",
    title = "OH: Distribution of Municipal Income Tax Rate Changes",
    subtitle = paste0("N = ", nrow(oh_changes), " changes across ",
      uniqueN(oh_changes$muni_fips), " municipalities")) +
  theme_minimal()
print(p_oh_hist)

## Histogram of rate changes: PA ----
pa_changes <- pa_rate_panel[!is.na(rate_change) & abs(rate_change) > 1e-6]

p_pa_hist <- ggplot(pa_changes, aes(x = rate_change)) +
  geom_histogram(binwidth = 0.05, fill = "firebrick", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Rate Change (pp)", y = "Count",
    title = "PA: Distribution of Municipal EIT Rate Changes",
    subtitle = paste0("N = ", nrow(pa_changes), " changes across ",
      uniqueN(pa_changes$municipality_id), " municipalities")) +
  theme_minimal()
print(p_pa_hist)

## Cumulative adoption: cities going from no tax to having a tax ----
# OH: first year each municipality appears in the rate panel with rate > 0
oh_adoption <- oh_rate_panel[rate > 0, .(adoption_year = min(year)), by = muni_fips]
oh_adoption_cumul <- oh_adoption[, .N, by = adoption_year][order(adoption_year)]
oh_adoption_cumul[, cumul_cities := cumsum(N)]
oh_adoption_cumul[, state := "Ohio"]

# PA: first year each municipality has a non-zero EIT
pa_adoption <- pa_rate_panel[!is.na(resident_eit) & resident_eit > 0,
  .(adoption_year = min(year)), by = municipality_id]
pa_adoption_cumul <- pa_adoption[, .N, by = adoption_year][order(adoption_year)]
pa_adoption_cumul[, cumul_cities := cumsum(N)]
pa_adoption_cumul[, state := "Pennsylvania"]

p_oh_adoption <- ggplot(oh_adoption_cumul, aes(x = adoption_year, y = cumul_cities)) +
  geom_step(linewidth = 1, color = "steelblue") +
  geom_point(size = 1.5, color = "steelblue") +
  labs(x = "Year", y = "Cumulative Cities with Local Income Tax",
    title = "OH: Adoption of Municipal Income Taxes",
    subtitle = "First year municipality observed with positive rate in data") +
  theme_minimal()
print(p_oh_adoption)

p_pa_adoption <- ggplot(pa_adoption_cumul, aes(x = adoption_year, y = cumul_cities)) +
  geom_step(linewidth = 1, color = "firebrick") +
  geom_point(size = 1.5, color = "firebrick") +
  labs(x = "Year", y = "Cumulative Municipalities with EIT",
    title = "PA: Adoption of Municipal Earned Income Taxes",
    subtitle = "First year municipality observed with positive rate in data") +
  theme_minimal()
print(p_pa_adoption)


# 4. Treated vs Control OD Over Time ----

## OH: treated = cities that ever raised rate; control = cities that never changed ----
oh_ever_treated <- oh_rate_panel[event_type == "increase", unique(muni_fips)]
oh_never_changed <- oh_rate_panel[, .(any_change = any(event_type %in% c("increase", "decrease"),
  na.rm = TRUE)), by = muni_fips][any_change == FALSE, muni_fips]

oh_panel[, city_group := fcase(
  work_city %in% oh_ever_treated, "Treated (raised rate)",
  work_city %in% oh_never_changed, "Control (never changed)",
  default = NA_character_
)]

oh_od_trends <- oh_panel[!is.na(city_group) & total_jobs > 50,
  .(mean_share = mean(share_live_in, na.rm = TRUE),
    mean_log_live_in = mean(log_live_in, na.rm = TRUE),
    mean_log_total = mean(log_total_jobs, na.rm = TRUE),
    n_cities = uniqueN(work_city)),
  by = .(year, city_group)]

p_oh_od_share <- ggplot(oh_od_trends, aes(x = year, y = mean_share, color = city_group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(x = "Year", y = "Mean Share of Workers Living in City",
    title = "OH: Share Living in City — Treated vs Control",
    subtitle = "Treated = ever raised rate; Control = never changed rate",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_oh_od_share)

p_oh_od_log <- ggplot(oh_od_trends, aes(x = year, y = mean_log_live_in, color = city_group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(x = "Year", y = "Mean log(Workers Living in City)",
    title = "OH: Workers Living in City — Treated vs Control",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_oh_od_log)

## PA: treated = municipalities that ever raised EIT; control = never changed ----
pa_ever_treated <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_never_changed <- pa_rate_panel[!is.na(cousub_fips),
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = cousub_fips][any_change == FALSE, cousub_fips]

pa_panel[, city_group := fcase(
  work_city %in% pa_ever_treated, "Treated (raised rate)",
  work_city %in% pa_never_changed, "Control (never changed)",
  default = NA_character_
)]

pa_od_trends <- pa_panel[!is.na(city_group) & total_jobs > 50,
  .(mean_share = mean(share_live_in, na.rm = TRUE),
    mean_log_live_in = mean(log_live_in, na.rm = TRUE),
    mean_log_total = mean(log_total_jobs, na.rm = TRUE),
    n_cities = uniqueN(work_city)),
  by = .(year, city_group)]

p_pa_od_share <- ggplot(pa_od_trends, aes(x = year, y = mean_share, color = city_group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(x = "Year", y = "Mean Share of Workers Living in Municipality",
    title = "PA: Share Living in Municipality — Treated vs Control",
    subtitle = "Treated = ever raised EIT; Control = never changed EIT",
    color = NULL) +
  scale_color_manual(values = c("Treated (raised rate)" = "firebrick",
    "Control (never changed)" = "grey40")) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_pa_od_share)

p_pa_od_log <- ggplot(pa_od_trends, aes(x = year, y = mean_log_live_in, color = city_group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(x = "Year", y = "Mean log(Workers Living in Municipality)",
    title = "PA: Workers Living in Municipality — Treated vs Control",
    color = NULL) +
  scale_color_manual(values = c("Treated (raised rate)" = "firebrick",
    "Control (never changed)" = "grey40")) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_pa_od_log)


## OH: Normalized to event time (share_live_in indexed to t = -1) ----
# For each treated city, compute share relative to the year before first rate change
oh_first_change <- oh_rate_panel[event_type == "increase",
  .(event_year = min(year)), by = muni_fips]

oh_event_norm <- merge(oh_panel[total_jobs > 50], oh_first_change,
  by.x = "work_city", by.y = "muni_fips", all.x = FALSE)
oh_event_norm[, rel_year := year - event_year]
oh_event_norm <- oh_event_norm[abs(rel_year) <= 5]

# Index share_live_in to t = -1 value for each city
oh_baseline <- oh_event_norm[rel_year == -1, .(work_city, baseline_share = share_live_in)]
oh_event_norm <- merge(oh_event_norm, oh_baseline, by = "work_city")
oh_event_norm[, share_indexed := share_live_in - baseline_share]

oh_event_norm_agg <- oh_event_norm[, .(
  mean_indexed = mean(share_indexed, na.rm = TRUE),
  se = sd(share_indexed, na.rm = TRUE) / sqrt(.N),
  n_cities = uniqueN(work_city)
), by = rel_year]

# Build control group: neighbors of treated cities, same event timing
oh_ctrl_data <- oh_panel[total_jobs > 50, .(work_city, year, share_live_in)]
setnames(oh_ctrl_data, "work_city", "neighbor_fips")

oh_control_events <- merge(
  oh_first_change[, .(muni_fips, event_year)],
  oh_neighbors[, .(city_fips, neighbor_fips)],
  by.x = "muni_fips", by.y = "city_fips", allow.cartesian = TRUE
)
oh_control_events <- merge(oh_control_events, oh_ctrl_data,
  by = "neighbor_fips", allow.cartesian = TRUE)
oh_control_events[, rel_year := year - event_year]
oh_control_events <- oh_control_events[abs(rel_year) <= 5]

oh_ctrl_baseline <- oh_control_events[rel_year == -1,
  .(neighbor_fips, muni_fips, baseline_share = share_live_in)]
oh_control_events <- merge(oh_control_events, oh_ctrl_baseline,
  by = c("neighbor_fips", "muni_fips"))
oh_control_events[, share_indexed := share_live_in - baseline_share]

oh_ctrl_agg <- oh_control_events[, .(
  mean_indexed = mean(share_indexed, na.rm = TRUE),
  se = sd(share_indexed, na.rm = TRUE) / sqrt(.N)
), by = rel_year]
oh_ctrl_agg[, group := "Control (neighbors)"]

oh_treat_agg <- copy(oh_event_norm_agg)
oh_treat_agg[, group := "Treated (raised rate)"]

oh_both_agg <- rbind(
  oh_treat_agg[, .(rel_year, mean_indexed, se, group)],
  oh_ctrl_agg[, .(rel_year, mean_indexed, se, group)]
)

p_oh_norm <- ggplot(oh_both_agg, aes(x = rel_year, y = mean_indexed, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_pointrange(aes(ymin = mean_indexed - 1.96 * se, ymax = mean_indexed + 1.96 * se),
    position = position_dodge(width = 0.3)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Treated (raised rate)" = "steelblue",
    "Control (neighbors)" = "grey50")) +
  labs(x = "Years Relative to First Rate Increase",
    y = "Change in Share Living in City (relative to t = -1)",
    title = "OH: Share Living in City Around Tax Increase (Normalized)",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_oh_norm)

## PA: Normalized to event time ----
pa_first_change <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(event_year = min(year)), by = cousub_fips]

pa_event_norm <- merge(pa_panel[total_jobs > 50], pa_first_change,
  by.x = "work_city", by.y = "cousub_fips", all.x = FALSE)
pa_event_norm[, rel_year := year - event_year]
pa_event_norm <- pa_event_norm[abs(rel_year) <= 5]

pa_baseline <- pa_event_norm[rel_year == -1, .(work_city, baseline_share = share_live_in)]
pa_event_norm <- merge(pa_event_norm, pa_baseline, by = "work_city")
pa_event_norm[, share_indexed := share_live_in - baseline_share]

pa_event_norm_agg <- pa_event_norm[, .(
  mean_indexed = mean(share_indexed, na.rm = TRUE),
  se = sd(share_indexed, na.rm = TRUE) / sqrt(.N),
  n_cities = uniqueN(work_city)
), by = rel_year]

# Build PA control group: neighbors of treated municipalities
pa_ctrl_data <- pa_panel[total_jobs > 50, .(work_city, year, share_live_in)]
setnames(pa_ctrl_data, "work_city", "neighbor_fips")

pa_control_events <- merge(
  pa_first_change[, .(cousub_fips, event_year)],
  pa_neighbors[, .(city_fips, neighbor_fips)],
  by.x = "cousub_fips", by.y = "city_fips", allow.cartesian = TRUE
)
pa_control_events <- merge(pa_control_events, pa_ctrl_data,
  by = "neighbor_fips", allow.cartesian = TRUE)
pa_control_events[, rel_year := year - event_year]
pa_control_events <- pa_control_events[abs(rel_year) <= 5]

pa_ctrl_baseline <- pa_control_events[rel_year == -1,
  .(neighbor_fips, cousub_fips, baseline_share = share_live_in)]
pa_control_events <- merge(pa_control_events, pa_ctrl_baseline,
  by = c("neighbor_fips", "cousub_fips"))
pa_control_events[, share_indexed := share_live_in - baseline_share]

pa_ctrl_agg <- pa_control_events[, .(
  mean_indexed = mean(share_indexed, na.rm = TRUE),
  se = sd(share_indexed, na.rm = TRUE) / sqrt(.N)
), by = rel_year]
pa_ctrl_agg[, group := "Control (neighbors)"]

pa_treat_agg <- copy(pa_event_norm_agg)
pa_treat_agg[, group := "Treated (raised rate)"]

pa_both_agg <- rbind(
  pa_treat_agg[, .(rel_year, mean_indexed, se, group)],
  pa_ctrl_agg[, .(rel_year, mean_indexed, se, group)]
)

p_pa_norm <- ggplot(pa_both_agg, aes(x = rel_year, y = mean_indexed, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_pointrange(aes(ymin = mean_indexed - 1.96 * se, ymax = mean_indexed + 1.96 * se),
    position = position_dodge(width = 0.3)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Treated (raised rate)" = "firebrick",
    "Control (neighbors)" = "grey50")) +
  labs(x = "Years Relative to First EIT Rate Increase",
    y = "Change in Share Living in Municipality (relative to t = -1)",
    title = "PA: Share Living in Municipality Around EIT Increase (Normalized)",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_pa_norm)

## OH: Controls split by has-tax vs no-tax ----
# Tag each control neighbor by whether it has its own income tax
oh_ever_taxed <- oh_rate_panel[rate > 0, unique(muni_fips)]

oh_control_events[, nbr_has_tax := neighbor_fips %in% oh_ever_taxed]
oh_control_events[, ctrl_type := fifelse(nbr_has_tax,
  "Control: has tax", "Control: no tax")]

oh_ctrl_split_agg <- oh_control_events[, .(
  mean_indexed = mean(share_indexed, na.rm = TRUE),
  se = sd(share_indexed, na.rm = TRUE) / sqrt(.N)
), by = .(rel_year, ctrl_type)]

oh_split_agg <- rbind(
  oh_treat_agg[, .(rel_year, mean_indexed, se, group)],
  oh_ctrl_split_agg[, .(rel_year, mean_indexed, se, group = ctrl_type)]
)

p_oh_split <- ggplot(oh_split_agg, aes(x = rel_year, y = mean_indexed, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_pointrange(aes(ymin = mean_indexed - 1.96 * se, ymax = mean_indexed + 1.96 * se),
    position = position_dodge(width = 0.4), size = 0.3) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c(
    "Treated (raised rate)" = "steelblue",
    "Control: has tax" = "grey50",
    "Control: no tax" = "forestgreen")) +
  labs(x = "Years Relative to First Rate Increase",
    y = "Change in Share Living in City (relative to t = -1)",
    title = "OH: Treated vs Controls Split by Tax Status",
    subtitle = "Green = no-tax neighbors; Grey = neighbors with existing tax",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_oh_split)

## PA: Controls split by rate level ----
# In PA every municipality has EIT, so split by rate relative to treated
# "Lower rate" = neighbor's rate at event time < treated city's rate
# "Higher rate" = neighbor's rate >= treated city's rate at event time

# Get treated city's rate at event time
oh_treated_rate_at_event <- oh_rate_panel[event_type == "increase",
  .(muni_fips, event_year = year, treated_rate = rate)]

# For PA: all have taxes, split by whether neighbor rate is below median
pa_rate_at_event <- pa_rate_panel[!is.na(cousub_fips),
  .(cousub_fips, year, resident_eit)]

pa_control_events_rated <- merge(
  pa_control_events,
  pa_rate_at_event,
  by.x = c("neighbor_fips", "year"),
  by.y = c("cousub_fips", "year"),
  all.x = TRUE
)

# Also get the treated city's rate at event time
pa_treated_rates <- pa_rate_panel[!is.na(cousub_fips) & event_type == "increase",
  .(cousub_fips, event_year = year, treated_rate = resident_eit)]

pa_control_events_rated <- merge(
  pa_control_events_rated,
  pa_treated_rates,
  by.x = c("cousub_fips", "event_year"),
  by.y = c("cousub_fips", "event_year"),
  all.x = TRUE
)

pa_control_events_rated[, ctrl_type := fifelse(
  !is.na(resident_eit) & !is.na(treated_rate) & resident_eit < treated_rate,
  "Control: lower rate", "Control: higher/same rate")]

pa_ctrl_split_agg <- pa_control_events_rated[, .(
  mean_indexed = mean(share_indexed, na.rm = TRUE),
  se = sd(share_indexed, na.rm = TRUE) / sqrt(.N)
), by = .(rel_year, ctrl_type)]

pa_split_agg <- rbind(
  pa_treat_agg[, .(rel_year, mean_indexed, se, group)],
  pa_ctrl_split_agg[, .(rel_year, mean_indexed, se, group = ctrl_type)]
)

p_pa_split <- ggplot(pa_split_agg, aes(x = rel_year, y = mean_indexed, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_pointrange(aes(ymin = mean_indexed - 1.96 * se, ymax = mean_indexed + 1.96 * se),
    position = position_dodge(width = 0.4), size = 0.3) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c(
    "Treated (raised rate)" = "firebrick",
    "Control: lower rate" = "forestgreen",
    "Control: higher/same rate" = "grey50")) +
  labs(x = "Years Relative to First EIT Rate Increase",
    y = "Change in Share Living in Municipality (relative to t = -1)",
    title = "PA: Treated vs Controls Split by Relative Tax Rate",
    subtitle = "Green = neighbors with lower EIT rate; Grey = neighbors with higher/same rate",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_pa_split)

## OH: Decompose numerator vs denominator around treatment ----
# Index live_and_work (numerator) and total_jobs (denominator) to t = -1

# Treated cities
oh_baseline_levels <- oh_event_norm[rel_year == -1,
  .(work_city, base_live_in = live_and_work, base_total = total_jobs)]
oh_event_levels <- merge(oh_event_norm, oh_baseline_levels, by = "work_city")
oh_event_levels[, `:=`(
  live_in_indexed = log(live_and_work) - log(base_live_in),
  total_indexed = log(total_jobs) - log(base_total)
)]

oh_treat_levels_agg <- oh_event_levels[, .(
  mean_live_in = mean(live_in_indexed, na.rm = TRUE),
  se_live_in = sd(live_in_indexed, na.rm = TRUE) / sqrt(.N),
  mean_total = mean(total_indexed, na.rm = TRUE),
  se_total = sd(total_indexed, na.rm = TRUE) / sqrt(.N)
), by = rel_year]

# Control neighbors
oh_ctrl_levels <- merge(
  oh_control_events[, .(neighbor_fips, muni_fips, year, rel_year)],
  oh_panel[total_jobs > 50, .(work_city, year, live_and_work, total_jobs)],
  by.x = c("neighbor_fips", "year"),
  by.y = c("work_city", "year"), all.x = TRUE)

oh_ctrl_base_levels <- oh_ctrl_levels[rel_year == -1,
  .(neighbor_fips, muni_fips, base_live_in = live_and_work, base_total = total_jobs)]
oh_ctrl_levels <- merge(oh_ctrl_levels, oh_ctrl_base_levels,
  by = c("neighbor_fips", "muni_fips"))
oh_ctrl_levels[, `:=`(
  live_in_indexed = log(live_and_work) - log(base_live_in),
  total_indexed = log(total_jobs) - log(base_total)
)]

oh_ctrl_levels_agg <- oh_ctrl_levels[, .(
  mean_live_in = mean(live_in_indexed, na.rm = TRUE),
  se_live_in = sd(live_in_indexed, na.rm = TRUE) / sqrt(.N),
  mean_total = mean(total_indexed, na.rm = TRUE),
  se_total = sd(total_indexed, na.rm = TRUE) / sqrt(.N)
), by = rel_year]

# Combine for plotting
oh_decomp <- rbind(
  oh_treat_levels_agg[, .(rel_year, value = mean_live_in, se = se_live_in,
    group = "Treated", outcome = "log(Live & Work)")],
  oh_treat_levels_agg[, .(rel_year, value = mean_total, se = se_total,
    group = "Treated", outcome = "log(Total Jobs)")],
  oh_ctrl_levels_agg[, .(rel_year, value = mean_live_in, se = se_live_in,
    group = "Control", outcome = "log(Live & Work)")],
  oh_ctrl_levels_agg[, .(rel_year, value = mean_total, se = se_total,
    group = "Control", outcome = "log(Total Jobs)")]
)

p_oh_decomp <- ggplot(oh_decomp, aes(x = rel_year, y = value,
    color = group, linetype = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_line(linewidth = 0.7, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Treated" = "steelblue", "Control" = "grey50")) +
  scale_linetype_manual(values = c("log(Live & Work)" = "solid", "log(Total Jobs)" = "dashed")) +
  labs(x = "Years Relative to First Rate Increase",
    y = "Log change relative to t = -1",
    title = "OH: Decomposing the Share — Numerator vs Denominator",
    subtitle = "Solid = workers living in city; Dashed = total jobs in city",
    color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_oh_decomp)

## PA: Decompose numerator vs denominator ----
pa_baseline_levels <- pa_event_norm[rel_year == -1,
  .(base_live_in = mean(live_and_work), base_total = mean(total_jobs)), by = work_city]
pa_event_levels <- merge(pa_event_norm, pa_baseline_levels, by = "work_city")
pa_event_levels[, `:=`(
  live_in_indexed = log(live_and_work) - log(base_live_in),
  total_indexed = log(total_jobs) - log(base_total)
)]

pa_treat_levels_agg <- pa_event_levels[, .(
  mean_live_in = mean(live_in_indexed, na.rm = TRUE),
  se_live_in = sd(live_in_indexed, na.rm = TRUE) / sqrt(.N),
  mean_total = mean(total_indexed, na.rm = TRUE),
  se_total = sd(total_indexed, na.rm = TRUE) / sqrt(.N)
), by = rel_year]

# pa_control_events only has share_live_in; need to get live_and_work/total_jobs fresh
pa_ctrl_levels <- merge(
  pa_control_events[, .(neighbor_fips, cousub_fips, year, rel_year)],
  pa_panel[total_jobs > 50 & live_and_work > 0, .(work_city, year, live_and_work, total_jobs)],
  by.x = c("neighbor_fips", "year"),
  by.y = c("work_city", "year"), all.x = FALSE)

pa_ctrl_base_levels <- pa_ctrl_levels[rel_year == -1,
  .(base_live_in = mean(live_and_work, na.rm = TRUE),
    base_total = mean(total_jobs, na.rm = TRUE)),
  by = .(neighbor_fips, cousub_fips)]
pa_ctrl_levels <- merge(pa_ctrl_levels, pa_ctrl_base_levels,
  by = c("neighbor_fips", "cousub_fips"))
pa_ctrl_levels <- pa_ctrl_levels[base_live_in > 0 & base_total > 0]
pa_ctrl_levels[, `:=`(
  live_in_indexed = log(live_and_work) - log(base_live_in),
  total_indexed = log(total_jobs) - log(base_total)
)]

pa_ctrl_levels_agg <- pa_ctrl_levels[is.finite(live_in_indexed) & is.finite(total_indexed), .(
  mean_live_in = mean(live_in_indexed),
  se_live_in = sd(live_in_indexed) / sqrt(.N),
  mean_total = mean(total_indexed),
  se_total = sd(total_indexed) / sqrt(.N)
), by = rel_year]

pa_decomp <- rbind(
  pa_treat_levels_agg[, .(rel_year, value = mean_live_in, se = se_live_in,
    group = "Treated", outcome = "log(Live & Work)")],
  pa_treat_levels_agg[, .(rel_year, value = mean_total, se = se_total,
    group = "Treated", outcome = "log(Total Jobs)")],
  pa_ctrl_levels_agg[, .(rel_year, value = mean_live_in, se = se_live_in,
    group = "Control", outcome = "log(Live & Work)")],
  pa_ctrl_levels_agg[, .(rel_year, value = mean_total, se = se_total,
    group = "Control", outcome = "log(Total Jobs)")]
)

p_pa_decomp <- ggplot(pa_decomp, aes(x = rel_year, y = value,
    color = group, linetype = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_line(linewidth = 0.7, position = position_dodge(width = 0.2)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Treated" = "firebrick", "Control" = "grey50")) +
  scale_linetype_manual(values = c("log(Live & Work)" = "solid", "log(Total Jobs)" = "dashed")) +
  labs(x = "Years Relative to First EIT Rate Increase",
    y = "Log change relative to t = -1",
    title = "PA: Decomposing the Share — Numerator vs Denominator",
    subtitle = "Solid = workers living in municipality; Dashed = total jobs",
    color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_pa_decomp)


# 4b. Tract-Level Distance Gradient ----
# For each treated city, compute distance from every tract to the treated
# city centroid. Bin tracts by distance. Plot mean log(RAC) indexed to t=-1
# by distance bin over event time. Extends to 80km (~50 miles).

## Load tract-level RAC ----
oh_rac_tracts <- readRDS(file.path(data_dir, "lodes", "oh_rac_S000.rds"))
oh_rac_dt <- as.data.table(oh_rac_tracts)
oh_rac_dt[, year := as.integer(year)]

## Load TIGER geometries for distance computation ----
oh_places_geo <- readRDS(file.path(data_dir, "oh_places.rds"))
oh_tracts_geo <- tracts(state = "OH", year = 2020) %>% st_transform(32617)
oh_places_proj <- st_transform(oh_places_geo, 32617)

## Compute tract centroid to treated city centroid distances ----
# Using centroids for speed; ~3000 tracts × ~270 treated cities
tract_cents <- st_centroid(oh_tracts_geo)
tract_cent_df <- data.table(
  tract_geoid = oh_tracts_geo$GEOID,
  tract_x = st_coordinates(tract_cents)[, 1],
  tract_y = st_coordinates(tract_cents)[, 2]
)

oh_treated_fips <- oh_rate_panel[event_type == "increase", unique(muni_fips)]
treated_places <- oh_places_proj %>% filter(PLACEFP %in% oh_treated_fips)
treated_cents <- st_centroid(treated_places)
treated_cent_df <- data.table(
  muni_fips = treated_places$PLACEFP,
  city_x = st_coordinates(treated_cents)[, 1],
  city_y = st_coordinates(treated_cents)[, 2]
)

# Cross join and compute distances
print(paste("Computing distances:", nrow(tract_cent_df), "tracts x",
  nrow(treated_cent_df), "treated cities"))

# For efficiency, compute for each treated city separately
dist_list <- list()
for (j in seq_len(nrow(treated_cent_df))) {
  dx <- tract_cent_df$tract_x - treated_cent_df$city_x[j]
  dy <- tract_cent_df$tract_y - treated_cent_df$city_y[j]
  d_km <- sqrt(dx^2 + dy^2) / 1000

  # Keep tracts within 80km
  keep <- d_km <= 80
  if (sum(keep) == 0) next

  dist_list[[j]] <- data.table(
    tract_geoid = tract_cent_df$tract_geoid[keep],
    muni_fips = treated_cent_df$muni_fips[j],
    dist_km = d_km[keep]
  )
}
tract_city_dist <- rbindlist(dist_list)

# Add event year
tract_city_dist <- merge(tract_city_dist,
  oh_rate_panel[event_type == "increase", .(muni_fips, event_year = year)],
  by = "muni_fips", allow.cartesian = TRUE)

# Use first increase only
first_events <- tract_city_dist[, .(event_year = min(event_year)), by = muni_fips]
tract_city_dist <- merge(tract_city_dist, first_events, by = c("muni_fips", "event_year"))

print(paste("Tract-city-event pairs:", nrow(tract_city_dist)))

## Assign tracts to the NEAREST treated city ----
# Each tract gets assigned to its closest treated city
nearest_treated <- tract_city_dist[, .SD[which.min(dist_km)],
  by = .(tract_geoid, event_year)]

## Merge with RAC data ----
tract_event <- merge(nearest_treated,
  oh_rac_dt[, .(tract_geoid = h_tract, year, C000)],
  by = c("tract_geoid"), allow.cartesian = TRUE)
tract_event[, rel_year := year - event_year]
tract_event <- tract_event[abs(rel_year) <= 5 & C000 > 0]
tract_event[, log_rac := log(C000)]

## Filter control tracts: OH = only tracts NOT in any taxing municipality ----
# Use tract-place crosswalk to identify which tracts are in incorporated places
oh_tract_xwalk_dt <- as.data.table(readRDS(file.path(data_dir, "oh_tract_xwalk.rds")))

# All municipalities that ever had a tax (not just those that raised it)
oh_all_taxed <- oh_rate_panel[rate > 0, unique(muni_fips)]

tract_event <- merge(tract_event, oh_tract_xwalk_dt,
  by.x = "tract_geoid", by.y = "tract_geoid", all.x = TRUE)

# Tag tract type:
# "treated" = inside the treated city for this event
# "clean_control" = NOT in any taxing municipality
# "other_taxed" = inside a different taxing municipality (contaminated)
tract_event[, tract_type := fcase(
  place_fips == muni_fips, "treated",
  is.na(place_fips), "clean_control",
  !(place_fips %in% oh_all_taxed), "clean_control",
  default = "other_taxed"
)]

# Keep only treated tracts (for 0-5km bin) and clean controls (for other bins)
tract_event_clean <- tract_event[tract_type %in% c("treated", "clean_control")]

## Distance bins ----
tract_event_clean[, dist_bin := cut(dist_km,
  breaks = c(0, 5, 10, 20, 40, 60, 80),
  labels = c("0-5km\n(treated)", "5-10km", "10-20km", "20-40km", "40-60km", "60-80km"),
  include.lowest = TRUE)]

## Index to t = -1 ----
tract_baseline <- tract_event_clean[rel_year == -1,
  .(baseline_rac = mean(log_rac, na.rm = TRUE)), by = .(tract_geoid, muni_fips)]
tract_event_clean <- merge(tract_event_clean, tract_baseline, by = c("tract_geoid", "muni_fips"))
tract_event_clean[, rac_indexed := log_rac - baseline_rac]

## Aggregate by distance bin × relative year ----
dist_gradient <- tract_event_clean[!is.na(dist_bin) & is.finite(rac_indexed), .(
  mean_indexed = mean(rac_indexed),
  se = sd(rac_indexed) / sqrt(.N),
  n_tracts = uniqueN(tract_geoid)
), by = .(rel_year, dist_bin)]

p_oh_dist_gradient <- ggplot(dist_gradient,
    aes(x = rel_year, y = mean_indexed, color = dist_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "RdYlGn", direction = -1) +
  labs(x = "Years Relative to First Rate Increase",
    y = "Log change in RAC (relative to t = -1)",
    title = "OH: Tract-Level RAC by Distance to Treated City",
    subtitle = "Closest tracts (red) should decline most; distant tracts (green) unaffected",
    color = "Distance to\ntreated city") +
  theme_minimal() +
  theme(legend.position = "right")
print(p_oh_dist_gradient)

## OH: City-level OD decomposition by distance ----
# Use neighbor pairs but with distance bins instead of collapsing all controls
# For each treated city, bin control cities by distance, plot live_and_work
# and total_jobs separately

# Filter: only use control cities that have NO tax (clean controls for OH)
oh_ctrl_no_tax <- setdiff(
  oh_neighbors[, unique(neighbor_fips)],
  oh_rate_panel[rate > 0, unique(muni_fips)]
)

oh_ctrl_dist_decomp <- merge(
  oh_control_events[neighbor_fips %in% oh_ctrl_no_tax,
    .(neighbor_fips, muni_fips, year, rel_year)],
  oh_panel[total_jobs > 50 & live_and_work > 0,
    .(work_city, year, live_and_work, total_jobs)],
  by.x = c("neighbor_fips", "year"),
  by.y = c("work_city", "year"), all.x = FALSE)

# Get distance from neighbor to treated city
oh_ctrl_dist_decomp <- merge(oh_ctrl_dist_decomp,
  oh_neighbors[, .(city_fips, neighbor_fips, dist_km)],
  by.x = c("muni_fips", "neighbor_fips"),
  by.y = c("city_fips", "neighbor_fips"), all.x = TRUE)

# Baseline at t = -1
oh_ctrl_dist_base <- oh_ctrl_dist_decomp[rel_year == -1,
  .(base_lw = mean(live_and_work, na.rm = TRUE),
    base_tj = mean(total_jobs, na.rm = TRUE)),
  by = .(neighbor_fips, muni_fips)]
oh_ctrl_dist_decomp <- merge(oh_ctrl_dist_decomp, oh_ctrl_dist_base,
  by = c("neighbor_fips", "muni_fips"))

oh_ctrl_dist_decomp <- oh_ctrl_dist_decomp[base_lw > 0 & base_tj > 0]
oh_ctrl_dist_decomp[, `:=`(
  lw_indexed = log(live_and_work) - log(base_lw),
  tj_indexed = log(total_jobs) - log(base_tj)
)]

# Distance bins for control cities (extended to 80km)
oh_ctrl_dist_decomp[, dist_bin := cut(dist_km,
  breaks = c(0, 5, 10, 20, 40, 60, 80),
  labels = c("0-5km", "5-10km", "10-20km", "20-40km", "40-60km", "60-80km"),
  include.lowest = TRUE)]

oh_ctrl_dist_agg <- oh_ctrl_dist_decomp[!is.na(dist_bin) & is.finite(lw_indexed), .(
  mean_lw = mean(lw_indexed),
  mean_tj = mean(tj_indexed)
), by = .(rel_year, dist_bin)]

# Combine with treated city data
oh_dist_decomp_all <- rbind(
  oh_treat_levels_agg[, .(rel_year, mean_lw = mean_live_in, mean_tj = mean_total,
    dist_bin = "Treated (0km)")],
  oh_ctrl_dist_agg
)

# Pivot longer for faceted plot
oh_dist_decomp_long <- melt(oh_dist_decomp_all,
  id.vars = c("rel_year", "dist_bin"),
  measure.vars = c("mean_lw", "mean_tj"),
  variable.name = "outcome",
  value.name = "value")
oh_dist_decomp_long[, outcome := fifelse(outcome == "mean_lw",
  "Live & Work (numerator)", "Total Jobs (denominator)")]

p_oh_dist_decomp <- ggplot(oh_dist_decomp_long,
    aes(x = rel_year, y = value, color = dist_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~outcome) +
  scale_color_manual(values = c(
    "Treated (0km)" = "firebrick",
    "0-5km" = "darkorange",
    "5-10km" = "goldenrod",
    "10-20km" = "forestgreen",
    "20-40km" = "steelblue",
    "40-60km" = "slateblue",
    "60-80km" = "grey50")) +
  labs(x = "Years Relative to First Rate Increase",
    y = "Log change relative to t = -1",
    title = "OH: Live & Work vs Total Jobs by Distance (No-Tax Controls Only)",
    subtitle = "Red = treated city; controls = cities with no income tax",
    color = "Distance") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_oh_dist_decomp)

## PA: City-level OD decomposition by distance ----
pa_ctrl_dist_decomp <- merge(pa_control_events,
  pa_panel[total_jobs > 50, .(work_city, year, live_and_work, total_jobs)],
  by.x = c("neighbor_fips", "year"),
  by.y = c("work_city", "year"), all.x = TRUE)

pa_ctrl_dist_decomp <- merge(pa_ctrl_dist_decomp,
  pa_neighbors[, .(city_fips, neighbor_fips, dist_km)],
  by.x = c("cousub_fips", "neighbor_fips"),
  by.y = c("city_fips", "neighbor_fips"), all.x = TRUE)

pa_ctrl_dist_base <- pa_ctrl_dist_decomp[rel_year == -1,
  .(base_lw = mean(live_and_work, na.rm = TRUE),
    base_tj = mean(total_jobs, na.rm = TRUE)),
  by = .(neighbor_fips, cousub_fips)]
pa_ctrl_dist_decomp <- merge(pa_ctrl_dist_decomp, pa_ctrl_dist_base,
  by = c("neighbor_fips", "cousub_fips"))
pa_ctrl_dist_decomp[, `:=`(
  lw_indexed = log(live_and_work) - log(base_lw),
  tj_indexed = log(total_jobs) - log(base_tj)
)]

pa_ctrl_dist_decomp[, dist_bin := cut(dist_km,
  breaks = c(0, 5, 10, 20, 40, 60, 80),
  labels = c("0-5km", "5-10km", "10-20km", "20-40km", "40-60km", "60-80km"),
  include.lowest = TRUE)]

pa_ctrl_dist_agg <- pa_ctrl_dist_decomp[!is.na(dist_bin), .(
  mean_lw = mean(lw_indexed, na.rm = TRUE),
  mean_tj = mean(tj_indexed, na.rm = TRUE)
), by = .(rel_year, dist_bin)]

pa_dist_decomp_all <- rbind(
  pa_treat_levels_agg[, .(rel_year, mean_lw = mean_live_in, mean_tj = mean_total,
    dist_bin = "Treated (0km)")],
  pa_ctrl_dist_agg
)

pa_dist_decomp_long <- melt(pa_dist_decomp_all,
  id.vars = c("rel_year", "dist_bin"),
  measure.vars = c("mean_lw", "mean_tj"),
  variable.name = "outcome", value.name = "value")
pa_dist_decomp_long[, outcome := fifelse(outcome == "mean_lw",
  "Live & Work (numerator)", "Total Jobs (denominator)")]

p_pa_dist_decomp <- ggplot(pa_dist_decomp_long,
    aes(x = rel_year, y = value, color = dist_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~outcome) +
  scale_color_manual(values = c(
    "Treated (0km)" = "firebrick",
    "0-5km" = "darkorange",
    "5-10km" = "goldenrod",
    "10-20km" = "forestgreen",
    "20-40km" = "steelblue",
    "40-60km" = "slateblue",
    "60-80km" = "grey50")) +
  labs(x = "Years Relative to First EIT Rate Increase",
    y = "Log change relative to t = -1",
    title = "PA: Live & Work vs Total Jobs by Distance to Treated Municipality",
    color = "Distance") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_pa_dist_decomp)

## OH: Tract-level RAC distance gradient ----
pa_rac_tracts <- readRDS(file.path(data_dir, "lodes", "pa_rac_S000.rds"))
pa_rac_dt <- as.data.table(pa_rac_tracts)
pa_rac_dt[, year := as.integer(year)]

pa_cousub_geo <- readRDS(file.path(data_dir, "pa_cousub.rds"))
pa_tracts_geo <- tracts(state = "PA", year = 2020) %>% st_transform(32618)
pa_cousub_proj <- st_transform(pa_cousub_geo, 32618)

pa_tract_cents <- st_centroid(pa_tracts_geo)
pa_tract_cent_df <- data.table(
  tract_geoid = pa_tracts_geo$GEOID,
  tract_x = st_coordinates(pa_tract_cents)[, 1],
  tract_y = st_coordinates(pa_tract_cents)[, 2]
)

pa_treated_fips <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_treated_cousub <- pa_cousub_proj %>% filter(COUSUBFP %in% pa_treated_fips)
pa_treated_cents <- st_centroid(pa_treated_cousub)
pa_treated_cent_df <- data.table(
  cousub_fips = pa_treated_cousub$COUSUBFP,
  city_x = st_coordinates(pa_treated_cents)[, 1],
  city_y = st_coordinates(pa_treated_cents)[, 2]
)

pa_dist_list <- list()
for (j in seq_len(nrow(pa_treated_cent_df))) {
  dx <- pa_tract_cent_df$tract_x - pa_treated_cent_df$city_x[j]
  dy <- pa_tract_cent_df$tract_y - pa_treated_cent_df$city_y[j]
  d_km <- sqrt(dx^2 + dy^2) / 1000
  keep <- d_km <= 80
  if (sum(keep) == 0) next
  pa_dist_list[[j]] <- data.table(
    tract_geoid = pa_tract_cent_df$tract_geoid[keep],
    cousub_fips = pa_treated_cent_df$cousub_fips[j],
    dist_km = d_km[keep]
  )
}
pa_tract_city_dist <- rbindlist(pa_dist_list)

# First increase year per treated PA municipality
pa_first_events <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(event_year = min(year)), by = cousub_fips]
pa_tract_city_dist <- merge(pa_tract_city_dist, pa_first_events, by = "cousub_fips")

# Nearest treated municipality per tract
pa_nearest <- pa_tract_city_dist[, .SD[which.min(dist_km)],
  by = .(tract_geoid, event_year)]

# Merge with RAC
pa_tract_event <- merge(pa_nearest,
  pa_rac_dt[, .(tract_geoid = h_tract, year, C000)],
  by = "tract_geoid", allow.cartesian = TRUE)
pa_tract_event[, rel_year := year - event_year]
pa_tract_event <- pa_tract_event[abs(rel_year) <= 5 & C000 > 0]
pa_tract_event[, log_rac := log(C000)]

## Filter PA control tracts: only tracts in municipalities with LOWER rate ----
pa_tract_xwalk_dt <- as.data.table(readRDS(file.path(data_dir, "pa_tract_xwalk.rds")))

# Rename before merge to avoid collision
setnames(pa_tract_event, "cousub_fips", "treated_cousub")

# Add tract's own municipality
pa_tract_event <- merge(pa_tract_event,
  pa_tract_xwalk_dt[, .(tract_geoid, tract_cousub = cousub_fips)],
  by = "tract_geoid", all.x = TRUE)

# Get the treated city's rate at event time
pa_treated_rate_at_event <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(cousub_fips, event_year = year, treated_rate = resident_eit)]
pa_treated_first <- pa_treated_rate_at_event[, .SD[which.min(event_year)], by = cousub_fips]

pa_tract_event <- merge(pa_tract_event,
  pa_treated_first[, .(treated_cousub = cousub_fips, treated_rate)],
  by = "treated_cousub", all.x = TRUE)

# Get tract municipality's rate at event time
# Get tract's own municipality rate
pa_all_rates <- pa_rate_panel[!is.na(cousub_fips),
  .(tract_cousub = cousub_fips, year, tract_rate = resident_eit)]
pa_tract_event <- merge(pa_tract_event, pa_all_rates,
  by = c("tract_cousub", "year"), all.x = TRUE)

# Tag tract type
pa_tract_event[, tract_type := fcase(
  is.na(tract_cousub), "no_data",
  tract_cousub == treated_cousub, "treated",
  !is.na(tract_rate) & !is.na(treated_rate) & tract_rate < treated_rate, "lower_rate",
  default = "higher_rate"
)]

# Keep treated + lower_rate controls only
pa_tract_event_clean <- pa_tract_event[tract_type %in% c("treated", "lower_rate")]

pa_tract_event_clean[, dist_bin := cut(dist_km,
  breaks = c(0, 5, 10, 20, 40, 60, 80),
  labels = c("0-5km\n(treated)", "5-10km", "10-20km", "20-40km", "40-60km", "60-80km"),
  include.lowest = TRUE)]

pa_tract_baseline <- pa_tract_event_clean[rel_year == -1,
  .(baseline_rac = mean(log_rac, na.rm = TRUE)), by = .(tract_geoid, treated_cousub)]
pa_tract_event_clean <- merge(pa_tract_event_clean, pa_tract_baseline,
  by = c("tract_geoid", "treated_cousub"))
pa_tract_event_clean[, rac_indexed := log_rac - baseline_rac]

pa_dist_gradient <- pa_tract_event_clean[!is.na(dist_bin) & is.finite(rac_indexed), .(
  mean_indexed = mean(rac_indexed),
  se = sd(rac_indexed) / sqrt(.N),
  n_tracts = uniqueN(tract_geoid)
), by = .(rel_year, dist_bin)]

p_pa_dist_gradient <- ggplot(pa_dist_gradient,
    aes(x = rel_year, y = mean_indexed, color = dist_bin)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "RdYlGn", direction = -1) +
  labs(x = "Years Relative to First EIT Rate Increase",
    y = "Log change in RAC (relative to t = -1)",
    title = "PA: Tract-Level RAC by Distance to Treated Municipality",
    subtitle = "Closest tracts (red) should decline most; distant tracts (green) unaffected",
    color = "Distance to\ntreated muni") +
  theme_minimal() +
  theme(legend.position = "right")
print(p_pa_dist_gradient)


# 4c. Case Study: Circleville, OH ----
# Circleville raised rate from 1.5% to 2% in 2015
# Pre-COVID, south of Columbus, clean timing window (2014-2018)

case_fips <- "15070"  # Circleville
case_year <- 2015
case_name <- "Circleville"

# Get all places within 30km for the map
case_place <- oh_places_proj %>% filter(PLACEFP == case_fips)
case_centroid <- st_centroid(case_place)
case_buffer <- st_buffer(case_centroid, 30000)  # 30km

nearby_places <- oh_places_proj[st_intersects(oh_places_proj, case_buffer, sparse = FALSE), ]
nearby_tracts <- oh_tracts_geo[st_intersects(oh_tracts_geo, case_buffer, sparse = FALSE), ]

# Get RAC for nearby tracts pre and post
case_rac_pre <- oh_rac_dt[year == case_year - 1, .(tract_geoid = h_tract, rac_pre = C000)]
case_rac_post <- oh_rac_dt[year == min(case_year + 3, 2021), .(tract_geoid = h_tract, rac_post = C000)]

nearby_tracts_dt <- data.table(tract_geoid = nearby_tracts$GEOID)
nearby_tracts_dt <- merge(nearby_tracts_dt, case_rac_pre, by = "tract_geoid", all.x = TRUE)
nearby_tracts_dt <- merge(nearby_tracts_dt, case_rac_post, by = "tract_geoid", all.x = TRUE)
nearby_tracts_dt[, pct_change := (rac_post - rac_pre) / rac_pre * 100]

# Filter out low-population tracts (water, parks, empty land)
nearby_tracts_dt <- nearby_tracts_dt[!is.na(rac_pre) & rac_pre >= 50]

# Merge back to sf
nearby_tracts <- merge(nearby_tracts, nearby_tracts_dt, by.x = "GEOID", by.y = "tract_geoid")

# Tag places by tax status
oh_rate_at_event <- oh_rate_panel[year == case_year, .(muni_fips, rate)]
nearby_places <- nearby_places %>%
  left_join(as.data.frame(oh_rate_at_event), by = c("PLACEFP" = "muni_fips")) %>%
  mutate(
    place_type = case_when(
      PLACEFP == case_fips ~ "Treated (Chardon)",
      !is.na(rate) & rate > 0 ~ "Has tax",
      TRUE ~ "No tax"
    )
  )

# Transform to 4326 for plotting
nearby_tracts_4326 <- st_transform(nearby_tracts, 4326)
nearby_places_4326 <- st_transform(nearby_places, 4326)

# Winsorize extreme changes for color scale
nearby_tracts_4326 <- nearby_tracts_4326 %>%
  mutate(pct_change_w = pmax(pmin(pct_change, 30), -30))

p_case_change <- ggplot() +
  geom_sf(data = nearby_tracts_4326, aes(fill = pct_change_w), color = "grey80", linewidth = 0.05) +
  geom_sf(data = nearby_places_4326 %>% filter(place_type == "No tax"),
    fill = NA, color = "forestgreen", linewidth = 0.4) +
  geom_sf(data = nearby_places_4326 %>% filter(place_type == "Has tax"),
    fill = NA, color = "grey40", linewidth = 0.4) +
  geom_sf(data = nearby_places_4326 %>% filter(place_type == "Treated (Chardon)"),
    fill = NA, color = "firebrick", linewidth = 1) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "% Change\nin RAC",
    limits = c(-30, 30)) +
  labs(
    title = paste0("Case Study: ", case_name, ", OH — RAC Change Around Tax Increase"),
    subtitle = paste0("Rate increased in ", case_year,
      ". Red outline = treated; Green = no-tax; Grey = has tax. ",
      "Tract fill = % change in residential workers (t-1 to t+3)")
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
print(p_case_change)

# Also plot pre-treatment RAC levels for context
p_case_levels <- ggplot() +
  geom_sf(data = nearby_tracts_4326, aes(fill = rac_pre), color = "grey80", linewidth = 0.05) +
  geom_sf(data = nearby_places_4326 %>% filter(place_type == "No tax"),
    fill = NA, color = "forestgreen", linewidth = 0.4) +
  geom_sf(data = nearby_places_4326 %>% filter(place_type == "Has tax"),
    fill = NA, color = "grey40", linewidth = 0.4) +
  geom_sf(data = nearby_places_4326 %>% filter(place_type == "Treated (Chardon)"),
    fill = NA, color = "firebrick", linewidth = 1) +
  scale_fill_viridis_c(option = "plasma", name = "Workers\n(pre)", trans = "log10") +
  labs(
    title = paste0(case_name, ", OH — Pre-Treatment Residential Workers (", case_year - 1, ")"),
    subtitle = "Red = Chardon (treated); Green = no-tax municipalities; Grey = has tax"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
print(p_case_levels)


## Chardon distance profile: mean RAC change by distance ring ----
# Compute distance from each nearby tract centroid to Chardon boundary
chardon_border <- st_boundary(st_geometry(case_place))
nearby_cents <- st_centroid(nearby_tracts)
dist_to_chardon_m <- as.numeric(st_distance(nearby_cents, st_union(chardon_border)))

# Are tracts inside or outside Chardon?
inside_chardon <- st_intersects(nearby_cents, case_place, sparse = FALSE)[, 1]

# Signed distance: negative = inside, positive = outside
nearby_tracts$dist_to_chardon_km <- dist_to_chardon_m / 1000
nearby_tracts$dist_signed_km <- ifelse(inside_chardon,
  -nearby_tracts$dist_to_chardon_km,
  nearby_tracts$dist_to_chardon_km)

# Tag by tax status of the tract's municipality
nearby_tracts_with_place <- st_join(
  nearby_tracts %>% mutate(centroid = st_centroid(geometry)) %>% st_set_geometry("centroid"),
  nearby_places %>% select(PLACEFP, place_type),
  left = TRUE) %>%
  st_drop_geometry()

# Distance bins
nearby_tracts_with_place$dist_ring <- cut(nearby_tracts_with_place$dist_signed_km,
  breaks = c(-Inf, -2, 0, 2, 5, 10, 15, 20, 30),
  labels = c("Inside\n(>2km)", "Inside\n(0-2km)", "0-2km", "2-5km",
    "5-10km", "10-15km", "15-20km", "20-30km"))

case_profile <- as.data.table(nearby_tracts_with_place)[
  !is.na(dist_ring) & !is.na(pct_change) & is.finite(pct_change), .(
  mean_change = mean(pct_change, na.rm = TRUE),
  se = sd(pct_change, na.rm = TRUE) / sqrt(.N),
  n_tracts = .N,
  pct_no_tax = mean(place_type == "No tax" | is.na(place_type), na.rm = TRUE)
), by = dist_ring]

# Order factor
case_profile[, dist_ring := factor(dist_ring,
  levels = c("Inside\n(>2km)", "Inside\n(0-2km)", "0-2km", "2-5km",
    "5-10km", "10-15km", "15-20km", "20-30km"))]

p_case_profile <- ggplot(case_profile, aes(x = dist_ring, y = mean_change)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(aes(fill = mean_change > 0), width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_change - 1.96 * se, ymax = mean_change + 1.96 * se),
    width = 0.2) +
  geom_text(aes(label = paste0("n=", n_tracts)), y = -8, size = 2.5, color = "grey40") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"), guide = "none") +
  labs(x = "Distance from City Border",
    y = paste0("Mean % Change in RAC (", case_year - 1, " to ", min(case_year + 3, 2021), ")"),
    title = paste0(case_name, ": Distance Profile of Residential Worker Change"),
    subtitle = "Negative inside (red) = losing residents; Positive outside (blue) = gaining") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))
print(p_case_profile)


# 4d. Case Study: Gahanna, OH (COVID period) ----
# Gahanna raised rate from 1.5% to 2.5% in 2020
# Note: 2019-2021 window contaminated by COVID/remote work

gah_fips <- "29106"
gah_year <- 2020
gah_name <- "Gahanna"

gah_place <- oh_places_proj %>% filter(PLACEFP == gah_fips)
gah_centroid <- st_centroid(gah_place)
gah_buffer <- st_buffer(gah_centroid, 30000)

gah_places <- oh_places_proj[st_intersects(oh_places_proj, gah_buffer, sparse = FALSE), ]
gah_tracts <- oh_tracts_geo[st_intersects(oh_tracts_geo, gah_buffer, sparse = FALSE), ]

gah_rac_pre <- oh_rac_dt[year == gah_year - 1, .(tract_geoid = h_tract, rac_pre = C000)]
gah_rac_post <- oh_rac_dt[year == min(gah_year + 1, 2021), .(tract_geoid = h_tract, rac_post = C000)]

gah_tracts_dt <- data.table(tract_geoid = gah_tracts$GEOID)
gah_tracts_dt <- merge(gah_tracts_dt, gah_rac_pre, by = "tract_geoid", all.x = TRUE)
gah_tracts_dt <- merge(gah_tracts_dt, gah_rac_post, by = "tract_geoid", all.x = TRUE)
gah_tracts_dt[, pct_change := (rac_post - rac_pre) / rac_pre * 100]
gah_tracts_dt <- gah_tracts_dt[!is.na(rac_pre) & rac_pre >= 50]

gah_tracts <- merge(gah_tracts, gah_tracts_dt, by.x = "GEOID", by.y = "tract_geoid")

gah_rate_at_event <- oh_rate_panel[year == gah_year, .(muni_fips, rate)]
gah_places <- gah_places %>%
  left_join(as.data.frame(gah_rate_at_event), by = c("PLACEFP" = "muni_fips")) %>%
  mutate(place_type = case_when(
    PLACEFP == gah_fips ~ paste0("Treated (", gah_name, ")"),
    !is.na(rate) & rate > 0 ~ "Has tax",
    TRUE ~ "No tax"))

gah_tracts_4326 <- st_transform(gah_tracts, 4326)
gah_places_4326 <- st_transform(gah_places, 4326)
gah_tracts_4326 <- gah_tracts_4326 %>%
  mutate(pct_change_w = pmax(pmin(pct_change, 30), -30))

p_gah_change <- ggplot() +
  geom_sf(data = gah_tracts_4326, aes(fill = pct_change_w), color = "grey80", linewidth = 0.05) +
  geom_sf(data = gah_places_4326 %>% filter(place_type == "No tax"),
    fill = NA, color = "forestgreen", linewidth = 0.4) +
  geom_sf(data = gah_places_4326 %>% filter(place_type == "Has tax"),
    fill = NA, color = "grey40", linewidth = 0.4) +
  geom_sf(data = gah_places_4326 %>% filter(grepl("Treated", place_type)),
    fill = NA, color = "firebrick", linewidth = 1) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "% Change\nin RAC", limits = c(-30, 30)) +
  labs(title = paste0("Case Study: ", gah_name, ", OH -- RAC Change (COVID period)"),
    subtitle = paste0("Rate: 1.5% -> 2.5% in ", gah_year,
      ". Tract fill = % change (t-1 to t+1). Note: COVID contaminates this period.")) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
print(p_gah_change)


# 5. Scatter Plots ----

## OH: Rate levels vs share_live_in (binscatter) ----
p_oh_bin <- oh_panel[total_jobs > 50 & rate > 0] %>%
  mutate(rate_bin = cut(rate * 100, breaks = seq(0, 3, by = 0.25), include.lowest = TRUE)) %>%
  filter(!is.na(rate_bin)) %>%
  group_by(rate_bin) %>%
  summarise(mean_share = mean(share_live_in), se = sd(share_live_in) / sqrt(n()),
    .groups = "drop") %>%
  mutate(rate_mid = as.numeric(gsub("\\(|\\[|,.*", "", rate_bin)) + 0.125) %>%
  ggplot(aes(x = rate_mid, y = mean_share)) +
  geom_pointrange(aes(ymin = mean_share - 1.96 * se, ymax = mean_share + 1.96 * se)) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(x = "Municipal Income Tax Rate (%)", y = "Share of Workers Living in City",
    title = "OH: Tax Rate vs Residential Share (Binscatter)") +
  theme_minimal()
print(p_oh_bin)

## OH: Rate CHANGES vs share_live_in CHANGES ----
oh_panel[, d_share := share_live_in - shift(share_live_in), by = work_city]
oh_panel[, d_rate := rate - shift(rate), by = work_city]

p_oh_drate <- oh_panel[total_jobs > 50 & !is.na(d_rate) & d_rate != 0 & !is.na(d_share)] %>%
  ggplot(aes(x = d_rate * 100, y = d_share)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Change in Tax Rate (pp)", y = "Change in Share Living in City",
    title = "OH: Tax Rate Changes vs Residential Share Changes") +
  theme_minimal()
print(p_oh_drate)

## OH: Treated city rate change vs neighbor RAC change ----
oh_neighbor_scatter <- oh_rate_panel[event_type == "increase",
  .(treated_fips = muni_fips, event_year = year, treated_rate_change = rate_change)]

oh_neighbor_effects <- oh_neighbor_scatter %>%
  left_join(oh_neighbors, by = c("treated_fips" = "city_fips")) %>%
  left_join(
    oh_panel[, .(work_city, year, d_share)],
    by = c("neighbor_fips" = "work_city", "event_year" = "year")
  ) %>%
  filter(!is.na(d_share)) %>%
  group_by(treated_fips, event_year, treated_rate_change) %>%
  summarise(mean_nbr_d_share = mean(d_share, na.rm = TRUE),
    n_neighbors = n(), .groups = "drop")

p_oh_nbr <- oh_neighbor_effects %>%
  ggplot(aes(x = treated_rate_change * 100, y = mean_nbr_d_share)) +
  geom_point(aes(size = n_neighbors), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treated City Rate Change (pp)",
    y = "Mean Neighbor Share Change",
    title = "OH: Do Neighbors Gain Residents When a City Raises Taxes?",
    size = "N neighbors") +
  theme_minimal()
print(p_oh_nbr)

## PA: Rate levels vs share_live_in ----
p_pa_bin <- pa_panel[total_jobs > 50 & !is.na(resident_eit)] %>%
  mutate(rate_bin = cut(resident_eit, breaks = seq(0, 2, by = 0.1), include.lowest = TRUE)) %>%
  filter(!is.na(rate_bin)) %>%
  group_by(rate_bin) %>%
  summarise(mean_share = mean(share_live_in), se = sd(share_live_in) / sqrt(n()),
    .groups = "drop") %>%
  mutate(rate_mid = as.numeric(gsub("\\(|\\[|,.*", "", rate_bin)) + 0.05) %>%
  ggplot(aes(x = rate_mid, y = mean_share)) +
  geom_pointrange(aes(ymin = mean_share - 1.96 * se, ymax = mean_share + 1.96 * se)) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(x = "Municipal EIT Rate (%)", y = "Share of Workers Living in Municipality",
    title = "PA: EIT Rate vs Residential Share (Binscatter)") +
  theme_minimal()
print(p_pa_bin)

## PA: Rate changes vs share changes ----
pa_panel[, d_share := share_live_in - shift(share_live_in), by = work_city]
pa_panel[, d_rate := resident_eit - shift(resident_eit), by = work_city]

p_pa_drate <- pa_panel[total_jobs > 50 & !is.na(d_rate) & abs(d_rate) > 1e-6 & !is.na(d_share)] %>%
  ggplot(aes(x = d_rate * 100, y = d_share)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Change in EIT Rate (pp)", y = "Change in Share Living in Municipality",
    title = "PA: EIT Rate Changes vs Residential Share Changes") +
  theme_minimal()
print(p_pa_drate)


## PA: Rate change vs change in share_live_in (dose-response scatter) ----
# For each rate change event, compare share_live_in at t-1 vs t+2
pa_rate_events <- pa_rate_panel[!is.na(rate_change) & abs(rate_change) > 1e-6 &
  !is.na(cousub_fips) & rate_change < 5 & year >= 2003 & year <= 2019]

pa_share_lag <- pa_panel[, .(cousub_fips = work_city, year_p1 = year + 1, share_pre = share_live_in)]
pa_share_lead <- pa_panel[, .(cousub_fips = work_city, year_m2 = year - 2, share_post = share_live_in)]

pa_dose_scatter <- merge(pa_rate_events[, .(cousub_fips, year, rate_change, event_type)],
  pa_share_lag, by.x = c("cousub_fips", "year"), by.y = c("cousub_fips", "year_p1"), all.x = TRUE)
pa_dose_scatter <- merge(pa_dose_scatter, pa_share_lead,
  by.x = c("cousub_fips", "year"), by.y = c("cousub_fips", "year_m2"), all.x = TRUE)
pa_dose_scatter[, d_share := share_post - share_pre]
pa_dose_scatter <- pa_dose_scatter[!is.na(d_share)]

# Raw scatter
p_pa_dose_scatter <- ggplot(pa_dose_scatter, aes(x = rate_change, y = d_share)) +
  geom_point(aes(color = event_type), alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
  labs(x = "EIT Rate Change (pp)", y = "Change in Share Living In (t-1 to t+2)",
    title = "PA: Rate Changes vs Residential Sorting (Raw)",
    subtitle = "Each dot = one municipality rate change. Raw OLS is null; panel FE needed.",
    color = "Direction") +
  theme_minimal()
print(p_pa_dose_scatter)

# Binscatter
pa_dose_scatter[, rate_bin := cut(rate_change,
  breaks = c(-3, -1, -0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5, 1, 3),
  include.lowest = TRUE)]

pa_bin_means <- pa_dose_scatter[!is.na(rate_bin), .(
  mean_d_share = mean(d_share, na.rm = TRUE),
  se = sd(d_share, na.rm = TRUE) / sqrt(.N),
  n = .N,
  rate_mid = mean(rate_change)
), by = rate_bin]

p_pa_dose_bin <- ggplot(pa_bin_means, aes(x = rate_mid, y = mean_d_share)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = mean_d_share - 1.96 * se,
    ymax = mean_d_share + 1.96 * se, size = n), color = "firebrick") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_size_continuous(range = c(1, 4)) +
  labs(x = "EIT Rate Change (pp)", y = "Mean Change in Share Living In",
    title = "PA: Dose-Response Binscatter (Raw)",
    subtitle = "Binned means with 95% CI. Panel FE estimates: -0.009** to -0.013*",
    size = "N events") +
  theme_minimal()
print(p_pa_dose_bin)

## PA: Fiscal outcome scatters — what happens to city finances around rate changes? ----
# For each rate change event, compute change in fiscal outcomes (t-1 to t+2)

if (file.exists(file.path(data_dir, "pa_fiscal_panel.rds"))) {
  pa_fisc <- readRDS(file.path(data_dir, "pa_fiscal_panel.rds"))

  # Year-over-year changes in fiscal variables
  pa_fisc[, `:=`(
    d_eit_rev = eit_rev - shift(eit_rev, 1),
    d_re_rev = re_rev - shift(re_rev, 1),
    d_police = as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)) -
      shift(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)), 1),
    d_streets = as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads)) -
      shift(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads)), 1),
    d_tot_rev = tot_rev - shift(tot_rev, 1),
    d_tot_exp = tot_exp - shift(tot_exp, 1),
    d_debt = as.numeric(total_debt) - shift(as.numeric(total_debt), 1)
  ), by = cousub_fips]

  # Merge rate changes
  pa_rate_changes <- pa_rate_panel[!is.na(rate_change) & abs(rate_change) > 1e-6 &
    !is.na(cousub_fips) & rate_change < 5,
    .(cousub_fips, year, rate_change, event_type)]

  fisc_scatter <- merge(pa_rate_changes, pa_fisc,
    by.x = c("cousub_fips", "year"), by.y = c("cousub_fips", "yr"), all.x = TRUE)

  # Pct changes for better comparability
  fisc_scatter[, `:=`(
    pct_eit = d_eit_rev / shift(eit_rev, 1) * 100,
    pct_police = d_police / (as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)) - d_police) * 100,
    pct_tot_rev = d_tot_rev / (tot_rev - d_tot_rev) * 100,
    pct_streets = d_streets / (as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads)) - d_streets) * 100
  ), by = cousub_fips]

  # Winsorize extremes for plotting
  winsorize <- function(x, lo = -100, hi = 200) pmin(pmax(x, lo, na.rm = TRUE), hi, na.rm = TRUE)

  fisc_scatter[, `:=`(
    pct_eit_w = winsorize(pct_eit),
    pct_police_w = winsorize(pct_police),
    pct_tot_rev_w = winsorize(pct_tot_rev),
    pct_streets_w = winsorize(pct_streets)
  )]

  ## EIT rate change vs EIT revenue change
  p_fisc_eit <- ggplot(fisc_scatter[!is.na(pct_eit_w)],
      aes(x = rate_change, y = pct_eit_w)) +
    geom_point(aes(color = event_type), alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
    labs(x = "EIT Rate Change (pp)", y = "% Change in EIT Revenue",
      title = "PA: Rate Change vs EIT Revenue Change",
      color = "Direction") +
    theme_minimal()
  print(p_fisc_eit)

  ## EIT rate change vs police spending change
  p_fisc_police <- ggplot(fisc_scatter[!is.na(pct_police_w)],
      aes(x = rate_change, y = pct_police_w)) +
    geom_point(aes(color = event_type), alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
    labs(x = "EIT Rate Change (pp)", y = "% Change in Police Spending",
      title = "PA: Rate Change vs Police Budget Change",
      color = "Direction") +
    theme_minimal()
  print(p_fisc_police)

  ## EIT rate change vs total revenue change
  p_fisc_rev <- ggplot(fisc_scatter[!is.na(pct_tot_rev_w)],
      aes(x = rate_change, y = pct_tot_rev_w)) +
    geom_point(aes(color = event_type), alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
    labs(x = "EIT Rate Change (pp)", y = "% Change in Total Revenue",
      title = "PA: Rate Change vs Total Revenue Change",
      color = "Direction") +
    theme_minimal()
  print(p_fisc_rev)

  ## EIT rate change vs streets spending change
  p_fisc_streets <- ggplot(fisc_scatter[!is.na(pct_streets_w)],
      aes(x = rate_change, y = pct_streets_w)) +
    geom_point(aes(color = event_type), alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
    labs(x = "EIT Rate Change (pp)", y = "% Change in Streets/Roads Spending",
      title = "PA: Rate Change vs Streets Budget Change",
      color = "Direction") +
    theme_minimal()
  print(p_fisc_streets)

  ## Multi-panel: log changes (handles zeros better, filters small munis)
  # Use log(t+1) - log(t-1) for 2-year change around the event
  fisc_scatter[, `:=`(
    dlog_eit = log(eit_rev + 1) - log(shift(eit_rev, 1) + 1),
    dlog_police = log(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)) + 1) -
      log(shift(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)), 1) + 1),
    dlog_tot_rev = log(tot_rev + 1) - log(shift(tot_rev, 1) + 1),
    dlog_streets = log(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads)) + 1) -
      log(shift(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads)), 1) + 1)
  ), by = cousub_fips]

  # Filter: keep only municipalities with >$10K EIT revenue and >$50K total revenue
  fisc_clean <- fisc_scatter[eit_rev > 10000 & tot_rev > 50000]

  fisc_long <- melt(fisc_clean[, .(rate_change, event_type,
    `EIT Revenue` = dlog_eit,
    `Total Revenue` = dlog_tot_rev,
    `Police` = dlog_police,
    `Streets` = dlog_streets)],
    id.vars = c("rate_change", "event_type"),
    variable.name = "outcome", value.name = "log_change")

  # Winsorize log changes at +/- 1 (roughly ±100%)
  fisc_long[, log_change_w := pmin(pmax(log_change, -1, na.rm = TRUE), 1, na.rm = TRUE)]

  p_fisc_multi <- ggplot(fisc_long[!is.na(log_change_w) & is.finite(log_change_w)],
      aes(x = rate_change, y = log_change_w)) +
    geom_point(aes(color = event_type), alpha = 0.3, size = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~outcome, scales = "free_y") +
    scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
    labs(x = "EIT Rate Change (pp)", y = "Log Change in Outcome",
      title = "PA: How Municipal Finances Respond to EIT Rate Changes",
      subtitle = "Municipalities with >$10K EIT revenue. Log changes, winsorized at +/-100%.",
      color = "Direction") +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(p_fisc_multi)
}


# 4. Ohio OD Estimation ----

oh_samp <- oh_panel[total_jobs > 50]

## 4a. TWFE: share_live_in ~ rate | city + year ----
oh_m1 <- feols(share_live_in ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)

## 4b. Log outcomes ----
oh_m2 <- feols(log_live_in ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)
oh_m3 <- feols(log_live_out ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)
oh_m4 <- feols(log_total_jobs ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)

etable(oh_m1, oh_m2, oh_m3, oh_m4,
  headers = c("Share Live In", "log(Live In)", "log(Live Outside)", "log(Total Jobs)"))

## 4c. First differences ----
oh_m_fd <- feols(d_share ~ d_rate | year,
  data = oh_samp[!is.na(d_share) & !is.na(d_rate)],
  cluster = ~work_city)
etable(oh_m_fd, headers = "OH: First Differences")

## 4d. Event study ----
oh_first_change <- oh_samp[!is.na(rate_change) & abs(rate_change) > 1e-6,
  .(event_year = min(year)), by = work_city]
oh_event <- merge(oh_samp, oh_first_change, by = "work_city")
oh_event[, rel_year := year - event_year]
oh_event_samp <- oh_event[abs(rel_year) <= 5]

oh_m_event <- feols(share_live_in ~ i(rel_year, ref = -1) | work_city + year,
  data = oh_event_samp, cluster = ~work_city)
etable(oh_m_event, headers = "OH: Event Study (share_live_in)")

## 4e. Stacked DiD with neighbor controls ----
oh_treatment_events <- oh_rate_panel[event_type == "increase",
  .(muni_fips, event_year = year, rate_change)] %>% unique()

build_stacked <- function(panel, treatment_events, neighbors, id_col = "work_city",
                          rate_panel = NULL, rate_id = "muni_fips") {
  stacked_list <- list()
  for (j in seq_len(nrow(treatment_events))) {
    tf <- treatment_events[[rate_id]][j]
    ey <- treatment_events$event_year[j]
    window <- seq(max(ey - 5L, 2002L), min(ey + 5L, 2021L))
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

    cohort_data <- panel[get(id_col) %in% c(tf, clean_controls) & year %in% window]
    if (nrow(cohort_data) == 0) next

    cohort_data[, `:=`(
      cohort_id = paste0(tf, "_", ey),
      treated = get(id_col) == tf,
      rel_year = year - ey,
      post = year >= ey,
      cohort_city = paste0(tf, "_", ey, "_", get(id_col))
    )]
    stacked_list[[j]] <- cohort_data
  }
  rbindlist(stacked_list)
}

oh_stacked <- build_stacked(oh_samp, oh_treatment_events, oh_neighbors,
  rate_panel = oh_rate_panel, rate_id = "muni_fips")

print(paste("OH stacked:", nrow(oh_stacked), "rows,",
  uniqueN(oh_stacked$cohort_id), "cohorts"))

oh_m_stacked <- feols(
  share_live_in ~ i(rel_year, treated, ref = -1) | cohort_city + cohort_id^year,
  data = oh_stacked[total_jobs > 50],
  cluster = ~work_city
)
etable(oh_m_stacked, headers = "OH: Stacked Event Study")

## 4f. Out-of-state share: does tax increase push workers across state lines? ----
oh_m_oos <- feols(share_out_of_state ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)
oh_m_isd <- feols(share_in_state_diff ~ rate | work_city + year,
  data = oh_samp, cluster = ~work_city)

etable(oh_m1, oh_m_oos, oh_m_isd,
  headers = c("Share Live In", "Share Out-of-State", "Share In-State Diff City"))

## 4g. Event study: out-of-state share ----
oh_m_event_oos <- feols(share_out_of_state ~ i(rel_year, ref = -1) | work_city + year,
  data = oh_event_samp, cluster = ~work_city)
etable(oh_m_event, oh_m_event_oos,
  headers = c("Event: Share Live In", "Event: Share Out-of-State"))

## 4h. High earners vs all workers ----
oh_m_high <- feols(share_live_in ~ rate | work_city + year,
  data = oh_panel_high[total_jobs > 20], cluster = ~work_city)
etable(oh_m1, oh_m_high,
  headers = c("All Workers", "High Earners (>$3,333/mo)"))


# 5. Pennsylvania OD Estimation ----

pa_samp <- pa_panel[total_jobs > 50 & !is.na(resident_eit)]

## 5a. TWFE: share_live_in ~ rate | city + year ----
# PA sample with fiscal controls
pa_samp_ctrl <- pa_samp[!is.na(log_police) & !is.na(log_streets) &
  !is.na(log_debt) & !is.na(log_state_aid)]

pa_m1 <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
pa_m1_ctrl <- feols(share_live_in ~ resident_eit + log_police + log_streets +
  log_debt + log_state_aid | work_city + year,
  data = pa_samp_ctrl, cluster = ~work_city)

## 5b. Log outcomes ----
pa_m2 <- feols(log_live_in ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
pa_m3 <- feols(log_live_out ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
pa_m4 <- feols(log_total_jobs ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)

etable(pa_m1, pa_m1_ctrl, pa_m2, pa_m3, pa_m4,
  headers = c("Share Live In", "Share + Controls", "log(Live In)", "log(Live Outside)", "log(Total Jobs)"),
  keep = c("resident_eit"))

## 5c. First differences ----
pa_m_fd <- feols(d_share ~ d_rate | year,
  data = pa_samp[!is.na(d_share) & !is.na(d_rate) & abs(d_rate) > 1e-6],
  cluster = ~work_city)
etable(pa_m_fd, headers = "PA: First Differences")

## 5d. Event study ----
pa_first_change <- pa_samp[!is.na(rate_change) & abs(rate_change) > 1e-6,
  .(event_year = min(year)), by = work_city]
pa_event <- merge(pa_samp, pa_first_change, by = "work_city")
pa_event[, rel_year := year - event_year]
pa_event_samp <- pa_event[abs(rel_year) <= 5]

pa_m_event <- feols(share_live_in ~ i(rel_year, ref = -1) | work_city + year,
  data = pa_event_samp, cluster = ~work_city)

pa_event_ctrl <- merge(pa_samp_ctrl, pa_first_change, by = "work_city")
pa_event_ctrl[, rel_year := year - event_year]
pa_event_ctrl <- pa_event_ctrl[abs(rel_year) <= 5]

pa_m_event_ctrl <- feols(share_live_in ~ i(rel_year, ref = -1) + log_police +
  log_streets + log_debt + log_state_aid | work_city + year,
  data = pa_event_ctrl, cluster = ~work_city)

etable(pa_m_event, pa_m_event_ctrl,
  headers = c("PA Event Study", "PA Event Study + Controls"),
  keep = c("rel_year"))

## 5e. Out-of-state share ----
pa_m_oos <- feols(share_out_of_state ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)
pa_m_isd <- feols(share_in_state_diff ~ resident_eit | work_city + year,
  data = pa_samp, cluster = ~work_city)

etable(pa_m1, pa_m_oos, pa_m_isd,
  headers = c("Share Live In", "Share Out-of-State", "Share In-State Diff Muni"))

## 5f. Event study: out-of-state share ----
pa_m_event_oos <- feols(share_out_of_state ~ i(rel_year, ref = -1) | work_city + year,
  data = pa_event_samp, cluster = ~work_city)
etable(pa_m_event, pa_m_event_oos,
  headers = c("Event: Share Live In", "Event: Share Out-of-State"))

## 5g. High earners ----
pa_m_high <- feols(share_live_in ~ resident_eit | work_city + year,
  data = pa_panel_high[total_jobs > 20 & !is.na(resident_eit)],
  cluster = ~work_city)
etable(pa_m1, pa_m_high,
  headers = c("PA All Workers", "PA High Earners"))


# 6. Pooled OH + PA ----

oh_stack <- oh_samp[, .(state = "OH", city = work_city, year, share_live_in,
  rate, total_jobs, log_live_in, log_live_out, log_total_jobs)]
pa_stack <- pa_samp[, .(state = "PA", city = work_city, year, share_live_in,
  rate = resident_eit, total_jobs, log_live_in, log_live_out, log_total_jobs)]
pooled <- rbind(oh_stack, pa_stack)
pooled[, state_city := paste0(state, "_", city)]

pooled_m <- feols(share_live_in ~ rate | state_city + year,
  data = pooled, cluster = ~state_city)

# Pooled out-of-state
oh_stack_oos <- oh_samp[, .(state = "OH", city = work_city, year,
  share_out_of_state, rate, total_jobs)]
pa_stack_oos <- pa_samp[, .(state = "PA", city = work_city, year,
  share_out_of_state, rate = resident_eit, total_jobs)]
pooled_oos <- rbind(oh_stack_oos, pa_stack_oos)
pooled_oos[, state_city := paste0(state, "_", city)]

pooled_m_oos <- feols(share_out_of_state ~ rate | state_city + year,
  data = pooled_oos, cluster = ~state_city)

etable(oh_m1, pa_m1, pooled_m, pooled_m_oos,
  headers = c("OH: Live In", "PA: Live In", "Pooled: Live In", "Pooled: Out-of-State"))


# 7. Robustness & Extensions ----

## 7a. OH: Adoption DiD (binary tax on/off) ----
# Cities that went from 0 -> positive rate
oh_samp[, adopted := as.integer(rate > 0)]

# Identify cities that ever had rate = 0 then rate > 0 (true adopters)
oh_adoption_cities <- oh_rate_panel[!is.na(rate_lag) & rate_lag == 0 & rate > 0, unique(muni_fips)]
oh_removal_cities <- oh_rate_panel[!is.na(rate_lag) & rate_lag > 0 & rate == 0, unique(muni_fips)]

oh_samp[, is_adopter := work_city %in% oh_adoption_cities]
oh_samp[, is_remover := work_city %in% oh_removal_cities]

## 7b. Rate decrease mirror test ----
# If increases push people out, decreases should pull them in
oh_first_decrease <- oh_rate_panel[event_type == "decrease",
  .(first_decrease_year = min(year)), by = muni_fips]
oh_samp <- merge(oh_samp, oh_first_decrease,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_samp[, post_decrease := as.integer(!is.na(first_decrease_year) & year >= first_decrease_year)]
oh_samp[, has_decrease := !is.na(first_decrease_year)]

# Event study for decreases
oh_decrease_event <- oh_samp[has_decrease == TRUE]
oh_decrease_event[, rel_year_dec := year - first_decrease_year]
oh_dec_samp <- oh_decrease_event[abs(rel_year_dec) <= 5 & total_jobs > 50]

if (nrow(oh_dec_samp) > 100) {
  oh_m_decrease <- feols(share_live_in ~ i(rel_year_dec, ref = -1) | work_city + year,
    data = oh_dec_samp, cluster = ~work_city)
  etable(oh_m_decrease, headers = "OH: Event Study — Rate Decreases")
}

## PA: Rate decrease mirror test ----
pa_first_decrease <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
  .(first_decrease_year = min(year)), by = cousub_fips]
pa_samp <- merge(pa_samp, pa_first_decrease,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_samp[, post_decrease := as.integer(!is.na(first_decrease_year) & year >= first_decrease_year)]

pa_decrease_event <- pa_samp[!is.na(first_decrease_year)]
pa_decrease_event[, rel_year_dec := year - first_decrease_year]
pa_dec_samp <- pa_decrease_event[abs(rel_year_dec) <= 5 & total_jobs > 50]

if (nrow(pa_dec_samp) > 100) {
  pa_m_decrease <- feols(share_live_in ~ i(rel_year_dec, ref = -1) | work_city + year,
    data = pa_dec_samp, cluster = ~work_city)
  etable(pa_m_decrease, headers = "PA: Event Study — Rate Decreases")
}

## 7c. Asymmetry test: increases vs decreases in one model ----
# OH
oh_samp[, post_increase := as.integer(!is.na(first_increase_year) & year >= first_increase_year)]
oh_samp[, has_increase := !is.na(first_increase_year)]

oh_m_asym <- feols(
  share_live_in ~ has_increase:post_increase + has_decrease:post_decrease | work_city + year,
  data = oh_samp[total_jobs > 50],
  cluster = ~work_city
)
etable(oh_m_asym, headers = "OH: Asymmetry (Increase vs Decrease)")

# PA
pa_samp[, post_increase := as.integer(!is.na(first_increase_year) & year >= first_increase_year)]
pa_samp[, has_increase := !is.na(first_increase_year)]
pa_samp[, has_decrease := !is.na(first_decrease_year)]

pa_m_asym <- feols(
  share_live_in ~ has_increase:post_increase + has_decrease:post_decrease | work_city + year,
  data = pa_samp[total_jobs > 50],
  cluster = ~work_city
)
etable(oh_m_asym, pa_m_asym, headers = c("OH: Asymmetry", "PA: Asymmetry"))

## 7d. Dose-response: bigger changes -> bigger effects ----
# Interact post with size of rate change
oh_samp[, increase_size := fifelse(!is.na(first_increase_year),
  oh_rate_panel[event_type == "increase",
    .(size = max(rate_change)), by = muni_fips][
    match(oh_samp$work_city, muni_fips), size],
  0)]

oh_m_dose <- feols(
  share_live_in ~ increase_size:post_increase | work_city + year,
  data = oh_samp[total_jobs > 50],
  cluster = ~work_city
)
etable(oh_m1, oh_m_dose, headers = c("OH: Rate Level", "OH: Dose-Response"))

## 7e. Sun-Abraham staggered event study ----
# Uses sunab() from fixest to handle heterogeneous treatment effects
# Cohort = first_increase_year; never-treated cities are the control

# OH Sun-Abraham
oh_sunab_samp <- oh_samp[total_jobs > 50]
oh_sunab_samp[, cohort := fifelse(is.na(first_increase_year), 10000L,
  as.integer(first_increase_year))]

oh_m_sunab <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = oh_sunab_samp,
  cluster = ~work_city
)
etable(oh_m_sunab, headers = "OH: Sun-Abraham Event Study")

# PA Sun-Abraham
pa_sunab_samp <- pa_samp[total_jobs > 50]
pa_sunab_samp[, cohort := fifelse(is.na(first_increase_year), 10000L,
  as.integer(first_increase_year))]

pa_m_sunab <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_sunab_samp,
  cluster = ~work_city
)
etable(pa_m_sunab, headers = "PA: Sun-Abraham Event Study")

# Compare TWFE vs Sun-Abraham
etable(oh_m_event, oh_m_sunab, headers = c("OH: TWFE Event Study", "OH: Sun-Abraham"))
etable(pa_m_event, pa_m_sunab, headers = c("PA: TWFE Event Study", "PA: Sun-Abraham"))

## 7f. Sun-Abraham for decreases ----
oh_sunab_dec <- oh_samp[total_jobs > 50]
oh_sunab_dec[, cohort_dec := fifelse(is.na(first_decrease_year), 10000L,
  as.integer(first_decrease_year))]

if (oh_sunab_dec[cohort_dec < 10000, uniqueN(work_city)] > 10) {
  oh_m_sunab_dec <- feols(
    share_live_in ~ sunab(cohort_dec, year) | work_city + year,
    data = oh_sunab_dec,
    cluster = ~work_city
  )
  etable(oh_m_sunab_dec, headers = "OH: Sun-Abraham — Decreases")
}

pa_sunab_dec <- pa_samp[total_jobs > 50]
pa_sunab_dec[, cohort_dec := fifelse(is.na(first_decrease_year), 10000L,
  as.integer(first_decrease_year))]

if (pa_sunab_dec[cohort_dec < 10000, uniqueN(work_city)] > 10) {
  pa_m_sunab_dec <- feols(
    share_live_in ~ sunab(cohort_dec, year) | work_city + year,
    data = pa_sunab_dec,
    cluster = ~work_city
  )
  etable(pa_m_sunab_dec, headers = "PA: Sun-Abraham — Decreases")
}

## 7g. Cleaned stacked DiD ----
# Exclude neighbors that: (1) changed rate within ±2 years, (2) have higher rate (PA)
# OH: exclude any neighbor with a tax
oh_stacked_clean <- build_stacked(
  oh_samp[total_jobs > 50],
  oh_treatment_events,
  oh_neighbors[neighbor_fips %in% oh_ctrl_no_tax],  # only no-tax neighbors
  rate_panel = oh_rate_panel,
  rate_id = "muni_fips"
)

if (nrow(oh_stacked_clean) > 100) {
  oh_m_stacked_clean <- feols(
    share_live_in ~ i(rel_year, treated, ref = -1) | cohort_city + cohort_id^year,
    data = oh_stacked_clean[total_jobs > 50],
    cluster = ~work_city
  )
  etable(oh_m_stacked, oh_m_stacked_clean,
    headers = c("OH Stacked: All Neighbors", "OH Stacked: No-Tax Neighbors Only"))
}


# 7h. Continuous Rate Dynamics ----

## Build cumulative rate change and rate vs baseline variables
# OH: cumulative change = current rate - first observed rate for each city
oh_baseline_rate <- oh_rate_panel[, .(baseline_rate = first(rate)), by = muni_fips]
oh_samp <- merge(oh_samp, oh_baseline_rate,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_samp[, `:=`(
  rate_vs_baseline = rate - fifelse(is.na(baseline_rate), 0, baseline_rate),
  cumul_change = rate - fifelse(is.na(baseline_rate), 0, baseline_rate)
)]

# PA: same
pa_baseline_rate <- pa_rate_panel[!is.na(cousub_fips),
  .(baseline_rate = first(na.omit(resident_eit))), by = cousub_fips]
pa_samp <- merge(pa_samp, pa_baseline_rate,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_samp[, `:=`(
  rate_vs_baseline = resident_eit - fifelse(is.na(baseline_rate), 0, baseline_rate),
  cumul_change = resident_eit - fifelse(is.na(baseline_rate), 0, baseline_rate)
)]

## Model 1: Cumulative rate change (captures all increases/decreases) ----
oh_m_cumul <- feols(share_live_in ~ cumul_change | work_city + year,
  data = oh_samp[total_jobs > 50], cluster = ~work_city)

pa_m_cumul <- feols(share_live_in ~ cumul_change | work_city + year,
  data = pa_samp[total_jobs > 50 & !is.na(cumul_change)], cluster = ~work_city)

etable(oh_m1, oh_m_cumul, pa_m1, pa_m_cumul,
  headers = c("OH: Rate Level", "OH: Cumul Change", "PA: Rate Level", "PA: Cumul Change"))

## Model 2: Rate level × post adoption ----
# For cities that adopted (0 -> positive): does the rate level matter?
oh_samp[, ever_had_tax := rate > 0 | baseline_rate > 0]

oh_m_rate_x_post <- feols(
  share_live_in ~ rate:post_increase | work_city + year,
  data = oh_samp[total_jobs > 50 & has_increase == TRUE],
  cluster = ~work_city
)

pa_m_rate_x_post <- feols(
  share_live_in ~ resident_eit:post_increase | work_city + year,
  data = pa_samp[total_jobs > 50 & has_increase == TRUE & !is.na(resident_eit)],
  cluster = ~work_city
)

etable(oh_m_rate_x_post, pa_m_rate_x_post,
  headers = c("OH: Rate × Post (treated cities)", "PA: Rate × Post (treated cities)"))

## Model 3: Separate effects of rate level vs rate change ----
# Rate level = cross-sectional + within (absorbed by city FE leaves only within)
# Cumulative change = purely within-city temporal variation
# These should give similar coefficients if the within variation drives the result
oh_m_both <- feols(
  share_live_in ~ rate + cumul_change | work_city + year,
  data = oh_samp[total_jobs > 50], cluster = ~work_city
)

pa_m_both <- feols(
  share_live_in ~ resident_eit + cumul_change | work_city + year,
  data = pa_samp[total_jobs > 50 & !is.na(cumul_change) & !is.na(resident_eit)],
  cluster = ~work_city
)

etable(oh_m_both, pa_m_both,
  headers = c("OH: Rate + Cumul Change", "PA: Rate + Cumul Change"))

## Model 4: Non-linear rate effects ----
# Maybe the first 1% doesn't matter but going from 2% to 3% does
oh_samp[, rate_sq := rate^2]
oh_m_nonlin <- feols(share_live_in ~ rate + rate_sq | work_city + year,
  data = oh_samp[total_jobs > 50], cluster = ~work_city)

pa_samp[, rate_sq := resident_eit^2]
pa_m_nonlin <- feols(share_live_in ~ resident_eit + rate_sq | work_city + year,
  data = pa_samp[total_jobs > 50 & !is.na(resident_eit)], cluster = ~work_city)

etable(oh_m_nonlin, pa_m_nonlin,
  headers = c("OH: Quadratic Rate", "PA: Quadratic Rate"))

## Model 5: Rate bins — where does the effect kick in? ----
oh_samp[, rate_bin := cut(rate * 100,
  breaks = c(-0.1, 0, 0.5, 1, 1.5, 2, 3),
  labels = c("0%", "0-0.5%", "0.5-1%", "1-1.5%", "1.5-2%", "2-3%"))]

oh_m_bins <- feols(share_live_in ~ rate_bin | work_city + year,
  data = oh_samp[total_jobs > 50 & !is.na(rate_bin)], cluster = ~work_city)

etable(oh_m_bins, headers = "OH: Rate Bins")

## Model 6: Interact cumulative change with time since change ----
# Does the effect grow over time? (matches what Sun-Abraham shows)
oh_samp[, years_since_first_increase := fifelse(
  !is.na(first_increase_year) & year >= first_increase_year,
  year - first_increase_year, 0L)]

oh_m_time_interact <- feols(
  share_live_in ~ cumul_change + cumul_change:years_since_first_increase | work_city + year,
  data = oh_samp[total_jobs > 50 & has_increase == TRUE],
  cluster = ~work_city
)

pa_samp[, years_since_first_increase := fifelse(
  !is.na(first_increase_year) & year >= first_increase_year,
  year - first_increase_year, 0L)]

pa_m_time_interact <- feols(
  share_live_in ~ cumul_change + cumul_change:years_since_first_increase | work_city + year,
  data = pa_samp[total_jobs > 50 & has_increase == TRUE & !is.na(cumul_change)],
  cluster = ~work_city
)

etable(oh_m_time_interact, pa_m_time_interact,
  headers = c("OH: Change × Time Since", "PA: Change × Time Since"))


# 8. Event Study Plots ----

## OH event study ----
p_oh_event <- iplot(oh_m_event,
  main = "OH: Share Living in City After Tax Rate Change")
print(p_oh_event)

## PA event study ----
p_pa_event <- iplot(pa_m_event,
  main = "PA: Share Living in Municipality After EIT Rate Change")
print(p_pa_event)

## OH stacked event study ----
p_oh_stacked <- iplot(oh_m_stacked,
  main = "OH: Stacked DiD — Treated vs Neighbor Controls")
print(p_oh_stacked)

## OH out-of-state event study ----
p_oh_oos <- iplot(oh_m_event_oos,
  main = "OH: Share of Workers Living Out-of-State After Tax Increase")
print(p_oh_oos)

## PA out-of-state event study ----
p_pa_oos <- iplot(pa_m_event_oos,
  main = "PA: Share of Workers Living Out-of-State After EIT Increase")
print(p_pa_oos)

## Sun-Abraham plots ----
p_oh_sunab <- iplot(oh_m_sunab,
  main = "OH: Sun-Abraham Event Study (share_live_in)")
print(p_oh_sunab)

p_pa_sunab <- iplot(pa_m_sunab,
  main = "PA: Sun-Abraham Event Study (share_live_in)")
print(p_pa_sunab)

## Decrease event study plots ----
if (exists("oh_m_decrease")) {
  p_oh_dec <- iplot(oh_m_decrease,
    main = "OH: Rate Decreases — Do Workers Come Back?")
  print(p_oh_dec)
}
if (exists("pa_m_decrease")) {
  p_pa_dec <- iplot(pa_m_decrease,
    main = "PA: EIT Decreases — Do Workers Come Back?")
  print(p_pa_dec)
}

## Sun-Abraham decrease plots ----
if (exists("oh_m_sunab_dec")) {
  p_oh_sunab_dec <- iplot(oh_m_sunab_dec,
    main = "OH: Sun-Abraham — Rate Decreases")
  print(p_oh_sunab_dec)
}
if (exists("pa_m_sunab_dec")) {
  p_pa_sunab_dec <- iplot(pa_m_sunab_dec,
    main = "PA: Sun-Abraham — EIT Decreases")
  print(p_pa_sunab_dec)
}

# 9. Dose-Response Identification (Key Result) ----
# Among TREATED cities only: does the rate level matter beyond binary treatment?
# If fiscal stress alone drives the result, rate magnitude shouldn't matter.

## OH treatment info
oh_treated_info <- oh_rate_panel[event_type == "increase",
  .(max_increase = max(rate_change), first_inc_yr = min(year)), by = muni_fips]

oh_tp <- merge(oh_samp, oh_treated_info, by.x = "work_city", by.y = "muni_fips")
oh_tp[, post_tr := as.integer(year >= first_inc_yr)]

## PA treatment info
pa_treated_info <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips) &
  !is.na(rate_change) & rate_change < 5,
  .(max_increase = max(rate_change), first_inc_yr = min(year)), by = cousub_fips]

pa_tp <- merge(pa_samp, pa_treated_info, by.x = "work_city", by.y = "cousub_fips")
pa_tp[, post_tr := as.integer(year >= first_inc_yr)]

## 9a. Post only (fiscal stress) ----
oh_dose_m1 <- feols(share_live_in ~ post_tr | work_city + year,
  data = oh_tp[total_jobs > 50], cluster = ~work_city)

pa_dose_m1 <- feols(share_live_in ~ post_tr | work_city + year,
  data = pa_tp[total_jobs > 50], cluster = ~work_city)

## 9b. Rate level × post (tax channel) ----
oh_dose_m2 <- feols(share_live_in ~ rate:post_tr | work_city + year,
  data = oh_tp[total_jobs > 50], cluster = ~work_city)

pa_dose_m2 <- feols(share_live_in ~ resident_eit:post_tr | work_city + year,
  data = pa_tp[total_jobs > 50], cluster = ~work_city)

## 9c. Post + rate × post (decomposed) ----
oh_dose_m3 <- feols(share_live_in ~ post_tr + rate:post_tr | work_city + year,
  data = oh_tp[total_jobs > 50], cluster = ~work_city)

pa_dose_m3 <- feols(share_live_in ~ post_tr + resident_eit:post_tr | work_city + year,
  data = pa_tp[total_jobs > 50], cluster = ~work_city)

etable(oh_dose_m1, oh_dose_m2, oh_dose_m3,
  headers = c("OH: Post Only", "OH: Rate x Post", "OH: Post + Rate x Post"))

etable(pa_dose_m1, pa_dose_m2, pa_dose_m3,
  headers = c("PA: Post Only", "PA: Rate x Post", "PA: Post + Rate x Post"))


# 10. City-Pair OD Flows ----
# Where do movers go? Track flows FROM treated cities TO neighbors.

oh_od_raw <- as.data.table(readRDS(file.path(data_dir, "lodes", "oh_od_S000_main.rds")))
oh_od_raw[, year := as.integer(year)]
oh_xw <- as.data.table(readRDS(file.path(data_dir, "oh_tract_xwalk.rds")))

home_xw <- copy(oh_xw[, .(tract_geoid, place_fips)])
work_xw <- copy(oh_xw[, .(tract_geoid, place_fips)])
setnames(home_xw, c("tract_geoid", "place_fips"), c("h_tract", "home_city"))
setnames(work_xw, c("tract_geoid", "place_fips"), c("w_tract", "work_city"))

oh_od_raw <- merge(oh_od_raw, home_xw, by = "h_tract", all.x = TRUE)
oh_od_raw <- merge(oh_od_raw, work_xw, by = "w_tract", all.x = TRUE)

city_pair <- oh_od_raw[!is.na(work_city) & !is.na(home_city) & work_city != home_city,
  .(workers = sum(S000, na.rm = TRUE)), by = .(work_city, home_city, year)]

# Keep flows from treated cities to neighbors
treated_flows <- merge(city_pair,
  oh_treated_info[, .(work_city = muni_fips, first_inc_yr)], by = "work_city")
treated_flows <- merge(treated_flows, oh_neighbors,
  by.x = c("work_city", "home_city"), by.y = c("city_fips", "neighbor_fips"), all.x = TRUE)
treated_flows <- treated_flows[!is.na(dist_km)]
treated_flows[, `:=`(
  post = as.integer(year >= first_inc_yr),
  log_workers = log(workers + 1),
  pair_id = paste0(work_city, "_", home_city)
)]

## 10a. Do flows to neighbors increase? ----
m_pair1 <- feols(log_workers ~ post | pair_id + year,
  data = treated_flows, cluster = ~work_city)

## 10b. Distance gradient ----
treated_flows[, dist_bin := cut(dist_km, breaks = c(0, 10, 20, 40, 80),
  labels = c("0-10km", "10-20km", "20-40km", "40-80km"))]

m_pair2 <- feols(log_workers ~ post:dist_bin | pair_id + year,
  data = treated_flows[!is.na(dist_bin)], cluster = ~work_city)

## 10c. Tax status of destination ----
oh_all_taxed <- oh_rate_panel[rate > 0, unique(muni_fips)]
treated_flows[, nbr_has_tax := home_city %in% oh_all_taxed]

m_pair3 <- feols(log_workers ~ post + post:nbr_has_tax | pair_id + year,
  data = treated_flows, cluster = ~work_city)

etable(m_pair1, m_pair2, m_pair3,
  headers = c("Flow: Post", "Flow: Distance", "Flow: Nbr Tax Status"))

saveRDS(city_pair, file.path(data_dir, "oh_city_pair_od.rds"))


# 11. Rate Changes Over Time ----

## PA: rate change (x-axis) vs year of change (y-axis) ----
pa_changes <- pa_rate_panel[!is.na(rate_change) & abs(rate_change) > 1e-6 &
  !is.na(cousub_fips) & rate_change < 5]

p_pa_rate_yr <- ggplot(pa_changes, aes(x = rate_change, y = year)) +
  geom_jitter(aes(color = event_type), width = 0.02, height = 0.3, alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Rate Change (pp)", y = "Year of Change",
    title = "PA: Distribution of EIT Rate Changes Over Time",
    subtitle = "Each dot = one municipality-year rate change",
    color = "Direction") +
  theme_minimal()
print(p_pa_rate_yr)

## OH: same
oh_changes <- oh_rate_panel[!is.na(rate_change) & abs(rate_change) > 1e-6]
oh_changes[, rate_change_pp := rate_change * 100]

p_oh_rate_yr <- ggplot(oh_changes, aes(x = rate_change_pp, y = year)) +
  geom_jitter(aes(color = event_type), width = 0.02, height = 0.3, alpha = 0.5, size = 1.5) +
  scale_color_manual(values = c("increase" = "firebrick", "decrease" = "steelblue")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Rate Change (pp)", y = "Year of Change",
    title = "OH: Distribution of Municipal Income Tax Rate Changes Over Time",
    subtitle = "Each dot = one municipality-year rate change",
    color = "Direction") +
  theme_minimal()
print(p_oh_rate_yr)


# 12. Fiscal Impact: Revenue vs Base Erosion ----
# Uses PA DCED Annual Financial Report data
# Run: python scrape_pa_dced_afr.py (downloads actual municipal revenue/expenditure)

pa_afr_file <- file.path(data_dir, "pa_afr_panel.csv")

if (file.exists(pa_afr_file)) {
  pa_afr <- fread(pa_afr_file)

  # Use municipality_id column (municipal_code has NAs in some years)
  pa_afr[, muni_id := fifelse(!is.na(municipality_id), municipality_id, municipal_code)]

  # Crosswalk to cousub_fips
  pa_xwalk_afr <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  pa_afr <- merge(pa_afr, pa_xwalk_afr, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)

  # Year column
  pa_afr[, yr := fifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]

  # Merge EIT rate
  pa_rate_fiscal <- unique(pa_rate_panel[!is.na(cousub_fips),
    .(cousub_fips, year, resident_eit, first_increase_year)])
  pa_afr <- merge(pa_afr, pa_rate_fiscal,
    by.x = c("cousub_fips", "yr"), by.y = c("cousub_fips", "year"), all.x = TRUE)

  # Revenue columns (use longer-named versions where available)
  pa_afr[, `:=`(
    eit_rev = as.numeric(fifelse(!is.na(earned_income_tax_revenues),
      earned_income_tax_revenues, earned_income_tax)),
    re_rev = as.numeric(fifelse(!is.na(real_estate_tax_revenues),
      real_estate_tax_revenues, real_estate_tax)),
    tot_rev = as.numeric(fifelse(!is.na(total_revenues), total_revenues, total_revenue)),
    tot_exp = as.numeric(total_expenditures),
    tot_tax = as.numeric(fifelse(!is.na(total_taxes_revenues), total_taxes_revenues, total_taxes))
  )]

  pa_afr[, `:=`(
    log_eit = fifelse(eit_rev > 0, log(eit_rev), NA_real_),
    log_rev = fifelse(tot_rev > 0, log(tot_rev), NA_real_),
    log_re = fifelse(re_rev > 0, log(re_rev), NA_real_),
    log_exp = fifelse(tot_exp > 0, log(tot_exp), NA_real_),
    log_tax = fifelse(tot_tax > 0, log(tot_tax), NA_real_),
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
      `intergovernmental_revenues-state_government`, state_govt))), NA_real_),
    log_pop = fifelse(as.numeric(population) > 0, log(as.numeric(population)), NA_real_),
    log_mkt_val = fifelse(as.numeric(market_val_real_estate) > 0,
      log(as.numeric(market_val_real_estate)), NA_real_),
    pop = as.numeric(population)
  )]

  # Trim to 2019 to avoid COVID/ARPA fiscal distortion
  afr_samp <- pa_afr[!is.na(cousub_fips) & !is.na(resident_eit) & tot_rev > 0 & yr <= 2019]
  print(paste("Fiscal sample:", nrow(afr_samp), "municipality-years"))

  ## 12a. TWFE: Rate -> Revenue ----
  fisc_m1 <- feols(log_eit ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_eit)], cluster = ~cousub_fips)
  fisc_m2 <- feols(log_tax ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_tax)], cluster = ~cousub_fips)
  fisc_m3 <- feols(log_rev ~ resident_eit | cousub_fips + yr,
    data = afr_samp, cluster = ~cousub_fips)
  fisc_m4 <- feols(log_re ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_re)], cluster = ~cousub_fips)
  fisc_m5 <- feols(log_exp ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_exp)], cluster = ~cousub_fips)

  etable(fisc_m1, fisc_m2, fisc_m3, fisc_m4, fisc_m5,
    headers = c("log(EIT Rev)", "log(Total Tax)", "log(Total Rev)",
      "log(RE Tax)", "log(Expenditure)"))

  ## 12b. Sun-Abraham: EIT Revenue ----
  afr_samp[, cohort := fifelse(is.na(first_increase_year), 10000L,
    as.integer(first_increase_year))]

  fisc_sa <- feols(log_eit ~ sunab(cohort, yr) | cousub_fips + yr,
    data = afr_samp[!is.na(log_eit)], cluster = ~cousub_fips)

  p_fisc_sa <- iplot(fisc_sa,
    main = "PA: EIT Revenue Around Rate Increase (Sun-Abraham)")
  print(p_fisc_sa)

  saveRDS(afr_samp, file.path(data_dir, "pa_fiscal_panel.rds"))

  ## 12c. All fiscal outcomes ----
  fisc_m_re <- feols(log_re ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_re)], cluster = ~cousub_fips)
  fisc_m_police <- feols(log_police ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_police)], cluster = ~cousub_fips)
  fisc_m_exp <- feols(log_exp ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_exp)], cluster = ~cousub_fips)
  fisc_m_debt <- feols(log_debt ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_debt)], cluster = ~cousub_fips)
  fisc_m_state <- feols(log_state_aid ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_state_aid)], cluster = ~cousub_fips)
  fisc_m_pop <- feols(log_pop ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_pop) & pop > 0], cluster = ~cousub_fips)
  fisc_m_mkt <- feols(log_mkt_val ~ resident_eit | cousub_fips + yr,
    data = afr_samp[!is.na(log_mkt_val)], cluster = ~cousub_fips)

  etable(fisc_m1, fisc_m2, fisc_m3, fisc_m_re, fisc_m_police, fisc_m_exp, fisc_m_pop, fisc_m_mkt,
    headers = c("EIT Rev", "Total Tax", "Total Rev", "RE Tax", "Police",
      "Expenditure", "Population", "Mkt Value RE"),
    keep = "resident_eit")

  ## 12d. Fiscal asymmetry: increases vs decreases ----
  pa_first_dec <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
    .(first_dec_yr = min(year)), by = cousub_fips]
  afr_samp <- merge(afr_samp, pa_first_dec, by = "cousub_fips", all.x = TRUE)
  afr_samp[, `:=`(
    has_inc = !is.na(first_increase_year),
    post_inc = as.integer(!is.na(first_increase_year) & yr >= first_increase_year),
    has_dec = !is.na(first_dec_yr),
    post_dec = as.integer(!is.na(first_dec_yr) & yr >= first_dec_yr)
  )]

  fa_eit <- feols(log_eit ~ has_inc:post_inc + has_dec:post_dec | cousub_fips + yr,
    data = afr_samp[!is.na(log_eit)], cluster = ~cousub_fips)
  fa_tax <- feols(log_tax ~ has_inc:post_inc + has_dec:post_dec | cousub_fips + yr,
    data = afr_samp[!is.na(log_tax)], cluster = ~cousub_fips)
  fa_police <- feols(log_police ~ has_inc:post_inc + has_dec:post_dec | cousub_fips + yr,
    data = afr_samp[!is.na(log_police)], cluster = ~cousub_fips)
  fa_re <- feols(log_re ~ has_inc:post_inc + has_dec:post_dec | cousub_fips + yr,
    data = afr_samp[!is.na(log_re)], cluster = ~cousub_fips)

  etable(fa_eit, fa_tax, fa_police, fa_re,
    headers = c("EIT Rev", "Total Tax", "Police", "RE Tax Rev"))

} else {
  print("PA AFR data not found. Run: python scrape_pa_dced_afr.py")
}


# 13. Heterogeneity: Earnings and Age ----

## 13a. PA earnings heterogeneity (OD-based) ----
pa_low_file <- file.path(data_dir, "lodes", "pa_od_SE01_main.rds")
if (file.exists(pa_low_file)) {
  # Build low earner city OD
  build_seg_od <- function(od_rds) {
    od <- as.data.table(readRDS(od_rds))
    od[, year := as.integer(year)]
    pa_xw_seg <- as.data.table(readRDS(file.path(data_dir, "pa_tract_xwalk.rds")))
    h_xw <- copy(pa_xw_seg[, .(tract_geoid, cousub_fips)])
    w_xw <- copy(pa_xw_seg[, .(tract_geoid, cousub_fips)])
    setnames(h_xw, c("tract_geoid", "cousub_fips"), c("h_tract", "home_city"))
    setnames(w_xw, c("tract_geoid", "cousub_fips"), c("w_tract", "work_city"))
    od <- merge(od, h_xw, by = "h_tract", all.x = TRUE)
    od <- merge(od, w_xw, by = "w_tract", all.x = TRUE)
    city_od <- od[!is.na(work_city), .(
      total_jobs = sum(S000, na.rm = TRUE),
      live_and_work = sum(S000[home_city == work_city], na.rm = TRUE)
    ), by = .(work_city, year)]
    city_od[, share_live_in := live_and_work / total_jobs]
    pa_rate_seg <- unique(pa_rate_panel[!is.na(cousub_fips), .(cousub_fips, year, resident_eit)])
    merge(city_od, pa_rate_seg, by.x = c("work_city", "year"),
      by.y = c("cousub_fips", "year"), all.x = TRUE)
  }

  pa_low_panel <- build_seg_od(pa_low_file)

  # TWFE: low vs all vs high
  m_low <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_low_panel[total_jobs > 20 & !is.na(resident_eit)], cluster = ~work_city)
  m_all <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_panel[total_jobs > 50 & !is.na(resident_eit)], cluster = ~work_city)
  m_high <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_panel_high[total_jobs > 20 & !is.na(resident_eit)], cluster = ~work_city)

  etable(m_low, m_all, m_high,
    headers = c("Low (<$15K)", "All Workers", "High (>$40K)"))

  # Dose-response by earnings (treated only)
  pa_treated_seg <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips) &
    !is.na(rate_change) & rate_change < 5,
    .(first_inc_yr = min(year)), by = cousub_fips]

  dose_seg <- function(panel, min_jobs = 20) {
    tp <- merge(panel[total_jobs > min_jobs & !is.na(resident_eit)],
      pa_treated_seg, by.x = "work_city", by.y = "cousub_fips")
    tp[, post := as.integer(year >= first_inc_yr)]
    feols(share_live_in ~ resident_eit:post | work_city + year, data = tp, cluster = ~work_city)
  }

  d_low <- dose_seg(pa_low_panel)
  d_all <- dose_seg(pa_panel, 50)
  d_high <- dose_seg(pa_panel_high)

  etable(d_low, d_all, d_high,
    headers = c("Low: Dose", "All: Dose", "High: Dose"))
}

## 13b. PA age heterogeneity ----
pa_young_file <- file.path(data_dir, "lodes", "pa_od_SA01_main.rds")
if (file.exists(pa_young_file)) {
  pa_young_panel <- build_seg_od(pa_young_file)
  pa_prime_panel <- build_seg_od(file.path(data_dir, "lodes", "pa_od_SA02_main.rds"))
  pa_old_panel <- build_seg_od(file.path(data_dir, "lodes", "pa_od_SA03_main.rds"))

  # TWFE by age
  m_young <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_young_panel[total_jobs > 20 & !is.na(resident_eit)], cluster = ~work_city)
  m_prime <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_prime_panel[total_jobs > 20 & !is.na(resident_eit)], cluster = ~work_city)
  m_old_age <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_old_panel[total_jobs > 20 & !is.na(resident_eit)], cluster = ~work_city)

  etable(m_young, m_prime, m_old_age, m_all,
    headers = c("Young (<=29)", "Prime (30-54)", "Older (55+)", "All Workers"))

  # Dose-response by age
  d_young <- dose_seg(pa_young_panel)
  d_prime <- dose_seg(pa_prime_panel)
  d_old_age <- dose_seg(pa_old_panel)

  etable(d_young, d_prime, d_old_age, d_all,
    headers = c("Young: Dose", "Prime: Dose", "Older: Dose", "All: Dose"))
}


# 14. Federal Worker Placebo ----

pa_fed_file <- file.path(data_dir, "lodes", "pa_od_S000_JT04_main.rds")
if (file.exists(pa_fed_file)) {
  pa_fed_panel <- build_seg_od(pa_fed_file)

  # Note: federal workers dont pay municipal EIT
  # If tax drives sorting, federal share_live_in should NOT respond
  # But sample is small (~1600 obs) with noisy pre-trends

  m_fed <- feols(share_live_in ~ resident_eit | work_city + year,
    data = pa_fed_panel[total_jobs > 10 & !is.na(resident_eit)], cluster = ~work_city)

  etable(m_all, m_fed,
    headers = c("All Workers", "Federal Workers (placebo)"))

  # Sun-Abraham comparison
  pa_fed_panel[, cohort := fifelse(is.na(
    pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
      .(fiy = min(year)), by = cousub_fips][
      match(pa_fed_panel$work_city, cousub_fips), fiy]),
    10000L, as.integer(
    pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
      .(fiy = min(year)), by = cousub_fips][
      match(pa_fed_panel$work_city, cousub_fips), fiy]))]

  sa_fed <- feols(share_live_in ~ sunab(cohort, year) | work_city + year,
    data = pa_fed_panel[total_jobs > 10 & !is.na(resident_eit)], cluster = ~work_city)

  # Short-run comparison
  extract_sa <- function(m, label) {
    ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
    setnames(ct, c("rn", "est", "se", "t", "p"))
    ct[, rel := as.integer(gsub("year::", "", rn))]
    ct[rel >= -3 & rel <= 5, .(label, rel, est = round(est, 5), se = round(se, 5))]
  }

  print(dcast(rbind(
    extract_sa(pa_m_sunab, "All Workers"),
    extract_sa(sa_fed, "Federal")),
    rel ~ label, value.var = "est"))
}


# 15. Sample Sensitivity ----

cat("\n=== SENSITIVITY: PA TWFE BY MINIMUM TOTAL JOBS ===\n")
for (min_j in c(10, 50, 100, 200, 500)) {
  s <- pa_panel[total_jobs > min_j & !is.na(resident_eit)]
  m <- feols(share_live_in ~ resident_eit | work_city + year, data = s, cluster = ~work_city)
  cat(sprintf("  Min jobs > %4d: coef = %8.5f (SE %7.5f) N = %5d %s\n",
    min_j, coef(m), se(m), nobs(m),
    fifelse(abs(coef(m)) / se(m) > 1.96, "**", fifelse(abs(coef(m)) / se(m) > 1.65, "*", ""))))
}

beepr::beep()
