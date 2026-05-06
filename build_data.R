# Build Data: Local Income Tax Migration
# Run once to create all intermediate RDS files
# Prerequisites:
#   1. python3 scrape_pa_dced_eit.py (creates data/pa_eit_rate_panel.csv)
#   2. Rscript download_lodes.R (creates data/lodes_block/*.rds for PA)
# ============================================================================
#
# PA LODES is downloaded at block level (agg_geo = "block") and aggregated to
# cousub here via a block→cousub crosswalk built from TIGER 2020 blocks. This
# recovers all 2,561 PA cousubs; the prior tract-level pipeline (with centroid
# join) silently dropped 42% of cousubs (those that didn't host a tract
# centroid) and mis-attributed jobs for the rest. OH legacy path still uses
# tract-level data; OH was dropped from the paper.

library(sf)
library(dplyr)
library(data.table)
library(tigris)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)

options(tigris_use_cache = TRUE)

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")
lodes_dir       <- file.path(data_dir, "lodes")        # legacy tract-level (OH)
lodes_block_dir <- file.path(data_dir, "lodes_block")  # block-level (PA)


# 1. Ohio Rate Panel ----
# Source: OH Dept of Taxation "The Finder"
# muni_fips = Census PLACEFP (5-digit)

oh_rate_file <- file.path(data_dir, "oh_muni_rate_table.csv")
if (!file.exists(oh_rate_file)) {
  download.file(
    "https://thefinder.tax.ohio.gov/streamlinesalestaxweb/download/BoundaryData/Muni/OHMuniRateTable.csv",
    oh_rate_file, method = "curl", extra = "-k"
  )
}

oh_rates_raw <- fread(oh_rate_file, header = FALSE,
  col.names = c("start_date", "end_date", "muni_fips", "muni_name", "rate"))
oh_rates_raw[, muni_fips := sprintf("%05d", as.integer(muni_fips))]
oh_rates_raw[, `:=`(
  start_date = as.Date(as.character(start_date), format = "%Y%m%d"),
  end_date = as.Date(as.character(end_date), format = "%Y%m%d")
)]

# Expand to municipality x year; rate applies if active on April 1 (LODES ref date)
expand_oh_rates <- function(dt) {
  rows <- list()
  for (i in seq_len(nrow(dt))) {
    start_yr <- as.integer(format(dt$start_date[i], "%Y"))
    end_yr <- min(as.integer(format(dt$end_date[i], "%Y")), 2023L)
    if (end_yr < start_yr) next
    years <- start_yr:end_yr
    april1 <- as.Date(paste0(years, "-04-01"))
    active <- april1 >= dt$start_date[i] & april1 <= dt$end_date[i]
    if (sum(active) == 0) next
    rows[[i]] <- data.table(
      muni_fips = dt$muni_fips[i], muni_name = dt$muni_name[i],
      year = years[active], rate = dt$rate[i]
    )
  }
  rbindlist(rows)
}

oh_rate_panel <- expand_oh_rates(oh_rates_raw)
oh_rate_panel <- oh_rate_panel[order(muni_fips, year)]
oh_rate_panel[, rate_lag := shift(rate, 1), by = muni_fips]
oh_rate_panel[, rate_change := rate - rate_lag]
oh_rate_panel[, event_type := fcase(
  is.na(rate_change), NA_character_,
  rate_change > 0, "increase",
  rate_change < 0, "decrease",
  default = "no_change"
)]
first_inc <- oh_rate_panel[event_type == "increase",
  .(first_increase_year = min(year)), by = muni_fips]
oh_rate_panel <- merge(oh_rate_panel, first_inc, by = "muni_fips", all.x = TRUE)

saveRDS(oh_rate_panel, file.path(data_dir, "oh_rate_panel.rds"))
print(paste("OH rate panel:", nrow(oh_rate_panel), "rows,",
  oh_rate_panel[event_type == "increase", .N], "increases"))


# 2. Pennsylvania Rate Panel ----
# Source: DCED MunStats via scrape_pa_dced_eit.py

pa_eit_file <- file.path(data_dir, "pa_eit_rate_panel.csv")
if (!file.exists(pa_eit_file)) stop("Run: python3 scrape_pa_dced_eit.py first")

pa_raw <- fread(pa_eit_file)

# Per Act 32: a worker's bill = max(total resident rate at home,
# nonresident muni rate at work). Home side stacks muni_res + SD; work side is
# muni nonresident only (SDs do not levy a nonresident rate). We need both
# muni_res and muni_nonres in the panel so downstream models can implement
# the piecewise correctly.

pa_resident <- pa_raw[is_resident == TRUE,
  .(municipality_id, municipality, county, year, muni_eit_rate, sd_eit_rate, total_eit_rate)]
pa_nonres <- pa_raw[is_nonresident == TRUE,
  .(municipality_id, year, muni_nonres_rate = muni_eit_rate)]

# EIT rates are percentages (0-4% range); values > 5 are LST dollar amounts
# that leaked into the rate field — filter them out before aggregating
pa_resident <- pa_resident[is.na(muni_eit_rate) | muni_eit_rate <= 5]
pa_resident <- pa_resident[is.na(sd_eit_rate) | sd_eit_rate <= 5]
pa_resident <- pa_resident[is.na(total_eit_rate) | total_eit_rate <= 10]
pa_nonres <- pa_nonres[is.na(muni_nonres_rate) | muni_nonres_rate <= 5]

# Note: Philadelphia has a separate wage tax (4%+), not standard Act 511 EIT.
# It reports inconsistently, creating spurious rate changes.
# Keep in main analysis; exclude in robustness checks.

pa_rate_panel <- pa_resident[, .(
  resident_eit = max(muni_eit_rate, na.rm = TRUE),
  sd_eit = max(sd_eit_rate, na.rm = TRUE),
  total_eit = max(total_eit_rate, na.rm = TRUE)
), by = .(municipality_id, municipality, county, year)]
pa_nonres_panel <- pa_nonres[, .(
  muni_nonres_eit = max(muni_nonres_rate, na.rm = TRUE)
), by = .(municipality_id, year)]
pa_rate_panel <- merge(pa_rate_panel, pa_nonres_panel,
  by = c("municipality_id", "year"), all.x = TRUE)

pa_rate_panel[is.infinite(resident_eit), resident_eit := NA]
pa_rate_panel[is.infinite(sd_eit), sd_eit := NA]
pa_rate_panel[is.infinite(total_eit), total_eit := NA]
pa_rate_panel[is.infinite(muni_nonres_eit), muni_nonres_eit := NA]

# DCED central reporting of nonresident rates is incomplete pre-Act-32
# (only 53-60% of munis report 2002-2011, jumping to 72% in 2012). Diagnostics
# show this is a reporting gap, not an institutional gap: of 422 munis missing
# in 2010 but reporting 2015, only 0.2% have nonres=0 (so missing != "no
# nonres tax"). Nonresident rates are also extremely persistent: 96.3% of
# munis with post-2012 data have a single stable rate across 2012-2019.
# We backfill pre-2012 missing values with the earliest observed post-2012
# nonresident rate for each muni, and flag the imputation.
pa_rate_panel <- pa_rate_panel[order(municipality_id, year)]
pa_rate_panel[, nonres_imputed := is.na(muni_nonres_eit)]
post_nr <- pa_rate_panel[year >= 2012 & !is.na(muni_nonres_eit),
  .(muni_nonres_backfill = muni_nonres_eit[which.min(year)]), by = municipality_id]
pa_rate_panel <- merge(pa_rate_panel, post_nr, by = "municipality_id", all.x = TRUE)
pa_rate_panel[is.na(muni_nonres_eit) & !is.na(muni_nonres_backfill),
  muni_nonres_eit := muni_nonres_backfill]
pa_rate_panel[, muni_nonres_backfill := NULL]
# Backward-compat: total_eit should equal muni_res + sd (home-side comparison object)
# Recompute defensively in case the DCED total field disagrees with the sum.
pa_rate_panel[, total_eit := ifelse(is.na(resident_eit) | is.na(sd_eit),
  total_eit, resident_eit + sd_eit)]
# Wedge: muni nonres - muni res (positive when work-side rate exceeds home-side muni)
pa_rate_panel[, rate_wedge := muni_nonres_eit - resident_eit]

pa_rate_panel <- pa_rate_panel[order(municipality_id, year)]
# Existing change/event flags continue to track resident_eit (backward compat).
pa_rate_panel[, rate_lag := shift(resident_eit, 1), by = municipality_id]
pa_rate_panel[, rate_change := resident_eit - rate_lag]
pa_rate_panel[, total_eit_lag := shift(total_eit, 1), by = municipality_id]
pa_rate_panel[, total_rate_change := total_eit - total_eit_lag]
pa_rate_panel[, sd_eit_lag := shift(sd_eit, 1), by = municipality_id]
pa_rate_panel[, sd_rate_change := sd_eit - sd_eit_lag]
# New: parallel change flags for nonres rate (so analyses can detect nonres-only
# events that resident-rate-based event_type misses for ~24% of munis with a wedge).
pa_rate_panel[, nonres_lag := shift(muni_nonres_eit, 1), by = municipality_id]
pa_rate_panel[, nonres_change := muni_nonres_eit - nonres_lag]
pa_rate_panel[, event_type := fcase(
  is.na(rate_change), NA_character_,
  abs(rate_change) < 1e-6, "no_change",
  rate_change > 0, "increase",
  rate_change < 0, "decrease"
)]
pa_first_inc <- pa_rate_panel[event_type == "increase",
  .(first_increase_year = min(year)), by = municipality_id]
pa_rate_panel <- merge(pa_rate_panel, pa_first_inc, by = "municipality_id", all.x = TRUE)

## Crosswalk DCED -> Census COUSUBFP via name + county
pa_cousub <- county_subdivisions(state = "PA", year = 2020) %>% st_transform(4326)

pa_rate_panel[, dced_name := tolower(municipality)]
pa_rate_panel[, dced_name := gsub(" boro$", " borough", dced_name)]
pa_rate_panel[, dced_name := gsub(" twp$", " township", dced_name)]

# Additional normalization: collapse whitespace, fix abbreviations
pa_rate_panel[, dced_name := trimws(gsub("\\s+", " ", dced_name))]
pa_rate_panel[, dced_name := gsub("^mt ", "mount ", dced_name)]
pa_rate_panel[, dced_name := gsub(" mt ", " mount ", dced_name)]
pa_rate_panel[, dced_name := gsub("^st ", "st. ", dced_name)]
pa_rate_panel[, dced_name := gsub(" st ", " st. ", dced_name)]

# Compound words and punctuation that differ between DCED and TIGER
pa_rate_panel[, dced_name := gsub("^ohara ", "o'hara ", dced_name)]
pa_rate_panel[, dced_name := gsub("^wilkes barre ", "wilkes-barre ", dced_name)]
pa_rate_panel[, dced_name := gsub("^nanty glo ", "nanty-glo ", dced_name)]
pa_rate_panel[, dced_name := gsub("^oilcreek ", "oil creek ", dced_name)]
pa_rate_panel[, dced_name := gsub("^le boeuf ", "leboeuf ", dced_name)]
pa_rate_panel[, dced_name := gsub("^laplume ", "la plume ", dced_name)]

# Three PA municipalities use "municipality" instead of "borough" in TIGER
pa_rate_panel[municipality_id == 20303, dced_name := "bethel park municipality"]
pa_rate_panel[municipality_id == 22103, dced_name := "monroeville municipality"]
pa_rate_panel[municipality_id == 651043, dced_name := "murrysville municipality"]

# DCED "OIL CITY" lacks a classification suffix; TIGER is "Oil City city"
pa_rate_panel[municipality_id == 610512, dced_name := "oil city city"]

# 9 DCED municipalities no longer exist in 2020 TIGER boundaries (merged/dissolved):
# Strausstown Boro (BERKS), West Lawn Boro (BERKS), Wyomissing Hills Boro (BERKS),
# South Philipsburg Boro (CENTRE), Lumber City Boro (CLEARFIELD),
# East Fork Twp (POTTER), Upper Mahantango Twp (SCHUYLKILL),
# Uniondale Boro (SUSQUEHANNA), West Alexander Boro (WASHINGTON).
# These cannot be matched to 2020 Census geography.

tiger_xwalk <- as.data.table(pa_cousub)[, .(
  cousub_fips = COUSUBFP, tiger_name = tolower(NAMELSAD), county_fips = COUNTYFP)]
pa_counties <- as.data.table(counties(state = "PA", year = 2020))[, .(
  county_fips = COUNTYFP, county_name = toupper(NAME))]
tiger_xwalk <- merge(tiger_xwalk, pa_counties, by = "county_fips")

dced_unique <- unique(pa_rate_panel[, .(municipality_id, dced_name, county)])
dced_unique[, county_upper := toupper(county)]
xwalk <- merge(dced_unique, tiger_xwalk,
  by.x = c("dced_name", "county_upper"),
  by.y = c("tiger_name", "county_name"), all.x = TRUE)

pa_rate_panel <- merge(pa_rate_panel,
  unique(xwalk[!is.na(cousub_fips), .(municipality_id, cousub_fips)]),
  by = "municipality_id", all.x = TRUE)

saveRDS(pa_rate_panel, file.path(data_dir, "pa_rate_panel.rds"))
n_matched <- sum(!is.na(unique(pa_rate_panel[, .(municipality_id, cousub_fips)])$cousub_fips))
n_total <- uniqueN(pa_rate_panel$municipality_id)
print(paste("PA rate panel:", nrow(pa_rate_panel), "rows,",
  n_matched, "of", n_total, "matched (",
  round(100 * n_matched / n_total, 1), "%)"))


# 3. Tax Foundation Cross-Section ----

tf_file <- file.path(data_dir, "tax_foundation_local_income_tax_rates.xlsx")
if (!file.exists(tf_file)) {
  download.file(
    "https://taxfoundation.org/wp-content/uploads/2023/02/Local_Income_Tax_Rates_2021_JW-1.xlsx",
    tf_file, mode = "wb")
}
tf_rates <- read_excel(tf_file) %>%
  fill(State, .direction = "down") %>%
  clean_names() %>%
  mutate(
    resident_rate = as.numeric(resident_rate),
    nonresident_rate = as.numeric(nonresident_rate),
    rate_wedge = resident_rate - nonresident_rate)
saveRDS(tf_rates, file.path(data_dir, "tf_rates.rds"))


# 4. TIGER Boundaries + Tract Crosswalks ----

oh_places <- places(state = "OH", year = 2020) %>% st_transform(4326)
oh_tracts <- tracts(state = "OH", year = 2020) %>% st_transform(4326)
pa_tracts <- tracts(state = "PA", year = 2020) %>% st_transform(4326)

## OH: tract -> place
oh_tract_xwalk <- st_join(
  oh_tracts %>% mutate(centroid = st_centroid(geometry)) %>% st_set_geometry("centroid"),
  oh_places %>% select(place_fips = PLACEFP, place_name = NAME),
  left = TRUE) %>%
  st_drop_geometry() %>%
  select(tract_geoid = GEOID, place_fips, place_name)
saveRDS(oh_tract_xwalk, file.path(data_dir, "oh_tract_xwalk.rds"))

## PA: tract -> cousub
pa_tract_xwalk <- st_join(
  pa_tracts %>% mutate(centroid = st_centroid(geometry)) %>% st_set_geometry("centroid"),
  pa_cousub %>% select(cousub_fips = COUSUBFP, cousub_name = NAME),
  left = TRUE) %>%
  st_drop_geometry() %>%
  select(tract_geoid = GEOID, cousub_fips, cousub_name)
saveRDS(pa_tract_xwalk, file.path(data_dir, "pa_tract_xwalk.rds"))

## PA: block -> cousub (used by block-level LODES aggregation)
# Blocks nest within cousubs in 2020 Census hierarchy, so a centroid
# point-in-polygon is exact. Cached because the spatial join is the slowest
# step in the build (~30s after blocks are downloaded).
pa_block_xwalk_file <- file.path(data_dir, "pa_block_xwalk.rds")
if (!file.exists(pa_block_xwalk_file)) {
  pa_blocks <- tigris::blocks(state = "PA", year = 2020) %>%
    st_transform(4326) %>%
    select(block_geoid = GEOID20)
  pa_block_centroids <- pa_blocks %>%
    mutate(geometry = st_centroid(geometry))
  pa_block_xwalk <- st_join(
    pa_block_centroids,
    pa_cousub %>% select(cousub_fips = COUSUBFP) %>% st_transform(4326),
    join = st_within
  ) %>%
    st_drop_geometry() %>%
    as.data.table()
  saveRDS(pa_block_xwalk, pa_block_xwalk_file)
} else {
  pa_block_xwalk <- readRDS(pa_block_xwalk_file)
}

# Save place/cousub geometries for maps
saveRDS(oh_places, file.path(data_dir, "oh_places.rds"))
saveRDS(pa_cousub, file.path(data_dir, "pa_cousub.rds"))


# 5. City-to-City Neighbor Pairs ----

## OH: 20km radius
oh_places_proj <- st_transform(oh_places, 32617)
oh_dist_mat <- st_distance(st_centroid(oh_places_proj))
oh_fips_vec <- oh_places$PLACEFP

oh_nbr_list <- list()
for (i in seq_len(nrow(oh_dist_mat))) {
  dists <- as.numeric(oh_dist_mat[i, ])
  nbrs <- which(dists > 0 & dists <= 80000)
  if (length(nbrs) > 0) {
    oh_nbr_list[[i]] <- data.table(
      city_fips = oh_fips_vec[i], neighbor_fips = oh_fips_vec[nbrs],
      dist_km = dists[nbrs] / 1000)
  }
}
oh_neighbors <- rbindlist(oh_nbr_list)
saveRDS(oh_neighbors, file.path(data_dir, "oh_neighbors.rds"))
rm(oh_dist_mat)

## PA: 15km radius
pa_cousub_proj <- st_transform(pa_cousub, 32618)
pa_dist_mat <- st_distance(st_centroid(pa_cousub_proj))
pa_fips_vec <- pa_cousub$COUSUBFP

pa_nbr_list <- list()
for (i in seq_len(nrow(pa_dist_mat))) {
  dists <- as.numeric(pa_dist_mat[i, ])
  nbrs <- which(dists > 0 & dists <= 80000)
  if (length(nbrs) > 0) {
    pa_nbr_list[[i]] <- data.table(
      city_fips = pa_fips_vec[i], neighbor_fips = pa_fips_vec[nbrs],
      dist_km = dists[nbrs] / 1000)
  }
}
pa_neighbors <- rbindlist(pa_nbr_list)
saveRDS(pa_neighbors, file.path(data_dir, "pa_neighbors.rds"))
rm(pa_dist_mat)

print(paste("OH neighbors:", nrow(oh_neighbors), "pairs"))
print(paste("PA neighbors:", nrow(pa_neighbors), "pairs"))


# 6. City-Level OD Panels ----

build_city_od <- function(main_rds, aux_rds = NULL, tract_xwalk, id_col, state_fips) {
  # Load main (intra-state) OD
  od_dt <- as.data.table(readRDS(main_rds))
  od_dt[, year := as.integer(year)]

  # Load aux (cross-state) OD if available
  # Aux has workers who work in this state but live in another state
  if (!is.null(aux_rds) && file.exists(aux_rds)) {
    aux_dt <- as.data.table(readRDS(aux_rds))
    aux_dt[, year := as.integer(year)]
    # Keep only flows where workplace is in our state
    aux_dt <- aux_dt[substr(w_tract, 1, 2) == state_fips]
    od_dt <- rbind(od_dt, aux_dt, fill = TRUE)
    print(paste("  Added", nrow(aux_dt), "cross-state flows"))
  }

  # Assign tracts to cities
  home_xw <- copy(tract_xwalk[, c("tract_geoid", id_col), with = FALSE])
  work_xw <- copy(tract_xwalk[, c("tract_geoid", id_col), with = FALSE])
  setnames(home_xw, c("tract_geoid", id_col), c("h_tract", "home_city"))
  setnames(work_xw, c("tract_geoid", id_col), c("w_tract", "work_city"))

  od_dt <- merge(od_dt, home_xw, by = "h_tract", all.x = TRUE)
  od_dt <- merge(od_dt, work_xw, by = "w_tract", all.x = TRUE)

  # Flag out-of-state residents (home tract not in this state)
  od_dt[, home_out_of_state := substr(h_tract, 1, 2) != state_fips]

  city_od <- od_dt[!is.na(work_city), .(
    total_jobs = sum(S000, na.rm = TRUE),
    live_and_work = sum(S000[home_city == work_city], na.rm = TRUE),
    live_outside = sum(S000[is.na(home_city) | home_city != work_city], na.rm = TRUE),
    live_out_of_state = sum(S000[home_out_of_state == TRUE], na.rm = TRUE),
    live_in_state_diff_city = sum(S000[home_out_of_state == FALSE &
      (is.na(home_city) | home_city != work_city)], na.rm = TRUE)
  ), by = .(work_city, year)]

  city_od[, `:=`(
    share_live_in = live_and_work / total_jobs,
    share_out_of_state = live_out_of_state / total_jobs,
    share_in_state_diff = live_in_state_diff_city / total_jobs,
    log_total_jobs = log(total_jobs),
    log_live_in = ifelse(live_and_work > 0, log(live_and_work), NA_real_),
    log_live_out = ifelse(live_outside > 0, log(live_outside), NA_real_),
    log_out_of_state = ifelse(live_out_of_state > 0, log(live_out_of_state), NA_real_)
  )]
  city_od
}

oh_tract_dt <- as.data.table(oh_tract_xwalk)
pa_tract_dt <- as.data.table(pa_tract_xwalk)

# Block-level OD aggregator for PA. Mirrors build_city_od but consumes
# block-level LODES (h_geocode/w_geocode 15-digit GEOIDs) and merges via
# the block→cousub crosswalk. Aggregates to (work_city, year) as before.
build_city_od_block <- function(main_rds, aux_rds = NULL, block_xwalk,
                                state_fips) {
  od_dt <- as.data.table(readRDS(main_rds))
  od_dt[, year := as.integer(year)]
  setnames(od_dt, c("h_geocode", "w_geocode"), c("h_block", "w_block"),
           skip_absent = TRUE)

  if (!is.null(aux_rds) && file.exists(aux_rds)) {
    aux_dt <- as.data.table(readRDS(aux_rds))
    aux_dt[, year := as.integer(year)]
    setnames(aux_dt, c("h_geocode", "w_geocode"), c("h_block", "w_block"),
             skip_absent = TRUE)
    aux_dt <- aux_dt[substr(w_block, 1, 2) == state_fips]
    od_dt <- rbind(od_dt, aux_dt, fill = TRUE)
    print(paste("  Added", nrow(aux_dt), "cross-state flows"))
  }

  home_xw <- block_xwalk[, .(h_block = block_geoid, home_city = cousub_fips)]
  work_xw <- block_xwalk[, .(w_block = block_geoid, work_city = cousub_fips)]
  od_dt <- merge(od_dt, home_xw, by = "h_block", all.x = TRUE)
  od_dt <- merge(od_dt, work_xw, by = "w_block", all.x = TRUE)
  od_dt[, home_out_of_state := substr(h_block, 1, 2) != state_fips]

  city_od <- od_dt[!is.na(work_city), .(
    total_jobs = sum(S000, na.rm = TRUE),
    live_and_work = sum(S000[home_city == work_city], na.rm = TRUE),
    live_outside = sum(S000[is.na(home_city) | home_city != work_city], na.rm = TRUE),
    live_out_of_state = sum(S000[home_out_of_state == TRUE], na.rm = TRUE),
    live_in_state_diff_city = sum(S000[home_out_of_state == FALSE &
      (is.na(home_city) | home_city != work_city)], na.rm = TRUE)
  ), by = .(work_city, year)]

  city_od[, `:=`(
    share_live_in = live_and_work / total_jobs,
    share_out_of_state = live_out_of_state / total_jobs,
    share_in_state_diff = live_in_state_diff_city / total_jobs,
    log_total_jobs = log(total_jobs),
    log_live_in = ifelse(live_and_work > 0, log(live_and_work), NA_real_),
    log_live_out = ifelse(live_outside > 0, log(live_outside), NA_real_),
    log_out_of_state = ifelse(live_out_of_state > 0, log(live_out_of_state), NA_real_)
  )]
  city_od
}

oh_city_od <- build_city_od(
  file.path(lodes_dir, "oh_od_S000_main.rds"),
  file.path(lodes_dir, "oh_od_S000_aux.rds"),
  oh_tract_dt, "place_fips", "39")

oh_city_od_high <- build_city_od(
  file.path(lodes_dir, "oh_od_SE03_main.rds"),
  NULL, oh_tract_dt, "place_fips", "39")

pa_city_od <- build_city_od_block(
  file.path(lodes_block_dir, "pa_od_S000_main.rds"),
  file.path(lodes_block_dir, "pa_od_S000_aux.rds"),
  pa_block_xwalk, "42")

pa_city_od_high <- build_city_od_block(
  file.path(lodes_block_dir, "pa_od_SE03_main.rds"),
  NULL, pa_block_xwalk, "42")

saveRDS(oh_city_od, file.path(data_dir, "oh_city_od.rds"))
saveRDS(oh_city_od_high, file.path(data_dir, "oh_city_od_high.rds"))
saveRDS(pa_city_od, file.path(data_dir, "pa_city_od.rds"))
saveRDS(pa_city_od_high, file.path(data_dir, "pa_city_od_high.rds"))

print(paste("OH city OD:", nrow(oh_city_od), "rows"))
print(paste("PA city OD:", nrow(pa_city_od), "rows"))


# 7. Analysis-Ready Panels (OD + Rates) ----

## OH
oh_rate_merge <- oh_rate_panel[, .(muni_fips, year, rate, rate_change,
  event_type, first_increase_year)]
oh_panel <- merge(oh_city_od, oh_rate_merge,
  by.x = c("work_city", "year"), by.y = c("muni_fips", "year"), all.x = TRUE)
oh_panel[is.na(rate), rate := 0]

oh_panel_high <- merge(oh_city_od_high, oh_rate_merge,
  by.x = c("work_city", "year"), by.y = c("muni_fips", "year"), all.x = TRUE)
oh_panel_high[is.na(rate), rate := 0]

saveRDS(oh_panel, file.path(data_dir, "oh_panel.rds"))
saveRDS(oh_panel_high, file.path(data_dir, "oh_panel_high.rds"))

## PA
pa_rate_merge <- pa_rate_panel[!is.na(cousub_fips),
  .(cousub_fips, year, resident_eit, sd_eit, total_eit, muni_nonres_eit, rate_wedge,
    nonres_imputed, rate_change, nonres_change, event_type, first_increase_year)] %>%
  unique()
pa_panel <- merge(pa_city_od, pa_rate_merge,
  by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

pa_panel_high <- merge(pa_city_od_high, pa_rate_merge,
  by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

saveRDS(pa_panel, file.path(data_dir, "pa_panel.rds"))
saveRDS(pa_panel_high, file.path(data_dir, "pa_panel_high.rds"))

print(paste("OH panel:", nrow(oh_panel), "rows"))
print(paste("PA panel:", nrow(pa_panel), "rows, with rate:", sum(!is.na(pa_panel$resident_eit))))

print("All data built.")
beepr::beep()
