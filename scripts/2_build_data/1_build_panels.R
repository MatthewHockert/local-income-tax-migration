#!/usr/bin/env Rscript
# 1_build_panels.R
# Builds the panels the models use, from the downloaded files in data/raw/,
# and saves them to data/derived/. Needs internet only if tigris has not
# already cached the PA county and block maps.

suppressMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(data.table)
  library(readxl)
  library(janitor)
  library(tigris)
})

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

# Flags ----
RUN_DCED_REPARSE    <- TRUE  # DCED XLS -> pa_eit_rate_panel.csv, keeping SD id/name
RUN_SD_CONTROLS     <- TRUE  # Census F-33 -> SD share controls + NCES<->DCED xwalk
RUN_COUSUB_PANELS   <- TRUE  # cousub rate / OD / fiscal panels + block->cousub xwalk
RUN_RESIDENCE_PANEL <- TRUE  # cousub x year residence-side counts (home-side LODES)
RUN_BLOCK_PANEL     <- TRUE  # block x year panel inheriting rates/shares from cousub

# DCED reparse ----
# Rebuilds data/raw/pa_eit_rate_panel.csv from the yearly DCED spreadsheets,
# this time keeping each town's school district.
if (RUN_DCED_REPARSE) local({
  xls_files <- list.files(file.path(RAW_DIR, "pa_dced_raw"), pattern = "^pa_taxes_[0-9]{4}\\.xls$",
                          full.names = TRUE)
  print(sprintf("XLS files: %d", length(xls_files)))
  read_register_year <- function(file) {
    read_excel(file, .name_repair = "minimal") %>%
      rename(any_of(c(year = "Reporting Year", county = "County", municipality_id = "Municipality ID",
                      municipality = "Municipality", sd_id = "School District ID", sd_name = "School District",
                      tax_name = "Tax Name", muni_eit_rate = "Municipal Tax Amount",
                      sd_eit_rate = "School District Tax Amount", total_eit_rate = "Total Tax Rate"))) %>%
      transmute(year = as.integer(year), county, municipality, municipality_id = as.integer(municipality_id),
                sd_id = as.integer(sd_id), sd_name,
                muni_eit_rate = as.numeric(muni_eit_rate), sd_eit_rate = as.numeric(sd_eit_rate),
                total_eit_rate = as.numeric(total_eit_rate), tax_name,
                # the tax name says whether a line is the resident or nonresident rate
                is_resident    = grepl("resident", tolower(str_squish(tax_name))) &
                                 !grepl("non.?resident", tolower(str_squish(tax_name))),
                is_nonresident = grepl("non.?resident", tolower(str_squish(tax_name))))
  }
  register <- bind_rows(lapply(xls_files, read_register_year))
  fwrite(register, file.path(RAW_DIR, "pa_eit_rate_panel.csv"))
  print(sprintf("Rewrote panel: %d rows", nrow(register)))
  print(sprintf("Distinct sd_id: %d", n_distinct(register$sd_id)))
  multi_sd <- register %>%
    filter(is_resident, !is.na(sd_eit_rate)) %>%
    group_by(municipality_id, year) %>%
    summarise(n = n_distinct(sd_id), .groups = "drop") %>%
    filter(n > 1)
  print(sprintf("Multi-SD muni-years: %d", nrow(multi_sd)))
})

# SD fiscal controls ----
# School-district budget shares from the Census F-33, and a crosswalk that
# matches each Census district to its DCED id.
if (RUN_SD_CONTROLS) local({
  f33 <- fread(file.path(RAW_DIR, "pa_sd_finance_raw.csv"))
  # F-33 reports thousands in some years and dollars in others, which is fine for shares
  amount_cols <- intersect(c("TOTALREV", "TFEDREV", "TSTREV", "TLOCREV", "T06",
                             "TOTALEXP", "TCURINST", "TCURSSVC", "TCAPOUT", "_41F", "V33"), names(f33))
  for (col in amount_cols) set(f33, j = col, value = suppressWarnings(as.numeric(f33[[col]])))
  setnames(f33, "_41F", "DEBT_INT_EXP", skip_absent = TRUE)
  setnames(f33, "fy", "year")
  print(sprintf("Raw rows: %s", format(nrow(f33), big.mark = ",")))
  print("SCHLEV distribution (all levels kept):")
  print(f33[, .N, keyby = SCHLEV])

  sd_controls <- f33[, .(
    ncesid = NCESID, idcensus = IDCENSUS, sd_name_f33 = NAME, year = year, schlev = SCHLEV,
    enrollment = V33, total_rev = TOTALREV, total_exp = TOTALEXP,
    share_state_rev_sd     = ifelse(TOTALREV > 0, TSTREV  / TOTALREV, NA_real_),
    share_local_rev_sd     = ifelse(TOTALREV > 0, TLOCREV / TOTALREV, NA_real_),
    share_federal_rev_sd   = ifelse(TOTALREV > 0, TFEDREV / TOTALREV, NA_real_),
    share_property_tax_sd  = ifelse(TOTALREV > 0, T06     / TOTALREV, NA_real_),
    share_instructional_sd = ifelse(TOTALEXP > 0, TCURINST / TOTALEXP, NA_real_),
    share_support_sd       = ifelse(TOTALEXP > 0, TCURSSVC / TOTALEXP, NA_real_),
    share_capital_sd       = ifelse(TOTALEXP > 0, TCAPOUT  / TOTALEXP, NA_real_),
    share_debt_int_sd      = ifelse(TOTALEXP > 0, DEBT_INT_EXP / TOTALEXP, NA_real_))]
  for (col in grep("^share_", names(sd_controls), value = TRUE))
    set(sd_controls, which(!is.finite(sd_controls[[col]])), col, NA_real_)
  print(sprintf("SD controls panel: %s rows, %d unique SDs",
                format(nrow(sd_controls), big.mark = ","), uniqueN(sd_controls$ncesid)))

  register <- fread(file.path(RAW_DIR, "pa_eit_rate_panel.csv"))
  dced_sds <- unique(register[!is.na(sd_id), .(sd_id, sd_name)])
  print(sprintf("DCED unique SDs: %d", nrow(dced_sds)))
  # The two agencies spell district names differently ("MT LEBANON SD" vs
  # "Mount Lebanon School District"), so both get cleaned to the same bare words.
  normalize_sd <- function(names) {
    names <- tolower(names)
    names <- gsub("[^a-z0-9 ]", " ", names)   # punctuation first, e.g. "ALLEG.-CLARION"
    names <- gsub("\\s+", " ", names)
    for (pattern in c("\\basd\\b", "\\bschool district\\b", "\\bsch dist\\b", "\\bschool dist\\b",
                      "\\bs d\\b", "\\bsd\\b", "\\bsch\\b", "\\bschool\\b"))
      names <- gsub(pattern, "", names)
    abbreviations <- c(
      "stnycrk" = "stonycreek", "stnycreek" = "stonycreek", "stony creek" = "stonycreek",
      "south eastern" = "southeastern", "north eastern" = "northeastern",
      "north western" = "northwestern", "south western" = "southwestern",
      "clarionvalley" = "clarion valley", "brothersvalley" = "brothers valley",
      "strousbg" = "stroudsburg", "stroudsbg" = "stroudsburg", "kensngtn" = "kensington",
      "kenington" = "kensington", "pittsbgh" = "pittsburgh", "regn" = "regional", "reg" = "regional",
      "comm" = "community", "cmnty" = "community", "consolidatd" = "consolidated", "cons" = "consolidated",
      "alleg" = "allegheny", "alleghny" = "allegheny", "lancastr" = "lancaster", "swarthmr" = "swarthmore",
      "wallingfrd" = "wallingford", "swarthm" = "swarthmore", "northn" = "northern", "lwr" = "lower",
      "upr" = "upper", "turkeyft" = "turkeyfoot", "turkey foot" = "turkeyfoot", "vlly" = "valley",
      "val" = "valley", "hts" = "heights", "cty" = "city", "twnshp" = "township",
      # mountain/mount -> mt, so "tussey mt" and "mt lebanon" both match
      "mountain" = "mt", "mount" = "mt",
      # northern -> north is left out: it breaks too many other names
      "eastern" = "east", "western" = "west", "southern" = "south")
    for (short in names(abbreviations)) names <- gsub(paste0("\\b", short, "\\b"), abbreviations[[short]], names)
    names <- gsub("\\b(area|the|joint|borough|boro|city|township|twp|of|co|county|merged|union|intermediate|unit|comm)\\b",
                  "", names)
    names <- gsub("\\b[a-z]\\b", "", names)   # stray single letters ("albert gallatin a")
    trimws(gsub("\\s+", " ", names))
  }
  f33_names <- unique(sd_controls[, .(ncesid, sd_name_f33)])
  f33_names[, sd_name_norm := normalize_sd(sd_name_f33)]
  dced_sds[, sd_name_norm := normalize_sd(sd_name)]
  xwalk <- merge(f33_names, dced_sds, by = "sd_name_norm", allow.cartesian = TRUE)
  print(sprintf("Exact-match: NCESID -> DCED sd_id: %d / %d", uniqueN(xwalk$ncesid), uniqueN(f33_names$ncesid)))

  # Names that still don't match take the closest Census name, if it is nearly
  # identical (Jaro-Winkler distance under 0.15).
  unmatched_dced <- dced_sds[!sd_id %in% xwalk$sd_id]
  unmatched_f33  <- f33_names[!ncesid %in% xwalk$ncesid]
  if (nrow(unmatched_dced) > 0 && nrow(unmatched_f33) > 0) {
    print(sprintf("Fuzzy-matching %d unmatched DCED SDs against %d F-33 candidates...",
                  nrow(unmatched_dced), nrow(unmatched_f33)))
    fuzzy <- rbindlist(lapply(seq_len(nrow(unmatched_dced)), function(i) {
      dced_name <- unmatched_dced$sd_name_norm[i]
      if (is.na(dced_name) || dced_name == "") return(NULL)
      distances <- stringdist::stringdist(dced_name, unmatched_f33$sd_name_norm, method = "jw", p = 0.1)
      best <- which.min(distances)
      if (length(best) != 1 || distances[best] >= 0.15) return(NULL)
      data.table(sd_id = unmatched_dced$sd_id[i], ncesid = unmatched_f33$ncesid[best],
                 sd_name = unmatched_dced$sd_name[i], sd_name_f33 = unmatched_f33$sd_name_f33[best],
                 dist = distances[best])
    }))
    if (nrow(fuzzy) > 0) {
      print(sprintf("Fuzzy matched %d additional DCED SDs (jw < 0.15):", nrow(fuzzy)))
      print(fuzzy[order(dist)][1:min(20, .N)])
      xwalk <- rbind(xwalk, fuzzy[, .(sd_name_norm = NA_character_, ncesid, sd_name_f33, sd_id, sd_name)], fill = TRUE)
    }
  }
  # two districts were renamed or merged, so no name match can find them
  manual_pairs <- data.table(
    sd_id = c(107803L, 321103L), ncesid = c("4221930", "4203750"),
    sd_name = c("KNOCH SD", "RIVER VALLEY SD"),
    sd_name_f33 = c("SOUTH BUTLER CO SCH", "BLAIRSVILLE SALTSBURG SCHOOL DIST"),
    sd_name_norm = NA_character_)
  xwalk <- rbind(xwalk, manual_pairs, fill = TRUE)
  print(sprintf("Final matched DCED SDs: %d / %d (%.1f%%)", uniqueN(xwalk$sd_id), uniqueN(dced_sds$sd_id),
                100 * uniqueN(xwalk$sd_id) / uniqueN(dced_sds$sd_id)))
  print(sprintf("Unmatched F-33 SDs (%d)", length(setdiff(f33_names$ncesid, xwalk$ncesid))))
  print(sprintf("Unmatched DCED SDs (%d)", length(setdiff(dced_sds$sd_id, xwalk$sd_id))))

  sd_xwalk <- xwalk[, .(ncesid, sd_name_f33, sd_id, sd_name_dced = sd_name)]
  saveRDS(sd_xwalk, file.path(DERIVED_DIR, "pa_sd_id_xwalk.rds"))
  fwrite(sd_xwalk, file.path(DERIVED_DIR, "sd_id_xwalk.csv"))   # CSV copy for the Python property build
  # districts with no DCED match can't be linked to tax rates, so they drop out
  sd_controls <- merge(sd_controls, unique(xwalk[, .(ncesid, sd_id)]), by = "ncesid", all.x = TRUE)
  sd_controls <- sd_controls[!is.na(sd_id)]
  print(sprintf("SD controls (matched to DCED): %s rows, %d unique sd_id",
                format(nrow(sd_controls), big.mark = ","), uniqueN(sd_controls$sd_id)))
  saveRDS(sd_controls, file.path(DERIVED_DIR, "pa_sd_controls.rds"))
})

# Cousub panels ----
# The tax-rate panel, its match to Census towns (cousubs), and the workplace
# commuting counts that become the outcomes. data.table here because LODES has
# tens of millions of rows.
if (RUN_COUSUB_PANELS) local({
  options(tigris_use_cache = TRUE)
  lodes_dir <- file.path(RAW_DIR, "lodes_block")
  register_file <- file.path(RAW_DIR, "pa_eit_rate_panel.csv")
  if (!file.exists(register_file)) stop("Run: python3 scripts/1_get_data/scrape_pa_dced_eit.py first")
  register <- fread(register_file)

  # A worker pays the larger of the home rate (town + school district) and the
  # work town's nonresident rate, so both sides are kept. Districts have no nonresident rate.
  resident_lines <- register[is_resident == TRUE,
    .(municipality_id, municipality, county, year, muni_eit_rate, sd_eit_rate, total_eit_rate)]
  nonresident_lines <- register[is_nonresident == TRUE, .(municipality_id, year, muni_nonres_rate = muni_eit_rate)]
  # numbers above 5 (10 for the total) are Local Services Tax dollar amounts that
  # landed in the rate column, not rates
  resident_lines <- resident_lines[(is.na(muni_eit_rate) | muni_eit_rate <= 5) &
                                   (is.na(sd_eit_rate) | sd_eit_rate <= 5) &
                                   (is.na(total_eit_rate) | total_eit_rate <= 10)]
  nonresident_lines <- nonresident_lines[is.na(muni_nonres_rate) | muni_nonres_rate <= 5]
  # Philadelphia's wage tax is not a regular EIT and reports oddly, but it stays in

  # one rate per town-year; towns split across two school districts (~2%) keep the higher SD rate
  rate_panel <- resident_lines[, .(resident_eit = max(muni_eit_rate, na.rm = TRUE),
                                   sd_eit       = max(sd_eit_rate, na.rm = TRUE),
                                   total_eit    = max(total_eit_rate, na.rm = TRUE)),
                               by = .(municipality_id, municipality, county, year)]
  nonresident_panel <- nonresident_lines[, .(muni_nonres_eit = max(muni_nonres_rate, na.rm = TRUE)),
                                         by = .(municipality_id, year)]
  rate_panel <- merge(rate_panel, nonresident_panel, by = c("municipality_id", "year"), all.x = TRUE)
  for (rate_col in c("resident_eit", "sd_eit", "total_eit", "muni_nonres_eit"))
    rate_panel[is.infinite(get(rate_col)), (rate_col) := NA]

  # Before 2012 many towns never reported their nonresident rate, though they had one,
  # and these rates almost never change. So a pre-2012 gap takes the town's earliest
  # later rate, and nonres_imputed marks every filled-in value.
  rate_panel <- rate_panel[order(municipality_id, year)]
  rate_panel[, nonres_imputed := is.na(muni_nonres_eit)]
  backfill <- rate_panel[year >= 2012 & !is.na(muni_nonres_eit),
                         .(muni_nonres_backfill = muni_nonres_eit[which.min(year)]), by = municipality_id]
  rate_panel <- merge(rate_panel, backfill, by = "municipality_id", all.x = TRUE)
  rate_panel[is.na(muni_nonres_eit) & !is.na(muni_nonres_backfill), muni_nonres_eit := muni_nonres_backfill]
  rate_panel[, muni_nonres_backfill := NULL]
  # recompute the total as town + district, since DCED's own total sometimes disagrees
  rate_panel[, total_eit := ifelse(is.na(resident_eit) | is.na(sd_eit), total_eit, resident_eit + sd_eit)]
  rate_panel[, rate_wedge := muni_nonres_eit - resident_eit]

  rate_panel <- rate_panel[order(municipality_id, year)]
  rate_panel[, rate_lag := shift(resident_eit, 1), by = municipality_id]
  rate_panel[, rate_change := resident_eit - rate_lag]
  rate_panel[, total_eit_lag := shift(total_eit, 1), by = municipality_id]
  rate_panel[, total_rate_change := total_eit - total_eit_lag]
  rate_panel[, sd_eit_lag := shift(sd_eit, 1), by = municipality_id]
  rate_panel[, sd_rate_change := sd_eit - sd_eit_lag]
  rate_panel[, nonres_lag := shift(muni_nonres_eit, 1), by = municipality_id]
  rate_panel[, nonres_change := muni_nonres_eit - nonres_lag]
  rate_panel[, event_type := fcase(is.na(rate_change), NA_character_,
                                   abs(rate_change) < 1e-6, "no_change",
                                   rate_change > 0, "increase",
                                   rate_change < 0, "decrease")]
  first_increase <- rate_panel[event_type == "increase", .(first_increase_year = min(year)), by = municipality_id]
  rate_panel <- merge(rate_panel, first_increase, by = "municipality_id", all.x = TRUE)

  # Match each DCED town to its Census cousub by name within county.
  cousub_file <- file.path(DERIVED_DIR, "pa_cousub.rds")
  cousubs <- if (file.exists(cousub_file)) readRDS(cousub_file) else
    st_transform(county_subdivisions(state = "PA", year = 2020), 4326)
  rate_panel[, dced_name := tolower(municipality)]
  rate_panel[, dced_name := gsub(" boro$", " borough", dced_name)]
  rate_panel[, dced_name := gsub(" twp$", " township", dced_name)]
  rate_panel[, dced_name := trimws(gsub("\\s+", " ", dced_name))]
  rate_panel[, dced_name := gsub("^mt ", "mount ", dced_name)]
  rate_panel[, dced_name := gsub(" mt ", " mount ", dced_name)]
  rate_panel[, dced_name := gsub("^st ", "st. ", dced_name)]
  rate_panel[, dced_name := gsub(" st ", " st. ", dced_name)]
  # spellings that differ between DCED and TIGER
  rate_panel[, dced_name := gsub("^ohara ", "o'hara ", dced_name)]
  rate_panel[, dced_name := gsub("^wilkes barre ", "wilkes-barre ", dced_name)]
  rate_panel[, dced_name := gsub("^nanty glo ", "nanty-glo ", dced_name)]
  rate_panel[, dced_name := gsub("^oilcreek ", "oil creek ", dced_name)]
  rate_panel[, dced_name := gsub("^le boeuf ", "leboeuf ", dced_name)]
  rate_panel[, dced_name := gsub("^laplume ", "la plume ", dced_name)]
  # four towns matched by hand: Census calls three of them "municipality", and Oil City has no suffix
  rate_panel[municipality_id == 20303, dced_name := "bethel park municipality"]
  rate_panel[municipality_id == 22103, dced_name := "monroeville municipality"]
  rate_panel[municipality_id == 651043, dced_name := "murrysville municipality"]
  rate_panel[municipality_id == 610512, dced_name := "oil city city"]
  # Nine DCED towns stay unmatched on purpose: they no longer exist on the 2020 Census map
  # (Strausstown, West Lawn, Wyomissing Hills, South Philipsburg, Lumber City, East Fork,
  # Upper Mahantango, Uniondale, West Alexander).
  tiger_names <- as.data.table(cousubs)[, .(cousub_fips = COUSUBFP, tiger_name = tolower(NAMELSAD),
                                            county_fips = COUNTYFP)]
  tiger_names <- merge(tiger_names,
                       as.data.table(counties(state = "PA", year = 2020))[, .(county_fips = COUNTYFP, county_name = toupper(NAME))],
                       by = "county_fips")
  dced_names <- unique(rate_panel[, .(municipality_id, dced_name, county)])
  dced_names[, county_upper := toupper(county)]
  name_match <- merge(dced_names, tiger_names, by.x = c("dced_name", "county_upper"),
                      by.y = c("tiger_name", "county_name"), all.x = TRUE)
  rate_panel <- merge(rate_panel, unique(name_match[!is.na(cousub_fips), .(municipality_id, cousub_fips)]),
                      by = "municipality_id", all.x = TRUE)
  saveRDS(rate_panel, file.path(DERIVED_DIR, "pa_rate_panel.rds"))
  n_matched <- sum(!is.na(unique(rate_panel[, .(municipality_id, cousub_fips)])$cousub_fips))
  n_munis <- uniqueN(rate_panel$municipality_id)
  print(paste("PA rate panel:", nrow(rate_panel), "rows,", n_matched, "of", n_munis, "matched (",
              round(100 * n_matched / n_munis, 1), "%)"))

  # Each census block sits entirely inside one town, so the town under a block's
  # center is its town. The block crosswalk is cached because building it downloads every PA block.
  block_xwalk_file <- file.path(DERIVED_DIR, "pa_block_xwalk.rds")
  if (!file.exists(block_xwalk_file)) {
    block_centroids <- tigris::blocks(state = "PA", year = 2020) %>%
      st_transform(4326) %>%
      select(block_geoid = GEOID20) %>%
      mutate(geometry = st_centroid(geometry))
    block_xwalk <- st_join(block_centroids, cousubs %>% select(cousub_fips = COUSUBFP) %>% st_transform(4326),
                           join = st_within) %>%
      st_drop_geometry() %>%
      as.data.table()
    saveRDS(block_xwalk, block_xwalk_file)
  } else {
    block_xwalk <- readRDS(block_xwalk_file)
  }
  saveRDS(cousubs, file.path(DERIVED_DIR, "pa_cousub.rds"))

  # Each LODES row is the number of workers (S000) who live in one block and work
  # in another. Tag both ends with their town, then count per workplace town and
  # year how many workers live there too. This file only has PA residents.
  od <- as.data.table(readRDS(file.path(lodes_dir, "pa_od_S000_main.rds")))
  od[, year := as.integer(year)]
  setnames(od, c("h_geocode", "w_geocode"), c("h_block", "w_block"), skip_absent = TRUE)
  od <- merge(od, block_xwalk[, .(h_block = block_geoid, home_city = cousub_fips)], by = "h_block", all.x = TRUE)
  od <- merge(od, block_xwalk[, .(w_block = block_geoid, work_city = cousub_fips)], by = "w_block", all.x = TRUE)
  city_od <- od[!is.na(work_city), .(
    total_jobs              = sum(S000, na.rm = TRUE),
    live_and_work           = sum(S000[home_city == work_city], na.rm = TRUE),
    live_outside            = sum(S000[is.na(home_city) | home_city != work_city], na.rm = TRUE)),
    by = .(work_city, year)]
  city_od[, `:=`(
    share_live_in       = live_and_work / total_jobs,
    log_total_jobs      = log(total_jobs),
    log_live_in         = ifelse(live_and_work > 0, log(live_and_work), NA_real_),
    log_live_out        = ifelse(live_outside > 0, log(live_outside), NA_real_))]
  rm(od)

  # attach the tax rates to the workplace counts
  cousub_rates <- unique(rate_panel[!is.na(cousub_fips),
    .(cousub_fips, year, resident_eit, sd_eit, total_eit, muni_nonres_eit, rate_wedge,
      nonres_imputed, rate_change, nonres_change, event_type, first_increase_year)])
  panel <- merge(city_od, cousub_rates, by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)
  saveRDS(panel, file.path(DERIVED_DIR, "pa_panel.rds"))
  print(paste("PA panel:", nrow(panel), "rows, with rate:", sum(!is.na(panel$resident_eit))))
})

# Residence panel ----
# The same LODES counted from the home end: for each town and year, how many
# employed residents it has and how many of them work in town. The headline
# panel merges this on. (This used to be the separate build_residence_panel.R.)
if (RUN_RESIDENCE_PANEL) local({
  max_year <- 2019
  od <- as.data.table(readRDS(file.path(RAW_DIR, "lodes_block", "pa_od_S000_main.rds")))
  block_xwalk <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_block_xwalk.rds")))
  setnames(od, c("h_geocode", "w_geocode"), c("h_block", "w_block"), skip_absent = TRUE)
  od[, year := as.integer(year)]
  od <- od[year <= max_year]
  od <- merge(od, block_xwalk[, .(h_block = block_geoid, home_city = cousub_fips)], by = "h_block", all.x = TRUE)
  od <- merge(od, block_xwalk[, .(w_block = block_geoid, work_city = cousub_fips)], by = "w_block", all.x = TRUE)
  od <- od[!is.na(home_city) & !is.na(work_city)]   # both ends in a PA town; no out-of-state file here

  residence <- od[, .(residents_employed     = sum(S000, na.rm = TRUE),
                      residents_work_in_own  = sum(S000[home_city == work_city], na.rm = TRUE),
                      residents_work_outside = sum(S000[home_city != work_city], na.rm = TRUE)),
                  by = .(home_city, year)]
  residence[, share_work_in_own  := residents_work_in_own / residents_employed]
  residence[, share_work_outside := 1 - share_work_in_own]
  residence[, log_residents      := ifelse(residents_employed > 0, log(residents_employed), NA_real_)]
  residence[, log_work_in_own    := ifelse(residents_work_in_own > 0, log(residents_work_in_own), NA_real_)]
  residence[, log_work_outside   := ifelse(residents_work_outside > 0, log(residents_work_outside), NA_real_)]

  rate_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  home_rates <- rate_panel[!is.na(cousub_fips) & year <= max_year,
                           .(home_city = cousub_fips, year, resident_eit, sd_eit, total_eit, muni_nonres_eit,
                             event_type, first_increase_year, rate_change)]
  # a cousub with several DCED towns gets one row per town here; build_headline_env() averages them
  residence <- merge(residence, home_rates, by = c("home_city", "year"), all.x = TRUE)
  residence[, home_stack := resident_eit + sd_eit]
  saveRDS(residence, file.path(DERIVED_DIR, "pa_residence_panel.rds"))
  print(sprintf("Residence panel: %s rows, %d home cousubs",
                format(nrow(residence), big.mark = ","), uniqueN(residence$home_city)))
})

# Block panel ----
# Worker counts for every census block and year, with its town's tax rates and
# budget shares. Only the block summary table uses it (not in the paper).
if (RUN_BLOCK_PANEL) local({
  max_year <- 2019
  block_xwalk <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_block_xwalk.rds")))
  print(sprintf("Block-cousub crosswalk: %s blocks", format(nrow(block_xwalk), big.mark = ",")))
  od <- as.data.table(readRDS(file.path(RAW_DIR, "lodes_block", "pa_od_S000_main.rds")))
  setnames(od, c("h_geocode", "w_geocode"), c("h_block", "w_block"), skip_absent = TRUE)
  od[, year := as.integer(year)]
  od <- od[year <= max_year]
  od <- merge(od, block_xwalk[, .(h_block = block_geoid, home_muni = cousub_fips)], by = "h_block", all.x = TRUE)
  od <- merge(od, block_xwalk[, .(w_block = block_geoid, work_muni = cousub_fips)], by = "w_block", all.x = TRUE)
  od <- od[!is.na(home_muni) & !is.na(work_muni)]

  home_counts <- od[, .(home_count = sum(S000, na.rm = TRUE),
                        home_in_muni = sum(S000[home_muni == work_muni], na.rm = TRUE)),
                    by = .(block_geoid = h_block, year)]
  work_counts <- od[, .(work_count = sum(S000, na.rm = TRUE)), by = .(block_geoid = w_block, year)]
  block_panel <- merge(home_counts, work_counts, by = c("block_geoid", "year"), all = TRUE)
  block_panel[is.na(home_count),   home_count   := 0]
  block_panel[is.na(home_in_muni), home_in_muni := 0]
  block_panel[is.na(work_count),   work_count   := 0]
  log_pos <- function(value) ifelse(value > 0, log(as.numeric(value)), NA_real_)
  block_panel[, log_home_count     := log_pos(home_count)]
  block_panel[, log_work_count     := log_pos(work_count)]
  block_panel[, home_in_muni_share := ifelse(home_count > 0, home_in_muni / home_count, NA_real_)]
  block_panel[, share_work_outside := 1 - home_in_muni_share]
  block_panel <- merge(block_panel, block_xwalk[, .(block_geoid, muni = cousub_fips)], by = "block_geoid", all.x = TRUE)
  block_panel <- block_panel[!is.na(muni)]

  rate_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  rate_cols <- intersect(c("cousub_fips", "municipality_id", "year", "resident_eit", "sd_eit",
                           "total_eit", "muni_nonres_eit", "event_type"), names(rate_panel))
  rates <- unique(rate_panel[, ..rate_cols])
  block_panel <- merge(block_panel, rates, by.x = c("muni", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

  # a town's school district is the one it reports most often
  register <- fread(file.path(RAW_DIR, "pa_eit_rate_panel.csv"))[!is.na(sd_id), .(municipality_id, sd_id)]
  usual_sd <- register[, .N, by = .(municipality_id, sd_id)][order(municipality_id, -N), .SD[1], by = municipality_id][
    , .(municipality_id, sd_id)]
  block_panel <- merge(block_panel, usual_sd, by = "municipality_id", all.x = TRUE)

  # budget shares, same recipe as build_headline_env()
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  muni_xwalk <- unique(rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  afr <- merge(afr, muni_xwalk, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  afr[, afr_year := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
  fiscal <- afr[!is.na(cousub_fips), {
    total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
    total_exp <- as.numeric(total_expenditures)
    eit_revenue_amount <- as.numeric(ifelse(!is.na(earned_income_tax_revenues), earned_income_tax_revenues, earned_income_tax))
    re_tax_revenue <- as.numeric(ifelse(!is.na(real_estate_tax_revenues), real_estate_tax_revenues, real_estate_tax))
    intergov_federal <- as.numeric(`intergovernmental_revenues-federal_government`)
    intergov_state   <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                          `intergovernmental_revenues-state_government`, state_govt))
    intergov_local   <- as.numeric(`intergovernmental_revenues-local_government`)
    intergov <- rowSums(cbind(intergov_federal, intergov_state, intergov_local), na.rm = TRUE)
    intergov[is.na(intergov_federal) & is.na(intergov_state) & is.na(intergov_local)] <- NA_real_
    police_exp <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
    streets_exp <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                      `public_works-highways_and_streets_expenditures`, streets_and_roads))
    debt_service <- as.numeric(debt_service_expenditures)
    list(muni = cousub_fips, year = afr_year,
         share_police   = ifelse(total_exp > 0, police_exp   / total_exp, NA_real_),
         share_streets  = ifelse(total_exp > 0, streets_exp  / total_exp, NA_real_),
         share_debt_svc = ifelse(total_exp > 0, debt_service / total_exp, NA_real_),
         share_eit      = ifelse(total_rev > 0, eit_revenue_amount / total_rev, NA_real_),
         share_re       = ifelse(total_rev > 0, re_tax_revenue     / total_rev, NA_real_),
         share_intergov = ifelse(total_rev > 0, intergov           / total_rev, NA_real_))
  }]
  fiscal <- fiscal[!is.na(muni) & !is.na(year)]
  fiscal <- fiscal[, lapply(.SD, function(column) if (is.numeric(column)) mean(column, na.rm = TRUE) else first(column)),
                   by = .(muni, year)]
  for (col in names(fiscal)) if (is.numeric(fiscal[[col]])) set(fiscal, which(!is.finite(fiscal[[col]])), col, NA_real_)
  block_panel <- merge(block_panel, fiscal, by = c("muni", "year"), all.x = TRUE)

  print(sprintf("Block panel: %s rows; %s blocks; %d munis; %d SDs (DCED)",
                format(nrow(block_panel), big.mark = ","), format(uniqueN(block_panel$block_geoid), big.mark = ","),
                uniqueN(block_panel$muni), uniqueN(block_panel$sd_id)))
  saveRDS(block_panel, file.path(DERIVED_DIR, "pa_block_panel.rds"))
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
