#!/usr/bin/env Rscript
# 01_data_prep.R -- build the analysis panels from LOCAL inputs (no network*).
#
# Inputs (data/raw/):
#   pa_dced_raw/pa_taxes_YYYY.xls   DCED tax-register XLS (from 00_scrape.R)
#   pa_sd_finance_raw.csv           Census F-33 SD finances (from 00_scrape.R)
#   pa_afr_panel.csv                DCED muni AFR panel (from 00_scrape.R)
#   lodes_block/*.rds               PA block-level LODES (from 00_scrape.R)
#   pa_eit_rate_panel.csv           rewritten in place by the DCED reparse section
#
# Outputs (data/derived/):
#   pa_rate_panel.rds, pa_panel.rds, pa_panel_high.rds, pa_city_od.rds,
#   pa_city_od_high.rds, pa_tract_xwalk.rds, pa_block_xwalk.rds, pa_cousub.rds,
#   pa_neighbors.rds, pa_sd_controls.rds, pa_sd_id_xwalk.rds, pa_block_panel.rds
#
# *The cousub-panels section falls back to tigris (cached) only for PA
#  geometry not already on disk (pa_cousub.rds is vendored in data/derived/).
# Run from package root:  Rscript scripts/01_data_prep.R

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

.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_helpers.R"))

# Flags ----
RUN_DCED_REPARSE  <- TRUE  # DCED XLS -> pa_eit_rate_panel.csv, keeping SD id/name
RUN_SD_CONTROLS   <- TRUE  # Census F-33 -> SD share controls + NCES<->DCED xwalk
RUN_COUSUB_PANELS <- TRUE  # cousub rate / OD / fiscal panels + block->cousub xwalk
RUN_BLOCK_PANEL   <- TRUE  # block x year panel inheriting rates/shares from cousub
RUN_PROPERTY_REBASE <- TRUE  # repair millage/CLR assessment-basis mismatches

# DCED reparse ----
# Re-reads the raw DCED tax-register XLS files and rewrites
# data/raw/pa_eit_rate_panel.csv keeping the School District ID and name
# columns (the original scrape dropped them); flags resident vs nonresident
# tax lines from the tax-name text.
if (RUN_DCED_REPARSE) local({
  xls_files <- list.files(file.path(RAW_DIR, "pa_dced_raw"),
                          pattern = "^pa_taxes_[0-9]{4}\\.xls$",
                          full.names = TRUE)
  print(sprintf("XLS files: %d", length(xls_files)))
  clean <- function(s) tolower(str_squish(as.character(s)))

  # Lay explanation: the state publishes one messy Excel workbook per year,
  # with a separate row for every tax every town levies. This reads each
  # year's file and stacks them into one tidy table -- one row per town,
  # tax, and year -- keeping the school-district ID the first pass dropped.
  read_one <- function(f) {
    yr <- as.integer(str_extract(basename(f), "[0-9]{4}"))
    d <- as.data.table(read_excel(f, .name_repair = "minimal"))
    setnames(d, c("Reporting Year", "County", "Municipality ID", "Municipality",
                  "School District ID", "School District", "Tax Name",
                  "Municipal Tax Amount", "School District Tax Amount", "Total Tax Rate"),
             c("year", "county", "municipality_id", "municipality",
               "sd_id", "sd_name", "tax_name",
               "muni_eit_rate", "sd_eit_rate", "total_eit_rate"),
             skip_absent = TRUE)
    d <- d[, .(year, county, municipality, municipality_id, sd_id, sd_name,
               muni_eit_rate, sd_eit_rate, total_eit_rate, tax_name)]
    d[, year           := as.integer(year)]
    d[, municipality_id := as.integer(municipality_id)]
    d[, sd_id          := as.integer(sd_id)]
    d[, muni_eit_rate  := as.numeric(muni_eit_rate)]
    d[, sd_eit_rate    := as.numeric(sd_eit_rate)]
    d[, total_eit_rate := as.numeric(total_eit_rate)]
    d[, tn := clean(tax_name)]
    d[, is_resident    := grepl("resident", tn) & !grepl("non.?resident", tn)]
    d[, is_nonresident := grepl("non.?resident", tn)]
    d[, tn := NULL]
    d
  }

  all <- rbindlist(lapply(xls_files, read_one), fill = TRUE)
  fwrite(all, file.path(RAW_DIR, "pa_eit_rate_panel.csv"))
  print(sprintf("Rewrote panel: %d rows", nrow(all)))
  print(sprintf("Distinct sd_id: %d", uniqueN(all$sd_id)))

  mu <- all[is_resident & !is.na(sd_eit_rate),
            .(n = uniqueN(sd_id)), by = .(municipality_id, year)][n > 1]
  print(sprintf("Multi-SD muni-years: %d", nrow(mu)))
  sid <- mu$municipality_id[1]; sy <- mu$year[1]
  print(all[municipality_id == sid & year == sy & is_resident,
            .(year, municipality, sd_id, sd_name, muni_eit_rate, sd_eit_rate)])
})

# SD fiscal controls ----
# Builds school-district-level fiscal share controls parallel to the muni AFR
# shares from the Census F-33 data, and the NCESID -> DCED sd_id crosswalk
# (exact name match + Jaro-Winkler fuzzy fallback + 2 manual overrides).
# Outputs pa_sd_controls.rds (one row per sd_id x year) and pa_sd_id_xwalk.rds.
if (RUN_SD_CONTROLS) local({
  ## Load raw F-33 data, coerce numeric, compute SD share controls ----
  raw <- fread(file.path(RAW_DIR, "pa_sd_finance_raw.csv"))

  # F-33 amounts are in thousands of dollars (or sometimes whole dollars,
  # depending on year). The shares are unit-invariant, so this doesn't
  # matter for control construction.
  num_cols <- c("TOTALREV", "TFEDREV", "TSTREV", "TLOCREV", "T06",
                "TOTALEXP", "TCURINST", "TCURSSVC", "TCAPOUT", "_41F", "V33")
  for (cc in num_cols) {
    if (cc %in% names(raw)) {
      set(raw, j = cc, value = suppressWarnings(as.numeric(raw[[cc]])))
    }
  }
  setnames(raw, "_41F", "DEBT_INT_EXP", skip_absent = TRUE)
  setnames(raw, "fy", "year")

  print(sprintf("Raw rows: %s", format(nrow(raw), big.mark = ",")))

  # School level filter: keep K-12 unified districts only (SCHLEV "03" = K-12
  # unified; PA has additional codes like "01" elementary, "02" secondary).
  # Default: keep everything; flag SCHLEV distribution.
  print("SCHLEV distribution:")
  print(raw[, .N, keyby = SCHLEV])

  # Compute shares parallel to muni AFR
  sd_ctrl <- raw[, .(
    ncesid       = NCESID,
    idcensus     = IDCENSUS,
    sd_name_f33  = NAME,
    year         = year,
    schlev       = SCHLEV,
    enrollment   = V33,
    total_rev    = TOTALREV,
    total_exp    = TOTALEXP,
    share_state_rev_sd      = ifelse(TOTALREV > 0, TSTREV  / TOTALREV, NA_real_),
    share_local_rev_sd      = ifelse(TOTALREV > 0, TLOCREV / TOTALREV, NA_real_),
    share_federal_rev_sd    = ifelse(TOTALREV > 0, TFEDREV / TOTALREV, NA_real_),
    share_property_tax_sd   = ifelse(TOTALREV > 0, T06     / TOTALREV, NA_real_),
    share_instructional_sd  = ifelse(TOTALEXP > 0, TCURINST / TOTALEXP, NA_real_),
    share_support_sd        = ifelse(TOTALEXP > 0, TCURSSVC / TOTALEXP, NA_real_),
    share_capital_sd        = ifelse(TOTALEXP > 0, TCAPOUT  / TOTALEXP, NA_real_),
    share_debt_int_sd       = ifelse(TOTALEXP > 0, DEBT_INT_EXP / TOTALEXP, NA_real_)
  )]

  # Replace non-finite with NA
  for (cc in grep("^share_", names(sd_ctrl), value = TRUE)) {
    set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
  }

  print(sprintf("SD controls panel: %s rows, %d unique SDs",
                format(nrow(sd_ctrl), big.mark = ","),
                uniqueN(sd_ctrl$ncesid)))
  print("Share control summary (means across SD-years):")
  print(sd_ctrl[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 3)),
                .SDcols = grep("^share_", names(sd_ctrl), value = TRUE)])

  ## Build NCESID -> DCED sd_id crosswalk via SD name matching ----
  eit <- fread(file.path(RAW_DIR, "pa_eit_rate_panel.csv"))
  dced_sd <- unique(eit[!is.na(sd_id), .(sd_id, sd_name)])
  print(sprintf("DCED unique SDs: %d", nrow(dced_sd)))

  # Normalize SD names for matching (same function used in build_block_panel.R)
  # Lay explanation: the two agencies spell district names differently --
  # "MT LEBANON SD" vs "Mount Lebanon School District". This scrubs both
  # down to the same bare words (drop "school district", expand the
  # abbreviations) so names that mean the same thing become identical text.
  norm_sd <- function(x) {
    x <- tolower(x)
    # Strip non-alphanumeric BEFORE word-boundary matching to handle
    # punctuation like "ALLEG.-CLARION" or "PHILIPSBURG-OSCEOLA"
    x <- gsub("[^a-z0-9 ]", " ", x)
    x <- gsub("\\s+", " ", x)
    # Normalize all the variations of "school district"
    x <- gsub("\\basd\\b", "", x)
    x <- gsub("\\bschool district\\b", "", x)
    x <- gsub("\\bsch dist\\b", "", x)
    x <- gsub("\\bschool dist\\b", "", x)
    x <- gsub("\\bs d\\b", "", x)
    x <- gsub("\\bsd\\b", "", x)
    x <- gsub("\\bsch\\b", "", x)
    x <- gsub("\\bschool\\b", "", x)
    # Common DCED abbreviations -> NCES full forms
    abbrevs <- c(
      # Concatenations -> spaced
      "stnycrk"  = "stonycreek",
      "stnycreek" = "stonycreek",
      "stony creek" = "stonycreek",
      "south eastern" = "southeastern",
      "north eastern" = "northeastern",
      "north western" = "northwestern",
      "south western" = "southwestern",
      "clarionvalley" = "clarion valley",
      "brothersvalley" = "brothers valley",
      # Common abbreviations
      "strousbg" = "stroudsburg",
      "stroudsbg" = "stroudsburg",
      "kensngtn" = "kensington",
      "kenington" = "kensington",
      "pittsbgh" = "pittsburgh",
      "regn"     = "regional",
      "reg"      = "regional",
      "comm"     = "community",
      "cmnty"    = "community",
      "consolidatd" = "consolidated",
      "cons"     = "consolidated",
      "alleg"    = "allegheny",
      "alleghny" = "allegheny",
      "lancastr" = "lancaster",
      "swarthmr" = "swarthmore",
      "wallingfrd" = "wallingford",
      "swarthm"  = "swarthmore",
      "northn"   = "northern",
      "lwr"      = "lower",
      "upr"      = "upper",
      "turkeyft" = "turkeyfoot",
      "turkey foot" = "turkeyfoot",
      "vlly"     = "valley",
      "val"      = "valley",
      "hts"      = "heights",
      "cty"      = "city",
      "twnshp"   = "township",
      # Mt vs Mountain: collapse both to mt (keeps "tussey mt" matching
      # "tussey mountain" as well as "mt lebanon" matching "mount lebanon")
      "mountain" = "mt",
      "mount"    = "mt",
      # East/Eastern, etc. — collapse to short form for cross-source match
      "eastern"  = "east",
      "western"  = "west",
      "southern" = "south"
      # Note: northern -> north would conflict with too many edge cases
    )
    for (k in names(abbrevs)) {
      x <- gsub(paste0("\\b", k, "\\b"), abbrevs[[k]], x)
    }
    # Words that appear in one source but not the other
    x <- gsub("\\b(area|the|joint|borough|boro|city|township|twp|of|co|county|merged|union|intermediate|unit|comm)\\b",
              "", x)
    # Standalone single letters (e.g., "albert gallatin a" should drop the "a")
    x <- gsub("\\b[a-z]\\b", "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  f33_names <- unique(sd_ctrl[, .(ncesid, sd_name_f33)])
  f33_names[, sd_name_norm := norm_sd(sd_name_f33)]
  dced_sd[, sd_name_norm := norm_sd(sd_name)]

  xwalk <- merge(f33_names, dced_sd, by = "sd_name_norm", allow.cartesian = TRUE)
  print(sprintf("Exact-match: NCESID -> DCED sd_id: %d / %d",
                uniqueN(xwalk$ncesid), uniqueN(f33_names$ncesid)))

  # Fuzzy fallback for remaining unmatched DCED SDs.
  # For each unmatched DCED SD, find the closest F-33 SD by string distance.
  # Lay explanation: for names that still don't line up letter-for-letter,
  # work like a careful clerk: compare each leftover DCED name against every
  # leftover Census name, score how alike they look, and accept the best
  # candidate only when it is nearly identical (Jaro-Winkler distance < 0.15).
  unmatched_dced_now <- dced_sd[!sd_id %in% xwalk$sd_id]
  unmatched_f33_now  <- f33_names[!ncesid %in% xwalk$ncesid]
  if (nrow(unmatched_dced_now) > 0 && nrow(unmatched_f33_now) > 0 &&
      requireNamespace("stringdist", quietly = TRUE)) {
    print(sprintf("Fuzzy-matching %d unmatched DCED SDs against %d F-33 candidates...",
                  nrow(unmatched_dced_now), nrow(unmatched_f33_now)))
    fuzzy_pairs <- list()
    for (i in seq_len(nrow(unmatched_dced_now))) {
      dn <- unmatched_dced_now$sd_name_norm[i]
      if (is.na(dn) || dn == "") next
      dists <- stringdist::stringdist(dn, unmatched_f33_now$sd_name_norm,
                                      method = "jw", p = 0.1)
      best <- which.min(dists)
      if (length(best) == 1 && dists[best] < 0.15) {
        fuzzy_pairs[[length(fuzzy_pairs) + 1L]] <- data.table(
          sd_id   = unmatched_dced_now$sd_id[i],
          ncesid  = unmatched_f33_now$ncesid[best],
          sd_name = unmatched_dced_now$sd_name[i],
          sd_name_f33 = unmatched_f33_now$sd_name_f33[best],
          dist    = dists[best])
      }
    }
    if (length(fuzzy_pairs) > 0) {
      fuzzy_dt <- rbindlist(fuzzy_pairs)
      print(sprintf("Fuzzy matched %d additional DCED SDs (jw < 0.15):",
                    nrow(fuzzy_dt)))
      print(fuzzy_dt[order(dist)][1:min(20, .N)])
      # Append fuzzy matches to xwalk
      xwalk <- rbind(xwalk,
        fuzzy_dt[, .(sd_name_norm = NA_character_,
                     ncesid, sd_name_f33,
                     sd_id, sd_name)],
        fill = TRUE)
    }
  } else if (!requireNamespace("stringdist", quietly = TRUE)) {
    print("stringdist package not available; skipping fuzzy fallback.")
  }

  # Manual overrides for SDs whose DCED and Census F-33 names diverge entirely
  # (rebrands, mergers, alternate names that fuzzy match cannot bridge).
  manual_pairs <- data.table(
    sd_id   = c(107803L, 321103L),
    ncesid  = c("4221930", "4203750"),
    sd_name = c("KNOCH SD", "RIVER VALLEY SD"),
    sd_name_f33 = c("SOUTH BUTLER CO SCH", "BLAIRSVILLE SALTSBURG SCHOOL DIST")
  )
  manual_pairs[, sd_name_norm := NA_character_]
  xwalk <- rbind(xwalk, manual_pairs, fill = TRUE)

  print(sprintf("Final matched DCED SDs: %d / %d (%.1f%%)",
                uniqueN(xwalk$sd_id), uniqueN(dced_sd$sd_id),
                100 * uniqueN(xwalk$sd_id) / uniqueN(dced_sd$sd_id)))

  unmatched_f33 <- setdiff(f33_names$ncesid, xwalk$ncesid)
  if (length(unmatched_f33) > 0) {
    print(sprintf("Unmatched F-33 SDs (%d):", length(unmatched_f33)))
    print(head(f33_names[ncesid %in% unmatched_f33], 20))
  }
  unmatched_dced <- setdiff(dced_sd$sd_id, xwalk$sd_id)
  if (length(unmatched_dced) > 0) {
    print(sprintf("Unmatched DCED SDs (%d):", length(unmatched_dced)))
    print(head(dced_sd[sd_id %in% unmatched_dced], 20))
  }

  # Save the crosswalk and merge sd_id into the control panel
  saveRDS(xwalk[, .(ncesid, sd_name_f33, sd_id, sd_name_dced = sd_name)],
          file.path(DERIVED_DIR, "pa_sd_id_xwalk.rds"))

  sd_ctrl <- merge(sd_ctrl,
                   unique(xwalk[, .(ncesid, sd_id)]),
                   by = "ncesid", all.x = TRUE)

  # Drop SD-years without a DCED match (cant join to rate panel)
  sd_ctrl_matched <- sd_ctrl[!is.na(sd_id)]
  print(sprintf("SD controls (matched to DCED): %s rows, %d unique sd_id",
                format(nrow(sd_ctrl_matched), big.mark = ","),
                uniqueN(sd_ctrl_matched$sd_id)))

  saveRDS(sd_ctrl_matched, file.path(DERIVED_DIR, "pa_sd_controls.rds"))
  print("Saved data/derived/pa_sd_controls.rds")
  print("Saved data/derived/pa_sd_id_xwalk.rds")
})

# Cousub panels ----
# The main data build: DCED rate panel with Act 32 max-rule fields and the
# DCED -> Census COUSUBFP crosswalk, TIGER tract/block crosswalks, neighbor
# pairs, block-level LODES aggregated to cousub OD panels, and the
# analysis-ready pa_panel.rds (OD + rates). Block-level LODES recovers all
# 2,561 PA cousubs; the prior tract-centroid join silently dropped 42%.
if (RUN_COUSUB_PANELS) local({
  options(tigris_use_cache = TRUE)
  lodes_block_dir <- file.path(RAW_DIR, "lodes_block")  # block-level (PA)

  ## Pennsylvania rate panel ----
  # Source: DCED MunStats via scrape_pa_dced_eit.py
  pa_eit_file <- file.path(RAW_DIR, "pa_eit_rate_panel.csv")
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

  # Lay explanation: a town can appear on several rows in a year (one per
  # reported tax line). Collapsing with max() leaves one rate per town-year;
  # for the ~2% of towns split across two school districts it keeps the
  # higher SD rate -- exact for everyone else, conservative for those few.
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
  # Lay explanation: before 2012 many towns simply didn't report their
  # nonresident rate to the state, even though they had one. Because these
  # rates almost never move, we fill each gap with the town's earliest
  # post-2012 rate and flag every filled-in value as imputed.
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

  ## Crosswalk DCED -> Census COUSUBFP via name + county ----
  pa_cousub_file <- file.path(DERIVED_DIR, "pa_cousub.rds")
  pa_cousub <- if (file.exists(pa_cousub_file)) readRDS(pa_cousub_file) else st_transform(county_subdivisions(state = "PA", year = 2020), 4326)

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

  saveRDS(pa_rate_panel, file.path(DERIVED_DIR, "pa_rate_panel.rds"))
  n_matched <- sum(!is.na(unique(pa_rate_panel[, .(municipality_id, cousub_fips)])$cousub_fips))
  n_total <- uniqueN(pa_rate_panel$municipality_id)
  print(paste("PA rate panel:", nrow(pa_rate_panel), "rows,",
    n_matched, "of", n_total, "matched (",
    round(100 * n_matched / n_total, 1), "%)"))

  ## TIGER boundaries + tract crosswalks ----
  pa_tracts <- tracts(state = "PA", year = 2020) %>% st_transform(4326)

  # PA: tract -> cousub
  pa_tract_xwalk <- st_join(
    pa_tracts %>% mutate(centroid = st_centroid(geometry)) %>% st_set_geometry("centroid"),
    pa_cousub %>% select(cousub_fips = COUSUBFP, cousub_name = NAME),
    left = TRUE) %>%
    st_drop_geometry() %>%
    select(tract_geoid = GEOID, cousub_fips, cousub_name)
  saveRDS(pa_tract_xwalk, file.path(DERIVED_DIR, "pa_tract_xwalk.rds"))

  # PA: block -> cousub (used by block-level LODES aggregation)
  # Blocks nest within cousubs in 2020 Census hierarchy, so a centroid
  # point-in-polygon is exact. Cached because the spatial join is the slowest
  # step in the build (~30s after blocks are downloaded).
  pa_block_xwalk_file <- file.path(DERIVED_DIR, "pa_block_xwalk.rds")
  # Lay explanation: census blocks are the smallest map pieces, and each one
  # sits wholly inside a single municipality, like a tile inside one room.
  # Dropping a pin at each block's center and asking which town it lands in
  # gives every block exactly one town -- and one set of tax rates to inherit.
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
  saveRDS(pa_cousub, file.path(DERIVED_DIR, "pa_cousub.rds"))

  ## City-to-city neighbor pairs ----
  # PA: 15km radius
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
  saveRDS(pa_neighbors, file.path(DERIVED_DIR, "pa_neighbors.rds"))
  rm(pa_dist_mat)

  print(paste("PA neighbors:", nrow(pa_neighbors), "pairs"))

  ## City-level OD panels ----
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

    # Lay explanation: each LODES row says "this many people live in block A
    # and work in block B." After tagging both ends with their towns, this
    # tallies, for every workplace town and year, the total jobs there and
    # how many of those workers also live in that town versus commute in.
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

  pa_city_od <- build_city_od_block(
    file.path(lodes_block_dir, "pa_od_S000_main.rds"),
    file.path(lodes_block_dir, "pa_od_S000_aux.rds"),
    pa_block_xwalk, "42")

  pa_city_od_high <- build_city_od_block(
    file.path(lodes_block_dir, "pa_od_SE03_main.rds"),
    NULL, pa_block_xwalk, "42")

  saveRDS(pa_city_od, file.path(DERIVED_DIR, "pa_city_od.rds"))
  saveRDS(pa_city_od_high, file.path(DERIVED_DIR, "pa_city_od_high.rds"))

  print(paste("PA city OD:", nrow(pa_city_od), "rows"))

  ## Analysis-ready panels (OD + rates) ----
  pa_rate_merge <- pa_rate_panel[!is.na(cousub_fips),
    .(cousub_fips, year, resident_eit, sd_eit, total_eit, muni_nonres_eit, rate_wedge,
      nonres_imputed, rate_change, nonres_change, event_type, first_increase_year)] %>%
    unique()
  pa_panel <- merge(pa_city_od, pa_rate_merge,
    by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

  pa_panel_high <- merge(pa_city_od_high, pa_rate_merge,
    by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

  saveRDS(pa_panel, file.path(DERIVED_DIR, "pa_panel.rds"))
  saveRDS(pa_panel_high, file.path(DERIVED_DIR, "pa_panel_high.rds"))

  print(paste("PA panel:", nrow(pa_panel), "rows, with rate:", sum(!is.na(pa_panel$resident_eit))))

  print("All cousub panels built.")
})

# Block panel ----
# Block-level panel for the unified-design block-level robustness. Each block
# inherits its cousub's rates and SD assignment directly from the existing
# cousub-level infrastructure -- no new spatial work.
# Outputs data/derived/pa_block_panel.rds (block x year).
if (RUN_BLOCK_PANEL) local({
  max_year <- 2019

  ## Block -> cousub crosswalk ----
  xw <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_block_xwalk.rds")))
  print(sprintf("Block-cousub crosswalk: %s blocks",
                format(nrow(xw), big.mark = ",")))

  ## Block-level LODES OD aggregated to block-year home/work counts ----
  od <- as.data.table(readRDS(file.path(RAW_DIR, "lodes_block", "pa_od_S000_main.rds")))
  setnames(od, c("h_geocode", "w_geocode"), c("h_block", "w_block"), skip_absent = TRUE)
  od[, year := as.integer(year)]
  od <- od[year <= max_year]

  # Map each side to muni
  od <- merge(od, xw[, .(h_block = block_geoid, home_muni = cousub_fips)],
              by = "h_block", all.x = TRUE)
  od <- merge(od, xw[, .(w_block = block_geoid, work_muni = cousub_fips)],
              by = "w_block", all.x = TRUE)
  od <- od[!is.na(home_muni) & !is.na(work_muni)]

  home <- od[, .(home_count   = sum(S000, na.rm = TRUE),
                 home_in_muni = sum(S000[home_muni == work_muni], na.rm = TRUE)),
              by = .(block_geoid = h_block, year)]
  work <- od[, .(work_count = sum(S000, na.rm = TRUE)),
              by = .(block_geoid = w_block, year)]

  panel <- merge(home, work, by = c("block_geoid", "year"), all = TRUE)
  panel[is.na(home_count),   home_count   := 0]
  panel[is.na(home_in_muni), home_in_muni := 0]
  panel[is.na(work_count),   work_count   := 0]

  # Logs and shares
  log_pos <- function(x) ifelse(x > 0, log(as.numeric(x)), NA_real_)
  panel[, log_home_count    := log_pos(home_count)]
  panel[, log_work_count    := log_pos(work_count)]
  panel[, home_in_muni_share := ifelse(home_count > 0, home_in_muni / home_count, NA_real_)]
  panel[, share_work_outside := 1 - home_in_muni_share]

  ## Attach muni assignment ----
  panel <- merge(panel, xw[, .(block_geoid, muni = cousub_fips)],
                 by = "block_geoid", all.x = TRUE)
  panel <- panel[!is.na(muni)]

  ## Inherit rates from cousub-level rate panel ----
  rate <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  rate_cols <- intersect(c("cousub_fips", "municipality_id", "year", "resident_eit", "sd_eit",
                           "total_eit", "muni_nonres_eit", "event_type"),
                         names(rate))
  rate <- unique(rate[, ..rate_cols])
  panel <- merge(panel, rate, by.x = c("muni", "year"), by.y = c("cousub_fips", "year"),
                 all.x = TRUE)

  ## Attach primary sd_id per muni from raw DCED panel (modal SD across years) ----
  eit_raw <- fread(file.path(RAW_DIR, "pa_eit_rate_panel.csv"))
  eit_raw <- eit_raw[!is.na(sd_id), .(municipality_id, sd_id)]
  # Lay explanation: a town occasionally shows up under different school
  # districts across years (reporting quirks). We assign each town the
  # district it reports most often -- its usual school district -- and every
  # block in the town inherits that assignment.
  muni_sd <- eit_raw[, .N, by = .(municipality_id, sd_id)][
    order(municipality_id, -N), .SD[1], by = municipality_id][
    , .(municipality_id, sd_id)]
  panel <- merge(panel, muni_sd, by = "municipality_id", all.x = TRUE)

  ## Fiscal share controls from the muni AFR panel ----
  # Same six share-based controls recipe as q3_unified_full_table.R,
  # merged to the block panel via the muni assignment.
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  xw_id <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  afr <- merge(afr, xw_id, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
  fc <- afr[!is.na(cousub_fips), {
    total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
    total_exp <- as.numeric(total_expenditures)
    eit_rev <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                                 earned_income_tax_revenues, earned_income_tax))
    re_tax  <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                                 real_estate_tax_revenues, real_estate_tax))
    fed_ig <- as.numeric(`intergovernmental_revenues-federal_government`)
    sta_ig <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                 `intergovernmental_revenues-state_government`, state_govt))
    loc_ig <- as.numeric(`intergovernmental_revenues-local_government`)
    intergov <- rowSums(cbind(fed_ig, sta_ig, loc_ig), na.rm = TRUE)
    intergov[is.na(fed_ig) & is.na(sta_ig) & is.na(loc_ig)] <- NA_real_
    police_exp <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
    streets_exp <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                      `public_works-highways_and_streets_expenditures`,
                                      streets_and_roads))
    debt_svc <- as.numeric(debt_service_expenditures)
    list(muni = cousub_fips, year = yr,
         share_police   = ifelse(total_exp > 0, police_exp  / total_exp, NA_real_),
         share_streets  = ifelse(total_exp > 0, streets_exp / total_exp, NA_real_),
         share_debt_svc = ifelse(total_exp > 0, debt_svc    / total_exp, NA_real_),
         share_eit      = ifelse(total_rev > 0, eit_rev  / total_rev, NA_real_),
         share_re       = ifelse(total_rev > 0, re_tax   / total_rev, NA_real_),
         share_intergov = ifelse(total_rev > 0, intergov / total_rev, NA_real_))
  }]
  fc <- fc[!is.na(muni) & !is.na(year)]
  fc <- fc[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
           by = .(muni, year)]
  for (col in names(fc)) {
    if (is.numeric(fc[[col]])) set(fc, which(!is.finite(fc[[col]])), col, NA_real_)
  }
  panel <- merge(panel, fc, by = c("muni", "year"), all.x = TRUE)

  print(sprintf("Block panel: %s rows; %s blocks; %d munis; %d SDs (DCED)",
                format(nrow(panel), big.mark = ","),
                format(uniqueN(panel$block_geoid), big.mark = ","),
                uniqueN(panel$muni),
                uniqueN(panel$sd_id)))

  saveRDS(panel, file.path(DERIVED_DIR, "pa_block_panel.rds"))
  print("Saved data/derived/pa_block_panel.rds")
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()

# ---------------------------------------------------------------------------
# Property-tax effective rates: assessment-basis repair
# ---------------------------------------------------------------------------
# WHY THIS EXISTS
# Every effective rate in the property panel is  mills/1000 x CLR.  The CLR is
# one number per county-year, shared by all three layers (muni, school
# district, county).  When a county reassesses, the assessment basis resets:
# the CLR jumps toward 1.0 (assessed = market) and statutory millage should
# fall by the same factor, leaving the effective rate CONTINUOUS.  A
# reassessment changes the basis, not the burden.
#
# In the DCED source, many governments never rebased their reported millage.
# Their old-basis millage then gets multiplied by a new-basis CLR and the
# effective rate blows up.  Wilkes-Barre City is the worst case: Luzerne
# reassessed effective 2009 (its CLR moved in 2008), the county-wide median
# muni millage fell 22.50 -> 0.93, and Wilkes-Barre stayed at 73.63 -> 96.63
# -> 141.33.  Its effective rate reads 7-14 pp of market value for eleven
# straight years against its own correct pre-2008 range of 0.36-0.48 pp.
# Wilkes-Barre is an Inc-bind treated unit, so this is a corrupted control on
# a treated observation in the headline cell.
#
# TWO PROCESSES
#   1. Reassessment rebasing.  A unit that rebased shows a FLAT effective rate
#      through the basis reset; a stranded one shows the effective rate rise by
#      exactly the CLR ratio.  So the test is whether the unit's own
#      effective-rate jump MATCHES its county's CLR jump.  No threshold on the
#      CLR jump is used or wanted: 13 genuine reassessments have CLR jumps
#      below 1.5 (Allegheny 2013 = 1.17, York 2006 = 1.33, Lancaster 2018 =
#      1.36) because those counties already assessed near market, and the CLR
#      jump distribution is continuous with no natural break.  Matching the
#      two ratios identifies the failure and supplies the correction factor at
#      the same time, and it scales automatically.
#   2. Peer consistency.  Every unit in a county-year is multiplied by the SAME
#      CLR, so their effective rates should be comparable.  A unit sitting far
#      above its county-year peers for most of the panel is on the wrong basis
#      even if no reassessment occurred inside the sample window to reveal it.
#      This catches level mismatches and outright parse errors (Beaver SD
#      45853 reads 6555 mills against 63.55 and 67.05 either side).
#
# NOT USED: data/raw/multitax/pa_county_reassessment.csv.  Checked against what
# the CLR actually does, it agrees exactly for only 16 of 27 counties, is off by
# more than a year for 10 (Erie by 10, Lancaster by 13, Allegheny by 9), and
# misses 11 reassessments the CLR plainly shows (including Philadelphia 2014 and
# Wayne 2005).  It appears to record when a reassessment was ordered rather than
# when it took effect for tax purposes.  The CLR series is authoritative here
# because it IS the tax-basis ratio.  The file is loaded only to cross-tabulate
# in the audit log.
#
# The raw panel is never overwritten.  Output is a repaired copy plus a full
# audit log of every correction.
if (RUN_PROPERTY_REBASE) local({

  raw_path <- file.path(RAW_DIR, "multitax", "pa_property_burden_panel.csv")
  b <- fread(raw_path)

  # Tunables. PLAUSIBLE_PP is the ceiling above which a single layer's
  # effective rate is treated as impossible: PA rates run ~0.1-3 pp of market
  # value per layer, so 5 pp is already generous.
  PLAUSIBLE_PP    <- 5      # ceiling for a single layer, percentage points
  AT_MARKET       <- 0.95   # CLR at/above this = assessed value reset to market
  REBASE_TOL      <- 0.5    # millage must fall to <=50% of what the reset implies
  PEER_RATIO      <- 5      # x county-year peer median to suspect wrong basis
  PEER_PERSIST    <- 0.5    # ...in at least this share of the unit's years

  # CLR basis-change ratio, per county-year. Shared by all three layers.
  cl <- unique(b[, .(county, year, clr)])[order(county, year)]
  cl[, clr_prev := shift(clr), by = county]
  cl[, clr_ratio := clr / clr_prev]
  # A REASSESSMENT is not "the CLR moved a lot" -- the CLR drifts every year
  # with the market. It is the CLR being reset TO market value: assessed =
  # market, so the ratio lands at ~1.0 having been below it. That is the
  # definitional signature and it catches both the dramatic resets (Luzerne
  # 0.05 -> 1.00) and the quiet ones in counties already near market
  # (Allegheny 0.855 -> 1.00), which any jump-magnitude threshold misses.
  cl[, reassess := !is.na(clr_prev) & clr >= AT_MARKET & clr_prev < AT_MARKET]

  audit <- list()

  repair_layer <- function(id, mill_col, lbl) {
    keep <- unique(c(id, "county"))
    d <- unique(b[, c(keep, "year", mill_col), with = FALSE])
    setnames(d, mill_col, "mills")
    d <- merge(d, cl[, .(county, year, clr, clr_ratio, reassess)], by = c("county", "year"))
    setorderv(d, c(id, "year"))
    d[, eff := mills / 1000 * clr * 100]

    # ---- Process 1: reassessment rebasing -------------------------------
    d[, mill_prev := shift(mills), by = id]
    # In a reassessment year the basis changed by clr_ratio, so a unit that
    # rebased correctly shows its millage FALL by roughly that factor. A unit
    # still reporting on the old basis did not. Testing the millage response
    # (not the effective-rate ratio) is essential: because eff = mills x clr,
    # any unit with unchanged millage has eff_ratio identically equal to
    # clr_ratio, so matching those two ratios fires on ordinary CLR drift and
    # over-corrects roughly a third of the panel.
    # resid = 1 if the unit rebased exactly; = clr_ratio if it did not move at
    # all. Flag when it is well above 1, and use resid itself as the correction
    # so partial rebasing is handled too.
    d[, resid := (mills / mill_prev) * clr_ratio]
    d[, stranded := reassess %in% TRUE & !is.na(mill_prev) & mill_prev > 0 &
                    is.finite(resid) & resid > 1 / REBASE_TOL]
    d[, f1 := fifelse(stranded %in% TRUE, resid, 1)]
    d[, f1 := cummax(f1), by = id]          # carry forward: still on old basis
    # Release once the unit's own millage finally drops onto the new basis.
    d[, released := !is.na(mill_prev) & mills / mill_prev < 0.5]
    d[, rel := cumsum(fifelse(released %in% TRUE, 1L, 0L)), by = id]
    d[rel > 0, f1 := 1]
    d[, eff_p1 := eff / f1]

    # ---- Process 2: peer consistency ------------------------------------
    # Peers are the other units of the SAME layer in the same county-year,
    # all multiplied by the identical CLR. Counties have no within-county
    # peers, so their peer set is all other counties that year.
    peer_key <- if (identical(id, "county")) "year" else c("county", "year")
    d[, peer_med := median(eff_p1, na.rm = TRUE), by = peer_key]
    d[, peer_n   := .N, by = peer_key]
    d[, peer_ratio := eff_p1 / peer_med]
    # Persistent, not a one-off: a unit genuinely on the wrong basis is far
    # from its peers in most of its years.
    d[, persist := mean(peer_ratio > PEER_RATIO, na.rm = TRUE), by = id]
    d[, f2 := 1]
    d[peer_n >= 3 & persist >= PEER_PERSIST & peer_ratio > PEER_RATIO,
      f2 := peer_ratio]
    # One-off spikes above the ceiling are parse errors (decimal shifts);
    # correct them by their own peer ratio too, but log separately.
    d[, parse_err := peer_n >= 3 & persist < PEER_PERSIST &
                     eff_p1 > PLAUSIBLE_PP & peer_ratio > PEER_RATIO]
    d[parse_err %in% TRUE, f2 := peer_ratio]
    d[, eff_fixed := eff_p1 / f2]

    corrected <- d[f1 > 1 | f2 > 1]
    if (nrow(corrected)) {
      a <- corrected[, .(layer = lbl, unit = as.character(get(id)), county, year,
                         mills, clr,
                         eff_raw = eff, factor_p1 = f1, factor_p2 = f2,
                         eff_repaired = eff_fixed,
                         process = fifelse(f1 > 1 & f2 > 1, "both",
                                   fifelse(f1 > 1, "reassessment", "peer")))]
      audit[[lbl]] <<- a
    }
    message(sprintf(
      "[rebase] %-16s units corrected %4d | rows %5d | >%dpp: %5d -> %4d | max %8.2f -> %6.2f",
      lbl, uniqueN(corrected[[id]]), nrow(corrected), PLAUSIBLE_PP,
      sum(d$eff > PLAUSIBLE_PP, na.rm = TRUE),
      sum(d$eff_fixed > PLAUSIBLE_PP, na.rm = TRUE),
      max(d$eff, na.rm = TRUE), max(d$eff_fixed, na.rm = TRUE)))
    out <- unique(d[, c(id, "year", "eff_fixed"), with = FALSE], by = c(id, "year"))
    setnames(out, "eff_fixed", "eff_new")
    out
  }

  m_fix <- repair_layer("muni_id", "muni_re_total_mills", "MUNICIPAL")
  s_fix <- repair_layer("sd_id",   "sd_re_total_mills",   "SCHOOL DISTRICT")
  c_fix <- repair_layer("county",  "cty_re_total_mills",  "COUNTY")

  # Write the repaired panel: same schema as the raw file, effective rates
  # replaced (back to DECIMAL units, as the raw file stores them).
  out <- copy(b)
  out <- merge(out, m_fix[, .(muni_id, year, m_new = eff_new)], by = c("muni_id","year"), all.x = TRUE)
  out <- merge(out, s_fix[, .(sd_id,   year, s_new = eff_new)], by = c("sd_id","year"),   all.x = TRUE)
  out <- merge(out, c_fix[, .(county,  year, c_new = eff_new)], by = c("county","year"),  all.x = TRUE)
  out[!is.na(m_new), muni_eff_rate   := m_new / 100]
  out[!is.na(s_new), sd_eff_rate     := s_new / 100]
  out[!is.na(c_new), county_eff_rate := c_new / 100]
  out[, eff_property_tax_rate := rowSums(cbind(muni_eff_rate, sd_eff_rate, county_eff_rate), na.rm = TRUE)]
  out[, c("m_new","s_new","c_new") := NULL]

  fwrite(out, file.path(RAW_DIR, "multitax", "pa_property_burden_panel_repaired.csv"))
  message("[rebase] wrote data/raw/multitax/pa_property_burden_panel_repaired.csv")

  if (length(audit)) {
    log_dt <- rbindlist(audit, fill = TRUE)
    # Cross-check against the reassessment file (not used as an input).
    ra_path <- file.path(RAW_DIR, "multitax", "pa_county_reassessment.csv")
    if (file.exists(ra_path)) {
      ra <- fread(ra_path)[reassessed_in_panel == TRUE, .(county, file_reassess_year = in_panel_year)]
      log_dt <- merge(log_dt, ra, by = "county", all.x = TRUE)
    }
    fwrite(log_dt, file.path(RESULTS_DIR, "property_rebase_log.csv"))
    message(sprintf("[rebase] audit log: %s corrected unit-years -> data/results/property_rebase_log.csv",
                    format(nrow(log_dt), big.mark = ",")))
    print(log_dt[, .N, by = .(layer, process)][order(layer, process)])
  }
})
