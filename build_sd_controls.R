# build_sd_controls.R
# Builds school-district-level fiscal share controls parallel to the
# muni AFR shares, using Census Bureau F-33 data downloaded by
# scrape_nces_sd_finance.py.
#
# Inputs:
#   data/pa_sd_finance_raw.csv  (raw F-33 data, PA SDs, FY 2002-2019)
#   data/pa_eit_rate_panel.csv  (DCED SD ID + name)
#
# Outputs:
#   data/pa_sd_controls.rds      one row per (sd_id, year) with share
#                                 controls parallel to muni AFR shares
#   data/pa_sd_id_xwalk.rds      Census NCESID -> DCED sd_id crosswalk

suppressMessages({ library(data.table); library(stringr) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")

# ---------------------------------------------------------------------------
# 1. Load raw F-33 data, coerce numeric, compute SD share controls
# ---------------------------------------------------------------------------
raw <- fread(file.path(data_dir, "pa_sd_finance_raw.csv"))

# F-33 amounts are in thousands of dollars (or sometimes whole dollars,
# depending on year). The shares are unit-invariant, so this doesn't
# matter for control construction.
num_cols <- c("TOTALREV","TFEDREV","TSTREV","TLOCREV","T06",
              "TOTALEXP","TCURINST","TCURSSVC","TCAPOUT","_41F","V33")
for (cc in num_cols) {
  if (cc %in% names(raw)) {
    set(raw, j = cc, value = suppressWarnings(as.numeric(raw[[cc]])))
  }
}
setnames(raw, "_41F", "DEBT_INT_EXP", skip_absent = TRUE)
setnames(raw, "fy", "year")

cat(sprintf("Raw rows: %s\n", format(nrow(raw), big.mark = ",")))

# School level filter: keep K-12 unified districts only (SCHLEV "03" = K-12
# unified; PA has additional codes like "01" elementary, "02" secondary).
# Default: keep everything; flag SCHLEV distribution.
cat("\nSCHLEV distribution:\n")
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

cat(sprintf("\nSD controls panel: %s rows, %d unique SDs\n",
            format(nrow(sd_ctrl), big.mark = ","),
            uniqueN(sd_ctrl$ncesid)))
cat("\nShare control summary (means across SD-years):\n")
print(sd_ctrl[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 3)),
              .SDcols = grep("^share_", names(sd_ctrl), value = TRUE)])

# ---------------------------------------------------------------------------
# 2. Build NCESID -> DCED sd_id crosswalk via SD name matching
# ---------------------------------------------------------------------------
eit <- fread(file.path(data_dir, "pa_eit_rate_panel.csv"))
dced_sd <- unique(eit[!is.na(sd_id), .(sd_id, sd_name)])
cat(sprintf("\nDCED unique SDs: %d\n", nrow(dced_sd)))

# Normalize SD names for matching (same function used in build_block_panel.R)
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
    "stnycreek"= "stonycreek",
    "stony creek" = "stonycreek",
    "south eastern" = "southeastern",
    "north eastern" = "northeastern",
    "north western" = "northwestern",
    "south western" = "southwestern",
    "clarionvalley" = "clarion valley",
    "brothersvalley" = "brothers valley",
    # Common abbreviations
    "strousbg" = "stroudsburg",
    "stroudsbg"= "stroudsburg",
    "kensngtn" = "kensington",
    "kenington"= "kensington",
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
    "wallingfrd"= "wallingford",
    "swarthm"  = "swarthmore",
    "northn"   = "northern",
    "lwr"      = "lower",
    "upr"      = "upper",
    "turkeyft" = "turkeyfoot",
    "turkey foot"= "turkeyfoot",
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
cat(sprintf("\nExact-match: NCESID -> DCED sd_id: %d / %d\n",
            uniqueN(xwalk$ncesid), uniqueN(f33_names$ncesid)))

# Fuzzy fallback for remaining unmatched DCED SDs.
# For each unmatched DCED SD, find the closest F-33 SD by string distance.
unmatched_dced_now <- dced_sd[!sd_id %in% xwalk$sd_id]
unmatched_f33_now  <- f33_names[!ncesid %in% xwalk$ncesid]
if (nrow(unmatched_dced_now) > 0 && nrow(unmatched_f33_now) > 0 &&
    requireNamespace("stringdist", quietly = TRUE)) {
  cat(sprintf("\nFuzzy-matching %d unmatched DCED SDs against %d F-33 candidates...\n",
              nrow(unmatched_dced_now), nrow(unmatched_f33_now)))
  fuzzy_pairs <- list()
  for (i in seq_len(nrow(unmatched_dced_now))) {
    dn <- unmatched_dced_now$sd_name_norm[i]
    if (is.na(dn) || dn == "") next
    dists <- stringdist::stringdist(dn, unmatched_f33_now$sd_name_norm,
                                    method = "jw", p = 0.1)
    best <- which.min(dists)
    if (length(best) == 1 && dists[best] < 0.15) {
      fuzzy_pairs[[length(fuzzy_pairs)+1L]] <- data.table(
        sd_id   = unmatched_dced_now$sd_id[i],
        ncesid  = unmatched_f33_now$ncesid[best],
        sd_name = unmatched_dced_now$sd_name[i],
        sd_name_f33 = unmatched_f33_now$sd_name_f33[best],
        dist    = dists[best])
    }
  }
  if (length(fuzzy_pairs) > 0) {
    fuzzy_dt <- rbindlist(fuzzy_pairs)
    cat(sprintf("Fuzzy matched %d additional DCED SDs (jw < 0.15):\n",
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
  cat("\nstringdist package not available; skipping fuzzy fallback.\n")
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

cat(sprintf("\nFinal matched DCED SDs: %d / %d (%.1f%%)\n",
            uniqueN(xwalk$sd_id), uniqueN(dced_sd$sd_id),
            100 * uniqueN(xwalk$sd_id) / uniqueN(dced_sd$sd_id)))

unmatched_f33 <- setdiff(f33_names$ncesid, xwalk$ncesid)
if (length(unmatched_f33) > 0) {
  cat(sprintf("\nUnmatched F-33 SDs (%d):\n", length(unmatched_f33)))
  print(head(f33_names[ncesid %in% unmatched_f33], 20))
}
unmatched_dced <- setdiff(dced_sd$sd_id, xwalk$sd_id)
if (length(unmatched_dced) > 0) {
  cat(sprintf("\nUnmatched DCED SDs (%d):\n", length(unmatched_dced)))
  print(head(dced_sd[sd_id %in% unmatched_dced], 20))
}

# Save the crosswalk and merge sd_id into the control panel
saveRDS(xwalk[, .(ncesid, sd_name_f33, sd_id, sd_name_dced = sd_name)],
        file.path(data_dir, "pa_sd_id_xwalk.rds"))

sd_ctrl <- merge(sd_ctrl,
                  unique(xwalk[, .(ncesid, sd_id)]),
                  by = "ncesid", all.x = TRUE)

# Drop SD-years without a DCED match (cant join to rate panel)
sd_ctrl_matched <- sd_ctrl[!is.na(sd_id)]
cat(sprintf("\nSD controls (matched to DCED): %s rows, %d unique sd_id\n",
            format(nrow(sd_ctrl_matched), big.mark = ","),
            uniqueN(sd_ctrl_matched$sd_id)))

saveRDS(sd_ctrl_matched, file.path(data_dir, "pa_sd_controls.rds"))
cat("\nSaved data/pa_sd_controls.rds\n")
cat("Saved data/pa_sd_id_xwalk.rds\n")
