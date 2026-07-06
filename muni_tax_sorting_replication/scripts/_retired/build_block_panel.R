# build_block_panel.R
# Block-level panel for the unified-design block-level robustness.
# Each block inherits its cousub's rates and SD assignment directly from
# the existing cousub-level infrastructure -- no new spatial work.
#
# Outputs: data/pa_block_panel.rds (block x year)

suppressMessages({ library(data.table) })

# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
data_dir <- file.path(project_dir, "data")
max_year <- 2019

# 1. Block -> cousub crosswalk
xw <- as.data.table(readRDS(file.path(data_dir, "pa_block_xwalk.rds")))
cat(sprintf("Block-cousub crosswalk: %s blocks\n",
            format(nrow(xw), big.mark = ",")))

# 2. Block-level LODES OD aggregated to block-year home/work counts
cat("Loading block-level LODES OD...\n")
od <- as.data.table(readRDS(file.path(data_dir, "lodes_block/pa_od_S000_main.rds")))
setnames(od, c("h_geocode","w_geocode"), c("h_block","w_block"), skip_absent = TRUE)
od[, year := as.integer(year)]
od <- od[year <= max_year]

# Map each side to muni
od <- merge(od, xw[, .(h_block = block_geoid, home_muni = cousub_fips)],
            by = "h_block", all.x = TRUE)
od <- merge(od, xw[, .(w_block = block_geoid, work_muni = cousub_fips)],
            by = "w_block", all.x = TRUE)
od <- od[!is.na(home_muni) & !is.na(work_muni)]

cat("Aggregating to block-year home and work counts...\n")
home <- od[, .(home_count   = sum(S000, na.rm = TRUE),
               home_in_muni = sum(S000[home_muni == work_muni], na.rm = TRUE)),
            by = .(block_geoid = h_block, year)]
work <- od[, .(work_count = sum(S000, na.rm = TRUE)),
            by = .(block_geoid = w_block, year)]

panel <- merge(home, work, by = c("block_geoid","year"), all = TRUE)
panel[is.na(home_count),   home_count   := 0]
panel[is.na(home_in_muni), home_in_muni := 0]
panel[is.na(work_count),   work_count   := 0]

# Logs and shares
log_pos <- function(x) ifelse(x > 0, log(as.numeric(x)), NA_real_)
panel[, log_home_count    := log_pos(home_count)]
panel[, log_work_count    := log_pos(work_count)]
panel[, home_in_muni_share := ifelse(home_count > 0, home_in_muni / home_count, NA_real_)]
panel[, share_work_outside := 1 - home_in_muni_share]

# 3. Attach muni assignment
panel <- merge(panel, xw[, .(block_geoid, muni = cousub_fips)],
               by = "block_geoid", all.x = TRUE)
panel <- panel[!is.na(muni)]

# 4. Inherit rates from cousub-level rate panel
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
rate_cols <- intersect(c("cousub_fips","municipality_id","year","resident_eit","sd_eit",
                          "total_eit","muni_nonres_eit","event_type"),
                        names(rate))
rate <- unique(rate[, ..rate_cols])
panel <- merge(panel, rate, by.x = c("muni","year"), by.y = c("cousub_fips","year"),
               all.x = TRUE)

# 4b. Attach primary sd_id per muni from raw DCED panel (modal SD across years)
eit_raw <- fread(file.path(data_dir, "pa_eit_rate_panel.csv"))
eit_raw <- eit_raw[!is.na(sd_id), .(municipality_id, sd_id)]
muni_sd <- eit_raw[, .N, by = .(municipality_id, sd_id)][
  order(municipality_id, -N), .SD[1], by = municipality_id][
  , .(municipality_id, sd_id)]
panel <- merge(panel, muni_sd, by = "municipality_id", all.x = TRUE)

# 5. Build six share-based fiscal controls from pa_afr_panel.csv (same recipe
#    as q3_unified_full_table.R) and merge to block panel.
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
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
panel <- merge(panel, fc, by = c("muni","year"), all.x = TRUE)

cat(sprintf("\nBlock panel: %s rows; %s blocks; %d munis; %d SDs (DCED)\n",
            format(nrow(panel), big.mark = ","),
            format(uniqueN(panel$block_geoid), big.mark = ","),
            uniqueN(panel$muni),
            uniqueN(panel$sd_id)))

saveRDS(panel, file.path(data_dir, "pa_block_panel.rds"))
cat("Saved data/pa_block_panel.rds\n")
