# build_residence_panel.R
# Build the residence-side panel: aggregate block-level OD by HOME cousub.
# For each (home_cousub B, year t) we compute:
#   residents_employed       = total LODES jobs whose home block is in B
#   residents_work_in_own    = jobs where home_block in B and work_block in B
#   residents_work_elsewhere = residents_employed - residents_work_in_own
#   share_work_in_own        = residents_work_in_own / residents_employed
#
# This is the residence-side complement to pa_panel (workplace-side).
# Use case: when home_stack rises, do residents leave / change job
# location to where work-binds (so home stack is irrelevant)?

suppressMessages({library(data.table)})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
max_year    <- 2019

od    <- as.data.table(readRDS(file.path(data_dir, "lodes_block/pa_od_S000_main.rds")))
xw    <- as.data.table(readRDS(file.path(data_dir, "pa_block_xwalk.rds")))
setnames(od, c("h_geocode", "w_geocode"), c("h_block", "w_block"), skip_absent = TRUE)
od[, year := as.integer(year)]
od <- od[year <= max_year]
home_xw <- xw[, .(h_block = block_geoid, home_city = cousub_fips)]
work_xw <- xw[, .(w_block = block_geoid, work_city = cousub_fips)]
od <- merge(od, home_xw, by = "h_block", all.x = TRUE)
od <- merge(od, work_xw, by = "w_block", all.x = TRUE)
od <- od[!is.na(home_city) & !is.na(work_city)]

resid_panel <- od[, .(
  residents_employed     = sum(S000, na.rm = TRUE),
  residents_work_in_own  = sum(S000[home_city == work_city], na.rm = TRUE),
  residents_work_outside = sum(S000[home_city != work_city], na.rm = TRUE)
), by = .(home_city, year)]
resid_panel[, share_work_in_own := residents_work_in_own / residents_employed]
resid_panel[, share_work_outside := 1 - share_work_in_own]
resid_panel[, log_residents := ifelse(residents_employed > 0, log(residents_employed), NA_real_)]
resid_panel[, log_work_in_own := ifelse(residents_work_in_own > 0, log(residents_work_in_own), NA_real_)]
resid_panel[, log_work_outside := ifelse(residents_work_outside > 0, log(residents_work_outside), NA_real_)]

# Merge home rates (resident_eit, sd_eit, total -> home_stack)
rate_panel <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
rate_h <- rate_panel[!is.na(cousub_fips) & year <= max_year,
                     .(home_city = cousub_fips, year,
                       resident_eit, sd_eit, total_eit, muni_nonres_eit,
                       event_type, first_increase_year, rate_change)]
resid_panel <- merge(resid_panel, rate_h, by = c("home_city", "year"), all.x = TRUE)
resid_panel[, home_stack := resident_eit + sd_eit]

cat(sprintf("Residence panel: %s rows, %d unique home cousubs\n",
            format(nrow(resid_panel), big.mark = ","),
            uniqueN(resid_panel$home_city)))

saveRDS(resid_panel, file.path(data_dir, "pa_residence_panel.rds"))
cat("Saved data/pa_residence_panel.rds\n")
