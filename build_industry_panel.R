# build_industry_panel.R
# Aggregate LODES WAC (block-level) to cousub-level industry counts.
# Output: data/pa_industry_panel.rds with cousub x year x industry-bucket totals.
#
# LODES WAC NAICS-supersector codes (CNS01-CNS20):
#   CNS01: Agriculture (NAICS 11)
#   CNS02: Mining (21)
#   CNS03: Utilities (22)
#   CNS04: Construction (23)
#   CNS05: Manufacturing (31-33)
#   CNS06: Wholesale (42)
#   CNS07: Retail (44-45)
#   CNS08: Transp/Warehouse (48-49)
#   CNS09: Information (51)
#   CNS10: Finance/Insurance (52)
#   CNS11: Real Estate (53)
#   CNS12: Prof/Scientific/Tech (54)
#   CNS13: Management (55)
#   CNS14: Admin/Support/Waste (56)
#   CNS15: Educational Services (61)
#   CNS16: Health Care (62)
#   CNS17: Arts/Entertainment (71)
#   CNS18: Accommodation/Food (72)
#   CNS19: Other Services (81)
#   CNS20: Public Administration (92)
#
# Buckets used here:
#   consumer  = CNS07 + CNS17 + CNS18 + CNS19  (retail, arts, accom/food, other svcs)
#   tradeable = CNS05 + CNS06 + CNS09 + CNS10 + CNS12 (mfg, wholesale, info, finance, prof svcs)
#   health_edu = CNS15 + CNS16
#   public    = CNS20
#   other     = CNS01-04, 08, 11, 13, 14 (residual)

suppressMessages({library(data.table)})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")

wac <- as.data.table(readRDS(file.path(data_dir, "lodes_block/pa_wac_S000.rds")))
xw  <- as.data.table(readRDS(file.path(data_dir, "pa_block_xwalk.rds")))
setnames(wac, "w_geocode", "w_block", skip_absent = TRUE)
wac[, year := as.integer(year)]
wac <- wac[year <= 2019]

xw_keep <- xw[, .(w_block = block_geoid, work_city = cousub_fips)]
wac <- merge(wac, xw_keep, by = "w_block", all.x = TRUE)
wac <- wac[!is.na(work_city)]

cat(sprintf("PA WAC: %s rows after xwalk\n", format(nrow(wac), big.mark = ",")))
cat(sprintf("Years: %s\n", paste(range(wac$year), collapse = "-")))

consumer_codes  <- c("CNS07", "CNS17", "CNS18", "CNS19")
tradeable_codes <- c("CNS05", "CNS06", "CNS09", "CNS10", "CNS12")
health_edu_codes <- c("CNS15", "CNS16")
public_codes    <- c("CNS20")
other_codes     <- c("CNS01","CNS02","CNS03","CNS04","CNS08","CNS11","CNS13","CNS14")

industry <- wac[, .(
  jobs_total     = sum(C000, na.rm = TRUE),
  jobs_consumer  = rowSums(.SD[, consumer_codes,  with = FALSE], na.rm = TRUE) |> sum(),
  jobs_tradeable = rowSums(.SD[, tradeable_codes, with = FALSE], na.rm = TRUE) |> sum(),
  jobs_health_edu = rowSums(.SD[, health_edu_codes, with = FALSE], na.rm = TRUE) |> sum(),
  jobs_public    = rowSums(.SD[, public_codes,   with = FALSE], na.rm = TRUE) |> sum(),
  jobs_other     = rowSums(.SD[, other_codes,    with = FALSE], na.rm = TRUE) |> sum()
), by = .(work_city, year)]

# Per-bucket aggregation done correctly: sum each muni-year over blocks,
# summing the bucket-component CNS columns first.
# Re-do with explicit summation:
sum_cols_to_bucket <- function(p, bucket_name, cols) {
  p[, (bucket_name) := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
}
wac2 <- copy(wac)
sum_cols_to_bucket(wac2, "consumer",  consumer_codes)
sum_cols_to_bucket(wac2, "tradeable", tradeable_codes)
sum_cols_to_bucket(wac2, "health_edu", health_edu_codes)
sum_cols_to_bucket(wac2, "public",    public_codes)
sum_cols_to_bucket(wac2, "other",     other_codes)

industry <- wac2[, .(
  jobs_total      = sum(C000,       na.rm = TRUE),
  jobs_consumer   = sum(consumer,   na.rm = TRUE),
  jobs_tradeable  = sum(tradeable,  na.rm = TRUE),
  jobs_health_edu = sum(health_edu, na.rm = TRUE),
  jobs_public     = sum(public,     na.rm = TRUE),
  jobs_other      = sum(other,      na.rm = TRUE)
), by = .(work_city, year)]

setnames(industry, "work_city", "muni")

# Compute logs and shares of total
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
industry[, log_jobs_total      := log_pos(jobs_total)]
industry[, log_jobs_consumer   := log_pos(jobs_consumer)]
industry[, log_jobs_tradeable  := log_pos(jobs_tradeable)]
industry[, log_jobs_health_edu := log_pos(jobs_health_edu)]
industry[, log_jobs_public     := log_pos(jobs_public)]
industry[, log_jobs_other      := log_pos(jobs_other)]

industry[, share_consumer   := ifelse(jobs_total > 0, jobs_consumer  / jobs_total, NA_real_)]
industry[, share_tradeable  := ifelse(jobs_total > 0, jobs_tradeable / jobs_total, NA_real_)]
industry[, share_health_edu := ifelse(jobs_total > 0, jobs_health_edu/ jobs_total, NA_real_)]
industry[, share_public     := ifelse(jobs_total > 0, jobs_public    / jobs_total, NA_real_)]

cat(sprintf("\nIndustry panel: %s muni-years, %d unique munis\n",
            format(nrow(industry), big.mark = ","),
            uniqueN(industry$muni)))
cat("\nMean composition (across muni-years, unweighted):\n")
print(industry[, .(mean_consumer   = mean(share_consumer,   na.rm = TRUE),
                   mean_tradeable  = mean(share_tradeable,  na.rm = TRUE),
                   mean_health_edu = mean(share_health_edu, na.rm = TRUE),
                   mean_public     = mean(share_public,     na.rm = TRUE))])

saveRDS(industry, file.path(data_dir, "pa_industry_panel.rds"))
cat("\nSaved data/pa_industry_panel.rds\n")
