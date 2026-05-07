# build_highway_panel.R
# Pull TIGER primary roads (interstates) for PA, intersect with cousubs to
# determine which munis have an interstate passing through them.
#
# Stability check: compare 2010 vs 2020 networks.
# Output:
#   data/pa_highway_panel.rds    -- cousub_fips x year x has_interstate (year-varying)
#   data/pa_highway_static.rds   -- cousub_fips with TIGER 2020 has_interstate
#   data/pa_highway_corridors.rds -- cousub-pair list sharing any interstate

suppressMessages({
  library(sf); library(data.table); library(tigris)
})
options(tigris_use_cache = TRUE)

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")

cousub <- readRDS(file.path(data_dir, "pa_cousub.rds"))
cousub <- st_make_valid(cousub)
cousub$cousub_fips <- cousub$COUSUBFP

# Helper: pull primary roads, filter to interstates, compute has_interstate per cousub
build_year_indicator <- function(yr) {
  cat(sprintf("\n--- TIGER %d ---\n", yr))
  pr <- tryCatch(primary_roads(year = yr),
                 error = function(e) {
                   cat("primary_roads(year=", yr, ") failed:", conditionMessage(e), "\n")
                   NULL
                 })
  if (is.null(pr)) return(NULL)
  pr <- st_make_valid(pr)
  # Match CRS to cousub before any spatial op
  pr <- st_transform(pr, st_crs(cousub))
  # Restrict to PA bounding box for speed (primary_roads is nationwide)
  pa_bbox <- st_bbox(cousub)
  pr_pa <- suppressWarnings(st_crop(pr, pa_bbox))
  # Filter to interstates: RTTYP == "I"
  inter <- pr_pa[pr_pa$RTTYP == "I", ]
  cat(sprintf("Interstate segments in PA bbox: %d\n", nrow(inter)))
  # Spatial intersection: which cousubs have any interstate segment crossing them?
  ix <- st_intersects(cousub, inter, sparse = TRUE)
  has_interstate <- as.integer(lengths(ix) > 0)
  # List of distinct interstates per muni (for corridor analysis)
  interstate_names_per_muni <- sapply(seq_along(ix), function(i) {
    if (length(ix[[i]]) == 0) return("")
    paste(sort(unique(inter$FULLNAME[ix[[i]]])), collapse = "|")
  })
  data.table(cousub_fips = cousub$cousub_fips,
             year = yr,
             has_interstate = has_interstate,
             n_interstates = sapply(ix, length),
             interstates = interstate_names_per_muni)
}

# Pull 2010 and 2020 (or whatever tigris supports for primary_roads)
years_to_check <- c(2010, 2015, 2020)
yr_results <- lapply(years_to_check, build_year_indicator)
yr_results <- yr_results[!sapply(yr_results, is.null)]
hwy_yr <- rbindlist(yr_results)

# Stability check
cat("\n========== Stability check ==========\n")
wide <- dcast(hwy_yr[, .(cousub_fips, year, has_interstate)],
              cousub_fips ~ year, value.var = "has_interstate")
yrs_present <- setdiff(names(wide), "cousub_fips")
cat("Years pulled:", yrs_present, "\n")
if (length(yrs_present) >= 2) {
  early <- yrs_present[1]; late <- yrs_present[length(yrs_present)]
  cat(sprintf("Cousubs with interstate %s: %d\n", early, sum(wide[[early]] == 1, na.rm = TRUE)))
  cat(sprintf("Cousubs with interstate %s: %d\n", late, sum(wide[[late]] == 1, na.rm = TRUE)))
  changed <- wide[wide[[early]] != wide[[late]] & !is.na(wide[[early]]) & !is.na(wide[[late]])]
  cat(sprintf("Cousubs that changed has_interstate between %s and %s: %d\n",
              early, late, nrow(changed)))
  if (nrow(changed) > 0) print(head(changed, 10))
}

saveRDS(hwy_yr, file.path(data_dir, "pa_highway_panel.rds"))
cat("\nSaved data/pa_highway_panel.rds\n")

# Static (TIGER 2020 only) for general use
hwy_static <- hwy_yr[year == max(year), .(cousub_fips, has_interstate, n_interstates, interstates)]
saveRDS(hwy_static, file.path(data_dir, "pa_highway_static.rds"))
cat("Saved data/pa_highway_static.rds (", nrow(hwy_static), "munis,",
    sum(hwy_static$has_interstate), "with interstate)\n")

# Build corridor pairs: muni A and muni B share any interstate
cat("\n--- Building corridor pairs ---\n")
hwy_w_int <- hwy_static[has_interstate == 1L]
# Long form: muni - interstate
muni_int_long <- hwy_w_int[, .(interstate = unlist(strsplit(interstates, "\\|"))),
                            by = cousub_fips]
muni_int_long <- muni_int_long[interstate != ""]
cat(sprintf("Muni-interstate pairs: %d (across %d distinct interstates)\n",
            nrow(muni_int_long), uniqueN(muni_int_long$interstate)))
# Self-join on interstate -> corridor pairs
corridors <- merge(muni_int_long, muni_int_long, by = "interstate",
                    suffixes = c("_a", "_b"), allow.cartesian = TRUE)
corridors <- corridors[cousub_fips_a != cousub_fips_b,
                        .(muni_a = cousub_fips_a, muni_b = cousub_fips_b, interstate)]
corridors <- unique(corridors[, .(muni_a, muni_b)])
cat(sprintf("Corridor pairs: %s (unique unordered: %d)\n",
            format(nrow(corridors), big.mark = ","),
            nrow(corridors) / 2))
saveRDS(corridors, file.path(data_dir, "pa_highway_corridors.rds"))
cat("Saved data/pa_highway_corridors.rds\n")
