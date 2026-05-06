# Pilot: download one PA OD year at block level, build block->cousub crosswalk,
# aggregate to cousub, compare to current tract-aggregated panel.
#
# Goals:
#   1. Confirm cousub coverage jumps from ~58% to ~100%.
#   2. Confirm block-aggregated totals for tract-centroid cousubs are close to
#      the current tract-level totals (sanity check on noise infusion).
#   3. Establish file size / runtime budget for the full redownload.

suppressMessages({
  library(lehdr)
  library(tigris)
  library(sf)
  library(data.table)
  library(dplyr)
})

options(tigris_use_cache = TRUE)

data_dir   <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"
pilot_dir  <- file.path(data_dir, "lodes_pilot")
dir.create(pilot_dir, showWarnings = FALSE)

# 1. Download PA OD main 2019 at block level ----
pilot_file <- file.path(pilot_dir, "pa_od_S000_main_2019_block.rds")
if (!file.exists(pilot_file)) {
  cat("Downloading PA OD main 2019 at block level...\n")
  t0 <- Sys.time()
  od_block <- grab_lodes(
    state       = "pa",
    year        = 2019,
    lodes_type  = "od",
    job_type    = "JT00",
    segment     = "S000",
    state_part  = "main",
    agg_geo     = "block",      # <-- the change
    version     = "LODES8",
    use_cache   = TRUE
  )
  cat(sprintf("  Download time: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
  saveRDS(od_block, pilot_file)
} else {
  cat("Loading cached pilot file...\n")
  od_block <- readRDS(pilot_file)
}
od_block <- as.data.table(od_block)
cat(sprintf("Block-level OD rows: %s\n", format(nrow(od_block), big.mark = ",")))
cat(sprintf("File size on disk:   %.1f MB\n", file.info(pilot_file)$size / 1e6))

# Inspect column names (lehdr v1.1 uses h_geocode / w_geocode at block level)
cat("Columns:", paste(names(od_block), collapse = ", "), "\n")

# 2. Build block -> cousub crosswalk (one-time) ----
xwalk_file <- file.path(data_dir, "pa_block_xwalk.rds")
if (!file.exists(xwalk_file)) {
  cat("\nBuilding PA block -> cousub crosswalk via TIGER 2020 blocks...\n")
  t0 <- Sys.time()
  pa_blocks <- tigris::blocks(state = "PA", year = 2020) %>%
    st_transform(4326) %>%
    select(block_geoid = GEOID20)
  pa_cousub <- readRDS(file.path(data_dir, "pa_cousub.rds")) %>%
    select(cousub_fips = COUSUBFP) %>%
    st_transform(4326)
  # Blocks nest within cousubs in the 2020 Census hierarchy, so a centroid
  # point-in-polygon is exact. ~50x faster than st_intersects(..., largest=TRUE).
  block_centroids <- pa_blocks %>%
    mutate(geometry = st_centroid(geometry))
  xwalk <- st_join(block_centroids, pa_cousub, join = st_within) %>%
    st_drop_geometry() %>%
    as.data.table()
  cat(sprintf("  Crosswalk rows: %s | unique cousubs: %d\n",
              format(nrow(xwalk), big.mark = ","), uniqueN(xwalk$cousub_fips)))
  cat(sprintf("  Build time: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
  saveRDS(xwalk, xwalk_file)
} else {
  xwalk <- readRDS(xwalk_file)
}

# 3. Aggregate block-level OD to cousub-level ----
home_xw <- xwalk[, .(h_block = block_geoid, home_city = cousub_fips)]
work_xw <- xwalk[, .(w_block = block_geoid, work_city = cousub_fips)]

# lehdr at agg_geo='block' returns columns h_geocode, w_geocode
setnames(od_block, c("h_geocode", "w_geocode"), c("h_block", "w_block"),
         skip_absent = TRUE)

od_block <- merge(od_block, home_xw, by = "h_block", all.x = TRUE)
od_block <- merge(od_block, work_xw, by = "w_block", all.x = TRUE)

# Aggregate to (work_city, home_city, year)
od_cousub <- od_block[!is.na(work_city) & !is.na(home_city),
  .(jobs = sum(S000, na.rm = TRUE)),
  by = .(work_city, home_city, year)]

# Build the muni-level panel: total_jobs and live_and_work
muni_panel_block <- od_cousub[, .(
  total_jobs    = sum(jobs),
  live_and_work = sum(jobs[home_city == work_city]),
  share_live_in = sum(jobs[home_city == work_city]) / sum(jobs)
), by = .(work_city, year)]

# 4. Compare to current tract-based panel ----
pa_panel_current <- readRDS(file.path(data_dir, "pa_panel.rds")) %>% as.data.table()
current_2019 <- pa_panel_current[year == 2019,
  .(work_city, total_jobs_old = total_jobs, share_live_in_old = share_live_in)]

# Coverage check
cat("\n--- Coverage comparison ---\n")
cat(sprintf("Cousubs in current 2019 panel:        %d\n", uniqueN(current_2019$work_city)))
cat(sprintf("Cousubs in block-aggregated 2019:     %d\n", uniqueN(muni_panel_block$work_city)))
cat(sprintf("New cousubs recovered:                %d\n",
            uniqueN(muni_panel_block$work_city) - uniqueN(current_2019$work_city)))

# Sanity check on noise infusion: how close are totals for cousubs in BOTH?
both <- merge(muni_panel_block[year == 2019], current_2019, by = "work_city")
both[, ratio := total_jobs / total_jobs_old]
cat("\n--- Total jobs ratio (block-aggregated / tract-aggregated) for shared cousubs ---\n")
print(quantile(both$ratio, c(0, .25, .5, .75, .9, .95, 1), na.rm = TRUE))
cat("\n--- share_live_in difference (block - tract) ---\n")
both[, sli_diff := share_live_in - share_live_in_old]
print(quantile(both$sli_diff, c(0, .25, .5, .75, .9, .95, 1), na.rm = TRUE))

# 5. Project full redownload size ----
cat("\n--- Full redownload projection ---\n")
mb_per_seg_year <- file.info(pilot_file)$size / 1e6
n_segments <- 2 + 5  # OD: S000+SE03 main; RAC: S000+SE01+SE03+SA01+SA03
n_years    <- 18      # 2002-2019
cat(sprintf("Per segment-year: ~%.0f MB\n", mb_per_seg_year))
cat(sprintf("Estimated total:  ~%.1f GB across %d segments * %d years\n",
            mb_per_seg_year * n_segments * n_years / 1000, n_segments, n_years))

cat("\nPilot complete.\n")
