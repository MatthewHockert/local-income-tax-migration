# Download and cache all LODES data locally
# Run once; other scripts load from saved RDS files
# lehdr clears its cache after download, so we save to project data/

library(lehdr)
library(data.table)

source(here::here("scripts", "helpers", "config.R"))
lodes_dir <- file.path(RAW_DIR, "lodes_block")
dir.create(lodes_dir, showWarnings = FALSE)

download_and_save <- function(state, lodes_type, segment, state_part = NULL,
                              years = 2002:2021, suffix = "") {
  fname <- paste0(state, "_", lodes_type, "_", segment, suffix, ".rds")
  fpath <- file.path(lodes_dir, fname)

  if (file.exists(fpath)) {
    cat("Already cached:", fname, "\n")
    return(invisible(NULL))
  }

  cat("Downloading:", fname, "...\n")
  args <- list(
    state = state,
    year = years,
    lodes_type = lodes_type,
    job_type = "JT00",
    segment = segment,
    agg_geo = "block",
    version = "LODES8",
    use_cache = TRUE
  )
  if (!is.null(state_part)) args$state_part <- state_part

  df <- do.call(grab_lodes, args)
  saveRDS(df, fpath)
  cat("Saved:", fname, "(", nrow(df), "rows)\n")
}

# Pennsylvania home-to-work flows (OD), all workers, PA residents only
download_and_save("pa", "od", "S000", state_part = "main", suffix = "_main")

cat("\nAll LODES data cached in", lodes_dir, "\n")

beepr::beep()
