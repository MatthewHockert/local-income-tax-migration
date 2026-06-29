#!/usr/bin/env Rscript
# 00_scrape.R -- OPTIONAL. Download raw data from the internet (needs network).
# Skip this entirely if data/lodes_block/ is already populated (e.g. from the
# data deposit): stages 01+ run from local files and need no network.
# Run from package root:  Rscript scripts/00_scrape.R
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_run_stage.R"))
scripts <- c(
  "download_lodes.R"                           # PA block LODES OD from Census LEHD (~4GB, ~60 min)
)
run_stage("Scrape (optional -- needs internet)", scripts, .dir)
