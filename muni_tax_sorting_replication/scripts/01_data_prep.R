#!/usr/bin/env Rscript
# 01_data_prep.R -- build the analysis panels from LOCAL inputs (no network*).
# Needs the raw DCED/NCES files + the LODES from 00_scrape.R (or the data deposit)
# and the vendored geometry data/pa_cousub.rds. *build_data.R falls back to tigris
# (cached) only for PA geometry not already on disk.
# Run from package root:  Rscript scripts/01_data_prep.R
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_run_stage.R"))
scripts <- c(
  "reparse_dced.R",                            # DCED EIT rate panel, keeping SD id/name
  "build_sd_controls.R",                       # Census F-33 -> SD share controls + NCES<->DCED xwalk
  "build_data.R",                              # cousub rate / OD / fiscal panels + block->cousub xwalk
  "build_block_panel.R"                        # block x year panel inheriting rates/shares
)
run_stage("Data prep", scripts, .dir)
