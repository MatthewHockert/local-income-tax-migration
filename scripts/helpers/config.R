# config.R
# Every folder path the scripts use. here::here() finds the repo by looking for
# the .Rproj file, so scripts work from RStudio or the terminal.

ROOT       <- here::here()
DATA_DIR   <- file.path(ROOT, "data")
SCRIPT_DIR <- file.path(ROOT, "scripts")
TABLE_DIR  <- file.path(ROOT, "tables")
OUT_DIR    <- file.path(ROOT, "figures")

# data/raw: downloads. data/derived: built panels. data/results: model output.
RAW_DIR     <- file.path(DATA_DIR, "raw")
DERIVED_DIR <- file.path(DATA_DIR, "derived")
RESULTS_DIR <- file.path(DATA_DIR, "results")

dir.create(TABLE_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR,     showWarnings = FALSE, recursive = TRUE)
dir.create(DERIVED_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("[config] ROOT = %s\n", ROOT))
