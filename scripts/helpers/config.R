# config.R -- every path the pipeline uses, relative to the repo root.
# here::here() finds the root by climbing up from the working directory to the
# folder holding local_income_tax_migration.Rproj, so scripts work from RStudio
# (with the project open) or from the terminal anywhere inside the repo.

ROOT       <- here::here()
DATA_DIR   <- file.path(ROOT, "data")
SCRIPT_DIR <- file.path(ROOT, "scripts")
TABLE_DIR  <- file.path(ROOT, "tables")
OUT_DIR    <- file.path(ROOT, "figures")

# Data layout (restructured 2026-07-02) ----
# raw/     scraped inputs only -- never written by analysis scripts
# derived/ built panels and crosswalks (.rds) -- written by 2_build_data/1_build_panels.R
# results/ model-output CSVs -- written by stages 02-04
RAW_DIR     <- file.path(DATA_DIR, "raw")
DERIVED_DIR <- file.path(DATA_DIR, "derived")
RESULTS_DIR <- file.path(DATA_DIR, "results")

dir.create(TABLE_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR,     showWarnings = FALSE, recursive = TRUE)
dir.create(DERIVED_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("[config] ROOT = %s\n", ROOT))
