# run_active.R
# Master pipeline for the ACTIVE paper structure (continuous stacked DiD,
# three-level unified resident-side design). Mirrors the structure in
# paper/outline.md.
#
# Legacy scripts are NOT called from here. They remain in the repo for
# reference but are not part of the current paper pipeline. To run a
# legacy script intentionally, source it directly:
#   Rscript q3_robustness.R      (single-event TWFE robustness)
#   Rscript q3_stacked.R         (stacked binary DiD)
#   Rscript run_dcdh.R           (DCDH cell-by-cell)
#   etc.
#
# Stages of the active pipeline:
#   1. Data construction (muni / SD / block panels)
#   2. Main results (continuous stacked DiD per level)
#   3. Heterogeneity (Act 32, industry, muni type)
#   4. Robustness (realized-dose ES pre-trend test)
#
# Each stage is gated by a logical flag so individual stages can be
# rerun without redoing the whole pipeline.

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
setwd(project_dir)

# ---------------------------------------------------------------------------
# Run-stage flags. Set to FALSE to skip a stage.
# ---------------------------------------------------------------------------
RUN_DATA_BUILD     <- TRUE
RUN_MAIN_MUNI      <- TRUE
RUN_MAIN_SD        <- TRUE
RUN_MAIN_BLOCK     <- TRUE
RUN_HET_ACT32      <- TRUE
RUN_HET_INDUSTRY   <- TRUE
RUN_HET_MUNI_TYPE  <- TRUE
RUN_ROB_PRETREND   <- TRUE

cat("================================================================\n")
cat("ACTIVE PIPELINE: continuous stacked DiD, three-level unified design\n")
cat("================================================================\n\n")

# ---------------------------------------------------------------------------
# 1. Data construction
# ---------------------------------------------------------------------------
if (RUN_DATA_BUILD) {
  cat("[1] Data construction\n")
  source("build_data.R")           # cousub panel + rate panel + neighbors
  source("build_block_panel.R")    # block panel inheriting cousub rates
  # build_residence_panel.R is the residence-panel builder (already part
  # of build_data via build_city_od_block in current code).
}

# ---------------------------------------------------------------------------
# 2. Main results: continuous stacked DiD, per estimating unit
# ---------------------------------------------------------------------------
if (RUN_MAIN_MUNI) {
  cat("\n[2a] Main results: municipality level\n")
  source("q3_unified_full_table.R")     # unified spec, cousub level
}

if (RUN_MAIN_SD) {
  cat("\n[2b] Main results: school district level\n")
  source("q3_unified_sd_level.R")        # SD-level analog
}

if (RUN_MAIN_BLOCK) {
  cat("\n[2c] Main results: block level\n")
  source("q3_unified_block_joint.R")     # unstacked joint TWFE at block level
  # Stacked block-level main results: q3_unified_block_es_full.R also
  # produces post-event coefficients alongside ES, but as a diagnostic
  # script. The unstacked joint is the cleaner main-results spec.
}

# ---------------------------------------------------------------------------
# 3. Heterogeneity
# ---------------------------------------------------------------------------
if (RUN_HET_ACT32) {
  cat("\n[3a] Heterogeneity: pre/post Act 32\n")
  source("q3_unified_act32.R")           # currently cousub level only
  # TODO: extend Act 32 split to SD and block levels
}

if (RUN_HET_INDUSTRY) {
  cat("\n[3b] Heterogeneity: industry decomposition (services vs non-services)\n")
  source("q3_unified_industry.R")        # job-side outcomes
}

if (RUN_HET_MUNI_TYPE) {
  cat("\n[3c] Heterogeneity: muni type (city / borough / township)\n")
  source("q3_unified_heterogeneity.R")
}

# ---------------------------------------------------------------------------
# 4. Robustness: realized-dose event study (pre-trend diagnostic)
# ---------------------------------------------------------------------------
if (RUN_ROB_PRETREND) {
  cat("\n[4] Pre-trend test: realized-dose event study\n")
  source("q3_unified_headline_es.R")     # cousub realized-dose ES
  source("q3_unified_block_es_full.R")   # block realized-dose ES
  # TODO: SD-level realized-dose ES (analog to be built)
}

cat("\n================================================================\n")
cat("DONE. Active pipeline complete.\n")
cat("================================================================\n")
