# run_active.R
# Master pipeline for the ACTIVE paper structure (continuous stacked DiD,
# three-level unified resident-side design). Mirrors the structure in
# paper/outline.md and the script list in CLAUDE.md / README.md.
#
# Legacy scripts are NOT called from here. To run a legacy script
# intentionally, source it directly: `Rscript <name>.R`.
#
# Stages:
#   1. Data construction (muni AFR / SD F-33 / LODES / cousub / block panels)
#   2. Main results (continuous stacked DiD per level)
#   3. Heterogeneity (Act 32, industry, muni type)
#   4. Robustness (realized-dose ES, SA-IW binary ES, tercile, alt-control)
#   5. Paper artifacts (LaTeX tables + figures)
#
# Each stage is gated by a logical flag so stages can be rerun in isolation.

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
setwd(project_dir)

# ---------------------------------------------------------------------------
# Run-stage flags. Set to FALSE to skip.
# ---------------------------------------------------------------------------
RUN_DATA_BUILD     <- TRUE
RUN_MAIN_MUNI      <- TRUE
RUN_MAIN_SD        <- TRUE
RUN_MAIN_BLOCK     <- TRUE
RUN_HET_ACT32      <- TRUE
RUN_HET_INDUSTRY   <- TRUE
RUN_HET_MUNI_TYPE  <- TRUE
RUN_ROB_PRETREND   <- TRUE
RUN_ROB_SA_BINARY  <- TRUE
RUN_ROB_TERCILE    <- TRUE
RUN_ROB_ALTCTRL    <- TRUE
RUN_PAPER_BUILD    <- TRUE

cat("================================================================\n")
cat("ACTIVE PIPELINE: continuous stacked DiD, three-level unified design\n")
cat("================================================================\n\n")

# ---------------------------------------------------------------------------
# 1. Data construction
# ---------------------------------------------------------------------------
if (RUN_DATA_BUILD) {
  cat("[1] Data construction\n")
  source("build_sd_controls.R")     # F-33 -> SD share controls + ID xwalk
  source("build_data.R")            # cousub panel + rate panel + neighbors
  source("build_block_panel.R")     # block panel inheriting cousub rates
}

# ---------------------------------------------------------------------------
# 2. Main results: continuous stacked DiD, per estimating unit
# ---------------------------------------------------------------------------
if (RUN_MAIN_MUNI) {
  cat("\n[2a] Main results: municipality level\n")
  source("q3_unified_full_table.R")     # cousub headline (stacked)
}

if (RUN_MAIN_SD) {
  cat("\n[2b] Main results: school district level\n")
  source("q3_unified_sd_level.R")        # SD-level analog
}

if (RUN_MAIN_BLOCK) {
  cat("\n[2c] Main results: block level\n")
  source("q3_unified_block_joint.R")     # block headline (stacked)
}

# ---------------------------------------------------------------------------
# 3. Heterogeneity
# ---------------------------------------------------------------------------
if (RUN_HET_ACT32) {
  cat("\n[3a] Heterogeneity: pre/post Act 32 (all 3 levels)\n")
  source("q3_unified_act32_3level.R")
}

if (RUN_HET_INDUSTRY) {
  cat("\n[3b] Heterogeneity: industry decomposition\n")
  source("q3_unified_industry.R")        # cousub
  source("q3_unified_industry_sd.R")     # SD (aggregated)
}

if (RUN_HET_MUNI_TYPE) {
  cat("\n[3c] Heterogeneity: muni type (city / borough / township)\n")
  source("q3_unified_heterogeneity.R")
}

# ---------------------------------------------------------------------------
# 4. Robustness: pre-trend ES + binary SA-IW + tercile + alt-control
# ---------------------------------------------------------------------------
if (RUN_ROB_PRETREND) {
  cat("\n[4a] Pre-trend test: realized-dose event study (all 3 levels)\n")
  source("q3_unified_headline_es.R")     # cousub
  source("q3_unified_sd_es.R")           # SD
  source("q3_unified_block_es_full.R")   # block (heavy)
}

if (RUN_ROB_SA_BINARY) {
  cat("\n[4b] Binary Sun-Abraham IW ES (cousub + SD; block-level IW unstable)\n")
  source("q3_unified_sa_binary_es.R")
}

if (RUN_ROB_TERCILE) {
  cat("\n[4c] Dose-magnitude tercile (all 3 levels)\n")
  source("q3_unified_tercile_3level.R")
}

if (RUN_ROB_ALTCTRL) {
  cat("\n[4d] Alt-control (Cengiz clean-in-window; all 3 levels)\n")
  source("q3_unified_altctrl_3level.R")
}

# ---------------------------------------------------------------------------
# 5. Paper artifacts
# ---------------------------------------------------------------------------
if (RUN_PAPER_BUILD) {
  cat("\n[5] Building paper tables and figures\n")
  source("build_industry_table.R")
  source("build_paper_tables.R")
  source("build_industry_figure.R")
}

cat("\n================================================================\n")
cat("DONE. Active pipeline complete.\n")
cat("================================================================\n")
