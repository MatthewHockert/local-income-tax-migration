# run_active.R
# Master pipeline for the ACTIVE paper structure (continuous stacked DiD,
# MUNICIPALITY / cousub resident-side design). Mirrors the structure in
# paper/outline.md and the script list in CLAUDE.md / README.md.
#
# COUSUB-ONLY (repo-cleanup, 2026-05): the block-as-unit and SD-as-unit
# estimators, the industry decomposition, the muni-type heterogeneity, and
# the SA-binary / tercile / alt-control robustness passes were retired
# (moved to _retired/) because neither paper (UEA_draft.tex,
# muni_tax_sorting.tex) reports them. This pipeline now reproduces only the
# cousub results both papers consume. The honest-DiD sensitivity script is
# kept (muni_tax_sorting cites it); it self-skips its SD section when the
# retired SD prep is absent and computes its block bounds from the kept
# block panel.
#
# Legacy / retired scripts are NOT called from here. To run one
# intentionally, source it directly: `Rscript _retired/<name>.R`.
#
# Stages:
#   1.  Data construction (muni AFR / SD F-33 / LODES / cousub / block panels)
#   1b. Descriptive statistics (summary tables + key figures)
#   2.  Main result: cousub continuous stacked DiD
#   3.  Heterogeneity: pre/post Act 32 (cousub)
#   4.  Robustness: realized-dose ES (cousub) + honest-DiD sensitivity
#   5.  Paper artifacts (LaTeX tables + ES figures)
#
# Each stage is gated by a logical flag so stages can be rerun in isolation.

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
setwd(project_dir)

# ---------------------------------------------------------------------------
# Run-stage flags. Set to FALSE to skip.
# ---------------------------------------------------------------------------
RUN_DATA_BUILD     <- FALSE
RUN_DESCRIPTIVES   <- TRUE
RUN_MAIN_MUNI      <- TRUE
RUN_HET_ACT32      <- TRUE
RUN_ROB_PRETREND   <- TRUE
RUN_ROB_HONESTDID  <- TRUE
RUN_PAPER_BUILD    <- TRUE

cat("================================================================\n")
cat("ACTIVE PIPELINE: continuous stacked DiD, municipality (cousub) design\n")
cat("================================================================\n\n")

# ---------------------------------------------------------------------------
# 1. Data construction
# ---------------------------------------------------------------------------
if (RUN_DATA_BUILD) {
  cat("[1] Data construction\n")
  source("build_sd_controls.R")     # F-33 -> SD share controls + ID xwalk
  source("build_data.R")            # cousub panel + rate panel + neighbors
  source("build_block_panel.R")     # block panel (feeds honest-DiD block bounds)
}

# ---------------------------------------------------------------------------
# 1b. Descriptive statistics (summary tables + key figures)
# ---------------------------------------------------------------------------
if (RUN_DESCRIPTIVES) {
  cat("\n[1b] Descriptive statistics: summary tables + rate ECDF + Act-32 CDF\n")
  source("build_summary_stats.R")
}

# ---------------------------------------------------------------------------
# 2. Main result: continuous stacked DiD at the municipality (cousub) level
# ---------------------------------------------------------------------------
if (RUN_MAIN_MUNI) {
  cat("\n[2] Main result: municipality (cousub) level\n")
  source("q3_unified_full_table.R")     # cousub headline (stacked, muni + SD cells)
  source("q3_unified_pooled.R")         # pooled Inc/Dec benchmark (cousub)
  source("q3_unified_pooled_es.R")      # pooled realized-dose ES (cousub)
}

# ---------------------------------------------------------------------------
# 3. Heterogeneity: pre/post Act 32 (cousub)
# ---------------------------------------------------------------------------
if (RUN_HET_ACT32) {
  cat("\n[3] Heterogeneity: pre/post Act 32 (cousub)\n")
  source("q3_unified_act32_3level.R")
}

# ---------------------------------------------------------------------------
# 4. Robustness: realized-dose ES (cousub) + honest-DiD sensitivity
# ---------------------------------------------------------------------------
if (RUN_ROB_PRETREND) {
  cat("\n[4a] Pre-trend test: realized-dose event study (cousub)\n")
  source("q3_unified_headline_es.R")     # cousub realized-dose ES
}

if (RUN_ROB_HONESTDID) {
  cat("\n[4b] Rambachan-Roth honest-DiD sensitivity (cousub; SD self-skips)\n")
  source("q3_unified_honestdid.R")
}

# ---------------------------------------------------------------------------
# 5. Paper artifacts: LaTeX tables + ES figures
# ---------------------------------------------------------------------------
if (RUN_PAPER_BUILD) {
  cat("\n[5a] Building paper tables\n")
  source("build_paper_tables.R")

  cat("\n[5b] Building paper figures (realized-dose ES)\n")
  source("build_appendix_es_figures.R")   # unified ribbon-style cousub ES figs
}

cat("\n================================================================\n")
cat("DONE. Active (cousub) pipeline complete.\n")
cat("================================================================\n")
