#!/usr/bin/env Rscript
# 03_robustness.R -- Every robustness / alternative-specification model.
# Outputs -> data/q3_unified/*.csv and figures/*.png.
# Run from package root:  Rscript scripts/03_robustness.R
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_run_stage.R"))
scripts <- c(
  "q3_unified_pretrend_battery.R",            # multi-test pre-trend battery (Wald/leads/slope/HonestDiD)
  "q3_unified_act32_3level.R",                # pre/post Act 32 enforcement split
  "q3_unified_act47_robustness.R",            # Act 47 distressed-municipality exclusion
  "q3_unified_dcdh_robustness.R",             # de Chaisemartin-D'Haultfoeuille (non-negative weights)
  "q3_unified_curvature_robustness.R"         # ACRT/ATT curvature + dCDH linear-vs-quadratic
)
run_stage("Robustness", scripts, .dir)
