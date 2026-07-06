#!/usr/bin/env Rscript
# 04_figures.R -- Produce every figure and map. Run AFTER 02+03 (some read their CSVs).
# Outputs -> figures/*.png.
# Run from package root:  Rscript scripts/04_figures.R
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_run_stage.R"))
scripts <- c(
  "build_summary_stats.R",                    # summary tables + rate ECDF/CDF + descriptive figures (also .tex)
  "build_uea_aggregate_figures.R",            # pooled ES + dose-trajectory aggregate figures
  "build_dose_trajectory_inc_becomes.R",      # Inc-becomes dose-trajectory figure
  "build_appendix_es_figures.R",              # unified ribbon-style ES figures (appendix set)
  "build_extra_figures.R",                    # descriptive rate-change scatters (muni+SD, total), spaghetti, hist
  "build_acr_acrt_att_spline.R",              # ns(dose) ATT/ACRT dose-response (pooled, Inc-bind, Inc-becomes)
  "build_feols_es_bycell.R"                   # feols realized-dose ES by group (twin of dcdh_bycell_es, same scale)
)
run_stage("Figures", scripts, .dir)
