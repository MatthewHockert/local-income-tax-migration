#!/usr/bin/env Rscript
# 02_main_models.R -- Main models: the headline binding decomposition and benchmarks.
# Outputs -> data/q3_unified/*.csv (headline_full_table, pooled, *_es ...).
# Run from package root:  Rscript scripts/02_main_models.R
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_run_stage.R"))
scripts <- c(
  "q3_unified_full_table.R",                  # HEADLINE binding decomposition (Inc-bind etc.), all outcomes
  "q3_unified_pooled.R",                      # pooled-Increase per-pp benchmark
  "q3_unified_headline_es.R",                 # realized-dose event study (main pre-trend diagnostic)
  "q3_unified_pooled_es.R",                   # pooled-Increase event study
  "q3_unified_eit_revenue.R"                  # EIT revenue response (results section)
)
run_stage("Main models", scripts, .dir)
