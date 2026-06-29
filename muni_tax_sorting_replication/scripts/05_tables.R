#!/usr/bin/env Rscript
# 05_tables.R -- Build paper LaTeX tables and the all-results Excel workbook.
# Run AFTER 02+03. Outputs -> tables/*.tex and results/all_results.xlsx.
# Run from package root:  Rscript scripts/05_tables.R
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_run_stage.R"))
scripts <- c(
  "build_paper_tables.R"                      # all LaTeX table fragments -> tables/
)
run_stage("Tables", scripts, .dir)
