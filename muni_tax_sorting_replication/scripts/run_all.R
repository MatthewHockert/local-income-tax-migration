#!/usr/bin/env Rscript
# run_all.R -- full analysis pipeline from the vendored panels:
#   main models -> robustness -> figures -> tables. Each stage is an isolated
#   subprocess. Rebuilds every CSV / figure / table the package and paper use.
#   (Data prep, 01_data_prep.R, is NOT run here -- it regenerates panels from raw
#   inputs + network; the package ships the built panels. To compile the PDF, see
#   run_repro.sh or pdflatex in manuscript/.)
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
stages <- c("02_main_models.R","03_robustness.R","04_figures.R","05_tables.R")
# run from scripts/ with the basename so Rscript's --file= has no spaces to mangle
# (an absolute path containing a space, e.g. "Personal Info", becomes "~+~" and
#  breaks the stage runner's own config resolution).
oldwd <- getwd(); on.exit(setwd(oldwd)); setwd(.dir)
for (st in stages) {
  cat(sprintf("\n##### %s #####\n", st))
  system2("Rscript", shQuote(st))
}
cat("\nrun_all.R complete. (Run 01_data_prep.R separately to rebuild panels from raw.)\n")
