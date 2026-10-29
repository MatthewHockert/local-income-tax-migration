#!/usr/bin/env Rscript
# run_all.R -- full analysis pipeline from the vendored panels:
#   main models -> event studies -> robustness -> figures -> tables.
# Each stage is a self-contained flag-gated script run as an isolated
# subprocess (fresh R session per stage; per-section RUN_X flags inside each).
# Rebuilds every CSV / figure / table the package and paper use.
# (Data prep, 01_data_prep.R, is NOT run here -- it regenerates panels from raw
# inputs + network; the package ships the built panels. To compile the PDF, see
# run_repro.sh or pdflatex in manuscript/.)
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
stages <- c("02_main_models.R", "03_event_studies.R", "04_robustness.R",
            "05_figures.R", "06_tables.R")
# run from scripts/ with the basename so Rscript's --file= has no spaces to mangle
# (an absolute path containing a space, e.g. "Personal Info", becomes "~+~" and
#  breaks the scripts' own config resolution).
oldwd <- getwd(); on.exit(setwd(oldwd)); setwd(.dir)
log_dir <- file.path(dirname(.dir), "logs")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
fails <- character(0)
for (st in stages) {
  t0 <- Sys.time()
  print(sprintf("stage %s ...", st))
  lg <- file.path(log_dir, sub("\\.R$", ".log", st))
  rc <- system2("Rscript", shQuote(st), stdout = lg, stderr = lg)
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  print(sprintf("  %s: %s (%ds)  log: logs/%s", st,
                if (rc == 0) "OK" else sprintf("FAIL rc=%d", rc), dt, basename(lg)))
  if (rc != 0) fails <- c(fails, st)
}
if (length(fails)) {
  print(sprintf("run_all.R finished with failures: %s", paste(fails, collapse = ", ")))
} else {
  print("run_all.R complete. (Run 01_data_prep.R separately to rebuild panels from raw.)")
}
