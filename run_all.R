#!/usr/bin/env Rscript
# run_all.R
# Reruns every model, figure and table in the paper: Rscript run_all.R
# It skips downloading and building the data (scripts/1_get_data and 2_build_data).

steps <- c("scripts/3_models/1_main_models.R",
           "scripts/3_models/2_event_studies.R",
           "scripts/3_models/3_robustness.R",
           "scripts/4_output/1_figures.R",
           "scripts/4_output/2_tables.R")

setwd(here::here())
dir.create("logs", showWarnings = FALSE)

failed <- character(0)
# each script runs in a fresh R session, with its output saved to logs/
for (s in steps) {
  log_file <- file.path("logs", sub("\\.R$", ".log", basename(s)))
  t0 <- Sys.time()
  rc <- system2("Rscript", shQuote(s), stdout = log_file, stderr = log_file)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  message(sprintf("%-38s %s (%ds)", s, if (rc == 0) "OK" else "FAILED, see log", secs))
  if (rc != 0) failed <- c(failed, s)
}
if (length(failed)) stop("Failed: ", paste(failed, collapse = ", "))
