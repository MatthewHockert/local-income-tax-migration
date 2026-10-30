#!/usr/bin/env Rscript
# run_all.R
# Runs the pipeline end to end: Rscript run_all.R
# Each stage has a flag. The data build is off by default because it is a
# one-time step (the LODES flows alone are about 600 MB). Turn it on for a cold build.
# Downloading the raw data (scripts/1_get_data) is always done by hand; see the README.

# Stages ----
RUN_DATA_BUILD    <- FALSE  # data/raw -> data/derived (panels, SD crosswalk, property rates)
RUN_MAIN_MODELS   <- TRUE   # headline decomposition, pooled, EIT revenue, flexible dose
RUN_EVENT_STUDIES <- TRUE   # realized-dose event studies (pre-trend test)
RUN_ROBUSTNESS    <- TRUE   # dCDH, Act 32, Act 47, pre-trend battery, fake timing (dCDH is ~40 min)
RUN_FIGURES       <- TRUE   # every paper figure and the summary tables
RUN_TABLES        <- TRUE   # every paper table and the in-text number checks

# the property build is Python; set PYTHON to use a specific interpreter,
# e.g. PYTHON=~/packaging-test/pkg-test/bin/python Rscript run_all.R
python <- Sys.getenv("PYTHON", "python3")

steps <- c(
  if (RUN_DATA_BUILD)    c("scripts/2_build_data/1_build_panels.R",
                           "scripts/2_build_data/2_build_property_rates.py",
                           "scripts/2_build_data/3_repair_property_rates.R"),
  if (RUN_MAIN_MODELS)   "scripts/3_models/1_main_models.R",
  if (RUN_EVENT_STUDIES) "scripts/3_models/2_event_studies.R",
  if (RUN_ROBUSTNESS)    "scripts/3_models/3_robustness.R",
  if (RUN_FIGURES)       "scripts/4_output/1_figures.R",
  if (RUN_TABLES)        "scripts/4_output/2_tables.R")

setwd(here::here())
dir.create("logs", showWarnings = FALSE)

failed <- character(0)
# each script runs in a fresh session, with its output saved to logs/
for (s in steps) {
  log_file <- file.path("logs", sub("\\.(R|py)$", ".log", basename(s)))
  command <- if (grepl("\\.py$", s)) python else "Rscript"
  t0 <- Sys.time()
  rc <- system2(command, shQuote(s), stdout = log_file, stderr = log_file)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  message(sprintf("%-46s %s (%ds)", s, if (rc == 0) "OK" else "FAILED, see log", secs))
  if (rc != 0) failed <- c(failed, s)
}
if (length(failed)) stop("Failed: ", paste(failed, collapse = ", "))
