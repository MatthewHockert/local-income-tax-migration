# _run_stage.R -- shared helper for the NN_*.R stage runners.
# run_stage(name, scripts, dir): run each script as an ISOLATED Rscript subprocess
# (fresh session => no cross-script variable collisions), timing each and logging
# OK/FAIL. Per-script output goes to ../logs/<script>.log. Returns failed scripts.
run_stage <- function(name, scripts, dir) {
  log_dir <- file.path(dirname(dir), "logs"); dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  # run from scripts/ with the basename so Rscript's --file= has no spaces to mangle
  # (an absolute path containing a space, e.g. "Personal Info", becomes "~+~").
  oldwd <- getwd(); on.exit(setwd(oldwd)); setwd(dir)
  cat(sprintf("\n========== STAGE: %s  (%d scripts) ==========\n", name, length(scripts)))
  fails <- character(0)
  for (s in scripts) {
    t0 <- Sys.time(); cat(sprintf("  %-46s ", s)); flush.console()
    lg <- file.path(log_dir, sub("\\.R$", ".log", s))
    rc <- system2("Rscript", shQuote(s), stdout = lg, stderr = lg)
    dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (rc == 0) cat(sprintf("OK   (%4ds)\n", dt))
    else { cat(sprintf("FAIL (rc=%d)  see logs/%s\n", rc, basename(lg))); fails <- c(fails, s) }
  }
  cat(sprintf("---------- %s: %d/%d OK%s ----------\n", name, length(scripts)-length(fails),
      length(scripts), if (length(fails)) paste0("; FAILED: ", paste(fails, collapse=", ")) else ""))
  invisible(fails)
}
