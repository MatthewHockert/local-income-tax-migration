# config.R -- relative-path configuration for an R replication package.
#
# Resolves the package ROOT (the folder that contains scripts/ data/ tables/
# figures/) from this file's own location, so the scripts run no matter where the
# package is unzipped or which directory R is launched from. Every analysis script
# should source this as its first step:
#
#   .cfg_args <- commandArgs(trailingOnly = FALSE)
#   .cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
#   .cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
#   source(file.path(.cfg_dir, "config.R"))
#
# Then use DATA_DIR / TABLE_DIR / OUT_DIR instead of hardcoded paths.

.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) return(dirname(normalizePath(sub("^--file=", "", fa[1]))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

ROOT       <- normalizePath(file.path(.get_script_dir(), ".."))
DATA_DIR   <- file.path(ROOT, "data")
SCRIPT_DIR <- file.path(ROOT, "scripts")
TABLE_DIR  <- file.path(ROOT, "tables")
OUT_DIR    <- file.path(ROOT, "figures")

dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR,   showWarnings = FALSE, recursive = TRUE)

cat(sprintf("[config] ROOT = %s\n", ROOT))
