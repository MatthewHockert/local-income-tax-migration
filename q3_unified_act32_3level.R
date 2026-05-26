# q3_unified_act32_3level.R
# Pre-/post-Act-32 split of the headline cell x dose response.
#
# COUSUB-ONLY (repo-cleanup, 2026-05): the SD-unit and block-unit splits
# were trimmed out when those estimators were retired. The kept papers
# (UEA_draft.tex, muni_tax_sorting.tex) report only the cousub split.
# The cousub block is unchanged and produces byte-identical
# act32_split_cousub.csv. Prior 3-level version is in git history.
#
# Approach: source the cousub data prep, then split treated munis by
# whether their first event year falls before 2012 or in/after 2012, run
# two separate stacked-continuous regressions on each subset, and compare
# the Inc-cell coefficients.
#
# Outputs:
#   data/q3_unified/act32_split_cousub.csv

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
ACT32_YEAR <- 2012L

run_act32_split <- function(stk, c_col, cell_col, dose_col,
                             outcomes, ctrls, fe_str, cluster_var, level_tag) {
  ctrls_str <- paste(ctrls, collapse = " + ")
  results <- list()
  diagnostics <- list()
  cohort <- stk[, get(c_col)]
  for (label in c("pre", "post")) {
    if (label == "pre") {
      sub <- stk[is.na(get(c_col)) | get(c_col) < ACT32_YEAR]
    } else {
      sub <- stk[is.na(get(c_col)) | get(c_col) >= ACT32_YEAR]
    }
    sub[, (cell_col) := factor(get(cell_col),
                                levels = c("NC","Inc-bind","Inc-becomes",
                                            "Inc-nobind","Dec-bind",
                                            "Dec-becomes-non","Dec-nobind"))]
    fe_cols <- trimws(unlist(strsplit(fe_str, "[+^]")))
    fe_cols <- fe_cols[fe_cols != ""]
    for (o in outcomes) {
      cols <- intersect(c(o, dose_col, cell_col, ctrls,
                           all.vars(cluster_var), fe_cols),
                         names(sub))
      d <- sub[, ..cols]
      d <- d[complete.cases(d)]
      if (nrow(d) == 0) next
      f <- as.formula(paste0(o,
           " ~ i(", cell_col, ", ", dose_col, ", ref='NC') + ",
           ctrls_str, " | ", fe_str))
      m <- tryCatch(feols(f, data = d, cluster = cluster_var),
                     error = function(e) {
                       cat(sprintf("  %s %s err: %s\n", label, o, conditionMessage(e))); NULL })
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
               c("est","se","p"), skip_absent = TRUE)
      ic <- ct[grepl(paste0(cell_col, "::"), term)]
      ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
      ic[, period := label]
      ic[, outcome := o]
      ic[, n := nobs(m)]
      results[[length(results)+1L]] <- ic[, .(period, outcome, cell,
                                                est, se, p, n)]
      dep_vec <- m$residuals + m$fitted.values
      diagnostics[[length(diagnostics)+1L]] <- data.table(
        period   = label, outcome = o,
        n        = nobs(m),
        r2       = round(fitstat(m, "r2",  simplify = TRUE), 4),
        wr2      = round(fitstat(m, "wr2", simplify = TRUE), 4),
        dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
        dep_sd   = round(sd(dep_vec,   na.rm = TRUE), 4))
    }
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, paste0("act32_split_", level_tag, ".csv")))
  diag_dt <- rbindlist(diagnostics, fill = TRUE)
  fwrite(diag_dt, file.path(out_dir, paste0("act32_split_", level_tag, "_diag.csv")))
  cat(sprintf("\n[%s] Act 32 split saved (%d coef rows, %d diag rows).\n",
              level_tag, nrow(out), nrow(diag_dt)))
  invisible(out)
}

# ---------------------------------------------------------------------------
# Cousub level
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("Cousub-level Act 32 split\n")
cat("========================================================\n")
e_c <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_c)
stk_muni <- as.data.table(e_c$stk_muni)
ctrls_c  <- e_c$ctrls
run_act32_split(stk_muni, "c_muni", "treat_cell", "dose_stack",
                c("log_live_and_work","log_in_commuters",
                  "log_total_jobs","share_live_in","share_work_outside"),
                ctrls_c, "stack_muni + stack_year", ~muni, "cousub")

cat("\nDone. Output in data/q3_unified/act32_split_cousub.csv\n")
