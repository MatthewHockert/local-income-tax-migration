# q3_cdh_inc_cells.R
# de Chaisemartin & D'Haultfoeuille (2024) DIDmultiplegtDYN as a cross-check
# on the cousub Inc-bind / Inc-becomes / Inc-nobind binding-cell coefficients.
#
# Motivating concern: the stacked DiD locks each muni's cell from its first
# event. Subsequent rate moves accumulate into the dose but don't change
# the cell tag, so the worry is that within-Inc-cell munis with later
# direction-reversal events might bias the estimate. CDH handles this
# event-by-event: at every period a muni's treatment changes, it enters
# a separate switcher group with NC-stable controls drawn from the
# contemporaneous cross-section. Subsequent reversals enter their own
# switcher groups rather than contaminating the original event.
#
# Outputs:
#   data/q3_unified/cdh_inc_bind_lw.csv
#   data/q3_unified/cdh_inc_becomes_lw.csv
#   data/q3_unified/cdh_inc_nobind_lw.csv
#   data/q3_unified/cdh_inc_bind_incomm.csv  ... and same for two other cells
#   data/q3_unified/cdh_compare_stacked.csv  (side-by-side ATT comparison)

if (!requireNamespace("DIDmultiplegtDYN", quietly = TRUE)) {
  install.packages("DIDmultiplegtDYN", repos = "https://cran.r-project.org")
}

suppressMessages({
  library(data.table)
  library(DIDmultiplegtDYN)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified")

# ----------------------------------------------------------------------
# Load the muni-level panel with cell assignments
# ----------------------------------------------------------------------
cat("Loading panel...\n")
e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

# Keep the Inc cells + NC. CDH wants a long muni-year panel with treatment
# = dose Δτ^muni (continuous, accumulating over time) and group = muni.
# A muni's cell determines whether it is in the analysis sample for that
# CDH run; NC munis (cell == "NC") are the controls.

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")

run_cdh <- function(cell_name, outcome, panel, K_pre = 4, K_post = 5) {
  cat(sprintf("\n=== CDH: cell=%s, outcome=%s ===\n", cell_name, outcome))
  keep_cols <- c("muni","year","resident_eit","cell_muni", outcome, muni_ctrls)
  keep_cols <- intersect(keep_cols, names(panel))
  d <- panel[(cell_muni == cell_name | cell_muni == "NC") &
              !is.na(get(outcome)) & !is.na(resident_eit),
              ..keep_cols]
  setnames(d, c(outcome, "resident_eit", "cell_muni"), c("Y","D","cell"))
  d <- d[complete.cases(d)]
  d[, muni := as.integer(factor(muni))]
  cat(sprintf("  N=%s muni-years; treated munis=%d; NC munis=%d (with %d controls)\n",
              format(nrow(d), big.mark=","),
              uniqueN(d[cell == cell_name, muni]),
              uniqueN(d[cell == "NC", muni]),
              length(muni_ctrls)))

  # DIDmultiplegtDYN expects:
  #   df: data.frame with the panel
  #   outcome, group, time, treatment, controls as column-name strings
  #   effects: number of post-event periods
  #   placebo: number of pre-event periods (lead checks)
  res <- tryCatch(
    did_multiplegt_dyn(df = as.data.frame(d),
                        outcome = "Y",
                        group   = "muni",
                        time    = "year",
                        treatment = "D",
                        controls = muni_ctrls,
                        effects  = K_post,
                        placebo  = K_pre,
                        cluster  = "muni",
                        graph_off = TRUE),
    error = function(err) {
      cat(sprintf("  ERROR: %s\n", conditionMessage(err)))
      NULL
    })
  if (is.null(res)) return(NULL)

  # Extract ATT estimates
  effects <- as.data.table(res$results$Effects, keep.rownames = "term")
  placebos <- if (!is.null(res$results$Placebos))
                as.data.table(res$results$Placebos, keep.rownames = "term")
              else NULL
  list(effects = effects, placebos = placebos, raw = res)
}

# ----------------------------------------------------------------------
# Run on the three Inc cells × two clean-pretrend outcomes
# ----------------------------------------------------------------------
outcomes <- c("log_live_and_work", "log_in_commuters")
cells    <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
all_results <- list()

for (cl in cells) {
  for (o in outcomes) {
    r <- run_cdh(cl, o, panel)
    if (is.null(r)) next
    eff <- copy(r$effects); eff[, cell := cl]; eff[, outcome := o]; eff[, type := "effect"]
    if (!is.null(r$placebos)) {
      pl <- copy(r$placebos); pl[, cell := cl]; pl[, outcome := o]; pl[, type := "placebo"]
      all_results[[length(all_results)+1L]] <- rbind(eff, pl, fill = TRUE)
    } else {
      all_results[[length(all_results)+1L]] <- eff
    }
    safe_cl <- gsub("-", "_", tolower(cl))
    safe_o  <- gsub("log_", "", o)
    fname <- sprintf("cdh_%s_%s.csv", safe_cl, safe_o)
    fwrite(rbind(eff, if (!is.null(r$placebos)) r$placebos else NULL, fill = TRUE),
           file.path(out_dir, fname))
    cat(sprintf("  Saved %s\n", fname))
  }
}

# ----------------------------------------------------------------------
# Comparison vs stacked DiD headline
# ----------------------------------------------------------------------
stacked <- fread(file.path(out_dir, "headline_full_table.csv"))
stacked_inc <- stacked[grepl("treat_cell::Inc-", term) & outcome %in% outcomes,
                       .(outcome,
                          cell = sub("treat_cell::(Inc-[^:]+):.*", "\\1", term),
                          stacked_est = est, stacked_se = se, stacked_p = p)]

cdh_dt <- rbindlist(all_results, fill = TRUE)
# CDH "effect" rows have term like "Effect_1", "Effect_2", ... we want the
# average post effect, which corresponds to the row labelled "Av_tot_eff" or similar
# Different versions of the package report differently; collect what's available.
cat("\nCDH results (raw):\n")
print(cdh_dt)
fwrite(cdh_dt, file.path(out_dir, "cdh_inc_cells_all.csv"))

cat("\nStacked DiD comparison:\n")
print(stacked_inc)
fwrite(stacked_inc, file.path(out_dir, "cdh_compare_stacked.csv"))

cat("\nDone. See data/q3_unified/cdh_*.csv\n")
