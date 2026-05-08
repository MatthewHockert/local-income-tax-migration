# build_paper_tables.R
# Builds LaTeX table fragments from q3_unified output CSVs for direct
# \input{} into paper/main.tex. Each table reports per-cell coefficients
# with full diagnostics (SE, p, R^2, Within R^2, dep mean, N) by column.
#
# Outputs (relative to project root):
#   paper/tables/headline_muni_cell_results.tex     cousub muni cells x outcomes
#   paper/tables/headline_sd_cell_results.tex       SD-level
#   paper/tables/headline_block_cell_results.tex    block level
#   paper/tables/realized_dose_es_pretrend.tex      pre/post mean leads/lags

suppressMessages({ library(data.table) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
out_dir     <- file.path(project_dir, "paper", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
in_dir      <- file.path(project_dir, "data", "q3_unified")

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

# Convert R column names to TeX-safe LaTeX (escape underscores)
texsafe <- function(x) gsub("_", "\\\\_", x)

fmt <- function(x, d = 3) {
  if (is.null(x) || is.na(x) || !is.finite(x)) return("---")
  formatC(x, format = "f", digits = d)
}

# ---------------------------------------------------------------------------
# 1. Cousub muni-side cell coefficient table
# ---------------------------------------------------------------------------
make_cell_table <- function(coef_csv, diag_csv, outcomes, cells, side_prefix,
                             out_path, caption) {
  if (!file.exists(coef_csv)) {
    cat(sprintf("Skipping %s: %s not found\n", out_path, coef_csv))
    return(invisible())
  }
  coefs <- fread(coef_csv)
  diags <- if (file.exists(diag_csv)) fread(diag_csv) else NULL

  # Parse cell label
  if (!"cell" %in% names(coefs)) {
    coefs[, cell_parsed := sub(".*::([^:]+):.*", "\\1", term)]
  } else {
    coefs[, cell_parsed := cell]
  }
  # Filter to the side we want.
  # Output formats observed:
  #   (a) headline cousub: has `term` only (e.g. "treat_cell::Inc-bind:..."
  #                                          or "cell_sd::Inc-bind:...")
  #   (b) sd_level: has `cell` directly, no `term` column, no `side`
  #   (c) block_joint: has `side` ("muni"/"sd") and `cell` columns
  if ("side" %in% names(coefs)) {
    # Format (c): block_joint
    coefs <- coefs[side == side_prefix]
  } else if ("term" %in% names(coefs)) {
    # Format (a): cousub headline — filter by term prefix
    coefs <- coefs[grepl(paste0("^", side_prefix, "::"), term)]
  }
  # Format (b): sd_level — no filter needed (single side)

  # Build wide table: rows = cells, columns = outcomes
  rows <- character(0)
  for (cl in cells) {
    est_row <- paste0("\\textbf{", cl, "} ")
    se_row  <- "             "
    for (o in outcomes) {
      r <- coefs[outcome == o & cell_parsed == cl]
      if (nrow(r) == 1) {
        est_row <- paste0(est_row, " & ", fmt(r$est, 3), stars(r$p))
        se_row  <- paste0(se_row,  " & (", fmt(r$se, 3), ")")
      } else {
        est_row <- paste0(est_row, " & ---")
        se_row  <- paste0(se_row,  " & ")
      }
    }
    rows <- c(rows, paste0(est_row, " \\\\"), paste0(se_row, " \\\\"))
  }

  # Diagnostics rows
  diag_rows <- character(0)
  if (!is.null(diags)) {
    diag_keys <- c("R2"        = "R2",
                   "Within R2" = "Within_R2",
                   "Dep mean"  = "dep_mean",
                   "Dep SD"    = "dep_sd",
                   "N"         = "n")
    for (label in names(diag_keys)) {
      key <- diag_keys[[label]]
      row <- paste0(label, " ")
      for (o in outcomes) {
        d <- diags[outcome == o]
        v <- if (key %in% names(d) && nrow(d) >= 1) d[[key]][1] else NA
        if (label == "N") {
          row <- paste0(row, " & ",
                        format(as.integer(round(v)), big.mark = ",", scientific = FALSE))
        } else {
          row <- paste0(row, " & ", fmt(v, 3))
        }
      }
      diag_rows <- c(diag_rows, paste0(row, " \\\\"))
    }
  }

  col_spec <- paste0("l", paste(rep("c", length(outcomes)), collapse = ""))
  header   <- paste0("Cell & ",
                     paste0("\\texttt{", sapply(outcomes, texsafe), "}",
                            collapse = " & "),
                     " \\\\")

  body <- c(
    "\\begin{table}[H]",
    "  \\centering",
    sprintf("  \\caption{%s}", caption),
    sprintf("  \\label{tab:%s}", gsub("\\.tex$", "", basename(out_path))),
    "  \\small",
    sprintf("  \\begin{tabular}{%s}", col_spec),
    "    \\toprule",
    paste0("    ", header),
    "    \\midrule",
    paste0("    ", rows),
    "    \\midrule",
    paste0("    ", diag_rows),
    "    \\bottomrule",
    "  \\end{tabular}",
    "  \\vspace{0.3em}",
    "  \\begin{minipage}{0.95\\textwidth}",
    sprintf("    \\footnotesize \\textit{Notes:} %s",
            paste("Estimating equation:",
                  "$y = \\alpha_{i \\times s} + \\gamma_{t \\times s} +",
                  "\\sum_c \\beta^M_c \\mathbf{1}\\{cell^M = c\\} \\Delta\\tau^M +",
                  "\\sum_c \\beta^{SD}_c \\mathbf{1}\\{cell^{SD} = c\\} \\Delta\\tau^{SD} +",
                  "X'\\phi + \\varepsilon$.",
                  "Cluster-robust standard errors in parentheses.",
                  "$^{*}p < 0.10$, $^{**}p < 0.05$, $^{***}p < 0.01$.")),
    "  \\end{minipage}",
    "\\end{table}"
  )
  writeLines(body, out_path)
  cat(sprintf("Wrote %s\n", out_path))
}

# Outcome and cell sets per level
muni_outcomes <- c("log_live_and_work","log_in_commuters",
                   "log_total_residents","share_live_in","share_work_outside")
muni_cells    <- c("Inc-bind","Inc-becomes","Inc-nobind",
                   "Dec-bind","Dec-becomes-non","Dec-nobind")

block_outcomes <- c("log_home_count","log_work_count",
                    "home_in_muni_share","share_work_outside")

# ---------------------------------------------------------------------------
# Build the three tables (skip silently if outputs don't exist yet)
# ---------------------------------------------------------------------------
# Cousub headline: muni-side cell coefficients (treat_cell::*)
make_cell_table(
  file.path(in_dir, "headline_full_table.csv"),
  file.path(in_dir, "headline_diagnostics.csv"),
  muni_outcomes, muni_cells, "treat_cell",
  file.path(out_dir, "headline_muni_cell_results.tex"),
  "Cousub-level dose-response: muni resident-rate cells")

# Cousub headline: SD-side cell coefficients (cell_sd::*) from the same
# regression — partial coefficient on SD rate change holding muni-side fixed
make_cell_table(
  file.path(in_dir, "headline_full_table.csv"),
  file.path(in_dir, "headline_diagnostics.csv"),
  muni_outcomes, muni_cells, "cell_sd",
  file.path(out_dir, "headline_muni_sd_side_results.tex"),
  "Cousub-level dose-response: SD resident-rate cells (partial)")

# SD-level model
make_cell_table(
  file.path(in_dir, "sd_level_results.csv"),
  file.path(in_dir, "sd_level_diagnostics.csv"),
  muni_outcomes, muni_cells, "treat_cell",
  file.path(out_dir, "headline_sd_cell_results.tex"),
  "School-district-level dose-response by binding-state cell")

# Block-level model: muni-side and SD-side from the joint regression
make_cell_table(
  file.path(in_dir, "block_joint_results.csv"),
  file.path(in_dir, "block_joint_diagnostics.csv"),
  block_outcomes, muni_cells, "muni",
  file.path(out_dir, "headline_block_muni_results.tex"),
  "Block-level dose-response: muni resident-rate cells")

make_cell_table(
  file.path(in_dir, "block_joint_results.csv"),
  file.path(in_dir, "block_joint_diagnostics.csv"),
  block_outcomes, muni_cells, "sd",
  file.path(out_dir, "headline_block_sd_results.tex"),
  "Block-level dose-response: SD resident-rate cells")

cat("\nDone. Tables written to paper/tables/.\n")
