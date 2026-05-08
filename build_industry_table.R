# build_industry_table.R
# Build a focused industry-decomposition table for the paper:
# Inc-bind / Inc-becomes / Inc-nobind cell coefficients (muni resident-rate
# side) on industry-bucket outcomes (consumer-facing vs tradeable services
# vs total jobs). Reads data/q3_unified/industry_table_stacked_continuous.csv
# (output from q3_unified_industry.R) which is already in wide etable format
# and re-formats into the per-pp coefficient table the paper needs.

suppressMessages({ library(data.table) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
in_path  <- file.path(project_dir, "data/q3_unified/industry_table_stacked_continuous.csv")
out_path <- file.path(project_dir, "paper/tables/industry_cousub.tex")

a <- fread(in_path)

cells <- c("Inc-bind","Inc-becomes","Inc-nobind",
            "Dec-bind","Dec-becomes-non","Dec-nobind")
muni_cols <- paste0("muni_", cells)

outcomes <- c("log_jobs_total","log_jobs_consumer","log_jobs_tradeable",
               "share_consumer","share_tradeable")
out_lbl  <- c("\\texttt{log\\_jobs\\_total}",
               "\\texttt{log\\_jobs\\_consumer}",
               "\\texttt{log\\_jobs\\_tradeable}",
               "\\texttt{share\\_consumer}",
               "\\texttt{share\\_tradeable}")

a_sub <- a[outcome %in% outcomes]
a_sub[, ord := match(outcome, outcomes)]
setorder(a_sub, ord)

# Each cell already includes stars and SE. We display them as-is.
# Build a transposed table: rows = cells, columns = outcomes
rows <- character(0)
for (cl in cells) {
  col <- paste0("muni_", cl)
  if (!col %in% names(a_sub)) next
  values <- as.character(a_sub[[col]])
  values[is.na(values)] <- "---"
  values[values == ""] <- "---"
  rows <- c(rows, paste0("\\textbf{", cl, "} & ",
                          paste(values, collapse = " & "), " \\\\"))
}

col_spec <- paste0("l", paste(rep("c", length(outcomes)), collapse = ""))
header <- paste0("Cell & ", paste(out_lbl, collapse = " & "), " \\\\")
body <- c(
  "\\begin{table}[H]",
  "  \\centering",
  "  \\caption{Industry decomposition --- cousub level. Coefficients on the muni resident-rate cells from Equation~(\\ref{eq:headline}) re-estimated with industry-bucket outcomes (LODES WAC supersector grouping into consumer-facing services [NAICS 44-45, 71, 72, 81] vs tradeable goods/services [NAICS 31-33, 42, 51, 52, 54]).}",
  "  \\label{tab:industry_cousub}",
  "  \\small",
  sprintf("  \\begin{tabular}{%s}", col_spec),
  "    \\toprule",
  paste0("    ", header),
  "    \\midrule",
  paste0("    ", rows),
  "    \\bottomrule",
  "  \\end{tabular}",
  "\\end{table}"
)
writeLines(body, out_path)
cat(sprintf("Wrote %s (%d cell rows, %d outcomes)\n",
            out_path, length(rows), length(outcomes)))
