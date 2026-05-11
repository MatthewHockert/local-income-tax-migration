# build_industry_table.R
# Build a focused industry-decomposition table for the paper:
# Inc-bind / Inc-becomes / Inc-nobind cell coefficients (muni resident-rate
# side) on industry-bucket outcomes (consumer-facing vs tradeable services
# vs total jobs). Reads data/q3_unified/industry_table_stacked_continuous.csv
# (output from q3_unified_industry.R) which is already in wide etable format
# and re-formats into the per-pp coefficient table the paper needs.

suppressMessages({ library(data.table) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
in_path  <- file.path(project_dir, "data/q3_unified/industry_table_stacked_continuous.csv")
out_path <- file.path(project_dir, "paper/tables/industry_cousub.tex")

a <- fread(in_path)

cells <- c("Inc-bind","Inc-becomes","Inc-nobind",
            "Dec-bind","Dec-becomes-non","Dec-nobind")
muni_cols <- paste0("muni_", cells)

outcomes <- c("log_jobs_total","log_jobs_consumer","log_jobs_tradeable",
               "share_consumer","share_tradeable")
out_lbl  <- outcome_label(outcomes, short = TRUE)

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

## Diagnostics block (N, R^2, Within R^2, Dep mean, Dep SD per outcome)
diag_path <- file.path(project_dir, "data/q3_unified/industry_cousub_diag.csv")
diag_rows <- character(0)
if (file.exists(diag_path) && file.size(diag_path) > 0) {
  dd <- fread(diag_path)
  setkey(dd, outcome)
  pick <- function(field, fmt = "%s") {
    sapply(outcomes, function(o) {
      v <- dd[o, on = "outcome"][[field]]
      if (length(v) == 0 || all(is.na(v))) "---" else sprintf(fmt, v)
    })
  }
  fmt_n <- function(x) format(x, big.mark = ",")
  diag_rows <- c(
    paste0("$N$ & ", paste(sapply(pick("n"), function(x) {
      if (x == "---") x else fmt_n(as.integer(x))
    }), collapse = " & "), " \\\\"),
    paste0("$R^2$ & ", paste(pick("R2", "%.3f"), collapse = " & "), " \\\\"),
    paste0("Within $R^2$ & ", paste(pick("Within_R2", "%.3f"), collapse = " & "), " \\\\"),
    paste0("Dep.\\ mean & ", paste(pick("dep_mean", "%.3f"), collapse = " & "), " \\\\"),
    paste0("Dep.\\ SD & ", paste(pick("dep_sd", "%.3f"), collapse = " & "), " \\\\"))
}

col_spec <- paste0("l", paste(rep("c", length(outcomes)), collapse = ""))
header <- paste0("Cell & ", paste(out_lbl, collapse = " & "), " \\\\")
body <- c(
  "\\begin{table}[H]",
  "  \\centering",
  "  \\caption{Industry decomposition --- municipality level.}",
  "  \\label{tab:industry_cousub}",
  "  \\small",
  "  \\resizebox{\\textwidth}{!}{",
  sprintf("  \\begin{tabular}{%s}", col_spec),
  "    \\toprule",
  paste0("    ", header),
  "    \\midrule",
  paste0("    ", rows),
  if (length(diag_rows)) c("    \\midrule", paste0("    ", diag_rows)) else NULL,
  "    \\bottomrule",
  "  \\end{tabular}",
  "  }",
  "  \\vspace{0.3em}",
  "  \\begin{minipage}{0.95\\textwidth}",
  "    \\footnotesize \\textit{Notes:} Coefficients on the muni",
  "    resident-rate cells from the headline equation re-estimated with",
  "    industry-bucket outcomes (LODES WAC supersector grouping into",
  "    consumer-facing services [NAICS 44--45, 71, 72, 81] vs tradeable",
  "    goods/services [NAICS 31--33, 42, 51, 52, 54]). Cluster-robust",
  "    standard errors in parentheses.",
  "    $^{*}p < 0.10$, $^{**}p < 0.05$, $^{***}p < 0.01$.",
  "  \\end{minipage}",
  "\\end{table}"
)
writeLines(body, out_path)
cat(sprintf("Wrote %s (%d cell rows, %d outcomes)\n",
            out_path, length(rows), length(outcomes)))
