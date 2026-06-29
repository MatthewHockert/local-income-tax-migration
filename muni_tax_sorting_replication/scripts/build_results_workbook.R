# build_results_workbook.R
# Collect the paper's result CSVs into ONE Excel workbook with an INDEX sheet and
# one sheet per result table -- including the realized-dose and pooled EVENT-STUDY
# results (destined for the paper). Output: results/all_results.xlsx
# Run after stages 02-03 so the ES + robustness CSVs are current. Missing CSVs are
# skipped, so it is safe to run on a partial pipeline.

suppressMessages({ library(data.table); library(openxlsx) })
# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
in_dir  <- file.path(project_dir, "data", "q3_unified")
out_dir <- file.path(project_dir, "results"); dir.create(out_dir, showWarnings = FALSE)
out_xlsx <- file.path(out_dir, "all_results.xlsx")

# (file in data/q3_unified, sheet name <=31 chars, description)
manifest <- list(
  c("headline_full_table.csv",  "headline_decomp",  "Binding decomposition: per-pp dose coefficients, all cells x outcomes (HEADLINE)"),
  c("headline_diagnostics.csv", "headline_diag",    "Headline regression diagnostics (R2, within-R2, N, dep mean)"),
  c("pooled_results.csv",       "pooled_benchmark", "Pooled-Increase benchmark (per-pp dose + binary event)"),
  c("headline_es_coefs.csv",    "ES_dose_coefs",    "Realized-dose event study: coefficients by event-time (MAIN ES -- for paper)"),
  c("headline_es_att.csv",      "ES_dose_ATT",      "Realized-dose ES: post/pre ATT + joint-Wald pre-trend (for paper)"),
  c("pooled_es_cousub_att.csv", "ES_pooled_ATT",    "Pooled-Increase event study: post/pre ATT (for paper)"),
  c("act32_split_cousub.csv",   "act32_split",      "Robustness: pre/post Act-32 enforcement split"),
  c("dcdh_robustness.csv",      "dcdh_robustness",  "Robustness: de Chaisemartin-D'Haultfoeuille (non-negative weights) ES vs linear feols"),
  c("curvature_acrt_feols.csv", "curvature_acrt",   "Curvature: parametric ACRT(d) dose-response (marginal effect)"),
  c("curvature_att_curve.csv",  "curvature_att",    "Curvature: ATT(d) dose-response (the paper figure fig:curvature_att)"),
  c("pretrend_battery.csv",     "pretrend_battery", "MULTIPLE pre-trend tests: joint Wald, individual leads, linear slope, HonestDiD breakdown")
)

wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom")
idx_rows <- list(); added <- 0L
for (m in manifest) {
  f <- file.path(in_dir, m[1]); sh <- m[2]; desc <- m[3]
  if (!file.exists(f)) { cat(sprintf("skip (missing): %s\n", m[1])); next }
  dt <- fread(f)
  addWorksheet(wb, sh)
  writeData(wb, sh, dt, headerStyle = hdr)
  freezePane(wb, sh, firstRow = TRUE)
  setColWidths(wb, sh, cols = seq_len(ncol(dt)), widths = "auto")
  added <- added + 1L
  idx_rows[[length(idx_rows)+1L]] <- data.table(sheet = sh, rows = nrow(dt),
                                                cols = ncol(dt), source_csv = m[1],
                                                description = desc)
  cat(sprintf("added sheet %-18s (%d x %d)  <- %s\n", sh, nrow(dt), ncol(dt), m[1]))
}
idx <- rbindlist(idx_rows, fill = TRUE)
addWorksheet(wb, "INDEX"); writeData(wb, "INDEX",
  data.table(workbook = "Do Local Taxes Cause Residential Sorting? -- paper results",
             generated = "run build_results_workbook.R to refresh"))
writeData(wb, "INDEX", idx, startRow = 3, headerStyle = hdr)
setColWidths(wb, "INDEX", cols = 1:5, widths = c(20, 8, 8, 38, 90))
worksheetOrder(wb) <- c(length(names(wb)), seq_len(length(names(wb)) - 1L))  # INDEX first

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
cat(sprintf("\nSaved %d sheets + INDEX -> %s\n", added, out_xlsx))
