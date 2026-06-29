# build_results_workbook.R
# Collect all estimation results into ONE Excel workbook with an index sheet
# and one sheet per result table (incl. the multi-test pre-trend battery).
# Output: results/all_results.xlsx
# Run q3_unified_pretrend_battery.R first so the battery sheet is current.

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
  c("headline_full_table.csv",            "headline_decomp",   "Binding decomposition: per-pp dose coefficients, all cells x outcomes (HEADLINE)"),
  c("headline_diagnostics.csv",           "headline_diag",     "Headline regression diagnostics (R2, within-R2, N, dep mean)"),
  c("pooled_results.csv",                 "pooled_benchmark",  "Pooled-Increase benchmark (per-pp dose + binary event)"),
  c("headline_es_coefs.csv",              "ES_dose_coefs",     "Realized-dose event study: coefficients by event-time (MAIN ES)"),
  c("headline_es_att.csv",                "ES_dose_ATT",       "Realized-dose ES: post/pre ATT + joint-Wald pre-trend"),
  c("pretrend_battery.csv",               "pretrend_battery",  "MULTIPLE pre-trend tests: joint Wald, individual leads, linear slope, HonestDiD breakdown"),
  c("dcdh_robustness.csv",                "dcdh_robustness",   "Robustness: de Chaisemartin-D'Haultfoeuille (non-negative weights) ES vs linear feols"),
  c("curvature_acrt_feols.csv",           "curvature_acrt",    "Curvature: parametric ACRT(d) dose-response curve (does the per-pp effect bend?)"),
  c("curvature_dcdh_lin_vs_quad.csv",     "curvature_dcdh",    "Curvature: dCDH event study under linear vs quadratic dose model"),
  c("act32_split_cousub.csv",             "act32_split",       "Robustness: pre/post Act-32 enforcement split"),
  c("levels_robustness_decomp.csv",       "rob_levels_decomp", "Robustness: levels vs dose -- decomposition (identical coefs)"),
  c("levels_robustness_pooled.csv",       "rob_levels_pooled", "Robustness: levels vs dose -- aggregate"),
  c("levels_robustness_es.csv",           "rob_levels_ES",     "Robustness: level-tag vs dose-tag ES (why levels fail)"),
  c("eit_levels_ptax_controls_table.csv", "rob_ptax_controls", "Robustness: headline with city/SD/county property-tax level controls"),
  c("eit_levels_es_coefs.csv",            "rob_levelform_ES",  "Diagnostic: level-form ES (placebo comes out significant)"),
  c("proptax_eit_comparison.csv",         "proptax_compare",   "Property tax: per-$1k EIT-vs-property comparison + honest bounds"),
  c("proptax_layer_split.csv",            "proptax_layers",    "Property tax: muni/SD/county layer split"),
  c("eit_level_vs_proptax_crossref.csv",  "eit_vs_proptax",    "EIT level vs property-tax cross-reference")
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
  cat(sprintf("added sheet %-20s (%d x %d)  <- %s\n", sh, nrow(dt), ncol(dt), m[1]))
}
# index sheet, placed first
idx <- rbindlist(idx_rows, fill = TRUE)
addWorksheet(wb, "INDEX"); writeData(wb, "INDEX",
  data.table(workbook = "Do Local Taxes Cause Residential Sorting? -- all results",
             generated = "run build_results_workbook.R to refresh"))
writeData(wb, "INDEX", idx, startRow = 3, headerStyle = hdr)
setColWidths(wb, "INDEX", cols = 1:5, widths = c(20, 8, 8, 38, 90))
worksheetOrder(wb) <- c(length(names(wb)), seq_len(length(names(wb)) - 1L))  # INDEX first

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
cat(sprintf("\nSaved %d sheets + INDEX -> %s\n", added, out_xlsx))
