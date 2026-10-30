#!/usr/bin/env Rscript
# 2_tables.R -- the paper's LaTeX tables, built from data/results/, and the
# in-text number checks. Run after the model scripts.
# Each section is switched by a RUN_* flag. PARKED sections are not in the paper.
# Writes tables/*.tex and data/results/intext_numbers.csv.
# Run: Rscript scripts/4_output/2_tables.R

suppressMessages({
  library(dplyr)   # before data.table so data.table's first/last/between win
  library(data.table)
  library(openxlsx)
})

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

# Flags ----
RUN_PAPER_TABLES   <- TRUE    # all LaTeX table fragments -> tables/
RUN_INTEXT_NUMBERS <- TRUE    # gates re-pinned to the post-property prose, 2026-08-06
RUN_FLEX_DOSE_TABLE <- TRUE   # Appendix A flexible-dose ACR table
RUN_POOLED_ROBUSTNESS_TABLES <- TRUE  # pooled versions of the Section 6 checks
RUN_WORKBOOK       <- FALSE  # PARKED 2026-10-29, not in the paper: all result CSVs -> results/all_results.xlsx

# Paper tables ----
# One coefficient (with stars) and SE per cell x outcome, plus diagnostics.
# Several tables are written that the paper no longer \input's (kept by request).
if (RUN_PAPER_TABLES) local({
  # column order: share first, then log live-and-work
  muni_outcomes <- c("share_live_in", "log_live_and_work", "log_in_commuters", "log_total_jobs")
  cells_main    <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  muni_notes    <- "Stacked-DiD; muni$\\times$stack and year$\\times$stack FE; SE clustered at muni."
  stars_note    <- "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  muni_ctrl_labels <- c(
    share_police    = "police-services share",
    share_streets   = "streets share",
    share_debt_svc  = "debt-service share",
    share_eit       = "EIT-revenue share",
    share_re        = "real-estate-tax share",
    share_intergov  = "intergovernmental-revenue share",
    muni_eff_rate   = "muni property eff.\\ rate (pp)",
    sd_eff_rate     = "SD property eff.\\ rate (pp)",
    county_eff_rate = "county property eff.\\ rate (pp)")

  # ---- readers
  # Coefficients for one side of the joint regression. side is "muni"/"sd" (term
  # prefixes treat_cell:: / cell_sd::) or a prefix itself. Names harmonized to est/se/p.
  read_coefs <- function(coef_csv, side = NULL) {
    if (!file.exists(coef_csv)) return(NULL)
    coefs <- fread(coef_csv)
    if (!"cell" %in% names(coefs) && "term" %in% names(coefs))
      coefs[, cell := sub(".*::([^:]+):.*", "\\1", term)]
    if (!is.null(side)) {
      prefix <- c(muni = "treat_cell", sd = "cell_sd")[side]
      prefix <- if (is.na(prefix)) side else prefix
      coefs <- coefs[grepl(paste0("^", prefix, "::"), term)]
    }
    if (!"est" %in% names(coefs) && "Estimate" %in% names(coefs)) setnames(coefs, "Estimate", "est")
    if (!"se" %in% names(coefs) && "Std. Error" %in% names(coefs)) setnames(coefs, "Std. Error", "se")
    if (!"p"  %in% names(coefs) && "Pr(>|t|)"   %in% names(coefs)) setnames(coefs, "Pr(>|t|)", "p")
    coefs
  }
  # diagnostics with names harmonized to n / r2 / wr2
  read_diag <- function(diag_csv) {
    if (!file.exists(diag_csv)) return(data.table())
    diag <- fread(diag_csv)
    renames <- c(R2 = "r2", Within_R2 = "wr2", N = "n")
    for (old in intersect(names(renames), names(diag))) setnames(diag, old, renames[[old]])
    diag
  }
  # fiscal-control coefficients, long. An SE above 100 marks a near-singular
  # fit (collinear shares), so those rows print as missing.
  read_controls_long <- function(csv_path, term_filter = NULL) {
    if (!file.exists(csv_path) || file.size(csv_path) == 0) return(NULL)
    controls <- fread(csv_path)
    if ("Estimate" %in% names(controls) && !"est" %in% names(controls)) setnames(controls, "Estimate", "est")
    if ("Std. Error" %in% names(controls) && !"se" %in% names(controls)) setnames(controls, "Std. Error", "se")
    if ("Pr(>|t|)" %in% names(controls) && !"p" %in% names(controls)) setnames(controls, "Pr(>|t|)", "p")
    if (!all(c("outcome", "term", "est", "se", "p") %in% names(controls))) return(NULL)
    if (!is.null(term_filter)) controls <- controls[term %in% term_filter]
    controls[!is.na(se) & abs(se) > 100, `:=`(est = NA_real_, se = NA_real_, p = NA_real_)]
    controls[, .(outcome, term, est, se, p)]
  }

  # ---- shared pieces of a table
  # the two rows for one cell (estimates with stars, then SEs); NULL if every column is empty
  cell_rows <- function(coefs, cell_name, outcomes) {
    strings <- vapply(outcomes, function(outcome_name) {
      row <- filter(coefs, cell == cell_name, outcome == outcome_name)
      if (nrow(row) == 0) c("---", "") else c(fmt_coef(row$est[1], row$p[1]), fmt_se(row$se[1]))
    }, character(2))
    if (all(strings[1, ] == "---")) return(NULL)
    c(paste0("\\textbf{", cell_name, "} & ", paste(strings[1, ], collapse = " & "), " \\\\"),
      paste0("                  & ", paste(strings[2, ], collapse = " & "), " \\\\"))
  }
  # estimate-with-stars and SE strings for one row; dashes when missing or non-finite
  coef_pair <- function(row) {
    if (nrow(row) == 0 || is.na(row$est[1]) || !is.finite(row$est[1])) c("---", "")
    else c(fmt_coef(row$est[1], row$p[1]), fmt_se(row$se[1]))
  }
  # two rows per fiscal control; controls with no estimate anywhere are skipped
  control_rows <- function(controls, outcomes, ctrl_labels) {
    unlist(lapply(unique(controls$term), function(control_term) {
      strings <- vapply(outcomes, function(outcome_name) {
        row <- filter(controls, outcome == outcome_name, term == control_term)
        if (nrow(row) == 0 || is.na(row$est[1])) c("---", "")
        else c(fmt_coef(row$est[1], row$p[1]), fmt_se(row$se[1]))
      }, character(2))
      if (all(strings[1, ] == "---")) return(NULL)
      label <- if (control_term %in% names(ctrl_labels)) ctrl_labels[[control_term]] else gsub("_", "\\\\_", control_term)
      c(paste0(label, " & ", paste(strings[1, ], collapse = " & "), " \\\\"),
        paste0("                  & ", paste(strings[2, ], collapse = " & "), " \\\\"))
    }))
  }
  # one diagnostics value per outcome: N with commas, anything else to 3 decimals
  diag_values <- function(diag, field, outcomes) {
    vapply(outcomes, function(outcome_name) {
      row <- filter(diag, outcome == outcome_name)
      if (nrow(row) == 0 || !(field %in% names(row)) || is.na(row[[field]][1])) return("---")
      if (field == "n") format(as.integer(row[[field]][1]), big.mark = ",")
      else formatC(row[[field]][1], format = "f", digits = 3)
    }, character(1), USE.NAMES = FALSE)
  }
  # tabular inside a [H] table, scaled to the text width at 3+ columns, notes underneath
  table_shell <- function(header, body, caption, label_id, notes, ncols, row_spacing = "0.9") {
    tabular <- c(
      paste0("  \\begin{tabular}{", paste0("l", strrep("c", ncols)), "}"),
      "    \\toprule", paste0("    ", header), "    \\midrule",
      paste0("    ", body), "    \\bottomrule", "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    c("\\begin{table}[H]", "  \\centering",
      paste0("  \\caption{", caption, "}"), paste0("  \\label{", label_id, "}"),
      "  \\footnotesize", paste0("  \\renewcommand{\\arraystretch}{", row_spacing, "}"), tabular,
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", notes),
      "  \\end{minipage}", "\\end{table}")
  }

  # One side of the headline regression, via build_pub_table(), with each
  # cell's own event-study pre-trend p under its coefficient.
  write_cell_table <- function(coef_csv, diag_csv, side, outcomes, cells,
                               caption, label_id, out_name, notes = character(0)) {
    coefs <- read_coefs(coef_csv, side = side)
    if (is.null(coefs) || nrow(coefs) == 0) {
      print(sprintf("Skipping %s: source CSV missing or empty", out_name))
      return(invisible())
    }
    pretrend <- tryCatch(fread(file.path(RESULTS_DIR, "headline_es_att.csv"))[, .(cell, outcome, wald_pre_p)],
                         error = function(e) NULL)
    writeLines(build_pub_table(coefs_dt = coefs, diag_dt = read_diag(diag_csv), pretrend_dt = pretrend,
                               outcomes = outcomes, cells = cells, caption = caption,
                               label = label_id, short_labels = TRUE, notes = notes),
               file.path(TABLE_DIR, out_name))
  }

  # Muni-side and SD-side cells of the joint regression as two panels, with
  # the fiscal controls and diagnostics (N, R2, within R2, optional Wald row).
  write_merged_full_spec_table <- function(coef_csv, diag_csv, ctrl_csv, ctrl_labels,
                                           outcomes, cells, caption, label_id, out_name,
                                           notes = character(0), wald_dt = NULL, diag_dt_override = NULL) {
    coefs_muni <- read_coefs(coef_csv, side = "muni")
    coefs_sd   <- read_coefs(coef_csv, side = "sd")
    if (is.null(coefs_muni) || nrow(coefs_muni) == 0) {
      print(sprintf("Skipping %s: source CSV missing", out_name)); return(invisible())
    }
    diag     <- if (!is.null(diag_dt_override)) diag_dt_override else read_diag(diag_csv)
    controls <- read_controls_long(ctrl_csv, term_filter = names(ctrl_labels))
    ncols <- length(outcomes)
    multicol <- paste0("\\multicolumn{", ncols + 1, "}{l}")

    controls_block <- character(0)
    if (!is.null(controls) && nrow(controls) > 0)
      controls_block <- c("\\midrule", paste0(multicol, "{\\textit{Fiscal controls}} \\\\"),
                          control_rows(controls, outcomes, ctrl_labels))

    diag_block <- character(0)
    if (nrow(diag) > 0) {
      wald_row <- character(0)
      if (!is.null(wald_dt) && nrow(wald_dt) > 0) {
        wald_values <- vapply(outcomes, function(outcome_name) {
          row <- filter(wald_dt, outcome == outcome_name)
          if (nrow(row) == 0 || is.na(row$wald_p[1])) "---" else formatC(row$wald_p[1], format = "f", digits = 3)
        }, character(1))
        wald_row <- paste0("Joint Wald $p$ (leads) & ", paste(wald_values, collapse = " & "), " \\\\")
      }
      diag_block <- c("\\midrule", paste0(multicol, "{\\textit{Diagnostics}} \\\\"),
        paste0("$N$           & ", paste(diag_values(diag, "n",   outcomes), collapse = " & "), " \\\\"),
        paste0("$R^2$         & ", paste(diag_values(diag, "r2",  outcomes), collapse = " & "), " \\\\"),
        paste0("Within $R^2$  & ", paste(diag_values(diag, "wr2", outcomes), collapse = " & "), " \\\\"),
        wald_row)
    }

    body <- c(
      paste0(multicol, "{\\textit{Muni resident rate ($\\tau^M$)}} \\\\"),
      unlist(lapply(cells, function(cell_name) cell_rows(coefs_muni, cell_name, outcomes))),
      "\\midrule",
      paste0(multicol, "{\\textit{School district rate ($\\tau^{SD}$)}} \\\\"),
      unlist(lapply(cells, function(cell_name) cell_rows(coefs_sd, cell_name, outcomes))),
      controls_block, diag_block)
    header <- paste0("Cell & ", paste(outcome_label(outcomes, short = TRUE), collapse = " & "), " \\\\")
    notes_all <- paste(c(notes,
      "Both rate-setters enter the same joint regression; the table reports both sets of cell$\\times$dose coefficients with the shared fiscal controls.",
      stars_note), collapse = " ")
    writeLines(table_shell(header, body, caption, label_id, notes_all, ncols),
               file.path(TABLE_DIR, out_name))
  }

  # ---- headline tables
  write_cell_table(
    coef_csv = file.path(RESULTS_DIR, "headline_full_table.csv"),
    diag_csv = file.path(RESULTS_DIR, "headline_diagnostics.csv"),
    side = "treat_cell", outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: muni resident-rate cells",
    label_id = "tab:headline_muni_cell_results", out_name = "headline_muni_cell_results.tex",
    notes = muni_notes)
  write_cell_table(
    coef_csv = file.path(RESULTS_DIR, "headline_full_table.csv"),
    diag_csv = file.path(RESULTS_DIR, "headline_diagnostics.csv"),
    side = "cell_sd", outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: SD resident-rate cells (partial)",
    label_id = "tab:headline_muni_sd_side_results", out_name = "headline_muni_sd_side_results.tex",
    notes = c(muni_notes, "SD-side cells from the same regression as the muni-side table."))
  # The paper's headline table. No Wald row: the test covers only Inc-bind, so
  # one row under six coefficients would mislead; the battery has every cell.
  write_merged_full_spec_table(
    coef_csv = file.path(RESULTS_DIR, "headline_full_table.csv"),
    diag_csv = file.path(RESULTS_DIR, "headline_diagnostics.csv"),
    ctrl_csv = file.path(RESULTS_DIR, "headline_full_table.csv"),
    ctrl_labels = muni_ctrl_labels, outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: muni and SD resident-rate cells (joint regression)",
    label_id = "tab:headline_muni_combined", out_name = "headline_muni_combined.tex",
    notes = c(muni_notes,
              "Realized-dose pre-trend tests for every cell and outcome are reported in the pre-trend battery."))

  # ---- Act 47 exclusion, in the headline format (from 3_robustness.R)
  act47_path <- file.path(RESULTS_DIR, "act47_full_table.csv")
  if (file.exists(act47_path)) {
    write_merged_full_spec_table(
      coef_csv = act47_path, diag_csv = "", ctrl_csv = act47_path,
      ctrl_labels = muni_ctrl_labels, outcomes = muni_outcomes, cells = cells_main,
      caption  = "Inc-bind effect excluding Hazleton, the one Act 47 designated municipality",
      label_id = "tab:act47_full_results", out_name = "act47_full_results.tex",
      notes = c(muni_notes, "Leave-one-out on the Inc-bind cell, dropping Hazleton. Identical specification and dose source to the headline table, so the cells other than Inc-bind reproduce it."),
      diag_dt_override = unique(fread(act47_path)[, .(outcome, n, r2, wr2, dep_mean)]))
  }

  # ---- Act 32 split, full format: one table per period
  act32_path <- file.path(RESULTS_DIR, "act32_full_cousub.csv")
  if (file.exists(act32_path)) {
    act32 <- fread(act32_path)
    for (period_name in c("pre", "post")) {
      period_rows <- filter(act32, period == period_name)
      if (nrow(period_rows) == 0) next
      writeLines(build_pub_table(
        coefs_dt = period_rows %>%
          filter(grepl("^treat_cell::", term)) %>%
          transmute(outcome, cell = sub("^.*::([^:]+):.*$", "\\1", term), est, se, p),
        diag_dt  = distinct(period_rows, outcome, n, r2, wr2, dep_mean),
        outcomes = muni_outcomes, cells = cells_main,
        caption  = sprintf("Act 32 split, %s-enforcement cohorts (first event %s 2012): full specification",
                           period_name, ifelse(period_name == "pre", "before", "in/after")),
        label    = sprintf("tab:act32_full_%s", period_name),
        controls_dt = period_rows %>% filter(term %in% names(muni_ctrl_labels)) %>%
          select(outcome, term, est, se, p),
        control_labels = muni_ctrl_labels,
        notes = c(muni_notes,
                  if (period_name == "pre") "Inc-nobind is absent pre-2012: 11 municipalities are in the cell, but their dose term is collinear with the stack fixed effects on the thin pre-enforcement subsample and is dropped by the estimator.")),
        file.path(TABLE_DIR, sprintf("act32_full_%s.tex", period_name)))
    }
  }

  # ---- Act 32 split, summary format: each cell with a pre and a post row
  write_act32_table <- function(coef_csv, diag_csv, outcomes, cells, caption, label_id, out_name) {
    if (!file.exists(coef_csv) || file.size(coef_csv) == 0) {
      print(sprintf("Skipping Act32 %s", out_name)); return(invisible())
    }
    coefs <- fread(coef_csv)
    diag  <- if (!is.null(diag_csv) && file.exists(diag_csv)) fread(diag_csv) else NULL
    ncols <- length(outcomes)
    periods <- c("Pre-Act 32" = "pre", "Post-Act 32" = "post")
    blank_cells <- paste(rep("&", ncols), collapse = " ")

    body <- character(0)
    dropped <- character(0)
    for (cell_name in cells) {
      cell_lines <- character(0)
      for (period_label in names(periods)) {
        strings <- vapply(outcomes, function(outcome_name) {
          coef_pair(filter(coefs, cell == cell_name, period == periods[[period_label]], outcome == outcome_name))
        }, character(2))
        if (all(strings[1, ] == "---")) next
        cell_lines <- c(cell_lines,
          paste0("\\quad ", period_label, " & ", paste(strings[1, ], collapse = " & "), " \\\\"),
          paste0("                & ", paste(strings[2, ], collapse = " & "), " \\\\"))
      }
      if (!length(cell_lines)) { dropped <- c(dropped, cell_name); next }
      body <- c(body, paste0("\\textbf{", cell_name, "} ", blank_cells, " \\\\"), cell_lines)
    }
    # two regressions per outcome, so diagnostics for each period
    if (!is.null(diag)) {
      body <- c(body, "\\midrule")
      for (period_label in names(periods)) {
        period_diag <- filter(diag, period == periods[[period_label]])
        body <- c(body, paste0("\\textit{", period_label, "} ", blank_cells, " \\\\"),
          paste0("\\quad $N$ & ",          paste(diag_values(period_diag, "n",   outcomes), collapse = " & "), " \\\\"),
          paste0("\\quad $R^2$ & ",        paste(diag_values(period_diag, "r2",  outcomes), collapse = " & "), " \\\\"),
          paste0("\\quad Within $R^2$ & ", paste(diag_values(period_diag, "wr2", outcomes), collapse = " & "), " \\\\"))
      }
    }
    notes <- paste(c("Cluster-robust standard errors in parentheses.",
                     "Treated cohorts split at first event year $c < 2012$ (Pre-Act~32) vs $c \\geq 2012$ (Post-Act~32).",
                     if (length(dropped) > 0) paste0("Cells omitted (no estimable coefficients): ", paste(dropped, collapse = ", "), "."),
                     stars_note), collapse = " ")
    header <- paste0("Outcome & ", paste(outcome_label(outcomes, short = TRUE), collapse = " & "), " \\\\")
    writeLines(table_shell(header, body, caption, label_id, notes, ncols, row_spacing = "0.85"),
               file.path(TABLE_DIR, out_name))
  }
  write_act32_table(file.path(RESULTS_DIR, "act32_split_cousub.csv"),
                    file.path(RESULTS_DIR, "act32_split_cousub_diag.csv"),
                    muni_outcomes, cells_main,
                    "Pre-/post-Act-32 split --- municipality level",
                    "tab:act32_cousub", "act32_cousub.tex")

  # ---- pooled specification: the Inc coefficient for each rate setter, with
  # fiscal controls and the pooled event study's Wald p in the per-pp version
  write_pooled_table <- function(spec_mode, outcomes, caption, label_id, out_name, notes = character(0)) {
    results_path <- file.path(RESULTS_DIR, "pooled_results.csv")
    diag_path    <- file.path(RESULTS_DIR, "pooled_diagnostics.csv")
    if (!file.exists(results_path)) {
      print(sprintf("Skipping %s: pooled_results.csv missing", out_name)); return(invisible())
    }
    results <- filter(fread(results_path), level == "cousub", mode == spec_mode)
    diag <- if (file.exists(diag_path)) filter(fread(diag_path), level == "cousub", mode == spec_mode) else data.table()
    if (nrow(results) == 0) { print(sprintf("No rows for cousub/%s", spec_mode)); return(invisible()) }
    ncols <- length(outcomes)
    side_labels <- c(muni = "Municipality rate ($\\tau^{M}$)", sd = "School district rate ($\\tau^{SD}$)")

    body <- character(0)
    for (side_name in names(side_labels)) {
      strings <- vapply(outcomes, function(outcome_name) {
        coef_pair(filter(results, side == side_name, direction == "Inc", outcome == outcome_name))
      }, character(2))
      body <- c(body,
        paste0("\\textbf{", side_labels[[side_name]], "} ", paste(rep("&", ncols), collapse = " "), " \\\\"),
        paste0("\\quad $\\Delta\\tau > 0$ (Inc) & ", paste(strings[1, ], collapse = " & "), " \\\\"),
        paste0("                       & ", paste(strings[2, ], collapse = " & "), " \\\\"))
    }
    if (spec_mode == "per_pp") {
      controls <- read_controls_long(file.path(RESULTS_DIR, "pooled_full_table.csv"),
                                     term_filter = names(muni_ctrl_labels))
      if (!is.null(controls) && nrow(controls) > 0)
        body <- c(body, "\\midrule", paste0("\\multicolumn{", ncols + 1, "}{l}{\\textit{Fiscal controls}} \\\\"),
                  control_rows(controls, outcomes, muni_ctrl_labels))
    }
    if (nrow(diag) > 0) {
      body <- c(body, "\\midrule",
        paste0("$N$ & ",          paste(diag_values(diag, "n",   outcomes), collapse = " & "), " \\\\"),
        paste0("$R^2$ & ",        paste(diag_values(diag, "r2",  outcomes), collapse = " & "), " \\\\"),
        paste0("Within $R^2$ & ", paste(diag_values(diag, "wr2", outcomes), collapse = " & "), " \\\\"))
      es_summary_path <- file.path(RESULTS_DIR, "pooled_es_cousub_att.csv")
      if (spec_mode == "per_pp" && file.exists(es_summary_path)) {
        es_summary <- filter(fread(es_summary_path), direction == "Inc")
        wald_values <- vapply(outcomes, function(outcome_name) {
          row <- filter(es_summary, outcome == outcome_name)
          if (nrow(row) == 0 || is.na(row$wald_pre_p[1])) "---" else formatC(row$wald_pre_p[1], format = "f", digits = 3)
        }, character(1))
        body <- c(body, paste0("Joint Wald $p$ (leads) & ", paste(wald_values, collapse = " & "), " \\\\"))
      }
    }
    notes_all <- paste(c(notes,
      "Cluster-robust standard errors in parentheses. Each regression includes both rate-setters' direction-pooled Inc treatments jointly; the reported coefficients are $\\beta^M_{\\text{Inc}}$ and $\\beta^{SD}_{\\text{Inc}}$.",
      stars_note), collapse = " ")
    header <- paste0("Outcome & ", paste(outcome_label(outcomes, short = TRUE), collapse = " & "), " \\\\")
    writeLines(table_shell(header, body, caption, label_id, notes_all, ncols, row_spacing = "0.85"),
               file.path(TABLE_DIR, out_name))
  }
  write_pooled_table("per_pp", muni_outcomes,
                     "Pooled Increase headline --- municipality level (per pp)",
                     "tab:pooled_per_pp_cousub", "pooled_per_pp_cousub.tex")
  write_pooled_table("binary", muni_outcomes,
                     "Pooled Increase headline --- municipality level (binary event)",
                     "tab:pooled_binary_cousub", "pooled_binary_cousub.tex",
                     notes = "Binary post-treatment indicator: 1 in the event year and after.")

  # ---- event-study summaries: post mean, pre mean and Wald p per row
  fmt_wald_p <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("---")
    if (p < 1e-4) return("$<\\!0.0001$")
    formatC(p, format = "f", digits = 4)
  }
  write_es_summary <- function(att_csv, row_col, row_label, row_levels, outcomes,
                               caption, label_id, out_name, extra_notes = character(0)) {
    if (!file.exists(att_csv) || file.size(att_csv) == 0) {
      print(sprintf("Skipping %s: %s missing", out_name, att_csv)); return(invisible())
    }
    es_summary <- fread(att_csv)
    if ("att_post_per_pp" %in% names(es_summary)) setnames(es_summary, "att_post_per_pp", "att_post")
    if ("att_pre_per_pp"  %in% names(es_summary)) setnames(es_summary, "att_pre_per_pp",  "att_pre")
    ncols <- length(outcomes)

    body <- character(0)
    dropped <- character(0)
    for (level_name in row_levels) {
      strings <- vapply(outcomes, function(outcome_name) {
        row <- filter(es_summary, .data[[row_col]] == level_name, outcome == outcome_name)
        if (nrow(row) == 0) return(rep("---", 5))
        c(fmt_est(row$att_post[1]), fmt_se(row$att_post_se[1]), fmt_est(row$att_pre[1]),
          fmt_se(row$att_pre_se[1]), if ("wald_pre_p" %in% names(row)) fmt_wald_p(row$wald_pre_p[1]) else "---")
      }, character(5))
      if (all(strings[1, ] == "---" & strings[2, ] == "---")) { dropped <- c(dropped, level_name); next }
      body <- c(body,
        paste0("\\textbf{", level_name, "} (post) & ", paste(strings[1, ], collapse = " & "), " \\\\"),
        paste0("                  & ", paste(strings[2, ], collapse = " & "), " \\\\"),
        paste0("\\textbf{", level_name, "} (pre) & ", paste(strings[3, ], collapse = " & "), " \\\\"),
        paste0("                  & ", paste(strings[4, ], collapse = " & "), " \\\\"),
        paste0("\\textbf{", level_name, "} Wald $p$ & ", paste(strings[5, ], collapse = " & "), " \\\\"))
    }
    # a diagnostics row only when the field has a value (ES summaries carry N but not R2)
    diag_line <- function(field, label_text) {
      values <- diag_values(es_summary, field, outcomes)
      if (all(values == "---")) character(0) else paste0(label_text, " & ", paste(values, collapse = " & "), " \\\\")
    }
    diag_rows <- character(0)
    if (any(c("r2", "wr2", "dep_mean", "dep_sd", "n") %in% names(es_summary)))
      diag_rows <- c(paste0("\\multicolumn{", ncols + 1, "}{l}{\\textit{Diagnostics}} \\\\"),
                     diag_line("n",      "$N$             "),
                     diag_line("r2",     "$R^2$           "),
                     diag_line("wr2",    "Within $R^2$    "),
                     diag_line("dep_sd", "Dep.\\ SD        "))
    notes <- paste(c(
      "Cluster-robust standard errors in parentheses.",
      "Post = mean of $\\beta_k$ for $k = 0, \\ldots, 5$; pre = mean for $k \\leq -2$.",
      "Wald $p$ is the cluster-robust joint test of $H_0\\colon \\beta_k = 0$ across pre-event $k$.",
      extra_notes,
      if (length(dropped) > 0) paste0("Rows omitted: ", paste(dropped, collapse = ", "), "."),
      stars_note), collapse = " ")
    header <- paste0(row_label, " & ", paste(outcome_label(outcomes, short = TRUE), collapse = " & "), " \\\\")
    writeLines(table_shell(header, c(body, "\\midrule", diag_rows), caption, label_id, notes, ncols,
                           row_spacing = "0.85"),
               file.path(TABLE_DIR, out_name))
  }
  write_es_summary(
    att_csv = file.path(RESULTS_DIR, "pooled_es_cousub_att.csv"),
    row_col = "direction", row_label = "Direction", row_levels = "Inc", outcomes = muni_outcomes,
    caption  = "Pooled realized-dose event study --- municipality level.",
    label_id = "tab:es_pooled_cousub", out_name = "es_pooled_cousub_summary.tex")
  write_es_summary(
    att_csv = file.path(RESULTS_DIR, "headline_es_att.csv"),
    row_col = "cell", row_label = "Cell", row_levels = cells_main, outcomes = muni_outcomes,
    caption  = "Realized-dose event study by binding-state cell --- municipality level.",
    label_id = "tab:es_realized_cousub", out_name = "es_realized_cousub.tex",
    extra_notes = "Per-cell decomposition of the muni-level realized-dose ES; each cell is run as a separate regression on \\{NC, cell\\}. Inc-bind is the load-bearing cell, with clean pre-trends on the three count outcomes.")

  # ---- the headline regression with fiscal controls, appendix version
  write_merged_full_spec_table(
    coef_csv = file.path(RESULTS_DIR, "headline_full_table.csv"),
    diag_csv = file.path(RESULTS_DIR, "headline_diagnostics.csv"),
    ctrl_csv = file.path(RESULTS_DIR, "headline_full_table.csv"),
    ctrl_labels = muni_ctrl_labels, outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: muni and SD resident-rate cells with fiscal controls (full spec)",
    label_id = "tab:app_full_muni_cell_results", out_name = "app_full_muni_cell_results.tex",
    notes = muni_notes)

  ncols <- length(muni_outcomes)
  outcome_header <- function(first) paste0(first, " & ", paste(outcome_label(muni_outcomes, short = TRUE), collapse = " & "), " \\\\")
  fmt_3dp <- function(value) formatC(value, format = "f", digits = 3)

  # ---- pre-trend battery: the tests per group x outcome
  battery_path <- file.path(RESULTS_DIR, "pretrend_battery.csv")
  if (file.exists(battery_path)) {
    battery <- fread(battery_path)
    group_labels <- c("Inc-bind" = "Inc-bind", "Inc-becomes" = "Inc-becomes",
                      "Inc-nobind" = "Inc-nobind", "Pooled-Inc" = "Pooled Increase")
    battery_values <- function(group_name, field, fmt = fmt_3dp) {
      vapply(muni_outcomes, function(outcome_name) {
        row <- filter(battery, cell == group_name, outcome == outcome_name)
        if (nrow(row) == 0 || is.na(row[[field]][1])) "---"
        else if (is.logical(row[[field]][1])) ifelse(row[[field]][1], "yes", "no")
        else fmt(row[[field]][1])
      }, character(1))
    }
    body <- character(0)
    for (group_name in names(group_labels)) {
      if (!any(battery$cell == group_name)) next
      body <- c(body,
        paste0("\\textbf{", group_labels[[group_name]], "} (", battery$n_treated[battery$cell == group_name][1],
               " munis) & ", paste(rep("", ncols), collapse = " & "), " \\\\"),
        paste0("\\quad Joint Wald $p$ (leads) & ", paste(battery_values(group_name, "wald_joint_p"), collapse = " & "), " \\\\"),
        paste0("\\quad Leads significant at 5\\% (of 4) & ",
               paste(battery_values(group_name, "n_leads_sig5", function(x) sprintf("%d", as.integer(x))), collapse = " & "), " \\\\"),
        paste0("\\quad Linear pre-slope $p$ & ", paste(battery_values(group_name, "lin_slope_p"), collapse = " & "), " \\\\"),
        paste0("\\quad Robust clean & ", paste(battery_values(group_name, "robust_clean"), collapse = " & "), " \\\\"))
    }
    writeLines(table_shell(outcome_header("Cell"), body,
      "Pre-trend battery: three tests per binding cell", "tab:pretrend_battery",
      paste0("All three tests are run on the realized-dose leads, and larger $p$ means no detectable pre-trend. ",
             "Leads significant counts individually significant pre-event coefficients. Robust clean requires no significant lead and a flat slope. ",
             "The joint Wald over-rejects on thin cohorts and does not drive the verdict. The pooled row is shown for comparison."),
      ncols), file.path(TABLE_DIR, "pretrend_battery.tex"))
  }

  # ---- dose-permutation inference (its model section is parked, so this
  # rebuilds from the last saved summary, if any)
  permutation_path <- file.path(RESULTS_DIR, "perm_inference_summary.csv")
  if (file.exists(permutation_path)) {
    permutation <- fread(permutation_path)
    body <- sprintf("%s & %s & %s & %s \\\\", permutation$cell,
                    formatC(permutation$beta_true, format = "f", digits = 2),
                    formatC(permutation$perm_mean, format = "f", digits = 2),
                    formatC(permutation$perm_p, format = "f", digits = 3))
    writeLines(c("\\begin{table}[H]", "  \\centering",
      "  \\caption{Dose-permutation randomization inference (log live-and-work)}",
      "  \\label{tab:perm_inference}", "  \\footnotesize",
      "  \\begin{tabular}{lccc}", "    \\toprule",
      "    Cell & True $\\beta$ & Permutation mean & Permutation $p$ \\\\",
      "    \\midrule", paste0("    ", body), "    \\bottomrule", "  \\end{tabular}",
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.9\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", permutation$n_perm[1],
             " permutations of the realized dose magnitudes across treated municipalities, holding each municipality's event timing and binding cell fixed. ",
             "Permutation $p$ is the share of permuted coefficients at least as large in absolute value as the true one; small values mean the size of the increase, not the identity of the municipality, drives the estimate."),
      "  \\end{minipage}", "\\end{table}"), file.path(TABLE_DIR, "perm_inference.tex"))
  }

  # ---- fake-timing placebo: one panel per shift (3, 4, 5 years early)
  fake_path <- file.path(RESULTS_DIR, "fake_timing_placebo.csv")
  if (file.exists(fake_path)) {
    placebo <- fread(fake_path)
    if (!"shift" %in% names(placebo)) placebo[, shift := 4L]
    body <- character(0)
    for (shift_years in sort(unique(placebo$shift))) {
      shift_rows <- filter(placebo, shift == shift_years)
      body <- c(body, paste0("\\multicolumn{", ncols + 1, "}{l}{\\textit{Placebo event at $c-", shift_years, "$ (",
                             shift_rows$n_treated[1], " eligible munis)}} \\\\"))
      for (cell_name in cells_main) {
        strings <- vapply(muni_outcomes, function(outcome_name) {
          row <- filter(shift_rows, cell == cell_name, outcome == outcome_name)
          if (nrow(row) == 0 || is.na(row$est[1])) c("---", "") else c(fmt_coef(row$est[1], row$p[1]), fmt_se(row$se[1]))
        }, character(2))
        if (all(strings[1, ] == "---")) next
        body <- c(body,
                  paste0("\\quad ", cell_name, " & ", paste(strings[1, ], collapse = " & "), " \\\\"),
                  paste0("       & ", paste(strings[2, ], collapse = " & "), " \\\\"))
      }
      if (shift_years != max(placebo$shift)) body <- c(body, "\\midrule")
    }
    writeLines(table_shell(outcome_header("Cell"), body,
      "Fake-timing placebos: events shifted 3--5 years early, pre-event data only", "tab:fake_timing",
      paste0("Each treated municipality is assigned a placebo event 3, 4 or 5 years before its real one and only data preceding the real event enter, so every coefficient should be null. ",
             "The three panels share municipalities and overlapping windows, so they are consistency checks rather than independent tests, and eligibility varies with the shift. SE in parens. ",
             stars_note),
      ncols), file.path(TABLE_DIR, "fake_timing.tex"))
  }

  # ---- dCDH: effect at k = +5 per group, with the per-pp ATE, joint placebo p,
  # N and switchers. Stars use a normal approximation to est/se.
  dcdh_path <- file.path(RESULTS_DIR, "dcdh_bycell.csv")
  if (file.exists(dcdh_path)) {
    dcdh_k5 <- filter(fread(dcdh_path), rel == 5)
    group_labels <- c(pooled = "Pooled Increase", incbind = "Inc-bind", incbecomes = "Inc-becomes")
    dcdh_values <- function(group_name, field, fmt) {
      vapply(muni_outcomes, function(outcome_name) {
        row <- filter(dcdh_k5, group == group_name, outcome == outcome_name)
        if (nrow(row) == 0 || !(field %in% names(row)) || is.na(row[[field]][1])) "---" else fmt(row[[field]][1])
      }, character(1))
    }
    fmt_count <- function(value) format(as.integer(value), big.mark = ",")
    body <- character(0)
    for (group_name in names(group_labels)) {
      strings <- vapply(muni_outcomes, function(outcome_name) {
        row <- filter(dcdh_k5, group == group_name, outcome == outcome_name)
        if (nrow(row) == 0) c("---", "")
        else c(fmt_coef(row$est[1], 2 * pnorm(-abs(row$est[1] / row$se[1]))), fmt_se(row$se[1]))
      }, character(2))
      body <- c(body,
        paste0("\\textbf{", group_labels[[group_name]], "} & ", paste(strings[1, ], collapse = " & "), " \\\\"),
        paste0("                  & ", paste(strings[2, ], collapse = " & "), " \\\\"),
        paste0("\\quad Avg total effect per pp ($\\approx$ ACR) & ", paste(dcdh_values(group_name, "ate", fmt_3dp), collapse = " & "), " \\\\"),
        paste0("\\quad Joint placebo $p$ & ", paste(dcdh_values(group_name, "joint_placebo_p", fmt_3dp), collapse = " & "), " \\\\"),
        paste0("\\quad $N$ (at $k=+5$) & ", paste(dcdh_values(group_name, "n_obs", fmt_count), collapse = " & "), " \\\\"),
        paste0("\\quad Switchers & ", paste(dcdh_values(group_name, "n_switchers", fmt_count), collapse = " & "), " \\\\"))
    }
    writeLines(table_shell(outcome_header("Group"), body,
      "Cumulative effect at $k=+5$ by group, de Chaisemartin and D'Haultf\\oe uille estimator", "tab:dcdh_results",
      paste0("Effects are levels at the realized dose, not per-point slopes, so compare them to the linear ACR times the group's mean dose. The per-pp row is already dose-normalized. Larger placebo $p$ means no detectable pre-trend. ",
             "Estimated with the \\texttt{continuous} option and analytical standard errors, which the package documentation notes can be liberal, so the standard errors are a lower bound. ",
             "Controls are partialled out within baseline-dose groups, so no control coefficients exist to report. ",
             "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$ (normal approximation)."),
      ncols), file.path(TABLE_DIR, "dcdh_results.tex"))
  }

  # ---- event studies, every coefficient: one table per group, diagnostics underneath
  write_es_full_table <- function(coefs, group_label, label_id, out_name, diag = NULL) {
    if (is.null(coefs) || nrow(coefs) == 0) return(invisible())
    body <- character(0)
    for (event_time in sort(unique(coefs$rel))) {
      strings <- vapply(muni_outcomes, function(outcome_name) {
        row <- filter(coefs, rel == event_time, outcome == outcome_name)
        if (nrow(row) == 0) c("---", "") else c(fmt_coef(row$est[1], row$p[1]), fmt_se(row$se[1]))
      }, character(2))
      if (!any(coefs$rel == event_time & coefs$outcome %in% muni_outcomes)) next
      body <- c(body,
                paste0("$k = ", event_time, "$ & ", paste(strings[1, ], collapse = " & "), " \\\\"),
                paste0("        & ", paste(strings[2, ], collapse = " & "), " \\\\"))
    }
    if (!is.null(diag) && nrow(diag)) {
      diag_labels <- c(n = "$N$", n_munis = "Municipalities", r2 = "$R^2$",
                       wr2 = "Within $R^2$", wald_pre_p = "Joint Wald $p$ (leads)")
      diag_lines <- unlist(lapply(intersect(names(diag_labels), names(diag)), function(field) {
        values <- vapply(muni_outcomes, function(outcome_name) {
          row <- filter(diag, outcome == outcome_name)
          if (!nrow(row) || is.na(row[[field]][1])) return("---")
          value <- row[[field]][1]
          if (field == "n") formatC(value, format = "d", big.mark = ",")
          else if (field == "n_munis") formatC(value, format = "d")
          else sprintf("%.3f", value)
        }, character(1))
        paste0(diag_labels[[field]], " & ", paste(values, collapse = " & "), " \\\\")
      }))
      if (length(diag_lines)) body <- c(body, "\\midrule", diag_lines)
    }
    writeLines(table_shell(outcome_header("Event time"), body,
      paste0("Realized-dose event study, full coefficients: ", group_label), label_id,
      "Per-pp realized-dose coefficient at each event time; $k=-1$ omitted as the reference. SE clustered by municipality. Joint Wald tests the four pre-event leads against zero. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.",
      ncols), file.path(TABLE_DIR, out_name))
  }
  es_coefs_path <- file.path(RESULTS_DIR, "headline_es_coefs.csv")
  if (file.exists(es_coefs_path)) {
    es_coefs <- fread(es_coefs_path)
    es_summary_path <- file.path(RESULTS_DIR, "headline_es_att.csv")
    es_summary <- if (file.exists(es_summary_path)) fread(es_summary_path) else NULL
    for (cell_name in cells_main) {
      write_es_full_table(filter(es_coefs, cell == cell_name),
                          if (cell_name == "Inc-nobind") "Inc-nobind cell (placebo)" else paste(cell_name, "cell"),
                          paste0("tab:es_full_", tolower(gsub("-", "", cell_name))),
                          paste0("es_full_", tolower(gsub("-", "", cell_name)), ".tex"),
                          if (is.null(es_summary)) NULL else filter(es_summary, cell == cell_name))
    }
  }
  pooled_coefs_path <- file.path(RESULTS_DIR, "pooled_es_cousub_coefs.csv")
  if (file.exists(pooled_coefs_path)) {
    pooled_coefs <- fread(pooled_coefs_path)
    if ("direction" %in% names(pooled_coefs)) pooled_coefs <- filter(pooled_coefs, direction == "Inc")
    pooled_summary_path <- file.path(RESULTS_DIR, "pooled_es_cousub_att.csv")
    pooled_summary <- if (file.exists(pooled_summary_path)) fread(pooled_summary_path) else NULL
    if (!is.null(pooled_summary) && "direction" %in% names(pooled_summary))
      pooled_summary <- filter(pooled_summary, direction == "Inc")
    write_es_full_table(pooled_coefs, "Pooled Increase", "tab:es_full_pooled", "es_full_pooled.tex", pooled_summary)
  }
})

# In-text numbers ----
# Every number the prose quotes, re-derived from its CSV into
# data/results/intext_numbers.csv, with gates that stop the build if one drifts.
# Only the typical-event doses are typed in: 0.243 and 0.384 pp, the mean
# cumulative dose at +5 (cell_counts_summary.csv, dose_k5).
if (RUN_INTEXT_NUMBERS) local({
  headline <- fread(file.path(RESULTS_DIR, "headline_full_table.csv"))
  live_work_coef <- function(cell_name, column = "est") {
    headline[[column]][headline$term == sprintf("treat_cell::%s:dose_stack", cell_name) &
                         headline$outcome == "log_live_and_work"][1]
  }
  inc_bind     <- live_work_coef("Inc-bind")
  inc_becomes  <- live_work_coef("Inc-becomes")
  inc_nobind   <- live_work_coef("Inc-nobind")
  inc_nobind_p <- live_work_coef("Inc-nobind", "p")
  n_obs <- headline$n[headline$outcome == "log_live_and_work"][1]
  dose_inc_bind <- 0.243; dose_inc_becomes <- 0.384

  revenue <- fread(file.path(RESULTS_DIR, "eit_revenue_offset.csv"))
  splits  <- fread(file.path(RESULTS_DIR, "eit_revenue_split.csv"))
  # the paper's ~37% offset is the low-baseline (standard-structure) group, not
  # the overall mean; the "five high-baseline" munis are Inc-bind-hi
  offset_low  <- splits$offset[splits$split == "cap_1pp" & splits$cell == "Inc-bind-lo"][1]
  offset_high <- splits$offset[splits$split == "cap_1pp" & splits$cell == "Inc-bind-hi"][1]

  pct <- function(log_change) round(100 * (exp(log_change) - 1), 1)
  intext <- tibble(
    claim = c(
      "Inc-bind muni-rate effect on log live-and-work (per pp)",
      "Inc-bind full-pp percent change  exp(b)-1",
      "Inc-bind typical-event log change  0.243*b",
      "Inc-bind typical-event percent change",
      "Inc-becomes effect on log live-and-work (per pp)",
      "Inc-becomes typical-event log change  0.384*b",
      "Inc-becomes typical-event percent change",
      "Inc-nobind placebo (per pp; null)",
      "Inc-nobind placebo p-value",
      "Estimation sample N (cousub-years)",
      "EIT revenue net per-pp coefficient (beta_net)",
      "EIT revenue behavioral offset, standard-structure (low-baseline) munis",
      "EIT revenue behavioral offset, high-baseline munis"),
    value = c(
      round(inc_bind, 3), pct(inc_bind), round(dose_inc_bind * inc_bind, 3), pct(dose_inc_bind * inc_bind),
      round(inc_becomes, 3), round(dose_inc_becomes * inc_becomes, 3), pct(dose_inc_becomes * inc_becomes),
      round(inc_nobind, 3), round(inc_nobind_p, 3), n_obs,
      round(revenue$beta_net[1], 3), round(offset_low, 3), round(offset_high, 3)),
    paper_says = c("-0.537", "-41.6%", "-0.131", "-12.2%",
                   "-0.250", "-0.096", "-9.2%", "null", "> 0.1", "—",
                   "1.04", "~ 37%", "(five high-baseline)"),
    source = c(rep("headline_full_table.csv  (sheet headline_decomp)", 9),
               "headline_full_table.csv",
               "eit_revenue_offset.csv  (sheet rev_offset)",
               "eit_revenue_split.csv  (sheet rev_split, cap_1pp Inc-bind-lo)",
               "eit_revenue_split.csv  (sheet rev_split, cap_1pp Inc-bind-hi)"))
  fwrite(intext, file.path(RESULTS_DIR, "intext_numbers.csv"))

  stopifnot(
    "Inc-bind log_LW drifted from -0.537"    = abs(inc_bind    - (-0.537)) < 0.01,
    "Inc-becomes log_LW drifted from -0.250" = abs(inc_becomes - (-0.250)) < 0.01,
    "Inc-nobind placebo no longer null"      = inc_nobind_p > 0.10,
    "Estimation N changed from 171,451"      = n_obs == 171451L,
    "EIT revenue net coef drifted from 1.04" = abs(revenue$beta_net[1] - 1.041) < 0.02,
    "Low-baseline offset drifted from ~37%"  = abs(offset_low - 0.374) < 0.03)
  print(sprintf("[validation] in-text numbers reconcile: Inc-bind=%.3f (%.0f%%), Inc-becomes=%.3f, placebo p=%.2f, N=%s, rev_net=%.2f, offset_lo=%.0f%%",
                inc_bind, pct(inc_bind), inc_becomes, inc_nobind_p, format(n_obs, big.mark = ","),
                revenue$beta_net[1], 100 * offset_low))
})

# Flexible-dose ACR table ----
# Appendix A: the ACR under the linear, quadratic and spline dose (from
# RUN_FLEX_DOSE in 1_main_models.R), with the mean dose per group in the note.
if (RUN_FLEX_DOSE_TABLE) local({
  acr_path <- file.path(RESULTS_DIR, "flex_dose_acr.csv")
  if (!file.exists(acr_path)) return(invisible())
  acr_table <- fread(acr_path)
  outcomes <- c("share_live_in", "log_live_and_work", "log_in_commuters", "log_total_jobs")
  header   <- c("Share live in muni", "Log(work in own muni)", "Log(in-commuters)", "Log(total jobs)")
  row_groups <- list(list("pooled", "Inc", "\\textbf{Pooled Increase}"),
                     list("decomp", "Inc-bind", "\\textbf{Inc-bind}"),
                     list("decomp", "Inc-becomes", "\\textbf{Inc-becomes}"),
                     list("decomp", "Inc-nobind", "\\textbf{Inc-nobind}"))
  # estimate, SE and p columns for each dose specification, and its row label
  specifications <- list(c("acr_linear", "se_linear", "p_linear", "\\quad Linear (headline)"),
                         c("acr_quad", "se_quad", "p_quad", "\\quad Quadratic"),
                         c("acr_spline", "se_spline", "p_spline", "\\quad Natural spline"))
  body <- character(0); dose_notes <- character(0)
  for (row_group in row_groups) {
    group_rows <- filter(acr_table, spec == row_group[[1]], cell == row_group[[2]])
    if (!nrow(group_rows)) next
    body <- c(body, paste0("    ", row_group[[3]], " & & & & \\\\"))
    for (specification in specifications) {
      strings <- vapply(outcomes, function(outcome_name) {
        row <- filter(group_rows, outcome == outcome_name)
        estimate <- if (!nrow(row) || is.na(row[[specification[1]]][1])) "---"
                    else fmt_coef(row[[specification[1]]][1], row[[specification[3]]][1])
        se <- if (!nrow(row) || is.na(row[[specification[2]]][1])) "" else fmt_se(row[[specification[2]]][1])
        c(estimate, se)
      }, character(2))
      body <- c(body, paste0("    ", specification[4], " & ", paste(strings[1, ], collapse = " & "), " \\\\"),
                      paste0("        & ", paste(strings[2, ], collapse = " & "), " \\\\"))
    }
    dose_notes <- c(dose_notes, sprintf("%s %.3f (%d munis)", gsub("\\\\textbf\\{|\\}", "", row_group[[3]]),
                                        group_rows$mean_dose[1], group_rows$n_treated[1]))
  }
  writeLines(c("\\begin{table}[H]", "  \\centering",
    "  \\caption{ACR under three dose specifications}",
    "  \\label{tab:flex_dose_acr}", "  \\footnotesize",
    "  \\renewcommand{\\arraystretch}{0.9}",
    "  \\resizebox{\\textwidth}{!}{",
    "  \\begin{tabular}{lcccc}", "    \\toprule",
    paste0("    & ", paste(header, collapse = " & "), " \\\\"), "    \\midrule",
    body, "    \\bottomrule", "  \\end{tabular}", "  }",
    "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
    paste0("    \\footnotesize \\textit{Notes:} Each row is the ACR, the average of ACRT$(d)$ weighted by $\\omega(d)$, which puts one weight on each treated municipality at the dose it took. ",
           "The linear row is the headline coefficient. Mean doses: ", paste(dose_notes, collapse = ", "), ". ",
           "The quadratic and spline rows enter the dose as the cumulative change rather than the rate level, which is the same as the headline for a linear term but not for a nonlinear one. ",
           "Sample, controls, school-district co-treatment, fixed effects and clustering are the headline's, ",
           sprintf("$N = %s$. ", formatC(unique(acr_table$n)[1], format = "d", big.mark = ",")),
           "Inc-nobind's 14 treated municipalities cannot identify a spline. ",
           "SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
    "  \\end{minipage}", "\\end{table}"), file.path(TABLE_DIR, "flex_dose_acr.tex"))
  message("Saved flex_dose_acr.tex")
})

# Results workbook (build_results_workbook.R) ----
# Collect the paper's result CSVs into ONE Excel workbook with an INDEX sheet and
# one sheet per result table -- including the realized-dose and pooled EVENT-STUDY
# results (destined for the paper). Output: results/all_results.xlsx
# Run after stages 02-03 so the ES + robustness CSVs are current. Missing CSVs are
# skipped, so it is safe to run on a partial pipeline.
if (RUN_WORKBOOK) local({
  RESULTS_DIR  <- RESULTS_DIR
  TABLE_DIR <- file.path(ROOT, "results"); dir.create(TABLE_DIR, showWarnings = FALSE)
  out_xlsx <- file.path(TABLE_DIR, "all_results.xlsx")

  # (file in data/results, sheet name <=31 chars, description)
  # Lay explanation: this list is the workbook's table of contents -- every
  # result CSV behind the paper becomes its own named sheet in one Excel
  # file, so a reader can open it and audit any number in the paper without
  # installing R or rerunning anything.
  manifest <- list(
    c("headline_full_table.csv",  "headline_decomp",  "Binding decomposition: per-pp dose coefficients, all cells x outcomes (HEADLINE)"),
    c("headline_diagnostics.csv", "headline_diag",    "Headline regression diagnostics (R2, within-R2, N, dep mean)"),
    c("intext_numbers.csv",       "in_text_numbers",  "Reconciliation of the headline IN-TEXT prose numbers -> value + source (gate-checked)"),
    c("pooled_results.csv",       "pooled_benchmark", "Pooled-Increase benchmark (per-pp dose + binary event)"),
    c("pooled_diagnostics.csv",   "pooled_diag",      "Pooled benchmark regression diagnostics (R2, N, dep mean)"),
    c("headline_es_coefs.csv",    "ES_dose_coefs",    "Realized-dose event study: coefficients by event-time (MAIN ES -- for paper)"),
    c("headline_es_att.csv",      "ES_dose_ATT",      "Realized-dose ES: post/pre ATT + joint-Wald pre-trend (for paper)"),
    c("pooled_es_cousub_att.csv", "ES_pooled_ATT",    "Pooled-Increase event study: post/pre ATT (for paper)"),
    c("act32_split_cousub.csv",   "act32_split",      "Robustness: pre/post Act-32 enforcement split"),
    c("act32_split_cousub_diag.csv","act32_diag",     "Act-32 split regression diagnostics (R2, N, dep mean)"),
    c("dcdh_robustness.csv",      "dcdh_robustness",  "Robustness: de Chaisemartin-D'Haultfoeuille (non-negative weights) ES vs linear feols"),
    c("curvature_acrt_feols.csv", "curvature_acrt",   "Curvature: parametric ACRT(d) dose-response (marginal effect)"),
    c("curvature_att_curve.csv",  "curvature_att",    "Curvature: ATT(d) dose-response (the paper figure fig:curvature_att)"),
    c("pretrend_battery.csv",     "pretrend_battery", "MULTIPLE pre-trend tests: joint Wald, individual leads, linear pre-slope"),
    c("eit_revenue_offset.csv",   "rev_offset",       "EIT revenue: net response + Laffer offset at mean/median rate (in-text numbers)"),
    c("eit_revenue_decomp.csv",   "rev_decomp",       "EIT revenue: gross vs net decomposition"),
    c("eit_revenue_split.csv",    "rev_split",        "EIT revenue: pre/post Act-32 split"),
    c("act47_robustness_cousub.csv","act47_robustness","Robustness: Act 47 distressed-municipality exclusion (paper table)"),
    c("dcdh_bycell.csv",          "dcdh_bycell",      "Robustness: dCDH ES by group (pooled / Inc-bind / Inc-becomes), level-at-dose scale, +-5"),
    c("feols_es_bycell.csv",      "feols_es_bycell",  "feols realized-dose ES by group, level-at-mean-dose scale (twin of dcdh_bycell)"),
    c("eit_revenue_es.csv",       "rev_es",           "EIT revenue: realized-dose event study"),
    c("curvature_acrt_cells.csv", "curvature_cells",  "Curvature: per-cell ACRT diagnostics"),
    c("curvature_dcdh_lin_vs_quad.csv","curvature_dcdh","Curvature: dCDH linear-vs-quadratic comparison"),
    c("pooled_es_cousub_coefs.csv","ES_pooled_coefs", "Pooled-Increase event study: full lead/lag coefficients by event-time"),
    c("summary_muni.csv",         "summary_muni",     "Summary statistics: muni-year level (paper table)"),
    c("summary_block.csv",        "summary_block",    "Summary statistics: block-year level (paper table)"),
    c("summary_sd.csv",           "summary_sd",       "Summary statistics: school-district level (paper table)"),
    c("cell_counts_summary.csv",  "cell_counts",      "Binding-cell counts + dose ranges (paper table)"),
    c("perm_inference_summary.csv","perm_inference",  "Robustness: dose-permutation randomization inference (500 draws, log live-and-work)"),
    c("fake_timing_placebo.csv",  "fake_timing",      "Robustness: fake-timing placebo (events shifted 4 years early, pre-event data only)")
  )

  wb <- createWorkbook()
  hdr <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom")
  idx_rows <- list(); added <- 0L
  for (m in manifest) {
    f <- file.path(RESULTS_DIR, m[1]); sh <- m[2]; desc <- m[3]
    if (!file.exists(f)) { print(sprintf("skip (missing): %s", m[1])); next }
    dt <- fread(f)
    addWorksheet(wb, sh)
    writeData(wb, sh, dt, headerStyle = hdr)
    freezePane(wb, sh, firstRow = TRUE)
    setColWidths(wb, sh, cols = seq_len(ncol(dt)), widths = "auto")
    added <- added + 1L
    idx_rows[[length(idx_rows)+1L]] <- data.table(sheet = sh, rows = nrow(dt),
                                                  cols = ncol(dt), source_csv = m[1],
                                                  description = desc)
  }
  idx <- rbindlist(idx_rows, fill = TRUE)
  addWorksheet(wb, "INDEX"); writeData(wb, "INDEX",
    data.table(workbook = "Do Local Taxes Cause Residential Sorting? -- paper results",
               generated = "run scripts/4_output/2_tables.R (RUN_WORKBOOK) to refresh"))
  writeData(wb, "INDEX", idx, startRow = 3, headerStyle = hdr)
  setColWidths(wb, "INDEX", cols = 1:5, widths = c(20, 8, 8, 38, 90))
  worksheetOrder(wb) <- c(length(names(wb)), seq_len(length(names(wb)) - 1L))  # INDEX first
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
})


# Pooled robustness tables ----
# The pooled-specification versions of the Section 6 checks, one table each.
# Estimates always carry \sym{} (empty when not significant), as before.
if (RUN_POOLED_ROBUSTNESS_TABLES) local({
  outcomes <- c("share_live_in", "log_live_and_work", "log_in_commuters", "log_total_jobs")
  header   <- c("Share live in muni", "Log(work in own muni)", "Log(in-commuters)", "Log(total jobs)")
  pooled_term <- "direction_muni::Inc:dose_stack"

  table_lines <- function(body, caption, label_id, notes) c(
    "\\begin{table}[H]", "  \\centering", sprintf("  \\caption{%s}", caption),
    sprintf("  \\label{%s}", label_id), "  \\small", "  \\resizebox{\\textwidth}{!}{",
    "  \\begin{tabular}{lcccc}", "    \\toprule",
    sprintf("    & %s \\\\", paste(header, collapse = " & ")), "    \\midrule",
    body, "    \\bottomrule", "  \\end{tabular}", "  }", "  \\vspace{0.3em}",
    "  \\begin{minipage}{0.95\\textwidth}",
    paste("    \\footnotesize \\textit{Notes:}", notes), "  \\end{minipage}", "\\end{table}")
  # estimate and SE rows for the pooled Inc term
  term_rows <- function(results, label, wanted_term = pooled_term) {
    strings <- vapply(outcomes, function(outcome_name) {
      row <- filter(results, outcome == outcome_name, grepl(wanted_term, term, fixed = TRUE))
      if (!nrow(row)) c("---", "")
      else c(paste0(fmt_est(row$est[1]), "\\sym{", sig_stars(row$p[1]), "}"), fmt_se(row$se[1]))
    }, character(2))
    c(sprintf("    %s & %s \\\\", label, paste(strings[1, ], collapse = " & ")),
      sprintf("     & %s \\\\", paste(strings[2, ], collapse = " & ")))
  }

  act32_path <- file.path(RESULTS_DIR, "pooled_act32.csv")
  if (file.exists(act32_path)) {
    act32 <- fread(act32_path)
    body <- unlist(lapply(c("pre", "post"), function(period_name) {
      period_rows <- filter(act32, period == period_name)
      if (!nrow(period_rows)) return(NULL)
      term_rows(period_rows, sprintf("%s-Act 32 cohorts (%d munis)", tools::toTitleCase(period_name),
                                     period_rows$n_treated[1]))
    }))
    writeLines(table_lines(body, "Pooled Increase effect, split at the Act 32 enforcement break", "tab:pooled_act32",
      paste("Cohorts split at Act 32's effective year, 2012. Specification otherwise",
            "unchanged. SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")),
      file.path(TABLE_DIR, "pooled_act32.tex"))
    message("Saved tables/pooled_act32.tex")
  }

  act47_path <- file.path(RESULTS_DIR, "pooled_act47.csv")
  if (file.exists(act47_path)) {
    act47 <- fread(act47_path)
    body <- unlist(lapply(unique(act47$spec), function(spec_name)
      term_rows(filter(act47, spec == spec_name), spec_name)))
    writeLines(table_lines(body, "Pooled Increase effect, excluding Act 47 distressed municipalities", "tab:pooled_act47",
      paste("Act 47 status from DCED's distressed register. Excluded from the treated",
            "group only. SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")),
      file.path(TABLE_DIR, "pooled_act47.tex"))
    message("Saved tables/pooled_act47.tex")
  }

  fake_path <- file.path(RESULTS_DIR, "pooled_fake_timing.csv")
  if (file.exists(fake_path)) {
    placebo <- fread(fake_path)
    body <- unlist(lapply(sort(unique(placebo$shift)), function(shift_years) {
      shift_rows <- filter(placebo, shift == shift_years)
      if (!nrow(shift_rows)) return(NULL)
      c(sprintf("    \\multicolumn{5}{l}{\\textit{Placebo event at $c-%d$ (%d eligible munis)}} \\\\",
                shift_years, shift_rows$n_treated[1]),
        term_rows(shift_rows, "\\quad Pooled Inc", wanted_term = "direction_muni::Inc:dose_fake"))
    }))
    writeLines(table_lines(body, "Pooled Increase effect at placebo event dates", "tab:pooled_fake_timing",
      paste("Placebo events 3, 4 or 5 years early, using only data before the real",
            "change, so every coefficient should be null. Counts fall with the shift",
            "because eligibility needs two placebo pre-years inside the panel. Panels",
            "overlap. SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")),
      file.path(TABLE_DIR, "pooled_fake_timing.tex"))
    message("Saved tables/pooled_fake_timing.tex")
  }

  battery_path <- file.path(RESULTS_DIR, "pooled_pretrend_battery.csv")
  if (file.exists(battery_path)) {
    battery <- fread(battery_path)
    battery_values <- function(field, fmt) vapply(outcomes, function(outcome_name) {
      row <- filter(battery, outcome == outcome_name)
      if (!nrow(row) || is.na(row[[field]][1])) "---" else fmt(row[[field]][1])
    }, character(1))
    fmt_3dp <- function(value) formatC(value, format = "f", digits = 3)
    body <- c(
      sprintf("    Joint Wald $p$ (leads) & %s \\\\", paste(battery_values("wald_joint_p", fmt_3dp), collapse = " & ")),
      sprintf("    Leads significant at 5\\%% (of 4) & %s \\\\",
              paste(battery_values("n_leads_sig5", function(x) sprintf("%d", as.integer(x))), collapse = " & ")),
      sprintf("    Smallest lead $p$ & %s \\\\", paste(battery_values("min_lead_p", fmt_3dp), collapse = " & ")),
      sprintf("    Linear pre-slope & %s \\\\", paste(battery_values("lin_slope", fmt_3dp), collapse = " & ")),
      sprintf("    Linear pre-slope $p$ & %s \\\\", paste(battery_values("lin_slope_p", fmt_3dp), collapse = " & ")),
      sprintf("    Clean & %s \\\\", paste(battery_values("robust_clean", function(x) ifelse(as.logical(x), "yes", "no")),
                                         collapse = " & ")))
    writeLines(table_lines(body, "Pre-trend battery for the pooled Increase specification", "tab:pooled_pretrend_battery",
      paste("Tests on the four pre-event leads of the pooled realized-dose event study.",
            "Clean means the joint Wald does not reject at 10\\% and no single lead is",
            "significant at 5\\%.")),
      file.path(TABLE_DIR, "pooled_pretrend_battery.tex"))
    message("Saved tables/pooled_pretrend_battery.tex")
  }
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
