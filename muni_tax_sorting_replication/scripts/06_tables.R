#!/usr/bin/env Rscript
# 06_tables.R -- Paper LaTeX tables, in-text-number reconciliation, and the
# all-results Excel workbook, inline and flag-gated.
# Consolidates: build_paper_tables.R, build_intext_numbers.R,
#   build_results_workbook.R.
# Run AFTER 02+03 so the result CSVs in data/results/ are current.
# Outputs -> tables/*.tex, data/results/intext_numbers.csv, results/all_results.xlsx.
# Run from package root:  Rscript scripts/06_tables.R

suppressMessages({
  library(data.table)
  library(openxlsx)
})

.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_helpers.R"))

# Flags ----
RUN_PAPER_TABLES   <- TRUE    # all LaTeX table fragments -> tables/
RUN_INTEXT_NUMBERS <- TRUE    # gates re-pinned to the post-property prose, 2026-08-06
RUN_WORKBOOK       <- TRUE    # all result CSVs -> results/all_results.xlsx

# Paper tables (build_paper_tables.R) ----
# Build publication-style LaTeX table fragments from the results CSVs for
# direct \input{} into the paper.
#
# COUSUB-ONLY (repo-cleanup, 2026-05): builders for the retired SD-unit,
# block-unit, industry, Sun-Abraham binary, tercile, alt-control, and the
# block/SD pooled & ES & appendix-full tables were removed, together with
# the helper functions only those builders used. What remains builds only
# the cousub/municipality fragments the kept paper \input's
# (headline_muni_combined, act32_cousub, pooled_per_pp_cousub,
# es_pooled_cousub_summary, es_realized_cousub, app_full_muni_cell_results)
# plus a few muni-level supporting tables that read kept inputs.
#
# Each table reports coefficient (with sig stars) and SE (in parens below)
# per cell-by-outcome cell, plus a per-column diagnostic block (N, R^2,
# within R^2, dep mean, dep SD). Uses the formatting helpers in _helpers.R for
# the label map, formatters, and table builder.
if (RUN_PAPER_TABLES) local({
  out_dir <- TABLE_DIR
  in_dir  <- RESULTS_DIR

  # Outcome and cell sets per level
  # Column order per user (2026-07-06): share first, then log live-and-work.
  muni_outcomes  <- c("share_live_in","log_live_and_work",
                      "log_in_commuters","log_total_jobs")
  block_outcomes        <- c("log_home_count","log_work_count",
                             "home_in_muni_share")
  block_outcomes_main   <- c("log_home_count","log_work_count")
  block_outcomes_shares <- c("home_in_muni_share")
  cells_main     <- c("Inc-bind","Inc-becomes","Inc-nobind")

  # Read + parse helpers (data CSVs come in slightly different formats)
  read_coefs <- function(coef_csv, side = NULL) {
    if (!file.exists(coef_csv)) return(NULL)
    d <- fread(coef_csv)
    # Parse cell column
    if (!"cell" %in% names(d)) {
      if ("term" %in% names(d)) {
        d[, cell := sub(".*::([^:]+):.*", "\\1", term)]
      }
    }
    # Lay explanation: the headline model estimates the muni-rate and the
    # school-rate effects together in one regression, so its output file
    # mixes both. Each coefficient's name is prefixed with the rate-setter
    # it belongs to; this pulls out just the rows for the side we need.
    # Filter by side. Rename the parameter to avoid data.table column masking:
    # if `side` is also a column name, `d[d$side == side]` evaluates RHS in
    # the data.table's column scope, becoming `column == column` (always TRUE).
    if (!is.null(side)) {
      # Lay explanation: without the rename, R would find a column named
      # "side" inside the table and compare it to itself -- every row would
      # match. Copying the argument to a fresh name avoids that mix-up.
      .side_arg <- side
      # Aliases: cousub headline CSV uses term prefixes `treat_cell::` (muni)
      # and `cell_sd::` (SD); block CSV uses a literal `side` column.
      term_prefix_alias <- c(muni = "treat_cell", sd = "cell_sd")
      if ("side" %in% names(d)) {
        d <- d[side == .side_arg]
      } else if ("term" %in% names(d)) {
        pfx <- if (.side_arg %in% names(term_prefix_alias))
                term_prefix_alias[[.side_arg]] else .side_arg
        d <- d[grepl(paste0("^", pfx, "::"), term)]
      }
    }
    # Harmonize coefficient column names
    if (!"est" %in% names(d)) {
      if ("Estimate" %in% names(d)) setnames(d, "Estimate", "est")
    }
    if (!"se" %in% names(d) && "Std. Error" %in% names(d)) setnames(d, "Std. Error", "se")
    if (!"p"  %in% names(d) && "Pr(>|t|)"   %in% names(d)) setnames(d, "Pr(>|t|)", "p")
    d
  }

  read_diag <- function(diag_csv) {
    if (!file.exists(diag_csv)) return(data.table())
    d <- fread(diag_csv)
    # Harmonize diagnostics names: lowercase r2/wr2 plus dep_mean/dep_sd
    ren <- c(R2 = "r2", Within_R2 = "wr2",
             dep_mean = "dep_mean", dep_sd = "dep_sd",
             n = "n", N = "n")
    for (nm in names(ren)) {
      if (nm %in% names(d) && ren[nm] != nm) setnames(d, nm, ren[nm])
    }
    d
  }

  # Convenience wrapper for the simplest "one side per table" case
  write_cell_table <- function(coef_csv, diag_csv, side, outcomes, cells,
                                caption, label_id, out_name,
                                short = TRUE, notes = character(0)) {
    coefs <- read_coefs(coef_csv, side = side)
    if (is.null(coefs) || nrow(coefs) == 0) {
      print(sprintf("Skipping %s: source CSV missing or empty", out_name))
      return(invisible())
    }
    diag <- read_diag(diag_csv)
    # Per-cell pre-trend p from that cell's own realized-dose event study,
    # so the diagnostic that licenses each coefficient sits with it.
    pt <- tryCatch({
      a <- fread(file.path(in_dir, "headline_es_att.csv"))
      a[, .(cell, outcome, wald_pre_p)]
    }, error = function(e) NULL)
    body <- build_pub_table(
      coefs_dt  = coefs,
      diag_dt   = diag,
      pretrend_dt = pt,
      outcomes  = outcomes,
      cells     = cells,
      caption   = caption,
      label     = label_id,
      short_labels = short,
      notes     = notes
    )
    writeLines(body, file.path(out_dir, out_name))
  }

  # write_merged_cell_table: muni-side + SD-side from the SAME joint
  # regression in one table, with section headers. Used for appendix tables
  # where the same outcomes are shown for both rate-setters.
  # Read fiscal controls in long format (outcome, term, est, se, p)
  read_controls_long <- function(csv_path, term_filter = NULL) {
    if (!file.exists(csv_path) || file.size(csv_path) == 0) return(NULL)
    d <- fread(csv_path)
    if ("Estimate" %in% names(d) && !"est" %in% names(d)) setnames(d, "Estimate", "est")
    if ("Std. Error" %in% names(d) && !"se" %in% names(d)) setnames(d, "Std. Error", "se")
    if ("Pr(>|t|)" %in% names(d) && !"p" %in% names(d)) setnames(d, "Pr(>|t|)", "p")
    needed <- c("outcome","term","est","se","p")
    if (!all(needed %in% names(d))) return(NULL)
    if (!is.null(term_filter)) d <- d[term %in% term_filter]
    # Mask rows with absurd SEs (near-singular fit; happens with collinear
    # share controls in some regressions). Treat SE > 100 as not identified.
    d[!is.na(se) & abs(se) > 100, `:=`(est = NA_real_, se = NA_real_, p = NA_real_)]
    d[, .(outcome, term, est, se, p)]
  }

  # Pretty labels for controls used in appendix full-spec tables.
  # share_eit / share_re labels kept for legacy CSVs, but the active control set
  # (build_headline_env ctrls) replaced them with the three property
  # effective rates (millage x CLR) on 2026-07-02.
  muni_ctrl_labels <- c(
    share_police    = "police-services share",
    share_streets   = "streets share",
    share_debt_svc  = "debt-service share",
    share_eit       = "EIT-revenue share",
    share_re        = "real-estate-tax share",
    share_intergov  = "intergovernmental-revenue share",
    muni_eff_rate   = "muni property eff.\\ rate (pp)",
    sd_eff_rate     = "SD property eff.\\ rate (pp)",
    county_eff_rate = "county property eff.\\ rate (pp)"
  )
  sd_ctrl_labels <- c(
    share_state_rev_sd       = "state-revenue share",
    share_local_rev_sd       = "local-revenue share",
    share_federal_rev_sd     = "federal-revenue share",
    share_property_tax_sd    = "property-tax share",
    share_instructional_sd   = "instructional-expenditure share",
    share_support_sd         = "support-services share",
    share_capital_sd         = "capital-expenditure share",
    share_debt_int_sd        = "debt-interest share",
    mean_muni_rate           = "mean municipality rate"
  )
  block_ctrl_labels <- c(muni_ctrl_labels, sd_ctrl_labels)

  write_merged_cell_table <- function(coef_csv, diag_csv,
                                        outcomes, cells,
                                        caption, label_id, out_name,
                                        short = TRUE, notes = character(0)) {
    coefs_muni <- read_coefs(coef_csv, side = "muni")
    coefs_sd   <- read_coefs(coef_csv, side = "sd")
    if (is.null(coefs_muni) || nrow(coefs_muni) == 0) {
      print(sprintf("Skipping %s: source CSV missing", out_name)); return(invisible())
    }
    diag <- read_diag(diag_csv)
    short_lab <- short
    ncols <- length(outcomes)
    hdr_labels <- outcome_label(outcomes, short = short_lab)
    hdr <- paste0("Cell & ", paste(hdr_labels, collapse = " & "), " \\\\")

    fmt_one <- function(side_coefs, cells, ncols) {
      body <- character(0); dropped <- character(0)
      for (cl in cells) {
        est_row <- character(ncols); se_row <- character(ncols); any_present <- FALSE
        for (j in seq_along(outcomes)) {
          r <- side_coefs[cell == cl & outcome == outcomes[j]]
          if (nrow(r) == 0) { est_row[j] <- "---"; se_row[j] <- ""; next }
          stars <- sig_stars(r$p[1])
          est_str <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
          est_row[j] <- est_str; se_row[j] <- fmt_se(r$se[1])
          if (est_str != "---") any_present <- TRUE
        }
        if (!any_present) { dropped <- c(dropped, cl); next }
        body <- c(body,
                  paste0("\\textbf{", cl, "} & ", paste(est_row, collapse = " & "), " \\\\"),
                  paste0("                  & ", paste(se_row,  collapse = " & "), " \\\\"))
      }
      list(body = body, dropped = dropped)
    }

    muni_section <- fmt_one(coefs_muni, cells, ncols)
    sd_section   <- fmt_one(coefs_sd,   cells, ncols)
    mc <- paste0("\\multicolumn{", ncols + 1, "}{l}")

    body <- c(
      paste0(mc, "{\\textit{Muni resident rate ($\\tau^M$)}} \\\\"),
      muni_section$body,
      "\\midrule",
      paste0(mc, "{\\textit{School district rate ($\\tau^{SD}$)}} \\\\"),
      sd_section$body
    )

    diag_block <- character(0)
    if (nrow(diag) > 0) {
      pick <- function(field) {
        out <- character(ncols)
        for (j in seq_along(outcomes)) {
          r <- diag[outcome == outcomes[j]]
          if (nrow(r) == 0 || !(field %in% names(r))) { out[j] <- "---"; next }
          v <- r[[field]][1]
          if (is.na(v)) { out[j] <- "---"; next }
          out[j] <- if (field == "n") format(as.integer(v), big.mark = ",")
                     else formatC(v, format = "f", digits = 3)
        }
        out
      }
      diag_block <- c(
        paste0(mc, "{\\textit{Diagnostics}} \\\\"),
        paste0("$N$           & ", paste(pick("n"),        collapse = " & "), " \\\\"),
        paste0("$R^2$         & ", paste(pick("r2"),       collapse = " & "), " \\\\"),
        paste0("Within $R^2$  & ", paste(pick("wr2"),      collapse = " & "), " \\\\")
      )
    }

    col_spec <- paste0("l", strrep("c", ncols))
    notes_all <- paste(c(notes,
                          "Both rate-setters enter the same joint regression; the table reports both sets of cell$\\times$dose coefficients.",
                          if (length(muni_section$dropped) + length(sd_section$dropped) > 0)
                            paste0("Cells omitted (no estimable coefficients): ",
                                    paste(unique(c(muni_section$dropped, sd_section$dropped)), collapse = ", "), "."),
                          "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
                        collapse = " ")
    tabular <- c(
      paste0("  \\begin{tabular}{", col_spec, "}"),
      "    \\toprule",
      paste0("    ", hdr),
      "    \\midrule",
      paste0("    ", body),
      if (length(diag_block)) "    \\midrule" else character(0),
      if (length(diag_block)) paste0("    ", diag_block) else character(0),
      "    \\bottomrule",
      "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")

    out <- c(
      "\\begin{table}[H]",
      "  \\centering",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{", label_id, "}"),
      "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.9}",
      tabular,
      "  \\vspace{0.3em}",
      "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", notes_all),
      "  \\end{minipage}",
      "\\end{table}")
    writeLines(out, file.path(out_dir, out_name))
  }

  # write_merged_full_spec_table: one table with the muni-side and SD-side cells
  # as two panels (both from the same joint regression), the shared fiscal
  # controls, and diagnostics. Used for the combined Appendix A cousub table.
  write_merged_full_spec_table <- function(coef_csv, diag_csv, ctrl_csv, ctrl_labels,
                                            outcomes, cells, caption, label_id, out_name,
                                            short = TRUE, notes = character(0),
                                            wald_dt = NULL, diag_dt_override = NULL) {
    coefs_muni <- read_coefs(coef_csv, side = "muni")
    coefs_sd   <- read_coefs(coef_csv, side = "sd")
    if (is.null(coefs_muni) || nrow(coefs_muni) == 0) {
      print(sprintf("Skipping %s: source CSV missing", out_name)); return(invisible())
    }
    diag     <- if (!is.null(diag_dt_override)) diag_dt_override else read_diag(diag_csv)
    controls <- read_controls_long(ctrl_csv, term_filter = names(ctrl_labels))
    ncols <- length(outcomes)
    hdr <- paste0("Cell & ", paste(outcome_label(outcomes, short = short), collapse = " & "), " \\\\")
    mc  <- paste0("\\multicolumn{", ncols + 1, "}{l}")

    fmt_cells <- function(side_coefs) {
      body <- character(0)
      for (cl in cells) {
        est_row <- character(ncols); se_row <- character(ncols); any_present <- FALSE
        for (j in seq_along(outcomes)) {
          r <- side_coefs[cell == cl & outcome == outcomes[j]]
          if (nrow(r) == 0) { est_row[j] <- "---"; se_row[j] <- ""; next }
          stars <- sig_stars(r$p[1])
          est_str <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
          est_row[j] <- est_str; se_row[j] <- fmt_se(r$se[1]); if (est_str != "---") any_present <- TRUE
        }
        if (!any_present) next
        body <- c(body,
                  paste0("\\textbf{", cl, "} & ", paste(est_row, collapse = " & "), " \\\\"),
                  paste0("                  & ", paste(se_row,  collapse = " & "), " \\\\"))
      }
      body
    }

    controls_block <- character(0)
    if (!is.null(controls) && nrow(controls) > 0) {
      controls_block <- c("\\midrule", paste0(mc, "{\\textit{Fiscal controls}} \\\\"))
      for (tm in unique(controls$term)) {
        est_vec <- character(ncols); se_vec <- character(ncols); any_present <- FALSE
        for (j in seq_along(outcomes)) {
          r <- controls[outcome == outcomes[j] & term == tm]
          if (nrow(r) == 0 || is.na(r$est[1])) { est_vec[j] <- "---"; se_vec[j] <- ""; next }
          any_present <- TRUE; stars <- sig_stars(r$p[1])
          est_vec[j] <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
          se_vec[j] <- fmt_se(r$se[1])
        }
        if (!any_present) next
        pretty <- if (tm %in% names(ctrl_labels)) ctrl_labels[[tm]] else gsub("_", "\\\\_", tm)
        controls_block <- c(controls_block,
                            paste0(pretty, " & ", paste(est_vec, collapse = " & "), " \\\\"),
                            paste0("                  & ", paste(se_vec, collapse = " & "), " \\\\"))
      }
    }

    diag_block <- character(0)
    if (nrow(diag) > 0) {
      pick <- function(field) {
        out <- character(ncols)
        for (j in seq_along(outcomes)) {
          r <- diag[outcome == outcomes[j]]
          if (nrow(r) == 0 || !(field %in% names(r))) { out[j] <- "---"; next }
          v <- r[[field]][1]; if (is.na(v)) { out[j] <- "---"; next }
          out[j] <- if (field == "n") format(as.integer(v), big.mark = ",") else formatC(v, format = "f", digits = 3)
        }
        out
      }
      wald_row <- character(0)
      if (!is.null(wald_dt) && nrow(wald_dt) > 0) {
        wv <- character(ncols)
        for (j in seq_along(outcomes)) {
          r <- wald_dt[outcome == outcomes[j]]
          wv[j] <- if (nrow(r) == 0 || is.na(r$wald_p[1])) "---"
                   else formatC(r$wald_p[1], format = "f", digits = 3)
        }
        wald_row <- paste0("Joint Wald $p$ (leads) & ", paste(wv, collapse = " & "), " \\\\")
      }
      diag_block <- c("\\midrule", paste0(mc, "{\\textit{Diagnostics}} \\\\"),
        paste0("$N$           & ", paste(pick("n"),        collapse = " & "), " \\\\"),
        paste0("$R^2$         & ", paste(pick("r2"),       collapse = " & "), " \\\\"),
        paste0("Within $R^2$  & ", paste(pick("wr2"),      collapse = " & "), " \\\\"),
        wald_row)
    }

    body <- c(
      paste0(mc, "{\\textit{Muni resident rate ($\\tau^M$)}} \\\\"),
      fmt_cells(coefs_muni),
      "\\midrule",
      paste0(mc, "{\\textit{School district rate ($\\tau^{SD}$)}} \\\\"),
      fmt_cells(coefs_sd),
      controls_block, diag_block)

    col_spec <- paste0("l", strrep("c", ncols))
    notes_all <- paste(c(notes,
      "Both rate-setters enter the same joint regression; the table reports both sets of cell$\\times$dose coefficients with the shared fiscal controls.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."), collapse = " ")
    tabular <- c(
      paste0("  \\begin{tabular}{", col_spec, "}"),
      "    \\toprule", paste0("    ", hdr), "    \\midrule",
      paste0("    ", body), "    \\bottomrule", "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c("\\begin{table}[H]", "  \\centering",
      paste0("  \\caption{", caption, "}"), paste0("  \\label{", label_id, "}"),
      "  \\footnotesize", "  \\renewcommand{\\arraystretch}{0.9}", tabular,
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", notes_all),
      "  \\end{minipage}", "\\end{table}")
    writeLines(out, file.path(out_dir, out_name))
  }

  # 1. Headline tables (municipality + SD + block)
  muni_notes <- c(
    paste("Stacked-DiD; muni$\\times$stack and year$\\times$stack FE; SE clustered at muni.")
  )

  write_cell_table(
    coef_csv = file.path(in_dir, "headline_full_table.csv"),
    diag_csv = file.path(in_dir, "headline_diagnostics.csv"),
    side     = "treat_cell",
    outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: muni resident-rate cells",
    label_id = "tab:headline_muni_cell_results",
    out_name = "headline_muni_cell_results.tex",
    notes    = muni_notes
  )

  write_cell_table(
    coef_csv = file.path(in_dir, "headline_full_table.csv"),
    diag_csv = file.path(in_dir, "headline_diagnostics.csv"),
    side     = "cell_sd",
    outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: SD resident-rate cells (partial)",
    label_id = "tab:headline_muni_sd_side_results",
    out_name = "headline_muni_sd_side_results.tex",
    notes    = c(muni_notes[1],
                 "SD-side cells from the same regression as the muni-side table.")
  )

  # Combined muni + SD coefficients from the same joint regression in one FULL
  # table: cells + fiscal controls + diagnostics (no dep-SD row).
  # No Wald row: the table carries six coefficient rows and the test covers only
  # the Inc-bind cell, so a single row under all six invites a misreading. All
  # cells and outcomes are in the pre-trend battery, Table~\\ref{tab:pretrend_battery}.
  write_merged_full_spec_table(
    coef_csv = file.path(in_dir, "headline_full_table.csv"),
    diag_csv = file.path(in_dir, "headline_diagnostics.csv"),
    ctrl_csv = file.path(in_dir, "headline_full_table.csv"),
    ctrl_labels = muni_ctrl_labels,
    outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: muni and SD resident-rate cells (joint regression)",
    label_id = "tab:headline_muni_combined",
    out_name = "headline_muni_combined.tex",
    notes    = c(muni_notes,
                 "Realized-dose pre-trend tests for every cell and outcome are reported in the pre-trend battery.")
  )

  # Act 47 exclusion, main-results format: full coeftable on the sample that
  # drops the Act 47 Inc-bind munis (built by 04_robustness.R).
  a47_path <- file.path(in_dir, "act47_full_table.csv")
  if (file.exists(a47_path)) {
    a47_diag <- unique(fread(a47_path)[, .(outcome, n, r2, wr2, dep_mean)])
    write_merged_full_spec_table(
      coef_csv = a47_path,
      diag_csv = "",
      ctrl_csv = a47_path,
      ctrl_labels = muni_ctrl_labels,
      outcomes = muni_outcomes, cells = cells_main,
      caption  = "Inc-bind effect excluding Hazleton, the one Act 47 designated municipality",
      label_id = "tab:act47_full_results",
      out_name = "act47_full_results.tex",
      notes    = c(muni_notes, "Leave-one-out on the Inc-bind cell, dropping Hazleton. This block builds the treatment as a change from the pre-event mean rather than as a level, so it runs on a slightly larger sample than the headline table and its baseline reads $-0.539$ against the headline $-0.537$."),
      diag_dt_override = a47_diag
    )
  }

  # Act 32 split, full format: one table per period (cells + controls + diag).
  a32_path <- file.path(in_dir, "act32_full_cousub.csv")
  if (file.exists(a32_path)) {
    a32 <- fread(a32_path)
    for (per in c("pre", "post")) {
      ap <- a32[period == per]
      if (nrow(ap) == 0) next
      cf <- ap[grepl("^treat_cell::", term)]
      cf[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
      ctl <- ap[term %in% names(muni_ctrl_labels), .(outcome, term, est, se, p)]
      dg <- unique(ap[, .(outcome, n, r2, wr2, dep_mean)])
      body <- build_pub_table(
        coefs_dt = cf[, .(outcome, cell, est, se, p)],
        diag_dt  = dg,
        outcomes = muni_outcomes, cells = cells_main,
        caption  = sprintf("Act 32 split, %s-enforcement cohorts (first event %s 2012): full specification",
                           per, ifelse(per == "pre", "before", "in/after")),
        label    = sprintf("tab:act32_full_%s", per),
        controls_dt = ctl, control_labels = muni_ctrl_labels,
        notes    = c(muni_notes,
                     if (per == "pre") "Inc-nobind is absent pre-2012: 11 municipalities are in the cell, but their dose term is collinear with the stack fixed effects on the thin pre-enforcement subsample and is dropped by the estimator."))
      writeLines(body, file.path(out_dir, sprintf("act32_full_%s.tex", per)))
    }
  }

  # Cousub per-cell realized-dose ES (tab:es_realized_cousub) is built below via
  # write_es_summary so it carries the joint pre-trend Wald p row; it backs the
  # decomposition-diagnostic prose in section 5.

  # 4. Pre/Post Act 32
  write_act32_table <- function(coef_csv, diag_csv, outcomes, cells,
                                  caption, label_id, out_name, notes = character(0)) {
    if (!file.exists(coef_csv) || file.size(coef_csv) == 0) {
      print(sprintf("Skipping Act32 %s", out_name)); return(invisible())
    }
    a <- fread(coef_csv)
    d <- if (!is.null(diag_csv) && file.exists(diag_csv)) fread(diag_csv) else NULL

    # Build the table body with hierarchical layout:
    #   \textbf{Inc-bind}
    #     \quad Pre  & est & est & ... \\
    #                & (se) & (se) & ... \\
    #     \quad Post & est & est & ... \\
    #                & (se) & (se) & ... \\
    short <- TRUE
    hdr_labels <- outcome_label(outcomes, short = short)
    ncols <- length(outcomes)
    hdr <- paste0("Outcome & ", paste(hdr_labels, collapse = " & "), " \\\\")

    body <- character(0)
    dropped <- character(0)
    fmt_cell <- function(est, se, p) {
      if (length(est) == 0 || is.na(est) || !is.finite(est)) return(list("---",""))
      stars <- sig_stars(p)
      est_str <- if (nchar(stars) == 0) fmt_est(est) else paste0(fmt_est(est), "\\sym{", stars, "}")
      list(est_str, fmt_se(se))
    }
    for (cl in cells) {
      cell_lines <- character(0)
      any_present <- FALSE
      for (per_label in c("Pre-Act 32","Post-Act 32")) {
        per <- if (per_label == "Pre-Act 32") "pre" else "post"
        est_row <- character(ncols); se_row <- character(ncols)
        row_present <- FALSE
        for (j in seq_along(outcomes)) {
          r <- a[cell == cl & period == per & outcome == outcomes[j]]
          if (nrow(r) == 0) { est_row[j] <- "---"; se_row[j] <- ""; next }
          v <- fmt_cell(r$est[1], r$se[1], r$p[1])
          est_row[j] <- v[[1]]; se_row[j] <- v[[2]]
          if (est_row[j] != "---") row_present <- TRUE
        }
        if (!row_present) next
        any_present <- TRUE
        cell_lines <- c(cell_lines,
          paste0("\\quad ", per_label, " & ",
                  paste(est_row, collapse = " & "), " \\\\"),
          paste0("                & ",
                  paste(se_row,  collapse = " & "), " \\\\"))
      }
      if (!any_present) { dropped <- c(dropped, cl); next }
      body <- c(body,
                paste0("\\textbf{", cl, "} ", paste(rep("&", ncols), collapse = " "), " \\\\"),
                cell_lines)
    }

    # Diagnostics block (per outcome). For Act-32 we have TWO regressions per
    # outcome (pre / post). We show separate rows for each.
    diag_block <- character(0)
    if (!is.null(d)) {
      for (per_label in c("Pre-Act 32","Post-Act 32")) {
        per <- if (per_label == "Pre-Act 32") "pre" else "post"
        diag_block <- c(diag_block,
          paste0("\\textit{", per_label, "} ",
                 paste(rep("&", ncols), collapse = " "), " \\\\"))
        for (field in c("n","r2","wr2")) {
          label <- switch(field,
                          n = "\\quad $N$",
                          r2 = "\\quad $R^2$",
                          wr2 = "\\quad Within $R^2$",
                          dep_mean = "\\quad Dep.\\ mean",
                          dep_sd   = "\\quad Dep.\\ SD")
          vals <- character(ncols)
          for (j in seq_along(outcomes)) {
            row <- d[period == per & outcome == outcomes[j]]
            if (nrow(row) == 0 || !(field %in% names(row))) { vals[j] <- "---"; next }
            x <- row[[field]][1]
            vals[j] <- if (is.na(x)) "---"
                       else if (field == "n") formatC(as.numeric(x), format = "d", big.mark = ",")
                       else formatC(x, format = "f", digits = 3)
          }
          diag_block <- c(diag_block,
                          paste0(label, " & ", paste(vals, collapse = " & "), " \\\\"))
        }
      }
    }

    col_spec <- paste0("l", strrep("c", ncols))
    notes_all <- c(notes,
                    "Cluster-robust standard errors in parentheses.",
                    "Treated cohorts split at first event year $c < 2012$ (Pre-Act~32) vs $c \\geq 2012$ (Post-Act~32).",
                    if (length(dropped) > 0) paste0("Cells omitted (no estimable coefficients): ",
                                                     paste(dropped, collapse = ", "), "."),
                    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")
    notes_all <- paste(notes_all, collapse = " ")

    tabular <- c(
      paste0("  \\begin{tabular}{", col_spec, "}"),
      "    \\toprule",
      paste0("    ", hdr),
      "    \\midrule",
      paste0("    ", body),
      if (length(diag_block)) "    \\midrule" else character(0),
      if (length(diag_block)) paste0("    ", diag_block) else character(0),
      "    \\bottomrule",
      "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")

    out <- c(
      "\\begin{table}[H]",
      "  \\centering",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{", label_id, "}"),
      "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.85}",
      tabular,
      "  \\vspace{0.3em}",
      "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", notes_all),
      "  \\end{minipage}",
      "\\end{table}")
    writeLines(out, file.path(out_dir, out_name))
  }
  write_act32_table(file.path(in_dir, "act32_split_cousub.csv"),
                     file.path(in_dir, "act32_split_cousub_diag.csv"),
                     muni_outcomes, cells_main,
                     "Pre-/post-Act-32 split --- municipality level",
                     "tab:act32_cousub", "act32_cousub.tex")

  # 8. Pooled Increase headline tables
  # Each table reports four coefficients of interest per outcome:
  #   muni-Inc, muni-Dec, SD-Inc, SD-Dec (per-pp or binary, depending on mode).
  # The block level has 4 outcomes; the cousub level has up to 5; the SD
  # level has only one rate-setter side (sd-Inc, sd-Dec).
  write_pooled_table <- function(level_tag, spec_mode, outcomes, caption, label_id,
                                  out_name, has_two_sides = TRUE,
                                  notes = character(0)) {
    res_path <- file.path(in_dir, "pooled_results.csv")
    diag_path <- file.path(in_dir, "pooled_diagnostics.csv")
    if (!file.exists(res_path)) {
      print(sprintf("Skipping %s: pooled_results.csv missing", out_name))
      return(invisible())
    }
    .lev <- level_tag; .mod <- spec_mode
    a <- fread(res_path)[level == .lev & mode == .mod]
    d <- if (file.exists(diag_path)) fread(diag_path)[level == .lev & mode == .mod]
         else data.table()
    if (nrow(a) == 0) { print(sprintf("No rows for %s/%s", level_tag, mode)); return(invisible()) }

    # Build hierarchical rows: side header -> Inc subrow -> Dec subrow
    sides <- if (has_two_sides) c("muni","sd") else c("sd")
    side_labels <- c(muni = "Municipality rate ($\\tau^{M}$)",
                      sd   = "School district rate ($\\tau^{SD}$)")
    short <- TRUE
    hdr_labels <- outcome_label(outcomes, short = short)
    ncols <- length(outcomes)
    hdr <- paste0("Outcome & ", paste(hdr_labels, collapse = " & "), " \\\\")
    body <- character(0)
    fmt_cell <- function(est, se, p) {
      if (length(est) == 0 || is.na(est) || !is.finite(est)) return(list("---",""))
      stars <- sig_stars(p)
      est_str <- if (nchar(stars) == 0) fmt_est(est) else paste0(fmt_est(est), "\\sym{", stars, "}")
      list(est_str, fmt_se(se))
    }
    for (side_tag in sides) {
      body <- c(body,
                paste0("\\textbf{", side_labels[[side_tag]], "} ",
                       paste(rep("&", ncols), collapse = " "), " \\\\"))
      for (dir in c("Inc")) {
        est_row <- character(ncols); se_row <- character(ncols)
        for (j in seq_along(outcomes)) {
          r <- a[side == side_tag & direction == dir & outcome == outcomes[j]]
          if (nrow(r) == 0) { est_row[j] <- "---"; se_row[j] <- ""; next }
          v <- fmt_cell(r$est[1], r$se[1], r$p[1])
          est_row[j] <- v[[1]]; se_row[j] <- v[[2]]
        }
        pretty_dir <- if (dir == "Inc") "$\\Delta\\tau > 0$ (Inc)" else "$\\Delta\\tau < 0$ (Dec)"
        body <- c(body,
                  paste0("\\quad ", pretty_dir, " & ",
                         paste(est_row, collapse = " & "), " \\\\"),
                  paste0("                       & ",
                         paste(se_row,  collapse = " & "), " \\\\"))
      }
    }
    # Fiscal-control coefficients (per-pp table only, from pooled_full_table.csv)
    ctrl_block <- character(0)
    if (spec_mode == "per_pp") {
      pc <- read_controls_long(file.path(in_dir, "pooled_full_table.csv"),
                               term_filter = names(muni_ctrl_labels))
      if (!is.null(pc) && nrow(pc) > 0) {
        mcp <- paste0("\\multicolumn{", ncols + 1, "}{l}")
        ctrl_block <- c("\\midrule", paste0(mcp, "{\\textit{Fiscal controls}} \\\\"))
        for (tm in unique(pc$term)) {
          est_vec <- character(ncols); se_vec <- character(ncols); any_present <- FALSE
          for (j in seq_along(outcomes)) {
            r <- pc[outcome == outcomes[j] & term == tm]
            if (nrow(r) == 0 || is.na(r$est[1])) { est_vec[j] <- "---"; se_vec[j] <- ""; next }
            any_present <- TRUE; stars <- sig_stars(r$p[1])
            est_vec[j] <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
            se_vec[j] <- fmt_se(r$se[1])
          }
          if (!any_present) next
          pretty <- if (tm %in% names(muni_ctrl_labels)) muni_ctrl_labels[[tm]] else gsub("_", "\\\\_", tm)
          ctrl_block <- c(ctrl_block,
                          paste0(pretty, " & ", paste(est_vec, collapse = " & "), " \\\\"),
                          paste0("                  & ", paste(se_vec, collapse = " & "), " \\\\"))
        }
      }
    }

    # Diagnostics: per-outcome N, R2, dep mean (+ pooled-ES joint Wald p, per-pp only)
    diag_block <- character(0)
    if (nrow(d) > 0) {
      for (field in c("n","r2","wr2")) {
        label <- switch(field,
                        n = "$N$", r2 = "$R^2$",
                        wr2 = "Within $R^2$",
                        dep_mean = "Dep.\\ mean")
        vals <- character(ncols)
        for (j in seq_along(outcomes)) {
          row <- d[outcome == outcomes[j]]
          if (nrow(row) == 0 || !(field %in% names(row))) { vals[j] <- "---"; next }
          x <- row[[field]][1]
          vals[j] <- if (is.na(x)) "---"
                     else if (field == "n") formatC(as.numeric(x), format = "d", big.mark = ",")
                     else formatC(x, format = "f", digits = 3)
        }
        diag_block <- c(diag_block,
                        paste0(label, " & ", paste(vals, collapse = " & "), " \\\\"))
      }
      if (spec_mode == "per_pp") {
        pw_path <- file.path(in_dir, "pooled_es_cousub_att.csv")
        if (file.exists(pw_path)) {
          pw <- fread(pw_path)[direction == "Inc"]
          wv <- character(ncols)
          for (j in seq_along(outcomes)) {
            r <- pw[outcome == outcomes[j]]
            wv[j] <- if (nrow(r) == 0 || is.na(r$wald_pre_p[1])) "---"
                     else formatC(r$wald_pre_p[1], format = "f", digits = 3)
          }
          diag_block <- c(diag_block,
            paste0("Joint Wald $p$ (leads) & ", paste(wv, collapse = " & "), " \\\\"))
        }
      }
    }

    col_spec <- paste0("l", strrep("c", ncols))
    notes_all <- paste(c(notes,
      paste0("Cluster-robust standard errors in parentheses.",
             if (has_two_sides) " Each regression includes both rate-setters' direction-pooled Inc treatments jointly; the reported coefficients are $\\beta^M_{\\text{Inc}}$ and $\\beta^{SD}_{\\text{Inc}}$." else ""),
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
      collapse = " ")
    tabular <- c(
      paste0("  \\begin{tabular}{", col_spec, "}"),
      "    \\toprule",
      paste0("    ", hdr),
      "    \\midrule",
      paste0("    ", body),
      if (length(ctrl_block)) paste0("    ", ctrl_block) else character(0),
      if (length(diag_block)) "    \\midrule" else character(0),
      if (length(diag_block)) paste0("    ", diag_block) else character(0),
      "    \\bottomrule",
      "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c(
      "\\begin{table}[H]",
      "  \\centering",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{", label_id, "}"),
      "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.85}",
      tabular,
      "  \\vspace{0.3em}",
      "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", notes_all),
      "  \\end{minipage}",
      "\\end{table}")
    writeLines(out, file.path(out_dir, out_name))
  }

  # Per-pp pooled tables
  write_pooled_table("cousub", "per_pp", muni_outcomes,
                     "Pooled Increase headline --- municipality level (per pp)",
                     "tab:pooled_per_pp_cousub", "pooled_per_pp_cousub.tex")

  # Binary pooled tables
  write_pooled_table("cousub", "binary", muni_outcomes,
                     "Pooled Increase headline --- municipality level (binary event)",
                     "tab:pooled_binary_cousub", "pooled_binary_cousub.tex",
                     notes = c("Binary post-treatment indicator: 1 in the event year and after."))

  # 9. ES summary tables (post-mean / pre-mean ATT with Wald pre-trend p)
  # These tables sit immediately before each ES figure in the paper so the
  # reader sees the numerical ATT summary alongside the dynamic-response plot.
  # Source CSVs are att.csv outputs from each ES script; columns include
  # att_post, att_post_se, att_pre, att_pre_se, wald_pre_stat, wald_pre_df,
  # wald_pre_p, n. Tables use the short outcome labels and the publication
  # theme.

  fmt_wald_p <- function(p) {
    if (is.null(p) || length(p) == 0 || is.na(p)) return("---")
    if (p < 1e-4) return("$<\\!0.0001$")
    formatC(p, format = "f", digits = 4)
  }

  write_es_summary <- function(att_csv, row_var, row_levels, outcomes,
                                caption, label_id, out_name,
                                short_labels = TRUE,
                                extra_notes = character(0)) {
    if (!file.exists(att_csv) || file.size(att_csv) == 0) {
      print(sprintf("Skipping %s: %s missing", out_name, att_csv)); return(invisible())
    }
    a <- fread(att_csv)
    # Harmonize column names across ES variants
    if ("att_post_per_pp" %in% names(a)) setnames(a, "att_post_per_pp", "att_post", skip_absent = TRUE)
    if ("att_pre_per_pp"  %in% names(a)) setnames(a, "att_pre_per_pp",  "att_pre",  skip_absent = TRUE)

    ncols <- length(outcomes)
    hdr_labels <- outcome_label(outcomes, short = short_labels)
    hdr <- paste0(row_var$label, " & ",
                   paste(hdr_labels, collapse = " & "), " \\\\")
    body <- character(0)
    dropped <- character(0)
    for (lvl in row_levels) {
      post_row <- character(ncols); se_row <- character(ncols)
      pre_row  <- character(ncols); pre_se_row <- character(ncols)
      wald_row <- character(ncols)
      any_present <- FALSE
      for (j in seq_along(outcomes)) {
        r <- a[get(row_var$col) == lvl & outcome == outcomes[j]]
        if (nrow(r) == 0) {
          post_row[j] <- se_row[j] <- pre_row[j] <- pre_se_row[j] <- wald_row[j] <- "---"
          next
        }
        r <- r[1]
        any_present <- TRUE
        post_row[j]   <- fmt_est(r$att_post)
        se_row[j]     <- fmt_se(r$att_post_se)
        pre_row[j]    <- fmt_est(r$att_pre)
        pre_se_row[j] <- fmt_se(r$att_pre_se)
        wald_row[j]   <- if ("wald_pre_p" %in% names(r))
                            fmt_wald_p(r$wald_pre_p) else "---"
      }
      if (!any_present) { dropped <- c(dropped, lvl); next }
      body <- c(body,
                paste0("\\textbf{", lvl, "} (post) & ",
                       paste(post_row, collapse = " & "), " \\\\"),
                paste0("                  & ",
                       paste(se_row,   collapse = " & "), " \\\\"),
                paste0("\\textbf{", lvl, "} (pre) & ",
                       paste(pre_row, collapse = " & "), " \\\\"),
                paste0("                  & ",
                       paste(pre_se_row, collapse = " & "), " \\\\"),
                paste0("\\textbf{", lvl, "} Wald $p$ & ",
                       paste(wald_row, collapse = " & "), " \\\\"))
    }
    # Diagnostics block (per outcome; uses Inc-direction model)
    diag_rows <- character(0)
    if (any(c("r2","wr2","dep_mean","dep_sd","n") %in% names(a))) {
      pick <- function(field) {
        out <- character(ncols)
        for (j in seq_along(outcomes)) {
          r <- a[outcome == outcomes[j]]
          if (nrow(r) == 0 || !(field %in% names(r))) { out[j] <- "---"; next }
          v <- r[[field]][1]
          if (is.na(v)) { out[j] <- "---"; next }
          out[j] <- if (field == "n") format(as.integer(v), big.mark = ",")
                     else formatC(v, format = "f", digits = 3)
        }
        out
      }
      # Only emit a diagnostic row when the field has at least one real value;
      # ES att CSVs carry N but not R^2/dep stats, so those rows are dropped.
      diag_line <- function(field, label_text) {
        vals <- pick(field)
        if (all(vals == "---")) return(character(0))
        paste0(label_text, " & ", paste(vals, collapse = " & "), " \\\\")
      }
      diag_rows <- c(
        paste0("\\multicolumn{", ncols + 1, "}{l}{\\textit{Diagnostics}} \\\\"),
        diag_line("n",        "$N$             "),
        diag_line("r2",       "$R^2$           "),
        diag_line("wr2",      "Within $R^2$    "),
        diag_line("dep_sd",   "Dep.\\ SD        ")
      )
    }
    body <- c(body, "\\midrule", diag_rows)
    col_spec <- paste0("l", strrep("c", ncols))
    notes_all <- paste(c(
      "Cluster-robust standard errors in parentheses.",
      "Post = mean of $\\beta_k$ for $k = 0, \\ldots, 5$; pre = mean for $k \\leq -2$.",
      "Wald $p$ is the cluster-robust joint test of $H_0\\colon \\beta_k = 0$ across pre-event $k$.",
      extra_notes,
      if (length(dropped) > 0)
        paste0("Rows omitted: ", paste(dropped, collapse = ", "), "."),
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
      collapse = " ")
    tabular <- c(
      paste0("  \\begin{tabular}{", col_spec, "}"),
      "    \\toprule",
      paste0("    ", hdr),
      "    \\midrule",
      paste0("    ", body),
      "    \\bottomrule",
      "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c(
      "\\begin{table}[H]",
      "  \\centering",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{", label_id, "}"),
      "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.85}",
      tabular,
      "  \\vspace{0.3em}",
      "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", notes_all),
      "  \\end{minipage}",
      "\\end{table}")
    writeLines(out, file.path(out_dir, out_name))
  }

  # Pooled ES summary tables: rows = direction (Inc, Dec)
  direction_row <- list(col = "direction", label = "Direction")

  write_es_summary(
    att_csv = file.path(in_dir, "pooled_es_cousub_att.csv"),
    row_var = direction_row, row_levels = c("Inc"),
    outcomes = muni_outcomes,
    caption  = "Pooled realized-dose event study --- municipality level.",
    label_id = "tab:es_pooled_cousub",
    out_name = "es_pooled_cousub_summary.tex")

  # Block per-cell ES summary
  cell_row <- list(col = "cell", label = "Cell")

  # Cousub per-cell ES summary -- backs the decomposition-diagnostic prose; the
  # joint pre-trend Wald p row is the source of the numbers cited in section 5.2.
  write_es_summary(
    att_csv = file.path(in_dir, "headline_es_att.csv"),
    row_var = cell_row, row_levels = cells_main,
    outcomes = muni_outcomes,
    caption  = "Realized-dose event study by binding-state cell --- municipality level.",
    label_id = "tab:es_realized_cousub",
    out_name = "es_realized_cousub.tex",
    extra_notes = c("Per-cell decomposition of the muni-level realized-dose ES; each cell is run as a separate regression on \\{NC, cell\\}. Inc-bind is the load-bearing cell, with clean pre-trends on the three count outcomes."))

  # Appendix A: full-spec tables (cells + fiscal controls, same regression)
  ## A.1 Municipality-level (cousub): muni + SD resident-rate cells in ONE table,
  ## sharing the muni-AFR fiscal controls (both sides from the same joint regression).
  write_merged_full_spec_table(
    coef_csv = file.path(in_dir, "headline_full_table.csv"),
    diag_csv = file.path(in_dir, "headline_diagnostics.csv"),
    ctrl_csv = file.path(in_dir, "headline_full_table.csv"),
    ctrl_labels = muni_ctrl_labels,
    outcomes = muni_outcomes, cells = cells_main,
    caption  = "Municipality-level dose-response: muni and SD resident-rate cells with fiscal controls (full spec)",
    label_id = "tab:app_full_muni_cell_results",
    out_name = "app_full_muni_cell_results.tex",
    notes    = muni_notes
  )

  ## Pre-trend battery table: four tests per group x outcome.
  bat_path <- file.path(in_dir, "pretrend_battery.csv")
  if (file.exists(bat_path)) {
    bat <- fread(bat_path)
    bgl <- c("Inc-bind" = "Inc-bind", "Inc-becomes" = "Inc-becomes",
             "Inc-nobind" = "Inc-nobind", "Pooled-Inc" = "Pooled Increase")
    ncols <- length(muni_outcomes)
    fmt3 <- function(x) formatC(x, format = "f", digits = 3)
    pick_b <- function(cl, field, fmt = fmt3) {
      out <- character(ncols)
      for (j in seq_along(muni_outcomes)) {
        r <- bat[cell == cl & outcome == muni_outcomes[j]]
        out[j] <- if (nrow(r) == 0 || is.na(r[[field]][1])) "---"
                  else if (is.logical(r[[field]][1])) ifelse(r[[field]][1], "yes", "no")
                  else fmt(r[[field]][1])
      }
      out
    }
    body <- character(0)
    for (cl in names(bgl)) {
      if (!nrow(bat[cell == cl])) next
      nt <- bat[cell == cl, n_treated][1]
      body <- c(body,
        paste0("\\textbf{", bgl[[cl]], "} (", nt, " munis) & ",
               paste(rep("", ncols), collapse = " & "), " \\\\"),
        paste0("\\quad Joint Wald $p$ (leads) & ", paste(pick_b(cl, "wald_joint_p"), collapse = " & "), " \\\\"),
        paste0("\\quad Leads significant at 5\\% (of 4) & ",
               paste(pick_b(cl, "n_leads_sig5", fmt = function(x) sprintf("%d", as.integer(x))), collapse = " & "), " \\\\"),
        paste0("\\quad Linear pre-slope $p$ & ", paste(pick_b(cl, "lin_slope_p"), collapse = " & "), " \\\\"),
        paste0("\\quad Robust clean & ", paste(pick_b(cl, "robust_clean"), collapse = " & "), " \\\\"))
    }
    hdr <- paste0("Cell & ", paste(outcome_label(muni_outcomes, short = TRUE), collapse = " & "), " \\\\")
    tabular <- c(paste0("  \\begin{tabular}{l", strrep("c", ncols), "}"),
      "    \\toprule", paste0("    ", hdr), "    \\midrule", paste0("    ", body),
      "    \\bottomrule", "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c("\\begin{table}[H]", "  \\centering",
      "  \\caption{Pre-trend battery: three tests per binding cell}",
      "  \\label{tab:pretrend_battery}", "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.9}", tabular,
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} All three tests are run on the realized-dose leads, and larger $p$ means no detectable pre-trend. ",
             "Leads significant counts individually significant pre-event coefficients. Robust clean requires no significant lead and a flat slope. ",
             "The joint Wald over-rejects on thin cohorts and does not drive the verdict. The pooled row is shown for comparison."),
      "  \\end{minipage}", "\\end{table}")
    writeLines(out, file.path(out_dir, "pretrend_battery.tex"))
  }

  ## Dose-permutation randomization-inference table.
  pi_path <- file.path(in_dir, "perm_inference_summary.csv")
  if (file.exists(pi_path)) {
    ps <- fread(pi_path)
    body <- character(0)
    for (i in seq_len(nrow(ps))) {
      body <- c(body, paste0(ps$cell[i], " & ",
        formatC(ps$beta_true[i], format = "f", digits = 2), " & ",
        formatC(ps$perm_mean[i], format = "f", digits = 2), " & ",
        formatC(ps$perm_p[i], format = "f", digits = 3), " \\\\"))
    }
    out <- c("\\begin{table}[H]", "  \\centering",
      "  \\caption{Dose-permutation randomization inference (log live-and-work)}",
      "  \\label{tab:perm_inference}", "  \\footnotesize",
      "  \\begin{tabular}{lccc}", "    \\toprule",
      paste0("    Cell & True $\\beta$ & Permutation mean & Permutation $p$ \\\\"),
      "    \\midrule", paste0("    ", body), "    \\bottomrule", "  \\end{tabular}",
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.9\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} ", ps$n_perm[1],
             " permutations of the realized dose magnitudes across treated municipalities, holding each municipality's event timing and binding cell fixed. ",
             "Permutation $p$ is the share of permuted coefficients at least as large in absolute value as the true one; small values mean the size of the increase, not the identity of the municipality, drives the estimate."),
      "  \\end{minipage}", "\\end{table}")
    writeLines(out, file.path(out_dir, "perm_inference.tex"))
  }

  ## Fake-timing placebo table: one panel per placebo shift (3 / 4 / 5 years
  ## early), cells x outcomes within each panel.
  ft_path <- file.path(in_dir, "fake_timing_placebo.csv")
  if (file.exists(ft_path)) {
    ft <- fread(ft_path)
    if (!"shift" %in% names(ft)) ft[, shift := 4L]
    ncols <- length(muni_outcomes)
    mc <- paste0("\\multicolumn{", ncols + 1, "}{l}")
    body <- character(0)
    for (sh in sort(unique(ft$shift))) {
      fs <- ft[shift == sh]
      nt <- fs$n_treated[1]
      body <- c(body,
        paste0(mc, "{\\textit{Placebo event at $c-", sh, "$ (", nt,
               " eligible munis)}} \\\\"))
      for (cl in cells_main) {
        est_row <- character(ncols); se_row <- character(ncols); any_p <- FALSE
        for (j in seq_along(muni_outcomes)) {
          r <- fs[cell == cl & outcome == muni_outcomes[j]]
          if (nrow(r) == 0 || is.na(r$est[1])) { est_row[j] <- "---"; se_row[j] <- ""; next }
          any_p <- TRUE
          stars <- sig_stars(r$p[1])
          est_row[j] <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
          se_row[j]  <- fmt_se(r$se[1])
        }
        if (!any_p) next
        body <- c(body,
                  paste0("\\quad ", cl, " & ", paste(est_row, collapse = " & "), " \\\\"),
                  paste0("       & ", paste(se_row, collapse = " & "), " \\\\"))
      }
      if (sh != max(ft$shift)) body <- c(body, "\\midrule")
    }
    hdr <- paste0("Cell & ", paste(outcome_label(muni_outcomes, short = TRUE), collapse = " & "), " \\\\")
    tabular <- c(paste0("  \\begin{tabular}{l", strrep("c", ncols), "}"),
      "    \\toprule", paste0("    ", hdr), "    \\midrule", paste0("    ", body),
      "    \\bottomrule", "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c("\\begin{table}[H]", "  \\centering",
      "  \\caption{Fake-timing placebos: events shifted 3--5 years early, pre-event data only}",
      "  \\label{tab:fake_timing}", "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.9}", tabular,
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} Each treated municipality is assigned a placebo event 3, 4 or 5 years before its real one and only data preceding the real event enter, so every coefficient should be null. ",
             "The three panels share municipalities and overlapping windows, so they are consistency checks rather than independent tests, and eligibility varies with the shift. SE in parens. ",
             "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
      "  \\end{minipage}", "\\end{table}")
    writeLines(out, file.path(out_dir, "fake_timing.tex"))
  }

  ## dCDH results table: per-group effect at +5 (level-at-realized-dose scale)
  ## with the joint placebo p as the per-group diagnostic. Rows = groups,
  ## columns = outcomes; matches the main-table layout conventions.
  dcdh_path <- file.path(in_dir, "dcdh_bycell.csv")
  if (file.exists(dcdh_path)) {
    dc <- fread(dcdh_path)
    glab <- c(pooled = "Pooled Increase", incbind = "Inc-bind", incbecomes = "Inc-becomes")
    # Dep. mean per group x outcome, computed from the same panel subset the
    # dCDH estimator saw ({group's cells + NC}, complete cases on
    # outcome + controls). Needs the headline env (adds ~40s to this stage).
    dcdh_groups <- list(pooled = c("Inc-bind", "Inc-becomes", "Inc-nobind"),
                        incbind = "Inc-bind", incbecomes = "Inc-becomes")
    env_dm <- build_headline_env()
    pan_dm <- env_dm$panel[as.character(cell_muni) %in%
                             c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind") &
                             !is.na(resident_eit)]
    dep_means <- rbindlist(lapply(names(dcdh_groups), function(gn) {
      pg <- pan_dm[as.character(cell_muni) %in% c("NC", dcdh_groups[[gn]])]
      rbindlist(lapply(muni_outcomes, function(o) {
        cols <- c("muni", "year", "resident_eit", o, env_dm$ctrls)
        d <- pg[, ..cols][complete.cases(pg[, ..cols])]
        data.table(group = gn, outcome = o, dep_mean = mean(d[[o]], na.rm = TRUE))
      }))
    }))
    ncols <- length(muni_outcomes)
    body <- character(0)
    pick_dc <- function(gn, field, fmt) {
      out <- character(ncols)
      for (j in seq_along(muni_outcomes)) {
        r <- dc[group == gn & outcome == muni_outcomes[j] & rel == 5]
        out[j] <- if (nrow(r) == 0 || !(field %in% names(r)) || is.na(r[[field]][1])) "---"
                  else fmt(r[[field]][1])
      }
      out
    }
    fmt3 <- function(x) formatC(x, format = "f", digits = 3)
    fmtn <- function(x) format(as.integer(x), big.mark = ",")
    for (gn in names(glab)) {
      est_row <- character(ncols); se_row <- character(ncols)
      for (j in seq_along(muni_outcomes)) {
        r <- dc[group == gn & outcome == muni_outcomes[j] & rel == 5]
        if (nrow(r) == 0) { est_row[j] <- "---"; se_row[j] <- ""; next }
        pz <- 2 * pnorm(-abs(r$est[1] / r$se[1]))
        stars <- sig_stars(pz)
        est_row[j] <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
        se_row[j]  <- fmt_se(r$se[1])
      }
      body <- c(body,
                paste0("\\textbf{", glab[[gn]], "} & ", paste(est_row, collapse = " & "), " \\\\"),
                paste0("                  & ", paste(se_row, collapse = " & "), " \\\\"),
                # Lay explanation: the rows above give the alternative
                # estimator's total effect of a typical event; this row is
                # its dose-normalized version -- effect per percentage point
                # -- so its answer sits on the same per-pp scale as the
                # headline ACR and the two can be compared directly.
                paste0("\\quad Avg total effect per pp ($\\approx$ ACR) & ",
                       paste(pick_dc(gn, "ate", fmt3), collapse = " & "), " \\\\"),
                paste0("\\quad Joint placebo $p$ & ",
                       paste(pick_dc(gn, "joint_placebo_p", fmt3), collapse = " & "), " \\\\"),
                paste0("\\quad $N$ (at $k=+5$) & ",
                       paste(pick_dc(gn, "n_obs", fmtn), collapse = " & "), " \\\\"),
                paste0("\\quad Switchers & ",
                       paste(pick_dc(gn, "n_switchers", fmtn), collapse = " & "), " \\\\"))
    }
    hdr <- paste0("Group & ", paste(outcome_label(muni_outcomes, short = TRUE), collapse = " & "), " \\\\")
    tabular <- c(paste0("  \\begin{tabular}{l", strrep("c", ncols), "}"),
      "    \\toprule", paste0("    ", hdr), "    \\midrule", paste0("    ", body),
      "    \\bottomrule", "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c("\\begin{table}[H]", "  \\centering",
      "  \\caption{Cumulative effect at $k=+5$ by group, de Chaisemartin and D'Haultf\\oe uille estimator}",
      "  \\label{tab:dcdh_results}", "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.9}", tabular,
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
      paste0("    \\footnotesize \\textit{Notes:} Effects are levels at the realized dose, not per-point slopes, so compare them to the linear ACR times the group's mean dose. The per-pp row is already dose-normalized. Larger placebo $p$ means no detectable pre-trend. ",
             "Estimated with the \\texttt{continuous} option and analytical standard errors, which the package documentation notes can be liberal, so the standard errors are a lower bound. ",
             "Controls are partialled out within baseline-dose groups, so no control coefficients exist to report. ",
             "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$ (normal approximation)."),
      "  \\end{minipage}", "\\end{table}")
    writeLines(out, file.path(out_dir, "dcdh_results.tex"))
  }

  ## Event-study appendix full tables: lead/lag coefficients by event time,
  ## one table per group (Inc-bind / Inc-becomes / Inc-nobind / Pooled-Inc).
  write_es_full_table <- function(dt, group_label, label_id, out_name, diag = NULL) {
    if (is.null(dt) || nrow(dt) == 0) return(invisible())
    rels <- sort(unique(dt$rel))
    ncols <- length(muni_outcomes)
    body <- character(0)
    for (k in rels) {
      est_row <- character(ncols); se_row <- character(ncols); any_present <- FALSE
      for (j in seq_along(muni_outcomes)) {
        r <- dt[rel == k & outcome == muni_outcomes[j]]
        if (nrow(r) == 0) { est_row[j] <- "---"; se_row[j] <- ""; next }
        any_present <- TRUE
        stars <- sig_stars(r$p[1])
        est_row[j] <- if (nchar(stars) == 0) fmt_est(r$est[1]) else paste0(fmt_est(r$est[1]), "\\sym{", stars, "}")
        se_row[j]  <- fmt_se(r$se[1])
      }
      if (!any_present) next
      body <- c(body,
                paste0("$k = ", k, "$ & ", paste(est_row, collapse = " & "), " \\\\"),
                paste0("        & ", paste(se_row, collapse = " & "), " \\\\"))
    }
    # Diagnostics footer: one row per statistic, aligned to the outcome columns.
    if (!is.null(diag) && nrow(diag)) {
      pick <- function(col) vapply(muni_outcomes, function(o) {
        r <- diag[outcome == o]
        if (!nrow(r) || is.null(r[[col]]) || is.na(r[[col]][1])) return("---")
        v <- r[[col]][1]
        if (col == "n") formatC(v, format = "d", big.mark = ",")
        else if (col == "n_munis") formatC(v, format = "d")
        else sprintf("%.3f", v)
      }, character(1))
      dg <- character(0)
      for (cc in c("n", "n_munis", "r2", "wr2", "wald_pre_p")) {
        if (!cc %in% names(diag)) next
        lab <- c(n = "$N$", n_munis = "Municipalities", r2 = "$R^2$",
                 wr2 = "Within $R^2$", wald_pre_p = "Joint Wald $p$ (leads)")[[cc]]
        dg <- c(dg, paste0(lab, " & ", paste(pick(cc), collapse = " & "), " \\\\"))
      }
      if (length(dg)) body <- c(body, "\\midrule", dg)
    }
    hdr <- paste0("Event time & ", paste(outcome_label(muni_outcomes, short = TRUE), collapse = " & "), " \\\\")
    tabular <- c(paste0("  \\begin{tabular}{l", strrep("c", ncols), "}"),
      "    \\toprule", paste0("    ", hdr), "    \\midrule", paste0("    ", body),
      "    \\bottomrule", "  \\end{tabular}")
    if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
    out <- c("\\begin{table}[H]", "  \\centering",
      paste0("  \\caption{Realized-dose event study, full coefficients: ", group_label, "}"),
      paste0("  \\label{", label_id, "}"), "  \\footnotesize",
      "  \\renewcommand{\\arraystretch}{0.9}", tabular,
      "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
      "    \\footnotesize \\textit{Notes:} Per-pp realized-dose coefficient at each event time; $k=-1$ omitted as the reference. SE clustered by municipality. Joint Wald tests the four pre-event leads against zero. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.",
      "  \\end{minipage}", "\\end{table}")
    writeLines(out, file.path(out_dir, out_name))
  }
  es_coef_path <- file.path(in_dir, "headline_es_coefs.csv")
  if (file.exists(es_coef_path)) {
    ec <- fread(es_coef_path)
    ea_path <- file.path(in_dir, "headline_es_att.csv")
    ea <- if (file.exists(ea_path)) fread(ea_path) else NULL
    dsub <- function(cl) if (is.null(ea)) NULL else ea[cell == cl]
    write_es_full_table(ec[cell == "Inc-bind"],    "Inc-bind cell",    "tab:es_full_incbind",    "es_full_incbind.tex",    dsub("Inc-bind"))
    write_es_full_table(ec[cell == "Inc-becomes"], "Inc-becomes cell", "tab:es_full_incbecomes", "es_full_incbecomes.tex", dsub("Inc-becomes"))
    write_es_full_table(ec[cell == "Inc-nobind"],  "Inc-nobind cell (placebo)", "tab:es_full_incnobind", "es_full_incnobind.tex", dsub("Inc-nobind"))
  }
  pe_coef_path <- file.path(in_dir, "pooled_es_cousub_coefs.csv")
  if (file.exists(pe_coef_path)) {
    pe <- fread(pe_coef_path)
    if ("direction" %in% names(pe)) pe <- pe[direction == "Inc"]
    pa_path <- file.path(in_dir, "pooled_es_cousub_att.csv")
    pa <- if (file.exists(pa_path)) fread(pa_path) else NULL
    if (!is.null(pa) && "direction" %in% names(pa)) pa <- pa[direction == "Inc"]
    write_es_full_table(pe, "Pooled Increase", "tab:es_full_pooled", "es_full_pooled.tex", pa)
  }
})

# In-text numbers (build_intext_numbers.R) ----
# Reconcile the manuscript's headline IN-TEXT numbers to the script/CSV that
# produces each one, in a single table -> data/results/intext_numbers.csv
# (workbook sheet "in_text_numbers"). In-text numbers are the exhibits most often
# forgotten and most embarrassing when they don't reconcile, so this makes them a
# first-class, gate-checked artifact like the tables and figures. Validation gates
# (stopifnot) halt loudly if a headline number drifts.
# Run after stage 02. Sources every value from a CSV; nothing is hand-entered
# except the typical event doses, which the abstract itself states (0.24, 0.41 pp,
# the cumulative dose at +5 from the dose-trajectory figures).
# Lay explanation: the numbers quoted in the paper's sentences are the
# easiest ones to let drift when results change. This section re-derives
# each from its source CSV, compares it to what the prose claims, and stops
# the whole build with an error if any headline number no longer matches.
if (RUN_INTEXT_NUMBERS) local({
  in_dir <- RESULTS_DIR

  h <- fread(file.path(in_dir, "headline_full_table.csv"))
  g <- function(cell) h[term == sprintf("treat_cell::%s:dose_stack", cell) &
                        outcome == "log_live_and_work", est][1]
  ib  <- g("Inc-bind"); ibec <- g("Inc-becomes"); inb <- g("Inc-nobind")
  inb_p <- h[term == "treat_cell::Inc-nobind:dose_stack" & outcome == "log_live_and_work", p][1]
  N <- h[outcome == "log_live_and_work", n][1]
  dose_ib <- 0.243; dose_ibec <- 0.384  # cumulative dtau at +5 (cell_counts_summary.csv, dose_k5)

  rev <- fread(file.path(in_dir, "eit_revenue_offset.csv"))
  spl <- fread(file.path(in_dir, "eit_revenue_split.csv"))
  # the paper's "~36% offset" is the STANDARD-STRUCTURE (low-baseline) subgroup, not the
  # overall mean offset; the "five high-baseline" munis are Inc-bind-hi.
  off_lo <- spl[split == "cap_1pp" & cell == "Inc-bind-lo", offset][1]
  off_hi <- spl[split == "cap_1pp" & cell == "Inc-bind-hi", offset][1]

  pct <- function(b) round(100 * (exp(b) - 1), 1)
  intext <- data.table(
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
      round(ib, 3), pct(ib), round(dose_ib * ib, 3), pct(dose_ib * ib),
      round(ibec, 3), round(dose_ibec * ibec, 3), pct(dose_ibec * ibec),
      round(inb, 3), round(inb_p, 3), N,
      round(rev$beta_net[1], 3), round(off_lo, 3), round(off_hi, 3)),
    paper_says = c("-0.537", "-41.6%", "-0.131", "-12.2%",
                   "-0.250", "-0.096", "-9.2%", "null", "> 0.1", "—",
                   "1.04", "~ 37%", "(five high-baseline)"),
    source = c(rep("headline_full_table.csv  (sheet headline_decomp)", 9),
               "headline_full_table.csv",
               "eit_revenue_offset.csv  (sheet rev_offset)",
               "eit_revenue_split.csv  (sheet rev_split, cap_1pp Inc-bind-lo)",
               "eit_revenue_split.csv  (sheet rev_split, cap_1pp Inc-bind-hi)"))
  fwrite(intext, file.path(in_dir, "intext_numbers.csv"))

  # --- validation gates: halt if a headline in-text number drifts -------------
  stopifnot(
    "Inc-bind log_LW drifted from -0.537"    = abs(ib   - (-0.537)) < 0.01,
    "Inc-becomes log_LW drifted from -0.250" = abs(ibec - (-0.250)) < 0.01,
    "Inc-nobind placebo no longer null"      = inb_p > 0.10,
    "Estimation N changed from 171,451"      = N == 171451L,
    "EIT revenue net coef drifted from 1.04" = abs(rev$beta_net[1] - 1.041) < 0.02,
    "Low-baseline offset drifted from ~37%"  = abs(off_lo - 0.374) < 0.03)
  print(sprintf("[validation] in-text numbers reconcile: Inc-bind=%.3f (%.0f%%), Inc-becomes=%.3f, placebo p=%.2f, N=%s, rev_net=%.2f, offset_lo=%.0f%%",
                ib, pct(ib), ibec, inb_p, format(N, big.mark = ","), rev$beta_net[1], 100*off_lo))
})

# Results workbook (build_results_workbook.R) ----
# Collect the paper's result CSVs into ONE Excel workbook with an INDEX sheet and
# one sheet per result table -- including the realized-dose and pooled EVENT-STUDY
# results (destined for the paper). Output: results/all_results.xlsx
# Run after stages 02-03 so the ES + robustness CSVs are current. Missing CSVs are
# skipped, so it is safe to run on a partial pipeline.
if (RUN_WORKBOOK) local({
  in_dir  <- RESULTS_DIR
  out_dir <- file.path(ROOT, "results"); dir.create(out_dir, showWarnings = FALSE)
  out_xlsx <- file.path(out_dir, "all_results.xlsx")

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
    f <- file.path(in_dir, m[1]); sh <- m[2]; desc <- m[3]
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
               generated = "run scripts/06_tables.R (RUN_WORKBOOK) to refresh"))
  writeData(wb, "INDEX", idx, startRow = 3, headerStyle = hdr)
  setColWidths(wb, "INDEX", cols = 1:5, widths = c(20, 8, 8, 38, 90))
  worksheetOrder(wb) <- c(length(names(wb)), seq_len(length(names(wb)) - 1L))  # INDEX first
  saveWorkbook(wb, out_xlsx, overwrite = TRUE)
})


# ---------------------------------------------------------------------------
# Pooled-specification robustness battery (appendix)
# ---------------------------------------------------------------------------
# One table per check, mirroring the decomposition tables in Section 6.
local({
  outs <- c("share_live_in", "log_live_and_work", "log_in_commuters", "log_total_jobs")
  hdr  <- c("Share live in muni", "Log(work in own muni)",
            "Log(in-commuters)", "Log(total jobs)")
  TERM <- "direction_muni::Inc:dose_stack"

  wrap <- function(body, cap, lab, notes) c(
    "\\begin{table}[H]", "  \\centering", sprintf("  \\caption{%s}", cap),
    sprintf("  \\label{%s}", lab), "  \\small", "  \\resizebox{\\textwidth}{!}{",
    "  \\begin{tabular}{lcccc}", "    \\toprule",
    sprintf("    & %s \\\\", paste(hdr, collapse = " & ")), "    \\midrule",
    body, "    \\bottomrule", "  \\end{tabular}", "  }", "  \\vspace{0.3em}",
    "  \\begin{minipage}{0.95\\textwidth}",
    paste("    \\footnotesize \\textit{Notes:}", notes), "  \\end{minipage}", "\\end{table}")

  # `want` rather than `term`: the data.table has a column called `term`, and a
  # parameter of the same name shadows it inside the j-expression.
  rowpair <- function(x, label, want = TERM) {
    est <- se <- character(length(outs))
    for (j in seq_along(outs)) {
      r <- x[outcome == outs[j]][grepl(want, term, fixed = TRUE)]
      if (!nrow(r)) { est[j] <- "---"; se[j] <- ""; next }
      est[j] <- paste0(fmt_est(r$est[1]), "\\sym{", sig_stars(r$p[1]), "}")
      se[j]  <- fmt_se(r$se[1])
    }
    c(sprintf("    %s & %s \\\\", label, paste(est, collapse = " & ")),
      sprintf("     & %s \\\\", paste(se, collapse = " & ")))
  }

  ## Act 32
  f <- file.path(RESULTS_DIR, "pooled_act32.csv")
  if (file.exists(f)) {
    d <- fread(f); body <- character(0)
    for (pd in c("pre", "post")) {
      x <- d[period == pd]; if (!nrow(x)) next
      nt <- x$n_treated[1]
      body <- c(body, rowpair(x, sprintf("%s-Act 32 cohorts (%d munis)",
                                          tools::toTitleCase(pd), nt)))
    }
    writeLines(wrap(body,
      "Pooled Increase effect, split at the Act 32 enforcement break",
      "tab:pooled_act32",
      paste("Cohorts split at Act 32's effective year, 2012. Specification otherwise",
            "unchanged. SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")),
      file.path(TABLE_DIR, "pooled_act32.tex"))
    message("Saved tables/pooled_act32.tex")
  }

  ## Act 47
  f <- file.path(RESULTS_DIR, "pooled_act47.csv")
  if (file.exists(f)) {
    d <- fread(f); body <- character(0)
    for (sp in unique(d$spec)) {
      x <- d[spec == sp]; body <- c(body, rowpair(x, sprintf("%s", sp)))
    }
    writeLines(wrap(body,
      "Pooled Increase effect, excluding Act 47 distressed municipalities",
      "tab:pooled_act47",
      paste("Act 47 status from DCED's distressed register. Excluded from the treated",
            "group only. SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")),
      file.path(TABLE_DIR, "pooled_act47.tex"))
    message("Saved tables/pooled_act47.tex")
  }

  ## Fake timing
  f <- file.path(RESULTS_DIR, "pooled_fake_timing.csv")
  if (file.exists(f)) {
    d <- fread(f); body <- character(0)
    for (sh in sort(unique(d$shift))) {
      x <- d[shift == sh]; if (!nrow(x)) next
      nt <- x$n_treated[1]
      body <- c(body, sprintf("    \\multicolumn{5}{l}{\\textit{Placebo event at $c-%d$ (%d eligible munis)}} \\\\", sh, nt),
                rowpair(x, "\\quad Pooled Inc", want = "direction_muni::Inc:dose_fake"))
    }
    writeLines(wrap(body,
      "Pooled Increase effect at placebo event dates",
      "tab:pooled_fake_timing",
      paste("Placebo events 3, 4 or 5 years early, using only data before the real",
            "change, so every coefficient should be null. Counts fall with the shift",
            "because eligibility needs two placebo pre-years inside the panel. Panels",
            "overlap. SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.")),
      file.path(TABLE_DIR, "pooled_fake_timing.tex"))
    message("Saved tables/pooled_fake_timing.tex")
  }

  ## Pre-trend battery
  f <- file.path(RESULTS_DIR, "pooled_pretrend_battery.csv")
  if (file.exists(f)) {
    d <- fread(f)
    pick <- function(field, fmt) vapply(outs, function(o) {
      r <- d[outcome == o]; if (!nrow(r) || is.na(r[[field]][1])) "---" else fmt(r[[field]][1])
    }, character(1))
    f3 <- function(x) formatC(x, format = "f", digits = 3)
    body <- c(
      sprintf("    Joint Wald $p$ (leads) & %s \\\\", paste(pick("wald_joint_p", f3), collapse = " & ")),
      sprintf("    Leads significant at 5\\%% (of 4) & %s \\\\",
              paste(pick("n_leads_sig5", function(x) sprintf("%d", as.integer(x))), collapse = " & ")),
      sprintf("    Smallest lead $p$ & %s \\\\", paste(pick("min_lead_p", f3), collapse = " & ")),
      sprintf("    Linear pre-slope & %s \\\\", paste(pick("lin_slope", f3), collapse = " & ")),
      sprintf("    Linear pre-slope $p$ & %s \\\\", paste(pick("lin_slope_p", f3), collapse = " & ")),
      sprintf("    Clean & %s \\\\", paste(pick("robust_clean",
              function(x) ifelse(as.logical(x), "yes", "no")), collapse = " & ")))
    writeLines(wrap(body,
      "Pre-trend battery for the pooled Increase specification",
      "tab:pooled_pretrend_battery",
      paste("Tests on the four pre-event leads of the pooled realized-dose event study.",
            "Clean means the joint Wald does not reject at 10\\% and no single lead is",
            "significant at 5\\%.")),
      file.path(TABLE_DIR, "pooled_pretrend_battery.tex"))
    message("Saved tables/pooled_pretrend_battery.tex")
  }
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
