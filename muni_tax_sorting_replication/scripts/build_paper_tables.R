# build_paper_tables.R
# Build publication-style LaTeX table fragments from q3_unified output
# CSVs for direct \input{} into the paper.
#
# COUSUB-ONLY (repo-cleanup, 2026-05): builders for the retired SD-unit,
# block-unit, industry, Sun-Abraham binary, tercile, alt-control, and the
# block/SD pooled & ES & appendix-full tables were removed, together with
# the helper functions only those builders used (write_es_att_table,
# write_sa_table, write_tercile_table, write_full_spec_table). What remains
# builds only the cousub/municipality fragments the kept papers \input
# (headline_muni_combined, act32_cousub, pooled_per_pp_cousub,
# es_pooled_cousub_summary, es_realized_cousub, app_full_muni_cell_results)
# plus a few muni-level supporting tables that read kept inputs. All consumed
# fragments are byte-identical to the pre-trim build. Prior version with the
# full 3-level table set is in git history.
#
# Each table reports coefficient (with sig stars) and SE (in parens
# below) per cell-by-outcome cell, plus a per-column diagnostic block
# (N, R^2, within R^2, dep mean, dep SD). Outcome columns are written
# out (no variable-form names). Equation is NOT included in table notes.
#
# Uses paper_helpers.R for the label map, formatters, and table builder.

suppressMessages({ library(data.table) })

# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
source(file.path(SCRIPT_DIR, "paper_helpers.R"))

out_dir <- file.path(project_dir, "tables")
in_dir  <- file.path(project_dir, "data", "q3_unified")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Outcome and cell sets per level
# ---------------------------------------------------------------------------
muni_outcomes  <- c("log_live_and_work","log_in_commuters",
                    "log_total_jobs","share_live_in")
block_outcomes        <- c("log_home_count","log_work_count",
                           "home_in_muni_share")
block_outcomes_main   <- c("log_home_count","log_work_count")
block_outcomes_shares <- c("home_in_muni_share")
cells_main     <- c("Inc-bind","Inc-becomes","Inc-nobind")

# ---------------------------------------------------------------------------
# Read + parse helpers (data CSVs come in slightly different formats)
# ---------------------------------------------------------------------------
read_coefs <- function(coef_csv, side = NULL) {
  if (!file.exists(coef_csv)) return(NULL)
  d <- fread(coef_csv)
  # Parse cell column
  if (!"cell" %in% names(d)) {
    if ("term" %in% names(d)) {
      d[, cell := sub(".*::([^:]+):.*", "\\1", term)]
    }
  }
  # Filter by side. Rename the parameter to avoid data.table column masking:
  # if `side` is also a column name, `d[d$side == side]` evaluates RHS in
  # the data.table's column scope, becoming `column == column` (always TRUE).
  if (!is.null(side)) {
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
    cat(sprintf("Skipping %s: source CSV missing or empty\n", out_name))
    return(invisible())
  }
  diag <- read_diag(diag_csv)
  body <- build_pub_table(
    coefs_dt  = coefs,
    diag_dt   = diag,
    outcomes  = outcomes,
    cells     = cells,
    caption   = caption,
    label     = label_id,
    short_labels = short,
    notes     = notes
  )
  writeLines(body, file.path(out_dir, out_name))
  cat(sprintf("Wrote %s\n", out_name))
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

# Pretty labels for controls used in appendix full-spec tables
muni_ctrl_labels <- c(
  share_police   = "police-services share",
  share_streets  = "streets share",
  share_debt_svc = "debt-service share",
  share_eit      = "EIT-revenue share",
  share_re       = "real-estate-tax share",
  share_intergov = "intergovernmental-revenue share"
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
    cat(sprintf("Skipping %s: source CSV missing\n", out_name)); return(invisible())
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
      paste0("Within $R^2$  & ", paste(pick("wr2"),      collapse = " & "), " \\\\"),
      paste0("Dep.\\ mean    & ", paste(pick("dep_mean"), collapse = " & "), " \\\\"),
      paste0("Dep.\\ SD      & ", paste(pick("dep_sd"),   collapse = " & "), " \\\\")
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
    "\\begin{table}[!htbp]",
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
  cat(sprintf("Wrote %s\n", out_name))
}

# write_merged_full_spec_table: one table with the muni-side and SD-side cells
# as two panels (both from the same joint regression), the shared fiscal
# controls, and diagnostics. Used for the combined Appendix A cousub table.
write_merged_full_spec_table <- function(coef_csv, diag_csv, ctrl_csv, ctrl_labels,
                                          outcomes, cells, caption, label_id, out_name,
                                          short = TRUE, notes = character(0)) {
  coefs_muni <- read_coefs(coef_csv, side = "muni")
  coefs_sd   <- read_coefs(coef_csv, side = "sd")
  if (is.null(coefs_muni) || nrow(coefs_muni) == 0) {
    cat(sprintf("Skipping %s: source CSV missing\n", out_name)); return(invisible())
  }
  diag     <- read_diag(diag_csv)
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
    diag_block <- c("\\midrule", paste0(mc, "{\\textit{Diagnostics}} \\\\"),
      paste0("$N$           & ", paste(pick("n"),        collapse = " & "), " \\\\"),
      paste0("$R^2$         & ", paste(pick("r2"),       collapse = " & "), " \\\\"),
      paste0("Within $R^2$  & ", paste(pick("wr2"),      collapse = " & "), " \\\\"),
      paste0("Dep.\\ mean    & ", paste(pick("dep_mean"), collapse = " & "), " \\\\"),
      paste0("Dep.\\ SD      & ", paste(pick("dep_sd"),   collapse = " & "), " \\\\"))
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
  out <- c("\\begin{table}[!htbp]", "  \\centering",
    paste0("  \\caption{", caption, "}"), paste0("  \\label{", label_id, "}"),
    "  \\footnotesize", "  \\renewcommand{\\arraystretch}{0.9}", tabular,
    "  \\vspace{0.3em}", "  \\begin{minipage}{0.95\\textwidth}",
    paste0("    \\footnotesize \\textit{Notes:} ", notes_all),
    "  \\end{minipage}", "\\end{table}")
  writeLines(out, file.path(out_dir, out_name))
  cat(sprintf("Wrote %s\n", out_name))
}

# ---------------------------------------------------------------------------
# 1. Headline tables (municipality + SD + block)
# ---------------------------------------------------------------------------
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

# Combined muni + SD coefficients from the same joint regression in one table
write_merged_cell_table(
  coef_csv = file.path(in_dir, "headline_full_table.csv"),
  diag_csv = file.path(in_dir, "headline_diagnostics.csv"),
  outcomes = muni_outcomes, cells = cells_main,
  caption  = "Municipality-level dose-response: muni and SD resident-rate cells (joint regression)",
  label_id = "tab:headline_muni_combined",
  out_name = "headline_muni_combined.tex",
  notes    = muni_notes
)



# Cousub per-cell realized-dose ES (tab:es_realized_cousub) is built below via
# write_es_summary so it carries the joint pre-trend Wald p row; it backs the
# decomposition-diagnostic prose in section 5.


# ---------------------------------------------------------------------------
# 4. Pre/Post Act 32
# ---------------------------------------------------------------------------
write_act32_table <- function(coef_csv, diag_csv, outcomes, cells,
                                caption, label_id, out_name, notes = character(0)) {
  if (!file.exists(coef_csv) || file.size(coef_csv) == 0) {
    cat(sprintf("Skipping Act32 %s\n", out_name)); return(invisible())
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
      for (field in c("n","r2","wr2","dep_mean","dep_sd")) {
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
    "\\begin{table}[!htbp]",
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
  cat(sprintf("Wrote %s\n", out_name))
}
write_act32_table(file.path(in_dir, "act32_split_cousub.csv"),
                   file.path(in_dir, "act32_split_cousub_diag.csv"),
                   muni_outcomes, cells_main,
                   "Pre-/post-Act-32 split --- municipality level",
                   "tab:act32_cousub", "act32_cousub.tex")




# ---------------------------------------------------------------------------
# 8. Pooled Inc/Dec headline tables
# ---------------------------------------------------------------------------
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
    cat(sprintf("Skipping %s: pooled_results.csv missing\n", out_name))
    return(invisible())
  }
  .lev <- level_tag; .mod <- spec_mode
  a <- fread(res_path)[level == .lev & mode == .mod]
  d <- if (file.exists(diag_path)) fread(diag_path)[level == .lev & mode == .mod]
       else data.table()
  if (nrow(a) == 0) { cat(sprintf("No rows for %s/%s\n", level_tag, mode)); return(invisible()) }

  # Build hierarchical rows: side header → Inc subrow → Dec subrow
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
  # Diagnostics: per-outcome N, R², dep mean, dep SD (from pooled_diagnostics.csv)
  diag_block <- character(0)
  if (nrow(d) > 0) {
    for (field in c("n","r2","wr2","dep_mean","dep_sd")) {
      label <- switch(field,
                      n = "$N$", r2 = "$R^2$",
                      wr2 = "Within $R^2$",
                      dep_mean = "Dep.\\ mean",
                      dep_sd   = "Dep.\\ SD")
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
    if (length(diag_block)) "    \\midrule" else character(0),
    if (length(diag_block)) paste0("    ", diag_block) else character(0),
    "    \\bottomrule",
    "  \\end{tabular}")
  if (ncols >= 3) tabular <- c("  \\resizebox{\\textwidth}{!}{", tabular, "  }")
  out <- c(
    "\\begin{table}[!htbp]",
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
  cat(sprintf("Wrote %s\n", out_name))
}

# Per-pp pooled tables
write_pooled_table("cousub", "per_pp", muni_outcomes,
                   "Pooled Inc/Dec headline --- municipality level (per pp)",
                   "tab:pooled_per_pp_cousub", "pooled_per_pp_cousub.tex")

# Binary pooled tables
write_pooled_table("cousub", "binary", muni_outcomes,
                   "Pooled Inc/Dec headline --- municipality level (binary event)",
                   "tab:pooled_binary_cousub", "pooled_binary_cousub.tex",
                   notes = c("Binary post-treatment indicator: 1 in the event year and after."))

# ---------------------------------------------------------------------------
# 9. ES summary tables (post-mean / pre-mean ATT with Wald pre-trend p)
# ---------------------------------------------------------------------------
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
    cat(sprintf("Skipping %s: %s missing\n", out_name, att_csv)); return(invisible())
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
                   else formatC(v, format = "f", digits = 4)
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
      diag_line("dep_mean", "Dep.\\ mean      "),
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
    "\\begin{table}[!htbp]",
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
  cat(sprintf("Wrote %s\n", out_name))
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

# ===========================================================================
# Appendix A: full-spec tables (cells + fiscal controls, same regression)
# ===========================================================================
cat("\n--- Building Appendix A full-spec tables (cells + controls) ---\n")

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



cat("\nDone. Tables written to paper/tables/.\n")
