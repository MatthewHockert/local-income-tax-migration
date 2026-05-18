# build_paper_tables.R
# Build publication-style LaTeX table fragments from q3_unified output
# CSVs for direct \input{} into paper/main.tex and paper/appendix.tex.
#
# Each table reports coefficient (with sig stars) and SE (in parens
# below) per cell-by-outcome cell, plus a per-column diagnostic block
# (N, R^2, within R^2, dep mean, dep SD). Outcome columns are written
# out (no variable-form names). Equation is NOT included in table notes.
#
# Uses paper_helpers.R for the label map, formatters, and table builder.

suppressMessages({ library(data.table) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))

out_dir <- file.path(project_dir, "paper", "tables")
in_dir  <- file.path(project_dir, "data", "q3_unified")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Outcome and cell sets per level
# ---------------------------------------------------------------------------
muni_outcomes  <- c("log_live_and_work","log_in_commuters",
                    "log_total_residents","share_live_in")
block_outcomes        <- c("log_home_count","log_work_count",
                           "home_in_muni_share")
block_outcomes_main   <- c("log_home_count","log_work_count")
block_outcomes_shares <- c("home_in_muni_share")
cells_main     <- c("Inc-bind","Inc-becomes","Inc-nobind",
                    "Dec-bind","Dec-becomes-non","Dec-nobind")

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
    if ("side" %in% names(d)) {
      d <- d[side == .side_arg]
    } else if ("term" %in% names(d)) {
      d <- d[grepl(paste0("^", .side_arg, "::"), term)]
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

# ---------------------------------------------------------------------------
# 1. Headline tables (municipality + SD + block)
# ---------------------------------------------------------------------------
muni_notes <- c(
  paste("Continuous stacked-DiD with municipality-by-stack and",
        "year-by-stack fixed effects; standard errors clustered at the",
        "municipality level."),
  paste("Each column is a separate regression on the indicated outcome.",
        "Both muni-side and SD-side cell$\\times$dose interactions enter",
        "the same regression; this table reports the muni-side cells.")
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
               paste("SD-side cell$\\times$dose coefficients from the same",
                     "regression as the muni-side cell table; the muni-side",
                     "interaction is partialled out."))
)

sd_notes <- c(
  paste("Continuous stacked-DiD with SD-by-stack and year-by-stack fixed",
        "effects; standard errors clustered at the school-district level.",
        "Unit of observation is SD$\\times$year."),
  paste("Binding-cell classification uses the within-SD mean of",
        "$\\tau^{\\text{nr,muni}}$ as the work-side comparator,",
        "aggregating the institutional nonresident-rate field over the",
        "SD's constituent munis.")
)
write_cell_table(
  coef_csv = file.path(in_dir, "sd_level_results.csv"),
  diag_csv = file.path(in_dir, "sd_level_diagnostics.csv"),
  side     = "treat_cell",
  outcomes = muni_outcomes, cells = cells_main,
  caption  = "School-district-level dose-response by binding-state cell",
  label_id = "tab:headline_sd_cell_results",
  out_name = "headline_sd_cell_results.tex",
  notes    = sd_notes
)

block_notes <- c(
  paste("Continuous stacked-DiD with block-by-stack and year-by-stack fixed",
        "effects; standard errors two-way clustered at the muni and SD levels."),
  paste("Each block is exposed to one municipality and one school district;",
        "both rate-setters' policy variation enters at its native level.")
)
## Main (count outcomes only)
write_cell_table(
  coef_csv = file.path(in_dir, "block_joint_results.csv"),
  diag_csv = file.path(in_dir, "block_joint_diagnostics.csv"),
  side     = "muni",
  outcomes = block_outcomes_main, cells = cells_main,
  caption  = "Block-level dose-response: muni resident-rate cells (count outcomes)",
  label_id = "tab:headline_block_muni_results",
  out_name = "headline_block_muni_results.tex",
  notes    = c(block_notes,
               "Share outcomes are reported in the appendix; sparse block-level denominators make share estimates mechanically fragile.")
)
write_cell_table(
  coef_csv = file.path(in_dir, "block_joint_results.csv"),
  diag_csv = file.path(in_dir, "block_joint_diagnostics.csv"),
  side     = "sd",
  outcomes = block_outcomes_main, cells = cells_main,
  caption  = "Block-level dose-response: SD resident-rate cells (count outcomes)",
  label_id = "tab:headline_block_sd_results",
  out_name = "headline_block_sd_results.tex",
  notes    = c(block_notes,
               "Share outcomes are reported in the appendix.")
)

## Appendix (share outcomes only)
write_cell_table(
  coef_csv = file.path(in_dir, "block_joint_results.csv"),
  diag_csv = file.path(in_dir, "block_joint_diagnostics.csv"),
  side     = "muni",
  outcomes = block_outcomes_shares, cells = cells_main,
  caption  = "Block-level dose-response: muni resident-rate cells (share outcomes, supporting)",
  label_id = "tab:headline_block_muni_shares",
  out_name = "headline_block_muni_shares.tex",
  notes    = c(block_notes,
               "Block-level share outcomes are reported for completeness. Sparse denominators and endogenous denominator dynamics make these estimates mechanically fragile at block geography; they are not used to identify the headline causal effect.")
)
write_cell_table(
  coef_csv = file.path(in_dir, "block_joint_results.csv"),
  diag_csv = file.path(in_dir, "block_joint_diagnostics.csv"),
  side     = "sd",
  outcomes = block_outcomes_shares, cells = cells_main,
  caption  = "Block-level dose-response: SD resident-rate cells (share outcomes, supporting)",
  label_id = "tab:headline_block_sd_shares",
  out_name = "headline_block_sd_shares.tex",
  notes    = c(block_notes,
               "Block-level share outcomes are reported for completeness; not used for headline identification.")
)

# ---------------------------------------------------------------------------
# 2. Realized-dose ES (pre/post ATT)
# ---------------------------------------------------------------------------
write_es_att_table <- function(att_csv, outcomes, cells, caption, label_id,
                                 out_name, short = TRUE, notes = character(0)) {
  if (!file.exists(att_csv) || file.size(att_csv) == 0) {
    cat(sprintf("Skipping ES %s\n", out_name)); return(invisible())
  }
  a <- fread(att_csv)
  if ("att_post_per_pp" %in% names(a)) {
    setnames(a, c("att_post_per_pp","att_pre_per_pp"), c("att_post","att_pre"),
             skip_absent = TRUE)
  }
  safe_p <- function(est, se) {
    if (length(se) != 1 || is.na(se) || !is.finite(se) || se <= 0) return(NA_real_)
    if (length(est) != 1 || is.na(est) || !is.finite(est)) return(NA_real_)
    2 * pnorm(-abs(est / se))
  }
  rows <- list()
  for (cl in cells) {
    for (o in outcomes) {
      r <- a[outcome == o & cell == cl]
      if (nrow(r) == 0) next
      rows[[length(rows)+1L]] <- data.table(
        outcome = o, cell = paste0(cl, " (post)"),
        est = r$att_post, se = r$att_post_se,
        p = safe_p(r$att_post, r$att_post_se))
      rows[[length(rows)+1L]] <- data.table(
        outcome = o, cell = paste0(cl, " (pre)"),
        est = r$att_pre, se = r$att_pre_se,
        p = safe_p(r$att_pre, r$att_pre_se))
    }
  }
  coefs <- rbindlist(rows, fill = TRUE)
  row_labels <- as.character(unlist(lapply(cells,
                                            function(c) c(paste0(c, " (post)"),
                                                          paste0(c, " (pre)")))))
  diag <- data.table(outcome = outcomes,
                     n = NA_integer_, r2 = NA_real_, wr2 = NA_real_,
                     dep_mean = NA_real_, dep_sd = NA_real_)
  for (o in outcomes) {
    n_row <- a[outcome == o, max(n, na.rm = TRUE)]
    if (is.finite(n_row)) diag[outcome == o, n := as.integer(n_row)]
  }
  body <- build_pub_table(
    coefs_dt = coefs, diag_dt = diag,
    outcomes = outcomes, cells = row_labels,
    caption = caption, label = label_id,
    short_labels = short,
    diag_fields = "n",
    notes = c(notes,
              paste("Each cell reports two rows: post-event ATT (mean of",
                    "$\\beta_k$ for $k = 0..5$) and pre-event lead (mean of",
                    "$\\beta_k$ for $k \\leq -2$).")))
  writeLines(body, file.path(out_dir, out_name))
  cat(sprintf("Wrote %s\n", out_name))
}

write_es_att_table(file.path(in_dir, "headline_es_att.csv"),
                    muni_outcomes, cells_main,
                    "Realized-dose ES --- municipality level",
                    "tab:es_realized_cousub",
                    "es_realized_cousub.tex")
write_es_att_table(file.path(in_dir, "sd_es_att.csv"),
                    muni_outcomes, cells_main,
                    "Realized-dose ES --- school district level",
                    "tab:es_realized_sd",
                    "es_realized_sd.tex")
write_es_att_table(file.path(in_dir, "block_es_full_att.csv"),
                    block_outcomes, cells_main,
                    "Realized-dose ES --- block level (muni cohort)",
                    "tab:es_realized_block",
                    "es_realized_block.tex")

# ---------------------------------------------------------------------------
# 3. Binary Sun-Abraham IW ES (mirror of realized-dose)
# ---------------------------------------------------------------------------
write_sa_table <- function(att_csv, outcomes, cells, caption, label_id, out_name) {
  if (!file.exists(att_csv) || file.size(att_csv) == 0) {
    cat(sprintf("Skipping SA %s\n", out_name)); return(invisible())
  }
  a <- fread(att_csv)
  if (!"att_post" %in% names(a) || !"att_pre" %in% names(a)) {
    cat(sprintf("Skipping SA %s: malformed\n", out_name)); return(invisible())
  }
  safe_p <- function(est, se) {
    if (length(se) != 1 || is.na(se) || !is.finite(se) || se <= 0) return(NA_real_)
    if (length(est) != 1 || is.na(est) || !is.finite(est)) return(NA_real_)
    2 * pnorm(-abs(est / se))
  }
  rows <- list()
  for (cl in cells) for (o in outcomes) {
    r <- a[outcome == o & cell == cl]
    if (nrow(r) == 0) next
    rows[[length(rows)+1L]] <- data.table(
      outcome = o, cell = paste0(cl, " (post)"),
      est = r$att_post, se = r$att_post_se,
      p = safe_p(r$att_post, r$att_post_se))
    rows[[length(rows)+1L]] <- data.table(
      outcome = o, cell = paste0(cl, " (pre)"),
      est = r$att_pre, se = r$att_pre_se,
      p = safe_p(r$att_pre, r$att_pre_se))
  }
  coefs <- rbindlist(rows, fill = TRUE)
  row_labels <- as.character(unlist(lapply(cells,
                                            function(c) c(paste0(c, " (post)"),
                                                          paste0(c, " (pre)")))))
  diag <- data.table(outcome = outcomes,
                     n = NA_integer_, r2 = NA_real_, wr2 = NA_real_,
                     dep_mean = NA_real_, dep_sd = NA_real_)
  body <- build_pub_table(coefs, diag, outcomes, row_labels,
                            caption, label_id, short_labels = TRUE,
                            diag_fields = character(0),
                            notes = c("Binary Sun--Abraham interaction-weighted ES.",
                                       "Post = mean of $\\beta_k$ for $k=0..5$; pre = mean for $k\\leq-2$."))
  writeLines(body, file.path(out_dir, out_name))
  cat(sprintf("Wrote %s\n", out_name))
}
write_sa_table(file.path(in_dir, "sa_binary_cousub_att.csv"),
               muni_outcomes, cells_main,
               "Binary Sun--Abraham IW ES --- municipality level",
               "tab:sa_binary_cousub", "sa_binary_cousub.tex")
write_sa_table(file.path(in_dir, "sa_binary_sd_att.csv"),
               muni_outcomes, cells_main,
               "Binary Sun--Abraham IW ES --- school district level",
               "tab:sa_binary_sd", "sa_binary_sd.tex")
write_sa_table(file.path(in_dir, "sa_binary_block_att.csv"),
               block_outcomes, cells_main,
               "Binary Sun--Abraham IW ES --- block level",
               "tab:sa_binary_block", "sa_binary_block.tex")

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
write_act32_table(file.path(in_dir, "act32_split_sd.csv"),
                   file.path(in_dir, "act32_split_sd_diag.csv"),
                   muni_outcomes, cells_main,
                   "Pre-/post-Act-32 split --- school district level",
                   "tab:act32_sd", "act32_sd.tex")
write_act32_table(file.path(in_dir, "act32_split_block.csv"),
                   file.path(in_dir, "act32_split_block_diag.csv"),
                   block_outcomes, cells_main,
                   "Pre-/post-Act-32 split --- block level",
                   "tab:act32_block", "act32_block.tex")

# ---------------------------------------------------------------------------
# 5. Industry decomposition (SD aggregate)
# ---------------------------------------------------------------------------
if (file.exists(file.path(in_dir, "industry_sd.csv")) &&
    file.size(file.path(in_dir, "industry_sd.csv")) > 0) {
  ind_outcomes <- c("log_jobs_total","log_jobs_consumer","log_jobs_tradeable",
                     "share_consumer","share_tradeable")
  write_cell_table(
    coef_csv = file.path(in_dir, "industry_sd.csv"),
    diag_csv = file.path(in_dir, "industry_sd_diag.csv"),
    side     = "treat_cell",
    outcomes = ind_outcomes, cells = cells_main,
    caption  = "Industry decomposition --- school district level",
    label_id = "tab:industry_sd",
    out_name = "industry_sd.tex",
    notes    = c("Cousub-level industry job counts aggregated to SD via the muni--SD assignment."))
}

# ---------------------------------------------------------------------------
# 6. Tercile dose-magnitude
# ---------------------------------------------------------------------------
write_tercile_table <- function(coef_csv, caption, label_id, out_name) {
  if (!file.exists(coef_csv) || file.size(coef_csv) == 0) {
    cat(sprintf("Skipping tercile %s\n", out_name)); return(invisible())
  }
  a <- fread(coef_csv)
  outcomes <- unique(a$outcome)
  rows <- list()
  for (terc in c("small","medium","large")) for (o in outcomes) {
    r <- a[tercile == terc & outcome == o]
    if (nrow(r) == 0) next
    rows[[length(rows)+1L]] <- data.table(
      outcome = o, cell = terc, est = r$est, se = r$se, p = r$p)
  }
  coefs <- rbindlist(rows, fill = TRUE)
  row_labels <- c("small","medium","large")
  diag_csv <- sub("\\.csv$", "_diag.csv", coef_csv)
  if (file.exists(diag_csv) && file.size(diag_csv) > 0) {
    dd <- fread(diag_csv)
    diag <- dd[, .(n        = max(n, na.rm = TRUE),
                   r2       = mean(R2, na.rm = TRUE),
                   wr2      = mean(Within_R2, na.rm = TRUE),
                   dep_mean = mean(dep_mean, na.rm = TRUE),
                   dep_sd   = mean(dep_sd, na.rm = TRUE)),
                by = outcome]
    diag_fields <- c("n","r2","wr2","dep_mean","dep_sd")
  } else {
    diag <- data.table(outcome = outcomes,
                       n = NA_integer_, r2 = NA_real_, wr2 = NA_real_,
                       dep_mean = NA_real_, dep_sd = NA_real_)
    diag_fields <- character(0)
  }
  body <- build_pub_table(coefs, diag, outcomes, row_labels, caption, label_id,
                            short_labels = TRUE,
                            diag_fields = diag_fields,
                            notes = c(paste("Inc-bind units split by tercile of cumulative",
                                            "$\\Delta\\tau$ at $+5$. Per-pp dose-response",
                                            "should be approximately constant under linearity."),
                                      "Diagnostics ($N$, $R^2$, Dep.\\ mean/SD) are averaged across terciles within each outcome (FE structure is identical across terciles)."))
  writeLines(body, file.path(out_dir, out_name))
  cat(sprintf("Wrote %s\n", out_name))
}
write_tercile_table(file.path(in_dir, "tercile_cousub.csv"),
                     "Dose-magnitude linearity --- municipality level",
                     "tab:tercile_cousub", "tercile_cousub.tex")
write_tercile_table(file.path(in_dir, "tercile_sd.csv"),
                     "Dose-magnitude linearity --- school district level",
                     "tab:tercile_sd", "tercile_sd.tex")
write_tercile_table(file.path(in_dir, "tercile_block.csv"),
                     "Dose-magnitude linearity --- block level",
                     "tab:tercile_block", "tercile_block.tex")

# ---------------------------------------------------------------------------
# 7. Alt-control (Cengiz clean-in-window)
# ---------------------------------------------------------------------------
write_cell_table(
  coef_csv = file.path(in_dir, "altctrl_cousub.csv"),
  diag_csv = file.path(in_dir, "altctrl_cousub_diag.csv"),
  side     = "treat_cell",
  outcomes = muni_outcomes, cells = cells_main,
  caption  = "Alternative control (clean-in-window) --- municipality level",
  label_id = "tab:altctrl_cousub",
  out_name = "altctrl_cousub.tex",
  notes    = c("Control pool restricted to munis with no rate change anywhere in the event window."))
write_cell_table(
  coef_csv = file.path(in_dir, "altctrl_sd.csv"),
  diag_csv = file.path(in_dir, "altctrl_sd_diag.csv"),
  side     = "treat_cell",
  outcomes = muni_outcomes, cells = cells_main,
  caption  = "Alternative control (clean-in-window) --- school district level",
  label_id = "tab:altctrl_sd",
  out_name = "altctrl_sd.tex",
  notes    = c("Control pool restricted to SDs with no rate change anywhere in the event window."))
write_cell_table(
  coef_csv = file.path(in_dir, "altctrl_block.csv"),
  diag_csv = file.path(in_dir, "altctrl_block_diag.csv"),
  side     = "treat_cell",
  outcomes = block_outcomes, cells = cells_main,
  caption  = "Alternative control (clean-in-window) --- block level",
  label_id = "tab:altctrl_block",
  out_name = "altctrl_block.tex",
  notes    = c("Control pool restricted to blocks whose muni has no rate change in the event window."))

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
    for (dir in c("Inc","Dec")) {
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
           if (has_two_sides) " Each regression includes both rate-setters' direction-pooled treatments jointly; the four reported coefficients are $\\beta^M_{\\text{Inc}}$, $\\beta^M_{\\text{Dec}}$, $\\beta^{SD}_{\\text{Inc}}$, $\\beta^{SD}_{\\text{Dec}}$." else ""),
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
    "  \\small",
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
write_pooled_table("block",  "per_pp", block_outcomes,
                   "Pooled Inc/Dec headline --- block level (per pp)",
                   "tab:pooled_per_pp_block", "pooled_per_pp_block.tex")
write_pooled_table("cousub", "per_pp", muni_outcomes,
                   "Pooled Inc/Dec headline --- municipality level (per pp)",
                   "tab:pooled_per_pp_cousub", "pooled_per_pp_cousub.tex")
write_pooled_table("sd",     "per_pp", muni_outcomes,
                   "Pooled Inc/Dec headline --- school district level (per pp)",
                   "tab:pooled_per_pp_sd", "pooled_per_pp_sd.tex",
                   has_two_sides = TRUE,
                   notes = c("SD-level regression: $\\tau^{SD}$ is the school-district's own rate; the muni-rate row uses the within-SD mean of constituent muni resident rates as an aggregated treatment."))

# Binary pooled tables
write_pooled_table("block",  "binary", block_outcomes,
                   "Pooled Inc/Dec headline --- block level (binary event)",
                   "tab:pooled_binary_block", "pooled_binary_block.tex",
                   notes = c("Binary post-treatment indicator: 1 in the event year and after."))
write_pooled_table("cousub", "binary", muni_outcomes,
                   "Pooled Inc/Dec headline --- municipality level (binary event)",
                   "tab:pooled_binary_cousub", "pooled_binary_cousub.tex",
                   notes = c("Binary post-treatment indicator: 1 in the event year and after."))
write_pooled_table("sd",     "binary", muni_outcomes,
                   "Pooled Inc/Dec headline --- school district level (binary event)",
                   "tab:pooled_binary_sd", "pooled_binary_sd.tex",
                   has_two_sides = FALSE,
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
    diag_rows <- c(
      paste0("\\multicolumn{", ncols + 1, "}{l}{\\textit{Diagnostics}} \\\\"),
      paste0("$N$              & ", paste(pick("n"),       collapse = " & "), " \\\\"),
      paste0("$R^2$            & ", paste(pick("r2"),      collapse = " & "), " \\\\"),
      paste0("Within $R^2$     & ", paste(pick("wr2"),     collapse = " & "), " \\\\"),
      paste0("Dep.\\ mean       & ", paste(pick("dep_mean"), collapse = " & "), " \\\\"),
      paste0("Dep.\\ SD         & ", paste(pick("dep_sd"),   collapse = " & "), " \\\\")
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
    "  \\small",
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
  att_csv = file.path(in_dir, "pooled_es_block_att.csv"),
  row_var = direction_row, row_levels = c("Inc","Dec"),
  outcomes = block_outcomes,
  caption  = "Pooled realized-dose event study --- block level (muni-cohort).",
  label_id = "tab:es_pooled_block",
  out_name = "es_pooled_block_summary.tex",
  extra_notes = c("Stack built on muni-cohort years; event time defined against each block's muni-rate event."))

write_es_summary(
  att_csv = file.path(in_dir, "pooled_es_block_sd_att.csv"),
  row_var = direction_row, row_levels = c("Inc","Dec"),
  outcomes = block_outcomes,
  caption  = "Pooled realized-dose event study --- block level (SD-cohort).",
  label_id = "tab:es_pooled_block_sd",
  out_name = "es_pooled_block_sd_summary.tex",
  extra_notes = c("Stack built on SD-cohort years; event time defined against each block's SD-rate event."))

write_es_summary(
  att_csv = file.path(in_dir, "pooled_es_cousub_att.csv"),
  row_var = direction_row, row_levels = c("Inc","Dec"),
  outcomes = muni_outcomes,
  caption  = "Pooled realized-dose event study --- municipality level.",
  label_id = "tab:es_pooled_cousub",
  out_name = "es_pooled_cousub_summary.tex")

write_es_summary(
  att_csv = file.path(in_dir, "pooled_es_sd_att.csv"),
  row_var = direction_row, row_levels = c("Inc","Dec"),
  outcomes = muni_outcomes,
  caption  = "Pooled realized-dose event study --- school district level.",
  label_id = "tab:es_pooled_sd",
  out_name = "es_pooled_sd_summary.tex")

# Block per-cell ES summary
cell_row <- list(col = "cell", label = "Cell")
write_es_summary(
  att_csv = file.path(in_dir, "block_es_full_att.csv"),
  row_var = cell_row, row_levels = cells_main,
  outcomes = block_outcomes,
  caption  = "Realized-dose event study by binding-state cell --- block level (muni cohort).",
  label_id = "tab:es_block_by_cell",
  out_name = "es_block_by_cell_summary.tex",
  extra_notes = c("Per-cell decomposition of the pooled block-level ES; each cell is run as a separate regression on \\{NC, cell\\}."))

cat("\nDone. Tables written to paper/tables/.\n")
