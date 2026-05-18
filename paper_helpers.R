# paper_helpers.R
# Shared helpers for paper artifact generation: variable label map,
# ggplot theme + red/blue palette, and publication-style table formatter.
# Sourced by build_paper_tables.R, build_industry_figure.R, and the new
# block ES overlay figure script.

suppressMessages({
  library(data.table); library(ggplot2)
})

# ---------------------------------------------------------------------------
# Variable label map
# ---------------------------------------------------------------------------
# Maps raw column names to publication-quality outcome labels. Used in
# tables (column headers) and figures (facet labels / axis titles).

OUTCOME_LABELS <- c(
  # municipality / SD level
  log_live_and_work    = "Log(residents working in own municipality)",
  log_in_commuters     = "Log(in-commuters)",
  log_out_commuters    = "Log(out-commuters)",
  log_total_jobs       = "Log(total jobs)",
  log_total_residents  = "Log(employed residents)",
  share_live_in        = "Share of workers living in own muni",
  share_work_outside   = "Share of residents working outside",
  share_nonres         = "Share of workers who are nonresidents",
  # block level
  log_home_count       = "Log(workers living in block)",
  log_work_count       = "Log(workers at block)",
  home_in_muni_share   = "Share of block residents in own muni",
  # industry decomposition
  log_jobs_total       = "Log(total jobs)",
  log_jobs_consumer    = "Log(consumer-facing jobs)",
  log_jobs_tradeable   = "Log(tradeable jobs)",
  log_jobs_health_edu  = "Log(health/education jobs)",
  log_jobs_public      = "Log(public-sector jobs)",
  log_jobs_other       = "Log(other jobs)",
  share_consumer       = "Share of consumer-facing jobs",
  share_tradeable      = "Share of tradeable jobs"
)

# Short labels for narrow tables and figure facets
OUTCOME_LABELS_SHORT <- c(
  log_live_and_work    = "Log(work in own muni)",
  log_in_commuters     = "Log(in-commuters)",
  log_out_commuters    = "Log(out-commuters)",
  log_total_jobs       = "Log(total jobs)",
  log_total_residents  = "Log(employed residents)",
  share_live_in        = "Share live in muni",
  share_work_outside   = "Share work outside",
  share_nonres         = "Share nonres workers",
  log_home_count       = "Log(workers live)",
  log_work_count       = "Log(workers work)",
  home_in_muni_share   = "Share live+work in muni",
  log_jobs_total       = "Log(total jobs)",
  log_jobs_consumer    = "Log(consumer jobs)",
  log_jobs_tradeable   = "Log(tradeable jobs)",
  log_jobs_health_edu  = "Log(health/edu jobs)",
  log_jobs_public      = "Log(public jobs)",
  log_jobs_other       = "Log(other jobs)",
  share_consumer       = "Consumer share",
  share_tradeable      = "Tradeable share"
)

outcome_label <- function(x, short = FALSE) {
  lookup <- if (short) OUTCOME_LABELS_SHORT else OUTCOME_LABELS
  out <- lookup[x]
  # On fallback, use the raw name but escape underscores so it's TeX-safe.
  fallback <- gsub("_", "\\\\_", x[is.na(out)])
  out[is.na(out)] <- fallback
  unname(out)
}

# ---------------------------------------------------------------------------
# ggplot theme + red/blue palette
# ---------------------------------------------------------------------------
# Color convention used throughout the paper:
#   red  = muni-side coefficient / muni cohort
#   blue = SD-side coefficient / SD cohort

PAPER_RED  <- "#C0392B"   # muted firebrick
PAPER_BLUE <- "#1F4E79"   # navy blue
PAPER_GREY <- "grey50"

theme_paper <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text       = element_text(face = "bold", size = base_size - 1),
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(1.4, "lines"),
      axis.title       = element_text(size = base_size - 1)
    )
}

# ---------------------------------------------------------------------------
# Wald joint pre-trend test
# ---------------------------------------------------------------------------
# Given a fitted fixest model `m` and a regex matching pre-event coefficient
# names (e.g., the rel::-5, rel::-4, ... rel::-2 set in a realized-dose ES),
# return a list with the joint Wald statistic, df, and p-value.
#
# Uses the model's cluster-robust vcov to construct the joint test:
#   W = b' V^{-1} b  ~ chi-squared(k)
#
# Returns list(stat, df, p, terms). Returns list(NA, NA, NA, character(0))
# if fewer than 2 matching terms or the V is singular.
wald_pretrend <- function(m, term_regex = "^rel::-[2-9]") {
  coef_names <- names(coef(m))
  pre_idx <- grep(term_regex, coef_names)
  if (length(pre_idx) < 2)
    return(list(stat = NA_real_, df = 0L, p = NA_real_,
                terms = coef_names[pre_idx]))
  b <- coef(m)[pre_idx]
  V <- vcov(m)[pre_idx, pre_idx, drop = FALSE]
  inv <- tryCatch(solve(V), error = function(e) NULL)
  if (is.null(inv))
    return(list(stat = NA_real_, df = length(pre_idx),
                p = NA_real_, terms = names(b)))
  stat <- as.numeric(t(b) %*% inv %*% b)
  p <- pchisq(stat, df = length(b), lower.tail = FALSE)
  list(stat = stat, df = length(b), p = p, terms = names(b))
}

scale_color_side <- function(...) {
  scale_color_manual(values = c(Municipality = PAPER_RED,
                                "School district" = PAPER_BLUE,
                                "School District" = PAPER_BLUE,
                                muni = PAPER_RED, sd = PAPER_BLUE,
                                Muni = PAPER_RED, SD = PAPER_BLUE),
                     ...)
}
scale_fill_side <- function(...) {
  scale_fill_manual(values = c(Municipality = PAPER_RED,
                               "School district" = PAPER_BLUE,
                               "School District" = PAPER_BLUE,
                               muni = PAPER_RED, sd = PAPER_BLUE,
                               Muni = PAPER_RED, SD = PAPER_BLUE),
                    ...)
}

# ---------------------------------------------------------------------------
# Number formatters
# ---------------------------------------------------------------------------
sig_stars <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.10] <- "*"
  out[!is.na(p) & p < 0.05] <- "**"
  out[!is.na(p) & p < 0.01] <- "***"
  out
}
fmt_est <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return("")
  ifelse(is.na(x), "---", formatC(x, format = "f", digits = digits))
}
fmt_se <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return("")
  ifelse(is.na(x), "", sprintf("(%s)", formatC(x, format = "f", digits = digits)))
}
fmt_n <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  ifelse(is.na(x), "---", formatC(as.numeric(x), format = "d", big.mark = ","))
}
fmt_pct <- function(x, digits = 3) {
  if (is.null(x)) return("")
  ifelse(is.na(x), "---", formatC(x, format = "f", digits = digits))
}

# ---------------------------------------------------------------------------
# Publication-style table formatter
# ---------------------------------------------------------------------------
# Build a LaTeX table fragment in publication style:
#   - columns = outcomes (using OUTCOME_LABELS or _SHORT)
#   - rows    = cells (binding-state categories); each cell takes TWO lines:
#               coefficient (with sig stars) on the first, SE in parens on
#               the second
#   - per-column diagnostics block at the bottom: N, R^2, within R^2,
#               dep mean, dep SD
#   - no equation in the note
#
# Arguments:
#   coefs_dt   data.table with columns: outcome, cell, est, se, p
#              (long format). Cells are factored in cell_order.
#   diag_dt    data.table with columns: outcome, n, r2, wr2, dep_mean, dep_sd
#              (one row per outcome). Optional cols: n_clusters, fe_label,
#              cluster_label.
#   outcomes   character vector of outcomes (column order) -- looked up in
#              OUTCOME_LABELS.
#   cells      character vector of cells (row order).
#   caption    table caption.
#   label      \label tag.
#   short_labels  if TRUE, use OUTCOME_LABELS_SHORT in column headers.
#   notes      character vector of additional sentences for the note.
#   col_spec   optional column spec override (defaults to lc..c with one c per outcome).
#
# Returns:  a character vector (one element = one line of LaTeX).
build_pub_table <- function(coefs_dt, diag_dt, outcomes, cells,
                              caption, label, short_labels = TRUE,
                              notes = character(0), col_spec = NULL,
                              sideways = FALSE,
                              diag_fields = c("n","r2","wr2","dep_mean","dep_sd"),
                              controls_dt = NULL, control_labels = NULL,
                              section_header_above_cells = NULL) {
  stopifnot(is.data.table(coefs_dt), is.data.table(diag_dt))
  coefs_dt <- copy(coefs_dt)[, outcome := as.character(outcome)]
  diag_dt  <- copy(diag_dt)
  if ("outcome" %in% names(diag_dt)) diag_dt[, outcome := as.character(outcome)]

  if (is.null(col_spec)) {
    col_spec <- paste0("l", strrep("c", length(outcomes)))
  }

  # Header row of outcome labels
  hdr_labels <- outcome_label(outcomes, short = short_labels)
  hdr <- paste0("Outcome & ", paste(hdr_labels, collapse = " & "), " \\\\")

  # Coefficient rows. Rows whose every outcome cell is missing/NA are
  # dropped from the table and listed in a "missing rows" footnote.
  body <- character(0)
  dropped_cells <- character(0)
  for (cl in cells) {
    est_vec <- character(length(outcomes))
    se_vec  <- character(length(outcomes))
    any_present <- FALSE
    for (j in seq_along(outcomes)) {
      o <- outcomes[j]
      row <- coefs_dt[outcome == o & cell == cl]
      if (nrow(row) == 0) {
        est_vec[j] <- "---"; se_vec[j] <- ""
      } else {
        est1 <- row$est[1]; se1 <- row$se[1]; p1 <- row$p[1]
        if (is.na(est1) || !is.finite(est1)) {
          est_vec[j] <- "---"; se_vec[j] <- ""
        } else {
          any_present <- TRUE
          stars <- sig_stars(p1)
          est_vec[j] <- if (nchar(stars) == 0)
                           fmt_est(est1)
                         else
                           paste0(fmt_est(est1), "\\sym{", stars, "}")
          se_vec[j]  <- fmt_se(se1)
        }
      }
    }
    if (!any_present) {
      dropped_cells <- c(dropped_cells, cl)
      next
    }
    body <- c(body,
              paste0("\\textbf{", cl, "} & ", paste(est_vec, collapse = " & "), " \\\\"),
              paste0("                  & ", paste(se_vec,  collapse = " & "), " \\\\"))
  }
  if (length(dropped_cells) > 0) {
    notes <- c(notes,
               paste0("Rows omitted (no estimable coefficients in any outcome",
                      " column): ", paste(dropped_cells, collapse = ", "),
                      ". These cells contain too few units, or all",
                      " coefficients were dropped for collinearity with the",
                      " stack-interacted fixed effects."))
  }

  # Controls block (optional). Adds rows after the cell body.
  controls_block <- character(0)
  if (!is.null(controls_dt) && is.data.table(controls_dt) && nrow(controls_dt) > 0) {
    ncols <- length(outcomes)
    mc <- paste0("\\multicolumn{", ncols + 1, "}{l}")
    controls_block <- c(controls_block,
                        "\\midrule",
                        paste0(mc, "{\\textit{Fiscal controls}} \\\\"))
    ctrl_terms <- unique(controls_dt$term)
    label_map <- if (!is.null(control_labels)) control_labels else setNames(ctrl_terms, ctrl_terms)
    for (tm in ctrl_terms) {
      est_vec <- character(ncols); se_vec <- character(ncols)
      any_present <- FALSE
      for (j in seq_along(outcomes)) {
        r <- controls_dt[outcome == outcomes[j] & term == tm]
        if (nrow(r) == 0) { est_vec[j] <- "---"; se_vec[j] <- ""; next }
        e <- r$est[1]; se1 <- r$se[1]; p1 <- r$p[1]
        if (is.na(e)) { est_vec[j] <- "---"; se_vec[j] <- ""; next }
        any_present <- TRUE
        stars <- sig_stars(p1)
        est_vec[j] <- if (nchar(stars) == 0) fmt_est(e) else paste0(fmt_est(e), "\\sym{", stars, "}")
        se_vec[j]  <- fmt_se(se1)
      }
      if (!any_present) next
      pretty <- if (tm %in% names(label_map)) label_map[[tm]] else gsub("_", "\\\\_", tm)
      controls_block <- c(controls_block,
                          paste0(pretty, " & ", paste(est_vec, collapse = " & "), " \\\\"),
                          paste0("                  & ", paste(se_vec,  collapse = " & "), " \\\\"))
    }
  }

  # Per-outcome diagnostics block
  diag_rows <- function(field, label_text, fmt = function(x) formatC(x, format = "f", digits = 3)) {
    vals <- character(length(outcomes))
    for (j in seq_along(outcomes)) {
      o <- outcomes[j]
      row <- if ("outcome" %in% names(diag_dt)) diag_dt[outcome == o] else diag_dt[0]
      vals[j] <- if (nrow(row) == 0 || !(field %in% names(row))) "---" else {
        x <- row[[field]]
        if (is.null(x) || length(x) == 0 || all(is.na(x))) "---" else fmt(x[1])
      }
    }
    paste0(label_text, " & ", paste(vals, collapse = " & "), " \\\\")
  }
  diag_lookup <- list(
    n        = list(field = "n",        label = "$N$",         fmt = fmt_n),
    r2       = list(field = "r2",       label = "$R^2$",       fmt = function(x) formatC(x, format = "f", digits = 3)),
    wr2      = list(field = "wr2",      label = "Within $R^2$",fmt = function(x) formatC(x, format = "f", digits = 3)),
    dep_mean = list(field = "dep_mean", label = "Dep.\\ mean", fmt = function(x) formatC(x, format = "f", digits = 3)),
    dep_sd   = list(field = "dep_sd",   label = "Dep.\\ SD",   fmt = function(x) formatC(x, format = "f", digits = 3))
  )
  diag_block <- character(0)
  for (k in diag_fields) {
    if (k %in% names(diag_lookup)) {
      spec <- diag_lookup[[k]]
      diag_block <- c(diag_block, diag_rows(spec$field, spec$label, fmt = spec$fmt))
    }
  }

  note_default <- "Cluster-robust standard errors in parentheses."
  note_sig     <- "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  note_all     <- paste(c(note_default, notes, note_sig), collapse = " ")

  # Auto-fit wide tables: if there are >= 4 outcome columns, wrap the
  # tabular in \resizebox{\textwidth}{!} so the table never overflows the
  # text width. Narrower tables are typeset at native size.
  wide <- length(outcomes) >= 3
  tabular <- c(
    paste0("  \\begin{tabular}{", col_spec, "}"),
    "    \\toprule",
    paste0("    ", hdr),
    "    \\midrule",
    paste0("    ", body),
    if (length(controls_block)) paste0("    ", controls_block) else character(0),
    if (length(diag_block)) "    \\midrule" else character(0),
    if (length(diag_block)) paste0("    ", diag_block) else character(0),
    "    \\bottomrule",
    "  \\end{tabular}"
  )
  if (wide) tabular <- c("  \\resizebox{\\textwidth}{!}{",
                          tabular, "  }")
  out <- c(
    if (sideways) "\\begin{sidewaystable}" else "\\begin{table}[!htbp]",
    "  \\centering",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    "  \\footnotesize",
    "  \\renewcommand{\\arraystretch}{0.9}",
    tabular,
    "  \\vspace{0.3em}",
    "  \\begin{minipage}{0.95\\textwidth}",
    paste0("    \\footnotesize \\textit{Notes:} ", note_all),
    "  \\end{minipage}",
    if (sideways) "\\end{sidewaystable}" else "\\end{table}"
  )
  out
}

# ---------------------------------------------------------------------------
# Helper: read a results CSV and a diagnostics CSV, return list with both
# coerced to data.table.
# ---------------------------------------------------------------------------
read_results <- function(results_path, diag_path = NULL) {
  r <- as.data.table(read.csv(results_path, stringsAsFactors = FALSE))
  d <- if (!is.null(diag_path) && file.exists(diag_path))
         as.data.table(read.csv(diag_path,  stringsAsFactors = FALSE))
       else data.table()
  list(coefs = r, diag = d)
}
