#!/usr/bin/env Rscript
# 05_figures.R -- Every figure and descriptive table-figure, inline and flag-gated.
# Consolidates: build_summary_stats.R, build_uea_aggregate_figures.R,
#   build_dose_trajectory_inc_becomes.R, build_appendix_es_figures.R,
#   build_extra_figures.R, build_acr_acrt_att_spline.R, build_feols_es_bycell.R,
#   build_law_timeline.R.
# Run AFTER 02+03 (several sections read their result CSVs). The headline
# environment (panel + muni stack + controls) is built ONCE via
# build_headline_env() from _helpers.R and shared by the sections that
# previously sys.source()-d q3_unified_full_table.R.
# Outputs -> figures/*.png, tables/summary_*.tex, data/results/*.csv mirrors.
# Run from package root:  Rscript scripts/05_figures.R

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(sf)
  library(ggrepel)
  library(splines)
})

.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "_helpers.R"))

# Flags ----
RUN_SUMMARY_STATS        <- TRUE   # summary tables + rate ECDF/CDF + map + binding-levels figs
RUN_AGGREGATE_FIGS       <- TRUE   # pooled ES + dose-trajectory aggregate figures
RUN_DOSE_TRAJECTORY      <- TRUE   # Inc-becomes dose-trajectory figure
RUN_APPENDIX_ES_FIGS     <- TRUE   # unified ribbon-style ES figures (appendix set)
RUN_EXTRA_FIGS           <- TRUE   # descriptive rate-change scatters, spaghetti, histograms
RUN_SPLINE_DOSE_RESPONSE <- TRUE   # ns(dose) ATT/ACRT dose-response (pooled, Inc-bind, Inc-becomes)
RUN_FEOLS_ES_BYCELL      <- TRUE   # feols realized-dose ES by group (twin of dcdh_bycell_es)
RUN_LAW_TIMELINE         <- TRUE   # PA local-tax statute timeline
RUN_DESIGN_SCHEMATICS    <- TRUE   # conceptual design schematics (max-rule, dose vs tag)
RUN_TAX_MIX_FIG          <- TRUE   # tax-revenue mix by source, muni + SD panels

# Shared headline environment ----
# Built once for the sections that need the classified cousub panel / muni
# stack (previously each sys.source()-d q3_unified_full_table.R).
needs_env <- RUN_SUMMARY_STATS || RUN_AGGREGATE_FIGS || RUN_DOSE_TRAJECTORY ||
  RUN_SPLINE_DOSE_RESPONSE || RUN_FEOLS_ES_BYCELL
if (needs_env) {
  env <- build_headline_env()
}

# Summary stats (build_summary_stats.R) ----
# Descriptive-statistics tables and figures for section 4.5 of the paper.
# Emits: tables/summary_{muni,block,sd}.tex, tables/cell_counts_summary.tex,
#   figures/rate_double_histogram.png, figures/act32_cdf.png,
#   figures/rate_changes_by_year.png, figures/pa_rate_trajectories.png,
#   figures/pa_map.png, figures/binding_levels_by_year.png
if (RUN_SUMMARY_STATS) local({
  fig_dir <- OUT_DIR
  tab_dir <- TABLE_DIR

  max_year <- 2019L

  # Load panels
  rate    <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  cousub  <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_panel.rds")))
  block   <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_block_panel.rds")))
  sd_ctrl <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_sd_controls.rds")))
  afr_ok  <- file.exists(file.path(RAW_DIR, "pa_afr_panel.csv"))
  if (afr_ok) afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))

  rate   <- rate[year <= max_year]
  cousub <- cousub[year <= max_year]
  block  <- block[year <= max_year]

  # Helper: numeric summary row
  summarize_vec <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(rep(NA_real_, 7))
    c(N = length(x), mean = mean(x), sd = sd(x),
      p25 = quantile(x, 0.25), p50 = median(x), p75 = quantile(x, 0.75),
      min = min(x), max = max(x))
  }

  write_summary_table <- function(rows, caption, label_id, out_name, notes = character(0)) {
    rows_dt <- rbindlist(rows, fill = TRUE)
    body <- c(
      "\\begin{table}[H]", "  \\centering",
      sprintf("  \\caption{%s}", caption),
      sprintf("  \\label{%s}", label_id),
      "  \\small",
      "  \\begin{tabular}{lrrrrr}",
      "    \\toprule",
      "    Variable & N & Mean & SD & Min & Max \\\\",
      "    \\midrule")
    for (i in seq_len(nrow(rows_dt))) {
      r <- rows_dt[i]
      # A row whose label starts with "PANEL:" is a section header, not data.
      if (grepl("^PANEL:", r$label)) {
        body <- c(body, "    \\addlinespace",
                  sprintf("    \\multicolumn{6}{l}{\\textit{%s}} \\\\",
                          sub("^PANEL:\\s*", "", r$label)),
                  "    \\addlinespace")
        next
      }
      body <- c(body, sprintf(
        "    %s & %s & %.2f & %.2f & %.2f & %.2f \\\\",
        r$label,
        format(as.integer(r$N), big.mark = ","),
        r$mean, r$sd, r$min, r$max))
    }
    body <- c(body, "    \\bottomrule", "  \\end{tabular}",
              "  \\vspace{0.3em}",
              "  \\begin{minipage}{0.95\\textwidth}",
              sprintf("    \\footnotesize \\textit{Notes:} %s",
                      paste(notes, collapse = " ")),
              "  \\end{minipage}",
              "\\end{table}")
    writeLines(body, file.path(tab_dir, out_name))
    fwrite(rows_dt, file.path(RESULTS_DIR, sub("\\.tex$", ".csv", out_name)))
  }

  mk_row <- function(label, vec) {
    s <- summarize_vec(vec)
    data.table(label = label, N = s[1], mean = s[2], sd = s[3],
               p25 = s[4], p50 = s[5], p75 = s[6], min = s[7], max = s[8])
  }
  mk_panel <- function(title) data.table(label = paste0("PANEL: ", title))

  # Muni-type tagging (city / borough / township)
  muni_type_from_name <- function(nm) {
    nm <- toupper(nm)
    fifelse(grepl("\\bCITY\\b", nm), "City",
     fifelse(grepl("\\bBORO|BOROUGH\\b", nm), "Borough",
      fifelse(grepl("\\bTWP|TOWNSHIP\\b", nm), "Township", "Other")))
  }
  rate[, muni_type := muni_type_from_name(municipality)]

  # 1. Muni-level summary -- ESTIMATION SAMPLE
  # The table must describe the muni-years the headline regressions actually
  # use, not the raw LODES panel. Three restrictions get us there, in the same
  # order build_headline_env() applies them:
  #   (a) raw cousub panel, 2002-2019
  #   (b) drop flip-floppers ("Uncertain") and any muni with a Dec event
  #       -> env$panel
  #   (c) drop muni-years missing any fiscal control (feols complete.cases)
  #       -> est
  # Each outcome row then reports its OWN non-missing N inside `est`, which is
  # exactly that outcome's regression N at the muni-year level (before the
  # stack replicates NC munis across cohorts).
  ctrls <- env$ctrls
  est <- copy(env$panel)
  est <- est[complete.cases(est[, ..ctrls])]

  raw_muni <- copy(cousub)
  setnames(raw_muni, "work_city", "muni", skip_absent = TRUE)
  sample_steps <- data.table(
    step = c("Raw cousub panel, 2002--2019",
             "Less flip-floppers and decrease munis",
             "Less muni-years missing a fiscal control"),
    munis      = c(uniqueN(raw_muni$muni), uniqueN(env$panel$muni), uniqueN(est$muni)),
    muni_years = c(nrow(raw_muni),         nrow(env$panel),         nrow(est)))
  fwrite(sample_steps, file.path(RESULTS_DIR, "sample_construction.csv"))

  # Shares reported in percent so two decimals stay informative.
  muni_rows <- list(
    # Row order is deliberate: the two residence-side rates first, then the
    # stack they sum to, then the work-side nonresident rate last, since it
    # is the comparison rate rather than part of the stack.
    mk_panel("Tax rates (percentage points)"),
    mk_row("Resident EIT rate $\\tau^{M}$",            est$resident_eit),
    mk_row("School-district EIT rate $\\tau^{SD}$",    est$sd_eit),
    mk_row("Home stack $\\tau^{M}+\\tau^{SD}$",        est$home_stack),
    mk_row("Nonresident EIT rate $\\tau^{nr,M}$",      est$muni_nonres_eit),
    mk_row("Effective property rate, muni",            est$muni_eff_rate),
    mk_row("Effective property rate, school district", est$sd_eff_rate),
    mk_row("Effective property rate, county",          est$county_eff_rate),

    # Live-and-work and its share are the two the paper leans on, so they sit
    # together at the top of the block.
    mk_panel("Outcomes"),
    mk_row("Live-and-work workers",                    est$live_and_work),
    mk_row("Share live-and-work in muni (\\%)",     100 * est$share_live_in),
    mk_row("In-commuters",                             est$in_commuters),
    mk_row("Total jobs",                               est$total_jobs),

    mk_panel("Fiscal controls (percent of total)"),
    mk_row("Police, share of expenditure",             100 * est$share_police),
    mk_row("Streets, share of expenditure",            100 * est$share_streets),
    mk_row("Debt service, share of expenditure",       100 * est$share_debt_svc),
    mk_row("Intergovernmental, share of revenue",      100 * est$share_intergov)
  )
  write_summary_table(muni_rows,
    "Estimation-sample summary statistics, muni-year (2002--2019)",
    "tab:summary_muni", "summary_muni.tex",
    notes = c(sprintf(paste("Estimation sample: %s muni-years across %s municipalities,",
                            "from a raw panel of %s. Each row reports its own non-missing $N$."),
                      format(nrow(est), big.mark = ","),
                      format(uniqueN(est$muni), big.mark = ","),
                      format(sample_steps$muni_years[1], big.mark = ",")),
              "Rates in percentage points. Effective property rates are millage times the county",
              "common-level ratio, after the repair in Section~\\ref{subsec:sources}."))

  # Muni-type sub-table (means only by type), on the same estimation sample
  rate_type <- unique(rate[, .(municipality_id, year, muni_type)])
  rate_type[, municipality_id := as.character(municipality_id)]
  m <- merge(est[, muni := as.character(muni)], rate_type,
             by.x = c("muni","year"), by.y = c("municipality_id","year"),
             all.x = TRUE)
  type_split <- m[!is.na(muni_type),
                  .(N_muni_years = .N,
                    mean_res = mean(resident_eit, na.rm = TRUE),
                    mean_nr  = mean(muni_nonres_eit, na.rm = TRUE),
                    mean_sd  = mean(sd_eit, na.rm = TRUE),
                    mean_stk = mean(resident_eit + sd_eit, na.rm = TRUE),
                    mean_jobs = mean(total_jobs, na.rm = TRUE),
                    mean_share_in = mean(share_live_in, na.rm = TRUE)),
                  by = muni_type]
  fwrite(type_split, file.path(DERIVED_DIR, "summary_muni_by_type.csv"))

  # 2. Block-level summary
  block_rows <- list(
    mk_row("Resident rate $\\tau^{M}$ (\\%)",  block$resident_eit),
    mk_row("Nonresident rate $\\tau^{nr,M}$ (\\%)", block$muni_nonres_eit),
    mk_row("SD rate $\\tau^{SD}$ (\\%)",       block$sd_eit),
    mk_row("Home count (workers living)",   block$home_count),
    mk_row("Work count (jobs in block)",    block$work_count),
    mk_row("Share live-and-work in muni",   block$home_in_muni_share),
    mk_row("Share work outside muni",       block$share_work_outside)
  )
  blocks_per_muni <- block[, .(n_blocks = uniqueN(block_geoid)), by = muni][, n_blocks]
  write_summary_table(c(block_rows,
                         list(mk_row("Blocks per muni", as.numeric(blocks_per_muni)))),
    "Block-year summary statistics (2002--2019)",
    "tab:summary_block", "summary_block.tex",
    notes = c("Block-year level; one observation per (census block, year).",
              sprintf("Panel covers %s blocks in %s municipalities.",
                      format(uniqueN(block$block_geoid), big.mark=","),
                      format(uniqueN(block$muni), big.mark=","))))

  # 3. SD-level summary
  sd_panel <- rate[!is.na(sd_eit) & year <= max_year,
                    .(sd_rate = mean(sd_eit, na.rm = TRUE),
                      res     = mean(resident_eit, na.rm = TRUE),
                      nr      = mean(muni_nonres_eit, na.rm = TRUE)),
                   by = .(municipality_id, year)]
  sd_rows <- list(
    mk_row("SD rate $\\tau^{SD}$ (\\%)",     sd_panel$sd_rate),
    mk_row("Resident rate $\\tau^{M}$ (\\%) (muni-year)", sd_panel$res),
    mk_row("Nonres rate $\\tau^{nr,M}$ (\\%) (muni-year)", sd_panel$nr)
  )
  if ("share_state_rev_sd" %in% names(sd_ctrl)) {
    for (cc in c("share_state_rev_sd","share_local_rev_sd","share_federal_rev_sd",
                 "share_property_tax_sd","share_instructional_sd","share_support_sd")) {
      if (cc %in% names(sd_ctrl))
        sd_rows[[length(sd_rows)+1L]] <- mk_row(gsub("_","\\\\_", cc), sd_ctrl[[cc]])
    }
  }
  write_summary_table(sd_rows,
    "School-district-year summary statistics (2002--2019)",
    "tab:summary_sd", "summary_sd.tex",
    notes = c("SD-year level. F-33 share controls are constructed from Census F-33 revenue/expenditure totals."))

  # 4. Cell counts -- same estimation sample as tab:summary_muni, so the
  # municipality counts here reconcile with the regression Ns rather than
  # with the raw panel.
  panel <- copy(est)

  cell_levels_keep <- c("NC","Inc-bind","Inc-becomes","Inc-nobind")
  cell_cnt <- panel[as.character(cell_muni) %in% cell_levels_keep,
                     .(n_muni_year = .N, n_munis = uniqueN(muni)),
                     keyby = .(cell_muni)]
  cell_cnt[, cell_muni := factor(as.character(cell_muni), levels = cell_levels_keep)]
  setorder(cell_cnt, cell_muni)

  # Mean dose at +5: the eventual cumulative increase, which is the figure the
  # Results section uses to convert per-point coefficients into effects. It is
  # NOT the same as the panel-wide mean below, which averages in pre-event
  # years where the dose is zero by construction.
  k5 <- env$stk_muni[rel == 5 & as.character(treat_cell) != "NC",
                     .(dose_k5 = mean(dose_change, na.rm = TRUE)),
                     by = .(cell_muni = treat_cell)]
  k5_pooled <- env$stk_muni[rel == 5 & as.character(treat_cell) != "NC",
                            mean(dose_change, na.rm = TRUE)]

  mean_dose <- panel[!is.na(cell_muni) & cell_muni != "NC",
                      .(mean_dose_pp = mean(dose_muni, na.rm = TRUE),
                        min_dose_pp  = min(dose_muni,  na.rm = TRUE),
                        max_dose_pp  = max(dose_muni,  na.rm = TRUE)),
                      by = .(cell_muni)]
  cell_tab <- merge(cell_cnt, mean_dose, by = "cell_muni", all.x = TRUE)
  cell_tab <- merge(cell_tab, k5, by = "cell_muni", all.x = TRUE)
  cell_tab[is.na(dose_k5), dose_k5 := 0]
  cell_tab[is.na(mean_dose_pp), mean_dose_pp := 0]
  cell_tab[is.na(min_dose_pp),  min_dose_pp  := 0]
  cell_tab[is.na(max_dose_pp),  max_dose_pp  := 0]

  # For Inc cells, the "extreme dose" is the max; for Dec cells, the min.
  cell_tab[, extreme_dose_pp := fifelse(
    grepl("^Dec", as.character(cell_muni)), min_dose_pp, max_dose_pp)]

  body <- c(
    "\\begin{table}[H]", "  \\centering",
    "  \\caption{Cell counts: muni resident-rate side, 2002--2019}",
    "  \\label{tab:cell_counts_summary}",
    "  \\small",
    "  \\begin{tabular}{lrrrrr}",
    "    \\toprule",
    "    Cell & Munis & Muni-years & Mean dose (pp) & Mean dose at $+5$ (pp) & Extreme dose (pp) \\\\",
    "    \\midrule")
  for (i in seq_len(nrow(cell_tab))) {
    r <- cell_tab[i]
    cell_label <- as.character(r$cell_muni)
    if (cell_label == "NC") cell_label <- "Never-changers"
    body <- c(body, sprintf(
      "    %s & %s & %s & %.2f & %.2f & %.2f \\\\",
      cell_label,
      format(r$n_munis, big.mark = ","),
      format(r$n_muni_year, big.mark = ","),
      r$mean_dose_pp, r$dose_k5, r$extreme_dose_pp))
  }
  body <- c(body,
            sprintf("    \\midrule\n    Pooled Increase & %s & %s & %.2f & %.2f & %.2f \\\\",
                    format(sum(cell_tab[cell_muni != "NC"]$n_munis), big.mark = ","),
                    format(sum(cell_tab[cell_muni != "NC"]$n_muni_year), big.mark = ","),
                    weighted.mean(cell_tab[cell_muni != "NC"]$mean_dose_pp,
                                  cell_tab[cell_muni != "NC"]$n_muni_year),
                    k5_pooled,
                    max(cell_tab[cell_muni != "NC"]$extreme_dose_pp)),
            "    \\bottomrule", "  \\end{tabular}",
            "  \\vspace{0.3em}",
            "  \\begin{minipage}{0.95\\textwidth}",
            "    \\footnotesize \\textit{Notes:} Estimation sample, as in Table~\\ref{tab:summary_muni}. Municipalities are classified on their first resident-rate change. Mean dose averages the cumulative rate change over every muni-year in the cell, including pre-event years where it is zero. Mean dose at $+5$ is the eventual cumulative increase, five years after the event, and is the figure used to convert coefficients into effects in Section~\\ref{sec:results}.",
            "  \\end{minipage}", "\\end{table}")
  writeLines(body, file.path(tab_dir, "cell_counts_summary.tex"))
  fwrite(cell_tab, file.path(RESULTS_DIR, "cell_counts_summary.csv"))

  # 5. Figure: double histogram (resident vs nonresident vs home stack)
  hist_dt <- rate[, .(municipality_id, year, resident_eit, muni_nonres_eit, sd_eit)]
  hist_dt[, home_stack := resident_eit + sd_eit]

  long <- melt(hist_dt[, .(resident_eit, muni_nonres_eit, home_stack)],
               measure.vars = c("resident_eit","muni_nonres_eit","home_stack"),
               variable.name = "rate_type", value.name = "rate")
  long <- long[is.finite(rate) & rate >= 0 & rate <= 5]
  long[, rate_type := factor(rate_type,
       levels = c("resident_eit","muni_nonres_eit","home_stack"),
       labels = c("Resident rate $\\tau^{M}$",
                  "Nonresident rate $\\tau^{nr,M}$",
                  "Home stack $\\tau^{M}+\\tau^{SD}$"))]

  g_hist <- ggplot(long, aes(x = rate, color = rate_type)) +
    stat_ecdf(geom = "step", linewidth = 1.0) +
    scale_color_manual(values = c(PAPER_RED, PAPER_BLUE, "#7B2CBF"),
                        name = NULL,
                        labels = c(expression("Resident rate "*tau^M),
                                   expression("Nonresident rate "*tau^{nr*","*M}),
                                   expression("Home stack "*tau^M+tau^{SD}))) +
    scale_x_continuous(breaks = seq(0, 4.5, by = 0.5),
                        limits = c(0, 4.5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                        labels = scales::label_percent()) +
    geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey60",
                linewidth = 0.3) +
    geom_vline(xintercept = 1.0, linetype = "dotted", color = "grey60",
                linewidth = 0.3) +
    annotate("text", x = 0.55, y = 0.05, label = "0.5% cap",
             hjust = 0, size = 3, color = "grey40") +
    annotate("text", x = 1.05, y = 0.05, label = "1% combined cap",
             hjust = 0, size = 3, color = "grey40") +
    labs(x = "Rate (percentage points)",
         y = "Share of municipality-years at or below rate") +
    theme_paper() +
    theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, "rate_double_histogram.png"), g_hist,
         width = 9, height = 5, dpi = 200)

  # 6. Figure: Act 32 CDF (share of munis reporting a nonresident rate, by year)
  cov_dt <- rate[, .(coverage = mean(!is.na(muni_nonres_eit) & !nonres_imputed,
                                      na.rm = TRUE)),
                  by = year][order(year)]
  g_cov <- ggplot(cov_dt[year >= 2002 & year <= 2019], aes(x = year, y = coverage)) +
    geom_line(color = PAPER_RED, linewidth = 1.1) +
    geom_point(color = PAPER_RED, size = 2) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "grey40") +
    annotate("text", x = 2012.2, y = 0.05,
              label = "Act 32 in effect", hjust = 0, size = 3.5, color = "grey25") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_x_continuous(breaks = seq(2002, 2019, by = 2)) +
    labs(x = "Year",
         y = "Share of PA munis reporting a nonresident rate") +
    theme_paper()
  ggsave(file.path(fig_dir, "act32_cdf.png"), g_cov,
         width = 8, height = 4.5, dpi = 200)

  # 7. Figure: rate changes by year (red increases above zero, blue decreases below)
  rate_yr <- rate[year >= 2002 & year <= 2019 & !is.na(rate_change),
                  .(n_inc = sum(rate_change >  1e-6)),
                  by = year]
  rc_long <- melt(rate_yr, id.vars = "year",
                  measure.vars = c("n_inc"),
                  variable.name = "direction", value.name = "n")
  rc_long[, direction := factor(direction,
          levels = c("n_inc"),
          labels = c("Resident-rate increase"))]

  g_rc <- ggplot(rc_long, aes(x = year, y = n, fill = direction)) +
    geom_col(width = 0.75, alpha = 0.9) +
    geom_hline(yintercept = 0, color = "grey20", linewidth = 0.4) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "grey40") +
    annotate("text", x = 2012.2, y = max(rc_long$n) * 0.9,
             label = "Act 32", hjust = 0, size = 3.5, color = "grey25") +
    scale_fill_manual(values = c("Resident-rate increase" = PAPER_RED),
                       name = NULL) +
    scale_x_continuous(breaks = seq(2002, 2019, by = 2)) +
    labs(x = "Year", y = "Number of municipalities (resident-rate increase)") +
    theme_paper() +
    theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, "rate_changes_by_year.png"), g_rc,
         width = 8.5, height = 5, dpi = 200)

  # 8. Figure: top rate-changers trajectories (cumulative |dtau| vs # events, side by side)
  muni_summary <- rate[year >= 2002 & year <= 2019,
     .(cum_abs_dtau = sum(abs(rate_change), na.rm = TRUE),
       n_events    = sum(abs(rate_change) > 1e-6, na.rm = TRUE),
       muni_name   = first(municipality),
       county      = first(county)),
     by = municipality_id]

  top_by_cum <- muni_summary[order(-cum_abs_dtau)][1:20]
  top_by_cum[, panel := "Top 20 by cumulative $|\\Delta\\tau|$"]
  top_by_cnt <- muni_summary[order(-n_events)][1:20]
  top_by_cnt[, panel := "Top 20 by number of rate change events"]

  selected <- unique(rbind(top_by_cum, top_by_cnt, fill = TRUE))
  traj <- rate[year >= 2002 & year <= 2019 &
                municipality_id %in% selected$municipality_id,
                .(municipality_id, municipality, year, resident_eit)]

  # Build per-panel data with facets
  make_panel_dt <- function(ids, panel_label) {
    d <- rate[year >= 2002 & year <= 2019 & municipality_id %in% ids,
               .(municipality_id, municipality, year, resident_eit)]
    d[, panel := panel_label]
    d[, muni_lbl := tools::toTitleCase(tolower(municipality))]
    d
  }
  dt_cum <- make_panel_dt(top_by_cum$municipality_id, "Top 20 by cumulative |Δτ|")
  dt_cnt <- make_panel_dt(top_by_cnt$municipality_id, "Top 20 by number of events")

  mk_traj_panel <- function(dt, title) {
    ggplot(dt, aes(x = year, y = resident_eit, group = municipality_id)) +
      geom_step(color = PAPER_RED, linewidth = 0.7) +
      facet_wrap(~ muni_lbl, ncol = 4, scales = "free_y") +
      scale_x_continuous(breaks = c(2005, 2010, 2015)) +
      labs(x = "Year", y = expression("Resident rate "*tau^M*" (pp)"),
           title = title) +
      theme_paper() +
      theme(strip.text = element_text(size = 7),
            axis.text  = element_text(size = 7),
            panel.spacing = unit(0.4, "lines"),
            plot.title = element_text(size = 11, face = "bold"))
  }
  g_cum  <- mk_traj_panel(dt_cum, "Top 20 by cumulative |Δτ|")
  g_cnt  <- mk_traj_panel(dt_cnt, "Top 20 by number of rate-change events")
  g_traj <- g_cum | g_cnt
  ggsave(file.path(fig_dir, "pa_rate_trajectories.png"), g_traj,
         width = 16, height = 9, dpi = 180)

  # 9. Figure: PA map with major rate-changing cities labeled
  pa_cs <- st_as_sf(readRDS(file.path(DERIVED_DIR, "pa_cousub.rds")))

  # Per-muni change classification from the rate panel
  chg <- rate[year >= 2002 & year <= 2019 & !is.na(rate_change),
              .(n_inc = sum(rate_change >  1e-6),
                n_dec = sum(rate_change < -1e-6)),
              by = .(cousub_fips, municipality)]
  chg[, change_type := fcase(
    n_inc > 0 & n_dec > 0, "Both",
    n_inc > 0, "Increased",
    n_dec > 0, "Decreased",
    default = "No change")]

  # Pull jobs counts to rank cities for labeling
  jobs <- cousub[, .(total_jobs = mean(total_jobs, na.rm = TRUE)),
                  by = .(muni = as.character(work_city))]

  map_dt <- pa_cs |>
    dplyr::left_join(chg, by = c("COUSUBFP" = "cousub_fips")) |>
    dplyr::left_join(jobs, by = c("COUSUBFP" = "muni")) |>
    dplyr::mutate(change_type = ifelse(is.na(change_type), "No change", change_type))

  # Centroids for labeling the top rate-changers by jobs
  label_dt <- map_dt[!is.na(map_dt$municipality) &
                       map_dt$change_type %in% c("Increased","Decreased","Both"), ]
  label_dt <- label_dt[order(-label_dt$total_jobs), ][1:30, ]
  label_dt <- label_dt[!is.na(label_dt$municipality), ]
  label_centroids <- suppressWarnings(st_centroid(label_dt))
  lc_coords <- st_coordinates(label_centroids)
  label_centroids$x <- lc_coords[, 1]
  label_centroids$y <- lc_coords[, 2]
  label_centroids$muni_lbl <- tools::toTitleCase(tolower(label_centroids$municipality))

  # Dissolve cousub geometries into a single state outline for the thicker border
  pa_outline <- st_union(map_dt)

  map_dt$change_type <- factor(map_dt$change_type,
                               levels = c("Increased","Decreased","Both","No change"))
  g_map <- ggplot(map_dt) +
    geom_sf(aes(fill = change_type), color = "black", linewidth = 0.08) +
    geom_sf(data = pa_outline, fill = NA, color = "black", linewidth = 0.5) +
    scale_fill_manual(values = c("Increased" = PAPER_RED, "Decreased" = PAPER_BLUE,
                                 "Both" = "#7B2CBF", "No change" = "grey92"),
                      breaks = c("Increased","Decreased","Both","No change"),
                      name = NULL, drop = FALSE) +
    coord_sf() +
    labs(title = NULL) +
    theme_paper() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom")
  ggsave(file.path(fig_dir, "pa_map.png"), g_map,
         width = 11, height = 7, dpi = 220)

  # 10-12. Aggregate "work side rarely binds" figures
  # Supports one claim: PA municipalities raise the home stack (resident + SD)
  # faster than the work (nonresident) rate, so the work side almost never
  # becomes the binding margin -- which is why the paper estimates no work-side
  # binding decomposition. AGGREGATE only: no municipalities named or plotted.
  #
  #   home stack     = resident rate + SD rate
  #   work rate      = municipality nonresident rate
  #   binding margin = home stack - work rate   (positive => home binds)
  #
  # Population: muni-years that GENUINELY REPORT a work rate (non-imputed). DCED
  # work-rate reporting is incomplete before Act 32 (2012), so N rises sharply
  # at 2012; imputed pre-2012 values are excluded so the comparison is real.
  # Tie convention: fold exact ties into "home binds" -- if home stack >= work
  # rate, the home side is the margin a mover faces -- so the work rate binds
  # only when work_rate > home_stack (strictly). Home side = navy, work = red.

  BM_HOME <- PAPER_BLUE      # home side (resident + SD stack)
  BM_WORK <- PAPER_RED       # work side (nonresident rate)

  bm_rate <- rate[!is.na(cousub_fips) & year >= 2002 & year <= max_year]
  # Muni-year aggregation mirroring the headline env (resident = mean,
  # sd = max, nonres = mean). nonres counts as reported only if not imputed.
  bm_agg <- bm_rate[, .(resident_eit    = mean(resident_eit,    na.rm = TRUE),
                        sd_eit          = max(sd_eit,            na.rm = TRUE),
                        muni_nonres_eit = mean(muni_nonres_eit,  na.rm = TRUE),
                        nonres_imputed  = as.logical(max(nonres_imputed, na.rm = TRUE))),
                    by = .(muni = cousub_fips, year)]
  for (cc in c("resident_eit", "sd_eit", "muni_nonres_eit"))
    bm_agg[!is.finite(get(cc)), (cc) := NA_real_]
  bm_agg[, home_stack := resident_eit + sd_eit]
  bm_agg[, work_rate  := muni_nonres_eit]

  # Restrict to muni-years reporting a (non-imputed) work rate, with a defined
  # home stack, so the home-vs-work comparison is real.
  bm_wb <- bm_agg[!is.na(home_stack) & !is.na(work_rate) & nonres_imputed == FALSE]
  # Lay explanation: a worker's bill is whichever is bigger -- the home-side
  # stack or the workplace rate. This flags the rare muni-years where the
  # workplace rate strictly wins; exact ties count as home-binding, since
  # the home stack is then the margin a mover would actually face.
  bm_wb[, work_binds := work_rate > home_stack + 1e-6]

  bm_yr <- bm_wb[, .(n           = .N,
                     mean_home   = mean(home_stack),
                     mean_work   = mean(work_rate),
                     median_home = median(home_stack),
                     median_work = median(work_rate),
                     share_work  = mean(work_binds)),
                 by = year][order(year)]

  print(bm_yr)
  print(sprintf("Overall: work binds in %.2f%% of %s reporting muni-years (coverage rises at the 2012 Act 32 break).",
                100 * mean(bm_wb$work_binds), format(nrow(bm_wb), big.mark = ",")))

  ## -- Figure A: levels by year -- overlaid translucent bars (dark theme) -----
  # Tax rates in pp (not shares). Home stack drawn first so it sits BEHIND;
  # work rate drawn on top. Both are the same width, fully overlapping, and
  # semi-transparent: the lavender work-rate bar fills the lower portion and the
  # maroon home stack shows above it. Light background, translucent-bar style.
  bm_bg   <- "white"     # light background
  bm_fg   <- "grey20"    # dark text / axes
  bm_grid <- "grey88"    # faint horizontal gridlines
  # Red/blue from support_dose_trajectory_muni_vs_sd.png (aggregate-figures section):
  # blue #1F5673, red #C5283D; light tints for the translucent fills.
  HOME_FILL <- "#9AB3C0"; HOME_LINE <- "#1F5673"   # home stack (back, blue)
  WORK_FILL <- "#E59EA7"; WORK_LINE <- "#C5283D"   # work rate (front, red)
  home_lab <- "Home Stack (resident + SD)"
  work_lab <- "Work Rate (nonresident)"

  g_lev <- ggplot(bm_yr, aes(x = year)) +
    geom_col(aes(y = mean_home, fill = home_lab, color = home_lab),
             width = 0.82, alpha = 0.72, linewidth = 0.6) +
    geom_col(aes(y = mean_work, fill = work_lab, color = work_lab),
             width = 0.82, alpha = 0.72, linewidth = 0.6) +
    scale_fill_manual(values = setNames(c(HOME_FILL, WORK_FILL), c(home_lab, work_lab)),
                      name = NULL, breaks = c(home_lab, work_lab)) +
    scale_color_manual(values = setNames(c(HOME_LINE, WORK_LINE), c(home_lab, work_lab)),
                       name = NULL, breaks = c(home_lab, work_lab)) +
    scale_x_continuous(breaks = 2002:2019) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
    labs(x = "Year", y = "Mean EIT rate (pp)") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background       = element_rect(fill = bm_bg, color = NA),
      panel.background      = element_rect(fill = bm_bg, color = NA),
      panel.grid.major.y    = element_line(color = bm_grid, linewidth = 0.3),
      panel.grid.minor      = element_blank(),
      panel.grid.major.x    = element_blank(),
      axis.line             = element_line(color = bm_fg, linewidth = 0.4),
      axis.ticks            = element_line(color = bm_fg, linewidth = 0.4),
      axis.text             = element_text(color = bm_fg),
      axis.text.x           = element_text(color = bm_fg, angle = 45, hjust = 1),
      axis.title            = element_text(color = bm_fg),
      legend.text           = element_text(color = bm_fg, size = 14),
      legend.position       = "bottom",
      legend.key            = element_rect(fill = bm_bg, color = NA),
      legend.key.width      = unit(2.2, "lines"),
      legend.background     = element_rect(fill = bm_bg, color = NA),
      legend.box.background = element_rect(color = bm_fg, fill = NA),
      legend.box.margin     = margin(4, 8, 4, 8)
    )
  ggsave(file.path(fig_dir, "binding_levels_by_year.png"), g_lev,
         width = 10, height = 6, dpi = 300)
})

# Aggregate figures (build_uea_aggregate_figures.R) ----
# UEA-draft-specific aggregate figures: drop the city/borough/township
# muni-type split from three figures, and produce a two-line muni-vs-SD
# dose trajectory.
# Outputs: figures/support_cohort_timing_agg.png,
#   figures/support_dose_trajectory_muni_vs_sd.png,
#   figures/support_dose_distribution_agg.png,
#   figures/pooled_es_muni_only.png
if (RUN_AGGREGATE_FIGS) local({
  fig_dir <- OUT_DIR

  PAPER_RED  <- "#C5283D"
  PAPER_BLUE <- "#1F5673"
  PAPER_GREY <- "#7A7A7A"

  theme_paper <- function(base_size = 11) {
    theme_bw(base_size = base_size) +
      theme(plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey30"),
            panel.grid.minor = element_blank())
  }

  # Load panel + cell labels from the shared headline env
  panel <- copy(env$panel)

  # Figure 1: aggregate Inc-bind cohort timing (no muni-type split)
  incbind <- unique(panel[cell_muni == "Inc-bind" & !is.na(c_muni),
                            .(muni, c_muni)])

  g1 <- ggplot(incbind, aes(x = c_muni)) +
    geom_histogram(binwidth = 1, fill = PAPER_RED, color = "white",
                    linewidth = 0.3) +
    scale_x_continuous(breaks = seq(2003, 2019, 1)) +
    labs(title = "Inc-bind first-event year",
          subtitle = sprintf("All %d Inc-bind municipalities (drop sample, post-NC-fix)",
                             nrow(incbind)),
          x = "Year of first municipal resident-rate event (cohort)",
          y = "Count of municipalities") +
    theme_paper()
  ggsave(file.path(fig_dir, "support_cohort_timing_agg.png"),
          g1, width = 9, height = 4.5, dpi = 300)

  # Figure 2: dose trajectory -- muni cohort vs SD cohort
  # Muni-side Inc-bind dose trajectory
  muni_traj <- panel[cell_muni == "Inc-bind" & !is.na(c_muni) &
                     year >= c_muni - 5 & year <= c_muni + 5,
                     .(rel = year - c_muni, dose = dose_muni)]
  muni_traj_summary <- muni_traj[, .(mean_dose = mean(dose, na.rm = TRUE),
                                       q25 = quantile(dose, 0.25, na.rm = TRUE),
                                       q75 = quantile(dose, 0.75, na.rm = TRUE),
                                       side = "Municipality cohort"),
                                   by = rel]

  # SD-side Inc-bind dose trajectory (use cell_sd + dose_sd + c_sd if available)
  sd_cols_present <- all(c("cell_sd","dose_sd","c_sd") %in% names(panel))
  if (sd_cols_present) {
    sd_traj <- panel[cell_sd == "Inc-bind" & !is.na(c_sd) &
                     year >= c_sd - 5 & year <= c_sd + 5,
                     .(rel = year - c_sd, dose = dose_sd)]
    sd_traj_summary <- sd_traj[, .(mean_dose = mean(dose, na.rm = TRUE),
                                     q25 = quantile(dose, 0.25, na.rm = TRUE),
                                     q75 = quantile(dose, 0.75, na.rm = TRUE),
                                     side = "School-district cohort"),
                                by = rel]
    combined <- rbind(muni_traj_summary, sd_traj_summary)
  } else {
    print("WARNING: SD-side cell/dose/cohort columns not in panel; muni-only.")
    combined <- muni_traj_summary
  }

  g2 <- ggplot(combined, aes(x = rel, y = mean_dose, color = side,
                                fill = side, group = side)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_color_manual(values = c("Municipality cohort"      = PAPER_RED,
                                    "School-district cohort" = PAPER_BLUE),
                        name = NULL) +
    scale_fill_manual(values = c("Municipality cohort"      = PAPER_RED,
                                   "School-district cohort" = PAPER_BLUE),
                       guide = "none") +
    scale_x_continuous(breaks = -5:5) +
    labs(title = "Cumulative Inc-bind dose by event-time",
          subtitle = "Mean and inter-quartile range relative to pre-event baseline",
          x = "Event time k (years relative to first rate event)",
          y = expression("Cumulative "*Delta*tau*" (pp)")) +
    theme_paper() +
    theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, "support_dose_trajectory_muni_vs_sd.png"),
          g2, width = 9, height = 4.5, dpi = 300)

  # Figure 3: aggregate dose distribution at k=+5 (no muni-type split)
  dose_at_5 <- panel[cell_muni == "Inc-bind" & !is.na(c_muni) &
                     year == c_muni + 5,
                     .(muni, dose_5yr = dose_muni)]
  # Fill in missing rel=5 with max post-event dose
  missing_munis <- setdiff(incbind$muni, dose_at_5$muni)
  if (length(missing_munis) > 0) {
    fill <- panel[cell_muni == "Inc-bind" & muni %in% missing_munis &
                  year >= c_muni,
                  .(dose_5yr = max(dose_muni, na.rm = TRUE)), by = muni]
    dose_at_5 <- rbind(dose_at_5, fill, fill = TRUE)
  }

  g3 <- ggplot(dose_at_5, aes(x = dose_5yr)) +
    geom_histogram(binwidth = 0.1, fill = PAPER_RED, color = "white",
                    linewidth = 0.3) +
    scale_x_continuous(breaks = seq(0, 1.5, by = 0.1)) +
    labs(title = "Cumulative Inc-bind dose at event-time k = +5",
          subtitle = sprintf("All %d Inc-bind municipalities; aggregated across muni type",
                             nrow(dose_at_5)),
          x = expression("Cumulative "*Delta*tau^muni*" at k=+5 (pp)"),
          y = "Count of municipalities") +
    theme_paper()
  ggsave(file.path(fig_dir, "support_dose_distribution_agg.png"),
          g3, width = 9, height = 4.5, dpi = 300)

  # Figure 4 & 5: split pooled ES muni-vs-SD overlay into two figures
  prep_pooled <- function(path) {
    if (!file.exists(path)) return(NULL)
    d <- fread(path)
    ref_keys <- intersect(c("outcome","direction"), names(d))
    ref <- unique(d[, ..ref_keys])
    ref[, `:=`(rel = -1L, est = 0, se = 0, p = NA, n = NA)]
    rbind(d, ref, fill = TRUE)
  }

  # SD-level pooled ES dropped from the paper (2026-07-02) -- muni only.
  cousub_dt <- prep_pooled(file.path(RESULTS_DIR, "pooled_es_cousub_coefs.csv"))

  plot_one_level <- function(dt, level_label, color_pos, out_path) {
    if (is.null(dt) || nrow(dt) == 0) return(invisible())
    dt <- copy(dt)
    dt[, lo := est - 1.96 * se]
    dt[, hi := est + 1.96 * se]
    dt[, direction := factor(direction, levels = c("Inc","Dec"))]
    muni_outcomes <- c("log_live_and_work","log_in_commuters",
                        "log_total_jobs", "share_live_in")
    dt <- dt[outcome %in% muni_outcomes]
    outcome_pretty <- c(
      log_live_and_work = "log(live-and-work)",
      log_in_commuters  = "log(in-commuters)",
      log_total_jobs    = "log(total jobs)",
      share_live_in     = "Share live-in-muni"
    )
    dt[, outcome := factor(outcome, levels = names(outcome_pretty),
                            labels = outcome_pretty)]
    p <- ggplot(dt, aes(x = rel, y = est, color = direction, fill = direction,
                          group = direction)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, color = NA) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.1) +
      facet_wrap(~ outcome, ncol = 2, scales = "free_y") +
      scale_color_manual(values = c(Inc = PAPER_RED, Dec = PAPER_BLUE),
                          labels = c(Inc = "Pooled Inc", Dec = "Pooled Dec"),
                          name = NULL) +
      scale_fill_manual(values = c(Inc = PAPER_RED, Dec = PAPER_BLUE),
                         guide = "none") +
      labs(title = sprintf("%s pooled realized-dose event study", level_label),
            subtitle = "Per-pp coefficient by event-time. 95% confidence ribbons.",
            x = "Event time k",
            y = expression(beta[k])) +
      theme_paper(base_size = 10) +
      theme(legend.position = "bottom")
    ggsave(out_path, p, width = 11, height = 6.5, dpi = 300)
  }

  plot_one_level(cousub_dt, "Municipality-level",
                  color_pos = NULL,
                  out_path = file.path(fig_dir, "pooled_es_muni_only.png"))
})

# Dose trajectory Inc-becomes (build_dose_trajectory_inc_becomes.R) ----
# Inc-becomes analog of the muni-vs-SD overlay in the aggregate-figures section.
if (RUN_DOSE_TRAJECTORY) local({
  fig_dir <- OUT_DIR

  PAPER_RED  <- "#C5283D"; PAPER_BLUE <- "#1F5673"

  theme_paper <- function(bs = 11) theme_bw(base_size = bs) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank())

  panel <- copy(env$panel)

  muni_traj <- panel[cell_muni == "Inc-becomes" & !is.na(c_muni) &
                     year >= c_muni - 5 & year <= c_muni + 5,
                     .(rel = year - c_muni, dose = dose_muni)]
  muni_summary <- muni_traj[, .(mean_dose = mean(dose, na.rm = TRUE),
                                   q25 = quantile(dose, 0.25, na.rm = TRUE),
                                   q75 = quantile(dose, 0.75, na.rm = TRUE),
                                   side = "Municipality cohort"), by = rel]

  sd_traj <- panel[cell_sd == "Inc-becomes" & !is.na(c_sd) &
                   year >= c_sd - 5 & year <= c_sd + 5,
                   .(rel = year - c_sd, dose = dose_sd)]
  sd_summary <- sd_traj[, .(mean_dose = mean(dose, na.rm = TRUE),
                               q25 = quantile(dose, 0.25, na.rm = TRUE),
                               q75 = quantile(dose, 0.75, na.rm = TRUE),
                               side = "School-district cohort"), by = rel]

  combined <- rbind(muni_summary, sd_summary)

  g <- ggplot(combined, aes(x = rel, y = mean_dose, color = side, fill = side, group = side)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.8) + geom_point(size = 2) +
    scale_color_manual(values = c("Municipality cohort" = PAPER_RED,
                                    "School-district cohort" = PAPER_BLUE), name = NULL) +
    scale_fill_manual(values = c("Municipality cohort" = PAPER_RED,
                                   "School-district cohort" = PAPER_BLUE), guide = "none") +
    scale_x_continuous(breaks = -5:5) +
    labs(title = "Cumulative Inc-becomes dose by event-time",
          subtitle = "Mean and inter-quartile range relative to pre-event baseline",
          x = "Event time k (years relative to first rate event)",
          y = expression("Cumulative "*Delta*tau*" (pp)")) +
    theme_paper() + theme(legend.position = "bottom")

  ggsave(file.path(fig_dir, "support_dose_trajectory_inc_becomes.png"),
          g, width = 9, height = 4.5, dpi = 300)
})

# Appendix ES figures (build_appendix_es_figures.R) ----
# Regenerate the appendix-bound event-study figures in a uniform ggplot style
# matching the main-text overlay plots. MUNI (cousub) realized-dose ONLY:
# the SD- and block-level units and the binary Sun-Abraham ES were dropped
# from the paper (2026-07-02) -- their builder scripts live in
# scripts/_retired/ if those results are ever needed again.
# Input:  data/results/headline_es_coefs.csv
# Output: figures/es_realized_<outcome>.png
if (RUN_APPENDIX_ES_FIGS) local({
  data_dir <- RESULTS_DIR
  fig_dir  <- OUT_DIR

  PAPER_RED  <- "#C5283D"
  PAPER_BLUE <- "#1F5673"

  # Cell-order matches the institutional taxonomy (Inc cells only; paper drops Dec)
  cell_levels <- c("Inc-bind","Inc-becomes","Inc-nobind")

  make_plot <- function(dt, title_prefix, outcome_val,
                         caption_subtitle, color = PAPER_RED) {
    dd <- dt[outcome == outcome_val & cell %in% cell_levels]
    if (nrow(dd) == 0) return(NULL)
    # Insert reference period (rel = -1, est = 0) for visual continuity
    ref_rows <- unique(dd[, .(cell)])[, .(cell, rel = -1L, est = 0,
                                           se = 0, p = NA_real_, n = NA_integer_,
                                           outcome = outcome_val)]
    # Keep only columns common to both
    common <- intersect(names(dd), names(ref_rows))
    dd <- rbind(dd[, ..common], ref_rows[, ..common], fill = TRUE)
    dd[, lo := est - 1.96 * se]
    dd[, hi := est + 1.96 * se]
    dd[, cell := factor(cell, levels = cell_levels)]
    setorder(dd, cell, rel)
    p <- ggplot(dd, aes(x = rel, y = est)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey55",
                  linewidth = 0.4) +
      geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey55",
                  linewidth = 0.4) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = color, alpha = 0.18) +
      geom_line(color = color, linewidth = 0.7) +
      geom_point(color = color, size = 1.6) +
      facet_wrap(~ cell, ncol = 3, scales = "free_y") +
      labs(title = sprintf("%s: %s", title_prefix, outcome_val),
            subtitle = caption_subtitle,
            x = "Event time k",
            y = expression(beta[k])) +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 10, color = "grey25"),
            strip.background = element_rect(fill = "grey95", color = NA),
            strip.text = element_text(face = "bold", size = 10),
            panel.grid.minor = element_blank())
    p
  }

  save_for <- function(coefs_path, file_prefix, title_prefix, subtitle,
                        color = PAPER_RED, width = 10, height = 6) {
    if (!file.exists(coefs_path)) {
      print(sprintf("skip: %s not found", coefs_path)); return(invisible())
    }
    dt <- fread(coefs_path)
    outcomes <- unique(dt$outcome)
    for (o in outcomes) {
      p <- make_plot(dt, title_prefix, o, subtitle, color)
      if (is.null(p)) next
      out_path <- file.path(fig_dir, sprintf("%s_%s.png", file_prefix, o))
      ggsave(out_path, p, width = width, height = height, dpi = 300)
    }
  }

  save_for(file.path(data_dir, "headline_es_coefs.csv"),
            file_prefix    = "es_realized",
            title_prefix   = "Muni-level realized-dose ES",
            subtitle       = "Per-pp dose-response at event-time k; 95% CI ribbons; k=-1 omitted as reference",
            color          = PAPER_RED)
})

# Extra descriptive figures (build_extra_figures.R) ----
# Descriptive figures of the local-tax rate changes. FLIP-FLOPPERS (units with both
# a + and a - change in a given rate) are DROPPED per rate to match the estimation
# sample. Two change measures: cumulative = cumulative dtau at +5; first = the first
# change only. Figures:
#   1a. scatter baseline level vs change, MUNI + SD overlaid -- {inc,dec} x {cum,first}
#   1b. scatter baseline level vs change, TOTAL home stack    -- {inc,dec} x {cum,first}
#   2.  spaghetti: every muni's cumulative dose by event-time -- {inc,dec}
#   3.  mirrored histogram of muni change magnitude           -- {cum,first}
#   4.  histogram of baseline muni-rate levels
if (RUN_EXTRA_FIGS) local({
  fig_dir <- OUT_DIR; K <- 5L
  RED <- "#b2182b"; BLU <- "#2166ac"

  raw <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  raw <- raw[!is.na(cousub_fips) & !is.na(resident_eit) & year >= 2002 & year <= 2019,
             .(muni = cousub_fips, year, muni_rate = resident_eit, sd_rate = sd_eit)]
  raw[, stack := muni_rate + fifelse(is.na(sd_rate), 0, sd_rate)]   # home stack (SD=0 if none)

  # --- per-rate change summary: baseline level, first change, cumulative at +K,
  #     dropping THAT rate's own flip-floppers. Returns the per-unit summary and the
  #     event-time trajectory. ---------------------------------------------------
  build_summ <- function(dt, col) {
    x <- dt[!is.na(get(col)), .(muni, year, rate = get(col))]
    x <- x[, .(rate = mean(rate, na.rm = TRUE)), by = .(muni, year)]
    setorder(x, muni, year)
    x[, d := c(NA_real_, diff(rate)), by = muni]
    # Lay explanation: munis that both raised and cut this rate in the
    # window ("flip-floppers") are dropped, matching the estimation sample;
    # their back-and-forth would blur what a single change looks like.
    flip <- x[abs(d) > 1e-6, .(ni = sum(d > 0), nd = sum(d < 0)), by = muni][ni > 0 & nd > 0, muni]
    x <- x[!muni %in% flip]
    # Lay explanation: two sizes for each muni's change. "First" is the
    # initial step alone; "cumulative" is where the rate sits five years
    # later relative to the year before that step -- so later top-ups or
    # partial walk-backs count in cumulative but not in first.
    ev <- x[abs(d) > 1e-6, .(c_year = year[1], first_d = d[1]), by = muni]
    ev[, dir := fifelse(first_d > 0, "Increase", "Decrease")]
    x <- merge(x, ev, by = "muni", all.x = TRUE)
    x[, rel := year - c_year]
    x <- merge(x, x[rel == -1, .(muni, baseline = rate)], by = "muni", all.x = TRUE)
    x[, cum_change := rate - baseline]
    s <- x[!is.na(c_year) & rel >= 0 & rel <= K, .SD[which.max(rel)], by = muni][
          !is.na(baseline), .(muni, dir, baseline, first = first_d, cumulative = cum_change)]
    list(summ = s, traj = x[!is.na(c_year) & rel >= -K & rel <= K & !is.na(baseline)])
  }

  M  <- build_summ(raw, "muni_rate")
  S  <- build_summ(raw, "sd_rate")
  HS <- build_summ(raw, "stack")
  sm_muni <- M$summ[, setter := "Muni"]
  sm_sd   <- S$summ[, setter := "SD"]
  sm_stk  <- HS$summ
  print(sprintf("muni: %d inc / %d dec | SD: %d inc / %d dec | stack: %d inc / %d dec",
                sm_muni[dir=="Increase",.N], sm_muni[dir=="Decrease",.N],
                sm_sd[dir=="Increase",.N], sm_sd[dir=="Decrease",.N],
                sm_stk[dir=="Increase",.N], sm_stk[dir=="Decrease",.N]))

  th <- theme_bw(base_size = 11)
  meas_lab <- c(cumulative = "cumulative change at +5 (pp)", first = "first-change (pp)")

  ## 1a. scatter: MUNI + SD overlaid (square / triangle), ONE pooled trend line --
  s1 <- rbind(sm_muni, sm_sd)
  for (dd in c("Increase","Decrease")) for (m in c("cumulative","first")) {
    d0 <- s1[dir == dd]; d0[, yv := get(m)]
    g <- ggplot(d0, aes(baseline, yv, shape = setter)) +
      geom_point(alpha = 0.5, color = "black", size = 2) +
      geom_smooth(aes(group = 1), method = "lm", se = FALSE,
                  color = "grey40", linewidth = 0.6) +
      scale_shape_manual(values = c("Muni" = 0, "SD" = 2), name = NULL) +
      labs(title = sprintf("Baseline level vs %s (%s)", meas_lab[m], tolower(dd)),
           subtitle = "muni (square) and school-district (triangle) rate changes; single trend line fit to all points; flip-floppers excluded",
           x = "baseline rate level (%)", y = meas_lab[m]) +
      th + theme(legend.position = "bottom")
    ggsave(file.path(fig_dir, sprintf("scatter_level_vs_change_%s_%s.png",
           ifelse(dd=="Increase","inc","dec"), m)), g, width = 7, height = 5, dpi = 300)
  }

  ## 1b. scatter: TOTAL home stack (filled circle, all black) -------------------
  for (dd in c("Increase","Decrease")) for (m in c("cumulative","first")) {
    d0 <- sm_stk[dir == dd]; d0[, yv := get(m)]
    g <- ggplot(d0, aes(baseline, yv)) +
      geom_point(alpha = 0.5, color = "black", shape = 16, size = 2) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey40", linewidth = 0.6) +
      labs(title = sprintf("Baseline level vs %s (%s) — total home stack", meas_lab[m], tolower(dd)),
           subtitle = "home stack = muni + SD rate (moves if either changes); flip-floppers excluded",
           x = "baseline home-stack level (muni + SD, %)", y = meas_lab[m]) + th
    ggsave(file.path(fig_dir, sprintf("scatter_level_vs_change_total_%s_%s.png",
           ifelse(dd=="Increase","inc","dec"), m)), g, width = 7, height = 5, dpi = 300)
  }

  ## 2. spaghetti (muni) -------------------------------------------------------
  for (dd in c("Increase","Decrease")) {
    g <- ggplot(M$traj[dir == dd], aes(rel, cum_change, group = muni)) +
      geom_hline(yintercept = 0, color = "grey70") +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
      geom_line(alpha = 0.25, color = ifelse(dd=="Increase", RED, BLU)) +
      labs(title = sprintf("Cumulative muni-rate change by event-time, every %s unit", tolower(dd)),
           subtitle = "0 = first change; each line one muni; flip-floppers excluded",
           x = "event time (years from first change)", y = "cumulative rate change (pp)") + th
    ggsave(file.path(fig_dir, sprintf("trajectories_spaghetti_%s.png",
           ifelse(dd=="Increase","inc","dec"))), g, width = 7, height = 5, dpi = 300)
  }

  ## 3. mirrored histogram (muni) ----------------------------------------------
  bw <- 0.05
  for (m in c("cumulative","first")) {
    mh <- copy(sm_muni); mh[, mag := abs(get(m))]
    hd <- mh[, .(n = .N), by = .(dir, bin = round(mag/bw)*bw)]
    hd[, y := fifelse(dir == "Increase", n, -n)]
    g <- ggplot(hd, aes(bin, y, fill = dir)) +
      geom_col(width = bw*0.9) + geom_hline(yintercept = 0, color = "grey40") +
      scale_fill_manual(values = c("Increase"=RED, "Decrease"=BLU)) +
      labs(title = sprintf("Distribution of muni %s magnitude", meas_lab[m]),
           subtitle = "|change| (pp); increasers up, decreasers down; flip-floppers excluded",
           x = sprintf("|%s|", meas_lab[m]), y = "number of municipalities", fill = "direction") +
      th + theme(legend.position = "bottom")
    ggsave(file.path(fig_dir, sprintf("change_distribution_mirrored_%s.png", m)),
           g, width = 7.5, height = 5, dpi = 300)
  }

  ## 4. histogram of baseline muni-rate levels ---------------------------------
  lev <- raw[, .(level = mean(muni_rate, na.rm = TRUE)), by = muni][is.finite(level)]
  g4 <- ggplot(lev, aes(level)) +
    geom_histogram(binwidth = 0.05, fill = "#4d4d4d", color = "white") +
    labs(title = "Distribution of municipal resident-rate levels",
         subtitle = "one observation per municipality (mean level)",
         x = "resident rate level (%)", y = "number of municipalities") + th
  ggsave(file.path(fig_dir, "level_histogram.png"), g4, width = 7, height = 5, dpi = 300)
})

# Spline dose-response (build_acr_acrt_att_spline.R) ----
# Natural-spline ns(dose) dose-response (ATT/ACRT) for the MUNI resident-rate dose,
# in the explainer layout, for THREE groups: the POOLED Increase aggregate, the
# Inc-bind cell, and the Inc-becomes cell. Each is the continuous-dose spline on
# {its treated cells + NC}, controlling for the SD rate changes (i(cell_sd, dose_sd))
# so the muni dose-response is net of SD -- matching the headline spec. The linear ACR
# is drawn as a flat reference and labeled. (Quadratic dropped.)
# Outputs: figures/acr_acrt_att_spline_{pooled,incbind,incbecomes}.png
if (RUN_SPLINE_DOSE_RESPONSE) local({
  fig_dir <- OUT_DIR

  stk <- copy(env$stk_muni); ctrls <- env$ctrls; cs <- paste(ctrls, collapse = " + ")
  stk[, inc := as.integer(as.character(treat_cell) %in% c("Inc-bind","Inc-becomes","Inc-nobind"))]
  # dose_change, NOT dose_stack: since the levels switch dose_stack holds the
  # rate LEVEL, which is fine inside a regression (unit FE absorb the
  # difference) but wrong as an x-axis. Plotting the level would contrast NC
  # pinned at 0 against treated munis at 0.5-2.5 and invent a dose-response.
  stk[, dose := fifelse(inc == 1, dose_change, 0)]
  lab <- c(log_live_and_work="Work in own muni", share_live_in="Share live in muni",
           log_total_jobs="Total jobs", log_in_commuters="In-commuters")
  outs <- c("log_live_and_work","share_live_in","log_total_jobs","log_in_commuters")
  # Lay explanation: each group's curve is only drawn out to doses that
  # group actually has (its own 95th percentile) -- plotting further would
  # extrapolate the curve over empty space (caught 2026-07-06: Inc-bind was
  # being drawn to the POOLED max of ~0.85pp though its own doses end ~0.5).
  dmax <- quantile(stk[inc == 1 & dose > 0, dose], 0.95, na.rm = TRUE)  # pooled range (labels)
  SDT  <- "i(cell_sd, dose_sd, ref='NC')"     # SD-side control (net out SD changes, per headline)

  # Spline fit per group ----
  # Key design choice: subset to {this group's treated cells + NC} and fit a
  # CONTINUOUS-dose spline — NOT a cell-interacted spline. i(treat_cell,
  # ns_basis) absorbs the level and pinned Inc-bind ATT near zero (bug found
  # 2026-07-01); the subset approach is the working equivalent. Knots at the
  # group's own dose terciles; boundary at [0, max dose] so the basis is
  # linear beyond the observed range (ns property).

  # The blue dashed line is the paper's OWN estimate, read from the headline
  # and pooled result files rather than refitted here. Refitting on this
  # block's sample gave the same number to three or four decimals but not
  # always to the digit printed on the figure, so a reader checking the label
  # against Table 3 or Table 4 could find -0.285 against -0.284.
  .hl_pool <- tryCatch(fread(file.path(RESULTS_DIR, "pooled_results.csv")), error = function(e) NULL)
  .hl_cell <- tryCatch(fread(file.path(RESULTS_DIR, "headline_full_table.csv")), error = function(e) NULL)
  headline_acr <- function(treated_cells, o) {
    if (length(treated_cells) > 1L) {
      if (is.null(.hl_pool)) return(NA_real_)
      v <- .hl_pool[grepl("direction_muni::Inc", term) & outcome == o, est]
    } else {
      if (is.null(.hl_cell)) return(NA_real_)
      v <- .hl_cell[term == sprintf("treat_cell::%s:dose_stack", treated_cells) & outcome == o, est]
    }
    if (length(v)) as.numeric(v[1]) else NA_real_
  }

  fit_group <- function(treated_cells) {
    ATT <- list(); ACRT <- list(); BAS <- list(); QUA <- list(); keep <- c(treated_cells, "NC")
    dmax_g <- quantile(stk[as.character(treat_cell) %in% treated_cells & dose > 0, dose],
                       0.95, na.rm = TRUE)
    dg <- seq(0, dmax_g, length.out = 80)
    for (o in outs) {
      cols <- c("muni","dose","cell_sd","dose_sd","treat_cell","stack_muni","stack_year", o, ctrls)
      d <- stk[as.character(treat_cell) %in% keep, ..cols]; d <- d[complete.cases(d)]
      kk <- quantile(d[dose > 0, dose], c(1/3, 2/3), na.rm = TRUE)
      bb <- c(0, max(d[dose > 0, dose], na.rm = TRUE))
      # Lay explanation: ns() turns the single dose column into three fixed
      # curve-shaped columns -- like a flexible ruler cut at the knots. An
      # ordinary regression on those three columns can then trace a smooth
      # curve through the dose-response instead of forcing a straight line.
      B  <- ns(d$dose, knots = kk, Boundary.knots = bb); bn <- paste0("sp", seq_len(ncol(B)))
      d[, (bn) := as.data.table(unclass(B))]
      # Lay explanation: two fits of the same model. fS lets the effect bend
      # with dose (the spline columns); fL forces one straight line, and its
      # single slope is the reported headline number (the ACR). Plotting
      # both shows how much the straight line smooths over.
      fS <- feols(as.formula(paste0(o," ~ ",paste(bn,collapse=" + ")," + ",SDT," + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
      fL <- feols(as.formula(paste0(o," ~ dose + ",SDT," + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
      # Quadratic on the SAME sample and controls, so the spline and the
      # polynomial are directly comparable rather than differing in setup too.
      d[, dose2 := dose^2]
      fQ <- feols(as.formula(paste0(o," ~ dose + dose2 + ",SDT," + ",cs," | stack_muni + stack_year")), d, cluster=~muni)
      bq <- coef(fQ)[c("dose","dose2")]; Vq <- vcov(fQ)[c("dose","dose2"), c("dose","dose2")]
      q_p <- fQ$coeftable["dose2", "Pr(>|t|)"]
      csp <- coef(fS)[bn]; Vsp <- vcov(fS)[bn, bn]
      acr <- headline_acr(treated_cells, o)
      if (is.na(acr)) acr <- as.numeric(coef(fL)["dose"])
      # ATT(d) = B(d)'b - B(0)'b  (effect of dose d relative to dose 0); the
      # G %*% Vsp %*% G' diagonal gives delta-method variances for the curve.
      # Lay explanation: to read the fitted curve at each dose, evaluate the
      # basis there and subtract its value at dose zero. The gap between
      # "given this much dose" and "given none" is the total effect ATT(d).
      Bg <- predict(B, dg); B0 <- as.numeric(predict(B, 0)); G <- sweep(Bg, 2, B0)
      att_s <- as.numeric(G %*% csp); se_s <- sqrt(pmax(rowSums((G %*% Vsp) * G), 0))
      # ACRT(d) = dATT/dd via numerical derivative of the basis (central diff).
      # Lay explanation: the marginal effect ACRT(d) is the curve's slope.
      # Nudge the dose a hair up and a hair down, see how far the curve
      # moves, and divide -- a numerical speedometer reading at each dose.
      h <- 1e-4; Bp <- (predict(B, dg + h) - predict(B, dg - h)) / (2*h)
      acrt_s <- as.numeric(Bp %*% csp); se_a <- sqrt(pmax(rowSums((Bp %*% Vsp) * Bp), 0))
      ATT[[o]]  <- data.table(outcome=lab[o], dose=dg, y=att_s,  lo=att_s-1.96*se_s,  hi=att_s+1.96*se_s)
      ACRT[[o]] <- data.table(outcome=lab[o], dose=dg, y=acrt_s, lo=acrt_s-1.96*se_a, hi=acrt_s+1.96*se_a, acr=acr)
      # Basis parameters, so the interactive explorer can rebuild B(d) and show
      # the decomposition b_j * B_j(d) that sums to ATT(d).
      BAS[[o]] <- data.table(outcome=lab[o], knot1=kk[1], knot2=kk[2],
                             bk_lo=bb[1], bk_hi=bb[2],
                             b1=csp[1], b2=csp[2], b3=csp[3], acr=acr,
                             q1=bq[1], q2=bq[2], q_dose2_p=q_p)
      # Quadratic ATT(d) = b1 d + b2 d^2 and ACRT(d) = b1 + 2 b2 d, delta-method SEs.
      attq  <- bq[1]*dg + bq[2]*dg^2
      vattq <- dg^2*Vq[1,1] + dg^4*Vq[2,2] + 2*dg^3*Vq[1,2]
      acrtq <- bq[1] + 2*bq[2]*dg
      vacrq <- Vq[1,1] + 4*dg^2*Vq[2,2] + 4*dg*Vq[1,2]
      QUA[[o]] <- rbind(
        data.table(outcome=lab[o], dose=dg, curve="ATT_quad",  y=attq,
                   lo=attq-1.96*sqrt(pmax(vattq,0)), hi=attq+1.96*sqrt(pmax(vattq,0)), acr=NA_real_),
        data.table(outcome=lab[o], dose=dg, curve="ACRT_quad", y=acrtq,
                   lo=acrtq-1.96*sqrt(pmax(vacrq,0)), hi=acrtq+1.96*sqrt(pmax(vacrq,0)), acr=NA_real_))
    }
    list(ATT = rbindlist(ATT), ACRT = rbindlist(ACRT),
         BASIS = rbindlist(BAS), QUAD = rbindlist(QUA))
  }

  plot_group <- function(res, ttl, outfile) {
    P_ATT  <- "ATT(d): total effect"
    P_ACRT <- "ACRT(d): marginal effect + ACR"
    both <- rbind(res$ATT[, .(outcome,dose,y,lo,hi)][, panel := P_ATT],
                  res$ACRT[, .(outcome,dose,y,lo,hi)][, panel := P_ACRT])
    ord <- as.vector(t(outer(lab[outs], c(P_ATT, P_ACRT), paste, sep=" — ")))
    both[, fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
    acr <- unique(res$ACRT[, .(outcome, acr)])[, panel := P_ACRT]
    acr[, fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
    acr[, ltxt := sprintf("ACR = %.3f", acr)]
    # The linear model the paper actually estimates, drawn on BOTH panels so the
    # spline can be read against it rather than on its own. On the ATT panel the
    # linear model implies ATT(d) = ACR x d, a ray from the origin; on the ACRT
    # panel it implies a constant marginal effect equal to the ACR.
    lin <- unique(res$ACRT[, .(outcome, acr)])[
      , .(dose = c(0, dmax), y = c(0, acr * dmax)), by = .(outcome, acr)]
    lin[, panel := P_ATT]
    lin[, fc := factor(paste(outcome, panel, sep=" — "), levels=ord)]
    g <- ggplot(both, aes(dose, y)) +
      geom_hline(yintercept=0, linetype="dotted", color="grey70") +
      geom_ribbon(aes(ymin=lo, ymax=hi), fill="#1b7837", alpha=0.13, color=NA) +
      geom_line(color="#1b7837", linewidth=0.7) +
      geom_line(data=lin, aes(dose, y, group=outcome), color="#2166ac",
                linetype="longdash", linewidth=0.5, inherit.aes=FALSE) +
      geom_hline(data=acr, aes(yintercept=acr), color="#2166ac", linetype="longdash", linewidth=0.5) +
      geom_text(data=acr, aes(x=dmax*0.45, y=acr, label=ltxt), color="#2166ac", vjust=-0.5, size=2.6, inherit.aes=FALSE) +
      facet_wrap(~ fc, scales="free_y", ncol=2) +
      labs(title=ttl,
           subtitle="Natural-spline ns(dose), net of SD rate changes. Green = spline; blue dashed = the linear model the paper estimates (left: ACR x d; right: constant ACR).",
           x="dose (cumulative muni rate change, pp)", y=NULL) +
      theme_bw(base_size=9) + theme(plot.subtitle=element_text(size=8))
    ggsave(file.path(fig_dir, outfile), g, width=10, height=11, dpi=300)
  }

  ## Knot-placement diagnostic ----
  ## Where the spline is ALLOWED to bend, against the data that decide it:
  ## every treated muni-year dose (points, jittered) with the interior knots
  ## (tercile rule, solid) and boundary knots (dotted) computed EXACTLY as in
  ## fit_group() -- same subset, same quantile call, headline-outcome sample.
  knot_groups <- list("Pooled Increase" = c("Inc-bind", "Inc-becomes", "Inc-nobind"),
                      "Inc-bind" = "Inc-bind", "Inc-becomes" = "Inc-becomes")
  kd_pts <- list(); kd_kn <- list()
  for (gl in names(knot_groups)) {
    cols <- c("muni", "dose", "cell_sd", "dose_sd", "treat_cell",
              "stack_muni", "stack_year", "log_live_and_work", ctrls)
    dk <- stk[as.character(treat_cell) %in% c(knot_groups[[gl]], "NC"), ..cols]
    dk <- dk[complete.cases(dk)]
    # Lay explanation: kk = the two dose values where the fitted curve is
    # ALLOWED to bend. They are placed so a third of the treated observations
    # sit below the first and two-thirds below the second -- the bends go
    # where the data actually are, not where we guess.
    kk <- quantile(dk[dose > 0, dose], c(1/3, 2/3), na.rm = TRUE)
    # Lay explanation: bb = the ends of the curve's playground. Outside
    # [0, biggest observed dose] the curve is forced to be a straight line,
    # so a handful of extreme munis cannot make it swing wildly.
    bb <- c(0, max(dk[dose > 0, dose], na.rm = TRUE))
    # Lay explanation: keep every treated dose observation for the plot;
    # control munis all sit at exactly 0 and would just pile up there.
    kd_pts[[gl]] <- data.table(grp = gl, dose = dk[dose > 0, dose])
    kd_kn[[gl]]  <- data.table(grp = gl,
                               knot = c(kk, bb),
                               type = c("interior", "interior", "boundary", "boundary"))
  }
  kd_pts <- rbindlist(kd_pts); kd_kn <- rbindlist(kd_kn)
  kd_pts[, grp := factor(grp, levels = names(knot_groups))]
  kd_kn[, grp := factor(grp, levels = names(knot_groups))]
  kd_lab <- kd_kn[type == "interior"][, ltxt := sprintf("%.2f", knot)]
  g_kn <- ggplot(kd_pts, aes(x = dose, y = 0)) +
    geom_jitter(height = 0.32, alpha = 0.25, size = 1.1, color = "grey30") +
    geom_vline(data = kd_kn[type == "interior"], aes(xintercept = knot),
               color = "#2166ac", linewidth = 0.6) +
    geom_vline(data = kd_kn[type == "boundary"], aes(xintercept = knot),
               color = "grey55", linetype = "dotted", linewidth = 0.5) +
    geom_text(data = kd_lab, aes(x = knot, y = 0.48, label = ltxt),
              color = "#2166ac", size = 3, vjust = 0) +
    facet_wrap(~ grp, ncol = 1) +
    scale_y_continuous(limits = c(-0.55, 0.55), breaks = NULL) +
    labs(title = "Where the spline can bend: treated doses and knot placement",
         subtitle = "Points: treated muni-year doses (NC zeros excluded). Solid blue: interior knots (dose terciles, per group). Dotted: boundary knots (linear beyond).",
         x = "dose (cumulative muni rate change, pp)", y = NULL) +
    theme_bw(base_size = 10) +
    theme(plot.subtitle = element_text(size = 8))
  ggsave(file.path(fig_dir, "spline_knot_placement.png"), g_kn,
         width = 9, height = 5.5, dpi = 300)

  res_pooled     <- fit_group(c("Inc-bind","Inc-becomes","Inc-nobind"))
  res_incbind    <- fit_group("Inc-bind")
  res_incbecomes <- fit_group("Inc-becomes")
  plot_group(res_pooled,     "Muni dose-response (spline) — POOLED Increase",  "acr_acrt_att_spline_pooled.png")
  plot_group(res_incbind,    "Muni dose-response (spline) — Inc-bind cell",    "acr_acrt_att_spline_incbind.png")
  plot_group(res_incbecomes, "Muni dose-response (spline) — Inc-becomes cell", "acr_acrt_att_spline_incbecomes.png")

  ## Export the fitted curves so the interactive explorer (shiny/spline_explorer)
  ## can read them without refitting the stacked panel.
  export_curves <- function(res, grp) rbind(
    res$ATT[,  .(group = grp, outcome, dose, curve = "ATT",  y, lo, hi, acr = NA_real_)],
    res$ACRT[, .(group = grp, outcome, dose, curve = "ACRT", y, lo, hi, acr)],
    res$QUAD[, .(group = grp, outcome, dose, curve, y, lo, hi, acr)])
  fwrite(rbind(export_curves(res_pooled,     "Pooled Increase"),
               export_curves(res_incbind,    "Inc-bind"),
               export_curves(res_incbecomes, "Inc-becomes")),
         file.path(RESULTS_DIR, "spline_curves.csv"))
  fwrite(rbind(copy(res_pooled$BASIS)[,     group := "Pooled Increase"],
               copy(res_incbind$BASIS)[,    group := "Inc-bind"],
               copy(res_incbecomes$BASIS)[, group := "Inc-becomes"]),
         file.path(RESULTS_DIR, "spline_basis.csv"))
  message("Saved data/results/spline_curves.csv + spline_basis.csv")

  # (A compact 2-panel per-cell ATT figure was removed 2026-07-06: the paper
  #  uses the full per-cell plot_group() figures, same layout as the pooled one.)
})

# feols ES by cell (build_feols_es_bycell.R) ----
# feols realized-dose event study, by GROUP (pooled / Inc-bind / Inc-becomes) x 4
# outcomes, in the SAME faceted/3-group layout as figures/dcdh_bycell_es.png. The
# coefficients are rescaled to the LEVEL-AT-MEAN-DOSE scale (beta_k x mean realized
# dose) so this is a direct twin of the dCDH figure: pooled and Inc-becomes should
# track the dCDH paths; Inc-bind shows the feols view (strong) vs the attenuated
# dCDH (continuous estimator can't see the discrete binding rule).
# Output: figures/feols_es_bycell.png + data/results/feols_es_bycell.csv
if (RUN_FEOLS_ES_BYCELL) local({
  out_dir <- RESULTS_DIR; fig_dir <- OUT_DIR
  K <- 5L; WIN <- c(-5L, 5L)   # full stacked event window (matches dCDH at +-5)

  panel <- copy(env$panel); ctrls <- env$ctrls; cs <- paste(ctrls, collapse = " + ")
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind","Dec-bind","Dec-becomes-non","Dec-nobind")

  # realized muni dose (cumulative dtau, c-1 -> c+5)  [from q3_unified_headline_es.R]
  mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu_year, muni, year)
  rd <- mu_year[!is.na(c_muni), {
    pre <- resident_eit[year == c_muni - 1L]; yr_max <- min(c_muni + K, max(year, na.rm = TRUE))
    end <- resident_eit[year == yr_max]
    list(realized_dose_muni = (if (length(end) == 1) end else NA_real_) -
                              (if (length(pre) == 1) pre else NA_real_))
  }, by = muni]
  rd[is.na(realized_dose_muni), realized_dose_muni := 0]
  rd <- rbind(rd, data.table(muni = unique(panel[cell_muni == "NC", muni]), realized_dose_muni = 0))
  panel <- merge(panel, rd, by = "muni", all.x = TRUE); panel[is.na(realized_dose_muni), realized_dose_muni := 0]

  build_stack <- function(p, K = 5L) {
    treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni)]); ncm <- unique(p[cell_muni == "NC", muni])
    rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
      win <- (cy - K):(cy + K); tm <- treated[cohort == cy]; if (!nrow(tm)) return(NULL)
      dt <- p[muni %in% tm$muni & year %in% win]; dt[, treat_cell_muni := cell_muni]
      dc <- p[muni %in% ncm & year %in% win];     dc[, treat_cell_muni := "NC"]
      d <- rbind(dt, dc, fill = TRUE); d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
    }), fill = TRUE)
  }
  stk <- build_stack(panel)
  stk[, stack_muni := paste0(stack, "_", muni)]; stk[, stack_year := paste0(stack, "_", year)]
  stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
  stk[, cell_sd := factor(cell_sd, levels = cell_levels)]; stk[, cell_nr := factor(cell_nr, levels = cell_levels)]
  stk[is.na(dose_sd), dose_sd := 0]; stk[is.na(dose_nr), dose_nr := 0]

  outs <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
  grp  <- list(pooled = c("Inc-bind","Inc-becomes","Inc-nobind"), incbind = "Inc-bind", incbecomes = "Inc-becomes")

  res <- list()
  for (gn in names(grp)) {
    keep <- c("NC", grp[[gn]])
    mean_dose <- stk[treat_cell_muni %in% grp[[gn]] & realized_dose_muni > 0, mean(realized_dose_muni, na.rm = TRUE)]
    for (o in outs) {
      cols <- c("muni","rel","stack_muni","stack_year","treat_cell_muni","realized_dose_muni",
                "cell_sd","dose_sd", o, ctrls)
      dd <- stk[treat_cell_muni %in% keep, ..cols]; dd <- dd[complete.cases(dd)]
      f <- as.formula(paste0(o, " ~ i(rel, realized_dose_muni, ref = -1) + i(cell_sd, dose_sd, ref = 'NC')",
                             " + ", cs, " | stack_muni + stack_year"))
      m <- feols(f, dd, cluster = ~muni)
      ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
      ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
      res[[paste(gn, o)]] <- ct[, .(group = gn, outcome = o, rel, beta = Estimate, se = `Std. Error`, mean_dose)]
    }
  }
  R <- rbindlist(res)
  R <- rbind(R, unique(R[, .(group, outcome, mean_dose)])[, .(group, outcome, rel = -1L, beta = 0, se = 0, mean_dose)], fill = TRUE)
  # Lay explanation: the regression's beta_k is the effect per percentage
  # point of tax. Multiplying by the group's typical total rate change
  # converts it to the effect of a typical event -- the same yardstick the
  # alternative dCDH estimator reports, so the two figures line up.
  R[, `:=`(est = beta * mean_dose, lo = (beta - 1.96 * se) * mean_dose, hi = (beta + 1.96 * se) * mean_dose)]
  R <- R[rel >= WIN[1] & rel <= WIN[2]]
  fwrite(R, file.path(out_dir, "feols_es_bycell.csv"))

  lab <- c(log_live_and_work="Log(work in own muni)", log_in_commuters="Log(in-commuters)",
           log_total_jobs="Log(total jobs)", share_live_in="Share live in muni")
  glab <- c(pooled="Pooled Increase", incbind="Inc-bind", incbecomes="Inc-becomes")
  R[, `:=`(ol = lab[outcome], gl = factor(glab[group], levels = glab))]
  g <- ggplot(R, aes(rel, est, color = gl, fill = gl)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~ ol, scales = "free_y") +
    scale_color_manual(values = c("Pooled Increase"="#4d4d4d","Inc-bind"="#b2182b","Inc-becomes"="#2166ac"), name = NULL) +
    scale_fill_manual(values  = c("Pooled Increase"="#4d4d4d","Inc-bind"="#b2182b","Inc-becomes"="#2166ac"), name = NULL) +
    labs(title = "feols realized-dose event study by group -- twin of the dCDH figure",
         subtitle = "beta_k x mean realized dose (level-at-mean-dose scale, comparable to dCDH); SD + nonres controls",
         x = "periods since rate change", y = "effect at mean dose") +
    theme_bw(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, "feols_es_bycell.png"), g, width = 9, height = 6, dpi = 300)
  print(R[rel %in% c(-2, 3), .(group, outcome, rel, est = round(est, 3))][order(group, outcome, rel)])
})

# Law timeline (build_law_timeline.R) ----
# Background figure: timeline of the PA local-tax statutes (EIT + property) that
# generate or constrain the variation, with the estimation window shaded.
# Output: figures/law_timeline.png
if (RUN_LAW_TIMELINE) local({
  fig_dir <- OUT_DIR

  # Every statute walked through in the Institutional Background section, in
  # order. `type` marks the 2012 binding date (the only label set in bold);
  # `ypos` staggers labels so temporal neighbours sit on different levels (the
  # 1998-2012 cluster is the tight one).
  ev <- data.table(
    year  = c(1932, 1965, 1972, 1984, 1987, 1998, 2001, 2006, 2008, 2012),
    name  = c("Sterling Act", "Act 511", "Home Rule", "Act 205", "Act 47",
              "Act 50", "Act 24", "Act 1", "Act 32 enacted", "Act 32 effective"),
    desc  = c("Phila. wage tax\n(outside Act 32)",
              "Local Tax Enabling Act\n1% cap shared 50/50;\nresident credit",
              "Charter Law\n(residents only)",
              "Pension distress\n(may exceed cap,\nincl. nonresidents)",
              "Municipal distress\n(may exceed cap,\nincl. nonresidents)",
              "SD EIT to 1.5%\nby referendum",
              "SD occupation-tax swap;\nmunis added 2002",
              "Taxpayer Relief Act\n(SD EIT above cap\nby referendum)",
              "consolidated\nwithholding",
              "max-rule binding\nvia withholding"),
    type  = c("EIT statute", "EIT statute", "EIT statute", "EIT statute",
              "EIT statute", "EIT statute", "EIT statute",
              "EIT statute", "EIT statute", "Binding date"),
    ypos  = c( 1.0, -1.0,  2.4, -2.4,  1.0, -1.0,  2.4, -2.4,  1.0, -1.0))
  ev[, lab := paste0(name, " (", year, ")\n", desc)]
  ev[, face := ifelse(type == "Binding date", "bold", "plain")]  # bold the binding date

  samp <- data.table(xmin = 2002, xmax = 2019)

  g <- ggplot() +
    # estimation window
    geom_rect(data = samp, aes(xmin = xmin, xmax = xmax, ymin = -4.0, ymax = 3.9),
              fill = "grey85", alpha = 0.5) +
    annotate("text", x = 2010.5, y = 3.7, label = "Estimation sample 2002–2019",
             size = 3, color = "grey35", fontface = "italic") +
    # the timeline spine
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
    # connectors + points + labels
    geom_segment(data = ev, aes(x = year, xend = year, y = 0, yend = ypos),
                 color = "black", linewidth = 0.5) +
    geom_point(data = ev, aes(x = year, y = 0), color = "black", size = 3) +
    geom_label(data = ev, aes(x = year, y = ypos, label = lab),
               color = "black", size = 2.5, linewidth = 0, fill = "white",
               lineheight = 0.9, vjust = ifelse(ev$ypos > 0, 0, 1),
               fontface = ev$face) +
    scale_x_continuous(breaks = seq(1930, 2020, 10), limits = c(1926, 2024)) +
    scale_y_continuous(limits = c(-4.0, 3.9)) +
    labs(title = "Timeline of Pennsylvania local-tax rules",
         subtitle = "Statutes governing the Earned Income Tax (EIT) and shaping the design",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "grey30"),
          axis.line.x = element_line(color = "grey60"),
          legend.position = "none",
          plot.title = element_text(face = "bold"))

  ggsave(file.path(fig_dir, "law_timeline.png"), g, width = 11, height = 6.5, dpi = 300)
})

# Design schematics ----
# Two CONCEPTUAL schematic figures explaining the research design. All
# numbers are hand-made and purely illustrative (no estimation, no data
# files): (1) the max-rule binding mechanism behind the Inc-bind /
# Inc-becomes / Inc-nobind cells, and (2) why the constant realized-dose
# tag can test pre-trends when the mechanically zero pre-event dose
# cannot. (A third stacking-construction schematic was cut from the
# paper 2026-07-22.)
# Outputs: figures/schematic_binding_mechanism.png,
#   figures/schematic_dose_vs_tag.png
if (RUN_DESIGN_SCHEMATICS) local({
  fig_dir <- OUT_DIR
  ACCENT  <- "#C5283D"

  ## 1. The max-rule binding mechanism ----
  # Lay explanation: a worker pays whichever is bigger -- the home-side
  # stack (grey bar) or the workplace rate (dashed floor). The red tick
  # marks the actual bill. The same statutory increase moves the bill
  # fully, partly, or not at all depending on where the bar sits relative
  # to the floor -- that is the whole cell taxonomy in one picture.
  floor_pp <- 1.0
  cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  bars <- data.table(
    cell   = factor(rep(cells, each = 2), levels = cells),
    period = factor(rep(c("Before", "After"), 3), levels = c("Before", "After")),
    stack  = c(1.4, 1.8, 0.8, 1.3, 0.6, 0.9))
  bars[, bill := pmax(stack, floor_pp)]
  ann1 <- data.table(
    cell = factor(cells, levels = cells),
    lab  = c("Bill: +0.4 pp\n(full increase)",
             "Bill: +0.3 pp\n(only the part above the floor)",
             "Bill: +0.0 pp\n(placebo)"))
  g1 <- ggplot(bars, aes(x = period)) +
    geom_col(aes(y = stack), fill = "grey78", color = "grey45",
             width = 0.55, linewidth = 0.3) +
    geom_hline(yintercept = floor_pp, linetype = "dashed",
               color = "grey40", linewidth = 0.5) +
    geom_errorbar(aes(ymin = bill, ymax = bill), width = 0.7,
                  color = ACCENT, linewidth = 1.3) +
    geom_text(data = ann1, aes(x = 1.5, y = 2.12, label = lab),
              color = ACCENT, size = 2.6, lineheight = 0.95) +
    annotate("text", x = 1.5, y = floor_pp + 0.08,
             label = "work-rate floor = 1.0 pp",
             size = 2.3, color = "grey35") +
    facet_wrap(~ cell, ncol = 3) +
    scale_y_continuous(limits = c(0, 2.28),
                       expand = expansion(mult = c(0, 0.01))) +
    labs(title = "The max-rule: when does a rate increase move the bill?",
         subtitle = "Grey bars: home stack (resident + SD rate), before vs after an increase. Red tick: bill = max(home stack, work-rate floor). Illustrative rates, not estimates.",
         x = NULL, y = "Rate (percentage points)") +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 7.5, color = "grey25"),
          strip.background = element_rect(fill = "grey95", color = NA),
          strip.text = element_text(face = "bold", size = 9),
          panel.grid = element_blank())
  ggsave(file.path(fig_dir, "schematic_binding_mechanism.png"), g1,
         width = 9, height = 4, dpi = 300)

  ## 2. Dose vs realized-dose tag ----
  # Lay explanation: the headline regressor (the dose) is zero before the
  # event by construction, so it carries no pre-event information -- it
  # cannot reveal a pre-trend. The event study instead tags each muni with
  # its eventual total change (a constant), and interacts that tag with
  # event-time dummies: if big-dose munis were already drifting before the
  # event, the pre-event interactions light up.
  p1 <- "Dose (used by the headline model)"
  p2 <- "Realized-dose tag (used by the event study)"
  dose3 <- data.table(k = -5:5)
  dose3[, y := fifelse(k < 0, 0, fifelse(k < 3, 0.25, 0.35))]
  dose3[, panel := p1]
  tag3 <- data.table(k = c(-5, 5), y = 0.35, panel = p2)
  d3 <- rbind(dose3, tag3)
  d3[, panel := factor(panel, levels = c(p1, p2))]
  notes3 <- data.table(
    panel = factor(c(p1, p2), levels = c(p1, p2)),
    x = -4.9, y = c(0.42, 0.10),
    lab = c("Mechanically zero before the event,\nso it cannot test pre-trends.",
            "A constant magnitude tag:\ninteracted with event-time\ndummies it CAN test whether\nbig-dose munis drifted\nbefore the event."))
  tagl3 <- data.table(panel = factor(p2, levels = c(p1, p2)), x = 0, y = 0.375,
                      lab = 'tilde(D)[i]*" = 0.35 pp (constant)"')
  g3 <- ggplot(d3, aes(x = k, y = y)) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey55",
               linewidth = 0.4) +
    geom_step(data = d3[panel == p1], color = ACCENT, linewidth = 0.8) +
    geom_line(data = d3[panel == p2], color = ACCENT, linewidth = 0.8) +
    geom_text(data = tagl3, aes(x = x, y = y, label = lab), parse = TRUE,
              color = ACCENT, size = 2.8, inherit.aes = FALSE) +
    geom_text(data = notes3, aes(x = x, y = y, label = lab), size = 2.4,
              color = "grey30", hjust = 0, lineheight = 0.95,
              inherit.aes = FALSE) +
    facet_wrap(~ panel, ncol = 2) +
    scale_x_continuous(breaks = -5:5) +
    scale_y_continuous(limits = c(-0.02, 0.47)) +
    labs(title = "Why the event study can test pre-trends: dose vs realized-dose tag",
         subtitle = "One representative municipality; illustrative magnitudes (0.25 pp at k = 0, topped up to 0.35 pp at k = +3).",
         x = "Event time k (years relative to first event)",
         y = "Percentage points") +
    theme_bw(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 8, color = "grey25"),
          strip.background = element_rect(fill = "grey95", color = NA),
          strip.text = element_text(face = "bold", size = 9),
          panel.grid.minor = element_blank())
  ggsave(file.path(fig_dir, "schematic_dose_vs_tag.png"), g3,
         width = 9, height = 3.5, dpi = 300)
})

# Tax-revenue mix by source (build_tax_mix_figure.R) ----
# Composition of local tax revenue in PA, 2002-2019, as each source's share of
# its own layer's total tax revenue. Two panels, because the two rate-setting
# layers in the design have opposite revenue structures and pooling them would
# bury the EIT under school-district property tax:
#   (a) municipalities -- DCED AFR, six sources
#   (b) school districts -- Census F-33, four sources (SCHLEV 03 only)
# Shares are of statewide summed dollars, not the mean of unit-level shares, so
# the figure describes where PA local tax revenue actually comes from rather
# than what the median jurisdiction looks like.
# PHILADELPHIA IS EXCLUDED from both panels. Its wage tax is a Sterling Act levy
# predating Act 511, outside the max-rule the paper studies, and it is dropped
# from the estimation sample. It is also 43-47% of all municipal tax revenue in
# the state, so leaving it in inverts the municipal panel: EIT 46% / property 32%
# with Philadelphia, property 46% / EIT 36% without.
# Outputs -> figures/tax_revenue_mix.png, data/results/tax_revenue_mix.csv
# Lay explanation: this is the institutional backdrop for the paper -- it shows
# that the earned income tax funds about a third of municipal budgets, roughly
# on par with property tax, while school districts run almost entirely on
# property tax and collect only about a tenth of their taxes from the EIT.
if (RUN_TAX_MIX_FIG) local({
  fig_dir <- OUT_DIR; YRS <- c(2002L, 2019L)

  ## --- municipal: DCED Annual Financial Report -------------------------------
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  afr[, yr := as.integer(ifelse(!is.na(reporting_year), reporting_year, year))]
  afr <- afr[!is.na(muni_id) & yr %between% YRS &
             !municipality_name %like% "PHILADELPHIA"]

  # DCED changed the AFR export format in 2006: 2002-2005 rows carry the short
  # column names and 2006+ rows the "*_revenues" names, side by side as separate
  # columns of the same file. Coalesce new-over-old, as build_headline_env() does.
  cz <- function(new, old) {
    v <- suppressWarnings(as.numeric(afr[[new]]))
    w <- suppressWarnings(as.numeric(afr[[old]]))
    fifelse(!is.na(v), v, w)
  }
  add <- function(...) {
    m <- cbind(...)
    out <- rowSums(m, na.rm = TRUE)
    out[rowSums(!is.na(m)) == 0] <- NA_real_
    out
  }
  muni <- afr[, .(
    year        = yr,
    `Property`  = cz("real_estate_tax_revenues",   "real_estate_tax"),
    `Income (EIT)` = cz("earned_income_tax_revenues", "earned_income_tax"),
    `Realty transfer` = cz("realty_transfer_tax_revenues", "realty_transfer_tax"),
    `Sales and gross receipts` = add(cz("business_gross_receipts_tax_revenues", "business_gross_recpts"),
                                     cz("amusement_and_admissions_tax_revenues", "amusmnt_admsns"),
                                     cz("mechanical_device_tax_revenues", "mechanical_devices_tax")),
    # per capita + occupation + local services (the occupational privilege tax
    # before Act 222 of 2004 renamed it) -- all levied per head, not on a base
    `Head taxes` = add(cz("per_capita_tax_revenues", "per_capita_tax"),
                       cz("occupational_tax_revenues", "occupation_tax"),
                       cz("local_services_tax_revenues", "occptnl_priv_tax")),
    `Other`     = cz("all_other_taxes_revenues", "all_other_taxes"),
    reported_total = cz("total_taxes_revenues", "total_taxes"))]

  cats_m <- c("Property", "Income (EIT)", "Realty transfer",
              "Sales and gross receipts", "Head taxes", "Other")
  agg_m <- muni[, c(lapply(.SD, sum, na.rm = TRUE), .(n_units = .N)),
                by = year, .SDcols = c(cats_m, "reported_total")]

  # The itemised lines do not always exhaust DCED's own reported tax total --
  # a handful of cities (Pittsburgh above all) collect taxes that the AFR folds
  # into the total without giving them a line. Rather than drop those dollars or
  # quietly change the denominator, the unitemised residual is added to "Other",
  # so shares are of DCED's reported total and sum to 100 by construction.
  agg_m[, itemised := rowSums(.SD), .SDcols = cats_m]
  agg_m[, residual := pmax(reported_total - itemised, 0)]
  print(sprintf("[tax mix] muni: %d years, %d-%d units/yr, unitemised residual %.1f-%.1f%% of reported taxes",
                nrow(agg_m), min(agg_m$n_units), max(agg_m$n_units),
                100 * min(agg_m$residual / agg_m$reported_total),
                100 * max(agg_m$residual / agg_m$reported_total)))
  if (agg_m[, max(residual / reported_total)] > 0.10)
    warning("unitemised municipal tax residual exceeds 10% in some year")
  agg_m[, Other := Other + residual]

  ## --- school district: Census F-33 -----------------------------------------
  # SCHLEV 03 = operating local school districts; the other levels (charters,
  # supervisory unions, service agencies) report no tax revenue. F-33 dollars
  # are in thousands, so scale to match the AFR.
  f33 <- fread(file.path(RAW_DIR, "pa_sd_finance_raw.csv"))
  f33 <- f33[SCHLEV == "03" & fy %between% YRS & !NAME %like% "PHILADELPHIA"]
  sd_dt <- f33[, .(year = as.integer(fy),
                   `Property`     = as.numeric(T06) * 1000,
                   `Income (EIT)` = as.numeric(T40) * 1000,
                   `Sales and gross receipts` = (as.numeric(T09) + as.numeric(T15)) * 1000,
                   `Other`        = as.numeric(T99) * 1000)]
  cats_s <- c("Property", "Income (EIT)", "Sales and gross receipts", "Other")
  agg_s <- sd_dt[, c(lapply(.SD, sum, na.rm = TRUE), .(n_units = .N)),
                 by = year, .SDcols = cats_s]
  print(sprintf("[tax mix] SD: %d years, %d-%d districts/yr",
                nrow(agg_s), min(agg_s$n_units), max(agg_s$n_units)))

  ## --- shares ---------------------------------------------------------------
  to_long <- function(a, cats, lab) {
    l <- melt(a[, c("year", cats), with = FALSE], id.vars = "year",
              variable.name = "source", value.name = "dollars")
    l[, share := 100 * dollars / sum(dollars), by = year]
    l[, layer := lab][]
  }
  long <- rbind(to_long(agg_m, cats_m, "(a) Municipalities (DCED AFR)"),
                to_long(agg_s, cats_s, "(b) School districts (Census F-33)"))
  long[, source := factor(source, levels = cats_m)]
  fwrite(long[order(layer, source, year)], file.path(RESULTS_DIR, "tax_revenue_mix.csv"))
  print(long[year %in% c(2002, 2019)][order(layer, -share),
             .(layer, year, source, share = round(share, 1))])

  ## --- figure ---------------------------------------------------------------
  # Colour and linetype both encode the source, so the panels stay readable in
  # greyscale; the Okabe-Ito palette keeps them separable for colour-blind readers.
  pal <- c("Property" = "#0072B2", "Income (EIT)" = "#D55E00",
           "Realty transfer" = "#009E73", "Sales and gross receipts" = "#CC79A7",
           "Head taxes" = "#E69F00", "Other" = "#666666")
  lty <- c("Property" = "solid", "Income (EIT)" = "solid",
           "Realty transfer" = "dashed", "Sales and gross receipts" = "dotted",
           "Head taxes" = "dotdash", "Other" = "longdash")
  lwd <- c("Property" = 1.0, "Income (EIT)" = 1.0, "Realty transfer" = 0.6,
           "Sales and gross receipts" = 0.6, "Head taxes" = 0.6, "Other" = 0.6)

  g <- ggplot(long, aes(year, share, colour = source,
                        linetype = source, linewidth = source)) +
    geom_line() +
    # Free y: school-district property tax sits near 86%, and a shared 0-100 axis
    # would compress every other source into an unreadable band at the floor.
    facet_wrap(~ layer, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = pal, name = NULL, drop = FALSE) +
    scale_linetype_manual(values = lty, name = NULL, drop = FALSE) +
    scale_linewidth_manual(values = lwd, name = NULL, drop = FALSE, guide = "none") +
    scale_x_continuous(breaks = seq(2002, 2018, 4)) +
    scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks(6),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(title = "Shares of total local tax revenue, Pennsylvania 2002-2019",
         subtitle = paste("Each source as a percent of its own layer's total tax revenue; statewide summed dollars.",
                          "Philadelphia excluded: its wage tax is a Sterling Act levy outside the Act 511 max-rule.",
                          sep = "\n"),
         x = "Year", y = "Share of total taxes (%)") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 8, colour = "grey25"),
          strip.background = element_rect(fill = "grey95", colour = NA),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()) +
    guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
  ggsave(file.path(fig_dir, "tax_revenue_mix.png"), g,
         width = 10, height = 5, dpi = 300)
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()
