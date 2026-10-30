#!/usr/bin/env Rscript
# 1_figures.R -- every figure, plus the descriptive tables (summary statistics
# and cell counts). Run after the model scripts: several sections read their CSVs.
# Each section is switched by a RUN_* flag. PARKED sections are not in the paper.
# Writes figures/*.png, tables/summary_*.tex, tables/cell_counts_summary.tex and
# .csv copies in data/results/.
# Run: Rscript scripts/4_output/1_figures.R

suppressMessages({
  library(dplyr)       # loaded before data.table so data.table's first/last/between win
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(sf)
  library(ggrepel)
  library(splines)
})

source(here::here("scripts", "helpers", "helpers.R"))  # paths, panel/stack builder, shared helpers

# Flags ----
RUN_SUMMARY_STATS        <- TRUE   # summary tables + rate ECDF/CDF + map + binding-levels figs
RUN_DOSE_TRAJECTORY      <- FALSE  # PARKED 2026-10-29: Inc-bind + Inc-becomes dose trajectories; not in the paper, source of the 0.24 / 0.38 typical-event doses
RUN_SPLINE_DOSE_RESPONSE <- TRUE   # ns(dose) ATT/ACRT dose-response (pooled, Inc-bind, Inc-becomes)
RUN_REVENUE_TRADEOFF_FIGS <- FALSE # PARKED 2026-10-29, not in the paper: workers vs EIT-revenue dose response, and the tradeoff against the pre-existing rate
RUN_COHORT_ATT_FIG <- FALSE # PARKED 2026-10-29, not in the paper: one ATT per stack, Cengiz-style event distribution
RUN_LEVEL_ADJUST_FIG <- FALSE # PARKED 2026-10-29, not in the paper: realized-dose ES with and without the tau0 trend adjustment
RUN_FEOLS_ES_BYCELL      <- TRUE   # feols realized-dose ES by group (twin of dcdh_bycell_es)
RUN_LAW_TIMELINE         <- TRUE   # PA local-tax statute timeline

# Shared setup ----
needs_env <- RUN_SUMMARY_STATS || RUN_DOSE_TRAJECTORY ||
  RUN_SPLINE_DOSE_RESPONSE || RUN_FEOLS_ES_BYCELL
if (needs_env) {
  env <- build_headline_env()
}

# Summary stats ----
# Tables summary_muni, summary_block, summary_sd and cell_counts_summary (.tex,
# with .csv copies), and the figures rate_double_histogram, act32_cdf,
# rate_changes_by_year, pa_rate_trajectories, pa_map, binding_levels_by_year.
if (RUN_SUMMARY_STATS) local({
  max_year <- 2019L
  rate_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))[year <= max_year]
  work_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_panel.rds")))[year <= max_year]
  block_panel <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_block_panel.rds")))[year <= max_year]
  sd_controls <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_sd_controls.rds")))

  # ---- summary-table helpers
  summarize_vec <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0) return(rep(NA_real_, 7))
    c(N = length(values), mean = mean(values), sd = sd(values),
      p25 = quantile(values, 0.25), p50 = median(values), p75 = quantile(values, 0.75),
      min = min(values), max = max(values))
  }
  summary_row <- function(label, values) {
    stats <- summarize_vec(values)
    tibble(label = label, N = stats[1], mean = stats[2], sd = stats[3],
           p25 = stats[4], p50 = stats[5], p75 = stats[6], min = stats[7], max = stats[8])
  }
  # a label starting "PANEL:" prints as a section header
  panel_header <- function(title) tibble(label = paste0("PANEL: ", title))

  write_summary_table <- function(rows, caption, label_id, out_name, notes = character(0)) {
    rows <- bind_rows(rows)
    body <- c(
      "\\begin{table}[H]", "  \\centering",
      sprintf("  \\caption{%s}", caption),
      sprintf("  \\label{%s}", label_id),
      "  \\small",
      "  \\begin{tabular}{lrrrrr}",
      "    \\toprule",
      "    Variable & N & Mean & SD & Min & Max \\\\",
      "    \\midrule")
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, ]
      if (grepl("^PANEL:", row$label)) {
        body <- c(body, "    \\addlinespace",
                  sprintf("    \\multicolumn{6}{l}{\\textit{%s}} \\\\", sub("^PANEL:\\s*", "", row$label)),
                  "    \\addlinespace")
        next
      }
      body <- c(body, sprintf("    %s & %s & %.2f & %.2f & %.2f & %.2f \\\\",
                              row$label, format(as.integer(row$N), big.mark = ","),
                              row$mean, row$sd, row$min, row$max))
    }
    body <- c(body, "    \\bottomrule", "  \\end{tabular}",
              "  \\vspace{0.3em}",
              "  \\begin{minipage}{0.95\\textwidth}",
              sprintf("    \\footnotesize \\textit{Notes:} %s", paste(notes, collapse = " ")),
              "  \\end{minipage}",
              "\\end{table}")
    writeLines(body, file.path(TABLE_DIR, out_name))
    fwrite(rows, file.path(RESULTS_DIR, sub("\\.tex$", ".csv", out_name)))
  }

  # ---- 1. muni-year summary on the estimation sample: the panel after the
  # flip-flopper and decrease drops, less muni-years missing a fiscal control.
  # Each row reports its own non-missing N.
  ctrls <- env$ctrls
  estimation <- env$panel %>% filter(if_all(all_of(ctrls), ~ !is.na(.x)))
  raw_munis <- rename(work_panel, muni = work_city)
  sample_steps <- tibble(
    step = c("Raw cousub panel, 2002--2019",
             "Less flip-floppers and decrease munis",
             "Less muni-years missing a fiscal control"),
    munis      = c(n_distinct(raw_munis$muni), n_distinct(env$panel$muni), n_distinct(estimation$muni)),
    muni_years = c(nrow(raw_munis), nrow(env$panel), nrow(estimation)))
  fwrite(sample_steps, file.path(RESULTS_DIR, "sample_construction.csv"))

  # rates first, then the stack, then the work-side rate it is compared with;
  # shares in percent so two decimals stay informative
  muni_rows <- list(
    panel_header("Tax rates (percentage points)"),
    summary_row("Resident EIT rate $\\tau^{M}$",            estimation$resident_eit),
    summary_row("School-district EIT rate $\\tau^{SD}$",    estimation$sd_eit),
    summary_row("Home stack $\\tau^{M}+\\tau^{SD}$",        estimation$home_stack),
    summary_row("Nonresident EIT rate $\\tau^{nr,M}$",      estimation$muni_nonres_eit),
    summary_row("Effective property rate, muni",            estimation$muni_eff_rate),
    summary_row("Effective property rate, school district", estimation$sd_eff_rate),
    summary_row("Effective property rate, county",          estimation$county_eff_rate),
    panel_header("Outcomes"),
    summary_row("Live-and-work workers",                    estimation$live_and_work),
    summary_row("Share live-and-work in muni (\\%)",     100 * estimation$share_live_in),
    summary_row("In-commuters",                             estimation$in_commuters),
    summary_row("Total jobs",                               estimation$total_jobs),
    panel_header("Fiscal controls (percent of total)"),
    summary_row("Police, share of expenditure",             100 * estimation$share_police),
    summary_row("Streets, share of expenditure",            100 * estimation$share_streets),
    summary_row("Debt service, share of expenditure",       100 * estimation$share_debt_svc),
    summary_row("Intergovernmental, share of revenue",      100 * estimation$share_intergov)
  )
  write_summary_table(muni_rows,
    "Estimation-sample summary statistics, muni-year (2002--2019)",
    "tab:summary_muni", "summary_muni.tex",
    notes = c(sprintf(paste("Estimation sample: %s muni-years across %s municipalities,",
                            "from a raw panel of %s. Each row reports its own non-missing $N$."),
                      format(nrow(estimation), big.mark = ","),
                      format(n_distinct(estimation$muni), big.mark = ","),
                      format(sample_steps$muni_years[1], big.mark = ",")),
              "Rates in percentage points. Effective property rates are millage times the county",
              "common-level ratio, after the repair in Section~\\ref{subsec:sources}."))

  # Means by muni type (city / borough / township), joined on the cousub code.
  # Every cousub-year has exactly one type, so the join cannot fan out.
  muni_types <- rate_panel %>%
    filter(!is.na(cousub_fips)) %>%
    distinct(cousub_fips, year, municipality) %>%
    mutate(muni_type = case_when(grepl("\\bCITY\\b", toupper(municipality)) ~ "City",
                                 grepl("\\bBORO|BOROUGH\\b", toupper(municipality)) ~ "Borough",
                                 grepl("\\bTWP|TOWNSHIP\\b", toupper(municipality)) ~ "Township",
                                 TRUE ~ "Other")) %>%
    distinct(cousub_fips, year, muni_type)
  estimation %>%
    inner_join(muni_types, by = c("muni" = "cousub_fips", "year")) %>%
    group_by(muni_type) %>%
    summarise(N_muni_years = n(),
              mean_res = mean(resident_eit, na.rm = TRUE),
              mean_nr  = mean(muni_nonres_eit, na.rm = TRUE),
              mean_sd  = mean(sd_eit, na.rm = TRUE),
              mean_stk = mean(resident_eit + sd_eit, na.rm = TRUE),
              mean_jobs = mean(total_jobs, na.rm = TRUE),
              mean_share_in = mean(share_live_in, na.rm = TRUE), .groups = "drop") %>%
    fwrite(file.path(RESULTS_DIR, "summary_muni_by_type.csv"))

  # ---- 2. block-year summary (not in the paper). data.table: ~5M rows
  block_rows <- list(
    summary_row("Resident rate $\\tau^{M}$ (\\%)",  block_panel$resident_eit),
    summary_row("Nonresident rate $\\tau^{nr,M}$ (\\%)", block_panel$muni_nonres_eit),
    summary_row("SD rate $\\tau^{SD}$ (\\%)",       block_panel$sd_eit),
    summary_row("Home count (workers living)",   block_panel$home_count),
    summary_row("Work count (jobs in block)",    block_panel$work_count),
    summary_row("Share live-and-work in muni",   block_panel$home_in_muni_share),
    summary_row("Share work outside muni",       block_panel$share_work_outside))
  blocks_per_muni <- block_panel[, .(n_blocks = uniqueN(block_geoid)), by = muni][, n_blocks]
  write_summary_table(c(block_rows, list(summary_row("Blocks per muni", as.numeric(blocks_per_muni)))),
    "Block-year summary statistics (2002--2019)",
    "tab:summary_block", "summary_block.tex",
    notes = c("Block-year level; one observation per (census block, year).",
              sprintf("Panel covers %s blocks in %s municipalities.",
                      format(uniqueN(block_panel$block_geoid), big.mark = ","),
                      format(uniqueN(block_panel$muni), big.mark = ","))))

  # ---- 3. school-district-year summary (not in the paper)
  sd_rates <- rate_panel %>%
    filter(!is.na(sd_eit)) %>%
    group_by(municipality_id, year) %>%
    summarise(sd_rate = mean(sd_eit, na.rm = TRUE), res = mean(resident_eit, na.rm = TRUE),
              nr = mean(muni_nonres_eit, na.rm = TRUE), .groups = "drop")
  sd_rows <- list(
    summary_row("SD rate $\\tau^{SD}$ (\\%)", sd_rates$sd_rate),
    summary_row("Resident rate $\\tau^{M}$ (\\%) (muni-year)", sd_rates$res),
    summary_row("Nonres rate $\\tau^{nr,M}$ (\\%) (muni-year)", sd_rates$nr))
  for (share_col in c("share_state_rev_sd", "share_local_rev_sd", "share_federal_rev_sd",
                      "share_property_tax_sd", "share_instructional_sd", "share_support_sd")) {
    if (share_col %in% names(sd_controls))
      sd_rows[[length(sd_rows) + 1L]] <- summary_row(gsub("_", "\\\\_", share_col), sd_controls[[share_col]])
  }
  write_summary_table(sd_rows,
    "School-district-year summary statistics (2002--2019)",
    "tab:summary_sd", "summary_sd.tex",
    notes = c("SD-year level. F-33 share controls are constructed from Census F-33 revenue/expenditure totals."))

  # ---- 4. cell counts on the same estimation sample, so they reconcile with
  # the regression Ns. Mean dose at +5 is the eventual increase used to turn
  # per-pp coefficients into effects; plain mean dose includes zero pre-event years.
  cell_order <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind")
  cell_counts <- estimation %>%
    filter(as.character(cell_muni) %in% cell_order) %>%
    group_by(cell_muni = factor(as.character(cell_muni), levels = cell_order)) %>%
    summarise(n_muni_year = n(), n_munis = n_distinct(muni), .groups = "drop")
  treated_k5 <- env$stk_muni %>% filter(rel == 5, as.character(treat_cell) != "NC")
  dose_k5 <- treated_k5 %>%
    group_by(cell_muni = factor(as.character(treat_cell), levels = cell_order)) %>%
    summarise(dose_k5 = mean(dose_change, na.rm = TRUE), .groups = "drop")
  dose_k5_pooled <- mean(treated_k5$dose_change, na.rm = TRUE)
  dose_range <- estimation %>%
    filter(!is.na(cell_muni), cell_muni != "NC") %>%
    group_by(cell_muni = factor(as.character(cell_muni), levels = cell_order)) %>%
    summarise(mean_dose_pp = mean(dose_muni, na.rm = TRUE),
              min_dose_pp  = min(dose_muni,  na.rm = TRUE),
              max_dose_pp  = max(dose_muni,  na.rm = TRUE), .groups = "drop")
  cell_table <- cell_counts %>%
    left_join(dose_range, by = "cell_muni") %>%
    left_join(dose_k5, by = "cell_muni") %>%
    mutate(across(c(dose_k5, mean_dose_pp, min_dose_pp, max_dose_pp), ~ coalesce(.x, 0)),
           extreme_dose_pp = if_else(grepl("^Dec", as.character(cell_muni)), min_dose_pp, max_dose_pp)) %>%
    arrange(cell_muni)
  treated_cells <- filter(cell_table, cell_muni != "NC")

  body <- c(
    "\\begin{table}[H]", "  \\centering",
    "  \\caption{Cell counts: muni resident-rate side, 2002--2019}",
    "  \\label{tab:cell_counts_summary}",
    "  \\small",
    "  \\begin{tabular}{lrrrrr}",
    "    \\toprule",
    "    Cell & Munis & Muni-years & Mean dose (pp) & Mean dose at $+5$ (pp) & Extreme dose (pp) \\\\",
    "    \\midrule")
  for (i in seq_len(nrow(cell_table))) {
    row <- cell_table[i, ]
    cell_label <- if (as.character(row$cell_muni) == "NC") "Never-changers" else as.character(row$cell_muni)
    body <- c(body, sprintf("    %s & %s & %s & %.2f & %.2f & %.2f \\\\",
                            cell_label, format(row$n_munis, big.mark = ","),
                            format(row$n_muni_year, big.mark = ","),
                            row$mean_dose_pp, row$dose_k5, row$extreme_dose_pp))
  }
  body <- c(body,
            sprintf("    \\midrule\n    Pooled Increase & %s & %s & %.2f & %.2f & %.2f \\\\",
                    format(sum(treated_cells$n_munis), big.mark = ","),
                    format(sum(treated_cells$n_muni_year), big.mark = ","),
                    weighted.mean(treated_cells$mean_dose_pp, treated_cells$n_muni_year),
                    dose_k5_pooled, max(treated_cells$extreme_dose_pp)),
            "    \\bottomrule", "  \\end{tabular}",
            "  \\vspace{0.3em}",
            "  \\begin{minipage}{0.95\\textwidth}",
            "    \\footnotesize \\textit{Notes:} Estimation sample, as in Table~\\ref{tab:summary_muni}. Municipalities are classified on their first resident-rate change. Mean dose averages the cumulative rate change over every muni-year in the cell, including pre-event years where it is zero. Mean dose at $+5$ is the eventual cumulative increase, five years after the event, and is the figure used to convert coefficients into effects in Section~\\ref{sec:results}.",
            "  \\end{minipage}", "\\end{table}")
  writeLines(body, file.path(TABLE_DIR, "cell_counts_summary.tex"))
  fwrite(cell_table, file.path(RESULTS_DIR, "cell_counts_summary.csv"))

  # ---- 5. figure: rate ECDFs (resident, nonresident, home stack), 0-5 pp
  rate_type_levels <- c("resident_eit", "muni_nonres_eit", "home_stack")
  rate_long <- rate_panel %>%
    transmute(resident_eit, muni_nonres_eit, home_stack = resident_eit + sd_eit) %>%
    tidyr::pivot_longer(everything(), names_to = "rate_type", values_to = "rate") %>%
    mutate(rate_type = factor(rate_type, levels = rate_type_levels)) %>%
    arrange(rate_type) %>%
    filter(is.finite(rate), rate >= 0, rate <= 5)
  ecdf_plot <- ggplot(rate_long, aes(x = rate, color = rate_type)) +
    stat_ecdf(geom = "step", linewidth = 1.0) +
    scale_color_manual(values = c(PAPER_RED, PAPER_BLUE, "#7B2CBF"), name = NULL,
                       labels = c(expression("Resident rate "*tau^M),
                                  expression("Nonresident rate "*tau^{nr*","*M}),
                                  expression("Home stack "*tau^M+tau^{SD}))) +
    scale_x_continuous(breaks = seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::label_percent()) +
    geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey60", linewidth = 0.3) +
    geom_vline(xintercept = 1.0, linetype = "dotted", color = "grey60", linewidth = 0.3) +
    annotate("text", x = 0.55, y = 0.05, label = "0.5% cap", hjust = 0, size = 3, color = "grey40") +
    annotate("text", x = 1.05, y = 0.05, label = "1% combined cap", hjust = 0, size = 3, color = "grey40") +
    labs(x = "Rate (percentage points)", y = "Share of municipality-years at or below rate") +
    theme_paper() +
    theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "rate_double_histogram.png"), ecdf_plot, width = 9, height = 5, dpi = 200)

  # ---- 6. figure: share of munis reporting a (non-imputed) nonresident rate, by year
  coverage <- rate_panel %>%
    group_by(year) %>%
    summarise(coverage = mean(!is.na(muni_nonres_eit) & !nonres_imputed, na.rm = TRUE), .groups = "drop") %>%
    filter(year >= 2002, year <= 2019)
  coverage_plot <- ggplot(coverage, aes(x = year, y = coverage)) +
    geom_line(color = PAPER_RED, linewidth = 1.1) +
    geom_point(color = PAPER_RED, size = 2) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "grey40") +
    annotate("text", x = 2012.2, y = 0.05, label = "Act 32 in effect", hjust = 0, size = 3.5, color = "grey25") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_x_continuous(breaks = seq(2002, 2019, by = 2)) +
    labs(x = "Year", y = "Share of PA munis reporting a nonresident rate") +
    theme_paper()
  ggsave(file.path(OUT_DIR, "act32_cdf.png"), coverage_plot, width = 8, height = 4.5, dpi = 200)

  # ---- 7. figure: resident-rate increases by year
  increases <- rate_panel %>%
    filter(year >= 2002, year <= 2019, !is.na(rate_change)) %>%
    group_by(year) %>%
    summarise(n = sum(rate_change > 1e-6), .groups = "drop") %>%
    mutate(direction = factor("Resident-rate increase"))
  increases_plot <- ggplot(increases, aes(x = year, y = n, fill = direction)) +
    geom_col(width = 0.75, alpha = 0.9) +
    geom_hline(yintercept = 0, color = "grey20", linewidth = 0.4) +
    geom_vline(xintercept = 2012, linetype = "dashed", color = "grey40") +
    annotate("text", x = 2012.2, y = max(increases$n) * 0.9, label = "Act 32", hjust = 0, size = 3.5, color = "grey25") +
    scale_fill_manual(values = c("Resident-rate increase" = PAPER_RED), name = NULL) +
    scale_x_continuous(breaks = seq(2002, 2019, by = 2)) +
    labs(x = "Year", y = "Number of municipalities (resident-rate increase)") +
    theme_paper() +
    theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "rate_changes_by_year.png"), increases_plot, width = 8.5, height = 5, dpi = 200)

  # ---- 8. figure: rate paths of the top 20 munis by cumulative |change| and by number of changes
  change_totals <- rate_panel[year >= 2002 & year <= 2019,
     .(cum_abs_dtau = sum(abs(rate_change), na.rm = TRUE),
       n_events     = sum(abs(rate_change) > 1e-6, na.rm = TRUE)),
     by = municipality_id]                                   # data.table keeps the order the ties break on
  top_by_change <- change_totals[order(-cum_abs_dtau)][1:20, municipality_id]
  top_by_count  <- change_totals[order(-n_events)][1:20, municipality_id]
  rate_paths <- function(ids) {
    rate_panel %>%
      filter(year >= 2002, year <= 2019, municipality_id %in% ids) %>%
      transmute(municipality_id, municipality, year, resident_eit,
                muni_lbl = tools::toTitleCase(tolower(municipality)))
  }
  path_plot <- function(paths, title) {
    ggplot(paths, aes(x = year, y = resident_eit, group = municipality_id)) +
      geom_step(color = PAPER_RED, linewidth = 0.7) +
      facet_wrap(~ muni_lbl, ncol = 4, scales = "free_y") +
      scale_x_continuous(breaks = c(2005, 2010, 2015)) +
      labs(x = "Year", y = expression("Resident rate "*tau^M*" (pp)"), title = title) +
      theme_paper() +
      theme(strip.text = element_text(size = 7), axis.text = element_text(size = 7),
            panel.spacing = unit(0.4, "lines"), plot.title = element_text(size = 11, face = "bold"))
  }
  ggsave(file.path(OUT_DIR, "pa_rate_trajectories.png"),
         path_plot(rate_paths(top_by_change), "Top 20 by cumulative |Δτ|") |
           path_plot(rate_paths(top_by_count), "Top 20 by number of rate-change events"),
         width = 16, height = 9, dpi = 180)

  # ---- 9. figure: PA map of munis by direction of resident-rate change
  cousubs <- st_as_sf(readRDS(file.path(DERIVED_DIR, "pa_cousub.rds")))
  change_types <- rate_panel[year >= 2002 & year <= 2019 & !is.na(rate_change),
                             .(n_inc = sum(rate_change >  1e-6), n_dec = sum(rate_change < -1e-6)),
                             by = .(cousub_fips, municipality)]
  change_types[, change_type := fcase(n_inc > 0 & n_dec > 0, "Both",
                                      n_inc > 0, "Increased",
                                      n_dec > 0, "Decreased",
                                      default = "No change")]
  mean_jobs <- work_panel[, .(total_jobs = mean(total_jobs, na.rm = TRUE)),
                          by = .(muni = as.character(work_city))]
  map_data <- cousubs %>%
    left_join(change_types, by = c("COUSUBFP" = "cousub_fips")) %>%
    left_join(mean_jobs, by = c("COUSUBFP" = "muni")) %>%
    mutate(change_type = ifelse(is.na(change_type), "No change", change_type))
  state_outline <- st_union(map_data)
  map_data$change_type <- factor(map_data$change_type, levels = c("Increased", "Decreased", "Both", "No change"))
  map_plot <- ggplot(map_data) +
    geom_sf(aes(fill = change_type), color = "black", linewidth = 0.08) +
    geom_sf(data = state_outline, fill = NA, color = "black", linewidth = 0.5) +
    scale_fill_manual(values = c("Increased" = PAPER_RED, "Decreased" = PAPER_BLUE,
                                 "Both" = "#7B2CBF", "No change" = "grey92"),
                      breaks = c("Increased", "Decreased", "Both", "No change"),
                      name = NULL, drop = FALSE) +
    coord_sf() +
    labs(title = NULL) +
    theme_paper() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank(), legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "pa_map.png"), map_plot, width = 11, height = 7, dpi = 220)

  # ---- 10. figure: mean home stack vs mean work (nonresident) rate by year
  # Muni-years that report a non-imputed work rate (reporting jumps at Act 32 in
  # 2012), collapsed as in build_headline_env (resident mean, SD max, nonresident
  # mean). The work rate binds only when strictly above the home stack.
  binding <- rate_panel[!is.na(cousub_fips) & year >= 2002,
                        .(resident_eit    = mean(resident_eit,    na.rm = TRUE),
                          sd_eit          = max(sd_eit,            na.rm = TRUE),
                          muni_nonres_eit = mean(muni_nonres_eit,  na.rm = TRUE),
                          nonres_imputed  = as.logical(max(nonres_imputed, na.rm = TRUE))),
                        by = .(muni = cousub_fips, year)]
  binding <- binding %>%
    mutate(across(c(resident_eit, sd_eit, muni_nonres_eit), ~ if_else(is.finite(.x), .x, NA_real_)),
           home_stack = resident_eit + sd_eit,
           work_rate  = muni_nonres_eit) %>%
    filter(!is.na(home_stack), !is.na(work_rate), nonres_imputed == FALSE) %>%
    mutate(work_binds = work_rate > home_stack + 1e-6)
  binding_by_year <- binding %>%
    group_by(year) %>%
    summarise(n = n(), mean_home = mean(home_stack), mean_work = mean(work_rate),
              median_home = median(home_stack), median_work = median(work_rate),
              share_work = mean(work_binds), .groups = "drop")
  print(binding_by_year)
  print(sprintf("Overall: work binds in %.2f%% of %s reporting muni-years (coverage rises at the 2012 Act 32 break).",
                100 * mean(binding$work_binds), format(nrow(binding), big.mark = ",")))

  home_label <- "Home Stack (resident + SD)"
  work_label <- "Work Rate (nonresident)"
  fills <- setNames(c("#9AB3C0", "#E59EA7"), c(home_label, work_label))
  lines <- setNames(c("#1F5673", "#C5283D"), c(home_label, work_label))
  levels_plot <- ggplot(binding_by_year, aes(x = year)) +
    geom_col(aes(y = mean_home, fill = home_label, color = home_label), width = 0.82, alpha = 0.72, linewidth = 0.6) +
    geom_col(aes(y = mean_work, fill = work_label, color = work_label), width = 0.82, alpha = 0.72, linewidth = 0.6) +
    scale_fill_manual(values = fills, name = NULL, breaks = c(home_label, work_label)) +
    scale_color_manual(values = lines, name = NULL, breaks = c(home_label, work_label)) +
    scale_x_continuous(breaks = 2002:2019) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
    labs(x = "Year", y = "Mean EIT rate (pp)") +
    theme_minimal(base_size = 14) +
    theme(
      plot.background       = element_rect(fill = "white", color = NA),
      panel.background      = element_rect(fill = "white", color = NA),
      panel.grid.major.y    = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.minor      = element_blank(),
      panel.grid.major.x    = element_blank(),
      axis.line             = element_line(color = "grey20", linewidth = 0.4),
      axis.ticks            = element_line(color = "grey20", linewidth = 0.4),
      axis.text             = element_text(color = "grey20"),
      axis.text.x           = element_text(color = "grey20", angle = 45, hjust = 1),
      axis.title            = element_text(color = "grey20"),
      legend.text           = element_text(color = "grey20", size = 14),
      legend.position       = "bottom",
      legend.key            = element_rect(fill = "white", color = NA),
      legend.key.width      = unit(2.2, "lines"),
      legend.background     = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(color = "grey20", fill = NA),
      legend.box.margin     = margin(4, 8, 4, 8))
  ggsave(file.path(OUT_DIR, "binding_levels_by_year.png"), levels_plot, width = 10, height = 6, dpi = 300)
})

# Dose trajectories (build_uea_aggregate_figures.R Fig 2 + build_dose_trajectory_inc_becomes.R) ----
# Cumulative dose by event time for the Inc-bind and Inc-becomes cells, muni
# cohort vs SD cohort. The mean at k = +5 is the typical-event dose used to
# translate per-pp coefficients in the prose (0.24 Inc-bind, 0.38 Inc-becomes).
# Outputs: figures/support_dose_trajectory_muni_vs_sd.png (Inc-bind),
#   figures/support_dose_trajectory_inc_becomes.png
if (RUN_DOSE_TRAJECTORY) local({
  fig_dir <- OUT_DIR

  PAPER_RED  <- "#C5283D"; PAPER_BLUE <- "#1F5673"

  theme_paper <- function(bs = 11) theme_bw(base_size = bs) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank())

  panel <- as_tibble(env$panel)

  # File names kept from the two scripts these came from so the paths don't move.
  out_file <- c("Inc-bind"    = "support_dose_trajectory_muni_vs_sd.png",
                "Inc-becomes" = "support_dose_trajectory_inc_becomes.png")

  # Mean and IQR of the cumulative dose by event time, for one rate setter.
  traj_summary <- function(cl, cell_col, c_col, dose_col, side_label) {
    panel %>%
      filter(.data[[cell_col]] == cl,
             !is.na(.data[[c_col]]),
             year >= .data[[c_col]] - 5,
             year <= .data[[c_col]] + 5) %>%
      mutate(rel = year - .data[[c_col]]) %>%
      group_by(rel) %>%
      summarise(mean_dose = mean(.data[[dose_col]], na.rm = TRUE),
                q25       = quantile(.data[[dose_col]], 0.25, na.rm = TRUE),
                q75       = quantile(.data[[dose_col]], 0.75, na.rm = TRUE),
                .groups   = "drop") %>%
      mutate(side = side_label)
  }

  for (cl in names(out_file)) {
    muni_summary <- traj_summary(cl, "cell_muni", "c_muni", "dose_muni", "Municipality cohort")
    sd_summary   <- traj_summary(cl, "cell_sd",   "c_sd",   "dose_sd",   "School-district cohort")
    combined     <- bind_rows(muni_summary, sd_summary)
    message(sprintf("[trajectory] %s muni mean dose at k=+5: %.3f",
                    cl, muni_summary$mean_dose[muni_summary$rel == 5]))

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
      labs(title = sprintf("Cumulative %s dose by event-time", cl),
            subtitle = "Mean and inter-quartile range relative to pre-event baseline",
            x = "Event time k (years relative to first rate event)",
            y = expression("Cumulative "*Delta*tau*" (pp)")) +
      theme_paper() + theme(legend.position = "bottom")

    ggsave(file.path(fig_dir, out_file[[cl]]), g, width = 9, height = 4.5, dpi = 300)
  }
})

# Spline dose-response ----
# ATT(d) and ACRT(d) for the muni dose from a natural spline, for the pooled
# Increase group, Inc-bind and Inc-becomes, each on {its treated cells + NC}
# with the SD co-treatment term as in the headline. Paper figures: att_linear_*
# (body) and att_flex_* (Appendix A). Also writes acr_acrt_att_spline_*,
# spline_knot_placement and the spline_*.csv files the explorer reads.
# The dose is dose_change, not the level dose_stack: as an x-axis the level
# would put NC at 0 against treated munis at 0.5-2.5 and invent a dose-response.
if (RUN_SPLINE_DOSE_RESPONSE) local({
  ctrls <- env$ctrls
  ctrls_str <- paste(ctrls, collapse = " + ")
  inc_cells <- c("Inc-bind", "Inc-becomes", "Inc-nobind")
  spline_stack <- env$stk_muni %>%
    mutate(is_increase = as.integer(as.character(treat_cell) %in% inc_cells),
           dose = if_else(is_increase == 1, dose_change, 0)) %>%
    as.data.table()
  outcome_labels <- c(log_live_and_work = "Work in own muni", share_live_in = "Share live in muni",
                      log_total_jobs = "Total jobs", log_in_commuters = "In-commuters")
  outcomes <- c("log_live_and_work", "share_live_in", "log_total_jobs", "log_in_commuters")
  # pooled 95th-percentile dose; only the (non-paper) plot_spline() figures use it
  pooled_dose_p95 <- quantile(spline_stack$dose[spline_stack$is_increase == 1 & spline_stack$dose > 0],
                              0.95, na.rm = TRUE)
  sd_term <- "i(cell_sd, dose_sd, ref='NC')"

  # The labeled linear ACR is the paper's own estimate, read from the result
  # files, so the figure label matches the table to the last digit.
  pooled_results   <- tryCatch(fread(file.path(RESULTS_DIR, "pooled_results.csv")), error = function(e) NULL)
  headline_results <- tryCatch(fread(file.path(RESULTS_DIR, "headline_full_table.csv")), error = function(e) NULL)
  headline_acr <- function(treated_cells, outcome_name, column = "est") {
    if (length(treated_cells) > 1L) {
      if (is.null(pooled_results)) return(NA_real_)
      values <- pooled_results[[column]][grepl("direction_muni::Inc", pooled_results$term) &
                                           pooled_results$outcome == outcome_name]
    } else {
      if (is.null(headline_results)) return(NA_real_)
      values <- headline_results[[column]][headline_results$term == sprintf("treat_cell::%s:dose_stack", treated_cells) &
                                             headline_results$outcome == outcome_name]
    }
    if (length(values)) as.numeric(values[1]) else NA_real_
  }

  # Fit one group. A continuous-dose spline on the subset, not a cell-interacted
  # one: i(treat_cell, spline) absorbed the level and pinned the Inc-bind ATT
  # near zero. Knots at the group's dose terciles, boundary [0, max dose]. The
  # curve is drawn only to the group's own 95th-percentile dose.
  fit_group <- function(treated_cells) {
    att_rows <- list(); acrt_rows <- list(); basis_rows <- list(); quad_rows <- list(); point_rows <- list()
    group_cells <- c(treated_cells, "NC")
    group_dose_p95 <- quantile(spline_stack$dose[as.character(spline_stack$treat_cell) %in% treated_cells &
                                                   spline_stack$dose > 0], 0.95, na.rm = TRUE)
    dose_grid <- seq(0, group_dose_p95, length.out = 80)
    for (outcome in outcomes) {
      model_data <- spline_stack[as.character(treat_cell) %in% group_cells,
                                 c("muni", "dose", "cell_sd", "dose_sd", "treat_cell",
                                   "stack_muni", "stack_year", outcome, ctrls), with = FALSE]
      model_data <- model_data[complete.cases(model_data)]
      interior_knots <- quantile(model_data$dose[model_data$dose > 0], c(1/3, 2/3), na.rm = TRUE)
      boundary_knots <- c(0, max(model_data$dose[model_data$dose > 0], na.rm = TRUE))
      spline_basis <- ns(model_data$dose, knots = interior_knots, Boundary.knots = boundary_knots)
      basis_cols <- paste0("sp", seq_len(ncol(spline_basis)))
      model_data[, (basis_cols) := as.data.table(unclass(spline_basis))]
      model_data[, dose2 := dose^2]
      rhs_rest <- paste0(" + ", sd_term, " + ", ctrls_str, " | stack_muni + stack_year")
      spline_model <- feols(as.formula(paste0(outcome, " ~ ", paste(basis_cols, collapse = " + "), rhs_rest)),
                            model_data, cluster = ~muni)
      linear_model <- feols(as.formula(paste0(outcome, " ~ dose", rhs_rest)), model_data, cluster = ~muni)
      quad_model   <- feols(as.formula(paste0(outcome, " ~ dose + dose2", rhs_rest)), model_data, cluster = ~muni)

      # One point per treated muni: its pre/post change in the outcome minus the
      # same change for the NC munis in its stack, at the dose it took. A raw
      # per-unit DiD with no controls, to show the data behind the curve.
      outcome_stack <- spline_stack[as.character(treat_cell) %in% group_cells & !is.na(get(outcome)) & !is.na(rel)]
      control_changes <- outcome_stack[treat_cell == "NC",
                                       .(nc_d = mean(get(outcome)[rel >= 0], na.rm = TRUE) -
                                                mean(get(outcome)[rel <  0], na.rm = TRUE)), by = stack]
      treated_changes <- outcome_stack[as.character(treat_cell) %in% treated_cells,
                                       .(t_d = mean(get(outcome)[rel >= 0], na.rm = TRUE) -
                                               mean(get(outcome)[rel <  0], na.rm = TRUE),
                                         dose = max(dose, na.rm = TRUE)), by = .(muni, stack)]
      unit_points <- merge(treated_changes, control_changes, by = "stack")[, y := t_d - nc_d]
      unit_points <- unit_points[is.finite(y) & is.finite(dose) & dose <= group_dose_p95]
      if (nrow(unit_points))
        point_rows[[outcome]] <- data.table(outcome = outcome_labels[outcome], dose = unit_points$dose, y = unit_points$y)

      quad_coefs <- coef(quad_model)[c("dose", "dose2")]
      quad_vcov  <- vcov(quad_model)[c("dose", "dose2"), c("dose", "dose2")]
      quad_p     <- quad_model$coeftable["dose2", "Pr(>|t|)"]
      spline_coefs <- coef(spline_model)[basis_cols]
      spline_vcov  <- vcov(spline_model)[basis_cols, basis_cols]

      # ACR from the flexible fits: ACRT(d) averaged over the doses treated munis took
      treated_doses <- unique(unit_points[, .(muni, dose)])[, .(dose = max(dose)), by = muni]$dose
      acr_quad <- NA_real_; acr_spline <- NA_real_
      if (length(treated_doses) > 1) {
        acr_quad <- as.numeric(c(1, 2 * mean(treated_doses)) %*% quad_coefs)
        step <- 1e-5
        upper <- pmin(treated_doses + step, boundary_knots[2]); lower <- pmax(treated_doses - step, 0)
        basis_slope <- (predict(spline_basis, upper) - predict(spline_basis, lower)) / (upper - lower)
        acr_spline <- as.numeric(colMeans(basis_slope) %*% spline_coefs)
      }
      acr    <- headline_acr(treated_cells, outcome)
      acr_se <- headline_acr(treated_cells, outcome, "se")
      if (is.na(acr)) { acr <- as.numeric(coef(linear_model)["dose"]); acr_se <- as.numeric(se(linear_model)["dose"]) }

      # ATT(d) = B(d)'b - B(0)'b, delta-method SEs from the basis difference
      basis_diff <- sweep(predict(spline_basis, dose_grid), 2, as.numeric(predict(spline_basis, 0)))
      att_spline <- as.numeric(basis_diff %*% spline_coefs)
      att_se     <- sqrt(pmax(rowSums((basis_diff %*% spline_vcov) * basis_diff), 0))
      # ACRT(d) = dATT/dd by central difference of the basis
      h <- 1e-4
      grid_slope <- (predict(spline_basis, dose_grid + h) - predict(spline_basis, dose_grid - h)) / (2 * h)
      acrt_spline <- as.numeric(grid_slope %*% spline_coefs)
      acrt_se     <- sqrt(pmax(rowSums((grid_slope %*% spline_vcov) * grid_slope), 0))
      label <- outcome_labels[outcome]
      att_rows[[outcome]]  <- data.table(outcome = label, dose = dose_grid, y = att_spline,
                                         lo = att_spline - 1.96 * att_se, hi = att_spline + 1.96 * att_se)
      acrt_rows[[outcome]] <- data.table(outcome = label, dose = dose_grid, y = acrt_spline,
                                         lo = acrt_spline - 1.96 * acrt_se, hi = acrt_spline + 1.96 * acrt_se,
                                         acr = acr, acr_se = acr_se)
      basis_rows[[outcome]] <- data.table(outcome = label, knot1 = interior_knots[1], knot2 = interior_knots[2],
                                          bk_lo = boundary_knots[1], bk_hi = boundary_knots[2],
                                          b1 = spline_coefs[1], b2 = spline_coefs[2], b3 = spline_coefs[3], acr = acr,
                                          acr_quad = acr_quad, acr_spline = acr_spline,
                                          mean_dose = mean(treated_doses), n_treated = length(treated_doses),
                                          q1 = quad_coefs[1], q2 = quad_coefs[2], q_dose2_p = quad_p)
      # quadratic: ATT(d) = b1 d + b2 d^2, ACRT(d) = b1 + 2 b2 d, delta-method SEs
      att_quad   <- quad_coefs[1] * dose_grid + quad_coefs[2] * dose_grid^2
      att_quad_v <- dose_grid^2 * quad_vcov[1, 1] + dose_grid^4 * quad_vcov[2, 2] + 2 * dose_grid^3 * quad_vcov[1, 2]
      acrt_quad   <- quad_coefs[1] + 2 * quad_coefs[2] * dose_grid
      acrt_quad_v <- quad_vcov[1, 1] + 4 * dose_grid^2 * quad_vcov[2, 2] + 4 * dose_grid * quad_vcov[1, 2]
      quad_rows[[outcome]] <- rbind(
        data.table(outcome = label, dose = dose_grid, curve = "ATT_quad", y = att_quad,
                   lo = att_quad - 1.96 * sqrt(pmax(att_quad_v, 0)), hi = att_quad + 1.96 * sqrt(pmax(att_quad_v, 0)),
                   acr = NA_real_),
        data.table(outcome = label, dose = dose_grid, curve = "ACRT_quad", y = acrt_quad,
                   lo = acrt_quad - 1.96 * sqrt(pmax(acrt_quad_v, 0)), hi = acrt_quad + 1.96 * sqrt(pmax(acrt_quad_v, 0)),
                   acr = NA_real_))
    }
    list(ATT = rbindlist(att_rows), ACRT = rbindlist(acrt_rows),
         BASIS = rbindlist(basis_rows), QUAD = rbindlist(quad_rows), PTS = rbindlist(point_rows))
  }

  # facet keys "<outcome> — <panel>" in a fixed order
  facet_order <- function(panels) as.vector(t(outer(outcome_labels[outcomes], panels, paste, sep = " — ")))
  add_facet <- function(data, panels) {
    mutate(data, facet = factor(paste(outcome, panel, sep = " — "), levels = facet_order(panels)))
  }

  # Body figure: the linear model only. ATT(d) = ACR x d with a 95% band; no
  # ACRT column, since under linearity it is the constant ACR.
  plot_linear <- function(fit, title, outfile) {
    att_panel <- "ATT(d): total effect"
    acr_by_outcome <- distinct(fit$ACRT, outcome, acr, acr_se) %>%
      inner_join(summarise(group_by(fit$ATT, outcome), dmax = max(dose)), by = "outcome")
    ray <- acr_by_outcome %>%
      group_by(outcome, acr, acr_se) %>%
      reframe(dose = seq(0, dmax, length.out = 60)) %>%
      mutate(y = acr * dose, lo = (acr - 1.96 * acr_se) * dose, hi = (acr + 1.96 * acr_se) * dose,
             panel = att_panel) %>%
      add_facet(att_panel)
    labels <- acr_by_outcome %>%
      mutate(label = sprintf("ACR = %.3f (%.3f)", acr, acr_se), panel = att_panel) %>%
      add_facet(att_panel)
    plot <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
      geom_ribbon(data = ray, aes(dose, ymin = lo, ymax = hi), fill = "#2166ac", alpha = 0.13, color = NA) +
      geom_line(data = ray, aes(dose, y), color = "#2166ac", linewidth = 0.7) +
      geom_text(data = labels, aes(x = dmax * 0.45, y = acr * dmax, label = label),
                color = "#2166ac", vjust = -0.5, size = 2.6) +
      facet_wrap(~ facet, scales = "free_y", ncol = 1) +
      labs(title = title,
           subtitle = paste0("Linear specification, net of SD rate changes.\n",
                             "ATT(d) = ACR x d with a 95% band. No ACRT panel, since under the\n",
                             "linear model the marginal effect is constant and equal to the ACR."),
           x = "dose (cumulative muni rate change, pp)", y = NULL) +
      theme_bw(base_size = 9) + theme(plot.subtitle = element_text(size = 8))
    ggsave(file.path(OUT_DIR, outfile), plot, width = 7.5, height = 8.5, dpi = 300)
  }

  # Appendix figure: quadratic and spline, with the linear model as a dashed reference
  plot_flex <- function(fit, title, outfile) {
    att_panel <- "ATT(d): total effect"; acrt_panel <- "ACRT(d): marginal effect"
    acr_by_outcome <- distinct(fit$ACRT, outcome, acr)
    curves <- bind_rows(
      fit$ATT  %>% transmute(outcome, dose, y, lo, hi, model = "Spline", panel = att_panel),
      fit$ACRT %>% transmute(outcome, dose, y, lo, hi, model = "Spline", panel = acrt_panel),
      fit$QUAD %>% filter(curve == "ATT_quad")  %>% transmute(outcome, dose, y, lo, hi, model = "Quadratic", panel = att_panel),
      fit$QUAD %>% filter(curve == "ACRT_quad") %>% transmute(outcome, dose, y, lo, hi, model = "Quadratic", panel = acrt_panel)) %>%
      add_facet(c(att_panel, acrt_panel))
    linear <- bind_rows(
      summarise(group_by(fit$ATT, outcome), dmax = max(dose)) %>%
        inner_join(acr_by_outcome, by = "outcome") %>%
        group_by(outcome) %>%
        reframe(dose = c(0, dmax), y = c(0, acr * dmax), panel = att_panel),
      summarise(group_by(fit$ACRT, outcome), dmax = max(dose)) %>%
        inner_join(acr_by_outcome, by = "outcome") %>%
        group_by(outcome) %>%
        reframe(dose = c(0, dmax), y = c(acr, acr), panel = acrt_panel)) %>%
      add_facet(c(att_panel, acrt_panel))
    ggplot() +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.7) +
      geom_ribbon(data = curves, aes(dose, ymin = lo, ymax = hi, fill = model), alpha = 0.12, colour = NA) +
      geom_line(data = linear, aes(dose, y), colour = "#2166ac", linetype = "longdash", linewidth = 0.6) +
      geom_line(data = curves, aes(dose, y, colour = model), linewidth = 0.9) +
      facet_wrap(~ facet, scales = "free_y", ncol = 2) +
      scale_colour_manual(values = c(Quadratic = "#7b3294", Spline = "#1b7837"), name = NULL) +
      scale_fill_manual(values   = c(Quadratic = "#7b3294", Spline = "#1b7837"), guide = "none") +
      labs(title = title,
           subtitle = paste("Quadratic and natural-spline dose-response, 95% bands.",
                            "Blue dashed: the linear model of the body, ACR x d on the left and a constant ACR on the right.",
                            "\nSame sample, controls, fixed effects and clustering as the headline."),
           x = "dose (cumulative muni rate change, pp)", y = NULL) +
      theme_bw(base_size = 9) +
      theme(legend.position = "bottom", plot.subtitle = element_text(size = 8))
    ggsave(file.path(OUT_DIR, outfile), width = 10, height = 11, dpi = 300)
  }

  # Spline-only figure (not in the paper). Its linear reference runs to the
  # POOLED 95th-percentile dose for every group, not the group's own range.
  plot_spline <- function(fit, title, outfile) {
    att_panel <- "ATT(d): total effect"; acrt_panel <- "ACRT(d): marginal effect + ACR"
    curves <- bind_rows(
      fit$ATT  %>% transmute(outcome, dose, y, lo, hi, panel = att_panel),
      fit$ACRT %>% transmute(outcome, dose, y, lo, hi, panel = acrt_panel)) %>%
      add_facet(c(att_panel, acrt_panel))
    acr_lines <- distinct(fit$ACRT, outcome, acr) %>%
      mutate(panel = acrt_panel, label = sprintf("ACR = %.3f", acr)) %>%
      add_facet(c(att_panel, acrt_panel))
    rays <- distinct(fit$ACRT, outcome, acr) %>%
      group_by(outcome, acr) %>%
      reframe(dose = c(0, pooled_dose_p95), y = c(0, acr * pooled_dose_p95)) %>%
      mutate(panel = att_panel) %>%
      add_facet(c(att_panel, acrt_panel))
    plot <- ggplot(curves, aes(dose, y)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#1b7837", alpha = 0.13, color = NA) +
      geom_line(color = "#1b7837", linewidth = 0.7) +
      geom_line(data = rays, aes(dose, y, group = outcome), color = "#2166ac",
                linetype = "longdash", linewidth = 0.5, inherit.aes = FALSE) +
      geom_hline(data = acr_lines, aes(yintercept = acr), color = "#2166ac", linetype = "longdash", linewidth = 0.5) +
      geom_text(data = acr_lines, aes(x = pooled_dose_p95 * 0.45, y = acr, label = label), color = "#2166ac",
                vjust = -0.5, size = 2.6, inherit.aes = FALSE) +
      facet_wrap(~ facet, scales = "free_y", ncol = 2) +
      labs(title = title,
           subtitle = "Natural-spline ns(dose), net of SD rate changes. Green = spline; blue dashed = the linear model the paper estimates (left: ACR x d; right: constant ACR).",
           x = "dose (cumulative muni rate change, pp)", y = NULL) +
      theme_bw(base_size = 9) + theme(plot.subtitle = element_text(size = 8))
    ggsave(file.path(OUT_DIR, outfile), plot, width = 10, height = 11, dpi = 300)
  }

  # Knot-placement figure (not in the paper): treated doses, interior knots
  # (solid) and boundary knots (dotted), computed as in fit_group() on the
  # log_live_and_work sample. The points are jittered without a seed.
  knot_groups <- list("Pooled Increase" = inc_cells, "Inc-bind" = "Inc-bind", "Inc-becomes" = "Inc-becomes")
  knot_points <- list(); knots <- list()
  for (group_name in names(knot_groups)) {
    knot_data <- spline_stack[as.character(treat_cell) %in% c(knot_groups[[group_name]], "NC"),
                              c("muni", "dose", "cell_sd", "dose_sd", "treat_cell",
                                "stack_muni", "stack_year", "log_live_and_work", ctrls), with = FALSE]
    knot_data <- knot_data[complete.cases(knot_data)]
    treated_dose <- knot_data$dose[knot_data$dose > 0]
    knot_points[[group_name]] <- data.table(grp = group_name, dose = treated_dose)
    knots[[group_name]] <- data.table(grp = group_name,
                                      knot = c(quantile(treated_dose, c(1/3, 2/3), na.rm = TRUE),
                                               0, max(treated_dose, na.rm = TRUE)),
                                      type = c("interior", "interior", "boundary", "boundary"))
  }
  knot_points <- rbindlist(knot_points)[, grp := factor(grp, levels = names(knot_groups))]
  knots <- rbindlist(knots)[, grp := factor(grp, levels = names(knot_groups))]
  knot_plot <- ggplot(knot_points, aes(x = dose, y = 0)) +
    geom_jitter(height = 0.32, alpha = 0.25, size = 1.1, color = "grey30") +
    geom_vline(data = knots[type == "interior"], aes(xintercept = knot), color = "#2166ac", linewidth = 0.6) +
    geom_vline(data = knots[type == "boundary"], aes(xintercept = knot), color = "grey55",
               linetype = "dotted", linewidth = 0.5) +
    geom_text(data = knots[type == "interior"][, label := sprintf("%.2f", knot)],
              aes(x = knot, y = 0.48, label = label), color = "#2166ac", size = 3, vjust = 0) +
    facet_wrap(~ grp, ncol = 1) +
    scale_y_continuous(limits = c(-0.55, 0.55), breaks = NULL) +
    labs(title = "Where the spline can bend: treated doses and knot placement",
         subtitle = "Points: treated muni-year doses (NC zeros excluded). Solid blue: interior knots (dose terciles, per group). Dotted: boundary knots (linear beyond).",
         x = "dose (cumulative muni rate change, pp)", y = NULL) +
    theme_bw(base_size = 10) +
    theme(plot.subtitle = element_text(size = 8))
  ggsave(file.path(OUT_DIR, "spline_knot_placement.png"), knot_plot, width = 9, height = 5.5, dpi = 300)

  fits <- list("Pooled Increase" = fit_group(inc_cells),
               "Inc-bind"        = fit_group("Inc-bind"),
               "Inc-becomes"     = fit_group("Inc-becomes"))
  file_tags <- c("Pooled Increase" = "pooled", "Inc-bind" = "incbind", "Inc-becomes" = "incbecomes")
  spline_titles <- c("Pooled Increase" = "POOLED Increase", "Inc-bind" = "Inc-bind cell", "Inc-becomes" = "Inc-becomes cell")
  for (group_name in names(fits)) {
    tag <- file_tags[[group_name]]
    plot_linear(fits[[group_name]], paste("Dose-response, linear specification —", group_name),
                sprintf("att_linear_%s.png", tag))
    plot_flex(fits[[group_name]], paste("Flexible dose-response —", group_name), sprintf("att_flex_%s.png", tag))
    plot_spline(fits[[group_name]], paste("Muni dose-response (spline) —", spline_titles[[group_name]]),
                sprintf("acr_acrt_att_spline_%s.png", tag))
  }

  # fitted curves, basis parameters and unit points for the spline explorer
  export_curves <- function(fit, group_name) rbind(
    fit$ATT[,  .(group = group_name, outcome, dose, curve = "ATT",  y, lo, hi, acr = NA_real_)],
    fit$ACRT[, .(group = group_name, outcome, dose, curve = "ACRT", y, lo, hi, acr)],
    fit$QUAD[, .(group = group_name, outcome, dose, curve, y, lo, hi, acr)])
  fwrite(rbindlist(lapply(names(fits), function(g) export_curves(fits[[g]], g))),
         file.path(RESULTS_DIR, "spline_curves.csv"))
  fwrite(rbindlist(lapply(names(fits), function(g) copy(fits[[g]]$BASIS)[, group := g])),
         file.path(RESULTS_DIR, "spline_basis.csv"))
  fwrite(rbindlist(lapply(names(fits), function(g) copy(fits[[g]]$PTS)[, group := g])),
         file.path(RESULTS_DIR, "spline_points.csv"))
  message("Saved data/results/spline_curves.csv + spline_basis.csv + spline_points.csv")
})

# feols ES by cell ----
# The realized-dose event study for the pooled Increase group, Inc-bind and
# Inc-becomes, drawn in the layout of dcdh_bycell_es.png. Coefficients are scaled
# to the effect at the group's mean realized dose (beta_k x mean dose), the scale
# dCDH reports, so the two figures can be read side by side. Like the pooled
# ES, the sample does not condition on dose_stack.
if (RUN_FEOLS_ES_BYCELL) local({
  K <- 5L
  ctrls <- env$ctrls
  ctrls_str <- paste(ctrls, collapse = " + ")
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind", "Dec-bind", "Dec-becomes-non", "Dec-nobind")
  es_panel <- add_realized_dose(copy(env$panel), K = K)

  treated_munis <- unique(es_panel[cell_muni != "NC", .(muni, cohort = c_muni)])
  nc_munis <- unique(es_panel[cell_muni == "NC", muni])
  es_stack <- rbindlist(lapply(sort(unique(treated_munis$cohort)), function(cohort_year) {
    window_years <- (cohort_year - K):(cohort_year + K)
    cohort_munis <- treated_munis[cohort == cohort_year]
    if (!nrow(cohort_munis)) return(NULL)
    treated_rows <- es_panel[muni %in% cohort_munis$muni & year %in% window_years]
    treated_rows[, treat_cell_muni := cell_muni]
    control_rows <- es_panel[muni %in% nc_munis & year %in% window_years]
    control_rows[, treat_cell_muni := "NC"]
    stack_rows <- rbind(treated_rows, control_rows, fill = TRUE)
    stack_rows[, stack := cohort_year]; stack_rows[, rel := as.integer(year - cohort_year)]
    stack_rows
  }), fill = TRUE)
  es_stack[, stack_muni := paste0(stack, "_", muni)]
  es_stack[, stack_year := paste0(stack, "_", year)]
  es_stack[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
  es_stack[, cell_sd := factor(cell_sd, levels = cell_levels)]
  es_stack[, cell_nr := factor(cell_nr, levels = cell_levels)]
  es_stack[is.na(dose_sd), dose_sd := 0]
  es_stack[is.na(dose_nr), dose_nr := 0]

  outcomes <- c("log_live_and_work", "log_in_commuters", "log_total_jobs", "share_live_in")
  groups <- list(pooled = c("Inc-bind", "Inc-becomes", "Inc-nobind"), incbind = "Inc-bind", incbecomes = "Inc-becomes")
  coef_rows <- list()
  for (group_name in names(groups)) {
    group_cells <- groups[[group_name]]
    group_dose <- es_stack$realized_dose_muni[es_stack$treat_cell_muni %in% group_cells &
                                                es_stack$realized_dose_muni > 0]
    mean_dose <- mean(group_dose, na.rm = TRUE)
    for (outcome in outcomes) {
      model_data <- es_stack %>%
        filter(treat_cell_muni %in% c("NC", group_cells)) %>%
        select(all_of(c("muni", "rel", "stack_muni", "stack_year", "treat_cell_muni",
                        "realized_dose_muni", "cell_sd", "dose_sd", outcome, ctrls))) %>%
        filter(complete.cases(.))
      model <- feols(as.formula(paste0(outcome, " ~ i(rel, realized_dose_muni, ref = -1) + i(cell_sd, dose_sd, ref = 'NC')",
                                       " + ", ctrls_str, " | stack_muni + stack_year")),
                     model_data, cluster = ~muni)
      coef_rows[[paste(group_name, outcome)]] <- as_tibble(model$coeftable, rownames = "term") %>%
        filter(grepl("^rel::", term)) %>%
        transmute(group = group_name, outcome = outcome,
                  rel = as.integer(sub("rel::(-?\\d+):.*", "\\1", term)),
                  beta = Estimate, se = `Std. Error`, mean_dose = mean_dose)
    }
  }
  coefs <- bind_rows(coef_rows)
  # rel = -1 is the reference period, drawn at 0
  event_study <- bind_rows(coefs, distinct(coefs, group, outcome, mean_dose) %>%
                             mutate(rel = -1L, beta = 0, se = 0, .before = mean_dose)) %>%
    mutate(est = beta * mean_dose, lo = (beta - 1.96 * se) * mean_dose, hi = (beta + 1.96 * se) * mean_dose) %>%
    filter(rel >= -5, rel <= 5)
  fwrite(event_study, file.path(RESULTS_DIR, "feols_es_bycell.csv"))

  outcome_labels <- c(log_live_and_work = "Log(work in own muni)", log_in_commuters = "Log(in-commuters)",
                      log_total_jobs = "Log(total jobs)", share_live_in = "Share live in muni")
  group_labels <- c(pooled = "Pooled Increase", incbind = "Inc-bind", incbecomes = "Inc-becomes")
  group_colors <- c("Pooled Increase" = "#4d4d4d", "Inc-bind" = "#b2182b", "Inc-becomes" = "#2166ac")
  plot_data <- mutate(event_study, outcome_label = outcome_labels[outcome],
                      group_label = factor(group_labels[group], levels = group_labels))
  plot <- ggplot(plot_data, aes(rel, est, color = group_label, fill = group_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
    geom_line() + geom_point(size = 1) +
    facet_wrap(~ outcome_label, scales = "free_y") +
    scale_color_manual(values = group_colors, name = NULL) +
    scale_fill_manual(values = group_colors, name = NULL) +
    labs(title = "feols realized-dose event study by group -- twin of the dCDH figure",
         subtitle = "beta_k x mean realized dose (level-at-mean-dose scale, comparable to dCDH); SD co-treatment controls",
         x = "periods since rate change", y = "effect at mean dose") +
    theme_bw(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(OUT_DIR, "feols_es_bycell.png"), plot, width = 9, height = 6, dpi = 300)
  event_study %>% filter(rel %in% c(-2, 3)) %>%
    transmute(group, outcome, rel, est = round(est, 3)) %>% arrange(group, outcome, rel) %>% print()
})

# Law timeline ----
# The PA local-tax statutes in the Institutional Background section, with the
# estimation window shaded. Labels alternate above and below the spine so
# neighbors (the 1998-2012 cluster) do not overlap. Only the 2012 binding date is bold.
if (RUN_LAW_TIMELINE) local({
  statutes <- tibble(
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
    type  = c(rep("EIT statute", 9), "Binding date"),
    ypos  = c( 1.0, -1.0,  2.4, -2.4,  1.0, -1.0,  2.4, -2.4,  1.0, -1.0)) %>%
    mutate(label = paste0(name, " (", year, ")\n", desc),
           face  = ifelse(type == "Binding date", "bold", "plain"))

  plot <- ggplot() +
    geom_rect(data = tibble(xmin = 2002, xmax = 2019),
              aes(xmin = xmin, xmax = xmax, ymin = -4.0, ymax = 3.9), fill = "grey85", alpha = 0.5) +
    annotate("text", x = 2010.5, y = 3.7, label = "Estimation sample 2002–2019",
             size = 3, color = "grey35", fontface = "italic") +
    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
    geom_segment(data = statutes, aes(x = year, xend = year, y = 0, yend = ypos), color = "black", linewidth = 0.5) +
    geom_point(data = statutes, aes(x = year, y = 0), color = "black", size = 3) +
    geom_label(data = statutes, aes(x = year, y = ypos, label = label),
               color = "black", size = 2.5, linewidth = 0, fill = "white", lineheight = 0.9,
               vjust = ifelse(statutes$ypos > 0, 0, 1), fontface = statutes$face) +
    scale_x_continuous(breaks = seq(1930, 2020, 10), limits = c(1926, 2024)) +
    scale_y_continuous(limits = c(-4.0, 3.9)) +
    labs(title = "Timeline of Pennsylvania local-tax rules",
         subtitle = "Statutes governing the Earned Income Tax (EIT) and shaping the design",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(color = "grey30"), axis.line.x = element_line(color = "grey60"),
          legend.position = "none", plot.title = element_text(face = "bold"))
  ggsave(file.path(OUT_DIR, "law_timeline.png"), plot, width = 11, height = 6.5, dpi = 300)
})

if (requireNamespace("beepr", quietly = TRUE)) beepr::beep()


# Revenue tradeoff figures (RUN_REVENUE_TRADEOFF_FIGS) ----
# Reads what 3_models/1_main_models.R -> RUN_REVENUE_TRADEOFF wrote. Two figures:
#   1. ATT(d) for both outcomes side by side, per cell.
#   2. How the revenue response and the tradeoff move with the rate already
#      levied, with the mechanical benchmark 1/tau0 for reference.
if (RUN_REVENUE_TRADEOFF_FIGS) local({
  f_dr <- file.path(RESULTS_DIR, "revenue_tradeoff_doseresponse.csv")
  f_gr <- file.path(RESULTS_DIR, "revenue_tradeoff_gradient.csv")
  if (!file.exists(f_dr)) { message("[fig] no revenue tradeoff results; skipping"); return(invisible(NULL)) }
  fig_dir <- OUT_DIR
  dr <- fread(f_dr); gr <- if (file.exists(f_gr)) fread(f_gr) else NULL
  cell_lab <- c("Inc" = "Pooled Increase", "Inc-bind" = "Inc-bind",
                "Inc-becomes" = "Inc-becomes", "Inc-nobind" = "Inc-nobind")
  dr[, cell_f := factor(cell_lab[cell], levels = unname(cell_lab))]
  dr[, out_f := factor(fifelse(outcome == "workers",
                               "Workers who live and work in the muni",
                               "Municipal EIT revenue"),
                       levels = c("Workers who live and work in the muni",
                                  "Municipal EIT revenue"))]
  g1 <- ggplot(dr, aes(dose, y)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = out_f), alpha = 0.15, colour = NA) +
    geom_line(aes(colour = out_f), linewidth = 0.7) +
    facet_wrap(~ cell_f + out_f, scales = "free", ncol = 2) +
    scale_colour_manual(values = c("#b2182b", "#2166ac")) +
    scale_fill_manual(values = c("#b2182b", "#2166ac")) +
    labs(title = "Dose response: residents lost and revenue gained",
         subtitle = paste0("ATT(d) with 95% bands, both outcomes on one shared sample.\n",
                           "Same controls, stacked fixed effects and municipality clustering as the headline."),
         x = "dose (cumulative muni rate change, pp)", y = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "none", strip.placement = "outside",
          plot.subtitle = element_text(size = 8))
  ggsave(file.path(fig_dir, "revenue_tradeoff_doseresponse.png"), g1,
         width = 8, height = 8.5, dpi = 300)

  if (!is.null(gr) && nrow(gr)) {
    gr[, cell_f := factor(cell_lab[cell], levels = unname(cell_lab))]
    long <- rbind(
      gr[, .(cell_f, tau0, est = beta_workers, lo = w_lo, hi = w_hi,
             panel = "Workers, per pp")],
      gr[, .(cell_f, tau0, est = beta_revenue, lo = r_lo, hi = r_hi,
             panel = "EIT revenue, per pp")])
    long[, panel := factor(panel, levels = c("Workers, per pp", "EIT revenue, per pp"))]
    mech <- gr[, .(cell_f, tau0, est = 1 / tau0)][
      is.finite(est) & est < 6][, panel := factor("EIT revenue, per pp",
        levels = c("Workers, per pp", "EIT revenue, per pp"))]
    # facet_wrap, not facet_grid: facet_grid shares the x scale down a column,
    # which squeezes the cells whose tau0 range is narrow into a fifth of the
    # panel. facet_wrap frees both scales panel by panel.
    lv <- as.vector(t(outer(levels(long$cell_f), levels(long$panel),
                            function(a, b) paste(a, b, sep = " -- "))))
    long[, fp := factor(paste(cell_f, panel, sep = " -- "), levels = lv)]
    mech[, fp := factor(paste(cell_f, panel, sep = " -- "), levels = lv)]
    g2 <- ggplot(long, aes(tau0, est)) +
      geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey60", alpha = 0.18) +
      geom_line(linewidth = 0.7, colour = "#2166ac") +
      geom_line(data = mech, aes(tau0, est), linetype = "dashed",
                colour = "grey35", linewidth = 0.5) +
      facet_wrap(~ fp, scales = "free", ncol = 2) +
      labs(title = "The tradeoff depends on the rate a municipality already levies",
           subtitle = paste0("Dose interacted with the pre-event resident rate. Grey dashed is the\n",
                             "mechanical revenue response 1/tau0, what a rate rise would deliver if nobody moved."),
           x = expression("pre-existing resident EIT rate " * tau[0] * " (pp)"),
           y = "effect per percentage point") +
      theme_bw(base_size = 9) + theme(plot.subtitle = element_text(size = 8))
    ggsave(file.path(fig_dir, "revenue_tradeoff_gradient.png"), g2,
           width = 8, height = 8, dpi = 300)
  }
  f_sp <- file.path(RESULTS_DIR, "revenue_tradeoff_spline.csv")
  if (file.exists(f_sp)) {
    sp <- fread(f_sp)[spec == "decomp" | spec == "pooled"]
    sp[, cell_f := factor(cell_lab[cell], levels = unname(cell_lab))]
    sp[, out_f := factor(fifelse(outcome == "workers",
                                 "Workers who live and work in the muni",
                                 "Municipal EIT revenue"),
                         levels = c("Workers who live and work in the muni",
                                    "Municipal EIT revenue"))]
    g3 <- ggplot(sp, aes(dose, est)) +
      geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
      geom_ribbon(aes(ymin = lo, ymax = hi, fill = out_f), alpha = 0.15, colour = NA) +
      geom_line(aes(colour = out_f), linewidth = 0.7) +
      facet_wrap(~ cell_f + out_f, scales = "free", ncol = 2) +
      scale_colour_manual(values = c("#b2182b", "#2166ac")) +
      scale_fill_manual(values = c("#b2182b", "#2166ac")) +
      labs(title = "Dose response, natural spline in the dose",
           subtitle = paste0("ATT(d) with 95% bands. Knots at the terciles of the positive treated doses.\n",
                             "Each cell is drawn only over the doses it actually contains."),
           x = "dose (cumulative muni rate change, pp)", y = NULL) +
      theme_bw(base_size = 9) +
      theme(legend.position = "none", plot.subtitle = element_text(size = 8))
    ggsave(file.path(fig_dir, "revenue_tradeoff_spline.png"), g3,
           width = 8, height = 8.5, dpi = 300)
  }
  message("Saved revenue tradeoff figures")
})


# Cohort-by-cohort ATT figure (RUN_COHORT_ATT_FIG) ----
# Reads 3_models/3_robustness.R -> RUN_EVENT_BY_EVENT. One point per sub-experiment,
# ordered within cell, against the pooled headline. The Cengiz et al. (2019)
# Appendix C figure: does the pooled average hide a mix of signs?
if (RUN_COHORT_ATT_FIG) local({
  fp <- file.path(RESULTS_DIR, "cohort_by_cohort_att.csv")
  if (!file.exists(fp)) { message("[fig] no cohort ATTs; skipping"); return(invisible(NULL)) }
  fig_dir <- OUT_DIR
  d <- fread(fp)
  # Headline reference lines, from the paper's own table.
  hl <- data.table(cell = c("Inc-bind","Inc-becomes","Inc-nobind"),
                   headline = c(-0.537, -0.250, -0.110))
  d <- merge(d, hl, by = "cell", all.x = TRUE)
  d[, cell_f := factor(cell, levels = c("Inc-bind","Inc-becomes","Inc-nobind"))]
  d[, lab := paste0(cohort, "  (n=", n_treated, ")")]
  setorder(d, cell_f, est)
  d[, ord := seq_len(.N)]
  g <- ggplot(d, aes(x = reorder(paste(cell, cohort), ord), y = est)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey55") +
    geom_hline(aes(yintercept = headline), colour = "#2166ac",
               linetype = "dashed", linewidth = 0.5) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = "grey60") +
    geom_point(aes(shape = sig, fill = sig), size = 2.4, colour = "#b2182b") +
    scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 19), guide = "none") +
    scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "#b2182b"), guide = "none") +
    scale_x_discrete(labels = setNames(d$lab, paste(d$cell, d$cohort))) +
    facet_wrap(~ cell_f, scales = "free", ncol = 1) +
    coord_flip() +
    labs(title = "One ATT per sub-experiment",
         subtitle = paste0(
           "Each point is a separate regression on a single stack: that cohort's treated\n",
           "municipalities against all never-changers. Blue dashed is the pooled headline.\n",
           "Filled = p<0.05, but several cohorts hold one treated muni, where the\n",
           "cluster-robust SE is not reliable (Conley and Taber 2011)."),
         x = "cohort year (treated municipalities)",
         y = "effect per percentage point, log live-and-work") +
    theme_bw(base_size = 9) + theme(plot.subtitle = element_text(size = 7.5))
  ggsave(file.path(fig_dir, "cohort_by_cohort_att.png"), g,
         width = 7.5, height = 8, dpi = 300)
  message("Saved cohort_by_cohort_att.png")
})


# Level-adjusted event-study figure (RUN_LEVEL_ADJUST_FIG) ----
# Reads 3_models/3_robustness.R -> RUN_LEVEL_ADJUST. Realized-dose ES for the pooled,
# Inc-bind and Inc-becomes groups on all four outcomes, drawn with and without
# the baseline-rate trend adjustment (A1, + tau0:year) so the correction is
# visible frame by frame rather than only in the aggregate coefficient.
if (RUN_LEVEL_ADJUST_FIG) local({
  fp <- file.path(RESULTS_DIR, "level_adjusted_es.csv")
  if (!file.exists(fp)) { message("[fig] no level-adjusted ES; skipping"); return(invisible(NULL)) }
  fig_dir <- OUT_DIR
  d <- fread(fp)
  ol <- c(log_live_and_work = "Log(work in own muni)", share_live_in = "Share live in muni",
          log_in_commuters = "Log(in-commuters)", log_total_jobs = "Log(total jobs)")
  cl <- c(Inc = "Pooled Increase", `Inc-bind` = "Inc-bind", `Inc-becomes` = "Inc-becomes")
  d[, out_f  := factor(ol[outcome], levels = unname(ol))]
  d[, cell_f := factor(cl[cell],    levels = unname(cl))]
  d[, adj_f  := factor(adjustment, levels = c("none", "A1 tau0 x year"),
                       labels = c("unadjusted", "baseline-rate trend adjusted"))]
  g <- ggplot(d, aes(rel, est, colour = adj_f, fill = adj_f)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "grey55") +
    geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey55") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.1) +
    facet_grid(out_f ~ cell_f, scales = "free_y", switch = "y") +
    scale_colour_manual(values = c("grey35", "#b2182b"), name = NULL) +
    scale_fill_manual(values = c("grey35", "#b2182b"), name = NULL) +
    scale_x_continuous(breaks = seq(-5, 5, 1)) +
    labs(title = "Realized-dose event study, with and without the baseline-rate trend adjustment",
         subtitle = paste0(
           "Adjustment is A1: one linear trend scaled by the municipality's pre-event rate level.\n",
           "Motivation: never-changers starting above 0.5pp were already losing live-and-work\n",
           "workers at -0.017 log points per year per pp of baseline (p = 0.016)."),
         x = "years relative to the rate change",
         y = "effect per percentage point of realized dose") +
    theme_bw(base_size = 9) +
    theme(legend.position = "top", strip.placement = "outside",
          plot.subtitle = element_text(size = 7.5))
  ggsave(file.path(fig_dir, "level_adjusted_es.png"), g, width = 9.5, height = 9, dpi = 300)
  message("Saved level_adjusted_es.png")
})
