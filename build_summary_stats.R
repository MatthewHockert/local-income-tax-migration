# build_summary_stats.R
# Descriptive-statistics tables and figures for §4.5 of the paper.
# Emits:
#   paper/tables/summary_muni.tex
#   paper/tables/summary_block.tex
#   paper/tables/summary_sd.tex
#   paper/tables/cell_counts_summary.tex
#   figures/rate_double_histogram.png
#   figures/act32_cdf.png
#   figures/rate_changes_by_year.png
#   figures/pa_rate_trajectories.png

suppressMessages({
  library(data.table); library(ggplot2); library(scales); library(patchwork)
  library(sf); library(ggrepel)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
fig_dir  <- file.path(project_dir, "figures")
tab_dir  <- file.path(project_dir, "paper", "tables")
dir.create(fig_dir, showWarnings = FALSE)
dir.create(tab_dir, showWarnings = FALSE)

max_year <- 2019L

# ---------------------------------------------------------------------------
# Load panels
# ---------------------------------------------------------------------------
rate    <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
cousub  <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))
block   <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
sd_ctrl <- as.data.table(readRDS(file.path(data_dir, "pa_sd_controls.rds")))
afr_ok  <- file.exists(file.path(data_dir, "pa_afr_panel.csv"))
if (afr_ok) afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))

rate   <- rate[year <= max_year]
cousub <- cousub[year <= max_year]
block  <- block[year <= max_year]

# ---------------------------------------------------------------------------
# Helper: numeric summary row
# ---------------------------------------------------------------------------
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
    "  \\resizebox{\\textwidth}{!}{",
    "  \\begin{tabular}{lrrrrrrrr}",
    "    \\toprule",
    "    Variable & N & Mean & SD & p25 & p50 & p75 & Min & Max \\\\",
    "    \\midrule")
  for (i in seq_len(nrow(rows_dt))) {
    r <- rows_dt[i]
    body <- c(body, sprintf(
      "    %s & %s & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\",
      r$label,
      format(as.integer(r$N), big.mark = ","),
      r$mean, r$sd, r$p25, r$p50, r$p75, r$min, r$max))
  }
  body <- c(body, "    \\bottomrule", "  \\end{tabular}", "  }",
            "  \\vspace{0.3em}",
            "  \\begin{minipage}{0.95\\textwidth}",
            sprintf("    \\footnotesize \\textit{Notes:} %s",
                    paste(notes, collapse = " ")),
            "  \\end{minipage}",
            "\\end{table}")
  writeLines(body, file.path(tab_dir, out_name))
  cat(sprintf("Wrote %s\n", out_name))
}

mk_row <- function(label, vec) {
  s <- summarize_vec(vec)
  data.table(label = label, N = s[1], mean = s[2], sd = s[3],
             p25 = s[4], p50 = s[5], p75 = s[6], min = s[7], max = s[8])
}

# ---------------------------------------------------------------------------
# Muni-type tagging (city / borough / township)
# ---------------------------------------------------------------------------
muni_type_from_name <- function(nm) {
  nm <- toupper(nm)
  fifelse(grepl("\\bCITY\\b", nm), "City",
   fifelse(grepl("\\bBORO|BOROUGH\\b", nm), "Borough",
    fifelse(grepl("\\bTWP|TOWNSHIP\\b", nm), "Township", "Other")))
}
rate[, muni_type := muni_type_from_name(municipality)]

# ---------------------------------------------------------------------------
# 1. Muni-level summary
# ---------------------------------------------------------------------------
cousub_keyed <- copy(cousub)
setnames(cousub_keyed, "work_city", "muni", skip_absent = TRUE)
rate_type <- unique(rate[, .(municipality_id, year, muni_type)])
rate_type[, municipality_id := as.character(municipality_id)]
cousub_keyed[, muni := as.character(muni)]
m <- merge(cousub_keyed, rate_type,
           by.x = c("muni","year"), by.y = c("municipality_id","year"),
           all.x = TRUE)
m[, home_stack := resident_eit + sd_eit]

muni_rows <- list(
  mk_row("Resident rate $\\tau^{M}$ (\\%)",        m$resident_eit),
  mk_row("Nonresident rate $\\tau^{nr,M}$ (\\%)",  m$muni_nonres_eit),
  mk_row("SD rate $\\tau^{SD}$ (\\%)",             m$sd_eit),
  mk_row("Home stack $\\tau^{M}+\\tau^{SD}$ (\\%)", m$home_stack),
  mk_row("$\\log$ live-and-work",                   m$log_live_in),
  mk_row("$\\log$ total jobs",                      m$log_total_jobs),
  mk_row("Share live-and-work in muni",             m$share_live_in),
  mk_row("Total jobs",                              m$total_jobs)
)
write_summary_table(muni_rows,
  "Muni-year summary statistics (2002--2019)",
  "tab:summary_muni", "summary_muni.tex",
  notes = c("Muni-year level; one observation per (municipality, year).",
            "Rates in percentage points. $\\tau^{nr,M}$ has reporting gaps before Act 32; the $N$ for that row is smaller than the full panel."))

# Muni-type sub-table (means only by type)
type_split <- m[!is.na(muni_type),
                .(N_muni_years = .N,
                  mean_res = mean(resident_eit, na.rm = TRUE),
                  mean_nr  = mean(muni_nonres_eit, na.rm = TRUE),
                  mean_sd  = mean(sd_eit, na.rm = TRUE),
                  mean_stk = mean(resident_eit + sd_eit, na.rm = TRUE),
                  mean_jobs = mean(total_jobs, na.rm = TRUE),
                  mean_share_in = mean(share_live_in, na.rm = TRUE)),
                by = muni_type]
fwrite(type_split, file.path(data_dir, "summary_muni_by_type.csv"))

# ---------------------------------------------------------------------------
# 2. Block-level summary
# ---------------------------------------------------------------------------
block_rows <- list(
  mk_row("Home count (workers living)",   block$home_count),
  mk_row("$\\log$ home count",            block$log_home_count),
  mk_row("Work count (jobs in block)",    block$work_count),
  mk_row("$\\log$ work count",            block$log_work_count),
  mk_row("Share live-and-work in muni",   block$home_in_muni_share),
  mk_row("Share work outside muni",       block$share_work_outside),
  mk_row("Resident rate $\\tau^{M}$ (\\%)",  block$resident_eit),
  mk_row("SD rate $\\tau^{SD}$ (\\%)",       block$sd_eit),
  mk_row("Nonresident rate $\\tau^{nr,M}$ (\\%)", block$muni_nonres_eit)
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

# ---------------------------------------------------------------------------
# 3. SD-level summary
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# 4. Cell counts (use cousub stack classification from q3_unified_full_table)
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for cell classifications...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

cell_cnt <- panel[, .(n_muni_year = .N,
                       n_munis = uniqueN(muni)),
                   keyby = .(cell_muni)]
cell_cnt[, cell_muni := factor(cell_muni,
   levels = c("NC","Inc-bind","Inc-becomes","Inc-nobind",
              "Dec-bind","Dec-becomes-non","Dec-nobind"))]
setorder(cell_cnt, cell_muni)

mean_dose <- panel[!is.na(cell_muni) & cell_muni != "NC",
                    .(mean_dose_pp = mean(dose_muni, na.rm = TRUE),
                      max_dose_pp  = max(dose_muni,  na.rm = TRUE)),
                    by = .(cell_muni)]
cell_tab <- merge(cell_cnt, mean_dose, by = "cell_muni", all.x = TRUE)
cell_tab[is.na(mean_dose_pp), mean_dose_pp := 0]
cell_tab[is.na(max_dose_pp),  max_dose_pp  := 0]

body <- c(
  "\\begin{table}[H]", "  \\centering",
  "  \\caption{Cell counts: muni resident-rate side, 2002--2019}",
  "  \\label{tab:cell_counts_summary}",
  "  \\small",
  "  \\begin{tabular}{lrrrr}",
  "    \\toprule",
  "    Cell & Munis & Muni-years & Mean dose (pp) & Max dose (pp) \\\\",
  "    \\midrule")
for (i in seq_len(nrow(cell_tab))) {
  r <- cell_tab[i]
  body <- c(body, sprintf(
    "    %s & %s & %s & %.3f & %.3f \\\\",
    as.character(r$cell_muni),
    format(r$n_munis, big.mark = ","),
    format(r$n_muni_year, big.mark = ","),
    r$mean_dose_pp, r$max_dose_pp))
}
body <- c(body, "    \\bottomrule", "  \\end{tabular}",
          "  \\vspace{0.3em}",
          "  \\begin{minipage}{0.95\\textwidth}",
          "    \\footnotesize \\textit{Notes:} Cells classify each muni's first resident-rate event by direction (Inc/Dec) and the binding-state transition relative to the muni's nonresident rate. NC is the never-changed control pool. Dose is the cumulative $\\Delta\\tau$ at event-time $k=+5$.",
          "  \\end{minipage}", "\\end{table}")
writeLines(body, file.path(tab_dir, "cell_counts_summary.tex"))
cat("Wrote cell_counts_summary.tex\n")

# ---------------------------------------------------------------------------
# 5. Figure: double histogram (resident vs nonresident vs home stack)
# ---------------------------------------------------------------------------
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
cat("Saved rate_double_histogram.png (ECDF)\n")

# ---------------------------------------------------------------------------
# 6. Figure: Act 32 CDF (share of munis reporting a nonresident rate, by year)
# ---------------------------------------------------------------------------
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
cat("Saved act32_cdf.png\n")

# ---------------------------------------------------------------------------
# 7. Figure: rate changes by year (red increases above zero, blue decreases below)
# ---------------------------------------------------------------------------
rate_yr <- rate[year >= 2002 & year <= 2019 & !is.na(rate_change),
                .(n_inc = sum(rate_change >  1e-6),
                  n_dec = sum(rate_change < -1e-6)),
                by = year]
rc_long <- melt(rate_yr, id.vars = "year",
                measure.vars = c("n_inc","n_dec"),
                variable.name = "direction", value.name = "n")
rc_long[, n := ifelse(direction == "n_dec", -n, n)]
rc_long[, direction := factor(direction,
        levels = c("n_inc","n_dec"),
        labels = c("Resident-rate increase","Resident-rate decrease"))]

g_rc <- ggplot(rc_long, aes(x = year, y = n, fill = direction)) +
  geom_col(width = 0.75, alpha = 0.9) +
  geom_hline(yintercept = 0, color = "grey20", linewidth = 0.4) +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2012.2, y = max(rc_long$n) * 0.9,
           label = "Act 32", hjust = 0, size = 3.5, color = "grey25") +
  scale_fill_manual(values = c("Resident-rate increase" = PAPER_RED,
                                "Resident-rate decrease" = PAPER_BLUE),
                     name = NULL) +
  scale_x_continuous(breaks = seq(2002, 2019, by = 2)) +
  labs(x = "Year", y = "Number of municipalities (+ increase / − decrease)") +
  theme_paper() +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "rate_changes_by_year.png"), g_rc,
       width = 8.5, height = 5, dpi = 200)
cat("Saved rate_changes_by_year.png\n")

# ---------------------------------------------------------------------------
# 8. Figure: top rate-changers trajectories (cumulative |dtau| vs # events, side by side)
# ---------------------------------------------------------------------------
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
cat("Saved pa_rate_trajectories.png\n")

# ---------------------------------------------------------------------------
# 9. Figure: PA map with major rate-changing cities labeled
# ---------------------------------------------------------------------------
cat("Building labeled PA map...\n")
pa_cs <- st_as_sf(readRDS(file.path(data_dir, "pa_cousub.rds")))

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

g_map <- ggplot() +
  geom_sf(data = map_dt[map_dt$change_type == "No change", ],
          fill = "grey96", color = "black", linewidth = 0.08) +
  geom_sf(data = map_dt[map_dt$change_type == "Increased", ],
          fill = PAPER_RED, color = "black", linewidth = 0.08, alpha = 0.85) +
  geom_sf(data = map_dt[map_dt$change_type == "Decreased", ],
          fill = PAPER_BLUE, color = "black", linewidth = 0.08, alpha = 0.85) +
  geom_sf(data = map_dt[map_dt$change_type == "Both", ],
          fill = "#7B2CBF", color = "black", linewidth = 0.08, alpha = 0.85) +
  geom_sf(data = pa_outline, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf() +
  labs(title = NULL,
       caption = "Red: muni raised resident rate. Blue: lowered. Purple: both directions. Grey: no change.") +
  theme_paper() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_text(size = 8, hjust = 0))
ggsave(file.path(fig_dir, "pa_map.png"), g_map,
       width = 11, height = 7, dpi = 220)
cat("Saved pa_map.png\n")

cat("\nDone. Tables in paper/tables/, figures in figures/.\n")
