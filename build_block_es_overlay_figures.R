# build_block_es_overlay_figures.R
# Make block-level event-study figures with muni-cohort and SD-cohort
# coefficients overlaid in the same panel, one figure per outcome.
# Uses the red/blue paper palette from paper_helpers.R.
#
# Inputs:
#   data/q3_unified/block_es_full_coefs.csv     (muni cohort, from
#                                                 q3_unified_block_es_full.R)
#   data/q3_unified/block_es_sd_full_coefs.csv  (SD cohort, from
#                                                 q3_unified_block_es_sd_full.R)
#
# Outputs (overwriting the existing single-side figures):
#   figures/block_es_overlay_log_home_count.png
#   figures/block_es_overlay_log_work_count.png
#   figures/block_es_overlay_home_in_muni_share.png
#   figures/block_es_overlay_share_work_outside.png

suppressMessages({ library(data.table); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
in_dir  <- file.path(project_dir, "data", "q3_unified")
fig_dir <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

muni_csv <- file.path(in_dir, "block_es_full_coefs.csv")
sd_csv   <- file.path(in_dir, "block_es_sd_full_coefs.csv")
if (!file.exists(muni_csv)) {
  stop("Missing ", muni_csv, " (run q3_unified_block_es_full.R)")
}
if (!file.exists(sd_csv)) {
  stop("Missing ", sd_csv, " (run q3_unified_block_es_sd_full.R)")
}

muni_dt <- fread(muni_csv)[, side := "Municipality"]
sd_dt   <- fread(sd_csv)[,   side := "School district"]
es <- rbind(muni_dt, sd_dt, fill = TRUE)

# Add the reference period at rel = -1 as a zero point with zero SE so the
# line is continuous through the event-time window.
ref <- unique(es[, .(outcome, cell, side)])[, .(outcome, cell, side,
                                                 rel = -1L, est = 0,
                                                 se = 0, p = NA, n = NA)]
es <- rbind(es, ref, fill = TRUE)
es[, lo := est - 1.96 * se]
es[, hi := est + 1.96 * se]

cell_levels_main <- c("Inc-bind","Inc-becomes","Inc-nobind",
                      "Dec-bind","Dec-becomes-non","Dec-nobind")
es[, cell := factor(cell, levels = cell_levels_main)]
es[, side := factor(side, levels = c("Municipality","School district"))]

outcomes <- c("log_home_count","log_work_count",
               "home_in_muni_share","share_work_outside")

for (o in outcomes) {
  dd <- es[outcome == o & !is.na(cell)]
  if (nrow(dd) == 0) next
  out_label <- outcome_label(o)
  p <- ggplot(dd, aes(x = rel, y = est, color = side, fill = side,
                       group = side)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = PAPER_GREY) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = PAPER_GREY) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    facet_wrap(~ cell, ncol = 3, scales = "free_y") +
    scale_color_side() + scale_fill_side() +
    labs(title = paste0("Block-level realized-dose event study: ", out_label),
         subtitle = paste("Per-pp coefficient at event-time k;",
                          "red = municipality-cohort, blue = SD-cohort;",
                          "stacked DiD with block-by-stack and year-by-stack FE,",
                          "two-way clustered (muni, SD)."),
         x = expression(paste("Event time ", italic(k))),
         y = expression(beta[k])) +
    theme_paper(base_size = 9)
  out_path <- file.path(fig_dir, paste0("block_es_overlay_", o, ".png"))
  ggsave(out_path, p, width = 11, height = 7, dpi = 300)
  cat(sprintf("Wrote %s\n", out_path))
}

cat("\nDone. Block ES overlay figures written to figures/.\n")
