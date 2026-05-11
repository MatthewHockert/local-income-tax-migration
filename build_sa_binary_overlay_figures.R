# build_sa_binary_overlay_figures.R
# Build binary Sun-Abraham IW event-study overlay figures: one figure per
# outcome with municipality-level (red) and school-district-level (blue)
# coefficients on the same axes, faceted by binding-state cell. Mirrors
# the block-ES overlay style. Uses paper_helpers.R for theme and palette.
#
# Inputs:
#   data/q3_unified/sa_binary_cousub_coefs.csv
#   data/q3_unified/sa_binary_sd_coefs.csv
#
# Outputs (replacing the old single-side figures):
#   figures/sa_binary_overlay_log_live_and_work.png
#   figures/sa_binary_overlay_log_in_commuters.png
#   figures/sa_binary_overlay_log_total_residents.png
#   figures/sa_binary_overlay_share_live_in.png
#   figures/sa_binary_overlay_share_work_outside.png

suppressMessages({ library(data.table); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
in_dir  <- file.path(project_dir, "data", "q3_unified")
fig_dir <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

cousub_csv <- file.path(in_dir, "sa_binary_cousub_coefs.csv")
sd_csv     <- file.path(in_dir, "sa_binary_sd_coefs.csv")
if (!file.exists(cousub_csv) || !file.exists(sd_csv)) {
  stop("Missing input(s). Run q3_unified_sa_binary_es.R first.")
}

cousub <- fread(cousub_csv)[, side := "Municipality"]
sd_dt  <- fread(sd_csv)[,     side := "School district"]
es <- rbind(cousub, sd_dt, fill = TRUE)

# Clip event-time window to a sensible band: [-5, +5]
es <- es[rel >= -5 & rel <= 5]

# Add reference at rel=-1 with zero estimate so lines are continuous
ref <- unique(es[, .(outcome, cell, side)])[
         , .(outcome, cell, side, rel = -1L, est = 0, se = 0, p = NA, n = NA)]
es <- rbind(es, ref, fill = TRUE)

# Confidence ribbons
es[, lo := est - 1.96 * se]
es[, hi := est + 1.96 * se]

cell_levels_main <- c("Inc-bind","Inc-becomes","Inc-nobind",
                      "Dec-bind","Dec-becomes-non","Dec-nobind")
es[, cell := factor(cell, levels = cell_levels_main)]
es[, side := factor(side, levels = c("Municipality","School district"))]

# Drop cells with no data on either side
es <- es[!is.na(cell)]

outcomes <- c("log_live_and_work","log_in_commuters","log_total_residents",
              "share_live_in","share_work_outside")

for (o in outcomes) {
  dd <- es[outcome == o]
  if (nrow(dd) == 0) next
  out_label <- outcome_label(o)
  p <- ggplot(dd, aes(x = rel, y = est, color = side, fill = side, group = side)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = PAPER_GREY) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = PAPER_GREY) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    facet_wrap(~ cell, ncol = 3, scales = "free_y") +
    scale_color_side() + scale_fill_side() +
    labs(title = paste0("Binary Sun--Abraham IW event study: ", out_label),
         subtitle = paste("Red = municipality level; blue = school district level.",
                          "Reference period $k=-1$ omitted; 95% confidence ribbons."),
         x = expression(paste("Event time ", italic(k))),
         y = expression(beta[k])) +
    theme_paper(base_size = 9)
  out_path <- file.path(fig_dir, paste0("sa_binary_overlay_", o, ".png"))
  ggsave(out_path, p, width = 11, height = 7, dpi = 300)
  cat(sprintf("Wrote %s\n", out_path))
}

cat("\nDone.\n")
