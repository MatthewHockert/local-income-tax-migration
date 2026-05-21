# build_pooled_es_figures.R
# Two pooled-Inc/Dec realized-dose event-study overlay figures:
#   1. Block-level: 4 outcomes × Inc (red) / Dec (blue)
#   2. Muni vs SD overlay: 5 outcomes × Inc (red) / Dec (blue)
#                            × muni (solid) / SD (dotted)
#
# Inputs:
#   data/q3_unified/pooled_es_cousub_coefs.csv  (cousub stacked-continuous ES)
#   data/q3_unified/pooled_es_block_coefs.csv   (block stacked-continuous ES)
#   data/q3_unified/pooled_es_sd_coefs.csv      (SD-level pooled ES)
#
# Outputs:
#   figures/pooled_es_overlay_block.png
#   figures/pooled_es_muni_sd_overlay.png

suppressMessages({ library(data.table); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
in_dir  <- file.path(project_dir, "data", "q3_unified")
fig_dir <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

block_muni_csv <- file.path(in_dir, "pooled_es_block_coefs.csv")
block_sd_csv   <- file.path(in_dir, "pooled_es_block_sd_coefs.csv")
cousub_csv <- file.path(in_dir, "pooled_es_cousub_coefs.csv")
sd_csv     <- file.path(in_dir, "pooled_es_sd_coefs.csv")

prep <- function(path, level_tag, side_tag = NA) {
  if (!file.exists(path)) return(NULL)
  d <- fread(path)[, level := level_tag]
  if ("direction" %in% names(d)) d <- d[direction != "Dec"]
  if (!is.na(side_tag)) d[, side := side_tag]
  # Add reference period at rel = -1 with zero so lines are continuous
  ref_keys <- intersect(c("outcome","direction","level","side"), names(d))
  ref <- unique(d[, ..ref_keys])
  ref[, `:=`(rel = -1L, est = 0, se = 0, p = NA, n = NA)]
  rbind(d, ref, fill = TRUE)
}

block_muni_dt <- prep(block_muni_csv, "block", side_tag = "Municipality")
block_sd_dt   <- prep(block_sd_csv,   "block", side_tag = "School district")
cousub_dt <- prep(cousub_csv, "Municipality")
sd_dt     <- prep(sd_csv,     "School district")

# ---------------------------------------------------------------------------
# Figure 1a: Block-level pooled ES, MUNI-cohort stack only
# Figure 1b: Block-level pooled ES, SD-cohort stack only
# ---------------------------------------------------------------------------
plot_block_pooled <- function(dt, side_label, color_main, out_path) {
  if (is.null(dt) || nrow(dt) == 0) return(invisible())
  dt <- copy(dt)
  dt[, lo := est - 1.96 * se]
  dt[, hi := est + 1.96 * se]
  dt[, direction := factor(direction, levels = c("Inc"))]
  block_outcomes <- c("log_home_count","log_work_count",
                       "home_in_muni_share","share_work_outside")
  dt[, outcome := factor(outcome, levels = block_outcomes,
                          labels = outcome_label(block_outcomes))]
  p <- ggplot(dt[!is.na(outcome)],
              aes(x = rel, y = est, color = direction, fill = direction,
                   group = direction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = PAPER_GREY) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = PAPER_GREY) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.1) +
    facet_wrap(~ outcome, ncol = 2, scales = "free_y") +
    scale_color_manual(values = c(Inc = PAPER_RED),
                        labels = c(Inc = "Pooled Inc")) +
    scale_fill_manual(values = c(Inc = PAPER_RED),
                       guide = "none") +
    labs(title = paste0("Block-level pooled realized-dose ES (", side_label, " cohort)"),
         subtitle = "Per-pp coefficient by event-time. Color = direction. 95% confidence ribbons.",
         x = expression(paste("Event time ", italic(k))),
         y = expression(beta[k])) +
    theme_paper(base_size = 10)
  ggsave(out_path, p, width = 11, height = 6.5, dpi = 300)
  cat(sprintf("Wrote %s\n", basename(out_path)))
}

plot_block_pooled(block_muni_dt, side_label = "muni-rate",
                   out_path = file.path(fig_dir, "pooled_es_block_muni_cohort.png"))
plot_block_pooled(block_sd_dt,   side_label = "school-district",
                   out_path = file.path(fig_dir, "pooled_es_block_sd_cohort.png"))

# Also keep the combined overlay as a supplementary figure for the appendix
block_dt <- rbind(block_muni_dt, block_sd_dt, fill = TRUE)
if (!is.null(block_dt) && nrow(block_dt) > 0) {
  block_dt[, lo := est - 1.96 * se]
  block_dt[, hi := est + 1.96 * se]
  block_dt[, direction := factor(direction, levels = c("Inc"))]
  block_dt[, side := factor(side, levels = c("Municipality","School district"))]
  block_outcomes <- c("log_home_count","log_work_count",
                      "home_in_muni_share","share_work_outside")
  block_dt[, outcome := factor(outcome, levels = block_outcomes,
                                labels = outcome_label(block_outcomes))]
  p1 <- ggplot(block_dt[!is.na(outcome)],
               aes(x = rel, y = est, color = direction, fill = direction,
                    linetype = side, group = interaction(direction, side))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = PAPER_GREY) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = PAPER_GREY) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.0) +
    facet_wrap(~ outcome, ncol = 2, scales = "free_y") +
    scale_color_manual(values = c(Inc = PAPER_RED),
                        labels = c(Inc = "Pooled Inc")) +
    scale_fill_manual(values = c(Inc = PAPER_RED),
                       guide = "none") +
    scale_linetype_manual(values = c(Municipality = "solid",
                                      "School district" = "dotted")) +
    labs(title = "Block-level pooled realized-dose event study (overlay)",
         subtitle = paste("Per-pp coefficient by event-time. Color: direction;",
                          "line type: rate-setter cohort. 95% confidence ribbons."),
         x = expression(paste("Event time ", italic(k))),
         y = expression(beta[k])) +
    theme_paper(base_size = 9) +
    theme(legend.box = "vertical")
  ggsave(file.path(fig_dir, "pooled_es_overlay_block.png"),
         p1, width = 12, height = 7.5, dpi = 300)
  cat("Wrote figures/pooled_es_overlay_block.png (supplementary overlay)\n")
}

# ---------------------------------------------------------------------------
# Figure 2: Muni vs SD pooled ES overlay (shared outcomes)
# ---------------------------------------------------------------------------
if (!is.null(cousub_dt) && !is.null(sd_dt)) {
  muni_sd <- rbind(cousub_dt, sd_dt, fill = TRUE)
  muni_sd[, lo := est - 1.96 * se]
  muni_sd[, hi := est + 1.96 * se]
  muni_sd[, direction := factor(direction, levels = c("Inc"))]
  muni_sd[, level := factor(level, levels = c("Municipality","School district"))]
  muni_outcomes <- c("log_live_and_work","log_in_commuters",
                     "log_total_jobs",
                     "share_live_in","share_work_outside")
  muni_sd[, outcome := factor(outcome, levels = muni_outcomes,
                               labels = outcome_label(muni_outcomes))]
  p2 <- ggplot(muni_sd[!is.na(outcome)],
               aes(x = rel, y = est, color = direction, fill = direction,
                    linetype = level, group = interaction(direction, level))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = PAPER_GREY) +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = PAPER_GREY) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.0) +
    facet_wrap(~ outcome, ncol = 2, scales = "free_y") +
    scale_color_manual(values = c(Inc = PAPER_RED),
                        labels = c(Inc = "Pooled Inc")) +
    scale_fill_manual(values = c(Inc = PAPER_RED),
                       guide = "none") +
    scale_linetype_manual(values = c(Municipality = "solid",
                                      "School district" = "dotted")) +
    labs(title = "Municipality vs. school-district pooled realized-dose event study",
         subtitle = paste("Per-pp coefficient by event-time. Color: direction;",
                          "line type: aggregation level. 95% confidence ribbons."),
         x = expression(paste("Event time ", italic(k))),
         y = expression(beta[k])) +
    theme_paper(base_size = 9) +
    theme(legend.box = "vertical")
  ggsave(file.path(fig_dir, "pooled_es_muni_sd_overlay.png"),
         p2, width = 12, height = 7.5, dpi = 300)
  cat("Wrote figures/pooled_es_muni_sd_overlay.png\n")
}

cat("\nDone.\n")
