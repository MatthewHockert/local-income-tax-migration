# build_appendix_es_figures.R
# Regenerate every appendix-bound event-study figure in a uniform
# ggplot style matching the main-text overlay plots
# (block_es_overlay_*.png, pooled_es_overlay_*.png).
#
# Inputs (all share schema: rel, est, se, p, outcome, cell, n):
#   data/q3_unified/headline_es_coefs.csv      (cousub realized-dose)
#   data/q3_unified/sd_es_coefs.csv            (SD realized-dose)
#   data/q3_unified/block_es_full_coefs.csv    (block realized-dose; main has block_es_overlay)
#   data/q3_unified/sa_binary_cousub_coefs.csv (cousub binary SA-IW)
#   data/q3_unified/sa_binary_sd_coefs.csv     (SD binary SA-IW)
#   data/q3_unified/sa_binary_block_coefs.csv  (block binary SA-IW)
#
# Outputs (overwrites existing files):
#   figures/es_realized_<outcome>.png    (cousub realized; in paper appendix B.1 muni level)
#   figures/sd_es_<outcome>.png          (SD realized; appendix B.1 SD level)
#   figures/sa_binary_cousub_<outcome>.png  (appendix B.2 muni level)
#   figures/sa_binary_sd_<outcome>.png      (appendix B.2 SD level)
#   figures/sa_binary_block_<outcome>.png   (appendix B.2 block level)

suppressMessages({ library(data.table); library(ggplot2) })

# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
data_dir    <- file.path(project_dir, "data", "q3_unified")
fig_dir     <- file.path(project_dir, "figures")

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
    cat(sprintf("  skip: %s not found\n", coefs_path)); return(invisible())
  }
  dt <- fread(coefs_path)
  outcomes <- unique(dt$outcome)
  for (o in outcomes) {
    p <- make_plot(dt, title_prefix, o, subtitle, color)
    if (is.null(p)) next
    out_path <- file.path(fig_dir, sprintf("%s_%s.png", file_prefix, o))
    ggsave(out_path, p, width = width, height = height, dpi = 300)
    cat(sprintf("  wrote %s\n", basename(out_path)))
  }
}

cat("=== Realized-dose ES (cousub) ===\n")
save_for(file.path(data_dir, "headline_es_coefs.csv"),
          file_prefix    = "es_realized",
          title_prefix   = "Muni-level realized-dose ES",
          subtitle       = "Per-pp dose-response at event-time k; 95% CI ribbons; k=-1 omitted as reference",
          color          = PAPER_RED)

cat("\n=== Realized-dose ES (school district) ===\n")
save_for(file.path(data_dir, "sd_es_coefs.csv"),
          file_prefix    = "sd_es",
          title_prefix   = "SD-level realized-dose ES",
          subtitle       = "Per-pp dose-response at event-time k; 95% CI ribbons; k=-1 omitted as reference",
          color          = PAPER_BLUE)

cat("\n=== Binary SA-IW ES (cousub) ===\n")
save_for(file.path(data_dir, "sa_binary_cousub_coefs.csv"),
          file_prefix    = "sa_binary_cousub",
          title_prefix   = "Muni-level binary Sun-Abraham IW ES",
          subtitle       = "Interaction-weighted post-vs-pre ATT at event-time k; 95% CI ribbons",
          color          = PAPER_RED)

cat("\n=== Binary SA-IW ES (school district) ===\n")
save_for(file.path(data_dir, "sa_binary_sd_coefs.csv"),
          file_prefix    = "sa_binary_sd",
          title_prefix   = "SD-level binary Sun-Abraham IW ES",
          subtitle       = "Interaction-weighted post-vs-pre ATT at event-time k; 95% CI ribbons",
          color          = PAPER_BLUE)

cat("\n=== Binary SA-IW ES (block) ===\n")
save_for(file.path(data_dir, "sa_binary_block_coefs.csv"),
          file_prefix    = "sa_binary_block",
          title_prefix   = "Block-level binary Sun-Abraham IW ES",
          subtitle       = "Interaction-weighted post-vs-pre ATT at event-time k; 95% CI ribbons",
          color          = "#3E8E2F")

cat("\nDone.\n")
