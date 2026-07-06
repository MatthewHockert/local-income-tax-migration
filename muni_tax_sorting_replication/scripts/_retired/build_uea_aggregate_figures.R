# build_uea_aggregate_figures.R
# UEA-draft-specific aggregate figures: drop the city/borough/township
# muni-type split from three figures, and produce a two-line muni-vs-SD
# dose trajectory.
#
# Outputs (new files; do not overwrite main-paper figures):
#   figures/support_cohort_timing_agg.png
#   figures/support_dose_trajectory_muni_vs_sd.png
#   figures/support_dose_distribution_agg.png
#   figures/pooled_es_muni_only.png
#   figures/pooled_es_sd_only.png

suppressMessages({
  library(data.table)
  library(ggplot2)
})

# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
data_dir <- file.path(project_dir, "data")
fig_dir  <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

PAPER_RED  <- "#C5283D"
PAPER_BLUE <- "#1F5673"
PAPER_GREY <- "#7A7A7A"

theme_paper <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank())
}

# ---------------------------------------------------------------------------
# Load panel + cell labels
# ---------------------------------------------------------------------------
e <- new.env()
sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

# ---------------------------------------------------------------------------
# Figure 1: aggregate Inc-bind cohort timing (no muni-type split)
# ---------------------------------------------------------------------------
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
cat("Saved support_cohort_timing_agg.png\n")

# ---------------------------------------------------------------------------
# Figure 2: dose trajectory — muni cohort vs SD cohort
# ---------------------------------------------------------------------------
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
  cat("WARNING: SD-side cell/dose/cohort columns not in panel; muni-only.\n")
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
cat("Saved support_dose_trajectory_muni_vs_sd.png\n")

# ---------------------------------------------------------------------------
# Figure 3: aggregate dose distribution at k=+5 (no muni-type split)
# ---------------------------------------------------------------------------
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
cat("Saved support_dose_distribution_agg.png\n")

# ---------------------------------------------------------------------------
# Figure 4 & 5: split pooled ES muni-vs-SD overlay into two figures
# ---------------------------------------------------------------------------
prep_pooled <- function(path) {
  if (!file.exists(path)) return(NULL)
  d <- fread(path)
  ref_keys <- intersect(c("outcome","direction"), names(d))
  ref <- unique(d[, ..ref_keys])
  ref[, `:=`(rel = -1L, est = 0, se = 0, p = NA, n = NA)]
  rbind(d, ref, fill = TRUE)
}

cousub_dt <- prep_pooled(file.path(data_dir, "q3_unified", "pooled_es_cousub_coefs.csv"))
sd_dt     <- prep_pooled(file.path(data_dir, "q3_unified", "pooled_es_sd_coefs.csv"))

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
  cat(sprintf("Saved %s\n", basename(out_path)))
}

plot_one_level(cousub_dt, "Municipality-level",
                color_pos = NULL,
                out_path = file.path(fig_dir, "pooled_es_muni_only.png"))
plot_one_level(sd_dt, "School-district-level",
                color_pos = NULL,
                out_path = file.path(fig_dir, "pooled_es_sd_only.png"))

cat("\nDone.\n")
