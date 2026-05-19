# build_dose_trajectory_inc_becomes.R
# Inc-becomes analog of the muni-vs-SD overlay in build_uea_aggregate_figures.R.

suppressMessages({ library(data.table); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_dir <- file.path(project_dir, "figures")

PAPER_RED  <- "#C5283D"; PAPER_BLUE <- "#1F5673"

theme_paper <- function(bs = 11) theme_bw(base_size = bs) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

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
cat("Saved support_dose_trajectory_inc_becomes.png\n")
