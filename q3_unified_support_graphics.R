# q3_unified_support_graphics.R
# Visualize the support of the unified design: cohort timing, dose magnitudes,
# cell composition by muni type, sample sizes per stack.

suppressMessages({
  library(data.table); library(fixest); library(ggplot2)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")
fig_dir <- file.path(project_dir, "figures")
max_year <- 2019

# ---------------------------------------------------------------------------
# Reload the unified panel + cells (matching q3_unified.R logic)
# ---------------------------------------------------------------------------
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_heterogeneity.R"), envir = e)
panel <- e$panel
muni_size <- e$muni_size  # has type info per muni

# Restrict to drop-sample (which q3_unified_heterogeneity already used)
incbind_munis <- unique(panel[cell_muni == "Inc-bind",
                               .(muni, c_muni, baseline_muni, type)])
cat(sprintf("Inc-bind munis: %d (city=%d, borough=%d, township=%d)\n",
            nrow(incbind_munis),
            sum(incbind_munis$type == "city"),
            sum(incbind_munis$type == "borough"),
            sum(incbind_munis$type == "township")))

# Theme for all plots (UMN-style)
gopher_maroon <- "#7A0019"; gopher_gold <- "#FFCC33"
support_theme <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

# ---------------------------------------------------------------------------
# Graphic 1: Cohort timing histogram (when do Inc-bind events happen?)
# ---------------------------------------------------------------------------
g1 <- ggplot(incbind_munis, aes(x = c_muni, fill = type)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values = c(city = gopher_maroon,
                                borough = gopher_gold,
                                township = "#666666"),
                    name = "Type") +
  scale_x_continuous(breaks = seq(2003, 2019, 2)) +
  labs(title = "Inc-bind events by year and muni type",
       subtitle = sprintf("All %d Inc-bind munis (drop sample, post-NC-fix)",
                          nrow(incbind_munis)),
       x = "Year of first muni-rate event (cohort)",
       y = "Number of munis") +
  support_theme
ggsave(file.path(fig_dir, "support_cohort_timing.png"),
       g1, width = 9, height = 4.5, dpi = 300)
cat("Saved: support_cohort_timing.png\n")

# ---------------------------------------------------------------------------
# Graphic 2: Dose magnitude histogram (how big are the rate changes?)
# ---------------------------------------------------------------------------
# Compute the cumulative dose at +5 years (or last available) for each Inc-bind muni
incbind_with_dose <- panel[cell_muni == "Inc-bind" & !is.na(c_muni),
                             .(muni, type, year, c_muni, dose_muni, rel = year - c_muni)]
dose_summary <- incbind_with_dose[rel == 5,
                                   .(muni, type, dose_5yr = dose_muni)]
# Fill in: if rel=5 not observed, use max post-event dose
fill_in <- incbind_with_dose[rel >= 0 & !muni %in% dose_summary$muni,
                              .(dose_5yr = max(dose_muni, na.rm = TRUE)),
                              by = .(muni, type)]
dose_summary <- rbind(dose_summary[, .(muni, type, dose_5yr)],
                       fill_in[, .(muni, type, dose_5yr)])

g2 <- ggplot(dose_summary, aes(x = dose_5yr, fill = type)) +
  geom_histogram(binwidth = 0.1, color = "white") +
  scale_fill_manual(values = c(city = gopher_maroon,
                                borough = gopher_gold,
                                township = "#666666"),
                    name = "Type") +
  labs(title = "Cumulative resident-rate dose at year +5 (Inc-bind munis)",
       subtitle = "Distribution of post-event rate change in pp",
       x = expression(Delta*tau^muni*" at year +5 (pp)"),
       y = "Number of munis") +
  support_theme
ggsave(file.path(fig_dir, "support_dose_distribution.png"),
       g2, width = 9, height = 4.5, dpi = 300)
cat("Saved: support_dose_distribution.png\n")

cat(sprintf("\nDose summary stats: median %.2fpp, mean %.2fpp, range [%.2f, %.2f]\n",
            median(dose_summary$dose_5yr, na.rm=TRUE),
            mean(dose_summary$dose_5yr, na.rm=TRUE),
            min(dose_summary$dose_5yr, na.rm=TRUE),
            max(dose_summary$dose_5yr, na.rm=TRUE)))

# ---------------------------------------------------------------------------
# Graphic 3: Cell composition heatmap (cells x types)
# ---------------------------------------------------------------------------
cell_type <- unique(panel[!is.na(cell_muni) & cell_muni != "Uncertain",
                            .(muni, cell_muni, type)])
cell_type_counts <- cell_type[, .N, by = .(cell_muni, type)]
cell_type_counts[, cell_muni := factor(cell_muni,
                  levels = c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                             "Dec-bind","Dec-becomes-non","Dec-nobind"))]
cell_type_counts[, type := factor(type, levels = c("city","borough","township","other"))]

g3 <- ggplot(cell_type_counts, aes(x = type, y = cell_muni, fill = N)) +
  geom_tile(color = "white") +
  geom_text(aes(label = N), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#FFE7D6", high = gopher_maroon, name = "Munis") +
  labs(title = "Sample sizes: cell x muni type",
       subtitle = "Drop sample, post-NC-fix",
       x = "Muni type", y = "Cell (resident-rate event classification)") +
  support_theme +
  theme(axis.text.y = element_text(family = "mono"))
ggsave(file.path(fig_dir, "support_cell_type_heatmap.png"),
       g3, width = 9, height = 5, dpi = 300)
cat("Saved: support_cell_type_heatmap.png\n")

# ---------------------------------------------------------------------------
# Graphic 4: Stack-coverage plot (how many obs per (stack, rel_year))
# ---------------------------------------------------------------------------
stk <- e$stk_city  # this exists; rebuild for full Inc-bind cell
# Rebuild stack on full Inc-bind sample
build_stack_full <- function(p, cell_v = "cell_muni", c_v = "c_muni", dose_v = "dose_muni",
                              K_stack = 5L) {
  treated_munis <- unique(p[!is.na(get(c_v)) & get(cell_v) == "Inc-bind",
                             .(muni, cohort = get(c_v))])
  nc_munis <- unique(p[get(cell_v) == "NC", muni])
  cohort_years <- sort(unique(treated_munis$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated_munis[cohort == cy]
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t[, treat := 1L]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat := 0L]
    rbind(d_t, d_c, fill = TRUE)[, .(muni, year, treat, stack = cy, rel = year - cy)]
  }), fill = TRUE)
}
stk_full <- build_stack_full(panel)

# Coverage: for each (stack, rel), count treated and NC munis
cov <- stk_full[, .(treated = sum(treat == 1L),
                    control = sum(treat == 0L)),
                by = .(stack, rel)]
cov[, treated_label := ifelse(rel == 0, paste0(treated, " treated"), as.character(treated))]

g4 <- ggplot(cov, aes(x = rel, y = factor(stack), fill = treated)) +
  geom_tile(color = "white") +
  geom_text(aes(label = treated), color = "black", size = 2.8) +
  scale_fill_gradient(low = "#FFE7D6", high = gopher_maroon, name = "Treated munis") +
  scale_x_continuous(breaks = -5:5) +
  labs(title = "Stacked design: support across cohort x event-time",
       subtitle = "Each cell = number of treated munis observed at (cohort year, relative time)",
       x = "Years relative to event", y = "Cohort year") +
  support_theme +
  theme(panel.grid = element_blank())
ggsave(file.path(fig_dir, "support_stack_coverage.png"),
       g4, width = 9, height = 4.5, dpi = 300)
cat("Saved: support_stack_coverage.png\n")

# ---------------------------------------------------------------------------
# Graphic 5: Dose-by-event-time trajectory
# ---------------------------------------------------------------------------
traj <- panel[cell_muni == "Inc-bind" & !is.na(c_muni) &
              year >= c_muni - 5 & year <= c_muni + 5,
              .(muni, type, c_muni, year, rel = year - c_muni, dose_muni)]
traj_summary <- traj[, .(mean_dose = mean(dose_muni, na.rm = TRUE),
                          q25 = quantile(dose_muni, 0.25, na.rm = TRUE),
                          q75 = quantile(dose_muni, 0.75, na.rm = TRUE)),
                      by = .(rel, type)]

g5 <- ggplot(traj_summary, aes(x = rel, y = mean_dose, color = type, group = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = gopher_maroon) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = type), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_color_manual(values = c(city = gopher_maroon,
                                  borough = gopher_gold,
                                  township = "#666666"),
                      name = "Type") +
  scale_fill_manual(values = c(city = gopher_maroon,
                                  borough = gopher_gold,
                                  township = "#666666"),
                      name = "Type") +
  scale_x_continuous(breaks = -5:5) +
  labs(title = "Cumulative dose by event-time, Inc-bind munis",
       subtitle = "Mean and IQR of cumulative resident-rate change relative to baseline",
       x = "Years relative to first event",
       y = expression("Cumulative "*Delta*tau^muni*" (pp)")) +
  support_theme
ggsave(file.path(fig_dir, "support_dose_trajectory.png"),
       g5, width = 9, height = 4.5, dpi = 300)
cat("Saved: support_dose_trajectory.png\n")

cat("\nDone. Five support graphics in figures/.\n")
