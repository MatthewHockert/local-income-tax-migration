# build_design_timeline_figure.R
# Stylized timeline visual of the stacked continuous DiD design.
# Shows 6 example treated municipalities (one per binding-state cell),
# each plotted at its event year c with a [c-K, c+K]=[c-5, c+5] window,
# alongside the never-changed (NC) control band that spans 2002-2019.
#
# Output: figures/design_timeline.png

suppressMessages({
  library(data.table)
  library(ggplot2)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_dir <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

PAPER_RED    <- "#C5283D"
PAPER_BLUE   <- "#1F5673"
PAPER_GOLD   <- "#D29D2B"
PAPER_GREY   <- "#7A7A7A"
PAPER_PURPLE <- "#7B2CBF"
PAPER_PINK   <- "#E96A99"
PAPER_NAVY   <- "#0B2545"

K <- 5L

# Stylized example municipalities, one per cell.  Names are illustrative;
# binding-state labels follow the institutional rule.
ex <- data.table(
  rank   = 6:1,
  name   = c("Wilkes-Barre (Act 47 city)",
              "Boalsburg (mid-PA borough)",
              "Greene Twp. (rural twp.)",
              "Aliquippa (Act 47 city)",
              "Lewistown (borough)",
              "Coatesville (Act 47 city)"),
  cell   = c("Inc-bind", "Inc-becomes", "Inc-nobind",
              "Dec-bind", "Dec-becomes-non", "Dec-nobind"),
  cohort = c(2009L, 2006L, 2010L, 2008L, 2007L, 2011L)
)
ex[, lower := cohort - K]
ex[, upper := cohort + K]
ex[, cell  := factor(cell, levels = c("Inc-bind","Inc-becomes","Inc-nobind",
                                        "Dec-bind","Dec-becomes-non","Dec-nobind"))]

# NC band: stretches the full panel, plotted at y = 0 (below the treated rows)
nc_band <- data.table(rank = 0, lower = 2002L, upper = 2019L,
                       cell = "NC", name = "Never-changed (NC) controls (~2,400 munis)")

# Stack for ggplot
plot_dt <- rbindlist(list(ex, nc_band), fill = TRUE)
plot_dt[, name := factor(name, levels = c(nc_band$name, rev(ex$name)))]

cell_colors <- c(
  "Inc-bind"        = PAPER_RED,
  "Inc-becomes"     = PAPER_GOLD,
  "Inc-nobind"      = PAPER_PINK,
  "Dec-bind"        = PAPER_NAVY,
  "Dec-becomes-non" = PAPER_BLUE,
  "Dec-nobind"      = "#8FAEC9",
  "NC"              = PAPER_GREY
)

p <- ggplot(plot_dt, aes(y = name, color = cell, fill = cell)) +
  # year gridlines
  geom_vline(xintercept = seq(2002, 2019, by = 2),
              color = "grey92", linewidth = 0.3) +
  # window: [c-K, c+K]
  geom_segment(aes(x = lower, xend = upper, yend = name),
                linewidth = 5, lineend = "round", alpha = 0.85) +
  # event year c marker (treated only)
  geom_point(data = ex, aes(x = cohort), shape = 21,
              color = "black", fill = "white", size = 3.2, stroke = 1.2) +
  # Labels at the event year
  geom_text(data = ex,
             aes(x = cohort, label = paste0("c=", cohort)),
             vjust = -1.8, size = 2.8, color = "black", fontface = "bold") +
  # Cell legend on the right side
  geom_text(data = ex,
             aes(x = upper + 0.4, label = cell, color = cell),
             hjust = 0, size = 3, fontface = "bold") +
  scale_color_manual(values = cell_colors, guide = "none") +
  scale_fill_manual(values = cell_colors, guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 2),
                      limits = c(2000, 2025),
                      expand = c(0, 0)) +
  # Shade panel-coverage window 2002-2019
  annotate("rect", xmin = 2002, xmax = 2019, ymin = -Inf, ymax = Inf,
            fill = "grey97", alpha = 0.4) +
  annotate("text", x = 2002, y = 7.7, label = "panel start (2002)",
            angle = 90, hjust = 0, vjust = -0.4, size = 2.5, color = "grey45") +
  annotate("text", x = 2019, y = 7.7, label = "panel end (2019)",
            angle = 90, hjust = 0, vjust = 1.2, size = 2.5, color = "grey45") +
  labs(title = "Stacked continuous DiD: timeline of example sub-experiments",
        subtitle = paste0(
          "Each row = one municipality. Bar = its [c-K, c+K] window ",
          "(K=5). Open circle = first-event year c. ",
          "NC band: every never-changed municipality contributes ",
          "observations to every cohort's sub-experiment."),
        x = "Calendar year",
        y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y        = element_text(size = 9),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey25"),
    plot.margin        = margin(10, 12, 10, 10)
  )

ggsave(file.path(fig_dir, "design_timeline.png"),
        p, width = 12, height = 5.5, dpi = 300)
cat("Saved figures/design_timeline.png\n")
