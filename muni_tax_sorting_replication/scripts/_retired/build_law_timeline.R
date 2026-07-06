# build_law_timeline.R
# Background figure: timeline of the PA local-tax statutes (EIT + property) that
# generate or constrain the variation, with the estimation window shaded.
# Output: figures/law_timeline.png

suppressMessages({ library(data.table); library(ggplot2) })
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
fig_dir <- file.path(ROOT, "figures")

ev <- data.table(
  year  = c(1932, 1965, 1972, 1987, 2006, 2008, 2012),
  name  = c("Sterling Act", "Act 511", "Home Rule", "Act 47",
            "Act 1", "Act 32 enacted", "Act 32 effective"),
  desc  = c("Phila. wage tax", "Local Tax Enabling Act\n(local EIT; 0.5% cap)",
            "Charter Law\n(may exceed cap)", "Distressed cities\n(may exceed cap)",
            "Taxpayer Relief Act\n(SD EIT shift; property\nanti-windfall)",
            "consolidated\nwithholding", "EIT becomes binding"),
  type  = c("EIT", "EIT", "EIT", "EIT", "Both", "EIT", "Key change"),
  ypos  = c( 1.0, -1.0,  1.0, -1.0,  2.4, -1.6,  1.4))      # vertical stagger
ev[, lab := paste0(name, " (", year, ")\n", desc)]
ev[, face := ifelse(type == "Key change", "bold", "plain")]   # bold the binding date

cols <- c("EIT" = "#2166ac", "Both" = "#1b7837", "Key change" = "#b2182b")
samp <- data.table(xmin = 2002, xmax = 2019)

g <- ggplot() +
  # estimation window
  geom_rect(data = samp, aes(xmin = xmin, xmax = xmax, ymin = -3.2, ymax = 3.2),
            fill = "grey85", alpha = 0.5) +
  annotate("text", x = 2010.5, y = -3.0, label = "Estimation sample 2002–2019",
           size = 3, color = "grey35", fontface = "italic") +
  # the timeline spine
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6) +
  # connectors + points + labels
  geom_segment(data = ev, aes(x = year, xend = year, y = 0, yend = ypos),
               color = "black", linewidth = 0.5) +
  geom_point(data = ev, aes(x = year, y = 0), color = "black", size = 3) +
  geom_label(data = ev, aes(x = year, y = ypos, label = lab),
             color = "black", size = 2.5, label.size = NA, fill = "white", lineheight = 0.9,
             vjust = ifelse(ev$ypos > 0, 0, 1), fontface = ev$face) +
  scale_x_continuous(breaks = seq(1930, 2020, 10), limits = c(1928, 2022)) +
  scale_y_continuous(limits = c(-3.6, 3.8)) +
  labs(title = "Timeline of Pennsylvania local-tax rules",
       subtitle = "Earned Income Tax (EIT) and property-tax statutes shaping the design",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey30"),
        axis.line.x = element_line(color = "grey60"),
        legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave(file.path(fig_dir, "law_timeline.png"), g, width = 10, height = 5, dpi = 300)
cat("Saved figures/law_timeline.png\n")
