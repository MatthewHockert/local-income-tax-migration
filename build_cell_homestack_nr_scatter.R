# build_cell_homestack_nr_scatter.R
# Within-cell diagnostic: how does the home stack (muni res + SD) co-move
# with the nonresident rate? Computes the correlation per cell and produces
# a scatter plot (one panel per cell + a combined faceted figure).
#
# Outputs:
#   figures/cell_homestack_nr_<cell>.png   (4 individual panels)
#   figures/cell_homestack_nr_facet.png    (single faceted figure)
#   data/q3_unified/cell_homestack_nr_corr.csv

suppressMessages({
  library(data.table)
  library(ggplot2)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_dir  <- file.path(project_dir, "figures")
data_dir <- file.path(project_dir, "data", "q3_unified")

PAPER_RED  <- "#C5283D"
PAPER_BLUE <- "#1F5673"
theme_paper <- function(base_size = 11) theme_bw(base_size = base_size) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text       = element_text(face = "bold"))

# ----------------------------------------------------------------------
# Pull the cousub panel via the master prep (gives cell_muni labels +
# the Dec filter already applied)
# ----------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for panel + cell labels...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

# Build the (muni, year) dataset
keep_cols <- c("muni", "year", "cell_muni",
               "resident_eit", "sd_eit", "muni_nonres_eit")
d <- panel[, ..keep_cols]
d <- d[!is.na(resident_eit) & !is.na(sd_eit) & !is.na(muni_nonres_eit)]
d[, home_stack := resident_eit + sd_eit]
d[, nr := muni_nonres_eit]

# Cell levels to plot (drop NC for the headline panels; include it in the
# faceted figure for context)
cell_order <- c("Inc-bind", "Inc-becomes", "Inc-nobind", "NC")
d[, cell_muni := factor(as.character(cell_muni),
                          levels = cell_order)]
d <- d[!is.na(cell_muni)]

cat(sprintf("\nObservations per cell:\n"))
print(d[, .N, by = cell_muni])

# ----------------------------------------------------------------------
# Per-cell correlation table (Pearson + Spearman)
# ----------------------------------------------------------------------
corr_dt <- d[, .(
  n_obs    = .N,
  n_munis  = uniqueN(muni),
  pearson  = cor(home_stack, nr, use = "pairwise.complete.obs"),
  pearson_p = suppressWarnings(cor.test(home_stack, nr)$p.value),
  spearman = cor(home_stack, nr, method = "spearman",
                  use = "pairwise.complete.obs"),
  mean_home = mean(home_stack),
  mean_nr   = mean(nr),
  sd_home  = sd(home_stack),
  sd_nr    = sd(nr)
), by = cell_muni]
setorder(corr_dt, cell_muni)
cat("\nCorrelation between home stack and nonresident rate, by cell:\n")
print(corr_dt)
fwrite(corr_dt, file.path(data_dir, "cell_homestack_nr_corr.csv"))

# ----------------------------------------------------------------------
# Helper: build one scatter for a given cell
# ----------------------------------------------------------------------
make_panel <- function(cell_name, dat, corr_row, color) {
  rmax <- max(c(dat$home_stack, dat$nr), na.rm = TRUE) * 1.05
  rmin <- 0
  ggplot(dat, aes(x = nr, y = home_stack)) +
    geom_abline(slope = 1, intercept = 0,
                 linetype = "dashed", color = "grey50") +
    geom_point(alpha = 0.45, color = color, size = 1.4) +
    geom_smooth(method = "lm", se = FALSE,
                 color = "black", linewidth = 0.5, formula = y ~ x) +
    coord_fixed(xlim = c(rmin, rmax), ylim = c(rmin, rmax)) +
    labs(title = cell_name,
          subtitle = sprintf("n = %s muni-years, r = %.3f (p = %s)",
                              format(corr_row$n_obs, big.mark = ","),
                              corr_row$pearson,
                              formatC(corr_row$pearson_p,
                                       format = "g", digits = 2)),
          x = expression(tau^{"nr,muni"}~"(%)"),
          y = expression(tau^{"res,muni"}+tau^{"SD"}~"(%)")) +
    theme_paper()
}

panel_color <- function(cl) switch(cl,
  "Inc-bind"    = PAPER_RED,
  "Inc-becomes" = "#E07B00",
  "Inc-nobind"  = PAPER_BLUE,
  "NC"          = "grey40",
  "black")

# Save one PNG per cell
for (cl in cell_order) {
  dsub <- d[cell_muni == cl]
  if (nrow(dsub) == 0) next
  cr <- corr_dt[cell_muni == cl]
  g <- make_panel(cl, dsub, cr, panel_color(cl))
  fname <- sprintf("cell_homestack_nr_%s.png",
                    gsub("-", "_", tolower(cl)))
  ggsave(file.path(fig_dir, fname), g, width = 5.5, height = 5.5, dpi = 300)
  cat(sprintf("Saved %s\n", fname))
}

# Faceted figure: all four cells in one png
d[, panel_label := sprintf("%s  (r = %.3f, n = %s)",
                            as.character(cell_muni),
                            corr_dt$pearson[match(as.character(cell_muni),
                                                    as.character(corr_dt$cell_muni))],
                            format(corr_dt$n_obs[match(as.character(cell_muni),
                                                         as.character(corr_dt$cell_muni))],
                                    big.mark = ","))]
# Preserve the factor order in the facet labels
panel_levels <- vapply(cell_order, function(cl) {
  cr <- corr_dt[cell_muni == cl]
  sprintf("%s  (r = %.3f, n = %s)", cl, cr$pearson,
           format(cr$n_obs, big.mark = ","))
}, character(1))
d[, panel_label := factor(panel_label, levels = panel_levels)]

rmax_all <- max(c(d$home_stack, d$nr), na.rm = TRUE) * 1.05
g_facet <- ggplot(d, aes(x = nr, y = home_stack, color = cell_muni)) +
  geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE,
                color = "black", linewidth = 0.5, formula = y ~ x) +
  facet_wrap(~ panel_label, ncol = 2) +
  coord_fixed(xlim = c(0, rmax_all), ylim = c(0, rmax_all)) +
  scale_color_manual(values = c("Inc-bind" = PAPER_RED,
                                  "Inc-becomes" = "#E07B00",
                                  "Inc-nobind" = PAPER_BLUE,
                                  "NC" = "grey40")) +
  labs(title = "Home stack vs. nonresident rate, by cell",
        subtitle = "Dashed 45-degree line = binding threshold (home stack = nr); above = home binds",
        x = expression(tau^{"nr,muni"}~"(%)"),
        y = expression(tau^{"res,muni"}+tau^{"SD"}~"(%)"),
        color = NULL) +
  theme_paper() +
  theme(legend.position = "none")
ggsave(file.path(fig_dir, "cell_homestack_nr_facet.png"),
        g_facet, width = 9.5, height = 9.5, dpi = 300)
cat("Saved cell_homestack_nr_facet.png\n")

cat("\nDone.\n")
