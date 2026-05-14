# build_fiscal_controls_figure.R
# Density figure showing the distribution of muni AFR shares and Census F-33
# SD shares used as time-varying fiscal controls in the regressions. Each
# panel is one share variable; each density is the pooled distribution
# across municipality-years (muni shares) or SD-years (SD shares).
#
# Inputs:
#   data/pa_panel.rds       (muni AFR shares: share_police, share_streets,
#                            share_debt_svc, share_eit, share_re,
#                            share_intergov)
#   data/pa_sd_controls.rds (Census F-33 SD shares)
#
# Output:
#   figures/fiscal_controls_distributions.png

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")
fig_dir  <- file.path(project_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

PAPER_RED  <- "#C5283D"
PAPER_BLUE <- "#1F5673"

# ---------- Muni AFR shares ----------
# Muni shares live on the block panel (each block inherits its muni's shares).
# Reduce to unique (muni, year) so we plot the muni-year distribution, not the
# block-year repetition.
block <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
muni <- unique(block[year >= 2002 & year <= 2019,
                       .(muni, year, share_police, share_streets,
                         share_debt_svc, share_eit, share_re, share_intergov)])

muni_share_cols <- c("share_police", "share_streets", "share_debt_svc",
                      "share_eit", "share_re", "share_intergov")
muni_share_cols <- intersect(muni_share_cols, names(muni))

muni_long <- melt(muni[, c(muni_share_cols), with = FALSE],
                   measure.vars = muni_share_cols,
                   variable.name = "share_var", value.name = "share")
muni_long <- muni_long[is.finite(share) & share >= 0 & share <= 1]
muni_long[, panel := "Municipality (DCED AFR)"]

# Pretty labels
muni_labels <- c(
  share_police     = "Police / Total Exp",
  share_streets    = "Streets / Total Exp",
  share_debt_svc   = "Debt Service / Total Exp",
  share_eit        = "EIT Revenue / Total Rev",
  share_re         = "Real Estate / Total Rev",
  share_intergov   = "Intergov. / Total Rev"
)
muni_long[, share_var := factor(share_var, levels = muni_share_cols,
                                  labels = muni_labels[muni_share_cols])]

# ---------- SD F-33 shares ----------
sd <- as.data.table(readRDS(file.path(data_dir, "pa_sd_controls.rds")))
sd <- sd[year >= 2002 & year <= 2019]

sd_share_cols <- c("share_state_rev_sd", "share_local_rev_sd",
                    "share_federal_rev_sd", "share_property_tax_sd",
                    "share_instructional_sd", "share_support_sd",
                    "share_capital_sd", "share_debt_int_sd")
sd_share_cols <- intersect(sd_share_cols, names(sd))

sd_long <- melt(sd[, c(sd_share_cols), with = FALSE],
                 measure.vars = sd_share_cols,
                 variable.name = "share_var", value.name = "share")
sd_long <- sd_long[is.finite(share) & share >= 0 & share <= 1]
sd_long[, panel := "School District (Census F-33)"]

sd_labels <- c(
  share_state_rev_sd    = "State Aid / Total Rev",
  share_local_rev_sd    = "Local Rev / Total Rev",
  share_federal_rev_sd  = "Federal / Total Rev",
  share_property_tax_sd = "Property Tax / Total Rev",
  share_instructional_sd = "Instructional / Total Exp",
  share_support_sd      = "Support Svcs / Total Exp",
  share_capital_sd      = "Capital / Total Exp",
  share_debt_int_sd     = "Debt Interest / Total Exp"
)
sd_long[, share_var := factor(share_var, levels = sd_share_cols,
                                labels = sd_labels[sd_share_cols])]

# ---------- Combine & plot ----------
all_long <- rbindlist(list(muni_long, sd_long), fill = TRUE)
all_long[, panel := factor(panel,
                            levels = c("Municipality (DCED AFR)",
                                       "School District (Census F-33)"))]

p <- ggplot(all_long, aes(x = share)) +
  geom_histogram(aes(y = after_stat(density), fill = panel),
                  bins = 40, color = "white", linewidth = 0.1,
                  alpha = 0.85) +
  facet_wrap(~ panel + share_var, ncol = 4, scales = "free",
              labeller = labeller(.multi_line = FALSE)) +
  scale_fill_manual(values = c(PAPER_RED, PAPER_BLUE), guide = "none") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                      breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "Share (numerator / total)",
       y = "Density",
       title = "Distribution of share-based fiscal controls",
       subtitle = "Top row: municipality AFR shares (Pennsylvania DCED). Bottom row: school district F-33 shares (Census).") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9, color = "grey25"),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text = element_text(size = 8),
        panel.grid.minor = element_blank())

ggsave(file.path(fig_dir, "fiscal_controls_distributions.png"),
        p, width = 11, height = 6, dpi = 200)
cat("Saved figures/fiscal_controls_distributions.png\n")
