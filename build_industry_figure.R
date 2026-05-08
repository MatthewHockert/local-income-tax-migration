# build_industry_figure.R
# Bar chart: Inc-bind cell coefficients on consumer vs tradeable jobs
# (cousub level) to make the local-services-demand-spillover story visual.

suppressMessages({ library(data.table); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_path <- file.path(project_dir, "figures/industry_inc_bind.png")

# Hard-code from the cousub industry table (data/q3_unified/industry_table_stacked_continuous.csv)
# Inc-bind row, parsed from the muni_Inc-bind column.
d <- data.table(
  bucket = c("Total","Consumer-facing","Tradeable","Health/Edu","Public","Other"),
  est    = c(-0.258, -0.297,  0.236, -0.253, -0.706, -0.489),
  se     = c( 0.087,  0.149,  0.283,  0.153,  0.408,  0.245),
  p      = c( 0.003,  0.046,  0.405,  0.099,  0.084,  0.046)
)
d[, lo := est - 1.96 * se]
d[, hi := est + 1.96 * se]
d[, bucket := factor(bucket,
                      levels = c("Total","Consumer-facing","Tradeable",
                                  "Health/Edu","Public","Other"))]

p <- ggplot(d, aes(x = bucket, y = est, fill = est < 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, color = "grey20") +
  scale_fill_manual(values = c(`TRUE` = "#d73027", `FALSE` = "#4575b4"),
                     guide = "none") +
  labs(title = "Industry decomposition: Inc-bind cell, log(jobs) per pp",
       subtitle = "Cousub-level stacked-continuous DiD, 95% CI bars",
       x = NULL, y = expression(beta[Inc-bind] / "pp")) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(fig_path, p, width = 7, height = 4.5, dpi = 300)
cat("Wrote", fig_path, "\n")
