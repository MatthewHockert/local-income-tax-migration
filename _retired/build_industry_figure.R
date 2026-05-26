# build_industry_figure.R
# Bar chart: Inc-bind cell coefficients on consumer-facing vs tradeable
# jobs (municipality level) to make the local-services-demand-spillover
# story visual. Uses the red/blue paper palette from paper_helpers.R.

suppressMessages({ library(data.table); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
fig_path <- file.path(project_dir, "figures/industry_inc_bind.png")

# Hard-coded from the municipality-level industry table
# (data/q3_unified/industry_table_stacked_continuous.csv), Inc-bind row.
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
d[, sign_color := ifelse(est < 0, "Negative", "Positive")]

p <- ggplot(d, aes(x = bucket, y = est, fill = sign_color)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = PAPER_GREY) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25,
                color = "grey20") +
  scale_fill_manual(values = c(Negative = PAPER_RED, Positive = PAPER_BLUE),
                    guide = "none") +
  labs(title = "Industry decomposition: Inc-bind cell, log(jobs) per pp",
       subtitle = paste("Municipality-level stacked-continuous DiD;",
                         "95% CI bars; red = negative, blue = positive."),
       x = NULL, y = expression(beta[Inc-bind] / "pp")) +
  theme_paper(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

ggsave(fig_path, p, width = 7, height = 4.5, dpi = 300)
cat("Wrote", fig_path, "\n")
