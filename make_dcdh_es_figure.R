# make_dcdh_es_figure.R
# Build the DCDH event-study figure for the inc-only + NC sample.
# Values transcribed from the q3_robustness.R output (no bootstrap; analytical
# SEs are liberal per package warning). When q3_robustness is re-run with
# saveRDS of the dcdh objects, this can pull from those instead.

suppressMessages({library(data.table); library(ggplot2)})
fig_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/figures"

# Effect periods +1 .. +8, placebos -5 .. -1 (with reference at -1 = 0)
mk_dcdh <- function(out_label, eff, eff_se, plb, plb_se) {
  data.table(
    rel_year = c(-5:-1, 1:8),
    est      = c(plb, eff),
    se       = c(plb_se, eff_se),
    cohort   = "DCDH",
    outcome  = out_label
  )
}

# share_live_in
sli <- mk_dcdh("share_live_in",
  eff    = c(0.0044, -0.0044, -0.0057, -0.0010,  0.0040,  0.0034, -0.0009,  0.0055),
  eff_se = c(0.0031,  0.0023,  0.0036,  0.0045,  0.0070,  0.0074,  0.0056,  0.0105),
  plb    = rev(c(0.0043,  0.0063,  0.0034,  0.0015, -0.0045)),  # placebo_1 = -1, placebo_5 = -5
  plb_se = rev(c(0.0064,  0.0060,  0.0057,  0.0093,  0.0068)))

# log(Resident workers)
lr  <- mk_dcdh("log(Resident workers)",
  eff    = c(-0.0054, -0.0261, -0.0518, -0.0362, -0.0134,  0.0092, -0.0182,  0.0231),
  eff_se = c( 0.0221,  0.0291,  0.0390,  0.0421,  0.0430,  0.0509,  0.0517,  0.0519),
  plb    = rev(c(-0.0332,  0.0284,  0.0733, -0.0091, -0.0230)),
  plb_se = rev(c( 0.0321,  0.0316,  0.0466,  0.0686,  0.0559)))

# log(Total jobs)
lj  <- mk_dcdh("log(Total jobs)",
  eff    = c(-0.0353,  0.0121, -0.0250, -0.0252, -0.0199, -0.0067, -0.0067,  0.0031),
  eff_se = c( 0.0179,  0.0236,  0.0331,  0.0370,  0.0385,  0.0559,  0.0534,  0.0672),
  plb    = rev(c(-0.0438,  0.0131,  0.0658, -0.0025,  0.0271)),
  plb_se = rev(c( 0.0332,  0.0256,  0.0245,  0.0439,  0.0388)))

es_dcdh <- rbind(sli, lr, lj)
# Reference period at rel_year = 0 (treatment year, set to zero by convention)
ref <- unique(es_dcdh[, .(cohort, outcome)])[, .(rel_year = 0, est = 0, se = 0,
                                                   cohort, outcome)]
es_dcdh <- rbind(es_dcdh, ref)
es_dcdh[, outcome := factor(outcome, levels = c("share_live_in",
                                                  "log(Resident workers)",
                                                  "log(Total jobs)"))]
es_dcdh <- es_dcdh[order(outcome, rel_year)]

muni_red <- "#C0392B"
es_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

p_dcdh <- ggplot(es_dcdh, aes(rel_year, est, color = cohort, group = cohort)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = muni_red) +
  geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se),
                width = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.6) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("DCDH" = muni_red)) +
  labs(title    = "DCDH event study (continuous treatment): inc-only + NC",
       subtitle = "Effects (1..8) + placebos (-5..-1); analytical SE (no bootstrap); 95% CI",
       x        = "Years relative to first muni rate increase",
       y        = "Coefficient",
       color    = NULL) +
  es_theme

ggsave(file.path(fig_dir, "q3_dcdh_es.png"), p_dcdh,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_dcdh_es.png\n")
