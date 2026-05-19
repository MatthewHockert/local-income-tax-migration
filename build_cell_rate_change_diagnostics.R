# build_cell_rate_change_diagnostics.R
# Within-muni co-movement of resident-rate and nonresident-rate CHANGES,
# by cell. Three outputs:
#   (1) Per-cell correlation of first-differences:
#         corr(Delta tau^M_{i,t}, Delta tau^{nr,M}_{i,t}) within each cell
#       restricted to muni-years with a non-zero change on at least one side.
#   (2) Joint-event count in the Inc-bind cohort: of the 24 first events,
#       how many also have a non-zero Delta tau^{nr,M} in the same year?
#   (3) Scatter of cumulative Delta tau^M vs cumulative Delta tau^{nr,M}
#       at k = +5 from the muni's first event, per Inc-bind muni.
#
# Outputs:
#   data/q3_unified/cell_rate_change_corr.csv
#   data/q3_unified/inc_bind_joint_event_count.csv
#   figures/inc_bind_cumdose_scatter.png
#   figures/cell_rate_change_scatter_facet.png

suppressMessages({ library(data.table); library(ggplot2) })

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

cat("Sourcing q3_unified_full_table.R for panel + cell labels...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

keep_cols <- c("muni", "year", "cell_muni", "c_muni",
               "resident_eit", "sd_eit", "muni_nonres_eit")
d <- panel[, ..keep_cols]
d <- d[!is.na(resident_eit) & !is.na(muni_nonres_eit)]
setorder(d, muni, year)

# First differences within muni
d[, dM  := c(NA_real_, diff(resident_eit)),    by = muni]
d[, dNR := c(NA_real_, diff(muni_nonres_eit)), by = muni]

cell_order <- c("Inc-bind", "Inc-becomes", "Inc-nobind", "NC")
d[, cell_muni := factor(as.character(cell_muni), levels = cell_order)]
d <- d[!is.na(cell_muni)]

# ------------------------------------------------------------------
# (1) Per-cell first-difference correlation
#     Restrict to muni-years with a non-zero change on at least one side
#     to avoid the trivial correlation of zeros-with-zeros.
# ------------------------------------------------------------------
d_any_change <- d[!is.na(dM) & !is.na(dNR) &
                    (abs(dM) > 1e-9 | abs(dNR) > 1e-9)]

corr_dt <- d_any_change[, {
  ct <- tryCatch(cor.test(dM, dNR), error = function(e) NULL)
  .(
    n_obs    = .N,
    n_munis  = uniqueN(muni),
    pearson  = if (!is.null(ct)) ct$estimate else NA_real_,
    pearson_p = if (!is.null(ct)) ct$p.value else NA_real_,
    spearman = suppressWarnings(cor(dM, dNR, method = "spearman")),
    n_both_change = sum(abs(dM) > 1e-9 & abs(dNR) > 1e-9),
    n_only_M  = sum(abs(dM) > 1e-9 & abs(dNR) <= 1e-9),
    n_only_NR = sum(abs(dM) <= 1e-9 & abs(dNR) > 1e-9)
  )
}, by = cell_muni]
setorder(corr_dt, cell_muni)
cat("\nFirst-difference correlation, by cell (any-side change years):\n")
print(corr_dt)
fwrite(corr_dt, file.path(data_dir, "cell_rate_change_corr.csv"))

# ------------------------------------------------------------------
# (2) Joint-event count for Inc-bind cohort (first event year each muni)
#     Question: of the 24 Inc-bind first events, how many also have a
#     non-zero Delta tau^{nr,M} in the same year?
# ------------------------------------------------------------------
first_events <- unique(d[cell_muni != "NC" & !is.na(c_muni),
                          .(muni, cell_muni, c_muni)])
first_events <- merge(first_events,
                       d[, .(muni, year, dM, dNR)],
                       by.x = c("muni", "c_muni"),
                       by.y = c("muni", "year"),
                       all.x = TRUE)
first_events[, joint_event := abs(dM) > 1e-9 & abs(dNR) > 1e-9]
first_events[, m_only := abs(dM) > 1e-9 & abs(dNR) <= 1e-9]
first_events[, nr_only := abs(dM) <= 1e-9 & abs(dNR) > 1e-9]

joint_summary <- first_events[, .(
  n = .N,
  n_joint = sum(joint_event, na.rm = TRUE),
  n_M_only = sum(m_only, na.rm = TRUE),
  n_NR_only = sum(nr_only, na.rm = TRUE)
), by = cell_muni]
setorder(joint_summary, cell_muni)
joint_summary[, pct_joint := round(100 * n_joint / n, 1)]
cat("\nJoint-event count at each cohort's first event year:\n")
print(joint_summary)
fwrite(joint_summary, file.path(data_dir, "inc_bind_joint_event_count.csv"))

# ------------------------------------------------------------------
# (3) Cumulative Delta tau^M vs cumulative Delta tau^{nr,M} at k=+5
#     from the muni's first event, scatter per Inc-bind muni.
# ------------------------------------------------------------------
K_post <- 5L
cum_dose <- function(cell_name) {
  munis_in <- unique(d[cell_muni == cell_name & !is.na(c_muni),
                        .(muni, c_muni)])
  rbindlist(lapply(seq_len(nrow(munis_in)), function(i) {
    m  <- munis_in$muni[i]
    cy <- munis_in$c_muni[i]
    sub <- d[muni == m & year >= cy - 1L & year <= cy + K_post]
    if (nrow(sub) == 0) return(NULL)
    base_M  <- sub[year == cy - 1L, resident_eit]
    base_NR <- sub[year == cy - 1L, muni_nonres_eit]
    end_M   <- sub[year == min(max(year), cy + K_post), resident_eit]
    end_NR  <- sub[year == min(max(year), cy + K_post), muni_nonres_eit]
    if (length(base_M) == 0 || length(end_M) == 0) return(NULL)
    data.table(muni = m, cell = cell_name,
                cum_dM  = end_M  - base_M,
                cum_dNR = end_NR - base_NR)
  }))
}
cum_dt <- rbindlist(lapply(c("Inc-bind", "Inc-becomes", "Inc-nobind"),
                            cum_dose))
cum_dt[, cell := factor(cell, levels = cell_order[1:3])]
fwrite(cum_dt, file.path(data_dir, "cum_dose_M_vs_NR.csv"))

# Inc-bind only scatter (the headline)
g_incbind <- ggplot(cum_dt[cell == "Inc-bind"],
                     aes(x = cum_dNR, y = cum_dM)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(color = PAPER_RED, size = 2.5, alpha = 0.8) +
  labs(title = "Inc-bind cohort: cumulative resident-rate vs nonresident-rate changes",
        subtitle = sprintf("k = -1 to k = +%d from each muni's first event; n = %d munis",
                            K_post,
                            uniqueN(cum_dt[cell == "Inc-bind", muni])),
        x = expression("Cumulative "*Delta*tau^{"nr,muni"}*" (pp)"),
        y = expression("Cumulative "*Delta*tau^M*" (pp)")) +
  theme_paper()
ggsave(file.path(fig_dir, "inc_bind_cumdose_scatter.png"),
        g_incbind, width = 6.5, height = 5.5, dpi = 300)
cat("\nSaved inc_bind_cumdose_scatter.png\n")

# Three-cell facet
g_facet <- ggplot(cum_dt, aes(x = cum_dNR, y = cum_dM, color = cell)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ cell, ncol = 3) +
  scale_color_manual(values = c("Inc-bind" = PAPER_RED,
                                  "Inc-becomes" = "#E07B00",
                                  "Inc-nobind" = PAPER_BLUE)) +
  labs(title = "Cumulative resident-rate vs nonresident-rate changes, by cell",
        subtitle = sprintf("Per muni from k = -1 to k = +%d; dashed line = 45 degrees", K_post),
        x = expression("Cumulative "*Delta*tau^{"nr,muni"}*" (pp)"),
        y = expression("Cumulative "*Delta*tau^M*" (pp)"),
        color = NULL) +
  theme_paper() + theme(legend.position = "none")
ggsave(file.path(fig_dir, "cell_rate_change_scatter_facet.png"),
        g_facet, width = 11, height = 4.5, dpi = 300)
cat("Saved cell_rate_change_scatter_facet.png\n\n")

# Summary stats per cell on cumulative changes
cat("Cumulative-dose summary by cell:\n")
print(cum_dt[, .(
  n = .N,
  mean_dM = round(mean(cum_dM), 3),
  mean_dNR = round(mean(cum_dNR), 3),
  cor_dM_dNR = round(cor(cum_dM, cum_dNR), 3),
  n_dM_pos_dNR_zero = sum(cum_dM > 1e-9 & abs(cum_dNR) <= 1e-9),
  n_both_pos = sum(cum_dM > 1e-9 & cum_dNR > 1e-9)
), by = cell])

cat("\nDone.\n")
