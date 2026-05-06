# q3_piecewise_es.R
# Event-study around extensive binding switches.
# For each pair (A,B) with at least one extensive switch year c*,
# plot log_flow at relative times t = -3..+3 around c*.

suppressMessages({library(data.table); library(fixest); library(ggplot2)})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
fig_dir     <- file.path(project_dir, "figures")
K <- 3

panel <- as.data.table(readRDS(file.path(data_dir, "pa_pair_panel.rds")))
panel <- panel[!is.na(home_stack) & !is.na(tau_nr_w_filled)]
panel[, work_binds := as.integer(home_stack <= tau_nr_w_filled)]
setorder(panel, work_city, home_city, year)
panel[, work_binds_lag := shift(work_binds), by = .(work_city, home_city)]
panel[, switch_to_home := !is.na(work_binds_lag) & work_binds_lag == 1 & work_binds == 0]
panel[, switch_to_work := !is.na(work_binds_lag) & work_binds_lag == 0 & work_binds == 1]

# First switch year per pair
firsts <- panel[switch_to_home == TRUE,
                .(c_eh = min(year)), by = .(work_city, home_city)]
firstw <- panel[switch_to_work == TRUE,
                .(c_ew = min(year)), by = .(work_city, home_city)]
panel <- merge(panel, firsts, by = c("work_city","home_city"), all.x = TRUE)
panel <- merge(panel, firstw, by = c("work_city","home_city"), all.x = TRUE)
panel[, pair_id := paste0(work_city, "_", home_city)]
panel[, log_flow := ifelse(flow > 0, log(flow), NA_real_)]

build_es <- function(c_var, label) {
  d <- copy(panel[!is.na(get(c_var))])
  d[, rel_year := year - get(c_var)]
  d <- d[abs(rel_year) <= K]
  if (uniqueN(d$pair_id) == 0) return(data.table())
  m <- feols(log_flow ~ i(rel_year, ref = -1) | pair_id + year,
             data = d, cluster = ~work_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+)$", "\\1", nms[is_es]))
  out <- data.table(rel_year = rel,
                    est = ct[is_es, "Estimate"],
                    se  = ct[is_es, "Std. Error"],
                    cohort = label)
  rbind(out,
        data.table(rel_year = -1, est = 0, se = 0, cohort = label))[order(rel_year)]
}

es_eh <- build_es("c_eh", "Switch to home-binding")
es_ew <- build_es("c_ew", "Switch to work-binding")
es <- rbind(es_eh, es_ew)
es[, lo := est - 1.96*se]; es[, hi := est + 1.96*se]
es[, cohort := factor(cohort, levels = c("Switch to home-binding",
                                          "Switch to work-binding"))]

p <- ggplot(es, aes(rel_year, est, color = cohort, group = cohort)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "#C0392B") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.7) +
  scale_color_manual(values = c("Switch to home-binding" = "#C0392B",
                                "Switch to work-binding" = "#2C7FB8")) +
  labs(title    = "Piecewise event study: pair-level extensive switch in binding side",
       subtitle = sprintf("log(flow) %d-yr window around the year a pair flips binding side; pair + year FE; 95%% CI", K),
       x = "Years relative to extensive switch",
       y = "Coefficient on log(flow)",
       color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"))
ggsave(file.path(fig_dir, "q3_piecewise_es.png"), p,
       width = 9, height = 4.2, dpi = 300)
cat("Saved: q3_piecewise_es.png\n")
cat("Pairs with switch_to_home:", uniqueN(panel[!is.na(c_eh), pair_id]), "\n")
cat("Pairs with switch_to_work:", uniqueN(panel[!is.na(c_ew), pair_id]), "\n")
