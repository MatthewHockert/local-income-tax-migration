# build_extra_figures.R
# Descriptive figures of the muni resident-rate (EIT) changes:
#  1. scatter: baseline level vs cumulative rate change (pp) -- inc & dec
#  2. spaghetti: every unit's cumulative dose by event-time (no averaging) -- inc & dec
#  3. mirrored histogram of cumulative change magnitude (inc top / dec bottom)
#  4. histogram of baseline rate levels
# Source: data/pa_rate_panel.rds (muni resident EIT, all units incl. decreasers).
# Definitions: "level" = pre-event baseline rate; "change" = cumulative dtau (pp);
#   direction = sign of the unit's FIRST rate change; window +/-5; sample 2002-2019.

suppressMessages({ library(data.table); library(ggplot2) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_dir <- file.path(project_dir, "figures"); K <- 5L

r <- as.data.table(readRDS(file.path(project_dir, "data/pa_rate_panel.rds")))
r <- r[!is.na(cousub_fips) & !is.na(resident_eit) & year >= 2002 & year <= 2019,
       .(muni = cousub_fips, year, rate = resident_eit)]
r <- r[, .(rate = mean(rate, na.rm = TRUE)), by = .(muni, year)]      # 1 obs/muni-year
setorder(r, muni, year)

# first change event per muni
r[, d := c(NA_real_, diff(rate)), by = muni]
ev <- r[abs(d) > 1e-6, .(c_year = year[1], first_d = d[1]), by = muni]
ev[, dir := fifelse(first_d > 0, "Increase", "Decrease")]
r <- merge(r, ev, by = "muni", all.x = TRUE)
r[, rel := year - c_year]

# baseline (rate at c-1) and cumulative change from baseline
base <- r[rel == -1, .(muni, baseline = rate)]
r <- merge(r, base, by = "muni", all.x = TRUE)
r[, cum_change := rate - baseline]

# per-muni summary at +5 (or last available within window)
treated <- r[!is.na(c_year)]
summ <- treated[rel >= 0 & rel <= K, .SD[which.max(rel)], by = muni][
  , .(muni, dir, baseline, cum_change)]
summ <- summ[!is.na(baseline)]
cat(sprintf("treated munis: %d increasers, %d decreasers\n",
            summ[dir=="Increase", .N], summ[dir=="Decrease", .N]))

th <- theme_bw(base_size = 11)

## 1. scatter: baseline level vs cumulative change ---------------------------
for (dd in c("Increase","Decrease")) {
  g <- ggplot(summ[dir == dd], aes(baseline, cum_change)) +
    geom_point(alpha = 0.5, color = ifelse(dd=="Increase","#b2182b","#2166ac")) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey30") +
    labs(title = sprintf("Baseline rate level vs cumulative change (%s)", tolower(dd)),
         subtitle = "one point per municipality; change = cumulative dtau at +5 (pp)",
         x = "baseline resident rate level (%)", y = "cumulative rate change (pp)") + th
  ggsave(file.path(fig_dir, sprintf("scatter_level_vs_change_%s.png",
         ifelse(dd=="Increase","inc","dec"))), g, width = 7, height = 5, dpi = 300)
}

## 2. spaghetti: every unit's cumulative dose by event-time ------------------
sp <- treated[rel >= -K & rel <= K & !is.na(baseline)]
for (dd in c("Increase","Decrease")) {
  g <- ggplot(sp[dir == dd], aes(rel, cum_change, group = muni)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_line(alpha = 0.25, color = ifelse(dd=="Increase","#b2182b","#2166ac")) +
    labs(title = sprintf("Cumulative rate change by event-time, every %s unit", tolower(dd)),
         subtitle = "0 = first rate change; each line is one municipality (no averaging)",
         x = "event time (years from first change)", y = "cumulative rate change (pp)") + th
  ggsave(file.path(fig_dir, sprintf("trajectories_spaghetti_%s.png",
         ifelse(dd=="Increase","inc","dec"))), g, width = 7, height = 5, dpi = 300)
}

## 3. mirrored histogram of cumulative change magnitude ----------------------
mh <- copy(summ); mh[, mag := abs(cum_change)]
bw <- 0.05
hist_dt <- mh[, .(n = .N), by = .(dir, bin = round(mag/bw)*bw)]
hist_dt[, y := fifelse(dir == "Increase", n, -n)]
g3 <- ggplot(hist_dt, aes(bin, y, fill = dir)) +
  geom_col(width = bw*0.9) +
  geom_hline(yintercept = 0, color = "grey40") +
  scale_fill_manual(values = c("Increase"="#b2182b","Decrease"="#2166ac")) +
  labs(title = "Distribution of cumulative rate-change magnitude",
       subtitle = "shared x = |cumulative dtau| (pp); increasers up, decreasers down",
       x = "cumulative rate change magnitude (pp)", y = "number of municipalities",
       fill = "direction") + th + theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "change_distribution_mirrored.png"), g3, width = 7.5, height = 5, dpi = 300)

## 4. histogram of baseline rate levels --------------------------------------
lev <- r[rel == -1 | is.na(c_year), .(muni, baseline = rate)][, .(level = baseline[1]), by = muni]
lev <- lev[is.finite(level)]
g4 <- ggplot(lev, aes(level)) +
  geom_histogram(binwidth = 0.05, fill = "#4d4d4d", color = "white") +
  labs(title = "Distribution of municipal resident-rate levels",
       subtitle = "one observation per municipality (pre-event / stable level)",
       x = "resident rate level (%)", y = "number of municipalities") + th
ggsave(file.path(fig_dir, "level_histogram.png"), g4, width = 7, height = 5, dpi = 300)

cat("Saved: scatter_level_vs_change_{inc,dec}, trajectories_spaghetti_{inc,dec},\n",
    "       change_distribution_mirrored, level_histogram  -> figures/\n")
