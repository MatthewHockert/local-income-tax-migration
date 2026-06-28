# build_extra_figures.R
# Descriptive figures of the muni resident-rate (EIT) changes. FLIP-FLOPPERS
# (munis with both a positive and a negative change) are DROPPED to match the
# estimation sample (q3_unified_full_table.R codes them "Uncertain" and excludes
# them). Two change measures are shown:
#   cumulative = cumulative dtau at +5 (after the first change)
#   first      = pp of the FIRST change only
# Figures:
#   1. scatter baseline level vs change  -- {inc,dec} x {cumulative,first}
#   2. spaghetti: every unit's cumulative dose by event-time -- {inc,dec}
#   3. mirrored histogram of change magnitude (inc up / dec down) -- {cumulative,first}
#   4. histogram of baseline rate levels (all munis)
# Definitions: "level" = pre-event baseline rate; direction = sign of first change;
#   window +/-5; sample 2002-2019.

suppressMessages({ library(data.table); library(ggplot2) })
# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
fig_dir <- file.path(project_dir, "figures"); K <- 5L
RED <- "#b2182b"; BLU <- "#2166ac"

r <- as.data.table(readRDS(file.path(project_dir, "data/pa_rate_panel.rds")))
r <- r[!is.na(cousub_fips) & !is.na(resident_eit) & year >= 2002 & year <= 2019,
       .(muni = cousub_fips, year, rate = resident_eit)]
r <- r[, .(rate = mean(rate, na.rm = TRUE)), by = .(muni, year)]
setorder(r, muni, year)
r[, d := c(NA_real_, diff(rate)), by = muni]

# --- flip-flopper exclusion (match the estimation sample) -------------------
ff <- r[abs(d) > 1e-6, .(n_inc = sum(d > 0), n_dec = sum(d < 0)), by = muni]
flip <- ff[n_inc > 0 & n_dec > 0, muni]
cat(sprintf("dropping %d flip-flopper munis (both + and - changes)\n", length(flip)))
r <- r[!muni %in% flip]

# first change event per (non-flip) muni
ev <- r[abs(d) > 1e-6, .(c_year = year[1], first_d = d[1]), by = muni]
ev[, dir := fifelse(first_d > 0, "Increase", "Decrease")]
r <- merge(r, ev, by = "muni", all.x = TRUE)
r[, rel := year - c_year]
base <- r[rel == -1, .(muni, baseline = rate)]
r <- merge(r, base, by = "muni", all.x = TRUE)
r[, cum_change := rate - baseline]

treated <- r[!is.na(c_year)]
summ <- treated[rel >= 0 & rel <= K, .SD[which.max(rel)], by = muni][
  , .(muni, dir, baseline, first_d, cumulative = cum_change)]
summ[, first := first_d]
summ <- summ[!is.na(baseline)]
cat(sprintf("after drop: %d increasers, %d decreasers\n",
            summ[dir=="Increase", .N], summ[dir=="Decrease", .N]))

th <- theme_bw(base_size = 11)
meas_lab <- c(cumulative = "cumulative change at +5 (pp)", first = "first-change (pp)")

## 1. scatter: baseline level vs change -- {inc,dec} x {cumulative,first} -----
for (dd in c("Increase","Decrease")) for (m in c("cumulative","first")) {
  d0 <- summ[dir == dd]; d0[, yv := get(m)]
  g <- ggplot(d0, aes(baseline, yv)) +
    geom_point(alpha = 0.5, color = ifelse(dd=="Increase", RED, BLU)) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey30") +
    labs(title = sprintf("Baseline level vs %s (%s)", meas_lab[m], tolower(dd)),
         subtitle = "one point per municipality; flip-floppers excluded",
         x = "baseline resident rate level (%)", y = meas_lab[m]) + th
  ggsave(file.path(fig_dir, sprintf("scatter_level_vs_change_%s_%s.png",
         ifelse(dd=="Increase","inc","dec"), m)), g, width = 7, height = 5, dpi = 300)
}

## 2. spaghetti: every unit's cumulative dose by event-time -- {inc,dec} ------
sp <- treated[rel >= -K & rel <= K & !is.na(baseline)]
for (dd in c("Increase","Decrease")) {
  g <- ggplot(sp[dir == dd], aes(rel, cum_change, group = muni)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_line(alpha = 0.25, color = ifelse(dd=="Increase", RED, BLU)) +
    labs(title = sprintf("Cumulative rate change by event-time, every %s unit", tolower(dd)),
         subtitle = "0 = first change; each line one muni (no averaging); flip-floppers excluded",
         x = "event time (years from first change)", y = "cumulative rate change (pp)") + th
  ggsave(file.path(fig_dir, sprintf("trajectories_spaghetti_%s.png",
         ifelse(dd=="Increase","inc","dec"))), g, width = 7, height = 5, dpi = 300)
}

## 3. mirrored histogram of change magnitude -- {cumulative,first} -----------
bw <- 0.05
for (m in c("cumulative","first")) {
  mh <- copy(summ); mh[, mag := abs(get(m))]
  hd <- mh[, .(n = .N), by = .(dir, bin = round(mag/bw)*bw)]
  hd[, y := fifelse(dir == "Increase", n, -n)]
  g <- ggplot(hd, aes(bin, y, fill = dir)) +
    geom_col(width = bw*0.9) + geom_hline(yintercept = 0, color = "grey40") +
    scale_fill_manual(values = c("Increase"=RED, "Decrease"=BLU)) +
    labs(title = sprintf("Distribution of %s magnitude", meas_lab[m]),
         subtitle = "shared x = |change| (pp); increasers up, decreasers down; flip-floppers excluded",
         x = sprintf("|%s|", meas_lab[m]), y = "number of municipalities", fill = "direction") +
    th + theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, sprintf("change_distribution_mirrored_%s.png", m)),
         g, width = 7.5, height = 5, dpi = 300)
}

## 4. histogram of baseline rate levels (all munis) --------------------------
lev <- r[rel == -1 | is.na(c_year), .(level = rate[1]), by = muni][is.finite(level)]
g4 <- ggplot(lev, aes(level)) +
  geom_histogram(binwidth = 0.05, fill = "#4d4d4d", color = "white") +
  labs(title = "Distribution of municipal resident-rate levels",
       subtitle = "one observation per municipality (pre-event / stable level)",
       x = "resident rate level (%)", y = "number of municipalities") + th
ggsave(file.path(fig_dir, "level_histogram.png"), g4, width = 7, height = 5, dpi = 300)

cat("Saved 9 figures -> figures/ (scatter x4, spaghetti x2, mirrored x2, level x1)\n")
