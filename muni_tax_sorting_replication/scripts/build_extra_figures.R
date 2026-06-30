# build_extra_figures.R
# Descriptive figures of the local-tax rate changes. FLIP-FLOPPERS (units with both
# a + and a - change in a given rate) are DROPPED per rate to match the estimation
# sample. Two change measures: cumulative = cumulative dtau at +5; first = the first
# change only. Figures:
#   1a. scatter baseline level vs change, MUNI + SD overlaid -- {inc,dec} x {cum,first}
#   1b. scatter baseline level vs change, TOTAL home stack    -- {inc,dec} x {cum,first}
#   2.  spaghetti: every muni's cumulative dose by event-time -- {inc,dec}
#   3.  mirrored histogram of muni change magnitude           -- {cum,first}
#   4.  histogram of baseline muni-rate levels

suppressMessages({ library(data.table); library(ggplot2) })
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
fig_dir <- file.path(project_dir, "figures"); K <- 5L
RED <- "#b2182b"; BLU <- "#2166ac"

raw <- as.data.table(readRDS(file.path(project_dir, "data/pa_rate_panel.rds")))
raw <- raw[!is.na(cousub_fips) & !is.na(resident_eit) & year >= 2002 & year <= 2019,
           .(muni = cousub_fips, year, muni_rate = resident_eit, sd_rate = sd_eit)]
raw[, stack := muni_rate + fifelse(is.na(sd_rate), 0, sd_rate)]   # home stack (SD=0 if none)

# --- per-rate change summary: baseline level, first change, cumulative at +K,
#     dropping THAT rate's own flip-floppers. Returns the per-unit summary and the
#     event-time trajectory. -----------------------------------------------------
build_summ <- function(dt, col) {
  x <- dt[!is.na(get(col)), .(muni, year, rate = get(col))]
  x <- x[, .(rate = mean(rate, na.rm = TRUE)), by = .(muni, year)]
  setorder(x, muni, year)
  x[, d := c(NA_real_, diff(rate)), by = muni]
  flip <- x[abs(d) > 1e-6, .(ni = sum(d > 0), nd = sum(d < 0)), by = muni][ni > 0 & nd > 0, muni]
  x <- x[!muni %in% flip]
  ev <- x[abs(d) > 1e-6, .(c_year = year[1], first_d = d[1]), by = muni]
  ev[, dir := fifelse(first_d > 0, "Increase", "Decrease")]
  x <- merge(x, ev, by = "muni", all.x = TRUE)
  x[, rel := year - c_year]
  x <- merge(x, x[rel == -1, .(muni, baseline = rate)], by = "muni", all.x = TRUE)
  x[, cum_change := rate - baseline]
  s <- x[!is.na(c_year) & rel >= 0 & rel <= K, .SD[which.max(rel)], by = muni][
        !is.na(baseline), .(muni, dir, baseline, first = first_d, cumulative = cum_change)]
  list(summ = s, traj = x[!is.na(c_year) & rel >= -K & rel <= K & !is.na(baseline)])
}

M  <- build_summ(raw, "muni_rate")
S  <- build_summ(raw, "sd_rate")
HS <- build_summ(raw, "stack")
sm_muni <- M$summ[, setter := "Muni"]
sm_sd   <- S$summ[, setter := "SD"]
sm_stk  <- HS$summ
cat(sprintf("muni: %d inc / %d dec | SD: %d inc / %d dec | stack: %d inc / %d dec\n",
            sm_muni[dir=="Increase",.N], sm_muni[dir=="Decrease",.N],
            sm_sd[dir=="Increase",.N], sm_sd[dir=="Decrease",.N],
            sm_stk[dir=="Increase",.N], sm_stk[dir=="Decrease",.N]))

th <- theme_bw(base_size = 11)
meas_lab <- c(cumulative = "cumulative change at +5 (pp)", first = "first-change (pp)")

## 1a. scatter: MUNI + SD overlaid (square / triangle, all black) -------------
s1 <- rbind(sm_muni, sm_sd)
for (dd in c("Increase","Decrease")) for (m in c("cumulative","first")) {
  d0 <- s1[dir == dd]; d0[, yv := get(m)]
  ll <- d0[, { f <- lm(yv ~ baseline); xr <- max(baseline, na.rm = TRUE)
               .(baseline = xr, yv = as.numeric(predict(f, data.frame(baseline = xr)))) },
           by = setter]                                  # trend-line end points (for direct labels)
  g <- ggplot(d0, aes(baseline, yv, shape = setter, linetype = setter)) +
    geom_point(alpha = 0.5, color = "black", size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "grey40", linewidth = 0.6) +
    geom_label(data = ll, aes(x = baseline, y = yv, label = setter), inherit.aes = FALSE,
               hjust = -0.05, size = 3, color = "grey20", fill = "white", label.size = NA) +
    scale_shape_manual(values = c("Muni" = 0, "SD" = 2), name = NULL) +
    scale_linetype_manual(values = c("Muni" = "solid", "SD" = "dashed"), name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
    labs(title = sprintf("Baseline level vs %s (%s)", meas_lab[m], tolower(dd)),
         subtitle = "muni (square) and school-district (triangle) rate changes; trend lines labeled; flip-floppers excluded",
         x = "baseline rate level (%)", y = meas_lab[m]) +
    th + theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, sprintf("scatter_level_vs_change_%s_%s.png",
         ifelse(dd=="Increase","inc","dec"), m)), g, width = 7, height = 5, dpi = 300)
}

## 1b. scatter: TOTAL home stack (filled circle, all black) -------------------
for (dd in c("Increase","Decrease")) for (m in c("cumulative","first")) {
  d0 <- sm_stk[dir == dd]; d0[, yv := get(m)]
  g <- ggplot(d0, aes(baseline, yv)) +
    geom_point(alpha = 0.5, color = "black", shape = 16, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey40", linewidth = 0.6) +
    labs(title = sprintf("Baseline level vs %s (%s) — total home stack", meas_lab[m], tolower(dd)),
         subtitle = "home stack = muni + SD rate (moves if either changes); flip-floppers excluded",
         x = "baseline home-stack level (muni + SD, %)", y = meas_lab[m]) + th
  ggsave(file.path(fig_dir, sprintf("scatter_level_vs_change_total_%s_%s.png",
         ifelse(dd=="Increase","inc","dec"), m)), g, width = 7, height = 5, dpi = 300)
}

## 2. spaghetti (muni) -------------------------------------------------------
for (dd in c("Increase","Decrease")) {
  g <- ggplot(M$traj[dir == dd], aes(rel, cum_change, group = muni)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    geom_line(alpha = 0.25, color = ifelse(dd=="Increase", RED, BLU)) +
    labs(title = sprintf("Cumulative muni-rate change by event-time, every %s unit", tolower(dd)),
         subtitle = "0 = first change; each line one muni; flip-floppers excluded",
         x = "event time (years from first change)", y = "cumulative rate change (pp)") + th
  ggsave(file.path(fig_dir, sprintf("trajectories_spaghetti_%s.png",
         ifelse(dd=="Increase","inc","dec"))), g, width = 7, height = 5, dpi = 300)
}

## 3. mirrored histogram (muni) ----------------------------------------------
bw <- 0.05
for (m in c("cumulative","first")) {
  mh <- copy(sm_muni); mh[, mag := abs(get(m))]
  hd <- mh[, .(n = .N), by = .(dir, bin = round(mag/bw)*bw)]
  hd[, y := fifelse(dir == "Increase", n, -n)]
  g <- ggplot(hd, aes(bin, y, fill = dir)) +
    geom_col(width = bw*0.9) + geom_hline(yintercept = 0, color = "grey40") +
    scale_fill_manual(values = c("Increase"=RED, "Decrease"=BLU)) +
    labs(title = sprintf("Distribution of muni %s magnitude", meas_lab[m]),
         subtitle = "|change| (pp); increasers up, decreasers down; flip-floppers excluded",
         x = sprintf("|%s|", meas_lab[m]), y = "number of municipalities", fill = "direction") +
    th + theme(legend.position = "bottom")
  ggsave(file.path(fig_dir, sprintf("change_distribution_mirrored_%s.png", m)),
         g, width = 7.5, height = 5, dpi = 300)
}

## 4. histogram of baseline muni-rate levels ---------------------------------
lev <- raw[, .(level = mean(muni_rate, na.rm = TRUE)), by = muni][is.finite(level)]
g4 <- ggplot(lev, aes(level)) +
  geom_histogram(binwidth = 0.05, fill = "#4d4d4d", color = "white") +
  labs(title = "Distribution of municipal resident-rate levels",
       subtitle = "one observation per municipality (mean level)",
       x = "resident rate level (%)", y = "number of municipalities") + th
ggsave(file.path(fig_dir, "level_histogram.png"), g4, width = 7, height = 5, dpi = 300)

cat("Saved 13 figures (scatter muni+SD x4, scatter total x4, spaghetti x2, mirrored x2, level x1)\n")
