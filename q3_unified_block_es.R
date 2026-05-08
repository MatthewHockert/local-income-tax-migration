# q3_unified_block_es.R
# Sun-Abraham continuous event study with realized-dose interaction.
# Pre-event coefficients are estimable because realized_dose is a constant
# muni-level quantity (the eventual cumulative Delta-tau at event-time +5),
# not a within-muni-year deviation. Pre-trend leads test whether high-dose
# munis trended differently than low-dose / NC before the rate change.
#
# Spec (per outcome):
#   y_{bt} = sum_{k != -1} sum_{c} beta_{k,c} * 1{rel = k} * 1{cell = c} *
#                                      realized_dose_m + X_{mt} gamma + alpha_b + lambda_t
#
# Output: ES coefficient table per (outcome, cell, rel) and ATT summary.

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

ctrls <- c("share_police","share_streets","share_debt_svc",
           "share_eit","share_re","share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")

# ---------------------------------------------------------------------------
# Load block panel and rebuild cell classification + cohorts
# (same logic as q3_unified_block_level.R)
# ---------------------------------------------------------------------------
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))

muni_year <- unique(b[, .(muni, year, resident_eit, sd_eit, muni_nonres_eit)])
setorder(muni_year, muni, year)
muni_year[, res_lag := shift(resident_eit), by = muni]
muni_year[, dose_muni := resident_eit - res_lag]
muni_year[, has_event := !is.na(dose_muni) & abs(dose_muni) > 1e-6]
cohort <- muni_year[has_event == TRUE,
                     .(c_muni = min(year), dir_muni = sign(dose_muni[which.min(year)])),
                     by = muni]

my <- merge(muni_year, cohort, by = "muni", all.x = TRUE)
my[, home_stack := resident_eit + sd_eit]
my[, bind := !is.na(home_stack) & !is.na(muni_nonres_eit) & home_stack > muni_nonres_eit]

cell_dt <- my[!is.na(c_muni), {
  pre  <- .SD[year == c_muni - 1L]
  post <- .SD[year == c_muni]
  cell <- "Uncertain"
  if (nrow(pre) == 1 && nrow(post) == 1 &&
      !is.na(pre$home_stack) && !is.na(post$home_stack) &&
      !is.na(pre$muni_nonres_eit) && !is.na(post$muni_nonres_eit)) {
    pre_b  <- pre$home_stack  > pre$muni_nonres_eit
    post_b <- post$home_stack > post$muni_nonres_eit
    if (dir_muni[1] > 0) {
      cell <- if (post_b && pre_b) "Inc-bind"
              else if (post_b && !pre_b) "Inc-becomes"
              else "Inc-nobind"
    } else {
      cell <- if (post_b && pre_b) "Dec-bind"
              else if (!post_b && pre_b) "Dec-becomes-non"
              else "Dec-nobind"
    }
  }
  list(cell_muni = cell)
}, by = muni]
nc_munis <- setdiff(unique(my$muni), unique(cohort$muni))
cell_dt <- rbind(cell_dt, data.table(muni = nc_munis, cell_muni = "NC"))
cell_dt <- cell_dt[cell_muni != "Uncertain"]

# ---------------------------------------------------------------------------
# Realized dose per muni: cumulative Delta-tau between cy-1 and cy+5
# (or last observed pre panel-end). Constant within muni.
# ---------------------------------------------------------------------------
realized_dose <- merge(my, cohort, by = "muni")[
  !is.na(c_muni.x), .(muni, year, resident_eit, c_muni = c_muni.x)]
rd <- realized_dose[, {
  pre  <- resident_eit[year == c_muni - 1L]
  yr_max <- min(c_muni + K_stack, max(year, na.rm = TRUE))
  end  <- resident_eit[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose = end_v - pre_v)
}, by = muni]
rd[is.na(realized_dose), realized_dose := 0]
# NC munis: realized_dose = 0
rd <- rbind(rd, data.table(muni = nc_munis, realized_dose = 0))

# ---------------------------------------------------------------------------
# Attach to block panel and build stack
# ---------------------------------------------------------------------------
b <- merge(b, cell_dt, by = "muni", all.x = TRUE)
b <- merge(b, cohort[, .(muni, c_muni)], by = "muni", all.x = TRUE)
b <- merge(b, rd, by = "muni", all.x = TRUE)
b <- b[!is.na(cell_muni)]

build_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }), fill = TRUE)
}

cat("Building stacked panel...\n")
stk <- build_stack(b)
stk[, stack_block := paste0(stack, "_", block_geoid)]
stk[, stack_year  := paste0(stack, "_", year)]
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
# rel must be integer with -1 as reference (omitted period)
stk[, rel := as.integer(rel)]
cat(sprintf("Stacked rows: %s; treated blocks: %s; NC blocks: %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell == "NC", block_geoid]), big.mark=",")))

# ---------------------------------------------------------------------------
# Run Sun-Abraham continuous ES per outcome
# Spec: i(rel, realized_dose, treat_cell, ref = -1, ref2 = "NC")
# fixest's i() supports two-way interaction; equivalent to dummy expansion.
# ---------------------------------------------------------------------------
es_results <- list()
att_results <- list()
for (o in outcomes) {
  d <- stk[, c("block_geoid","year","muni","sd_id","stack_block","stack_year",
                "treat_cell","realized_dose","rel", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  cat(sprintf("\n[%s] N=%s rows ... ", o, format(nrow(d), big.mark=",")))
  # Per-cell ES: subset to NC + one cell at a time so the ES is identified
  # cleanly per cell (avoids 6-way multinomial interaction fitting).
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- d[treat_cell %in% c("NC", cl)]
    if (nrow(dd) == 0 || sum(dd$treat_cell == cl) == 0) next
    f <- as.formula(paste0(o, " ~ i(rel, realized_dose, ref = -1) + ",
                            ctrls_str, " | stack_block + stack_year"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni + sd_id),
                   error = function(e) { cat("\n   error in", cl, ":", conditionMessage(e), "\n"); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic
    # ATT per cell: average of post-event coefs (rel = 0..5)
    post_est <- ic[rel >= 0 & rel <= 5, mean(est, na.rm = TRUE)]
    post_se  <- ic[rel >= 0 & rel <= 5, sqrt(mean(se^2, na.rm = TRUE) / .N)]
    pre_est  <- ic[rel <= -2, mean(est, na.rm = TRUE)]
    pre_se   <- ic[rel <= -2, sqrt(mean(se^2, na.rm = TRUE) / .N)]
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, cell = cl,
      att_post_per_pp = round(post_est, 3),
      att_post_se     = round(post_se, 3),
      att_pre_per_pp  = round(pre_est, 3),
      att_pre_se      = round(pre_se, 3),
      n = nobs(m))
  }
  cat("done.\n")
}
es_dt  <- rbindlist(es_results, fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "block_level_es_continuous.csv"))
fwrite(att_dt, file.path(out_dir, "block_level_att_continuous.csv"))
cat(sprintf("\nSaved ES (%d rows) and ATT (%d rows).\n", nrow(es_dt), nrow(att_dt)))

# ---------------------------------------------------------------------------
# ES figures: one panel per outcome, faceted by cell, lines for est w/ 95% CI
# Add rel=-1 = 0 reference point.
# ---------------------------------------------------------------------------
es_plot <- copy(es_dt)
es_plot <- rbind(es_plot,
                  unique(es_plot[, .(outcome, cell)])[, .(outcome, cell, rel = -1L,
                                                          est = 0, se = 0, p = NA, n = NA)],
                  fill = TRUE)
es_plot[, lo := est - 1.96 * se]
es_plot[, hi := est + 1.96 * se]

for (o in outcomes) {
  dd <- es_plot[outcome == o]
  p <- ggplot(dd, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) +
    geom_line() +
    facet_wrap(~ cell, ncol = 3, scales = "free_y") +
    labs(title = paste0("Sun-Abraham continuous ES (block-level): ", o),
         subtitle = "y-axis: per-pp realized-dose effect at event-time k; pre-trend test = leads",
         x = "Event time (rel)", y = "Coefficient on rel x realized_dose") +
    theme_bw(base_size = 9)
  ggsave(file.path(fig_dir, paste0("block_es_", o, ".png")),
         p, width = 8.5, height = 5, dpi = 300)
}
cat("\nSaved ES figures to figures/block_es_*.png\n")
cat("\n--- ATT summary ---\n")
print(att_dt)
