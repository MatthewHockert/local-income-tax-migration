# q3_unified_block_es_sd.R
# SD-side mirror of q3_unified_block_es.R. Treatment is the SD rate. Cohorts
# are SD-rate first-event years. Cells are classified by SD direction x
# binding state at the muni-SD pair level (binding = home_stack > muni_nonres).
# Realized dose = SD's cumulative Delta(sd_eit) at +5.
#
# This produces the SD-side parallel point estimates that complete the
# unified-model picture (muni side handled by q3_unified_block_es.R).

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
# Load block panel
# ---------------------------------------------------------------------------
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))

# SD-year rate panel (one rate per SD per year)
sd_year <- unique(b[!is.na(sd_id), .(sd_id, year, sd_eit)])
setorder(sd_year, sd_id, year)
sd_year[, sd_lag := shift(sd_eit), by = sd_id]
sd_year[, dose_sd := sd_eit - sd_lag]
sd_year[, has_event := !is.na(dose_sd) & abs(dose_sd) > 1e-6]

# SD cohort: first event year + direction
cohort_sd <- sd_year[has_event == TRUE,
                      .(c_sd = min(year), dir_sd = sign(dose_sd[which.min(year)])),
                      by = sd_id]
cat(sprintf("SDs with at least one rate event: %d / %d\n",
            nrow(cohort_sd), uniqueN(sd_year$sd_id)))

# Cell classification at muni-SD pair level using muni-year nonres + home_stack
my <- unique(b[, .(muni, sd_id, year, resident_eit, sd_eit, muni_nonres_eit)])
setorder(my, muni, sd_id, year)
my[, home_stack := resident_eit + sd_eit]
my[, bind := !is.na(home_stack) & !is.na(muni_nonres_eit) & home_stack > muni_nonres_eit]
my <- merge(my, cohort_sd, by = "sd_id", all.x = TRUE)

# Cell at the (muni, sd_id, c_sd) tuple - same muni can appear in multiple SD cohorts
# but in our primary-SD assignment each muni has one sd_id, so this collapses.
cell_dt <- my[!is.na(c_sd), {
  pre  <- .SD[year == c_sd - 1L]
  post <- .SD[year == c_sd]
  cell <- "Uncertain"
  if (nrow(pre) == 1 && nrow(post) == 1 &&
      !is.na(pre$home_stack) && !is.na(post$home_stack) &&
      !is.na(pre$muni_nonres_eit) && !is.na(post$muni_nonres_eit)) {
    pre_b  <- pre$home_stack  > pre$muni_nonres_eit
    post_b <- post$home_stack > post$muni_nonres_eit
    if (dir_sd[1] > 0) {
      cell <- if (post_b && pre_b) "Inc-bind"
              else if (post_b && !pre_b) "Inc-becomes"
              else "Inc-nobind"
    } else {
      cell <- if (post_b && pre_b) "Dec-bind"
              else if (!post_b && pre_b) "Dec-becomes-non"
              else "Dec-nobind"
    }
  }
  list(cell_sd = cell)
}, by = .(muni, sd_id)]

# NC munis: muni belongs to an SD with no rate event in panel
nc_pairs <- unique(b[!sd_id %in% cohort_sd$sd_id, .(muni, sd_id)])
cell_dt <- rbind(cell_dt, cbind(nc_pairs, cell_sd = "NC"))
cell_dt <- cell_dt[cell_sd != "Uncertain"]
cat("\nSD cell distribution (muni-SD pair level):\n")
print(cell_dt[, .N, keyby = cell_sd])

# Realized SD dose: cumulative Delta(sd_eit) between c_sd-1 and c_sd+5
rd <- sd_year[sd_id %in% cohort_sd$sd_id]
rd <- merge(rd, cohort_sd, by = "sd_id")
rd_realized <- rd[, {
  pre  <- sd_eit[year == c_sd - 1L]
  yr_max <- min(c_sd + K_stack, max(year, na.rm = TRUE))
  end  <- sd_eit[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose_sd = end_v - pre_v)
}, by = sd_id]
rd_realized[is.na(realized_dose_sd), realized_dose_sd := 0]
rd_realized <- rbind(rd_realized,
                      data.table(sd_id = setdiff(unique(b$sd_id), rd_realized$sd_id),
                                 realized_dose_sd = 0))

# Attach cell, cohort, realized dose to block panel
b <- merge(b, cell_dt, by = c("muni","sd_id"), all.x = TRUE)
b <- merge(b, cohort_sd[, .(sd_id, c_sd)], by = "sd_id", all.x = TRUE)
b <- merge(b, rd_realized, by = "sd_id", all.x = TRUE)
b <- b[!is.na(cell_sd)]

# Build stack on SD cohorts (NC pool = blocks whose sd_id has no event)
build_stack_sd <- function(p, K = K_stack) {
  treated <- unique(p[cell_sd != "NC", .(sd_id, muni, cohort = c_sd, cell = cell_sd)])
  nc_pairs <- unique(p[cell_sd == "NC", .(sd_id, muni)])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_pairs <- treated[cohort == cy]
    if (nrow(t_pairs) == 0) return(NULL)
    d_t <- p[sd_id %in% t_pairs$sd_id & year %in% win]
    d_t <- merge(d_t, unique(t_pairs[, .(sd_id, muni, treat_cell = cell)]),
                  by = c("sd_id","muni"), all.x = TRUE)
    d_t <- d_t[!is.na(treat_cell)]
    d_c <- p[sd_id %in% nc_pairs$sd_id & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }), fill = TRUE)
}

cat("\nBuilding SD-side stacked panel...\n")
stk <- build_stack_sd(b)
stk[, stack_block := paste0(stack, "_", block_geoid)]
stk[, stack_year  := paste0(stack, "_", year)]
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
stk[, rel := as.integer(rel)]
cat(sprintf("Stacked rows: %s; treated blocks: %s; NC blocks: %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell == "NC", block_geoid]), big.mark=",")))

# ---------------------------------------------------------------------------
# Run pooled stacked-continuous (single coefficient per cell) and ES per cell
# ---------------------------------------------------------------------------
# Time-varying SD dose (current sd_eit minus pre-event mean) for the pooled spec
stk[, base_rate_sd := mean(sd_eit[rel < 0], na.rm = TRUE), by = .(stack, sd_id)]
stk[!is.finite(base_rate_sd), base_rate_sd := 0]
stk[, dose_stack_sd := sd_eit - base_rate_sd]

pooled_results <- list()
for (o in outcomes) {
  d <- stk[, c("block_geoid","year","muni","sd_id","stack_block","stack_year",
                "treat_cell","dose_stack_sd","realized_dose_sd","rel", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  cat(sprintf("\n[%s] N=%s rows ... pooled ... ", o, format(nrow(d), big.mark=",")))
  f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack_sd, ref='NC') + ",
                          ctrls_str, " | stack_block + stack_year"))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("treat_cell::", term)]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  pooled_results[[length(pooled_results)+1L]] <- data.table(
    estimator = "stacked_continuous_sd_pooled", outcome = o,
    cell = ic$cell, est = ic$Estimate, se = ic$`Std. Error`,
    p = ic$`Pr(>|t|)`, n = nobs(m))
  cat("done\n")
}
pooled_dt <- rbindlist(pooled_results, fill = TRUE)
print(pooled_dt)
fwrite(pooled_dt, file.path(out_dir, "block_level_results_sd_pooled.csv"))

# Sun-Abraham continuous ES per cell
es_results <- list(); att_results <- list()
for (o in outcomes) {
  d_all <- stk[, c("block_geoid","year","muni","sd_id","stack_block","stack_year",
                    "treat_cell","realized_dose_sd","rel", o, ctrls), with = FALSE]
  d_all <- d_all[complete.cases(d_all)]
  if (nrow(d_all) == 0) next
  cat(sprintf("[%s] ES ... ", o))
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- d_all[treat_cell %in% c("NC", cl)]
    if (nrow(dd) == 0 || sum(dd$treat_cell == cl) == 0) next
    f <- as.formula(paste0(o, " ~ i(rel, realized_dose_sd, ref = -1) + ",
                            ctrls_str, " | stack_block + stack_year"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni + sd_id),
                   error = function(e) { cat(cl, ":", conditionMessage(e), " "); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic
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
fwrite(es_dt,  file.path(out_dir, "block_level_es_continuous_sd.csv"))
fwrite(att_dt, file.path(out_dir, "block_level_att_continuous_sd.csv"))
cat(sprintf("\nSaved SD ES (%d rows) and ATT (%d rows).\n", nrow(es_dt), nrow(att_dt)))

# Figures
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
    labs(title = paste0("SD-side Sun-Abraham continuous ES (block-level): ", o),
         subtitle = "y-axis: per-pp realized SD-dose effect at event-time k",
         x = "Event time (rel)", y = "Coefficient on rel x realized_dose_sd") +
    theme_bw(base_size = 9)
  ggsave(file.path(fig_dir, paste0("block_es_sd_", o, ".png")),
         p, width = 8.5, height = 5, dpi = 300)
}
cat("\n--- SD ATT summary ---\n")
print(att_dt)
cat("\nDone.\n")
