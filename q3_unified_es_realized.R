# q3_unified_es_realized.R
# Sun-Abraham continuous event-study using REALIZED dose for the muni-level
# stacked continuous DiD (the headline cousub-level spec from
# q3_unified_full_table.R). Pre-trend leads are estimable because realized
# dose is constant within muni.
#
# Spec, run per cell c (NC + cell c subset):
#   y_{m,t} = sum_{k != -1} beta_k * 1{rel=k} * realized_dose_m
#             + X_{m,t} gamma + alpha_{stack,m} + lambda_{stack,t} + epsilon
#
# Outputs: data/q3_unified/es_realized_coefs.csv
#          data/q3_unified/es_realized_att.csv
#          figures/es_realized_<outcome>.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

# Reuse data prep from the headline script (panel, ctrls, cell labels)
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel    <- e$panel
ctrls    <- e$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_live_and_work","log_in_commuters","log_total_residents",
              "share_live_in","share_work_outside")

# ---------------------------------------------------------------------------
# Compute realized dose per muni: cumulative Delta-tau between c_muni-1 and
# c_muni+5 (or last observed). Constant per muni; 0 for NC.
# ---------------------------------------------------------------------------
mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)])
setorder(mu_year, muni, year)
rd <- mu_year[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  yr_max <- min(c_muni + K_stack, max(year, na.rm = TRUE))
  end <- resident_eit[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose = end_v - pre_v)
}, by = muni]
rd[is.na(realized_dose), realized_dose := 0]
nc_munis <- unique(panel[cell_muni == "NC", muni])
rd <- rbind(rd, data.table(muni = nc_munis, realized_dose = 0))

panel <- merge(panel, rd, by = "muni", all.x = TRUE)
panel[is.na(realized_dose), realized_dose := 0]

# ---------------------------------------------------------------------------
# Build stacked panel (same as headline)
# ---------------------------------------------------------------------------
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
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }), fill = TRUE)
}

cat("Building stacked panel...\n")
stk <- build_stack(panel)
stk[, stack_muni := paste0(stack, "_", muni)]
stk[, stack_year := paste0(stack, "_", year)]
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
cat(sprintf("Stacked rows: %s; treated munis: %s; NC munis: %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell != "NC", muni]), big.mark=","),
            format(uniqueN(stk[treat_cell == "NC", muni]), big.mark=",")))

# ---------------------------------------------------------------------------
# Run ES per (outcome, cell): subset = NC + that cell only
# ---------------------------------------------------------------------------
es_results <- list(); att_results <- list()
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- stk[treat_cell %in% c("NC", cl)]
    dd <- dd[, c("muni","year","stack_muni","stack_year","treat_cell",
                  "realized_dose","rel", o, ctrls), with = FALSE]
    dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0 || sum(dd$treat_cell == cl) == 0) {
      cat(sprintf("  %s skip (no treated rows)\n", cl)); next
    }
    f <- as.formula(paste0(o, " ~ i(rel, realized_dose, ref = -1) + ",
                            ctrls_str, " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni),
                   error = function(ee) {
                     cat(sprintf("  %s error: %s\n", cl, conditionMessage(ee)))
                     NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) next
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, cell := cl]; ic[, n := nobs(m)]
    es_results[[length(es_results)+1L]] <- ic

    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, cell = cl,
      att_post_per_pp = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se     = round(sqrt(mean(post$se^2, na.rm = TRUE) / nrow(post)), 3),
      att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se      = round(sqrt(mean(pre$se^2, na.rm = TRUE) / nrow(pre)), 3),
      n = nobs(m))
    cat(sprintf("  %-16s post=%6.3f (SE %.3f) pre=%6.3f (SE %.3f) N=%s\n",
                cl,
                mean(post$est, na.rm=TRUE),
                sqrt(mean(post$se^2, na.rm=TRUE) / nrow(post)),
                mean(pre$est,  na.rm=TRUE),
                sqrt(mean(pre$se^2,  na.rm=TRUE) / nrow(pre)),
                format(nobs(m), big.mark=",")))
  }
}
es_dt  <- rbindlist(es_results, fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "es_realized_coefs.csv"))
fwrite(att_dt, file.path(out_dir, "es_realized_att.csv"))

# ---------------------------------------------------------------------------
# ES figures
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
  if (nrow(dd) == 0) next
  p <- ggplot(dd, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) +
    geom_line() +
    facet_wrap(~ cell, ncol = 3, scales = "free_y") +
    labs(title = paste0("Sun-Abraham continuous ES (cousub): ", o),
         subtitle = "y-axis: per-pp realized-dose effect at event-time k",
         x = "Event time", y = expression(beta[k])) +
    theme_bw(base_size = 9)
  ggsave(file.path(fig_dir, paste0("es_realized_", o, ".png")),
         p, width = 8.5, height = 5, dpi = 300)
}
cat("\n--- ATT summary ---\n")
print(att_dt)
cat("\nDone.\n")
