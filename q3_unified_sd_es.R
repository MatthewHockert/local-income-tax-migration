# q3_unified_sd_es.R
# School-district-level realized-dose Sun-Abraham continuous event study,
# parallel to q3_unified_headline_es.R (cousub-level) and
# q3_unified_block_es_full.R (block-level).
#
# Spec (per SD-side cell c, on subset {NC + cell c}):
#   y_{st} = alpha_{stack,SD} + gamma_{stack,year}
#          + sum_{k != -1} beta^{SD}_k * 1{rel = k} * realized_dose_sd_s
#          + within-SD aggregate of muni rates (control)
#          + SD F-33 fiscal share controls
#          + epsilon
# Cluster: ~ sd_id
#
# Outputs:
#   data/q3_unified/sd_es_coefs.csv
#   data/q3_unified/sd_es_att.csv
#   figures/sd_es_<outcome>.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

# Reuse SD-level data prep from q3_unified_sd_level.R via sourcing.
# That script already builds `sd_panel` with cell labels and `stk_sd` with
# stacks for the SD side under the new SD-controls spec.
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e)

# Pull out the panel and stacked panel + control set from that environment.
sd_panel <- e$sd_panel
stk_sd   <- e$stk_sd
sd_share_ctrls <- e$sd_share_ctrls

# Compute realized SD dose: cumulative Delta tau between c_sd-1 and c_sd+5
mu_year <- unique(sd_panel[, .(sd_id, year, sd_eit_rate, c_sd)])
setorder(mu_year, sd_id, year)
rd <- mu_year[!is.na(c_sd), {
  pre <- sd_eit_rate[year == c_sd - 1L]
  yr_max <- min(c_sd + K_stack, max(year, na.rm = TRUE))
  end <- sd_eit_rate[year == yr_max]
  pre_v <- if (length(pre) == 1) pre else NA_real_
  end_v <- if (length(end) == 1) end else NA_real_
  list(realized_dose_sd = end_v - pre_v)
}, by = sd_id]
rd[is.na(realized_dose_sd), realized_dose_sd := 0]
nc_sds <- unique(sd_panel[cell_sd == "NC", sd_id])
rd <- rbind(rd, data.table(sd_id = nc_sds, realized_dose_sd = 0))

stk_sd <- merge(stk_sd, rd, by = "sd_id", all.x = TRUE)
stk_sd[is.na(realized_dose_sd), realized_dose_sd := 0]
stk_sd[, rel := as.integer(year - stack)]

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
# stk_sd may use cell_sd or treat_cell — alias for compatibility
if (!"treat_cell" %in% names(stk_sd) && "cell_sd" %in% names(stk_sd)) {
  stk_sd[, treat_cell := cell_sd]
}
stk_sd[, treat_cell := factor(treat_cell, levels = cell_levels)]

outcomes <- c("log_live_and_work","log_in_commuters","log_total_residents",
              "share_live_in","share_work_outside")

# Controls: within-SD aggregate of muni rates + SD F-33 shares
ctrls_es <- c("mean_muni_rate", sd_share_ctrls)
ctrls_es <- intersect(ctrls_es, names(stk_sd))
ctrls_str <- paste(ctrls_es, collapse = " + ")

es_results <- list(); att_results <- list()
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  for (cl in setdiff(cell_levels, "NC")) {
    dd <- stk_sd[treat_cell %in% c("NC", cl)]
    cols <- c("sd_id","year","stack_sd","stack_year","treat_cell",
               "realized_dose_sd","rel", o, ctrls_es)
    cols <- intersect(cols, names(dd))
    dd <- dd[, ..cols]
    dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0 || sum(dd$treat_cell == cl) == 0) {
      cat(sprintf("  %s skip\n", cl)); next
    }
    f <- as.formula(paste0(o, " ~ i(rel, realized_dose_sd, ref = -1) + ",
                            ctrls_str, " | stack_sd + stack_year"))
    m <- tryCatch(feols(f, data = dd, cluster = ~sd_id),
                   error = function(ee) {
                     cat(sprintf("  %s error: %s\n", cl, conditionMessage(ee))); NULL })
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
      att_post_se     = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre_per_pp  = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se      = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      n = nobs(m))
    cat(sprintf("  %-16s post=%6.3f (SE %.3f)  pre=%6.3f (SE %.3f)  N=%s\n",
                cl, mean(post$est, na.rm=TRUE),
                sqrt(mean(post$se^2, na.rm=TRUE) / max(nrow(post),1)),
                mean(pre$est, na.rm=TRUE),
                sqrt(mean(pre$se^2, na.rm=TRUE) / max(nrow(pre),1)),
                format(nobs(m), big.mark=",")))
  }
}
es_dt  <- rbindlist(es_results, fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "sd_es_coefs.csv"))
fwrite(att_dt, file.path(out_dir, "sd_es_att.csv"))

# ES figures
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
    labs(title = paste0("SD-level Sun-Abraham ES (realized dose): ", o),
         subtitle = "y-axis: per-pp realized SD-dose effect at event-time k",
         x = "Event time", y = expression(beta[k])) +
    theme_bw(base_size = 9)
  ggsave(file.path(fig_dir, paste0("sd_es_", o, ".png")),
         p, width = 8.5, height = 5, dpi = 300)
}
cat("\n--- SD-level ATT summary ---\n")
print(att_dt)
