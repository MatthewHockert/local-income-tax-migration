# build_feols_es_bycell.R
# feols realized-dose event study, by GROUP (pooled / Inc-bind / Inc-becomes) x 4
# outcomes, in the SAME faceted/3-group layout as figures/dcdh_bycell_es.png. The
# coefficients are rescaled to the LEVEL-AT-MEAN-DOSE scale (beta_k x mean realized
# dose) so this is a direct twin of the dCDH figure: pooled and Inc-becomes should
# track the dCDH paths; Inc-bind shows the feols view (strong) vs the attenuated
# dCDH (continuous estimator can't see the discrete binding rule).
# Reuses the realized-dose + stack construction from q3_unified_headline_es.R.
# Output: figures/feols_es_bycell.png + data/q3_unified/feols_es_bycell.csv

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
.a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.dir <- if (length(.a)) dirname(normalizePath(sub("^--file=", "", .a[1]))) else getwd()
source(file.path(.dir, "config.R"))
out_dir <- file.path(ROOT, "data", "q3_unified"); fig_dir <- file.path(ROOT, "figures")
K <- 5L; WIN <- c(-5L, 5L)   # full stacked event window (matches dCDH at +-5)

e <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel); ctrls <- e$ctrls; cs <- paste(ctrls, collapse = " + ")
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind","Dec-bind","Dec-becomes-non","Dec-nobind")

# realized muni dose (cumulative dtau, c-1 -> c+5)  [from q3_unified_headline_es.R]
mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)]); setorder(mu_year, muni, year)
rd <- mu_year[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]; yr_max <- min(c_muni + K, max(year, na.rm = TRUE))
  end <- resident_eit[year == yr_max]
  list(realized_dose_muni = (if (length(end) == 1) end else NA_real_) -
                            (if (length(pre) == 1) pre else NA_real_))
}, by = muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
rd <- rbind(rd, data.table(muni = unique(panel[cell_muni == "NC", muni]), realized_dose_muni = 0))
panel <- merge(panel, rd, by = "muni", all.x = TRUE); panel[is.na(realized_dose_muni), realized_dose_muni := 0]

build_stack <- function(p, K = 5L) {
  treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni)]); ncm <- unique(p[cell_muni == "NC", muni])
  rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy - K):(cy + K); tm <- treated[cohort == cy]; if (!nrow(tm)) return(NULL)
    dt <- p[muni %in% tm$muni & year %in% win]; dt[, treat_cell_muni := cell_muni]
    dc <- p[muni %in% ncm & year %in% win];     dc[, treat_cell_muni := "NC"]
    d <- rbind(dt, dc, fill = TRUE); d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
}
stk <- build_stack(panel)
stk[, stack_muni := paste0(stack, "_", muni)]; stk[, stack_year := paste0(stack, "_", year)]
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk[, cell_sd := factor(cell_sd, levels = cell_levels)]; stk[, cell_nr := factor(cell_nr, levels = cell_levels)]
stk[is.na(dose_sd), dose_sd := 0]; stk[is.na(dose_nr), dose_nr := 0]

outs <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
grp  <- list(pooled = c("Inc-bind","Inc-becomes","Inc-nobind"), incbind = "Inc-bind", incbecomes = "Inc-becomes")

res <- list()
for (gn in names(grp)) {
  keep <- c("NC", grp[[gn]])
  mean_dose <- stk[treat_cell_muni %in% grp[[gn]] & realized_dose_muni > 0, mean(realized_dose_muni, na.rm = TRUE)]
  for (o in outs) {
    cols <- c("muni","rel","stack_muni","stack_year","treat_cell_muni","realized_dose_muni",
              "cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls)
    dd <- stk[treat_cell_muni %in% keep, ..cols]; dd <- dd[complete.cases(dd)]
    f <- as.formula(paste0(o, " ~ i(rel, realized_dose_muni, ref = -1) + i(cell_sd, dose_sd, ref = 'NC')",
                           " + i(cell_nr, dose_nr, ref = 'NC') + ", cs, " | stack_muni + stack_year"))
    m <- feols(f, dd, cluster = ~muni)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
    ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    res[[paste(gn, o)]] <- ct[, .(group = gn, outcome = o, rel, beta = Estimate, se = `Std. Error`, mean_dose)]
  }
}
R <- rbindlist(res)
R <- rbind(R, unique(R[, .(group, outcome, mean_dose)])[, .(group, outcome, rel = -1L, beta = 0, se = 0, mean_dose)], fill = TRUE)
R[, `:=`(est = beta * mean_dose, lo = (beta - 1.96 * se) * mean_dose, hi = (beta + 1.96 * se) * mean_dose)]
R <- R[rel >= WIN[1] & rel <= WIN[2]]
fwrite(R, file.path(out_dir, "feols_es_bycell.csv"))

lab <- c(log_live_and_work="Log(work in own muni)", log_in_commuters="Log(in-commuters)",
         log_total_jobs="Log(total jobs)", share_live_in="Share live in muni")
glab <- c(pooled="Pooled Increase", incbind="Inc-bind", incbecomes="Inc-becomes")
R[, `:=`(ol = lab[outcome], gl = factor(glab[group], levels = glab))]
g <- ggplot(R, aes(rel, est, color = gl, fill = gl)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line() + geom_point(size = 1) +
  facet_wrap(~ ol, scales = "free_y") +
  scale_color_manual(values = c("Pooled Increase"="#4d4d4d","Inc-bind"="#b2182b","Inc-becomes"="#2166ac"), name = NULL) +
  scale_fill_manual(values  = c("Pooled Increase"="#4d4d4d","Inc-bind"="#b2182b","Inc-becomes"="#2166ac"), name = NULL) +
  labs(title = "feols realized-dose event study by group -- twin of the dCDH figure",
       subtitle = "beta_k x mean realized dose (level-at-mean-dose scale, comparable to dCDH); SD + nonres controls",
       x = "periods since rate change", y = "effect at mean dose") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "feols_es_bycell.png"), g, width = 9, height = 6, dpi = 300)
cat("Saved figures/feols_es_bycell.png + data/q3_unified/feols_es_bycell.csv\n")
print(R[rel %in% c(-2, 3), .(group, outcome, rel, est = round(est, 3))][order(group, outcome, rel)])
