# q3_acr_dose_bins_pooledInc.R
# ACR-style dose-response curve pooling Inc-bind + Inc-becomes + Inc-nobind
# (NOT in pipeline / paper). Boosts N from 40 to ~124 munis by including
# all increasing decomposition cells. The x-axis is cumulative Delta-tau at
# k=+5; the y-axis is the within-bin per-pp coefficient on pooled-Inc vs NC.
#
# Note: Inc-nobind is the within-design placebo (statutory increase but bill
# does not move). Pooling it in gives a *statutory* ACR rather than a
# *binding* ACR. The contrast with the Inc-bind-only ACR is informative.
#
# Outputs:
#   data/q3_unified/acr_bins_cousub_pooledInc.csv
#   figures/acr_dose_curve_cousub_pooledInc.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures")

e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
stk_muni <- as.data.table(e$stk_muni)
ctrls    <- e$ctrls

inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
inc_munis <- unique(stk_muni[treat_cell %in% inc_cells, muni])
cat("Pooled Inc-any munis:", length(inc_munis), "\n")

doses <- stk_muni[muni %in% inc_munis & !is.na(c_muni) &
                    year >= c_muni & year <= c_muni + 5,
                  .(total_dose = max(dose_stack, na.rm = TRUE)), by = muni]
doses <- doses[is.finite(total_dose) & total_dose > 0]
cat("With positive dose:", nrow(doses), "\n")
cat("Dose summary (pp):\n"); print(summary(doses$total_dose))

breaks <- c(0.05, 0.15, 0.25, 0.35, 0.50, 0.75, 1.50)
doses[, bin := cut(total_dose, breaks = breaks, include.lowest = TRUE, right = FALSE)]
bin_mids <- (head(breaks, -1) + tail(breaks, -1)) / 2
bin_labels <- levels(doses$bin)
cat("\nBin counts:\n"); print(doses[, .N, by = bin][order(bin)])

ctrls_str <- paste(ctrls, collapse = " + ")
nc_munis  <- unique(stk_muni[treat_cell == "NC", muni])

outcomes <- c("log_live_and_work","log_total_residents","share_live_in")

results <- list()
for (b in bin_labels) {
  munis_b <- doses[bin == b, muni]
  if (length(munis_b) < 2) next
  sub <- stk_muni[muni %in% c(munis_b, nc_munis)]
  sub[, treat_bin := factor(ifelse(muni %in% munis_b, "Inc-any", "NC"),
                              levels = c("NC","Inc-any"))]
  for (o in outcomes) {
    f <- as.formula(paste0(
      o, " ~ i(treat_bin, dose_stack, ref = 'NC') + ",
      ctrls_str, " | stack_muni + stack_year"))
    cols <- intersect(c(o, "dose_stack","treat_bin", ctrls,
                          "muni","stack_muni","stack_year"), names(sub))
    d <- sub[, ..cols]; d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    m <- tryCatch(feols(f, data = d, cluster = ~muni), error = function(e) NULL)
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
              c("est","se","p"), skip_absent = TRUE)
    ic <- ct[grepl("Inc-any", term)]
    if (nrow(ic) == 0) next
    results[[length(results) + 1L]] <- data.table(
      bin = b, bin_mid = bin_mids[match(b, bin_labels)],
      n_munis = length(munis_b),
      outcome = o, est = ic$est, se = ic$se, p = ic$p,
      lo = ic$est - 1.96 * ic$se, hi = ic$est + 1.96 * ic$se,
      n_obs = nobs(m))
  }
}

res <- rbindlist(results)
fwrite(res, file.path(out_dir, "acr_bins_cousub_pooledInc.csv"))
cat("\nResults:\n"); print(res)

res[, outcome_pretty := factor(outcome,
       levels = c("log_live_and_work","log_total_residents","share_live_in"),
       labels = c("log(live-and-work)","log(total residents)","Share live-in-muni"))]

p <- ggplot(res, aes(x = bin_mid, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.02, color = "#1F5673") +
  geom_point(aes(size = n_munis), color = "#1F5673") +
  facet_wrap(~ outcome_pretty, scales = "free_y", ncol = 3) +
  scale_size_continuous(name = "N munis in bin", range = c(2, 6)) +
  scale_x_continuous(breaks = bin_mids) +
  labs(title = "ACR-style dose-response: pooled Inc (bind + becomes + nobind)",
        subtitle = "Per-pp coefficient by cumulative dose at k=+5 (cousub spec, all increasing cells pooled)",
        x = expression("Bin midpoint of cumulative "*Delta*tau^muni*" at k=+5 (pp)"),
        y = expression("Per-pp coefficient "*hat(beta)[b])) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"))

ggsave(file.path(fig_dir, "acr_dose_curve_cousub_pooledInc.png"), p,
        width = 11, height = 4.5, dpi = 300)
cat("\nSaved figures/acr_dose_curve_cousub_pooledInc.png\n")
cat("\nDone.\n")
