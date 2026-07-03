# q3_unified_dcdh_bycell.R
# de Chaisemartin & D'Haultfoeuille (2024) DIDmultiplegtDYN event study run
# SEPARATELY for the three groups that the headline reports -- the POOLED Increase
# aggregate, the Inc-bind cell, and the Inc-becomes cell -- across all four
# outcomes. The pooled run reproduces q3_unified_dcdh_robustness.R; the two cell
# runs give the binding decomposition its OWN non-negative-weight check, so the
# "single ACRT curve" claim does not rest on the aggregate dCDH alone.
#
# Each group restricts the headline cousub-year panel to {its treated cell + NC}.
# Treatment = resident rate LEVEL (continuous = 1); group = muni; time = year;
# muni fiscal-share controls; cluster = muni. (Like the aggregate dCDH, this nets
# out the muni controls but NOT the SD rate via the cell structure -- it is a
# treatment-level cross-check, not the SD-controlled headline spec.)
#
# Window: effects = 5, placebo = 5. SEs analytical (NBOOT=0; set DCDH_BOOTSTRAP=50).
# Output: data/q3_unified/dcdh_bycell.csv ; figures/dcdh_bycell_es.png

suppressMessages({ library(data.table); library(DIDmultiplegtDYN); library(ggplot2) })
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
out_dir <- file.path(ROOT, "data", "q3_unified"); fig_dir <- file.path(ROOT, "figures")
K_post <- 5; K_pre <- 5
NBOOT  <- as.integer(Sys.getenv("DCDH_BOOTSTRAP", "0"))

e <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel); ctrls <- e$ctrls
panel <- panel[as.character(cell_muni) %in% c("NC","Inc-bind","Inc-becomes","Inc-nobind") &
                 !is.na(resident_eit)]

# Pull the estimates out of the did_multiplegt_dyn object into a tidy table:
# Effects = post-event rows (rel 1..K), Placebos = pre-event rows (rel -1..-K,
# these are the pre-trend test), plus a rel=0 reference row at exactly 0.
# NOTE the scale: these are per-event LEVEL effects at the realized dose,
# NOT per-pp slopes — compare to feols only after multiplying the feols ACR
# by the group's mean dose (~0.33 pp).
extract_dcdh <- function(m) {
  res <- m$results
  eff <- as.data.table(res$Effects)[, .(rel = seq_len(.N), est = Estimate, se = SE,
                                         ci_lo = `LB CI`, ci_hi = `UB CI`)]
  pl <- if (!is.null(res$Placebos) && res$N_Placebos > 0)
          as.data.table(res$Placebos)[, .(rel = -seq_len(.N), est = Estimate, se = SE,
                                           ci_lo = `LB CI`, ci_hi = `UB CI`)] else NULL
  ref <- data.table(rel = 0L, est = 0, se = 0, ci_lo = 0, ci_hi = 0)
  rbindlist(list(pl, ref, eff), fill = TRUE)[order(rel)]
}

groups <- list(
  pooled     = c("NC","Inc-bind","Inc-becomes","Inc-nobind"),
  incbind    = c("NC","Inc-bind"),
  incbecomes = c("NC","Inc-becomes"))
outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")

all <- list()
for (gn in names(groups)) {
  pg <- panel[as.character(cell_muni) %in% groups[[gn]]]
  pg[, muni_int := as.integer(factor(muni))]
  for (o in outcomes) {
    cols <- c("muni_int","year","resident_eit", o, ctrls)
    d <- pg[, ..cols][complete.cases(pg[, ..cols])]; if (!nrow(d)) next
    setnames(d, o, "Y")
    cat(sprintf("[%s | %s] N=%s munis=%s ... ", gn, o, format(nrow(d), big.mark=","),
                uniqueN(d$muni_int))); flush.console()
    args <- list(df = as.data.frame(d), outcome = "Y", group = "muni_int", time = "year",
                 treatment = "resident_eit", effects = K_post, placebo = K_pre,
                 continuous = 1, controls = ctrls, cluster = "muni_int", graph_off = TRUE)
    if (NBOOT > 0) args$bootstrap <- NBOOT
    m <- tryCatch(do.call(did_multiplegt_dyn, args),
                  error = function(err) { cat("ERROR:", conditionMessage(err), "\n"); NULL })
    if (is.null(m)) next
    co <- extract_dcdh(m); co[, `:=`(group = gn, outcome = o)]
    pj <- tryCatch(m$results$p_jointplacebo, error = function(e) NA_real_)
    co[, joint_placebo_p := if (is.null(pj)) NA_real_ else pj]
    co[, ate := tryCatch(m$results$ATE$Estimate[1], error = function(e) NA_real_)]
    co[, se_type := if (NBOOT > 0) sprintf("bootstrap(%d)", NBOOT) else "analytical"]
    all[[paste(gn, o)]] <- co
    cat(sprintf("done; effect@+%d = %+.3f\n", max(co$rel), co[rel == max(rel), est]))
  }
}
res <- rbindlist(all, fill = TRUE)
fwrite(res, file.path(out_dir, "dcdh_bycell.csv"))

lab <- c(log_live_and_work="Log(work in own muni)", log_in_commuters="Log(in-commuters)",
         log_total_jobs="Log(total jobs)", share_live_in="Share live in muni")
glab <- c(pooled="Pooled Increase", incbind="Inc-bind", incbecomes="Inc-becomes")
res[, `:=`(ol = lab[outcome], gl = factor(glab[group], levels = glab))]
g <- ggplot(res, aes(rel, est, color = gl, fill = gl)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
  geom_line() + geom_point(size = 1) +
  facet_wrap(~ ol, scales = "free_y") +
  scale_color_manual(values = c("Pooled Increase"="#4d4d4d","Inc-bind"="#b2182b","Inc-becomes"="#2166ac"), name = NULL) +
  scale_fill_manual(values = c("Pooled Increase"="#4d4d4d","Inc-bind"="#b2182b","Inc-becomes"="#2166ac"), name = NULL) +
  labs(title = "dCDH event study by group -- non-negative-weight check on the decomposition",
       subtitle = "continuous treatment (resident rate level); pooled vs Inc-bind vs Inc-becomes; same headline sample",
       x = "periods since rate change", y = "estimated effect (per unit of rate)") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "dcdh_bycell_es.png"), g, width = 9, height = 6, dpi = 300)
cat("\nSaved data/q3_unified/dcdh_bycell.csv + figures/dcdh_bycell_es.png\n")
print(res[, .(group, outcome, rel, est = round(est,3), se = round(se,3))][order(group, outcome, rel)])
