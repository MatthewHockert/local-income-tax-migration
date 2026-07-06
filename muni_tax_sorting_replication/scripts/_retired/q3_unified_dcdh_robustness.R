# q3_unified_dcdh_robustness.R
# de Chaisemartin & D'Haultfoeuille (2024) DIDmultiplegtDYN as a robustness
# cross-check to the linear stacked-feols dose-response. The linear feols beta
# is an ACR only if the dose-response is ~linear; if the marginal effect bends
# with dose size, beta weights heterogeneous dose-specific responses with
# possibly-perverse weights. did_multiplegt_dyn re-estimates the dose-specific
# pieces and aggregates them with NON-NEGATIVE weights, so a matching event-
# study path validates the linear aggregation.
#
# SAME SAMPLE as the headline: the cousub-year panel from q3_unified_full_table.R
# (never-changers + the three Inc cells; Dec and flip-floppers/Uncertain already
# dropped; same six muni fiscal-share controls). Treatment = resident rate LEVEL
# (continuous = 1), group = muni, time = year, cluster = muni.
#
# Window: effects = 3, placebo = 2 -- the longest horizon the continuous +
# controlled estimator supports on this sample (longer errors out).
#
# SEs: analytical by default (NBOOT = 0). The package notes analytical SEs can
# be LIBERAL with the continuous option; set NBOOT >= 50 for the recommended
# (but slow, ~3 min/outcome) bootstrap. Point estimates are identical either way.
#
# Output: data/q3_unified/dcdh_robustness.csv ; figures/dcdh_vs_feols_es.png

suppressMessages({ library(data.table); library(DIDmultiplegtDYN); library(ggplot2) })
# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
data_dir <- file.path(project_dir, "data"); out_dir <- file.path(data_dir, "q3_unified")
fig_dir  <- file.path(project_dir, "figures")
# NB: did_multiplegt_dyn's effects/placebo must be DOUBLE, not integer (3L
# throws "Positive integer required" -- a package quirk).
K_post <- 3; K_pre <- 2
NBOOT  <- as.integer(Sys.getenv("DCDH_BOOTSTRAP", "0"))   # set DCDH_BOOTSTRAP=50 to bootstrap

e <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel); ctrls <- e$ctrls
panel <- panel[as.character(cell_muni) %in% c("NC","Inc-bind","Inc-becomes","Inc-nobind") &
                 !is.na(resident_eit)]
panel[, muni_int := as.integer(factor(muni))]

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

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
all <- list()
for (o in outcomes) {
  # only the columns the estimator needs (an extra character id column makes
  # did_multiplegt_dyn mis-read the effects option)
  cols <- c("muni_int","year","resident_eit", o, ctrls)
  d <- panel[, ..cols][complete.cases(panel[, ..cols])]; if (!nrow(d)) next
  setnames(d, o, "Y")
  cat(sprintf("[%s] N=%s munis=%s ... ", o, format(nrow(d), big.mark=","),
              uniqueN(d$muni_int))); flush.console()
  args <- list(df = as.data.frame(d), outcome = "Y", group = "muni_int", time = "year",
               treatment = "resident_eit", effects = K_post, placebo = K_pre,
               continuous = 1, controls = ctrls, cluster = "muni_int", graph_off = TRUE)
  if (NBOOT > 0) args$bootstrap <- NBOOT
  m <- tryCatch(do.call(did_multiplegt_dyn, args),
                error = function(err) { cat("ERROR:", conditionMessage(err), "\n"); NULL })
  if (is.null(m)) next
  co <- extract_dcdh(m); co[, outcome := o]
  pj <- tryCatch(m$results$p_jointplacebo, error = function(e) NA_real_)
  co[, joint_placebo_p := if (is.null(pj)) NA_real_ else pj]
  co[, se_type := if (NBOOT > 0) sprintf("bootstrap(%d)", NBOOT) else "analytical"]
  all[[o]] <- co
  cat(sprintf("done; effect@+%d = %+.3f\n", max(co$rel), co[rel == max(rel), est]))
}
res <- rbindlist(all, fill = TRUE)
fwrite(res, file.path(out_dir, "dcdh_robustness.csv"))

lab <- c(log_live_and_work="Log(work in own muni)", log_in_commuters="Log(in-commuters)",
         log_total_jobs="Log(total jobs)", share_live_in="Share live in muni")
res[, ol := lab[outcome]]
g <- ggplot(res, aes(rel, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#2166ac") +
  geom_line(color = "#2166ac") + geom_point(color = "#2166ac", size = 1) +
  facet_wrap(~ ol, scales = "free_y") +
  labs(title = "dCDH (de Chaisemartin-D'Haultfoeuille) event study -- robustness to linear feols",
       subtitle = "continuous treatment (resident rate level), non-negative weights; same headline sample",
       x = "periods since rate change", y = "estimated effect (per unit of rate)") +
  theme_bw(base_size = 10)
ggsave(file.path(fig_dir, "dcdh_vs_feols_es.png"), g, width = 9, height = 6, dpi = 300)
cat("\nSaved data/q3_unified/dcdh_robustness.csv + figures/dcdh_vs_feols_es.png\n")
print(res[, .(outcome, rel, est = round(est,3), se = round(se,3))][order(outcome, rel)])
