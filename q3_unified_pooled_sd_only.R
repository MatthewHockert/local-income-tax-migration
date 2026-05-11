# q3_unified_pooled_sd_only.R
# Run-after-the-fact helper: estimate the SD-level pooled Inc/Dec
# (per-pp and binary) and append results to data/q3_unified/pooled_results.csv
# and pooled_diagnostics.csv produced by q3_unified_pooled.R.
#
# This exists so that we don't have to re-run the expensive cousub+block
# pooled regressions just to add SD-level results. The cousub+SD+block
# version is permanently embedded in q3_unified_pooled.R for replication.

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")

inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
dec_cells <- c("Dec-bind","Dec-becomes-non","Dec-nobind")
direction_levels <- c("NC","Inc","Dec")

muni_outcomes  <- c("log_live_and_work","log_in_commuters",
                    "log_out_commuters","log_total_jobs",
                    "log_total_residents",
                    "share_live_in","share_work_outside","share_nonres")

cat("Sourcing SD-level setup...\n")
e_sd <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e_sd)
stk_sd <- as.data.table(e_sd$stk_sd)
# Note: mean_muni_rate enters via the muni-aggregate direction-pooled
# treatment below, so we drop it from the linear control vector here.
ctrls_sd <- intersect(e_sd$sd_share_ctrls, names(stk_sd))
sd_cell_col <- {
  if ("treat_cell" %in% names(stk_sd)) "treat_cell"
  else if ("cell_sd" %in% names(stk_sd)) "cell_sd"
  else stop("No SD cell column found in stk_sd")
}

# SD-side direction tag (Inc/Dec/NC) -- the SD's own rate-change direction.
stk_sd[, direction_sd := fcase(
  as.character(get(sd_cell_col)) %in% inc_cells, "Inc",
  as.character(get(sd_cell_col)) %in% dec_cells, "Dec",
  as.character(get(sd_cell_col)) == "NC", "NC",
  default = NA_character_)]
stk_sd[, direction_sd := factor(direction_sd, levels = direction_levels)]
stk_sd[, rel := year - stack]

# Muni-aggregate direction tag at SD level. Construction: each SD's
# within-SD mean of muni resident rates evolves as constituent munis
# change rates. Baseline = mean within the pre-event window for the
# SD's first event year (stack). Cumulative dose = mean_muni_rate -
# baseline. Direction is the sign of the cumulative dose at the end of
# the post-event window (event time +K_stack); NC if dose is identically
# zero across the window.
K_stack <- 5L
stk_sd[, baseline_mean_muni_rate := mean(mean_muni_rate[year < stack],
                                          na.rm = TRUE),
       by = .(sd_id, stack)]
stk_sd[!is.finite(baseline_mean_muni_rate), baseline_mean_muni_rate := 0]
stk_sd[, mean_muni_dose := mean_muni_rate - baseline_mean_muni_rate]
stk_sd[!is.finite(mean_muni_dose), mean_muni_dose := 0]

# Direction at the SD level: look at the SD's cumulative mean_muni_rate
# change at the last post-event year and sign-classify.
sd_dir_muni <- stk_sd[, .(end_dose = {
                       v <- mean_muni_dose[rel == K_stack]
                       if (length(v) == 1 && is.finite(v)) v else 0
                     }),
                     by = .(sd_id, stack)]
sd_dir_muni[, direction_muni_agg := fcase(
  end_dose >  1e-6, "Inc",
  end_dose < -1e-6, "Dec",
  default = "NC")]
stk_sd <- merge(stk_sd, sd_dir_muni[, .(sd_id, stack, direction_muni_agg)],
                 by = c("sd_id","stack"), all.x = TRUE)
stk_sd[, direction_muni_agg := factor(direction_muni_agg, levels = direction_levels)]

# Binary post-treatment indicators for both rate-setters
stk_sd[, ever_sd_inc_post   := as.integer(direction_sd       == "Inc" & rel >= 0)]
stk_sd[, ever_sd_dec_post   := as.integer(direction_sd       == "Dec" & rel >= 0)]
stk_sd[, ever_muni_inc_post := as.integer(direction_muni_agg == "Inc" & rel >= 0)]
stk_sd[, ever_muni_dec_post := as.integer(direction_muni_agg == "Dec" & rel >= 0)]

results <- list(); diagnostics <- list()
save_result <- function(mode, outcome, m, term_filter) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ic <- ct[grepl(term_filter, term)]
  if (nrow(ic) == 0) return(invisible())
  ic[, level := "sd"]; ic[, mode := mode]; ic[, outcome := outcome]
  ic[, n := nobs(m)]
  results[[length(results)+1L]] <<- ic[, .(level, mode, outcome, term,
                                            est, se, p, n)]
  dep_vec <- m$residuals + m$fitted.values
  diagnostics[[length(diagnostics)+1L]] <<- data.table(
    level = "sd", mode = mode, outcome = outcome,
    n = nobs(m),
    r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
    wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
    dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
    dep_sd   = round(sd(dep_vec, na.rm = TRUE), 4))
}

for (outcome in muni_outcomes) {
  cols <- c("sd_id","year",
             "direction_sd","dose_stack",
             "direction_muni_agg","mean_muni_dose",
             "ever_sd_inc_post","ever_sd_dec_post",
             "ever_muni_inc_post","ever_muni_dec_post",
             "stack_sd","stack_year",
             outcome, ctrls_sd)
  cols <- intersect(cols, names(stk_sd))
  d <- stk_sd[, ..cols]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  # Per-pp pooled: both rate-setters direction-pooled
  f_pp <- as.formula(paste0(outcome,
       " ~ i(direction_sd,       dose_stack,     ref='NC')",
       " + i(direction_muni_agg, mean_muni_dose, ref='NC') + ",
       paste(ctrls_sd, collapse = " + "),
       " | stack_sd + stack_year"))
  m_pp <- tryCatch(feols(f_pp, data = d, cluster = ~sd_id),
                    error = function(e) { cat("SD",outcome,"pp err:",conditionMessage(e),"\n"); NULL })
  if (!is.null(m_pp))
    save_result("per_pp", outcome, m_pp,
                "direction_(sd|muni_agg)::")
  # Binary analog: four post-treatment indicators
  f_bin <- as.formula(paste0(outcome,
       " ~ ever_sd_inc_post + ever_sd_dec_post",
       " + ever_muni_inc_post + ever_muni_dec_post + ",
       paste(ctrls_sd, collapse = " + "),
       " | stack_sd + stack_year"))
  m_bin <- tryCatch(feols(f_bin, data = d, cluster = ~sd_id),
                     error = function(e) { cat("SD",outcome,"bin err:",conditionMessage(e),"\n"); NULL })
  if (!is.null(m_bin))
    save_result("binary", outcome, m_bin,
                "ever_(sd|muni)_(inc|dec)_post")
  if (!is.null(m_pp) && !is.null(m_bin)) {
    cat(sprintf("[sd %s] N=%s\n  pp: SD-Inc=%.3f SD-Dec=%.3f Muni-Inc=%.3f Muni-Dec=%.3f\n  bin: SD-Inc=%.3f SD-Dec=%.3f Muni-Inc=%.3f Muni-Dec=%.3f\n",
                outcome, format(nrow(d), big.mark=","),
                coef(m_pp)["direction_sd::Inc:dose_stack"],
                coef(m_pp)["direction_sd::Dec:dose_stack"],
                coef(m_pp)["direction_muni_agg::Inc:mean_muni_dose"],
                coef(m_pp)["direction_muni_agg::Dec:mean_muni_dose"],
                coef(m_bin)["ever_sd_inc_post"],
                coef(m_bin)["ever_sd_dec_post"],
                coef(m_bin)["ever_muni_inc_post"],
                coef(m_bin)["ever_muni_dec_post"]))
  }
}

# Append to existing pooled_results.csv if present
res_dt <- rbindlist(results, fill = TRUE)
res_dt[, side := fcase(
  grepl("direction_sd::",         term), "sd",
  grepl("direction_muni_agg::",   term), "muni",
  grepl("ever_sd_(inc|dec)_post", term), "sd",
  grepl("ever_muni_(inc|dec)_post", term), "muni",
  default = NA_character_)]
res_dt[, direction := fcase(
  grepl("Inc", term, ignore.case = TRUE), "Inc",
  grepl("Dec", term, ignore.case = TRUE), "Dec",
  default = NA_character_)]
diag_dt <- rbindlist(diagnostics, fill = TRUE)

res_path <- file.path(out_dir, "pooled_results.csv")
diag_path <- file.path(out_dir, "pooled_diagnostics.csv")
if (file.exists(res_path)) {
  existing <- fread(res_path)
  existing <- existing[level != "sd"]
  combined <- rbind(existing, res_dt, fill = TRUE)
  fwrite(combined, res_path)
  cat(sprintf("Appended %d SD rows to %s\n", nrow(res_dt), basename(res_path)))
} else {
  fwrite(res_dt, res_path)
}
if (file.exists(diag_path)) {
  existing <- fread(diag_path)
  existing <- existing[level != "sd"]
  combined <- rbind(existing, diag_dt, fill = TRUE)
  fwrite(combined, diag_path)
  cat(sprintf("Appended %d SD diag rows to %s\n", nrow(diag_dt), basename(diag_path)))
} else {
  fwrite(diag_dt, diag_path)
}

cat("\nDone.\n")
