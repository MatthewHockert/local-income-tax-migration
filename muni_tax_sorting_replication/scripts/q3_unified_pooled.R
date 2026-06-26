# q3_unified_pooled.R
# Pooled increase/decrease headline: collapses the six binding-state cells
# into two aggregate treatments ("any Inc event", "any Dec event") to
# produce a literature-comparable per-pp dose-response and a binary
# event-indicator analog. SD-side cell × dose interactions retained from
# the headline spec; controls and fixed effects identical to the headline.
#
# COUSUB-ONLY (repo-cleanup, 2026-05): the SD-unit and block-unit pooled
# regressions were trimmed out when those estimators were retired. The kept
# papers (UEA_draft.tex, muni_tax_sorting.tex) report only the cousub
# pooled benchmark. The cousub block is unchanged and produces
# byte-identical cousub rows in pooled_results.csv. Prior 3-level version
# is in git history.
#
# Outputs:
#   data/q3_unified/pooled_results.csv          per-pp + binary, cousub
#   data/q3_unified/pooled_diagnostics.csv      per-outcome N, R², dep mean
#
# Notation:
#   direction $\in$ {NC, Inc, Dec}   for the per-pp spec (interacted with dose)
#   ever_inc_post, ever_dec_post     for the binary spec (post-treatment 0/1)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
K_stack <- 5L

inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
dec_cells <- c("Dec-bind","Dec-becomes-non","Dec-nobind")

direction_levels <- c("NC","Inc","Dec")

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
sd_ctrls   <- c("share_state_rev_sd","share_local_rev_sd",
                "share_federal_rev_sd","share_property_tax_sd",
                "share_instructional_sd","share_support_sd",
                "share_capital_sd","share_debt_int_sd")

muni_outcomes  <- c("log_live_and_work","log_in_commuters",
                    "log_out_commuters","log_total_jobs",
                    "log_total_residents",
                    "share_live_in","share_work_outside","share_nonres")
block_outcomes <- c("log_home_count","log_work_count",
                    "home_in_muni_share","share_work_outside")

# Collect results across all (level, mode, outcome) combinations
results <- list()
diagnostics <- list()
save_result <- function(level_tag, mode, outcome, m, term_filter = "direction::|ever_inc_post|ever_dec_post") {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"), skip_absent = TRUE)
  ic <- ct[grepl(term_filter, term)]
  if (nrow(ic) == 0) return(invisible())
  ic[, level := level_tag]
  ic[, mode  := mode]
  ic[, outcome := outcome]
  ic[, n := nobs(m)]
  results[[length(results)+1L]] <<- ic[, .(level, mode, outcome, term,
                                            est, se, p, n)]
  dep_vec <- m$residuals + m$fitted.values
  diagnostics[[length(diagnostics)+1L]] <<- data.table(
    level = level_tag, mode = mode, outcome = outcome,
    n = nobs(m),
    r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
    wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
    dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
    dep_sd   = round(sd(dep_vec, na.rm = TRUE), 4))
  invisible()
}

# ---------------------------------------------------------------------------
# 1. MUNICIPALITY LEVEL
# ---------------------------------------------------------------------------
cat("================================================================\n")
cat("Municipality-level pooled Inc/Dec\n")
cat("================================================================\n")
e_c <- new.env()
sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e_c)
stk_muni <- as.data.table(e_c$stk_muni)
ctrls_muni <- e_c$ctrls

# Direction tags pooled per rate-setter side: muni side (from treat_cell)
# and SD side (from cell_sd). Each side is collapsed to {NC, Inc, Dec}.
# Pooled per-pp regression is:
#   y = FE + i(direction_muni, dose_muni) + i(direction_sd, dose_sd) + X
# producing four coefficients of interest: muni-Inc, muni-Dec, SD-Inc, SD-Dec.
stk_muni[, direction_muni := fcase(
  as.character(treat_cell) %in% inc_cells, "Inc",
  as.character(treat_cell) %in% dec_cells, "Dec",
  as.character(treat_cell) == "NC", "NC",
  default = NA_character_)]
stk_muni[, direction_sd := fcase(
  as.character(cell_sd) %in% inc_cells, "Inc",
  as.character(cell_sd) %in% dec_cells, "Dec",
  as.character(cell_sd) == "NC", "NC",
  default = NA_character_)]
stk_muni[, direction_muni := factor(direction_muni, levels = direction_levels)]
stk_muni[, direction_sd   := factor(direction_sd,   levels = direction_levels)]
# Binary post-treatment indicators per side
stk_muni[, rel := year - stack]
stk_muni[, ever_muni_inc_post := as.integer(direction_muni == "Inc" & rel >= 0)]
stk_muni[, ever_muni_dec_post := as.integer(direction_muni == "Dec" & rel >= 0)]
stk_muni[, ever_sd_inc_post   := as.integer(direction_sd   == "Inc" & rel >= 0)]
stk_muni[, ever_sd_dec_post   := as.integer(direction_sd   == "Dec" & rel >= 0)]

run_muni_pooled <- function(outcome) {
  cols <- c("muni","year",
             "direction_muni","dose_stack","direction_sd","dose_sd",
             "ever_muni_inc_post","ever_muni_dec_post",
             "ever_sd_inc_post","ever_sd_dec_post",
             "stack_muni","stack_year",
             outcome, ctrls_muni)
  cols <- intersect(cols, names(stk_muni))
  d <- stk_muni[, ..cols]
  if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(invisible())
  # per-pp pooled: both sides pooled to Inc/Dec/NC
  f_pp <- as.formula(paste0(outcome,
       " ~ i(direction_muni, dose_stack, ref='NC')",
       " + i(direction_sd,   dose_sd,    ref='NC') + ",
       paste(ctrls_muni, collapse = " + "),
       " | stack_muni + stack_year"))
  m_pp <- feols(f_pp, data = d, cluster = ~muni)
  save_result("cousub", "per_pp", outcome, m_pp,
              term_filter = "direction_(muni|sd)::")
  # binary pooled: 4 post-treatment indicators
  f_bin <- as.formula(paste0(outcome,
       " ~ ever_muni_inc_post + ever_muni_dec_post",
       " + ever_sd_inc_post + ever_sd_dec_post + ",
       paste(ctrls_muni, collapse = " + "),
       " | stack_muni + stack_year"))
  m_bin <- feols(f_bin, data = d, cluster = ~muni)
  save_result("cousub", "binary", outcome, m_bin,
              term_filter = "ever_(muni|sd)_(inc|dec)_post")
  cat(sprintf("[cousub %s] N=%s\n  pp: M-Inc=%.3f M-Dec=%.3f SD-Inc=%.3f SD-Dec=%.3f\n  bin: M-Inc=%.3f M-Dec=%.3f SD-Inc=%.3f SD-Dec=%.3f\n",
              outcome, format(nrow(d), big.mark=","),
              coef(m_pp)["direction_muni::Inc:dose_stack"],
              coef(m_pp)["direction_muni::Dec:dose_stack"],
              coef(m_pp)["direction_sd::Inc:dose_sd"],
              coef(m_pp)["direction_sd::Dec:dose_sd"],
              coef(m_bin)["ever_muni_inc_post"],
              coef(m_bin)["ever_muni_dec_post"],
              coef(m_bin)["ever_sd_inc_post"],
              coef(m_bin)["ever_sd_dec_post"]))
}
for (o in muni_outcomes) run_muni_pooled(o)


# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
res_dt <- rbindlist(results, fill = TRUE)
# Extract clean "side" and "direction" columns from the term names.
# Each pooled regression contains up to four coefficients of interest:
#   muni-Inc, muni-Dec, SD-Inc, SD-Dec
res_dt[, side := fcase(
  grepl("direction_muni::",       term), "muni",
  grepl("direction_sd::",         term), "sd",
  grepl("ever_muni_(inc|dec)_post",term), "muni",
  grepl("ever_sd_(inc|dec)_post",  term), "sd",
  # SD-only level (no muni side in regression): the lone "direction::"
  grepl("direction::",            term), "sd",
  grepl("ever_inc_post|ever_dec_post", term), "sd",
  default = NA_character_)]
res_dt[, direction := fcase(
  grepl("Inc",   term, ignore.case = TRUE), "Inc",
  grepl("Dec",   term, ignore.case = TRUE), "Dec",
  default = NA_character_)]
fwrite(res_dt, file.path(out_dir, "pooled_results.csv"))
fwrite(rbindlist(diagnostics, fill = TRUE),
       file.path(out_dir, "pooled_diagnostics.csv"))

cat("\n--- Pooled Inc/Dec summary ---\n")
print(res_dt[, .(level, mode, outcome, direction,
                  est = round(est, 3), se = round(se, 3),
                  p = round(p, 4), n = format(n, big.mark = ","))])
cat("\nSaved data/q3_unified/pooled_results.csv\n")
cat("Saved data/q3_unified/pooled_diagnostics.csv\n")
