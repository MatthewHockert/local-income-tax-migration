# q3_unified_pooled.R
# Pooled increase/decrease headline: collapses the six binding-state cells
# into two aggregate treatments ("any Inc event", "any Dec event") to
# produce a literature-comparable per-pp dose-response and a binary
# event-indicator analog. Run at the municipality and block estimating
# units; SD-side cell × dose interactions retained from the headline
# spec; controls and fixed effects identical to the headline.
#
# Outputs:
#   data/q3_unified/pooled_results.csv          per-pp + binary, both levels
#   data/q3_unified/pooled_diagnostics.csv      per-outcome N, R², dep mean
#
# Notation:
#   direction $\in$ {NC, Inc, Dec}   for the per-pp spec (interacted with dose)
#   ever_inc_post, ever_dec_post     for the binary spec (post-treatment 0/1)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
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
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_c)
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
# 2. SCHOOL DISTRICT LEVEL
# ---------------------------------------------------------------------------
cat("\n================================================================\n")
cat("School-district-level pooled Inc/Dec\n")
cat("================================================================\n")
e_sd <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e_sd)
stk_sd <- as.data.table(e_sd$stk_sd)
ctrls_sd <- intersect(c("mean_muni_rate", e_sd$sd_share_ctrls), names(stk_sd))
# stk_sd uses cell_sd as its treat_cell field; create direction tag
sd_cell_col <- if ("treat_cell" %in% names(stk_sd)) "treat_cell" else "cell_sd"
stk_sd[, direction := fcase(
  as.character(get(sd_cell_col)) %in% inc_cells, "Inc",
  as.character(get(sd_cell_col)) %in% dec_cells, "Dec",
  as.character(get(sd_cell_col)) == "NC", "NC",
  default = NA_character_)]
stk_sd[, direction := factor(direction, levels = direction_levels)]
stk_sd[, rel := year - stack]
stk_sd[, ever_inc_post := as.integer(direction == "Inc" & rel >= 0)]
stk_sd[, ever_dec_post := as.integer(direction == "Dec" & rel >= 0)]

run_sd_pooled <- function(outcome) {
  cols <- c("sd_id","year","direction","dose_stack",
             "ever_inc_post","ever_dec_post","stack_sd","stack_year",
             outcome, ctrls_sd)
  cols <- intersect(cols, names(stk_sd))
  d <- stk_sd[, ..cols]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(invisible())
  # per-pp pooled
  f_pp <- as.formula(paste0(outcome,
       " ~ i(direction, dose_stack, ref='NC') + ",
       paste(ctrls_sd, collapse = " + "),
       " | stack_sd + stack_year"))
  m_pp <- tryCatch(feols(f_pp, data = d, cluster = ~sd_id),
                    error = function(e) { cat("SD",outcome,"err pp:",conditionMessage(e),"\n"); NULL })
  if (!is.null(m_pp))
    save_result("sd", "per_pp", outcome, m_pp, term_filter = "direction::")
  # binary pooled
  f_bin <- as.formula(paste0(outcome,
       " ~ ever_inc_post + ever_dec_post + ",
       paste(ctrls_sd, collapse = " + "),
       " | stack_sd + stack_year"))
  m_bin <- tryCatch(feols(f_bin, data = d, cluster = ~sd_id),
                     error = function(e) { cat("SD",outcome,"err bin:",conditionMessage(e),"\n"); NULL })
  if (!is.null(m_bin))
    save_result("sd", "binary", outcome, m_bin, term_filter = "ever_inc_post|ever_dec_post")
  if (!is.null(m_pp) && !is.null(m_bin))
    cat(sprintf("[sd %s] N=%s pp:Inc=%.3f Dec=%.3f bin:Inc=%.3f Dec=%.3f\n",
                outcome, format(nrow(d), big.mark=","),
                coef(m_pp)["direction::Inc:dose_stack"],
                coef(m_pp)["direction::Dec:dose_stack"],
                coef(m_bin)["ever_inc_post"],
                coef(m_bin)["ever_dec_post"]))
}
for (o in muni_outcomes) run_sd_pooled(o)

# ---------------------------------------------------------------------------
# 3. BLOCK LEVEL
# ---------------------------------------------------------------------------
cat("\n================================================================\n")
cat("Block-level pooled Inc/Dec\n")
cat("================================================================\n")
e_b <- new.env()
sys.source(file.path(project_dir, "q3_unified_block_joint.R"), envir = e_b)
stk_block <- as.data.table(e_b$stk)
ctrls_block <- e_b$ctrls

# Both rate-setter sides pooled to {NC, Inc, Dec}. Block panel inherits
# treat_cell_muni (muni side) and cell_sd (SD side).
stk_block[, direction_muni := fcase(
  as.character(treat_cell_muni) %in% inc_cells, "Inc",
  as.character(treat_cell_muni) %in% dec_cells, "Dec",
  as.character(treat_cell_muni) == "NC", "NC",
  default = NA_character_)]
stk_block[, direction_sd := fcase(
  as.character(cell_sd) %in% inc_cells, "Inc",
  as.character(cell_sd) %in% dec_cells, "Dec",
  as.character(cell_sd) == "NC", "NC",
  default = NA_character_)]
stk_block[, direction_muni := factor(direction_muni, levels = direction_levels)]
stk_block[, direction_sd   := factor(direction_sd,   levels = direction_levels)]
stk_block[, rel := year - stack]
stk_block[, ever_muni_inc_post := as.integer(direction_muni == "Inc" & rel >= 0)]
stk_block[, ever_muni_dec_post := as.integer(direction_muni == "Dec" & rel >= 0)]
stk_block[, ever_sd_inc_post   := as.integer(direction_sd   == "Inc" & rel >= 0)]
stk_block[, ever_sd_dec_post   := as.integer(direction_sd   == "Dec" & rel >= 0)]
stk_block[is.na(dose_sd), dose_sd := 0]

run_block_pooled <- function(outcome) {
  cols <- c("block_geoid","year","muni","sd_id","stack",
             "direction_muni","dose_stack","direction_sd","dose_sd",
             "ever_muni_inc_post","ever_muni_dec_post",
             "ever_sd_inc_post","ever_sd_dec_post",
             outcome, ctrls_block)
  cols <- intersect(cols, names(stk_block))
  d <- stk_block[, ..cols]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(invisible())
  cat(sprintf("[block %s] N=%s ... ", outcome, format(nrow(d), big.mark=",")))
  # per-pp pooled, both sides pooled to Inc/Dec
  f_pp <- as.formula(paste0(outcome,
       " ~ i(direction_muni, dose_stack, ref='NC')",
       " + i(direction_sd,   dose_sd,    ref='NC') + ",
       paste(ctrls_block, collapse = " + "),
       " | block_geoid^stack + year^stack"))
  m_pp <- feols(f_pp, data = d, cluster = ~muni + sd_id)
  save_result("block", "per_pp", outcome, m_pp,
              term_filter = "direction_(muni|sd)::")
  # binary pooled: 4 post-treatment indicators (muni Inc/Dec + SD Inc/Dec)
  f_bin <- as.formula(paste0(outcome,
       " ~ ever_muni_inc_post + ever_muni_dec_post",
       " + ever_sd_inc_post + ever_sd_dec_post + ",
       paste(ctrls_block, collapse = " + "),
       " | block_geoid^stack + year^stack"))
  m_bin <- feols(f_bin, data = d, cluster = ~muni + sd_id)
  save_result("block", "binary", outcome, m_bin,
              term_filter = "ever_(muni|sd)_(inc|dec)_post")
  cat(sprintf("done.\n  pp: M-Inc=%.3f M-Dec=%.3f SD-Inc=%.3f SD-Dec=%.3f\n  bin: M-Inc=%.3f M-Dec=%.3f SD-Inc=%.3f SD-Dec=%.3f\n",
              coef(m_pp)["direction_muni::Inc:dose_stack"],
              coef(m_pp)["direction_muni::Dec:dose_stack"],
              coef(m_pp)["direction_sd::Inc:dose_sd"],
              coef(m_pp)["direction_sd::Dec:dose_sd"],
              coef(m_bin)["ever_muni_inc_post"],
              coef(m_bin)["ever_muni_dec_post"],
              coef(m_bin)["ever_sd_inc_post"],
              coef(m_bin)["ever_sd_dec_post"]))
}
for (o in block_outcomes) run_block_pooled(o)

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
