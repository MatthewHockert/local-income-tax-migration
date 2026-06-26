# q3_unified_levels_robustness.R
# ---------------------------------------------------------------------------
# LEVELS robustness: re-run the three muni-level estimators using the rate
# LEVEL (tau) in place of the cumulative change (dose, Delta-tau).
#
#   (1) Aggregate / pooled-Inc benchmark
#   (2) Binding decomposition (Inc-bind / Inc-becomes / Inc-nobind)
#   (3) Realized event-study pre-trend diagnostic
#
# For (1) and (2) the level coefficient must coincide with the dose
# coefficient: dose_stack = resident_eit - baseline_muni, and baseline_muni
# is constant within muni x stack, hence absorbed by the stack_muni FE.
# Running both side by side is the proof.
#
# For (3) levels genuinely differ: the magnitude tag is interacted with
# event-time, so the baseline-times-event-time term is NOT absorbed.
#   dose ES   : i(rel, realized_dose,  ref=-1)   realized_dose  = tau(c+5) - tau(c-1)
#   level ES  : i(rel, realized_level, ref=-1)   realized_level = tau(c+5)
#
# Output: data/q3_unified/levels_robustness_{pooled,decomp,es}.csv
# ---------------------------------------------------------------------------

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
K_stack <- 5L

inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
dec_cells <- c("Dec-bind","Dec-becomes-non","Dec-nobind")

# ---------------------------------------------------------------------------
# Reuse the canonical panel + stacked panel from the headline build.
# This gives stk_muni with: treat_cell, cell_sd, dose_stack, dose_sd,
# resident_eit, sd_eit, c_muni, stack_muni, stack_year, stack, ctrls.
# ---------------------------------------------------------------------------
e_c <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_c)
stk <- as.data.table(e_c$stk_muni)
ctrls <- e_c$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

stk[, rel := year - stack]
stk[is.na(dose_sd), dose_sd := 0]
stk[, treat_cell := factor(as.character(treat_cell),
      levels = c("NC","Inc-bind","Inc-becomes","Inc-nobind"))]

# direction tag (pooled): collapse cells -> {NC, Inc}
stk[, direction_muni := fcase(
  as.character(treat_cell) %in% inc_cells, "Inc",
  as.character(treat_cell) == "NC", "NC", default = NA_character_)]
stk[, direction_sd := fcase(
  as.character(cell_sd) %in% inc_cells, "Inc",
  as.character(cell_sd) == "NC", "NC", default = NA_character_)]
stk[, direction_muni := factor(direction_muni, levels = c("NC","Inc"))]
stk[, direction_sd   := factor(direction_sd,   levels = c("NC","Inc"))]

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs",
              "share_live_in")

extract <- function(m, pattern) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct[grepl(pattern, term), .(term, est, se, p, n = nobs(m))]
}

# ===========================================================================
# (1) AGGREGATE / POOLED  -- level vs dose
# ===========================================================================
cat("\n================ (1) POOLED AGGREGATE : level vs dose ================\n")
pooled_rows <- list()
for (o in outcomes) {
  cols <- c("muni","direction_muni","direction_sd","dose_stack","dose_sd",
            "resident_eit","sd_eit","stack_muni","stack_year", o, ctrls)
  d <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (nrow(d) == 0) next
  f_dose <- as.formula(paste0(o,
    " ~ i(direction_muni, dose_stack, ref='NC') + i(direction_sd, dose_sd, ref='NC') + ",
    ctrls_str, " | stack_muni + stack_year"))
  f_lvl <- as.formula(paste0(o,
    " ~ i(direction_muni, resident_eit, ref='NC') + i(direction_sd, sd_eit, ref='NC') + ",
    ctrls_str, " | stack_muni + stack_year"))
  m_dose <- feols(f_dose, data = d, cluster = ~muni)
  m_lvl  <- feols(f_lvl,  data = d, cluster = ~muni)
  rd <- extract(m_dose, "direction_muni::Inc"); rd[, c("outcome","spec") := .(o,"dose")]
  rl <- extract(m_lvl,  "direction_muni::Inc"); rl[, c("outcome","spec") := .(o,"level")]
  pooled_rows[[length(pooled_rows)+1L]] <- rbind(rd, rl)
  cat(sprintf("%-20s muni-Inc  dose=%+.4f (se %.4f)   level=%+.4f (se %.4f)\n",
              o, rd$est[1], rd$se[1], rl$est[1], rl$se[1]))
}
pooled_dt <- rbindlist(pooled_rows, fill = TRUE)
fwrite(pooled_dt, file.path(out_dir, "levels_robustness_pooled.csv"))

# ===========================================================================
# (2) BINDING DECOMPOSITION  -- level vs dose
# ===========================================================================
cat("\n================ (2) BINDING DECOMPOSITION : level vs dose ================\n")
decomp_rows <- list()
for (o in outcomes) {
  cols <- c("muni","treat_cell","cell_sd","dose_stack","dose_sd",
            "resident_eit","sd_eit","stack_muni","stack_year", o, ctrls)
  d <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (nrow(d) == 0) next
  f_dose <- as.formula(paste0(o,
    " ~ i(treat_cell, dose_stack, ref='NC') + i(cell_sd, dose_sd, ref='NC') + ",
    ctrls_str, " | stack_muni + stack_year"))
  f_lvl <- as.formula(paste0(o,
    " ~ i(treat_cell, resident_eit, ref='NC') + i(cell_sd, sd_eit, ref='NC') + ",
    ctrls_str, " | stack_muni + stack_year"))
  m_dose <- feols(f_dose, data = d, cluster = ~muni)
  m_lvl  <- feols(f_lvl,  data = d, cluster = ~muni)
  rd <- extract(m_dose, "treat_cell::Inc"); rd[, c("outcome","spec") := .(o,"dose")]
  rl <- extract(m_lvl,  "treat_cell::Inc"); rl[, c("outcome","spec") := .(o,"level")]
  decomp_rows[[length(decomp_rows)+1L]] <- rbind(rd, rl)
  cat(sprintf("\n[%s]\n", o))
  for (cell in inc_cells) {
    de <- rd[grepl(cell, term)]; le <- rl[grepl(cell, term)]
    if (nrow(de) && nrow(le))
      cat(sprintf("  %-12s dose=%+.4f (se %.4f)   level=%+.4f (se %.4f)\n",
                  cell, de$est[1], de$se[1], le$est[1], le$se[1]))
  }
}
decomp_dt <- rbindlist(decomp_rows, fill = TRUE)
fwrite(decomp_dt, file.path(out_dir, "levels_robustness_decomp.csv"))

# ===========================================================================
# (3) REALIZED EVENT-STUDY  -- realized LEVEL vs realized DOSE
# ===========================================================================
cat("\n================ (3) REALIZED ES : level tag vs dose tag ================\n")
# realized_level = tau at min(c+K, last year); realized_dose = that minus pre.
ry <- unique(stk[, .(muni, year, resident_eit, c_muni)])
setorder(ry, muni, year)
rd_tag <- ry[, {
  if (all(is.na(c_muni))) {
    list(realized_level = mean(resident_eit, na.rm = TRUE), realized_dose = 0)
  } else {
    cc  <- c_muni[1]
    pre <- resident_eit[year == cc - 1L]
    ymx <- min(cc + K_stack, max(year, na.rm = TRUE))
    end <- resident_eit[year == ymx]
    pv <- if (length(pre) == 1) pre else NA_real_
    ev <- if (length(end) == 1) end else NA_real_
    list(realized_level = ev, realized_dose = ev - pv)
  }
}, by = muni]
rd_tag[is.na(realized_level), realized_level := 0]
rd_tag[is.na(realized_dose),  realized_dose  := 0]
stk <- merge(stk, rd_tag, by = "muni", all.x = TRUE)
stk[is.na(realized_level), realized_level := 0]
stk[is.na(realized_dose),  realized_dose  := 0]

es_rows <- list()
run_es <- function(o, cell, tag_var) {
  d <- stk[treat_cell %in% c("NC", cell)]
  cols <- c("muni","rel","stack_muni","stack_year", tag_var,
            "cell_sd","dose_sd", o, ctrls)
  d <- d[, ..cols][complete.cases(d[, ..cols])]
  if (nrow(d) == 0 || sum(d[[tag_var]] != 0) == 0) return(NULL)
  f <- as.formula(paste0(o,
       " ~ i(rel, ", tag_var, ", ref = -1) + i(cell_sd, dose_sd, ref='NC') + ",
       ctrls_str, " | stack_muni + stack_year"))
  m <- tryCatch(feols(f, data = d, cluster = ~muni), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("^rel::", term)]
  ct[, k := as.integer(gsub(".*rel::(-?[0-9]+):.*", "\\1", term))]
  # pre-trend joint Wald on leads (k <= -2)
  pre_terms <- ct[k <= -2, term]
  wp <- tryCatch(wald(m, paste0("rel::", "(", paste(ct[k <= -2, k], collapse="|"), "):"),
                      print = FALSE)$p, error = function(e) NA_real_)
  post <- ct[k >= 0, .(post_mean = mean(est))]
  cat(sprintf("  %-18s %-11s [%s]  pre(k=-5..-2) Wald p=%s | post mean=%+.4f\n",
              o, cell, tag_var, ifelse(is.na(wp),"NA",sprintf("%.3f",wp)),
              post$post_mean))
  ct[, c("outcome","cell","tag") := .(o, cell, tag_var)]
  ct[, wald_pre_p := wp]
  ct[]
}
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  for (cell in c("Inc-bind","Inc-nobind")) {
    for (tag in c("realized_dose","realized_level")) {
      r <- run_es(o, cell, tag)
      if (!is.null(r)) es_rows[[length(es_rows)+1L]] <- r
    }
  }
}
es_dt <- rbindlist(es_rows, fill = TRUE)
fwrite(es_dt, file.path(out_dir, "levels_robustness_es.csv"))

cat("\nSaved:\n")
cat("  data/q3_unified/levels_robustness_pooled.csv\n")
cat("  data/q3_unified/levels_robustness_decomp.csv\n")
cat("  data/q3_unified/levels_robustness_es.csv\n")
