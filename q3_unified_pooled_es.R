# q3_unified_pooled_es.R
# Pooled-Inc and pooled-Dec realized-dose event studies, at municipality
# and block levels. Each script outputs the per-event-time per-pp
# coefficient $\beta_k$ on the eventual cumulative dose $\widetilde D_i$
# interacted with the relative-event-time indicator, for two pools:
#   - Pool-Inc: all Inc-* treated units versus NC controls.
#   - Pool-Dec: all Dec-* treated units versus NC controls.
#
# Outputs:
#   data/q3_unified/pooled_es_<level>_<direction>_coefs.csv
#   data/q3_unified/pooled_es_<level>_<direction>_att.csv

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
K_stack <- 5L

inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
dec_cells <- c("Dec-bind","Dec-becomes-non","Dec-nobind")

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
sd_ctrls   <- c("share_state_rev_sd","share_local_rev_sd",
                "share_federal_rev_sd","share_property_tax_sd",
                "share_instructional_sd","share_support_sd",
                "share_capital_sd","share_debt_int_sd")

muni_outcomes  <- c("log_live_and_work","log_in_commuters",
                    "log_total_jobs",
                    "share_live_in","share_work_outside")
block_outcomes <- c("log_home_count","log_work_count",
                    "home_in_muni_share","share_work_outside")

# ---------------------------------------------------------------------------
# Helper: source the realized-dose cousub ES script's panel, but pool cells
# ---------------------------------------------------------------------------
cat("Sourcing cousub realized-dose ES prep...\n")
e_c <- new.env()
sys.source(file.path(project_dir, "q3_unified_headline_es.R"), envir = e_c)
stk_muni <- as.data.table(e_c$stk)  # stacked panel: treat_cell_muni column
ctrls_muni <- e_c$ctrls

stk_muni[, pool_inc := as.integer(as.character(treat_cell_muni) %in% inc_cells)]
stk_muni[, pool_dec := as.integer(as.character(treat_cell_muni) %in% dec_cells)]
stk_muni[, is_nc    := as.integer(as.character(treat_cell_muni) == "NC")]

run_pooled_es_muni <- function(direction) {
  dd <- if (direction == "Inc") stk_muni[pool_inc == 1 | is_nc == 1]
        else                    stk_muni[pool_dec == 1 | is_nc == 1]
  results <- list(); att <- list()
  for (o in muni_outcomes) {
    cols <- c("muni","year","stack","stack_muni","stack_year","rel",
               "realized_dose_muni","cell_sd","dose_sd","cell_nr","dose_nr",
               o, ctrls_muni)
    cols <- intersect(cols, names(dd))
    d <- dd[, ..cols]
    if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
    if (!is.null(d$dose_nr)) d[is.na(dose_nr), dose_nr := 0]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1)",
         " + i(cell_sd, dose_sd, ref = 'NC')",
         " + i(cell_nr, dose_nr, ref = 'NC') + ",
         paste(ctrls_muni, collapse = " + "),
         " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data = d, cluster = ~muni),
                   error = function(e) { cat(o,"err:",conditionMessage(e),"\n"); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) next
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, direction := direction]; ic[, n := nobs(m)]
    results[[length(results)+1L]] <- ic
    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    wp <- wald_pretrend(m, "^rel::-[2-9]")
    dep_vec <- m$fitted.values + m$residuals
    att[[length(att)+1L]] <- data.table(
      outcome = o, direction = direction,
      att_post = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se  = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      wald_pre_stat = round(wp$stat, 3),
      wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4),
      n = nobs(m),
      r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
      wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean = round(mean(dep_vec, na.rm = TRUE), 3),
      dep_sd = round(sd(dep_vec, na.rm = TRUE), 3))
    cat(sprintf("  [cousub %s pool %s] post=%.3f pre=%.3f  Wald(%d)=%.2f p=%.4f  N=%s\n",
                o, direction,
                mean(post$est, na.rm = TRUE),
                mean(pre$est, na.rm = TRUE),
                wp$df, wp$stat, wp$p,
                format(nobs(m), big.mark=",")))
  }
  list(coefs = rbindlist(results, fill = TRUE),
       att   = rbindlist(att, fill = TRUE))
}

cat("\nCousub pool-Inc:\n")
r_muni_inc <- run_pooled_es_muni("Inc")
cat("\nCousub pool-Dec:\n")
r_muni_dec <- run_pooled_es_muni("Dec")

fwrite(rbind(r_muni_inc$coefs, r_muni_dec$coefs, fill = TRUE),
       file.path(out_dir, "pooled_es_cousub_coefs.csv"))
fwrite(rbind(r_muni_inc$att, r_muni_dec$att, fill = TRUE),
       file.path(out_dir, "pooled_es_cousub_att.csv"))

# ---------------------------------------------------------------------------
# Block-level pooled ES has been factored into two standalone scripts to
# avoid memory pressure from sys.source()'ing block_es_full.R (which
# triggers the full per-cell ES as a side effect):
#   q3_unified_pooled_es_block_only.R  (muni-cohort, ~30 min)
#   q3_unified_pooled_es_block_sd.R    (SD-cohort,    ~30 min)
# The block branch below is disabled. Run the standalone scripts instead.
# ---------------------------------------------------------------------------
if (FALSE) {
cat("\nSourcing block realized-dose ES prep (heavy)...\n")
e_b <- new.env()
sys.source(file.path(project_dir, "q3_unified_block_es_full.R"), envir = e_b)
stk_block <- as.data.table(e_b$stk)
ctrls_block <- e_b$ctrls

stk_block[, pool_inc := as.integer(as.character(treat_cell_muni) %in% inc_cells)]
stk_block[, pool_dec := as.integer(as.character(treat_cell_muni) %in% dec_cells)]
stk_block[, is_nc    := as.integer(as.character(treat_cell_muni) == "NC")]

run_pooled_es_block <- function(direction) {
  dd <- if (direction == "Inc") stk_block[pool_inc == 1 | is_nc == 1]
        else                    stk_block[pool_dec == 1 | is_nc == 1]
  results <- list(); att <- list()
  for (o in block_outcomes) {
    cols <- c("block_geoid","year","muni","sd_id","stack","rel",
               "realized_dose_muni","cell_sd","dose_sd","cell_nr","dose_nr",
               o, ctrls_block)
    cols <- intersect(cols, names(dd))
    d <- dd[, ..cols]
    if (!is.null(d$dose_sd)) d[is.na(dose_sd), dose_sd := 0]
    if (!is.null(d$dose_nr)) d[is.na(dose_nr), dose_nr := 0]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    cat(sprintf("  [block %s pool %s] N=%s ... ", o, direction, format(nrow(d), big.mark=",")))
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1)",
         " + i(cell_sd, dose_sd, ref = 'NC')",
         " + i(cell_nr, dose_nr, ref = 'NC') + ",
         paste(ctrls_block, collapse = " + "),
         " | block_geoid^stack + year^stack"))
    m <- tryCatch(feols(f, data = d, cluster = ~muni + sd_id),
                   error = function(e) { cat("err:",conditionMessage(e),"\n"); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) { cat("no rel terms\n"); next }
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, direction := direction]; ic[, n := nobs(m)]
    results[[length(results)+1L]] <- ic
    post <- ic[rel >= 0 & rel <= 5]
    pre  <- ic[rel <= -2]
    dep_vec <- m$fitted.values + m$residuals
    att[[length(att)+1L]] <- data.table(
      outcome = o, direction = direction,
      att_post = round(mean(post$est, na.rm = TRUE), 3),
      att_post_se = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
      att_pre = round(mean(pre$est, na.rm = TRUE), 3),
      att_pre_se  = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
      n = nobs(m),
      r2 = round(fitstat(m, "r2", simplify = TRUE), 4),
      wr2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean = round(mean(dep_vec, na.rm = TRUE), 3),
      dep_sd = round(sd(dep_vec, na.rm = TRUE), 3))
    cat(sprintf("post=%.3f pre=%.3f\n",
                mean(post$est, na.rm = TRUE),
                mean(pre$est, na.rm = TRUE)))
  }
  list(coefs = rbindlist(results, fill = TRUE),
       att   = rbindlist(att, fill = TRUE))
}

cat("\nBlock pool-Inc:\n")
r_block_inc <- run_pooled_es_block("Inc")
cat("\nBlock pool-Dec:\n")
r_block_dec <- run_pooled_es_block("Dec")

fwrite(rbind(r_block_inc$coefs, r_block_dec$coefs, fill = TRUE),
       file.path(out_dir, "pooled_es_block_coefs.csv"))
fwrite(rbind(r_block_inc$att, r_block_dec$att, fill = TRUE),
       file.path(out_dir, "pooled_es_block_att.csv"))
}  # end disabled block branch

cat("\nDone. Pooled ES results saved.\n")
