# q3_unified_pooled_es_sd_only.R
# Pooled-Inc and pooled-Dec realized-dose event study at the SD level.
# The SD-level model has only one rate-setter side (the SD itself), so we
# pool all Inc-* SD cells into one realized-dose treatment tag and all
# Dec-* SD cells into another.
#
# Inputs (constructed by sourcing q3_unified_sd_es.R):
#   stacked SD panel with realized_dose_sd, rel, cell_sd
# Outputs:
#   data/q3_unified/pooled_es_sd_coefs.csv
#   data/q3_unified/pooled_es_sd_att.csv

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")
dec_cells <- c("Dec-bind","Dec-becomes-non","Dec-nobind")

muni_outcomes  <- c("log_live_and_work","log_in_commuters",
                    "log_total_residents",
                    "share_live_in","share_work_outside")

cat("Sourcing SD ES setup...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_es.R"), envir = e)
stk <- as.data.table(e$stk_sd)    # sd_es defines stk_sd, not stk
ctrls <- e$ctrls_es

# Pool tags
stk[, pool_cell := as.character(treat_cell)]
stk[, pool_inc := as.integer(pool_cell %in% inc_cells)]
stk[, pool_dec := as.integer(pool_cell %in% dec_cells)]
stk[, is_nc    := as.integer(pool_cell == "NC")]

run_pooled <- function(direction) {
  dd <- if (direction == "Inc") stk[pool_inc == 1 | is_nc == 1]
        else                    stk[pool_dec == 1 | is_nc == 1]
  res <- list(); att <- list()
  for (o in muni_outcomes) {
    cols <- c("sd_id","year","stack","stack_sd","stack_year","rel",
               "realized_dose_sd", o, ctrls)
    cols <- intersect(cols, names(dd))
    d <- dd[, ..cols]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_sd, ref = -1) + ",
         paste(ctrls, collapse = " + "),
         " | stack_sd + stack_year"))
    m <- tryCatch(feols(f, data = d, cluster = ~sd_id),
                   error = function(e) { cat(o,"err:",conditionMessage(e),"\n"); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    ic <- ct[grepl("^rel::", term)]
    if (nrow(ic) == 0) next
    ic[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ic[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
    ic[, outcome := o]; ic[, direction := direction]; ic[, n := nobs(m)]
    res[[length(res)+1L]] <- ic
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
    cat(sprintf("  [sd %s pool %s] post=%.3f pre=%.3f  Wald(%d)=%.2f p=%.4f\n",
                o, direction,
                mean(post$est, na.rm = TRUE),
                mean(pre$est, na.rm = TRUE),
                wp$df, wp$stat, wp$p))
  }
  list(coefs = rbindlist(res, fill = TRUE),
       att   = rbindlist(att, fill = TRUE))
}

cat("\nSD pool-Inc:\n")
r_inc <- run_pooled("Inc")

fwrite(r_inc$coefs,
       file.path(out_dir, "pooled_es_sd_coefs.csv"))
fwrite(r_inc$att,
       file.path(out_dir, "pooled_es_sd_att.csv"))
cat("\nDone.\n")
