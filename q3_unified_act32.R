# q3_unified_act32.R
# Add post-Act-32 (2012) interaction to the unified stacked-continuous DiD.
# Splits each cell x dose effect into pre-2012 and post-2012 components on
# all three sides (muni, SD, nr).
#
# Substantive prediction: pre-Act-32 binding-cell coefficients should be
# attenuated relative to post because the bill rule wasn't actually
# enforced pre-2012 (fragmented withholding, partial nonres reporting).
# Post-Act-32 binding-cell coefficients should match the current pooled
# headline.

suppressMessages({
  library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
max_year <- 2019
K_stack <- 5L

# ---------------------------------------------------------------------------
# Reuse data prep from q3_unified_full_table.R via source
# ---------------------------------------------------------------------------
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel    <- e$panel
stk_muni <- e$stk_muni
ctrls    <- e$ctrls

# Add post-Act-32 indicator to stacked panel
stk_muni[, post := as.integer(year >= 2012)]
# Build period-specific dose: dose_pre = dose * 1{pre}, dose_post = dose * 1{post}
stk_muni[, dose_pre  := dose_stack * (1L - post)]
stk_muni[, dose_post := dose_stack * post]

outcomes <- c("log_live_and_work", "log_in_commuters", "log_out_commuters",
              "log_total_jobs", "log_total_residents",
              "share_live_in", "share_work_outside", "share_nonres")

ctrls_str <- paste(ctrls, collapse = " + ")

run_act32 <- function(stk, outcome) {
  d <- stk[, c("muni","year","treat_cell","dose_pre","dose_post","stack_muni",
                "stack_year", outcome, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_pre, ref='NC') + ",
            "i(treat_cell, dose_post, ref='NC') + ",
            ctrls_str,
       " | stack_muni + stack_year"))
  feols(f, data = d, cluster = ~muni)
}

cat("\nRunning post-Act-32 interaction model for each outcome...\n")
results <- list()
diag_rows <- list()
for (o in outcomes) {
  cat("  ", o, "...\n")
  m <- run_act32(stk_muni, o)
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct[, outcome := o]
  ct[, n := nobs(m)]
  results[[o]] <- ct
  diag_rows[[o]] <- data.table(
    outcome = o,
    n = nobs(m),
    R2 = round(fitstat(m, "r2", simplify = TRUE), 4),
    Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
    dep_mean = round(mean(m$residuals + m$fitted.values, na.rm = TRUE), 4),
    dep_sd = round(sd(m$residuals + m$fitted.values, na.rm = TRUE), 4)
  )
}

coefs <- rbindlist(results, fill = TRUE)
coefs[, sig := fifelse(p < 0.01, "***",
                fifelse(p < 0.05, "**",
                fifelse(p < 0.10, "*", "")))]

# Tag period (pre/post) and cell
coefs[, period := fifelse(grepl("dose_pre", term), "pre",
                  fifelse(grepl("dose_post", term), "post", NA_character_))]
coefs[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]

fwrite(coefs, file.path(out_dir, "act32_full_table.csv"))

diag_dt <- rbindlist(diag_rows, fill = TRUE)
fwrite(diag_dt, file.path(out_dir, "act32_diagnostics.csv"))

cat("\n--- Headline result: log(live_and_work), Inc cells, by period ---\n")
inc_lw <- coefs[outcome == "log_live_and_work" &
                cell %in% c("Inc-bind","Inc-becomes","Inc-nobind") &
                period %in% c("pre","post")]
inc_lw_wide <- dcast(inc_lw[, .(cell, period, est = round(est, 3),
                                  se = round(se, 3), sig)],
                      cell ~ period, value.var = c("est","se","sig"))
print(inc_lw_wide)

cat("\n--- Diagnostics ---\n")
print(diag_dt)

cat("\nSaved data/q3_unified/act32_full_table.csv\n")
cat("Saved data/q3_unified/act32_diagnostics.csv\n")
