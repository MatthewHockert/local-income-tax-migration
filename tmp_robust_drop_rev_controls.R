# tmp_robust_drop_rev_controls.R
# One-off robustness: drop the three post-treatment revenue-share controls
# (share_eit, share_re, share_intergov) and re-estimate the block-level
# joint headline. Compare Inc-bind coefficient to the current spec.
#
# This is a temp script. If the result is interesting we'll fold it into
# the active pipeline as a robustness column.

suppressMessages({ library(data.table); library(fixest) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")

# ---- Build the block-level stacked panel exactly like q3_unified_block_joint.R
cat("Sourcing q3_unified_full_table.R for cell + dose...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
cousub_meta <- unique(as.data.table(e$panel)[, .(
  muni, year, cell_muni, cell_sd, cell_nr,
  dose_muni, dose_sd, dose_nr, c_muni, c_sd,
  share_police, share_streets, share_debt_svc,
  share_eit, share_re, share_intergov)])

b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                          "share_eit","share_re","share_intergov"),
                        names(b))
if (length(drop_cols)) b[, (drop_cols) := NULL]
b <- merge(b, cousub_meta, by = c("muni","year"), all.x = TRUE)

sd_ctrls_path <- file.path(data_dir, "pa_sd_controls.rds")
sd_ctrl <- as.data.table(readRDS(sd_ctrls_path))
sd_ctrls <- c("share_state_rev_sd","share_local_rev_sd",
              "share_federal_rev_sd","share_property_tax_sd",
              "share_instructional_sd","share_support_sd",
              "share_capital_sd","share_debt_int_sd")
keep <- intersect(c("sd_id","year", sd_ctrls), names(sd_ctrl))
sd_ctrl <- sd_ctrl[, ..keep]
sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                     if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                   by = .(sd_id, year)]
for (cc in sd_ctrls) {
  set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
}
b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
b[, cell_muni := factor(cell_muni, levels = cell_levels)]
b[, cell_sd   := factor(cell_sd,   levels = cell_levels)]

# Stack on muni cohorts (matches headline)
build_stack_muni <- function(p, K = 5L) {
  treated <- unique(p[cell_muni != "NC" & !is.na(cell_muni),
                       .(muni, cohort = c_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_m <- unique(treated[cohort == cy, .(muni)])
    if (nrow(t_m) == 0) return(NULL)
    d_t <- p[muni %in% t_m$muni & year %in% win]
    d_t[, treat_cell_muni := cell_muni]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell_muni := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]
    d
  }), fill = TRUE)
}
cat("Building stacked block panel...\n")
stk <- build_stack_muni(b)
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]

# ---- Control sets to compare
muni_exp_ctrls   <- c("share_police","share_streets","share_debt_svc")
muni_rev_ctrls   <- c("share_eit","share_re","share_intergov")
all_muni_ctrls   <- c(muni_exp_ctrls, muni_rev_ctrls)

run_spec <- function(outcome, muni_ctrls, label) {
  c_str <- paste(c(muni_ctrls, sd_ctrls), collapse = " + ")
  f <- as.formula(paste0(
    outcome, " ~ i(treat_cell_muni, dose_muni, ref = 'NC') +",
    "         i(cell_sd, dose_sd, ref = 'NC') + ",
    c_str, " | block_geoid^stack + year^stack"))
  d <- stk[, c("block_geoid","muni","sd_id","year","stack",
                "treat_cell_muni","cell_sd","dose_muni","dose_sd",
                outcome, muni_ctrls, sd_ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
           c("est","se","p"), skip_absent = TRUE)
  ic <- ct[grepl("treat_cell_muni::Inc-bind:dose_muni", term)]
  data.table(spec = label, outcome = outcome,
             est = ic$est, se = ic$se, p = ic$p,
             n = nobs(m),
             r2 = round(fitstat(m, "r2", simplify = TRUE), 4))
}

cat("\n========= RESULTS =========\n")
outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")
res <- list()
for (o in outcomes) {
  res[[length(res)+1L]] <- run_spec(o, all_muni_ctrls,
                                     "All 6 muni controls (current)")
  res[[length(res)+1L]] <- run_spec(o, muni_exp_ctrls,
                                     "Expenditure shares only (no rev shares)")
}
res_dt <- rbindlist(res, fill = TRUE)
print(res_dt[, .(spec, outcome, est = round(est, 4),
                  se = round(se, 4), p = round(p, 4), n)])

cat("\n--- Side-by-side per outcome ---\n")
for (o in outcomes) {
  cat(sprintf("\n%s:\n", o))
  print(res_dt[outcome == o, .(spec, est, se, p)])
}
