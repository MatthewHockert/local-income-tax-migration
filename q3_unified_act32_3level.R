# q3_unified_act32_3level.R
# Pre-/post-Act-32 split of the headline cell x dose response, run at all
# three estimating units (cousub, SD, block).
#
# Approach: for each level, source the unit-level data prep, then split
# treated units by whether their first event year falls before 2012 or
# in/after 2012, run two separate stacked-continuous regressions on each
# subset, and compare the Inc-cell coefficients.
#
# Outputs:
#   data/q3_unified/act32_split_<level>.csv

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
ACT32_YEAR <- 2012L

run_act32_split <- function(stk, c_col, cell_col, dose_col,
                             outcomes, ctrls, fe_str, cluster_var, level_tag) {
  ctrls_str <- paste(ctrls, collapse = " + ")
  results <- list()
  diagnostics <- list()
  cohort <- stk[, get(c_col)]
  for (label in c("pre", "post")) {
    if (label == "pre") {
      sub <- stk[is.na(get(c_col)) | get(c_col) < ACT32_YEAR]
    } else {
      sub <- stk[is.na(get(c_col)) | get(c_col) >= ACT32_YEAR]
    }
    sub[, (cell_col) := factor(get(cell_col),
                                levels = c("NC","Inc-bind","Inc-becomes",
                                            "Inc-nobind","Dec-bind",
                                            "Dec-becomes-non","Dec-nobind"))]
    fe_cols <- trimws(unlist(strsplit(fe_str, "[+^]")))
    fe_cols <- fe_cols[fe_cols != ""]
    for (o in outcomes) {
      cols <- intersect(c(o, dose_col, cell_col, ctrls,
                           all.vars(cluster_var), fe_cols),
                         names(sub))
      d <- sub[, ..cols]
      d <- d[complete.cases(d)]
      if (nrow(d) == 0) next
      f <- as.formula(paste0(o,
           " ~ i(", cell_col, ", ", dose_col, ", ref='NC') + ",
           ctrls_str, " | ", fe_str))
      m <- tryCatch(feols(f, data = d, cluster = cluster_var),
                     error = function(e) {
                       cat(sprintf("  %s %s err: %s\n", label, o, conditionMessage(e))); NULL })
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
               c("est","se","p"), skip_absent = TRUE)
      ic <- ct[grepl(paste0(cell_col, "::"), term)]
      ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
      ic[, period := label]
      ic[, outcome := o]
      ic[, n := nobs(m)]
      results[[length(results)+1L]] <- ic[, .(period, outcome, cell,
                                                est, se, p, n)]
      dep_vec <- m$residuals + m$fitted.values
      diagnostics[[length(diagnostics)+1L]] <- data.table(
        period   = label, outcome = o,
        n        = nobs(m),
        r2       = round(fitstat(m, "r2",  simplify = TRUE), 4),
        wr2      = round(fitstat(m, "wr2", simplify = TRUE), 4),
        dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
        dep_sd   = round(sd(dep_vec,   na.rm = TRUE), 4))
    }
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, paste0("act32_split_", level_tag, ".csv")))
  diag_dt <- rbindlist(diagnostics, fill = TRUE)
  fwrite(diag_dt, file.path(out_dir, paste0("act32_split_", level_tag, "_diag.csv")))
  cat(sprintf("\n[%s] Act 32 split saved (%d coef rows, %d diag rows).\n",
              level_tag, nrow(out), nrow(diag_dt)))
  invisible(out)
}

# ---------------------------------------------------------------------------
# Cousub level
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("Cousub-level Act 32 split\n")
cat("========================================================\n")
e_c <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_c)
stk_muni <- as.data.table(e_c$stk_muni)
ctrls_c  <- e_c$ctrls
run_act32_split(stk_muni, "c_muni", "treat_cell", "dose_stack",
                c("log_live_and_work","log_in_commuters",
                  "log_total_jobs","share_live_in","share_work_outside"),
                ctrls_c, "stack_muni + stack_year", ~muni, "cousub")

# ---------------------------------------------------------------------------
# SD level
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("SD-level Act 32 split\n")
cat("========================================================\n")
e_sd <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e_sd)
stk_sd <- as.data.table(e_sd$stk_sd)
sd_share_ctrls <- e_sd$sd_share_ctrls
ctrls_sd <- intersect(c("mean_muni_rate", sd_share_ctrls), names(stk_sd))
# stk_sd may use cell_sd; alias for compat
if (!"treat_cell" %in% names(stk_sd) && "cell_sd" %in% names(stk_sd)) {
  stk_sd[, treat_cell := cell_sd]
}
run_act32_split(stk_sd, "c_sd", "treat_cell", "dose_stack",
                c("log_live_and_work","log_in_commuters",
                  "log_total_residents","share_live_in","share_work_outside"),
                ctrls_sd, "stack_sd + stack_year", ~sd_id, "sd")

# ---------------------------------------------------------------------------
# Block level: stacked block panel (matches q3_unified_block_joint.R FE
# structure). Split by muni cohort year pre/post 2012.
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("Block-level Act 32 split (stacked design)\n")
cat("========================================================\n")
K_stack_block <- 5L
e_full <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_full)
cousub_panel <- as.data.table(e_full$panel)
meta <- unique(cousub_panel[, .(muni, year, cell_muni, c_muni,
                                 cell_sd, dose_sd,
                                 share_police, share_streets, share_debt_svc,
                                 share_eit, share_re, share_intergov)])

b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                          "share_eit","share_re","share_intergov","cell_muni"),
                        names(b))
if (length(drop_cols)) b[, (drop_cols) := NULL]
b <- merge(b, meta, by = c("muni","year"), all.x = TRUE)

sd_fc_path <- file.path(data_dir, "pa_sd_controls.rds")
sd_share_cols <- c("share_state_rev_sd","share_local_rev_sd",
                    "share_federal_rev_sd","share_property_tax_sd",
                    "share_instructional_sd","share_support_sd",
                    "share_capital_sd","share_debt_int_sd")
if (file.exists(sd_fc_path)) {
  sd_fc <- as.data.table(readRDS(sd_fc_path))
  keep <- intersect(c("sd_id","year", sd_share_cols), names(sd_fc))
  sd_fc <- sd_fc[, ..keep]
  sd_fc <- sd_fc[, lapply(.SD, function(x)
                  if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                  by = .(sd_id, year)]
  for (cc in sd_share_cols) {
    if (cc %in% names(sd_fc))
      set(sd_fc, which(!is.finite(sd_fc[[cc]])), cc, NA_real_)
  }
  b <- merge(b, sd_fc, by = c("sd_id","year"), all.x = TRUE)
}

cell_levels_block <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                       "Dec-bind","Dec-becomes-non","Dec-nobind")
b[, cell_muni := factor(cell_muni, levels = cell_levels_block)]
b[, cell_sd   := factor(cell_sd,   levels = cell_levels_block)]

# Build stacked block panel on muni cohorts (mirrors q3_unified_block_joint.R)
cat("Building stacked block panel (this is large)...\n")
treated_b <- unique(b[cell_muni != "NC" & !is.na(cell_muni),
                       .(muni, cohort = c_muni, cell = cell_muni)])
nc_munis_b <- unique(b[cell_muni == "NC", muni])
cohort_years_b <- sort(unique(treated_b$cohort))
build_one_block <- function(cy) {
  win <- (cy - K_stack_block):(cy + K_stack_block)
  t_munis <- treated_b[cohort == cy]
  if (nrow(t_munis) == 0) return(NULL)
  d_t <- b[muni %in% t_munis$muni & year %in% win]
  d_t[, treat_cell := cell_muni]
  d_c <- b[muni %in% nc_munis_b & year %in% win]
  d_c[, treat_cell := "NC"]
  d <- rbind(d_t, d_c, fill = TRUE)
  if (nrow(d) == 0) return(NULL)
  d[, stack := cy]; d[, c_muni := cy]
  d
}
stk_b <- rbindlist(lapply(cohort_years_b, build_one_block), fill = TRUE)
stk_b[, treat_cell := factor(treat_cell, levels = cell_levels_block)]
stk_b[, base_rate := mean(resident_eit[year < stack], na.rm = TRUE),
      by = .(stack, muni)]
stk_b[!is.finite(base_rate), base_rate := 0]
stk_b[, dose_stack := resident_eit - base_rate]
stk_b[treat_cell == "NC", dose_stack := 0]

cat(sprintf("Stack: %s rows; treated blocks %s; NC blocks %s\n",
            format(nrow(stk_b), big.mark=","),
            format(uniqueN(stk_b[treat_cell != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk_b[treat_cell == "NC", block_geoid]), big.mark=",")))

block_outcomes <- c("log_home_count","log_work_count",
                     "home_in_muni_share","share_work_outside")
block_ctrls <- intersect(c("share_police","share_streets","share_debt_svc",
                            "share_eit","share_re","share_intergov",
                            sd_share_cols),
                          names(stk_b))

run_act32_split(stk_b, "c_muni", "treat_cell", "dose_stack",
                block_outcomes, block_ctrls,
                "block_geoid^stack + year^stack", ~muni + sd_id, "block")

cat("\nDone. Outputs in data/q3_unified/act32_split_*.csv\n")
