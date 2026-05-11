# q3_unified_tercile_3level.R
# Dose-magnitude linearity check at all three estimating units.
# Splits Inc-bind units by tercile of total realized dose (small/med/large)
# and re-estimates the headline coefficient on each tercile. Tests whether
# the per-pp coefficient is constant across dose magnitudes.
#
# Outputs:
#   data/q3_unified/tercile_<level>.csv

suppressMessages({ library(data.table); library(fixest) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack <- 5L

run_tercile <- function(stk, c_col, cell_col, dose_col, rate_col,
                         outcomes, ctrls, fe_str, cluster_var, level_tag) {
  ctrls_str <- paste(ctrls, collapse = " + ")
  fe_cols <- trimws(unlist(strsplit(fe_str, "[+^]"))); fe_cols <- fe_cols[fe_cols != ""]

  # Compute total dose per Inc-bind unit (cumulative Delta-tau at +5)
  units_inc_bind <- stk[get(cell_col) == "Inc-bind", unique(get(c_col[1]))]
  if (length(units_inc_bind) < 6) {
    cat(sprintf("[%s] too few Inc-bind units (%d); skip\n",
                level_tag, length(units_inc_bind)))
    return(invisible())
  }
  if (length(units_inc_bind) < 9) {
    cat(sprintf("[%s] only %d Inc-bind units; tercile splits will be thin\n",
                level_tag, length(units_inc_bind)))
  }

  inc_bind_unit_col <- if (level_tag == "cousub") "muni"
                        else if (level_tag == "sd") "sd_id"
                        else "muni"  # block uses muni-cohort
  inc_bind_units <- unique(stk[get(cell_col) == "Inc-bind", get(inc_bind_unit_col)])

  doses <- stk[get(inc_bind_unit_col) %in% inc_bind_units &
                 !is.na(get(c_col)) &
                 get("year") >= get(c_col) &
                 get("year") <= get(c_col) + 5,
               .(total_dose = max(get(dose_col), na.rm = TRUE)),
               by = c(inc_bind_unit_col)]
  doses <- doses[is.finite(total_dose) & total_dose > 0]
  if (nrow(doses) < 9) {
    cat(sprintf("[%s] insufficient nonzero doses; skip\n", level_tag))
    return(invisible())
  }
  doses[, tercile := cut(total_dose,
                          breaks = quantile(total_dose, probs = c(0,1/3,2/3,1),
                                             na.rm = TRUE),
                          include.lowest = TRUE,
                          labels = c("small","medium","large"))]
  cat(sprintf("\n[%s] Inc-bind dose terciles:\n", level_tag))
  print(doses[, .(N = .N,
                   mean_dose = round(mean(total_dose),3),
                   min_dose = round(min(total_dose),3),
                   max_dose = round(max(total_dose),3)),
              by = tercile])

  results <- list(); diags <- list()
  nc_units <- unique(stk[get(cell_col) == "NC", get(inc_bind_unit_col)])
  for (terc in c("small","medium","large")) {
    munis_t <- doses[tercile == terc][[inc_bind_unit_col]]
    sub <- stk[get(inc_bind_unit_col) %in% c(munis_t, nc_units)]
    sub[, treat_terc := factor(
      ifelse(get(inc_bind_unit_col) %in% munis_t, "Inc-bind", "NC"),
      levels = c("NC","Inc-bind"))]
    for (o in outcomes) {
      cols <- intersect(c(o, dose_col, "treat_terc", ctrls,
                           all.vars(cluster_var), fe_cols),
                         names(sub))
      d <- sub[, ..cols]
      d <- d[complete.cases(d)]
      if (nrow(d) == 0) next
      f <- as.formula(paste0(o,
           " ~ i(treat_terc, ", dose_col, ", ref='NC') + ",
           ctrls_str, " | ", fe_str))
      m <- tryCatch(feols(f, data = d, cluster = cluster_var),
                     error = function(e) {
                       cat(sprintf("  %s %s err: %s\n", terc, o, conditionMessage(e))); NULL })
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")
      setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
               c("est","se","p"), skip_absent = TRUE)
      ic <- ct[grepl("Inc-bind", term)]
      if (nrow(ic) > 0) {
        results[[length(results)+1L]] <- data.table(
          tercile = terc, outcome = o,
          n_units = length(munis_t),
          est = ic$est, se = ic$se, p = ic$p, n = nobs(m))
        dep_vec <- m$residuals + m$fitted.values
        diags[[length(diags)+1L]] <- data.table(
          tercile   = terc, outcome = o, n = nobs(m),
          R2        = round(fitstat(m, "r2",  simplify = TRUE), 4),
          Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
          dep_mean  = round(mean(dep_vec, na.rm = TRUE), 4),
          dep_sd    = round(sd(dep_vec,   na.rm = TRUE), 4))
      }
    }
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, paste0("tercile_", level_tag, ".csv")))
  if (length(diags))
    fwrite(rbindlist(diags, fill = TRUE),
           file.path(out_dir, paste0("tercile_", level_tag, "_diag.csv")))
  cat(sprintf("\n[%s] tercile output saved (%d rows).\n", level_tag, nrow(out)))
  print(out)
  invisible(out)
}

# Cousub
cat("\n=========================================\nCousub-level dose-tercile\n=========================================\n")
e_c <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_c)
stk_muni <- as.data.table(e_c$stk_muni)
ctrls_c  <- e_c$ctrls
run_tercile(stk_muni, "c_muni", "treat_cell", "dose_stack", "resident_eit",
            c("log_live_and_work","log_total_residents","share_live_in"),
            ctrls_c, "stack_muni + stack_year", ~muni, "cousub")

# SD
cat("\n=========================================\nSD-level dose-tercile\n=========================================\n")
e_sd <- new.env(); sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e_sd)
stk_sd <- as.data.table(e_sd$stk_sd)
sd_share_ctrls <- e_sd$sd_share_ctrls
ctrls_sd <- intersect(c("mean_muni_rate", sd_share_ctrls), names(stk_sd))
if (!"treat_cell" %in% names(stk_sd) && "cell_sd" %in% names(stk_sd)) {
  stk_sd[, treat_cell := cell_sd]
}
run_tercile(stk_sd, "c_sd", "treat_cell", "dose_stack", "sd_eit_rate",
            c("log_live_and_work","log_total_residents","share_live_in"),
            ctrls_sd, "stack_sd + stack_year", ~sd_id, "sd")

# Block: skip stacking complexity; use unstacked TWFE on block panel
cat("\n=========================================\nBlock-level dose-tercile\n=========================================\n")
e_full <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_full)
cousub_panel <- as.data.table(e_full$panel)
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
meta <- unique(cousub_panel[, .(muni, year, cell_muni, c_muni, dose_muni,
                                 share_police, share_streets, share_debt_svc,
                                 share_eit, share_re, share_intergov)])
drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                          "share_eit","share_re","share_intergov","cell_muni"),
                        names(b))
b[, (drop_cols) := NULL]
b <- merge(b, meta, by = c("muni","year"), all.x = TRUE)
b[, treat_cell := factor(cell_muni,
                          levels = c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                                     "Dec-bind","Dec-becomes-non","Dec-nobind"))]

block_outcomes <- c("log_home_count","log_work_count","home_in_muni_share")
block_ctrls <- intersect(c("share_police","share_streets","share_debt_svc",
                            "share_eit","share_re","share_intergov"),
                          names(b))
run_tercile(b, "c_muni", "treat_cell", "dose_muni", "resident_eit",
            block_outcomes, block_ctrls,
            "block_geoid + year", ~muni + sd_id, "block")

cat("\nDone.\n")
