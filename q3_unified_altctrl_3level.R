# q3_unified_altctrl_3level.R
# Alternative control-group definition (Cengiz 2019-style "clean controls"):
# any unit with no rate event in the (c-K, c+K) window is eligible as a
# control for cohort c, even if it has events outside that window. NC-only
# is a strict subset of this looser definition. Compares headline
# coefficients across the two control-group definitions at each level.
#
# Output: data/q3_unified/altctrl_<level>.csv

suppressMessages({ library(data.table); library(fixest) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack <- 5L

# Cousub-level alternative control: clean-in-window
build_stack_clean <- function(p, c_col, dose_col, rate_col, K = K_stack) {
  treated <- unique(p[!is.na(get(c_col)) & p[[deparse(substitute(p))]] != "NC",
                       .(unit = NA, cohort = get(c_col))])
  # Reset (the dynamic build is tricky; just inline)
  invisible(NULL)
}

# Simpler: take stacked panel from each level's prep, identify each unit's
# rate-event years, and replace NC pool with clean-in-window pool per cohort.

run_altctrl_cousub <- function() {
  cat("\n=== Cousub-level alt-control ===\n")
  e <- new.env()
  sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
  panel <- as.data.table(e$panel)
  ctrls <- e$ctrls
  ctrls_str <- paste(ctrls, collapse = " + ")

  # Identify rate-event years per muni (from resident_eit)
  setorder(panel, muni, year)
  panel[, res_lag := shift(resident_eit), by = muni]
  panel[, has_event := !is.na(res_lag) & abs(resident_eit - res_lag) > 1e-6]
  events_by_muni <- panel[has_event == TRUE, .(yrs = list(year)), by = muni]

  treated <- unique(panel[cell_muni != "NC" & !is.na(c_muni) & cell_muni != "Uncertain",
                            .(muni, cohort = c_muni, cell = cell_muni)])
  cohort_years <- sort(unique(treated$cohort))

  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    is_clean <- sapply(events_by_muni$yrs, function(y) length(intersect(y, win)) == 0)
    clean_munis <- events_by_muni$muni[is_clean]
    clean_munis <- setdiff(clean_munis, t_munis$muni)
    d_t <- panel[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- panel[muni %in% clean_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(resident_eit[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := resident_eit - base_rate]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                    "Dec-bind","Dec-becomes-non","Dec-nobind")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  cat(sprintf("Clean-in-window stacked panel: %s rows; treated munis %s; clean controls %s\n",
              format(nrow(stk), big.mark = ","),
              format(uniqueN(stk[treat_cell != "NC", muni]), big.mark = ","),
              format(uniqueN(stk[treat_cell == "NC", muni]), big.mark = ",")))

  outcomes <- c("log_live_and_work","log_in_commuters",
                 "log_total_residents","share_live_in","share_work_outside")
  results <- list(); diags <- list()
  for (o in outcomes) {
    cols <- intersect(c("muni","year","stack_muni","stack_year","treat_cell",
                          "dose_stack", o, ctrls), names(stk))
    d <- stk[, ..cols]
    d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                            ctrls_str, " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data = d, cluster = ~muni),
                   error = function(e) NULL)
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
             c("est","se","p"), skip_absent = TRUE)
    ic <- ct[grepl("treat_cell::", term)]
    ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
    ic[, outcome := o]; ic[, n := nobs(m)]
    results[[length(results)+1L]] <- ic[, .(outcome, cell, est, se, p, n)]
    dep_vec <- m$residuals + m$fitted.values
    diags[[o]] <- data.table(
      outcome   = o, n = nobs(m),
      R2        = round(fitstat(m, "r2",  simplify = TRUE), 4),
      Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean  = round(mean(dep_vec, na.rm = TRUE), 4),
      dep_sd    = round(sd(dep_vec,   na.rm = TRUE), 4))
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, "altctrl_cousub.csv"))
  fwrite(rbindlist(diags, fill = TRUE),
         file.path(out_dir, "altctrl_cousub_diag.csv"))
  cat(sprintf("Saved altctrl_cousub.csv (%d rows) + diag\n", nrow(out)))
}

run_altctrl_cousub()

# ---------------------------------------------------------------------------
# SD-level clean-in-window
# ---------------------------------------------------------------------------
run_altctrl_sd <- function() {
  cat("\n=== SD-level alt-control ===\n")
  e <- new.env()
  sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e)
  panel <- as.data.table(e$sd_panel)
  sd_share_ctrls <- e$sd_share_ctrls
  ctrls <- intersect(c("mean_muni_rate", sd_share_ctrls), names(panel))
  ctrls_str <- paste(ctrls, collapse = " + ")

  setorder(panel, sd_id, year)
  panel[, sd_lag := shift(sd_eit_rate), by = sd_id]
  panel[, has_event := !is.na(sd_lag) & abs(sd_eit_rate - sd_lag) > 1e-6]
  events_by_sd <- panel[has_event == TRUE, .(yrs = list(year)), by = sd_id]

  treated <- unique(panel[!is.na(c_sd) & cell_sd != "NC" & cell_sd != "Uncertain",
                            .(sd_id, cohort = c_sd, cell = cell_sd)])
  cohort_years <- sort(unique(treated$cohort))

  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_sds <- treated[cohort == cy]
    if (nrow(t_sds) == 0) return(NULL)
    is_clean <- sapply(events_by_sd$yrs, function(y) length(intersect(y, win)) == 0)
    clean_sds <- events_by_sd$sd_id[is_clean]
    clean_sds <- setdiff(clean_sds, t_sds$sd_id)
    d_t <- panel[sd_id %in% t_sds$sd_id & year %in% win]
    d_t <- merge(d_t, t_sds[, .(sd_id, treat_cell = cell)], by = "sd_id", all.x = TRUE)
    d_c <- panel[sd_id %in% clean_sds & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  stk[, stack_sd := paste0(stack, "_", sd_id)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(sd_eit_rate[rel < 0], na.rm = TRUE), by = .(stack, sd_id)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := sd_eit_rate - base_rate]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                    "Dec-bind","Dec-becomes-non","Dec-nobind")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]

  outcomes <- c("log_live_and_work","log_in_commuters",
                 "log_total_residents","share_live_in","share_work_outside")
  results <- list(); diags <- list()
  for (o in outcomes) {
    cols <- intersect(c("sd_id","year","stack_sd","stack_year","treat_cell",
                          "dose_stack", o, ctrls), names(stk))
    d <- stk[, ..cols]; d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                            ctrls_str, " | stack_sd + stack_year"))
    m <- tryCatch(feols(f, data = d, cluster = ~sd_id),
                   error = function(e) NULL)
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
             c("est","se","p"), skip_absent = TRUE)
    ic <- ct[grepl("treat_cell::", term)]
    ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
    ic[, outcome := o]; ic[, n := nobs(m)]
    results[[length(results)+1L]] <- ic[, .(outcome, cell, est, se, p, n)]
    dep_vec <- m$residuals + m$fitted.values
    diags[[o]] <- data.table(
      outcome   = o, n = nobs(m),
      R2        = round(fitstat(m, "r2",  simplify = TRUE), 4),
      Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean  = round(mean(dep_vec, na.rm = TRUE), 4),
      dep_sd    = round(sd(dep_vec,   na.rm = TRUE), 4))
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, "altctrl_sd.csv"))
  fwrite(rbindlist(diags, fill = TRUE),
         file.path(out_dir, "altctrl_sd_diag.csv"))
  cat(sprintf("Saved altctrl_sd.csv (%d rows) + diag\n", nrow(out)))
}

run_altctrl_sd()

# ---------------------------------------------------------------------------
# Block-level clean-in-window
# Operate on the cousub muni cohort definition (each block inherits its
# muni's cohort), and define clean controls as blocks whose muni has no
# rate event in the cohort window.
# ---------------------------------------------------------------------------
run_altctrl_block <- function() {
  cat("\n=== Block-level alt-control ===\n")
  e_full <- new.env()
  sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_full)
  cousub_panel <- as.data.table(e_full$panel)

  setorder(cousub_panel, muni, year)
  cousub_panel[, res_lag := shift(resident_eit), by = muni]
  cousub_panel[, has_event := !is.na(res_lag) & abs(resident_eit - res_lag) > 1e-6]
  events_by_muni <- cousub_panel[has_event == TRUE, .(yrs = list(year)), by = muni]

  treated <- unique(cousub_panel[cell_muni != "NC" & !is.na(c_muni) & cell_muni != "Uncertain",
                                   .(muni, cohort = c_muni, cell = cell_muni)])
  cohort_years <- sort(unique(treated$cohort))

  block_panel <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
  # block_panel already has resident_eit from build_block_panel; pull
  # only what's missing (cell_muni, c_muni, share controls)
  meta <- unique(cousub_panel[, .(muni, year, c_muni,
                                    share_police, share_streets, share_debt_svc,
                                    share_eit, share_re, share_intergov)])
  drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                            "share_eit","share_re","share_intergov"),
                          names(block_panel))
  block_panel[, (drop_cols) := NULL]
  block_panel <- merge(block_panel, meta, by = c("muni","year"), all.x = TRUE)

  # SD share controls
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
    block_panel <- merge(block_panel, sd_fc, by = c("sd_id","year"), all.x = TRUE)
  }

  block_outcomes <- c("log_home_count","log_work_count",
                       "home_in_muni_share","share_work_outside")
  block_ctrls <- intersect(c("share_police","share_streets","share_debt_svc",
                              "share_eit","share_re","share_intergov",
                              sd_share_cols),
                            names(block_panel))
  ctrls_str <- paste(block_ctrls, collapse = " + ")

  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    is_clean <- sapply(events_by_muni$yrs, function(y) length(intersect(y, win)) == 0)
    clean_munis <- events_by_muni$muni[is_clean]
    clean_munis <- setdiff(clean_munis, t_munis$muni)
    d_t <- block_panel[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)],
                  by = "muni", all.x = TRUE)
    d_c <- block_panel[muni %in% clean_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(resident_eit[rel < 0], na.rm = TRUE),
      by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := resident_eit - base_rate]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                    "Dec-bind","Dec-becomes-non","Dec-nobind")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  cat(sprintf("Block clean-in-window stack: %s rows\n",
              format(nrow(stk), big.mark = ",")))

  results <- list(); diags <- list()
  for (o in block_outcomes) {
    cols <- intersect(c("block_geoid","year","muni","sd_id","stack_muni",
                          "stack_year","treat_cell","dose_stack",
                          o, block_ctrls), names(stk))
    d <- stk[, ..cols]; d <- d[complete.cases(d)]
    if (nrow(d) == 0) next
    f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                            ctrls_str, " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data = d, cluster = ~muni + sd_id),
                   error = function(e) {
                     cat(sprintf("  %s err: %s\n", o, conditionMessage(e))); NULL })
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")
    setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
             c("est","se","p"), skip_absent = TRUE)
    ic <- ct[grepl("treat_cell::", term)]
    ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
    ic[, outcome := o]; ic[, n := nobs(m)]
    results[[length(results)+1L]] <- ic[, .(outcome, cell, est, se, p, n)]
    dep_vec <- m$residuals + m$fitted.values
    diags[[o]] <- data.table(
      outcome   = o, n = nobs(m),
      R2        = round(fitstat(m, "r2",  simplify = TRUE), 4),
      Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
      dep_mean  = round(mean(dep_vec, na.rm = TRUE), 4),
      dep_sd    = round(sd(dep_vec,   na.rm = TRUE), 4))
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, "altctrl_block.csv"))
  fwrite(rbindlist(diags, fill = TRUE),
         file.path(out_dir, "altctrl_block_diag.csv"))
  cat(sprintf("Saved altctrl_block.csv (%d rows)\n", nrow(out)))
}

run_altctrl_block()

cat("\nDone.\n")
