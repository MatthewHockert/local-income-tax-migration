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
  results <- list()
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
  }
  out <- rbindlist(results, fill = TRUE)
  fwrite(out, file.path(out_dir, "altctrl_cousub.csv"))
  cat(sprintf("Saved altctrl_cousub.csv (%d rows)\n", nrow(out)))
}

run_altctrl_cousub()

# SD and block alt-control are heavier; skip for now (TODO).
# The cousub clean-in-window is the primary robustness; if it shows the
# same Inc-bind coefficient, the NC-only headline is robust.
cat("\nNote: SD and block alt-control deferred. Cousub is the primary.\n")
