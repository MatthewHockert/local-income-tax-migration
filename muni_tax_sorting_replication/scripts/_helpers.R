# _helpers.R
# Shared modeling machinery for the consolidated stage scripts (01-06).
# Sourced at the top of every stage script. Contains:
#   - path constants (RAW_DIR / DERIVED_DIR / RESULTS_DIR / FIG_DIR)
#   - build_headline_env(): panel build + cell classification + muni stack
#     (extracted verbatim from q3_unified_full_table.R on 2026-07-02)
#   - add_realized_dose(): the constant per-muni dose tag for event studies
#   - pretrend_bucket(): the full pre-trend test battery for any ES model
# paper_helpers.R (labels, theme, table formatter, wald_pretrend) is sourced
# here so stage scripts only need source("_helpers.R").

suppressMessages({
  library(data.table)
  library(fixest)
})

# Config and paths ----
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
# RAW_DIR / DERIVED_DIR / RESULTS_DIR come from config.R (single source).
FIG_DIR <- OUT_DIR
source(file.path(SCRIPT_DIR, "paper_helpers.R"))

# Headline environment ----
# Everything the models need, built once per stage script: the cousub panel
# (rates + AFR spending shares + property effective rates), the binding-cell
# classification, and the muni-side stacked panel. Returns a list so stage
# scripts do `env <- build_headline_env(); panel <- env$panel; ...` instead
# of sys.source()-ing q3_unified_full_table.R.
build_headline_env <- function(max_year = 2019, K_stack = 5L) {
  wp <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_panel.rds")))[year <= max_year]
  rp <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_residence_panel.rds")))[year <= max_year]
  rate <- as.data.table(readRDS(file.path(DERIVED_DIR, "pa_rate_panel.rds")))
  setnames(wp, "work_city", "muni", skip_absent = TRUE)
  setnames(rp, "home_city", "muni", skip_absent = TRUE)
  
  panel <- merge(
    wp[, .(muni, year, live_and_work, live_outside, total_jobs, share_live_in)],
    rp[, .(muni, year, residents_employed, residents_work_in_own,
           residents_work_outside, share_work_outside)],
    by = c("muni", "year"), all = TRUE)
  rate_keep <- rate[!is.na(cousub_fips) & year <= max_year,
                    .(muni = cousub_fips, year, resident_eit, sd_eit, muni_nonres_eit)]
  rate_keep <- rate_keep[, .(resident_eit = mean(resident_eit, na.rm = TRUE),
                             sd_eit       = max(sd_eit, na.rm = TRUE),
                             muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                         by = .(muni, year)]
  for (c in c("resident_eit","sd_eit","muni_nonres_eit"))
    rate_keep[!is.finite(get(c)), (c) := NA_real_]
  panel <- merge(panel, rate_keep, by = c("muni", "year"), all.x = TRUE)
  panel[, home_stack := resident_eit + sd_eit]
  panel[, in_commuters := live_outside]
  panel[, out_commuters := residents_work_outside]
  panel[, total_residents := residents_employed]
  log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
  panel[, log_live_and_work := log_pos(live_and_work)]
  panel[, log_in_commuters  := log_pos(in_commuters)]
  panel[, log_out_commuters := log_pos(out_commuters)]
  panel[, log_total_jobs    := log_pos(total_jobs)]
  panel[, log_total_residents := log_pos(total_residents)]
  panel[, share_nonres := ifelse(total_jobs > 0, in_commuters / total_jobs, NA_real_)]
  
  afr <- fread(file.path(RAW_DIR, "pa_afr_panel.csv"))
  xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
  fc <- afr[!is.na(cousub_fips), {
    total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
    total_exp <- as.numeric(total_expenditures)
    eit_rev <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                                 earned_income_tax_revenues, earned_income_tax))
    re_tax  <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                                 real_estate_tax_revenues, real_estate_tax))
    fed_ig <- as.numeric(`intergovernmental_revenues-federal_government`)
    sta_ig <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                 `intergovernmental_revenues-state_government`, state_govt))
    loc_ig <- as.numeric(`intergovernmental_revenues-local_government`)
    intergov <- rowSums(cbind(fed_ig, sta_ig, loc_ig), na.rm = TRUE)
    intergov[is.na(fed_ig) & is.na(sta_ig) & is.na(loc_ig)] <- NA_real_
    police_exp <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
    streets_exp <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                      `public_works-highways_and_streets_expenditures`,
                                      streets_and_roads))
    debt_svc <- as.numeric(debt_service_expenditures)
    list(muni = cousub_fips, year = yr,
         share_police    = ifelse(total_exp > 0, police_exp  / total_exp, NA_real_),
         share_streets   = ifelse(total_exp > 0, streets_exp / total_exp, NA_real_),
         share_debt_svc  = ifelse(total_exp > 0, debt_svc    / total_exp, NA_real_),
         share_eit       = ifelse(total_rev > 0, eit_rev  / total_rev, NA_real_),
         share_re        = ifelse(total_rev > 0, re_tax   / total_rev, NA_real_),
         share_intergov  = ifelse(total_rev > 0, intergov / total_rev, NA_real_))
  }]
  fc <- fc[!is.na(muni) & !is.na(year)]
  fc <- fc[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
           by = .(muni, year)]
  panel <- merge(panel, fc, by = c("muni", "year"), all.x = TRUE)
  
  # Property-tax effective rates (statutory millage x STEB common-level ratio),
  # kept SEPARATE per rate-setter: muni_eff_rate, sd_eff_rate, county_eff_rate.
  # Key: burden muni_id == DCED municipality_id -> cousub_fips via `xw` (line above).
  # Provenance: built in the parent repo
  #   scrape_pa_dced_county_tax.py -> build_pde_sd_millage.py ->
  #   multitax_gapfill_sd.py -> multitax_effective_rate.py
  # vendored here as data/multitax/pa_property_burden_panel.csv (regenerate via those).
  burden <- fread(file.path(RAW_DIR, "multitax", "pa_property_burden_panel.csv"))
  burden <- merge(burden[, .(municipality_id = muni_id, year,
                             muni_eff_rate, sd_eff_rate, county_eff_rate)],
                  xw, by = "municipality_id", all.x = TRUE)
  burden <- burden[!is.na(cousub_fips),
                   .(muni_eff_rate   = mean(muni_eff_rate,   na.rm = TRUE),
                     sd_eff_rate     = mean(sd_eff_rate,     na.rm = TRUE),
                     county_eff_rate = mean(county_eff_rate, na.rm = TRUE)),
                   by = .(muni = cousub_fips, year)]
  panel <- merge(panel, burden, by = c("muni", "year"), all.x = TRUE)
  
  panel <- panel[, lapply(.SD, function(x) {
    if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)
  }), by = .(muni, year)]
  for (col in names(panel)) {
    if (is.numeric(panel[[col]])) {
      set(panel, which(!is.finite(panel[[col]])), col, NA_real_)
    }
  }
  
  # Controls: keep the spending-composition shares; DROP the two tax-revenue shares
  # (share_re is subsumed by the property effective rates; share_eit is downstream of
  # the EIT rate = treatment, a bad control) and ADD the three property effective rates.
  ctrls <- c("share_police", "share_streets", "share_debt_svc", "share_intergov",
             "muni_eff_rate", "sd_eff_rate", "county_eff_rate")
  ctrls_str <- paste(ctrls, collapse = " + ")
  
  # Cell classification ----
  # The heart of the design (NC fix applied). For ONE rate-setter at a time
  # (muni / SD / nonres), find each muni's FIRST rate-change event and classify
  # it into a binding cell using the max-rule T = max(home_stack, nonres_rate):
  #   direction (Inc/Dec) x binds BEFORE (prebind) x binds AFTER (postbind)
  #   -> Inc-bind / Inc-becomes / Inc-nobind (+ Dec mirrors + 2 rare edge cells).
  # Munis that never change any rate -> "NC" (the control group).
  # Munis with changes in BOTH directions (flip-floppers) -> "Uncertain" (dropped).
  classify_treatment <- function(p, rate_var, bind_ref, tag) {
    setorder(p, muni, year)
    # home_binds: 1 if the residence-side stack exceeds the work-side rate,
    # i.e. the worker's bill is set by the HOME rates (the stack "binds").
    p[, home_binds := as.integer(home_stack > muni_nonres_eit)]
    lag_col <- paste0("lag_", tag)
    p[, (lag_col) := shift(get(rate_var)), by = muni]
    # Year-over-year change in this setter's rate -> Inc/Dec event flags.
    # 1e-6 tolerance guards float noise; NA = no change that year.
    ev_col <- paste0("ev_", tag)
    p[, (ev_col) := fifelse(is.na(get(lag_col)), NA_character_,
                    fifelse(get(rate_var) > get(lag_col) + 1e-6, "Inc",
                    fifelse(get(rate_var) < get(lag_col) - 1e-6, "Dec", NA_character_)))]
    # .SD[1] = the FIRST event per muni; its year becomes the cohort year c_<tag>.
    ev <- p[!is.na(get(ev_col)), .SD[1], by = muni,
            .SDcols = c("year", ev_col, lag_col, rate_var, "home_binds")]
    setnames(ev, c("year", ev_col, lag_col, rate_var, "home_binds"),
             c("c_var","ev_dir","baseline_var","first_post","postbind"))
    # prebind = binding state in the year BEFORE the event (lag of home_binds
    # evaluated at the event year); postbind = state in the event year itself.
    # The (prebind, postbind) pair is what splits Inc into bind/becomes/nobind.
    pbind <- p[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
    ev <- merge(ev, pbind[, .(muni, year, lag_home_binds)],
                by.x = c("muni","c_var"), by.y = c("muni","year"), all.x = TRUE)
    ev[, prebind := lag_home_binds]; ev[, lag_home_binds := NULL]
    ev[, cell := fcase(
      ev_dir == "Inc" & prebind == 1 & postbind == 1, "Inc-bind",
      ev_dir == "Inc" & prebind == 0 & postbind == 1, "Inc-becomes",
      ev_dir == "Inc" & prebind == 0 & postbind == 0, "Inc-nobind",
      ev_dir == "Inc" & prebind == 1 & postbind == 0, "Inc-loses",
      ev_dir == "Dec" & prebind == 1 & postbind == 1, "Dec-bind",
      ev_dir == "Dec" & prebind == 1 & postbind == 0, "Dec-becomes-non",
      ev_dir == "Dec" & prebind == 0 & postbind == 0, "Dec-nobind",
      ev_dir == "Dec" & prebind == 0 & postbind == 1, "Dec-gains",
      default = NA_character_
    )]
    setnames(ev, c("c_var","baseline_var","cell"),
             c(paste0("c_", tag), paste0("baseline_", tag), paste0("cell_", tag)))
    ev <- ev[, .SD, .SDcols = c("muni",
                                paste0("c_", tag),
                                paste0("baseline_", tag),
                                paste0("cell_", tag))]
    p2 <- merge(p, ev, by = "muni", all.x = TRUE)
    cell_v <- paste0("cell_", tag)
    c_v    <- paste0("c_", tag)
    base_v <- paste0("baseline_", tag)
    dose_v <- paste0("dose_", tag)
    # THE "NC FIX": a muni is NC only if this setter's rate NEVER changes over
    # the whole sample. Munis with changes that produced no classifiable first
    # event (e.g. missing lag year) are "Uncertain", NOT silently NC — an
    # earlier version leaked them into the control group.
    rate_changes <- p2[!is.na(get(rate_var)), .(
      n = sum(abs(diff(get(rate_var))) > 1e-6, na.rm = TRUE)
    ), by = muni]
    truly_stable <- rate_changes[n == 0, muni]
    p2[muni %in% truly_stable, (cell_v) := "NC"]
    p2[!muni %in% truly_stable & is.na(get(cell_v)), (cell_v) := "Uncertain"]
    # Flip-flopper filter: after the first event, all subsequent rate changes
    # in the same direction. Any opposite-direction reversal -> Uncertain.
    ff <- p2[!is.na(get(rate_var)), .(year, rate = get(rate_var)), by = muni][
      order(muni, year)]
    ff[, d := c(NA_real_, diff(rate)), by = muni]
    ff_summary <- ff[!is.na(d) & abs(d) > 1e-6, .(
      n_inc = sum(d > 0), n_dec = sum(d < 0)), by = muni]
    flip <- ff_summary[n_inc > 0 & n_dec > 0, muni]
    p2[muni %in% flip, (cell_v) := "Uncertain"]
    # Dose = cumulative rate change since the pre-event baseline, 0 before the
    # event and 0 for NC. Accumulates if the muni keeps raising after the first
    # event (flip-floppers are already excluded, so it is monotone).
    p2[, (dose_v) := fifelse(!is.na(get(c_v)) & year >= get(c_v),
                             get(rate_var) - get(base_v), 0)]
    cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                     "Dec-bind","Dec-becomes-non","Dec-nobind",
                     "Inc-loses","Dec-gains","Uncertain")
    p2[, (cell_v) := factor(get(cell_v), levels = cell_levels)]
    p2
  }
  
  panel <- classify_treatment(panel, "resident_eit",   "muni_nonres_eit", "muni")
  panel <- classify_treatment(panel, "sd_eit",          "muni_nonres_eit", "sd")
  panel <- classify_treatment(panel, "muni_nonres_eit", "home_stack",       "nr")
  u <- Reduce(union, list(unique(panel[cell_muni == "Uncertain", muni]),
                           unique(panel[cell_sd   == "Uncertain", muni]),
                           unique(panel[cell_nr   == "Uncertain", muni])))
  panel <- panel[!muni %in% u]
  
  # Drop Dec munis entirely (paper scope: Inc cells only).
  # Removes any muni whose muni-side OR SD-side classification is in the Dec set;
  # Dec munis are not treated, not controls.
  dec_cells_drop <- c("Dec-bind","Dec-becomes-non","Dec-nobind","Dec-gains")
  dec_munis <- unique(c(panel[cell_muni %in% dec_cells_drop, muni],
                         panel[cell_sd   %in% dec_cells_drop, muni],
                         panel[cell_nr   %in% dec_cells_drop, muni]))
  cat(sprintf("Dropping %d Dec munis (%.1f%% of panel).\n",
              length(dec_munis),
              100 * length(dec_munis) / uniqueN(panel$muni)))
  panel <- panel[!muni %in% dec_munis]
  panel[, cell_muni := droplevels(cell_muni)]
  panel[, cell_sd   := droplevels(cell_sd)]
  panel[, cell_nr   := droplevels(cell_nr)]
  
  # Stack builder ----
  # Cengiz-style ("Mode B"). One sub-experiment ("stack") per cohort year: the
  # cohort's treated munis in a +-K window around the event, plus a FRESH COPY
  # of every NC muni. NC munis therefore appear once per overlapping cohort —
  # this is why the FE must be stack-interacted (muni^stack, year^stack): each
  # copy gets its own intercept, keeping comparisons within-cohort and avoiding
  # treated-vs-treated contrasts.
  build_stack <- function(p, side_tag, dose_col, K = K_stack) {
    cell_v <- paste0("cell_", side_tag)
    c_v    <- paste0("c_", side_tag)
    treated_munis <- unique(p[!is.na(get(c_v)) & get(cell_v) != "NC",
                               .(muni, cohort = get(c_v), cell = get(cell_v))])
    nc_munis <- unique(p[get(cell_v) == "NC", muni])
    cohort_years <- sort(unique(treated_munis$cohort))
    build_one <- function(cy) {
      win <- (cy - K):(cy + K)
      t_munis <- treated_munis[cohort == cy]
      if (nrow(t_munis) == 0) return(NULL)
      d_t <- p[muni %in% t_munis$muni & year %in% win]
      d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
      d_c <- p[muni %in% nc_munis & year %in% win]
      d_c[, treat_cell := "NC"]
      d <- rbind(d_t, d_c, fill = TRUE)
      if (nrow(d) == 0) return(NULL)
      d[, stack := cy]; d[, rel := year - cy]
      d
    }
    stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
    stk[, stack_muni := paste0(stack, "_", muni)]
    stk[, stack_year := paste0(stack, "_", year)]
    # Re-baseline the dose WITHIN each stack window: subtract the muni's mean
    # pre-event dose so dose_stack is exactly 0 pre-event inside every stack
    # (a treated muni's global dose can be non-zero pre-window if its event
    # window is clipped by the sample edge).
    stk[, base_rate := mean(get(dose_col)[rel < 0], na.rm = TRUE),
        by = .(stack, muni)]
    stk[!is.finite(base_rate), base_rate := 0]
    stk[, dose_stack := get(dose_col) - base_rate]
    cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                     "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")
    stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
    stk
  }
  stk_muni <- build_stack(panel, "muni", "dose_muni")

  list(panel = panel, stk_muni = stk_muni, ctrls = ctrls,
       ctrls_str = paste(ctrls, collapse = " + "))
}

# Realized dose ----
# Constant per-muni magnitude tag for the realized-dose event studies:
# cumulative rate change from the year before the event (c-1) to c+K.
# 0 for never-changers. Adds column `realized_dose_muni` to the panel.
add_realized_dose <- function(panel, K = 5L) {
  mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)])
  setorder(mu_year, muni, year)
  rd <- mu_year[!is.na(c_muni), {
    pre <- resident_eit[year == c_muni - 1L]
    yr_max <- min(c_muni + K, max(year, na.rm = TRUE))
    end <- resident_eit[year == yr_max]
    list(realized_dose_muni = (if (length(end) == 1) end else NA_real_) -
                              (if (length(pre) == 1) pre else NA_real_))
  }, by = muni]
  rd[is.na(realized_dose_muni), realized_dose_muni := 0]
  rd <- rbind(rd, data.table(muni = unique(panel[cell_muni == "NC", muni]),
                             realized_dose_muni = 0))
  panel <- merge(panel, rd, by = "muni", all.x = TRUE)
  panel[is.na(realized_dose_muni), realized_dose_muni := 0]
  panel
}

# Pre-trend machinery ----
# hdid_breakdown(): HonestDiD (Rambachan-Roth) breakdown Mbar — the smallest
# relative-magnitudes bound at which the robust CI includes 0.
# Inf = robust beyond the grid; NA = not computable.
hdid_breakdown <- function(m, Mbarvec = c(0.5, 1, 1.5, 2)) {
  if (!requireNamespace("HonestDiD", quietly = TRUE))
    return(list(orig_excl0 = NA, breakdown = NA_real_))
  ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
  ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
  setorder(ct, rel)
  pre <- which(ct$rel <= -2); post <- which(ct$rel >= 0)
  if (length(pre) < 2 || length(post) < 1)
    return(list(orig_excl0 = NA, breakdown = NA_real_))
  bh <- ct$Estimate
  vc <- vcov(m); idx <- match(ct$term, rownames(vc)); sg <- vc[idx, idx]
  lv <- rep(1 / length(post), length(post))
  orig <- tryCatch(HonestDiD::constructOriginalCS(bh, sg, length(pre), length(post), l_vec = lv),
                   error = function(e) NULL)
  if (is.null(orig)) return(list(orig_excl0 = NA, breakdown = NA_real_))
  orig_excl0 <- !(orig$lb <= 0 & 0 <= orig$ub)
  sens <- tryCatch(as.data.table(HonestDiD::createSensitivityResults_relativeMagnitudes(
            bh, sg, length(pre), length(post), l_vec = lv, Mbarvec = Mbarvec,
            bound = "deviation from parallel trends")), error = function(e) NULL)
  if (is.null(sens)) return(list(orig_excl0 = orig_excl0, breakdown = NA_real_))
  incl0 <- sens[lb <= 0 & 0 <= ub]
  bd <- if (nrow(incl0)) min(incl0$Mbar) else Inf
  list(orig_excl0 = orig_excl0, breakdown = bd)
}

# pretrend_bucket(): run the WHOLE battery on one fitted event-study model.
#   1. joint Wald on leads (over-rejects on thin cohorts -- report, don't decide)
#   2. individual leads: count significant at 5%, min p
#   3. linear pre-trend slope (pass a pre-window model via lin_model)
#   4. HonestDiD breakdown Mbar
# robust_clean = no significant individual leads AND flat linear pre-slope.
# Returns a one-row data.table; rbind across cells/outcomes for the battery table.
pretrend_bucket <- function(m, lin_model = NULL, lin_term = "dose_rel",
                            Mbarvec = c(0.5, 1, 1.5, 2)) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate", "Std. Error", "Pr(>|t|)"),
           c("est", "se", "p"), skip_absent = TRUE)
  ct <- ct[grepl("^rel::", term)]
  ct[, k := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
  pre <- ct[k <= -2]
  wj <- tryCatch(wald_pretrend(m, "^rel::-[2-9]")$p, error = function(e) NA_real_)
  lin_b <- if (!is.null(lin_model) && lin_term %in% names(coef(lin_model)))
    coef(lin_model)[lin_term] else NA_real_
  lin_p <- if (!is.null(lin_model) && lin_term %in% names(coef(lin_model)))
    lin_model$coeftable[lin_term, "Pr(>|t|)"] else NA_real_
  hd <- hdid_breakdown(m, Mbarvec)
  data.table(
    wald_joint_p = round(wj, 4),
    n_leads = nrow(pre), n_leads_sig5 = pre[p < 0.05, .N],
    min_lead_p = round(suppressWarnings(min(pre$p)), 4),
    lin_slope = round(lin_b, 4), lin_slope_p = round(lin_p, 4),
    hdid_orig_excl0 = hd$orig_excl0, hdid_breakdown_Mbar = hd$breakdown,
    robust_clean = pre[p < 0.05, .N] == 0 &
      (is.na(lin_p) | lin_p > 0.10))
}
