# _helpers.R
# Shared modeling machinery for the consolidated stage scripts (01-06).
# Sourced at the top of every stage script. Contains:
#   - path constants (RAW_DIR / DERIVED_DIR / RESULTS_DIR / FIG_DIR)
#   - build_headline_env(): panel build + cell classification + muni stack
#     (extracted verbatim from q3_unified_full_table.R on 2026-07-02)
#   - add_realized_dose(): the constant per-muni dose tag for event studies
#   - pretrend_bucket(): the full pre-trend test battery for any ES model
# Also contains the paper formatting helpers (label map, ggplot theme,
# table formatter, wald_pretrend) merged from paper_helpers.R on 2026-07-06.

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

# Config and paths ----
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
# RAW_DIR / DERIVED_DIR / RESULTS_DIR come from config.R (single source).
FIG_DIR <- OUT_DIR

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
  # x100: raw effective rates are decimals (0.015 = 1.5%); rescale to
  # PERCENTAGE POINTS so control coefficients read per-pp, the same units
  # as the EIT dose. (Pure relabeling -- other coefficients are unaffected.)
  burden <- burden[!is.na(cousub_fips),
                   .(muni_eff_rate   = 100 * mean(muni_eff_rate,   na.rm = TRUE),
                     sd_eff_rate     = 100 * mean(sd_eff_rate,     na.rm = TRUE),
                     county_eff_rate = 100 * mean(county_eff_rate, na.rm = TRUE)),
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
    # Lay explanation: a PA worker pays whichever is bigger -- the combined
    # home-town + school-district rate, or the rate where they work.
    # home_binds = 1 means the home side wins that comparison, so it is the
    # home rates that actually set this worker's tax bill.
    p[, home_binds := as.integer(home_stack > muni_nonres_eit)]
    lag_col <- paste0("lag_", tag)
    p[, (lag_col) := shift(get(rate_var)), by = muni]
    # Year-over-year change in this setter's rate -> Inc/Dec event flags.
    # 1e-6 tolerance guards float noise; NA = no change that year.
    ev_col <- paste0("ev_", tag)
    # Lay explanation: flip through each town's rate history like a flipbook
    # and stamp every page where the rate moved -- "Inc" if it rose from the
    # year before, "Dec" if it fell. Years with no movement stay blank; the
    # first stamped page becomes that town's event year.
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
    # Lay explanation: two snapshots around the change -- was the worker's
    # bill set by home rates the year BEFORE (prebind), and is it the year
    # OF the change (postbind)? This before/after pair is what sorts each
    # increase into bind / becomes / nobind just below.
    pbind <- p[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
    ev <- merge(ev, pbind[, .(muni, year, lag_home_binds)],
                by.x = c("muni","c_var"), by.y = c("muni","year"), all.x = TRUE)
    ev[, prebind := lag_home_binds]; ev[, lag_home_binds := NULL]
    # Lay explanation: sort each change into a cell. Inc-bind = home rates
    # already set the bill and rose, so workers feel the hike; Inc-becomes =
    # the hike newly pushed home rates past the work rate; Inc-nobind = the
    # rate rose on paper but no bill moved (the placebo). Dec cells mirror.
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
    # Lay explanation: only towns whose rate sat perfectly still for the
    # whole sample earn the "NC" (never-changer) control badge. A town that
    # did change but whose change cannot be dated cleanly is labeled
    # "Uncertain" rather than quietly passed off as a control.
    rate_changes <- p2[!is.na(get(rate_var)), .(
      n = sum(abs(diff(get(rate_var))) > 1e-6, na.rm = TRUE)
    ), by = muni]
    truly_stable <- rate_changes[n == 0, muni]
    p2[muni %in% truly_stable, (cell_v) := "NC"]
    p2[!muni %in% truly_stable & is.na(get(cell_v)), (cell_v) := "Uncertain"]
    # Flip-flopper filter: after the first event, all subsequent rate changes
    # in the same direction. Any opposite-direction reversal -> Uncertain.
    # Lay explanation: a town that both raised AND cut its rate over the
    # years is like a witness who keeps changing their story -- we cannot
    # say whether it was "treated" with an increase or a decrease, so it is
    # dropped rather than guessed at.
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
    # Lay explanation: dose = how far the rate has climbed above where it
    # sat just before the first change -- like marking a child's height
    # against a pencil line on the doorframe. Zero before the change, zero
    # forever for control towns, and it grows with any later hikes.
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
  # Lay explanation: one mini-experiment per adoption year. The towns that
  # changed in, say, 2009 are compared to a fresh photocopy of every
  # never-changer over the same window; 2013's changers get their own copy,
  # and so on. Stacking the mini-experiments lets each cohort be judged
  # against clean controls only, never against other changers.
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
    # Lay explanation: inside each mini-experiment, re-zero every town's
    # dose against its own pre-change average, so "dose" always means "how
    # much higher than before THIS change" -- even for towns whose window
    # got clipped by the edge of the data.
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
  # Lay explanation: give each town ONE number -- how much its rate will
  # eventually have risen five years after the change. The event study then
  # interacts this fixed tag with year-by-year dummies to ask whether towns
  # headed for big hikes were already drifting away from small-hike towns
  # BEFORE anything actually happened.
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
# Lay explanation: a stress test. It asks how big a hidden pre-existing
# drift -- measured in multiples of the largest wobble we can already see
# before the event -- would have to be before the estimate could plausibly
# be zero. The bigger the breakdown number, the sturdier the result.
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
# Lay explanation: all four tests probe the same worry from different
# angles -- were treated and control towns already on different paths
# BEFORE the rate change? If so, the "effect" could just be old drift
# continuing, so a result only counts as clean when the battery agrees.
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

# Paper formatting helpers (merged from paper_helpers.R, 2026-07-06) ----
# ---------------------------------------------------------------------------
# Variable label map
# ---------------------------------------------------------------------------
# Maps raw column names to publication-quality outcome labels. Used in
# tables (column headers) and figures (facet labels / axis titles).

OUTCOME_LABELS <- c(
  # municipality / SD level
  log_live_and_work    = "Log(residents working in own municipality)",
  log_in_commuters     = "Log(in-commuters)",
  log_out_commuters    = "Log(out-commuters)",
  log_total_jobs       = "Log(total jobs)",
  log_total_residents  = "Log(employed residents)",
  share_live_in        = "Share of workers living in own muni",
  share_work_outside   = "Share of residents working outside",
  share_nonres         = "Share of workers who are nonresidents",
  # block level
  log_home_count       = "Log(workers living in block)",
  log_work_count       = "Log(workers at block)",
  home_in_muni_share   = "Share of block residents in own muni",
  # industry decomposition
  log_jobs_total       = "Log(total jobs)",
  log_jobs_consumer    = "Log(consumer-facing jobs)",
  log_jobs_tradeable   = "Log(tradeable jobs)",
  log_jobs_health_edu  = "Log(health/education jobs)",
  log_jobs_public      = "Log(public-sector jobs)",
  log_jobs_other       = "Log(other jobs)",
  share_consumer       = "Share of consumer-facing jobs",
  share_tradeable      = "Share of tradeable jobs"
)

# Short labels for narrow tables and figure facets
OUTCOME_LABELS_SHORT <- c(
  log_live_and_work    = "Log(work in own muni)",
  log_in_commuters     = "Log(in-commuters)",
  log_out_commuters    = "Log(out-commuters)",
  log_total_jobs       = "Log(total jobs)",
  log_total_residents  = "Log(employed residents)",
  share_live_in        = "Share live in muni",
  share_work_outside   = "Share work outside",
  share_nonres         = "Share nonres workers",
  log_home_count       = "Log(workers live)",
  log_work_count       = "Log(workers work)",
  home_in_muni_share   = "Share live+work in muni",
  log_jobs_total       = "Log(total jobs)",
  log_jobs_consumer    = "Log(consumer jobs)",
  log_jobs_tradeable   = "Log(tradeable jobs)",
  log_jobs_health_edu  = "Log(health/edu jobs)",
  log_jobs_public      = "Log(public jobs)",
  log_jobs_other       = "Log(other jobs)",
  share_consumer       = "Consumer share",
  share_tradeable      = "Tradeable share"
)

outcome_label <- function(x, short = FALSE) {
  lookup <- if (short) OUTCOME_LABELS_SHORT else OUTCOME_LABELS
  out <- lookup[x]
  # On fallback, use the raw name but escape underscores so it's TeX-safe.
  fallback <- gsub("_", "\\\\_", x[is.na(out)])
  out[is.na(out)] <- fallback
  unname(out)
}

# ---------------------------------------------------------------------------
# ggplot theme + red/blue palette
# ---------------------------------------------------------------------------
# Color convention used throughout the paper:
#   red  = muni-side coefficient / muni cohort
#   blue = SD-side coefficient / SD cohort

PAPER_RED  <- "#C0392B"   # muted firebrick
PAPER_BLUE <- "#1F4E79"   # navy blue
PAPER_GREY <- "grey50"

theme_paper <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text       = element_text(face = "bold", size = base_size - 1),
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(size = base_size - 1, color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(1.4, "lines"),
      axis.title       = element_text(size = base_size - 1)
    )
}

# ---------------------------------------------------------------------------
# Wald joint pre-trend test
# ---------------------------------------------------------------------------
# Given a fitted fixest model `m` and a regex matching pre-event coefficient
# names (e.g., the rel::-5, rel::-4, ... rel::-2 set in a realized-dose ES),
# return a list with the joint Wald statistic, df, and p-value.
#
# Uses the model's cluster-robust vcov to construct the joint test:
#   W = b' V^{-1} b  ~ chi-squared(k)
#
# Returns list(stat, df, p, terms). Returns list(NA, NA, NA, character(0))
# if fewer than 2 matching terms or the V is singular.
wald_pretrend <- function(m, term_regex = "^rel::-[2-9]") {
  coef_names <- names(coef(m))
  pre_idx <- grep(term_regex, coef_names)
  if (length(pre_idx) < 2)
    return(list(stat = NA_real_, df = 0L, p = NA_real_,
                terms = coef_names[pre_idx]))
  b <- coef(m)[pre_idx]
  V <- vcov(m)[pre_idx, pre_idx, drop = FALSE]
  inv <- tryCatch(solve(V), error = function(e) NULL)
  if (is.null(inv))
    return(list(stat = NA_real_, df = length(pre_idx),
                p = NA_real_, terms = names(b)))
  stat <- as.numeric(t(b) %*% inv %*% b)
  p <- pchisq(stat, df = length(b), lower.tail = FALSE)
  list(stat = stat, df = length(b), p = p, terms = names(b))
}

scale_color_side <- function(...) {
  scale_color_manual(values = c(Municipality = PAPER_RED,
                                "School district" = PAPER_BLUE,
                                "School District" = PAPER_BLUE,
                                muni = PAPER_RED, sd = PAPER_BLUE,
                                Muni = PAPER_RED, SD = PAPER_BLUE),
                     ...)
}
scale_fill_side <- function(...) {
  scale_fill_manual(values = c(Municipality = PAPER_RED,
                               "School district" = PAPER_BLUE,
                               "School District" = PAPER_BLUE,
                               muni = PAPER_RED, sd = PAPER_BLUE,
                               Muni = PAPER_RED, SD = PAPER_BLUE),
                    ...)
}

# ---------------------------------------------------------------------------
# Number formatters
# ---------------------------------------------------------------------------
sig_stars <- function(p) {
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.10] <- "*"
  out[!is.na(p) & p < 0.05] <- "**"
  out[!is.na(p) & p < 0.01] <- "***"
  out
}
fmt_est <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return("")
  ifelse(is.na(x), "---", formatC(x, format = "f", digits = digits))
}
fmt_se <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return("")
  ifelse(is.na(x), "", sprintf("(%s)", formatC(x, format = "f", digits = digits)))
}
fmt_n <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  ifelse(is.na(x), "---", formatC(as.numeric(x), format = "d", big.mark = ","))
}
fmt_pct <- function(x, digits = 3) {
  if (is.null(x)) return("")
  ifelse(is.na(x), "---", formatC(x, format = "f", digits = digits))
}

# ---------------------------------------------------------------------------
# Publication-style table formatter
# ---------------------------------------------------------------------------
# Build a LaTeX table fragment in publication style:
#   - columns = outcomes (using OUTCOME_LABELS or _SHORT)
#   - rows    = cells (binding-state categories); each cell takes TWO lines:
#               coefficient (with sig stars) on the first, SE in parens on
#               the second
#   - per-column diagnostics block at the bottom: N, R^2, within R^2,
#               dep mean, dep SD
#   - no equation in the note
#
# Arguments:
#   coefs_dt   data.table with columns: outcome, cell, est, se, p
#              (long format). Cells are factored in cell_order.
#   diag_dt    data.table with columns: outcome, n, r2, wr2, dep_mean, dep_sd
#              (one row per outcome). Optional cols: n_clusters, fe_label,
#              cluster_label.
#   outcomes   character vector of outcomes (column order) -- looked up in
#              OUTCOME_LABELS.
#   cells      character vector of cells (row order).
#   caption    table caption.
#   label      \label tag.
#   short_labels  if TRUE, use OUTCOME_LABELS_SHORT in column headers.
#   notes      character vector of additional sentences for the note.
#   col_spec   optional column spec override (defaults to lc..c with one c per outcome).
#
# Returns:  a character vector (one element = one line of LaTeX).
build_pub_table <- function(coefs_dt, diag_dt, outcomes, cells,
                              caption, label, short_labels = TRUE,
                              notes = character(0), col_spec = NULL,
                              sideways = FALSE,
                              diag_fields = c("n","r2","wr2","dep_mean"),
                              controls_dt = NULL, control_labels = NULL,
                              section_header_above_cells = NULL) {
  stopifnot(is.data.table(coefs_dt), is.data.table(diag_dt))
  coefs_dt <- copy(coefs_dt)[, outcome := as.character(outcome)]
  diag_dt  <- copy(diag_dt)
  if ("outcome" %in% names(diag_dt)) diag_dt[, outcome := as.character(outcome)]

  if (is.null(col_spec)) {
    col_spec <- paste0("l", strrep("c", length(outcomes)))
  }

  # Header row of outcome labels
  hdr_labels <- outcome_label(outcomes, short = short_labels)
  hdr <- paste0("Outcome & ", paste(hdr_labels, collapse = " & "), " \\\\")

  # Coefficient rows. Rows whose every outcome cell is missing/NA are
  # dropped from the table and listed in a "missing rows" footnote.
  body <- character(0)
  dropped_cells <- character(0)
  for (cl in cells) {
    est_vec <- character(length(outcomes))
    se_vec  <- character(length(outcomes))
    any_present <- FALSE
    for (j in seq_along(outcomes)) {
      o <- outcomes[j]
      row <- coefs_dt[outcome == o & cell == cl]
      if (nrow(row) == 0) {
        est_vec[j] <- "---"; se_vec[j] <- ""
      } else {
        est1 <- row$est[1]; se1 <- row$se[1]; p1 <- row$p[1]
        if (is.na(est1) || !is.finite(est1)) {
          est_vec[j] <- "---"; se_vec[j] <- ""
        } else {
          any_present <- TRUE
          stars <- sig_stars(p1)
          est_vec[j] <- if (nchar(stars) == 0)
                           fmt_est(est1)
                         else
                           paste0(fmt_est(est1), "\\sym{", stars, "}")
          se_vec[j]  <- fmt_se(se1)
        }
      }
    }
    if (!any_present) {
      dropped_cells <- c(dropped_cells, cl)
      next
    }
    body <- c(body,
              paste0("\\textbf{", cl, "} & ", paste(est_vec, collapse = " & "), " \\\\"),
              paste0("                  & ", paste(se_vec,  collapse = " & "), " \\\\"))
  }
  if (length(dropped_cells) > 0) {
    notes <- c(notes,
               paste0("Rows omitted (no estimable coefficients): ",
                      paste(dropped_cells, collapse = ", "), "."))
  }

  # Controls block (optional). Adds rows after the cell body.
  controls_block <- character(0)
  if (!is.null(controls_dt) && is.data.table(controls_dt) && nrow(controls_dt) > 0) {
    ncols <- length(outcomes)
    mc <- paste0("\\multicolumn{", ncols + 1, "}{l}")
    controls_block <- c(controls_block,
                        "\\midrule",
                        paste0(mc, "{\\textit{Fiscal controls}} \\\\"))
    ctrl_terms <- unique(controls_dt$term)
    label_map <- if (!is.null(control_labels)) control_labels else setNames(ctrl_terms, ctrl_terms)
    for (tm in ctrl_terms) {
      est_vec <- character(ncols); se_vec <- character(ncols)
      any_present <- FALSE
      for (j in seq_along(outcomes)) {
        r <- controls_dt[outcome == outcomes[j] & term == tm]
        if (nrow(r) == 0) { est_vec[j] <- "---"; se_vec[j] <- ""; next }
        e <- r$est[1]; se1 <- r$se[1]; p1 <- r$p[1]
        if (is.na(e)) { est_vec[j] <- "---"; se_vec[j] <- ""; next }
        any_present <- TRUE
        stars <- sig_stars(p1)
        est_vec[j] <- if (nchar(stars) == 0) fmt_est(e) else paste0(fmt_est(e), "\\sym{", stars, "}")
        se_vec[j]  <- fmt_se(se1)
      }
      if (!any_present) next
      pretty <- if (tm %in% names(label_map)) label_map[[tm]] else gsub("_", "\\\\_", tm)
      controls_block <- c(controls_block,
                          paste0(pretty, " & ", paste(est_vec, collapse = " & "), " \\\\"),
                          paste0("                  & ", paste(se_vec,  collapse = " & "), " \\\\"))
    }
  }

  # Per-outcome diagnostics block
  diag_rows <- function(field, label_text, fmt = function(x) formatC(x, format = "f", digits = 3)) {
    vals <- character(length(outcomes))
    for (j in seq_along(outcomes)) {
      o <- outcomes[j]
      row <- if ("outcome" %in% names(diag_dt)) diag_dt[outcome == o] else diag_dt[0]
      vals[j] <- if (nrow(row) == 0 || !(field %in% names(row))) "---" else {
        x <- row[[field]]
        if (is.null(x) || length(x) == 0 || all(is.na(x))) "---" else fmt(x[1])
      }
    }
    paste0(label_text, " & ", paste(vals, collapse = " & "), " \\\\")
  }
  diag_lookup <- list(
    n        = list(field = "n",        label = "$N$",         fmt = fmt_n),
    r2       = list(field = "r2",       label = "$R^2$",       fmt = function(x) formatC(x, format = "f", digits = 3)),
    wr2      = list(field = "wr2",      label = "Within $R^2$",fmt = function(x) formatC(x, format = "f", digits = 3)),
    dep_mean = list(field = "dep_mean", label = "Dep.\\ mean", fmt = function(x) formatC(x, format = "f", digits = 3)),
    dep_sd   = list(field = "dep_sd",   label = "Dep.\\ SD",   fmt = function(x) formatC(x, format = "f", digits = 3)),
    wald_p   = list(field = "wald_p",   label = "Joint Wald $p$ (leads)",
                    fmt = function(x) formatC(x, format = "f", digits = 3))
  )
  diag_block <- character(0)
  for (k in diag_fields) {
    if (k %in% names(diag_lookup)) {
      spec <- diag_lookup[[k]]
      diag_block <- c(diag_block, diag_rows(spec$field, spec$label, fmt = spec$fmt))
    }
  }

  note_sig <- "SE in parens. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  note_all <- paste(c(notes, note_sig), collapse = " ")

  # Auto-fit wide tables: if there are >= 4 outcome columns, wrap the
  # tabular in \resizebox{\textwidth}{!} so the table never overflows the
  # text width. Narrower tables are typeset at native size.
  wide <- length(outcomes) >= 3
  tabular <- c(
    paste0("  \\begin{tabular}{", col_spec, "}"),
    "    \\toprule",
    paste0("    ", hdr),
    "    \\midrule",
    paste0("    ", body),
    if (length(controls_block)) paste0("    ", controls_block) else character(0),
    if (length(diag_block)) "    \\midrule" else character(0),
    if (length(diag_block)) paste0("    ", diag_block) else character(0),
    "    \\bottomrule",
    "  \\end{tabular}"
  )
  if (wide) tabular <- c("  \\resizebox{\\textwidth}{!}{",
                          tabular, "  }")
  out <- c(
    if (sideways) "\\begin{sidewaystable}" else "\\begin{table}[!htbp]",
    "  \\centering",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    "  \\footnotesize",
    "  \\renewcommand{\\arraystretch}{0.9}",
    tabular,
    "  \\vspace{0.3em}",
    "  \\begin{minipage}{0.95\\textwidth}",
    paste0("    \\footnotesize \\textit{Notes:} ", note_all),
    "  \\end{minipage}",
    if (sideways) "\\end{sidewaystable}" else "\\end{table}"
  )
  out
}

# ---------------------------------------------------------------------------
# Helper: read a results CSV and a diagnostics CSV, return list with both
# coerced to data.table.
# ---------------------------------------------------------------------------
read_results <- function(results_path, diag_path = NULL) {
  r <- as.data.table(read.csv(results_path, stringsAsFactors = FALSE))
  d <- if (!is.null(diag_path) && file.exists(diag_path))
         as.data.table(read.csv(diag_path,  stringsAsFactors = FALSE))
       else data.table()
  list(coefs = r, diag = d)
}

