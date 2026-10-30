#!/usr/bin/env Rscript
# 3_repair_property_rates.R -- fix effective property-tax rates where the
# statutory millage and the CLR are on different assessment bases.
# Input:  data/derived/pa_property_burden_panel.csv (2_build_property_rates.py)
# Output: data/derived/pa_property_burden_panel_repaired.csv (what the models use)
#         data/results/property_rebase_log.csv (every corrected unit-year)
# Run:    Rscript scripts/2_build_data/3_repair_property_rates.R

suppressMessages(library(data.table))
source(here::here("scripts", "helpers", "config.R"))

# ---------------------------------------------------------------------------
# Property-tax effective rates: assessment-basis repair
# ---------------------------------------------------------------------------
# WHY THIS EXISTS
# Every effective rate in the property panel is  mills/1000 x CLR.  The CLR is
# one number per county-year, shared by all three layers (muni, school
# district, county).  When a county reassesses, the assessment basis resets:
# the CLR jumps toward 1.0 (assessed = market) and statutory millage should
# fall by the same factor, leaving the effective rate CONTINUOUS.  A
# reassessment changes the basis, not the burden.
#
# In the DCED source, many governments never rebased their reported millage.
# Their old-basis millage then gets multiplied by a new-basis CLR and the
# effective rate blows up.  Wilkes-Barre City is the worst case: Luzerne
# reassessed effective 2009 (its CLR moved in 2008), the county-wide median
# muni millage fell 22.50 -> 0.93, and Wilkes-Barre stayed at 73.63 -> 96.63
# -> 141.33.  Its effective rate reads 7-14 pp of market value for eleven
# straight years against its own correct pre-2008 range of 0.36-0.48 pp.
# Wilkes-Barre is an Inc-bind treated unit, so this is a corrupted control on
# a treated observation in the headline cell.
#
# TWO PROCESSES
#   1. Reassessment rebasing.  A unit that rebased shows a FLAT effective rate
#      through the basis reset; a stranded one shows the effective rate rise by
#      exactly the CLR ratio.  So the test is whether the unit's own
#      effective-rate jump MATCHES its county's CLR jump.  No threshold on the
#      CLR jump is used or wanted: 13 genuine reassessments have CLR jumps
#      below 1.5 (Allegheny 2013 = 1.17, York 2006 = 1.33, Lancaster 2018 =
#      1.36) because those counties already assessed near market, and the CLR
#      jump distribution is continuous with no natural break.  Matching the
#      two ratios identifies the failure and supplies the correction factor at
#      the same time, and it scales automatically.
#   2. Peer consistency.  Every unit in a county-year is multiplied by the SAME
#      CLR, so their effective rates should be comparable.  A unit sitting far
#      above its county-year peers for most of the panel is on the wrong basis
#      even if no reassessment occurred inside the sample window to reveal it.
#      This catches level mismatches and outright parse errors (Beaver SD
#      45853 reads 6555 mills against 63.55 and 67.05 either side).
#
# The raw panel is never overwritten.  Output is a repaired copy plus a full
# audit log of every correction.
local({

  raw_path <- file.path(DERIVED_DIR, "pa_property_burden_panel.csv")
  b <- fread(raw_path)

  # Tunables. PLAUSIBLE_PP is the ceiling above which a single layer's
  # effective rate is treated as impossible: PA rates run ~0.1-3 pp of market
  # value per layer, so 5 pp is already generous.
  PLAUSIBLE_PP    <- 5      # ceiling for a single layer, percentage points
  AT_MARKET       <- 0.95   # CLR at/above this = assessed value reset to market
  REBASE_TOL      <- 0.5    # millage must fall to <=50% of what the reset implies
  PEER_RATIO      <- 5      # x county-year peer median to suspect wrong basis
  PEER_PERSIST    <- 0.5    # ...in at least this share of the unit's years

  # CLR basis-change ratio, per county-year. Shared by all three layers.
  cl <- unique(b[, .(county, year, clr)])[order(county, year)]
  cl[, clr_prev := shift(clr), by = county]
  cl[, clr_ratio := clr / clr_prev]
  # A REASSESSMENT is not "the CLR moved a lot" -- the CLR drifts every year
  # with the market. It is the CLR being reset TO market value: assessed =
  # market, so the ratio lands at ~1.0 having been below it. That is the
  # definitional signature and it catches both the dramatic resets (Luzerne
  # 0.05 -> 1.00) and the quiet ones in counties already near market
  # (Allegheny 0.855 -> 1.00), which any jump-magnitude threshold misses.
  cl[, reassess := !is.na(clr_prev) & clr >= AT_MARKET & clr_prev < AT_MARKET]

  audit <- list()

  repair_layer <- function(id, mill_col, lbl) {
    keep <- unique(c(id, "county"))
    d <- unique(b[, c(keep, "year", mill_col), with = FALSE])
    setnames(d, mill_col, "mills")
    d <- merge(d, cl[, .(county, year, clr, clr_ratio, reassess)], by = c("county", "year"))
    setorderv(d, c(id, "year"))
    d[, eff := mills / 1000 * clr * 100]

    # ---- Process 1: reassessment rebasing -------------------------------
    d[, mill_prev := shift(mills), by = id]
    # In a reassessment year the basis changed by clr_ratio, so a unit that
    # rebased correctly shows its millage FALL by roughly that factor. A unit
    # still reporting on the old basis did not. Testing the millage response
    # (not the effective-rate ratio) is essential: because eff = mills x clr,
    # any unit with unchanged millage has eff_ratio identically equal to
    # clr_ratio, so matching those two ratios fires on ordinary CLR drift and
    # over-corrects roughly a third of the panel.
    # resid = 1 if the unit rebased exactly; = clr_ratio if it did not move at
    # all. Flag when it is well above 1, and use resid itself as the correction
    # so partial rebasing is handled too.
    d[, resid := (mills / mill_prev) * clr_ratio]
    d[, stranded := reassess %in% TRUE & !is.na(mill_prev) & mill_prev > 0 &
                    is.finite(resid) & resid > 1 / REBASE_TOL]
    d[, f1 := fifelse(stranded %in% TRUE, resid, 1)]
    d[, f1 := cummax(f1), by = id]          # carry forward: still on old basis
    # Release once the unit's own millage finally drops onto the new basis.
    d[, released := !is.na(mill_prev) & mills / mill_prev < 0.5]
    d[, rel := cumsum(fifelse(released %in% TRUE, 1L, 0L)), by = id]
    d[rel > 0, f1 := 1]
    d[, eff_p1 := eff / f1]

    # ---- Process 2: peer consistency ------------------------------------
    # Peers are the other units of the SAME layer in the same county-year,
    # all multiplied by the identical CLR. Counties have no within-county
    # peers, so their peer set is all other counties that year.
    peer_key <- if (identical(id, "county")) "year" else c("county", "year")
    d[, peer_med := median(eff_p1, na.rm = TRUE), by = peer_key]
    d[, peer_n   := .N, by = peer_key]
    d[, peer_ratio := eff_p1 / peer_med]
    # Persistent, not a one-off: a unit genuinely on the wrong basis is far
    # from its peers in most of its years.
    d[, persist := mean(peer_ratio > PEER_RATIO, na.rm = TRUE), by = id]
    d[, f2 := 1]
    d[peer_n >= 3 & persist >= PEER_PERSIST & peer_ratio > PEER_RATIO,
      f2 := peer_ratio]
    # One-off spikes above the ceiling are parse errors (decimal shifts);
    # correct them by their own peer ratio too, but log separately.
    d[, parse_err := peer_n >= 3 & persist < PEER_PERSIST &
                     eff_p1 > PLAUSIBLE_PP & peer_ratio > PEER_RATIO]
    d[parse_err %in% TRUE, f2 := peer_ratio]
    d[, eff_fixed := eff_p1 / f2]

    corrected <- d[f1 > 1 | f2 > 1]
    if (nrow(corrected)) {
      a <- corrected[, .(layer = lbl, unit = as.character(get(id)), county, year,
                         mills, clr,
                         eff_raw = eff, factor_p1 = f1, factor_p2 = f2,
                         eff_repaired = eff_fixed,
                         process = fifelse(f1 > 1 & f2 > 1, "both",
                                   fifelse(f1 > 1, "reassessment", "peer")))]
      audit[[lbl]] <<- a
    }
    message(sprintf(
      "[rebase] %-16s units corrected %4d | rows %5d | >%dpp: %5d -> %4d | max %8.2f -> %6.2f",
      lbl, uniqueN(corrected[[id]]), nrow(corrected), PLAUSIBLE_PP,
      sum(d$eff > PLAUSIBLE_PP, na.rm = TRUE),
      sum(d$eff_fixed > PLAUSIBLE_PP, na.rm = TRUE),
      max(d$eff, na.rm = TRUE), max(d$eff_fixed, na.rm = TRUE)))
    out <- unique(d[, c(id, "year", "eff_fixed"), with = FALSE], by = c(id, "year"))
    setnames(out, "eff_fixed", "eff_new")
    out
  }

  m_fix <- repair_layer("muni_id", "muni_re_total_mills", "MUNICIPAL")
  s_fix <- repair_layer("sd_id",   "sd_re_total_mills",   "SCHOOL DISTRICT")
  c_fix <- repair_layer("county",  "cty_re_total_mills",  "COUNTY")

  # Write the repaired panel: same schema as the raw file, effective rates
  # replaced (back to DECIMAL units, as the raw file stores them).
  out <- copy(b)
  out <- merge(out, m_fix[, .(muni_id, year, m_new = eff_new)], by = c("muni_id","year"), all.x = TRUE)
  out <- merge(out, s_fix[, .(sd_id,   year, s_new = eff_new)], by = c("sd_id","year"),   all.x = TRUE)
  out <- merge(out, c_fix[, .(county,  year, c_new = eff_new)], by = c("county","year"),  all.x = TRUE)
  out[!is.na(m_new), muni_eff_rate   := m_new / 100]
  out[!is.na(s_new), sd_eff_rate     := s_new / 100]
  out[!is.na(c_new), county_eff_rate := c_new / 100]
  out[, eff_property_tax_rate := rowSums(cbind(muni_eff_rate, sd_eff_rate, county_eff_rate), na.rm = TRUE)]
  out[, c("m_new","s_new","c_new") := NULL]

  fwrite(out, file.path(DERIVED_DIR, "pa_property_burden_panel_repaired.csv"))
  message("[rebase] wrote data/derived/pa_property_burden_panel_repaired.csv")

  if (length(audit)) {
    log_dt <- rbindlist(audit, fill = TRUE)
    fwrite(log_dt, file.path(RESULTS_DIR, "property_rebase_log.csv"))
    message(sprintf("[rebase] audit log: %s corrected unit-years -> data/results/property_rebase_log.csv",
                    format(nrow(log_dt), big.mark = ",")))
    print(log_dt[, .N, by = .(layer, process)][order(layer, process)])
  }
})
