#!/usr/bin/env Rscript
# 3_repair_property_rates.R -- fix effective property-tax rates where millage and
# the CLR are on different assessment bases (e.g. Wilkes-Barre after 2009).
# In:  data/derived/pa_property_burden_panel.csv
# Out: data/derived/pa_property_burden_panel_repaired.csv, data/results/property_rebase_log.csv

suppressMessages(library(data.table))
source(here::here("scripts", "helpers", "config.R"))

local({
  burden <- fread(file.path(DERIVED_DIR, "pa_property_burden_panel.csv"))

  plausible_pp     <- 5     # a single layer above this is impossible (real ones run ~0.1-3 pp)
  at_market        <- 0.95  # a CLR at or above this means assessed value was reset to market
  rebase_tolerance <- 0.5   # stranded if millage did not fall to within 1 / 0.5 of the reset
  peer_ratio_max   <- 5     # this many times the county-year peer median is suspect
  peer_persistence <- 0.5   # ...in at least this share of the unit's years

  # a reassessment = the CLR reaching market from below (not just a big CLR move)
  clr_changes <- unique(burden[, .(county, year, clr)])[order(county, year)]
  clr_changes[, clr_prev := shift(clr), by = county]
  clr_changes[, clr_ratio := clr / clr_prev]
  clr_changes[, reassess := !is.na(clr_prev) & clr >= at_market & clr_prev < at_market]

  audit <- list()

  repair_layer <- function(unit_col, mills_col, layer_label) {
    layer <- unique(burden[, c(unique(c(unit_col, "county")), "year", mills_col), with = FALSE])
    setnames(layer, mills_col, "mills")
    layer <- merge(layer, clr_changes[, .(county, year, clr, clr_ratio, reassess)], by = c("county", "year"))
    setorderv(layer, c(unit_col, "year"))
    layer[, eff := mills / 1000 * clr * 100]

    # ---- 1. reassessment: a unit whose millage did not fall when the CLR reset is stranded
    layer[, mills_prev := shift(mills), by = unit_col]
    layer[, rebase_residual := (mills / mills_prev) * clr_ratio]
    layer[, stranded := reassess %in% TRUE & !is.na(mills_prev) & mills_prev > 0 &
                        is.finite(rebase_residual) & rebase_residual > 1 / rebase_tolerance]
    layer[, reassess_factor := fifelse(stranded %in% TRUE, rebase_residual, 1)]
    layer[, reassess_factor := cummax(reassess_factor), by = unit_col]   # still on the old basis
    layer[, released := !is.na(mills_prev) & mills / mills_prev < 0.5]    # millage finally rebased
    layer[, n_releases := cumsum(fifelse(released %in% TRUE, 1L, 0L)), by = unit_col]
    layer[n_releases > 0, reassess_factor := 1]
    layer[, eff_after_reassess := eff / reassess_factor]

    # ---- 2. peers: a unit far above its county-year peers (same CLR) is on the wrong basis
    peer_key <- if (identical(unit_col, "county")) "year" else c("county", "year")
    layer[, peer_median := median(eff_after_reassess, na.rm = TRUE), by = peer_key]
    layer[, n_peers     := .N, by = peer_key]
    layer[, peer_ratio  := eff_after_reassess / peer_median]
    layer[, share_far_from_peers := mean(peer_ratio > peer_ratio_max, na.rm = TRUE), by = unit_col]
    layer[, peer_factor := 1]
    layer[n_peers >= 3 & share_far_from_peers >= peer_persistence & peer_ratio > peer_ratio_max,
          peer_factor := peer_ratio]
    layer[, parse_error := n_peers >= 3 & share_far_from_peers < peer_persistence &
                           eff_after_reassess > plausible_pp & peer_ratio > peer_ratio_max]
    layer[parse_error %in% TRUE, peer_factor := peer_ratio]
    layer[, eff_fixed := eff_after_reassess / peer_factor]

    corrected <- layer[reassess_factor > 1 | peer_factor > 1]
    if (nrow(corrected)) {
      audit[[layer_label]] <<- corrected[, .(
        layer = layer_label, unit = as.character(get(unit_col)), county, year, mills, clr,
        eff_raw = eff, factor_p1 = reassess_factor, factor_p2 = peer_factor, eff_repaired = eff_fixed,
        process = fifelse(reassess_factor > 1 & peer_factor > 1, "both",
                  fifelse(reassess_factor > 1, "reassessment", "peer")))]
    }
    message(sprintf(
      "[rebase] %-16s units corrected %4d | rows %5d | >%dpp: %5d -> %4d | max %8.2f -> %6.2f",
      layer_label, uniqueN(corrected[[unit_col]]), nrow(corrected), plausible_pp,
      sum(layer$eff > plausible_pp, na.rm = TRUE), sum(layer$eff_fixed > plausible_pp, na.rm = TRUE),
      max(layer$eff, na.rm = TRUE), max(layer$eff_fixed, na.rm = TRUE)))
    repaired <- unique(layer[, c(unit_col, "year", "eff_fixed"), with = FALSE], by = c(unit_col, "year"))
    setnames(repaired, "eff_fixed", "eff_new")
    repaired
  }

  muni_fixed   <- repair_layer("muni_id", "muni_re_total_mills", "MUNICIPAL")
  sd_fixed     <- repair_layer("sd_id",   "sd_re_total_mills",   "SCHOOL DISTRICT")
  county_fixed <- repair_layer("county",  "cty_re_total_mills",  "COUNTY")

  # Same schema as the input with the three layer rates replaced. The total uses
  # na.rm, so all-missing muni-years get 0 (the models use only the layers).
  repaired <- copy(burden)
  repaired <- merge(repaired, muni_fixed[, .(muni_id, year, muni_new = eff_new)], by = c("muni_id", "year"), all.x = TRUE)
  repaired <- merge(repaired, sd_fixed[, .(sd_id, year, sd_new = eff_new)], by = c("sd_id", "year"), all.x = TRUE)
  repaired <- merge(repaired, county_fixed[, .(county, year, county_new = eff_new)], by = c("county", "year"), all.x = TRUE)
  repaired[!is.na(muni_new),   muni_eff_rate   := muni_new / 100]
  repaired[!is.na(sd_new),     sd_eff_rate     := sd_new / 100]
  repaired[!is.na(county_new), county_eff_rate := county_new / 100]
  repaired[, eff_property_tax_rate := rowSums(cbind(muni_eff_rate, sd_eff_rate, county_eff_rate), na.rm = TRUE)]
  repaired[, c("muni_new", "sd_new", "county_new") := NULL]
  fwrite(repaired, file.path(DERIVED_DIR, "pa_property_burden_panel_repaired.csv"))
  message("[rebase] wrote data/derived/pa_property_burden_panel_repaired.csv")

  if (length(audit)) {
    audit_log <- rbindlist(audit, fill = TRUE)
    fwrite(audit_log, file.path(RESULTS_DIR, "property_rebase_log.csv"))
    message(sprintf("[rebase] audit log: %s corrected unit-years -> data/results/property_rebase_log.csv",
                    format(nrow(audit_log), big.mark = ",")))
    print(audit_log[, .N, by = .(layer, process)][order(layer, process)])
  }
})
