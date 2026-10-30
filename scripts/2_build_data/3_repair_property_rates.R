#!/usr/bin/env Rscript
# 3_repair_property_rates.R
# Fixes property-tax rates that blew up because a town kept reporting its old
# millage after its county reassessed.
# Reads data/derived/pa_property_burden_panel.csv and writes the repaired copy
# next to it, plus data/results/property_rebase_log.csv listing every fix.

suppressMessages(library(data.table))
source(here::here("scripts", "helpers", "config.R"))

local({
  burden <- fread(file.path(DERIVED_DIR, "pa_property_burden_panel.csv"))

  plausible_pp     <- 5     # real rates run 0.1-3 pp, so anything over 5 pp is a data error
  at_market        <- 0.95  # a CLR this close to 1 means the county just reassessed to market value
  rebase_tolerance <- 0.5   # a town counts as stuck if its millage fell less than half as much as it should have
  peer_ratio_max   <- 5     # 5x the other towns in the county that year is suspicious
  peer_persistence <- 0.5   # ...if it happens in at least half of the town's years

  # Find the reassessment years: the year a county's CLR jumps up to market value.
  # The CLR moves a little every year, so a big move alone does not count.
  clr_changes <- unique(burden[, .(county, year, clr)])[order(county, year)]
  clr_changes[, clr_prev := shift(clr), by = county]
  clr_changes[, clr_ratio := clr / clr_prev]
  clr_changes[, reassess := !is.na(clr_prev) & clr >= at_market & clr_prev < at_market]

  audit <- list()

  # Runs both fixes on one layer of tax (towns, school districts or counties).
  repair_layer <- function(unit_col, mills_col, layer_label) {
    layer <- unique(burden[, c(unique(c(unit_col, "county")), "year", mills_col), with = FALSE])
    setnames(layer, mills_col, "mills")
    layer <- merge(layer, clr_changes[, .(county, year, clr, clr_ratio, reassess)], by = c("county", "year"))
    setorderv(layer, c(unit_col, "year"))
    layer[, eff := mills / 1000 * clr * 100]   # effective rate, in percentage points

    # Fix 1: after a reassessment, a town's millage should drop by about as much as
    # the CLR rose. This ratio is about 1 if it did and big if the millage never moved.
    # (Comparing the rate's jump to the CLR's jump does not work: when millage stays
    # put, the two always match.)
    layer[, mills_prev := shift(mills), by = unit_col]
    layer[, rebase_residual := (mills / mills_prev) * clr_ratio]
    layer[, stranded := reassess %in% TRUE & !is.na(mills_prev) & mills_prev > 0 &
                        is.finite(rebase_residual) & rebase_residual > 1 / rebase_tolerance]
    layer[, reassess_factor := fifelse(stranded %in% TRUE, rebase_residual, 1)]
    # keep correcting in later years until the town's millage finally drops by half
    layer[, reassess_factor := cummax(reassess_factor), by = unit_col]
    layer[, released := !is.na(mills_prev) & mills / mills_prev < 0.5]
    layer[, n_releases := cumsum(fifelse(released %in% TRUE, 1L, 0L)), by = unit_col]
    layer[n_releases > 0, reassess_factor := 1]
    layer[, eff_after_reassess := eff / reassess_factor]

    # Fix 2: towns in the same county and year share one CLR, so their rates should
    # be similar. Counties are compared with all other counties that year.
    peer_key <- if (identical(unit_col, "county")) "year" else c("county", "year")
    layer[, peer_median := median(eff_after_reassess, na.rm = TRUE), by = peer_key]
    layer[, n_peers     := .N, by = peer_key]
    layer[, peer_ratio  := eff_after_reassess / peer_median]
    layer[, share_far_from_peers := mean(peer_ratio > peer_ratio_max, na.rm = TRUE), by = unit_col]
    layer[, peer_factor := 1]
    # far above its neighbors most years: the whole series is on the wrong basis
    layer[n_peers >= 3 & share_far_from_peers >= peer_persistence & peer_ratio > peer_ratio_max,
          peer_factor := peer_ratio]
    # far above in a single year and over 5 pp: a typo in the source (e.g. 6555 mills for 65.55)
    layer[, parse_error := n_peers >= 3 & share_far_from_peers < peer_persistence &
                           eff_after_reassess > plausible_pp & peer_ratio > peer_ratio_max]
    layer[parse_error %in% TRUE, peer_factor := peer_ratio]
    layer[, eff_fixed := eff_after_reassess / peer_factor]

    # log every row either fix changed
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

  # Put the fixed rates back into a copy of the input (converted back to decimals).
  repaired <- copy(burden)
  repaired <- merge(repaired, muni_fixed[, .(muni_id, year, muni_new = eff_new)], by = c("muni_id", "year"), all.x = TRUE)
  repaired <- merge(repaired, sd_fixed[, .(sd_id, year, sd_new = eff_new)], by = c("sd_id", "year"), all.x = TRUE)
  repaired <- merge(repaired, county_fixed[, .(county, year, county_new = eff_new)], by = c("county", "year"), all.x = TRUE)
  repaired[!is.na(muni_new),   muni_eff_rate   := muni_new / 100]
  repaired[!is.na(sd_new),     sd_eff_rate     := sd_new / 100]
  repaired[!is.na(county_new), county_eff_rate := county_new / 100]
  # note: a town missing all three layers gets a total of 0, not NA (the models only use the layers)
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
