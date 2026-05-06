# q3_piecewise.R
# Piecewise binding model on the OD-pair panel (no 80km filter; LODES OD
# distribution itself defines which pairs exist).
#
# Binding rule (work fully binds when equal):
#   home binds  iff  home_stack > nonres_work
#   work binds  iff  home_stack ≤ nonres_work
#
# Year-over-year per-pair binding-state classification:
#   ext_to_home: t-1 work-binds, t home-binds        (channel switched)
#   ext_to_work: t-1 home-binds, t work-binds        (channel switched)
#   int_home:    home-binds in both t-1 and t        (intensive on home channel)
#   int_work:    work-binds in both t-1 and t        (intensive on work channel)
#
# Decomposition regression:
#   log_flow_AB,t  =  β_eh * 1{ext_to_home}
#                   + β_ew * 1{ext_to_work}
#                   + β_ih * Δhome_stack_t  * 1{int_home}
#                   + β_iw * Δnonres_work_t * 1{int_work}
#                   + pair_id  + year  + ε
#
# Predictions:
#   β_ih < 0:  raising home stack while home binds reduces flow into A from B
#              (residents of B move; some leave A flows).
#   β_iw < 0:  raising work nonres while work binds reduces flow into A from B
#              (commuters into A drop when their bill rises).
#   β_eh, β_ew: jump in flow associated with the channel switch itself
#              (sign is theory-ambiguous; magnitude tells us if the switch
#              moment matters relative to dose changes).

suppressMessages({
  library(data.table); library(fixest); library(ggplot2)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
fig_dir     <- file.path(project_dir, "figures")
max_year    <- 2019

# ---- Load block-level OD + crosswalk + rate panel ----
od    <- as.data.table(readRDS(file.path(data_dir, "lodes_block/pa_od_S000_main.rds")))
xw    <- as.data.table(readRDS(file.path(data_dir, "pa_block_xwalk.rds")))
rate  <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

setnames(od, c("h_geocode", "w_geocode"), c("h_block", "w_block"), skip_absent = TRUE)
od[, year := as.integer(year)]
od <- od[year <= max_year]
home_xw <- xw[, .(h_block = block_geoid, home_city = cousub_fips)]
work_xw <- xw[, .(w_block = block_geoid, work_city = cousub_fips)]
od <- merge(od, home_xw, by = "h_block", all.x = TRUE)
od <- merge(od, work_xw, by = "w_block", all.x = TRUE)
od <- od[!is.na(home_city) & !is.na(work_city)]
pair_panel <- od[, .(flow = sum(S000, na.rm = TRUE)),
                 by = .(work_city, home_city, year)]
rm(od); gc()

cat(sprintf("Pair-panel raw (no distance filter): %s rows, %d work x %d home\n",
            format(nrow(pair_panel), big.mark=","),
            uniqueN(pair_panel$work_city), uniqueN(pair_panel$home_city)))

# ---- Attach rates: home stack from B, nonres_work + resident_work from A ----
rate_h <- rate[!is.na(cousub_fips) & year <= max_year,
               .(home_city = cousub_fips, year,
                 tau_res_h = resident_eit, tau_sd_h = sd_eit)]
rate_w <- rate[!is.na(cousub_fips) & year <= max_year,
               .(work_city = cousub_fips, year,
                 tau_nr_w   = muni_nonres_eit,
                 tau_res_w  = resident_eit,
                 tau_sd_w   = sd_eit)]
pair_panel <- merge(pair_panel, rate_h, by = c("home_city", "year"))
pair_panel <- merge(pair_panel, rate_w, by = c("work_city", "year"))
pair_panel[, home_stack := tau_res_h + tau_sd_h]
pair_panel <- pair_panel[!is.na(home_stack)]

# Two versions of nonres_work:
#   tau_nr_w           — observed only; pairs with NA dropped (strict)
#   tau_nr_w_filled    — fall back to work-muni's RESIDENT rate when nonres
#                        is missing. Standard "no wedge" assumption: when
#                        DCED reports a muni as collecting only the
#                        resident rate (no separate nonres line), treat
#                        nonresidents as paying that same rate.
pair_panel[, tau_nr_w_filled := ifelse(is.na(tau_nr_w), tau_res_w, tau_nr_w)]
pair_panel[, nr_imputed       := as.integer(is.na(tau_nr_w))]

cat(sprintf("\nObs with NA nonres before imputation: %s (%.1f%%)\n",
            format(sum(is.na(pair_panel$tau_nr_w)), big.mark=","),
            100*mean(is.na(pair_panel$tau_nr_w))))

# ============================================================================
# Run-the-piecewise function: parametrized on which nonres column to use
# AND optional thin-pair filter (avg flow > min_flow over the pair's panel)
# AND optional work_muni × year FE replacing year FE
# ============================================================================
run_piecewise <- function(panel, nonres_col, label,
                          min_flow = 0, use_workyear_fe = FALSE) {
  cat("\n##############################################\n")
  cat("##  PIECEWISE RUN:", label, "\n")
  cat("##  min_flow =", min_flow, " | work x year FE =", use_workyear_fe, "\n")
  cat("##############################################\n")
  d <- copy(panel)
  d[, tau_nr_w_use := get(nonres_col)]
  d <- d[!is.na(tau_nr_w_use)]
  d[, work_binds := as.integer(home_stack <= tau_nr_w_use)]
  d[, home_binds := 1 - work_binds]
  setorder(d, work_city, home_city, year)

  # Thin-pair filter: drop pairs whose average flow over the panel is too small
  if (min_flow > 0) {
    d[, avg_flow := mean(flow, na.rm = TRUE), by = .(work_city, home_city)]
    keep_pairs <- d[avg_flow > min_flow, unique(paste0(work_city, "_", home_city))]
    d <- d[paste0(work_city, "_", home_city) %in% keep_pairs]
    cat(sprintf("After thin-pair filter (avg flow > %s): %s rows, %d pairs\n",
                min_flow, format(nrow(d), big.mark=","),
                uniqueN(paste0(d$work_city, "_", d$home_city))))
  }

  d[, work_binds_lag := shift(work_binds),  by = .(work_city, home_city)]
  d[, home_stack_lag := shift(home_stack),  by = .(work_city, home_city)]
  d[, tau_nr_lag     := shift(tau_nr_w_use), by = .(work_city, home_city)]
  d[, dhs := home_stack - home_stack_lag]
  d[, dnr := tau_nr_w_use - tau_nr_lag]
  d[, transition := fcase(
    is.na(work_binds_lag),                                 NA_character_,
    work_binds_lag == 1 & work_binds == 0,                 "ext_to_home",
    work_binds_lag == 0 & work_binds == 1,                 "ext_to_work",
    work_binds_lag == 1 & work_binds == 1,                 "int_work",
    work_binds_lag == 0 & work_binds == 0,                 "int_home")]

  post <- d[year >= 2012 & !is.na(transition)]
  cat(sprintf("\nPair-years post-2012 used: %s\n", format(nrow(post), big.mark=",")))
  cat("Transition distribution:\n")
  print(post[, .N, by = transition][, pct := round(100*N/sum(N), 2)][])

  post[, pair_id := paste0(work_city, "_", home_city)]
  post[, work_year := paste0(work_city, "_", year)]
  post[, log_flow := ifelse(flow > 0, log(flow), NA_real_)]
  post[, ihs_flow := log(flow + sqrt(flow^2 + 1))]
  post[, bill     := pmax(home_stack, tau_nr_w_use)]
  post[, bill_lag := shift(bill), by = .(work_city, home_city)]
  post[, dbill    := bill - bill_lag]
  post[, dbill_int_home := ifelse(transition == "int_home"   & !is.na(dbill), dbill, 0)]
  post[, dbill_int_work := ifelse(transition == "int_work"   & !is.na(dbill), dbill, 0)]
  post[, dbill_ext_h    := ifelse(transition == "ext_to_home" & !is.na(dbill), dbill, 0)]
  post[, dbill_ext_w    := ifelse(transition == "ext_to_work" & !is.na(dbill), dbill, 0)]
  post[, home_binding   := factor(transition,
                                   levels = c("int_work","int_home","ext_to_home","ext_to_work"))]

  fe_str <- if (use_workyear_fe) "pair_id + work_year" else "pair_id + year"

  cat(sprintf("\n--- (A) Aggregate Δbill, FE = %s ---\n", fe_str))
  m_agg <- feols(as.formula(paste0("log_flow ~ dbill | ", fe_str)),
                 data = post, cluster = ~work_city)
  print(round(m_agg$coeftable, 4)); cat(sprintf("N = %d\n", nobs(m_agg)))

  cat(sprintf("\n--- (B) Decomposition log(flow), FE = %s ---\n", fe_str))
  m_dec_log <- feols(as.formula(paste0(
      "log_flow ~ dbill_int_home + dbill_int_work + dbill_ext_h + dbill_ext_w | ",
      fe_str)),
    data = post, cluster = ~work_city)
  print(round(m_dec_log$coeftable, 4))
  cat(sprintf("N = %d  pairs = %d\n", nobs(m_dec_log),
              uniqueN(post[!is.na(log_flow), pair_id])))

  # Cleanest piecewise test: home_binding indicator interacted with Δτ_nr
  # under work-muni×year FE (absorbs τ_nr_A directly).
  if (use_workyear_fe) {
    cat(sprintf("\n--- (D) Cleanest piecewise: i(home_binding, dnr) | %s ---\n", fe_str))
    m_clean <- feols(as.formula(paste0(
        "log_flow ~ i(home_binding, dnr, ref = 'int_work') | ", fe_str)),
      data = post, cluster = ~work_city)
    print(round(m_clean$coeftable, 4))
    cat(sprintf("N = %d\n", nobs(m_clean)))
  }

  invisible(list(m_agg = m_agg, m_dec_log = m_dec_log,
                 post = post, label = label))
}

# Run versions:
#   STRICT pair-FE + year FE              (the original)
#   STRICT pair-FE + work_muni × year FE  (fix muni-A-year confound)
#   STRICT pair-FE + work_muni × year FE + drop thin pairs (avg flow > 5)
#   FILLED pair-FE + work_muni × year FE  (imputation robustness)
res_v1 <- run_piecewise(pair_panel, "tau_nr_w",
                        "v1: STRICT, pair + year FE  (original)",
                        min_flow = 0, use_workyear_fe = FALSE)
res_v2 <- run_piecewise(pair_panel, "tau_nr_w",
                        "v2: STRICT, pair + work x year FE",
                        min_flow = 0, use_workyear_fe = TRUE)
res_v3 <- run_piecewise(pair_panel, "tau_nr_w",
                        "v3: STRICT, pair + work x year FE, drop thin pairs (avg>5)",
                        min_flow = 5, use_workyear_fe = TRUE)
res_v4 <- run_piecewise(pair_panel, "tau_nr_w_filled",
                        "v4: FILLED, pair + work x year FE",
                        min_flow = 0, use_workyear_fe = TRUE)

saveRDS(pair_panel, file.path(data_dir, "pa_pair_panel.rds"))
cat("\nSaved pair panel to data/pa_pair_panel.rds\n")
cat("\nDone.\n")
