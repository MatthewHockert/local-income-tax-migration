# q3_nonres.R
# Nonresident-side parallel to q3_robustness.R + q3_stacked.R.
# Treatment: muni_nonres_eit (the work muni's nonresident rate).
# Outcome:   log(live_outside) -- workers in the muni who don't live there.
#            Plus log(total_jobs) for comparison.
#
# Four estimators, each with aggregate coefficient + event-study figure:
#   (1) Single-event TWFE  (drop multi-event nonres-Inc munis)
#   (2) DCDH continuous, multi-event (no bootstrap; analytical SE)
#   (3) Stacked DiD, binary post
#   (4) Stacked DiD, continuous dose

suppressMessages({
  library(data.table); library(fixest); library(ggplot2)
  library(DIDmultiplegtDYN)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
fig_dir     <- file.path(project_dir, "figures")
max_year    <- 2019
K           <- 5

# --- Load + prep panel (mirror of run_slide_models.R) ---
pa_panel      <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
pa_rate_panel <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x="muni_id", by.y="municipality_id", all.x=TRUE)
afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
log_pos <- function(x) ifelse(as.numeric(x)>0, log(as.numeric(x)), NA_real_)
fc <- afr[!is.na(cousub_fips), {
  total_rev <- as.numeric(ifelse(!is.na(total_revenues),total_revenues,revenues))
  eit_rev <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),earned_income_tax_revenues,earned_income_tax))
  re_tax  <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),real_estate_tax_revenues,real_estate_tax))
  list(cousub_fips=cousub_fips, year=yr,
       log_police=log_pos(ifelse(!is.na(police_expenditures),police_expenditures,police)),
       log_streets=log_pos(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),`public_works-highways_and_streets_expenditures`,streets_and_roads)),
       log_debt=log_pos(as.numeric(total_debt)),
       log_state_aid=log_pos(ifelse(!is.na(`intergovernmental_revenues-state_government`),`intergovernmental_revenues-state_government`,state_govt)),
       share_eit=ifelse(total_rev>0,eit_rev/total_rev,NA_real_),
       share_re=ifelse(total_rev>0,re_tax/total_rev,NA_real_))
}]
pa_panel <- merge(pa_panel, fc, by.x=c("work_city","year"), by.y=c("cousub_fips","year"), all.x=TRUE)

# Outcomes for nonres side
pa_panel[, log_live_outside := ifelse(live_outside > 0, log(live_outside), NA_real_)]
pa_panel[, share_nonres     := ifelse(total_jobs > 0, live_outside / total_jobs, NA_real_)]

pa_samp <- pa_panel[total_jobs > 50 & !is.na(muni_nonres_eit) & !is.na(resident_eit) &
                    !is.na(sd_eit) &
                    !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) &
                    !is.na(log_state_aid) & !is.na(share_eit) & !is.na(share_re)]

# --- Classify nonresident-rate events ---
rate_nr <- pa_rate_panel[!is.na(cousub_fips) & !is.na(muni_nonres_eit)][order(cousub_fips, year)]
rate_nr[, nr_lag := shift(muni_nonres_eit), by = cousub_fips]
rate_nr[, nr_event := fifelse(is.na(nr_lag), NA_character_,
                       fifelse(muni_nonres_eit - nr_lag >  1e-6, "increase",
                       fifelse(nr_lag - muni_nonres_eit >  1e-6, "decrease", NA_character_)))]
ev_nr <- rate_nr[!is.na(nr_event),
                 .(any_inc = any(nr_event == "increase"),
                   any_dec = any(nr_event == "decrease"),
                   n_inc   = sum(nr_event == "increase"),
                   n_dec   = sum(nr_event == "decrease"),
                   first_inc = if (any(nr_event == "increase"))
                                  as.integer(min(year[nr_event == "increase"]))
                                else NA_integer_),
                 by = cousub_fips]
ev_nr[, group := fifelse(any_inc & !any_dec, "Inc",
              fifelse(!any_inc & any_dec, "Dec",
              fifelse(any_inc & any_dec, "FF", "NC")))]
gd <- merge(data.table(work_city = unique(pa_samp$work_city)),
            ev_nr[, .(work_city = cousub_fips, group, n_inc, n_dec, first_inc)],
            by = "work_city", all.x = TRUE)
gd[is.na(group), group := "NC"]
gd[is.na(n_inc), n_inc := 0]; gd[is.na(n_dec), n_dec := 0]

cat("Nonresident-rate group composition (estimation sample munis):\n")
print(gd[, .N, by = group])
cat("\nNonresident Inc-only by # of increases:\n")
print(gd[group == "Inc", .N, by = n_inc][order(n_inc)])

single_inc_munis <- gd[group == "Inc" & n_inc == 1, work_city]
nc_munis         <- gd[group == "NC", work_city]
multi_inc_munis  <- gd[group == "Inc" & n_inc >= 2, work_city]
cat(sprintf("\nSingle-event nonres-Inc: %d  |  NC: %d  |  Multi-event Inc dropped: %d\n",
            length(single_inc_munis), length(nc_munis), length(multi_inc_munis)))

# Treat year for single-event Inc munis = year of first nonres increase
treat_years <- ev_nr[cousub_fips %in% single_inc_munis, .(work_city = cousub_fips, treat_year = first_inc)]
pa_se <- pa_samp[work_city %in% c(single_inc_munis, nc_munis)]
pa_se <- merge(pa_se, treat_years, by = "work_city", all.x = TRUE)
pa_se[, treated_unit := !is.na(treat_year)]

# Outcome facet labels
outcome_labs <- c(share_nonres = "share_nonres",
                  log_live_outside = "log(Nonresident workers)",
                  log_total_jobs   = "log(Total jobs)")
outs <- names(outcome_labs)
muni_red <- "#C0392B"; sd_blue <- "#2C7FB8"

es_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

build_es_plot <- function(es_dt, title, subtitle, xlab, color_map) {
  es_dt <- es_dt[!is.na(outcome) & !is.na(est)]
  es_dt[, outcome := factor(as.character(outcome), levels = unname(outcome_labs))]
  ggplot(es_dt, aes(rel_year, est, color = cohort, group = cohort)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = muni_red) +
    geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se),
                  width = 0, linewidth = 0.4) +
    geom_line(linewidth = 0.5) + geom_point(size = 1.6) +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    scale_color_manual(values = color_map) +
    labs(title = title, subtitle = subtitle,
         x = xlab, y = "Coefficient", color = NULL) +
    es_theme
}

ctrls <- "log_police + log_streets + log_debt + log_state_aid + share_eit + share_re"
mk_fe <- function(lhs, rhs)
  as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls, " | work_city + year"))

extract_sa_es <- function(model, label) {
  ct <- model$coeftable
  nms <- rownames(ct)
  is_sa <- grepl("^year::-?[[:digit:]]+$", nms)
  rel  <- as.integer(sub("^year::(-?[[:digit:]]+)$", "\\1", nms[is_sa]))
  data.table(rel_year = rel,
             est = ct[is_sa, "Estimate"],
             se  = ct[is_sa, "Std. Error"],
             cohort = label)[order(rel_year)]
}

# ============================================================================
# (1) Single-event TWFE
# ============================================================================
cat("\n===== (1) Single-event TWFE =====\n")
m1_sn <- feols(mk_fe("share_nonres",     "muni_nonres_eit"), data = pa_se, cluster = ~work_city)
m1_lo <- feols(mk_fe("log_live_outside", "muni_nonres_eit"), data = pa_se, cluster = ~work_city)
m1_lj <- feols(mk_fe("log_total_jobs",   "muni_nonres_eit"), data = pa_se, cluster = ~work_city)
print(etable(m1_sn, m1_lo, m1_lj,
             headers = c("share_nonres", "log(NonRes)", "log(TotJobs)"),
             keep = "%muni_nonres_eit"))

# (1) ES with Sun-Abraham on single-event nonres-Inc + NC
pa_sa <- copy(pa_se)
pa_sa[, cohort := ifelse(is.na(treat_year), 10000L, as.integer(treat_year))]
es_sa <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_fe(o, "sunab(cohort, year)"), data = pa_sa, cluster = ~work_city)
  d <- extract_sa_es(m, "Nonres Inc-only (single-event)")
  d[, outcome := factor(o, levels = outs, labels = outcome_labs[outs])]
  d[abs(rel_year) <= K]
}))
p_sa <- build_es_plot(es_sa,
  title    = "(Nonres) Sun-Abraham event study: single-event Inc-only vs NC",
  subtitle = sprintf("%d single-event nonres-Inc munis + %d NC; ±%d-yr; 95%% CI",
                     length(single_inc_munis), length(nc_munis), K),
  xlab     = "Years relative to first nonres rate increase",
  color_map = c("Nonres Inc-only (single-event)" = muni_red))
ggsave(file.path(fig_dir, "q3_nonres_sa_single_event_es.png"), p_sa,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_nonres_sa_single_event_es.png\n")

# ============================================================================
# (2) DCDH continuous, multi-event: inc + NC
# ============================================================================
cat("\n===== (2) DCDH continuous on inc-only + NC =====\n")
inc_or_nc <- gd[group %in% c("NC", "Inc"), work_city]
pa_dcdh <- pa_samp[work_city %in% inc_or_nc]
pa_dcdh[, work_city_int := as.integer(as.factor(work_city))]
pa_dcdh_df <- as.data.frame(pa_dcdh)

dcdh_extract_es <- function(res, label) {
  if (is.null(res$results$Effects) || is.null(res$results$Placebos))
    return(data.table())
  eff <- as.data.table(res$results$Effects)
  plb <- as.data.table(res$results$Placebos)
  eff[, rel_year := seq_len(.N)]
  plb[, rel_year := -seq_len(.N)]
  d <- rbind(
    eff[, .(rel_year, est = Estimate, se = SE)],
    plb[, .(rel_year, est = Estimate, se = SE)],
    data.table(rel_year = 0, est = 0, se = 0)
  )
  d[, cohort := label]
  d[order(rel_year)]
}

dcdh_runs <- list()
for (o in outs) {
  cat("DCDH:", o, "...\n")
  res <- did_multiplegt_dyn(
    df = pa_dcdh_df, outcome = o,
    group = "work_city_int", time = "year",
    treatment = "muni_nonres_eit",
    effects = 8, placebo = 5, continuous = 1,
    cluster = "work_city_int", graph_off = TRUE)
  d <- dcdh_extract_es(res, "DCDH (nonres)")
  d[, outcome := factor(o, levels = outs, labels = outcome_labs[outs])]
  dcdh_runs[[o]] <- list(res = res, es = d)
}
es_dcdh <- rbindlist(lapply(dcdh_runs, function(x) x$es))
p_dcdh <- build_es_plot(es_dcdh,
  title    = "(Nonres) DCDH event study: continuous, inc + NC",
  subtitle = "Effects (1..8) + placebos (-5..-1); analytical SE; 95% CI",
  xlab     = "Years relative to first nonres rate increase",
  color_map = c("DCDH (nonres)" = muni_red))
ggsave(file.path(fig_dir, "q3_nonres_dcdh_es.png"), p_dcdh,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_nonres_dcdh_es.png\n")

# ATEs
ate_tab <- rbindlist(lapply(outs, function(o) {
  ate <- dcdh_runs[[o]]$res$results$ATE
  data.table(outcome = o,
             est = round(ate[1, "Estimate"], 4),
             se  = round(ate[1, "SE"], 4),
             lb  = round(ate[1, "LB CI"], 4),
             ub  = round(ate[1, "UB CI"], 4),
             N   = ate[1, "N"], switchers = ate[1, "Switchers"])
}))
cat("DCDH ATE:\n"); print(ate_tab)

# ============================================================================
# (3-4) Stacked DiD: build stacks (single-event Inc + NC, ±5)
# ============================================================================
build_stack <- function(c_year) {
  win <- (c_year - K):(c_year + K)
  treated_muni <- treat_years[treat_year == c_year, work_city]
  d_t <- pa_se[work_city %in% treated_muni & year %in% win]
  d_c <- pa_se[work_city %in% nc_munis      & year %in% win]
  d   <- rbind(d_t, d_c)
  if (nrow(d) == 0) return(NULL)
  d[, stack    := c_year]
  d[, rel_year := year - c_year]
  d[, treat    := work_city %in% treated_muni]
  d[, post     := as.integer(rel_year >= 0)]
  d[, treat_post := as.integer(treat & post == 1)]
  d
}
treated_cohorts <- sort(unique(treat_years$treat_year))
stack_dt <- rbindlist(lapply(treated_cohorts, build_stack), fill = TRUE)
stack_dt[, stack_muni := paste0(stack, "_", work_city)]
stack_dt[, stack_year := paste0(stack, "_", year)]

cat(sprintf("\nStacked nonres panel: %s rows, %d sub-experiments, %d unique munis\n",
            format(nrow(stack_dt), big.mark=","),
            uniqueN(stack_dt$stack), uniqueN(stack_dt$work_city)))

mk_stack <- function(lhs, rhs)
  as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls,
                    " | stack_muni + stack_year"))

# (3) Stacked binary
cat("\n===== (3) Stacked DiD binary post =====\n")
sb_atts <- lapply(outs, function(o) {
  m <- feols(mk_stack(o, "treat_post"), data = stack_dt, cluster = ~work_city)
  data.table(outcome=o, est=round(m$coeftable["treat_post","Estimate"],4),
             se=round(m$coeftable["treat_post","Std. Error"],4),
             p=round(m$coeftable["treat_post","Pr(>|t|)"],4), n=nobs(m))
})
print(rbindlist(sb_atts))

es_sb <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_stack(o, "i(rel_year, treat, ref = -1)"),
             data = stack_dt, cluster = ~work_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+:treat$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+):treat$", "\\1", nms[is_es]))
  data.table(rel_year = rel, est = ct[is_es,"Estimate"], se = ct[is_es,"Std. Error"],
             cohort = "Stacked binary (nonres)",
             outcome = factor(o, levels=outs, labels=outcome_labs[outs]))
}))
es_sb_ref <- unique(es_sb[, .(outcome, cohort)])[, .(rel_year=-1, est=0, se=0, cohort, outcome)]
es_sb <- rbind(es_sb, es_sb_ref)[order(outcome, rel_year)][abs(rel_year) <= K]
p_sb <- build_es_plot(es_sb,
  title    = "(Nonres) Stacked DiD event study (binary post)",
  subtitle = sprintf("Stacked sub-experiments per cohort; ±%d-yr; 95%% CI", K),
  xlab     = "Years relative to first nonres rate increase",
  color_map = c("Stacked binary (nonres)" = muni_red))
ggsave(file.path(fig_dir, "q3_nonres_stacked_binary_es.png"), p_sb,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_nonres_stacked_binary_es.png\n")

# (4) Stacked continuous: aggregate ATE + dose-based ES
cat("\n===== (4) Stacked DiD continuous Δrate =====\n")
stack_dt[, base_rate := mean(muni_nonres_eit[rel_year < 0], na.rm = TRUE),
         by = .(stack, work_city)]
stack_dt[, drate := muni_nonres_eit - base_rate]
sc_atts <- lapply(outs, function(o) {
  m <- feols(mk_stack(o, "drate"), data = stack_dt, cluster = ~work_city)
  data.table(outcome=o, est=round(m$coeftable["drate","Estimate"],4),
             se=round(m$coeftable["drate","Std. Error"],4),
             p=round(m$coeftable["drate","Pr(>|t|)"],4), n=nobs(m))
})
print(rbindlist(sc_atts))

# ES uses time-invariant per-unit dose (post avg minus pre baseline)
stack_dt[, post_rate := mean(muni_nonres_eit[rel_year >= 0], na.rm = TRUE),
         by = .(stack, work_city)]
stack_dt[, dose := ifelse(treat, post_rate - base_rate, 0)]

es_sc <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_stack(o, "i(rel_year, dose, ref = -1)"),
             data = stack_dt, cluster = ~work_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+:dose$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+):dose$", "\\1", nms[is_es]))
  data.table(rel_year = rel, est = ct[is_es,"Estimate"], se = ct[is_es,"Std. Error"],
             cohort = "Stacked continuous (nonres)",
             outcome = factor(o, levels=outs, labels=outcome_labs[outs]))
}))
es_sc_ref <- unique(es_sc[, .(outcome, cohort)])[, .(rel_year=-1, est=0, se=0, cohort, outcome)]
es_sc <- rbind(es_sc, es_sc_ref)[order(outcome, rel_year)][abs(rel_year) <= K]
p_sc <- build_es_plot(es_sc,
  title    = "(Nonres) Stacked DiD event study (continuous Δrate)",
  subtitle = sprintf("Coefficient = effect per pp of dose at event year; ±%d-yr; 95%% CI", K),
  xlab     = "Years relative to first nonres rate increase",
  color_map = c("Stacked continuous (nonres)" = sd_blue))
ggsave(file.path(fig_dir, "q3_nonres_stacked_continuous_es.png"), p_sc,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_nonres_stacked_continuous_es.png\n")

cat("\nDone.\n")
