# q3_residence.R
# Residence-side parallel to q3_robustness.R + q3_stacked.R + q3_nonres.R.
# Unit:      home cousub × year (pa_residence_panel.rds)
# Treatment: resident_eit (HOME muni's resident rate)
# Outcomes:
#   log(residents_employed)   -- count of employed people living in the muni
#   log(residents_work_outside) -- residents who commute out
#   share_work_outside        -- fraction of residents working outside the home muni
#
# Four estimators, each with aggregate coefficient + event-study figure:
#   (1) Single-event TWFE  (drop multi-event Inc munis)
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

# --- Load residence panel ---
res_panel     <- as.data.table(readRDS(file.path(data_dir, "pa_residence_panel.rds")))[year <= max_year]
pa_rate_panel <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

# Fiscal controls (merged on home muni, same definition as workplace panel)
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
res_panel <- merge(res_panel, fc, by.x=c("home_city","year"), by.y=c("cousub_fips","year"), all.x=TRUE)

# Estimation sample
pa_samp <- res_panel[residents_employed > 50 & !is.na(resident_eit) & !is.na(sd_eit) &
                     !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) &
                     !is.na(log_state_aid) & !is.na(share_eit) & !is.na(share_re)]

# --- Classify resident-rate events ---
rate_r <- pa_rate_panel[!is.na(cousub_fips) & !is.na(resident_eit)][order(cousub_fips, year)]
rate_r[, r_lag := shift(resident_eit), by = cousub_fips]
rate_r[, r_event := fifelse(is.na(r_lag), NA_character_,
                     fifelse(resident_eit - r_lag >  1e-6, "increase",
                     fifelse(r_lag - resident_eit >  1e-6, "decrease", NA_character_)))]
ev_r <- rate_r[!is.na(r_event),
               .(any_inc = any(r_event == "increase"),
                 any_dec = any(r_event == "decrease"),
                 n_inc   = sum(r_event == "increase"),
                 n_dec   = sum(r_event == "decrease"),
                 first_inc = if (any(r_event == "increase"))
                                as.integer(min(year[r_event == "increase"]))
                              else NA_integer_),
               by = cousub_fips]
ev_r[, group := fifelse(any_inc & !any_dec, "Inc",
            fifelse(!any_inc & any_dec, "Dec",
            fifelse(any_inc & any_dec, "FF", "NC")))]
gd <- merge(data.table(home_city = unique(pa_samp$home_city)),
            ev_r[, .(home_city = cousub_fips, group, n_inc, n_dec, first_inc)],
            by = "home_city", all.x = TRUE)
gd[is.na(group), group := "NC"]
gd[is.na(n_inc), n_inc := 0]; gd[is.na(n_dec), n_dec := 0]

cat("Resident-rate group composition (estimation sample home munis):\n")
print(gd[, .N, by = group])
cat("\nResident Inc-only by # of increases:\n")
print(gd[group == "Inc", .N, by = n_inc][order(n_inc)])

single_inc_munis <- gd[group == "Inc" & n_inc == 1, home_city]
nc_munis         <- gd[group == "NC", home_city]
multi_inc_munis  <- gd[group == "Inc" & n_inc >= 2, home_city]
cat(sprintf("\nSingle-event resident-Inc: %d  |  NC: %d  |  Multi-event Inc dropped: %d\n",
            length(single_inc_munis), length(nc_munis), length(multi_inc_munis)))

treat_years <- ev_r[cousub_fips %in% single_inc_munis, .(home_city = cousub_fips, treat_year = first_inc)]
pa_se <- pa_samp[home_city %in% c(single_inc_munis, nc_munis)]
pa_se <- merge(pa_se, treat_years, by = "home_city", all.x = TRUE)
pa_se[, treated_unit := !is.na(treat_year)]

# Outcome facet labels
outcome_labs <- c(share_work_outside = "share_work_outside",
                  log_residents      = "log(Residents employed)",
                  log_work_outside   = "log(Residents working outside)")
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
  as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls, " | home_city + year"))

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
m1_sw <- feols(mk_fe("share_work_outside", "resident_eit"), data = pa_se, cluster = ~home_city)
m1_lr <- feols(mk_fe("log_residents",      "resident_eit"), data = pa_se, cluster = ~home_city)
m1_lo <- feols(mk_fe("log_work_outside",   "resident_eit"), data = pa_se, cluster = ~home_city)
print(etable(m1_sw, m1_lr, m1_lo,
             headers = c("share_out", "log(Residents)", "log(WorkOut)"),
             keep = "%resident_eit"))

# (1) ES with Sun-Abraham on single-event resident-Inc + NC
pa_sa <- copy(pa_se)
pa_sa[, cohort := ifelse(is.na(treat_year), 10000L, as.integer(treat_year))]
es_sa <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_fe(o, "sunab(cohort, year)"), data = pa_sa, cluster = ~home_city)
  d <- extract_sa_es(m, "Resident Inc-only (single-event)")
  d[, outcome := factor(o, levels = outs, labels = outcome_labs[outs])]
  d[abs(rel_year) <= K]
}))
p_sa <- build_es_plot(es_sa,
  title    = "(Residence) Sun-Abraham event study: single-event Inc-only vs NC",
  subtitle = sprintf("%d single-event resident-Inc munis + %d NC; ±%d-yr; 95%% CI",
                     length(single_inc_munis), length(nc_munis), K),
  xlab     = "Years relative to first home rate increase",
  color_map = c("Resident Inc-only (single-event)" = muni_red))
ggsave(file.path(fig_dir, "q3_residence_sa_single_event_es.png"), p_sa,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_residence_sa_single_event_es.png\n")

# ============================================================================
# (2) DCDH continuous, multi-event: inc + NC
# ============================================================================
cat("\n===== (2) DCDH continuous on inc-only + NC =====\n")
inc_or_nc <- gd[group %in% c("NC", "Inc"), home_city]
pa_dcdh <- pa_samp[home_city %in% inc_or_nc]
pa_dcdh[, home_city_int := as.integer(as.factor(home_city))]
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
    group = "home_city_int", time = "year",
    treatment = "resident_eit",
    effects = 8, placebo = 5, continuous = 1,
    cluster = "home_city_int", graph_off = TRUE)
  d <- dcdh_extract_es(res, "DCDH (residence)")
  d[, outcome := factor(o, levels = outs, labels = outcome_labs[outs])]
  dcdh_runs[[o]] <- list(res = res, es = d)
}
es_dcdh <- rbindlist(lapply(dcdh_runs, function(x) x$es))
p_dcdh <- build_es_plot(es_dcdh,
  title    = "(Residence) DCDH event study: continuous, inc + NC",
  subtitle = "Effects (1..8) + placebos (-5..-1); analytical SE; 95% CI",
  xlab     = "Years relative to first home rate increase",
  color_map = c("DCDH (residence)" = muni_red))
ggsave(file.path(fig_dir, "q3_residence_dcdh_es.png"), p_dcdh,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_residence_dcdh_es.png\n")

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
  treated_muni <- treat_years[treat_year == c_year, home_city]
  d_t <- pa_se[home_city %in% treated_muni & year %in% win]
  d_c <- pa_se[home_city %in% nc_munis     & year %in% win]
  d   <- rbind(d_t, d_c)
  if (nrow(d) == 0) return(NULL)
  d[, stack    := c_year]
  d[, rel_year := year - c_year]
  d[, treat    := home_city %in% treated_muni]
  d[, post     := as.integer(rel_year >= 0)]
  d[, treat_post := as.integer(treat & post == 1)]
  d
}
treated_cohorts <- sort(unique(treat_years$treat_year))
stack_dt <- rbindlist(lapply(treated_cohorts, build_stack), fill = TRUE)
stack_dt[, stack_muni := paste0(stack, "_", home_city)]
stack_dt[, stack_year := paste0(stack, "_", year)]

cat(sprintf("\nStacked residence panel: %s rows, %d sub-experiments, %d unique munis\n",
            format(nrow(stack_dt), big.mark=","),
            uniqueN(stack_dt$stack), uniqueN(stack_dt$home_city)))

mk_stack <- function(lhs, rhs)
  as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls,
                    " | stack_muni + stack_year"))

# (3) Stacked binary
cat("\n===== (3) Stacked DiD binary post =====\n")
sb_atts <- lapply(outs, function(o) {
  m <- feols(mk_stack(o, "treat_post"), data = stack_dt, cluster = ~home_city)
  data.table(outcome=o, est=round(m$coeftable["treat_post","Estimate"],4),
             se=round(m$coeftable["treat_post","Std. Error"],4),
             p=round(m$coeftable["treat_post","Pr(>|t|)"],4), n=nobs(m))
})
print(rbindlist(sb_atts))

es_sb <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_stack(o, "i(rel_year, treat, ref = -1)"),
             data = stack_dt, cluster = ~home_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+:treat$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+):treat$", "\\1", nms[is_es]))
  data.table(rel_year = rel, est = ct[is_es,"Estimate"], se = ct[is_es,"Std. Error"],
             cohort = "Stacked binary (residence)",
             outcome = factor(o, levels=outs, labels=outcome_labs[outs]))
}))
es_sb_ref <- unique(es_sb[, .(outcome, cohort)])[, .(rel_year=-1, est=0, se=0, cohort, outcome)]
es_sb <- rbind(es_sb, es_sb_ref)[order(outcome, rel_year)][abs(rel_year) <= K]
p_sb <- build_es_plot(es_sb,
  title    = "(Residence) Stacked DiD event study (binary post)",
  subtitle = sprintf("Stacked sub-experiments per cohort; ±%d-yr; 95%% CI", K),
  xlab     = "Years relative to first home rate increase",
  color_map = c("Stacked binary (residence)" = muni_red))
ggsave(file.path(fig_dir, "q3_residence_stacked_binary_es.png"), p_sb,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_residence_stacked_binary_es.png\n")

# (4) Stacked continuous: aggregate ATE + dose-based ES
cat("\n===== (4) Stacked DiD continuous Δrate =====\n")
stack_dt[, base_rate := mean(resident_eit[rel_year < 0], na.rm = TRUE),
         by = .(stack, home_city)]
stack_dt[, drate := resident_eit - base_rate]
sc_atts <- lapply(outs, function(o) {
  m <- feols(mk_stack(o, "drate"), data = stack_dt, cluster = ~home_city)
  data.table(outcome=o, est=round(m$coeftable["drate","Estimate"],4),
             se=round(m$coeftable["drate","Std. Error"],4),
             p=round(m$coeftable["drate","Pr(>|t|)"],4), n=nobs(m))
})
print(rbindlist(sc_atts))

stack_dt[, post_rate := mean(resident_eit[rel_year >= 0], na.rm = TRUE),
         by = .(stack, home_city)]
stack_dt[, dose := ifelse(treat, post_rate - base_rate, 0)]

es_sc <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_stack(o, "i(rel_year, dose, ref = -1)"),
             data = stack_dt, cluster = ~home_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+:dose$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+):dose$", "\\1", nms[is_es]))
  data.table(rel_year = rel, est = ct[is_es,"Estimate"], se = ct[is_es,"Std. Error"],
             cohort = "Stacked continuous (residence)",
             outcome = factor(o, levels=outs, labels=outcome_labs[outs]))
}))
es_sc_ref <- unique(es_sc[, .(outcome, cohort)])[, .(rel_year=-1, est=0, se=0, cohort, outcome)]
es_sc <- rbind(es_sc, es_sc_ref)[order(outcome, rel_year)][abs(rel_year) <= K]
p_sc <- build_es_plot(es_sc,
  title    = "(Residence) Stacked DiD event study (continuous Δrate)",
  subtitle = sprintf("Coefficient = effect per pp of dose at event year; ±%d-yr; 95%% CI", K),
  xlab     = "Years relative to first home rate increase",
  color_map = c("Stacked continuous (residence)" = sd_blue))
ggsave(file.path(fig_dir, "q3_residence_stacked_continuous_es.png"), p_sc,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_residence_stacked_continuous_es.png\n")

cat("\nDone.\n")
