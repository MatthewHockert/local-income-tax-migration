# q3_stacked.R
# Robustness for Q3 staggered-treatment concern, on inc-only single-event + NC.
#
# Models, all with ±5-year window:
#   (1) Sun-Abraham on inc-only single-event vs NC                 [SA]
#   (2) Stacked DiD, binary post-treatment indicator               [stacked binary]
#   (3) Stacked DiD, continuous resident_eit treatment             [stacked continuous]
#
# Each produces a 3-panel event-study figure on share_live_in,
# log(Resident workers), log(Total jobs).
#
# Why this works: NC munis never change rates, so they are valid controls in
# every stack window. No "already-treated comparing to later-treated"
# negative-weighting problem.

suppressMessages({
  library(data.table); library(fixest); library(ggplot2)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
fig_dir     <- file.path(project_dir, "figures")
max_year    <- 2019
K           <- 5  # event window: -K to +K

# ---- Load + prep panel (mirror of run_slide_models.R) ----
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
pa_samp <- pa_panel[total_jobs>50 & !is.na(resident_eit) & !is.na(sd_eit) &
                    !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) &
                    !is.na(log_state_aid) & !is.na(share_eit) & !is.na(share_re)]

# ---- Group classification ----
ev <- pa_rate_panel[!is.na(cousub_fips) & event_type %in% c("increase","decrease"),
                    .(any_inc=any(event_type=="increase"),
                      any_dec=any(event_type=="decrease"),
                      n_inc=sum(event_type=="increase"),
                      n_dec=sum(event_type=="decrease")),
                    by=cousub_fips]
ev[, group := fifelse(any_inc & !any_dec, "Inc",
              fifelse(!any_inc & any_dec, "Dec",
              fifelse(any_inc & any_dec, "FF", "NC")))]
gd <- merge(data.table(work_city=unique(pa_samp$work_city)),
            ev[, .(work_city=cousub_fips, group, n_inc, n_dec)],
            by="work_city", all.x=TRUE)
gd[is.na(group), group := "NC"]; gd[is.na(n_inc), n_inc := 0]; gd[is.na(n_dec), n_dec := 0]

# Single-event sample
single_inc_munis <- gd[group == "Inc" & n_inc == 1, work_city]
nc_munis         <- gd[group == "NC", work_city]
sample_munis     <- c(single_inc_munis, nc_munis)
pa_se <- pa_samp[work_city %in% sample_munis]

# Treatment year per single-event Inc muni (year of the rate increase)
treat_years <- pa_rate_panel[!is.na(cousub_fips) & event_type == "increase" &
                              cousub_fips %in% single_inc_munis,
                              .(treat_year = min(year)), by = cousub_fips]
setnames(treat_years, "cousub_fips", "work_city")
pa_se <- merge(pa_se, treat_years, by = "work_city", all.x = TRUE)
pa_se[, treated_unit := !is.na(treat_year)]

cat(sprintf("Sample: %s rows, %d munis (%d single-Inc + %d NC)\n",
            format(nrow(pa_se), big.mark=","), uniqueN(pa_se$work_city),
            length(single_inc_munis), length(nc_munis)))

# Outcome facet labels (match prior figures)
outcome_labs <- c(share_live_in = "share_live_in",
                  log_live_in   = "log(Resident workers)",
                  log_total_jobs = "log(Total jobs)")
outs <- names(outcome_labs)
muni_red  <- "#C0392B"
sd_blue   <- "#2C7FB8"

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
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.6) +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    scale_color_manual(values = color_map) +
    labs(title = title, subtitle = subtitle,
         x = xlab, y = "Coefficient", color = NULL) +
    es_theme
}

ctrls <- "log_police + log_streets + log_debt + log_state_aid + share_eit + share_re"

# ============================================================================
# (1) Sun-Abraham on single-event Inc + NC
# ============================================================================
pa_sa <- copy(pa_se)
pa_sa[, cohort := ifelse(is.na(treat_year), 10000L, as.integer(treat_year))]

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

mk_fe <- function(lhs, rhs)
  as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls, " | work_city + year"))

es_sa <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_fe(o, "sunab(cohort, year)"),
             data = pa_sa, cluster = ~work_city)
  d <- extract_sa_es(m, "Inc-only (single-event)")
  d[, outcome := factor(o, levels = outs, labels = outcome_labs[outs])]
  d[abs(rel_year) <= K]
}))
p_sa <- build_es_plot(
  es_sa,
  title    = "Sun-Abraham event study: single-event Inc-only vs NC",
  subtitle = sprintf("68 single-event Inc munis + %d NC; ±%d-year window; 95%% CI",
                     length(nc_munis), K),
  xlab     = "Years relative to first muni rate increase",
  color_map = c("Inc-only (single-event)" = muni_red))
ggsave(file.path(fig_dir, "q3_sa_single_event_es.png"), p_sa,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_sa_single_event_es.png\n")

# ============================================================================
# (2) Stacked DiD, binary post-treatment
# ============================================================================
# For each treated cohort c, build a stack: c's treated muni + ALL NC munis,
# all observed t in [c-K, c+K]. With NC controls only, no future-treatment
# overlap risk.
treated_cohorts <- sort(unique(treat_years$treat_year))

build_stack <- function(c_year) {
  win <- (c_year - K):(c_year + K)
  treated_muni <- treat_years[treat_year == c_year, work_city]
  d_t <- pa_se[work_city %in% treated_muni & year %in% win]
  d_c <- pa_se[work_city %in% nc_munis  & year %in% win]
  d   <- rbind(d_t, d_c)
  if (nrow(d) == 0) return(NULL)
  d[, stack    := c_year]
  d[, rel_year := year - c_year]
  d[, treat    := work_city %in% treated_muni]
  d[, post     := as.integer(rel_year >= 0)]
  d[, treat_post := as.integer(treat & post == 1)]
  d
}

stack_dt <- rbindlist(lapply(treated_cohorts, build_stack), fill = TRUE)
stack_dt[, stack_muni := paste0(stack, "_", work_city)]
stack_dt[, stack_year := paste0(stack, "_", year)]

cat(sprintf("Stacked panel: %s rows, %d sub-experiments, %d unique munis\n",
            format(nrow(stack_dt), big.mark=","),
            uniqueN(stack_dt$stack), uniqueN(stack_dt$work_city)))

# ATT (single coefficient) and ES (event-time interactions)
mk_stack <- function(lhs, rhs, fe = "stack_muni + stack_year")
  as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls, " | ", fe))

# 2a. Stacked binary ATT
cat("\n===== Stacked DiD (binary post): ATT =====\n")
sb_atts <- lapply(outs, function(o) {
  m <- feols(mk_stack(o, "treat_post"),
             data = stack_dt, cluster = ~work_city)
  list(out = o, est = m$coeftable["treat_post", "Estimate"],
       se = m$coeftable["treat_post", "Std. Error"],
       p = m$coeftable["treat_post", "Pr(>|t|)"], n = nobs(m))
})
sb_tab <- rbindlist(lapply(sb_atts, function(x)
  data.table(outcome=x$out, est=round(x$est,4), se=round(x$se,4),
             p=round(x$p,4), n=x$n)))
print(sb_tab)

# 2b. Stacked binary event study
es_sb <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_stack(o,
       "i(rel_year, treat, ref = -1)"),
       data = stack_dt, cluster = ~work_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+:treat$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+):treat$", "\\1", nms[is_es]))
  data.table(rel_year = rel,
             est = ct[is_es, "Estimate"], se = ct[is_es, "Std. Error"],
             cohort = "Stacked binary",
             outcome = factor(o, levels=outs, labels=outcome_labs[outs]))
}))
# Add the dropped reference period as zero
es_sb_ref <- unique(es_sb[, .(outcome, cohort)])[, .(rel_year = -1, est = 0, se = 0,
                                                      cohort = cohort,
                                                      outcome = outcome)]
es_sb <- rbind(es_sb, es_sb_ref)[order(outcome, rel_year)][abs(rel_year) <= K]

p_sb <- build_es_plot(
  es_sb,
  title    = "Stacked DiD event study (binary post-treatment): single-event Inc vs NC",
  subtitle = sprintf("Stacked sub-experiments per cohort; ±%d-year window; 95%% CI", K),
  xlab     = "Years relative to first muni rate increase",
  color_map = c("Stacked binary" = muni_red))
ggsave(file.path(fig_dir, "q3_stacked_binary_es.png"), p_sb,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_stacked_binary_es.png\n")

# ============================================================================
# (3) Stacked DiD, continuous resident_eit
# ============================================================================
cat("\n===== Stacked DiD (continuous resident_eit): ATE =====\n")
# Aggregate ATE: in-stack drate from pre-treatment baseline
stack_dt[, base_rate := mean(resident_eit[rel_year < 0], na.rm = TRUE),
         by = .(stack, work_city)]
stack_dt[, drate := resident_eit - base_rate]

sc_atts <- lapply(outs, function(o) {
  m <- feols(mk_stack(o, "drate"), data = stack_dt, cluster = ~work_city)
  list(out=o, est=m$coeftable["drate","Estimate"],
       se=m$coeftable["drate","Std. Error"],
       p=m$coeftable["drate","Pr(>|t|)"], n=nobs(m))
})
sc_tab <- rbindlist(lapply(sc_atts, function(x)
  data.table(outcome=x$out, est=round(x$est,4), se=round(x$se,4),
             p=round(x$p,4), n=x$n)))
print(sc_tab)

# 3b. Stacked continuous event study.
# Use a TIME-INVARIANT per-unit dose: the rate change AT the event year
# (post minus pre baseline). For NC munis dose = 0. Then the
# i(rel_year, dose) interactions identify the per-pp effect at each rel_year
# and are NOT zero-by-construction in pre-periods.
stack_dt[, post_rate := mean(resident_eit[rel_year >= 0], na.rm = TRUE),
         by = .(stack, work_city)]
stack_dt[, dose := ifelse(treat, post_rate - base_rate, 0)]

es_sc <- rbindlist(lapply(outs, function(o) {
  m <- feols(mk_stack(o, "i(rel_year, dose, ref = -1)"),
             data = stack_dt, cluster = ~work_city)
  ct <- m$coeftable
  nms <- rownames(ct)
  is_es <- grepl("^rel_year::-?[[:digit:]]+:dose$", nms)
  rel <- as.integer(sub("^rel_year::(-?[[:digit:]]+):dose$", "\\1", nms[is_es]))
  data.table(rel_year = rel,
             est = ct[is_es, "Estimate"], se = ct[is_es, "Std. Error"],
             cohort = "Stacked continuous",
             outcome = factor(o, levels=outs, labels=outcome_labs[outs]))
}))
es_sc_ref <- unique(es_sc[, .(outcome, cohort)])[, .(rel_year = -1, est = 0, se = 0,
                                                      cohort = cohort,
                                                      outcome = outcome)]
es_sc <- rbind(es_sc, es_sc_ref)[order(outcome, rel_year)][abs(rel_year) <= K]

p_sc <- build_es_plot(
  es_sc,
  title    = "Stacked DiD event study (continuous Δrate): single-event Inc vs NC",
  subtitle = sprintf("Coefficient = effect per pp of within-stack rate change; ±%d-year window; 95%% CI", K),
  xlab     = "Years relative to first muni rate increase",
  color_map = c("Stacked continuous" = sd_blue))
ggsave(file.path(fig_dir, "q3_stacked_continuous_es.png"), p_sc,
       width = 11, height = 4, dpi = 300)
cat("Saved: q3_stacked_continuous_es.png\n")

cat("\nDone.\n")
