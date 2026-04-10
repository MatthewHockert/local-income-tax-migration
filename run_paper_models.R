# Paper-Ready Models: Binary DiD, Dose-Response, Event Studies
# Unified estimation with clean treated/control definitions.
# Figures saved to figures/ for inclusion in paper/main.tex.
# ============================================================================

library(data.table)
library(fixest)
library(ggplot2)

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")
fig_dir <- file.path(project_dir, "figures")
max_year <- 2019

oh_panel <- readRDS(file.path(data_dir, "oh_panel.rds"))[year <= max_year]
pa_panel <- readRDS(file.path(data_dir, "pa_panel.rds"))[year <= max_year]
oh_rate_panel <- readRDS(file.path(data_dir, "oh_rate_panel.rds"))
pa_rate_panel <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))

## Merge fiscal controls into PA panel ----
pa_afr_file <- file.path(data_dir, "pa_afr_panel.csv")
if (file.exists(pa_afr_file)) {
  pa_afr_raw <- fread(pa_afr_file)
  pa_xwalk_afr <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
  pa_afr_raw[, muni_id := fifelse(!is.na(municipality_id), municipality_id, municipal_code)]
  pa_afr_raw <- merge(pa_afr_raw, pa_xwalk_afr, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
  pa_afr_raw[, yr := fifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]

  fiscal_controls <- pa_afr_raw[!is.na(cousub_fips), .(
    cousub_fips, year = yr,
    log_police = fifelse(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)) > 0,
      log(as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police))), NA_real_),
    log_streets = fifelse(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads)) > 0,
      log(as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
      `public_works-highways_and_streets_expenditures`, streets_and_roads))), NA_real_),
    log_debt = fifelse(as.numeric(total_debt) > 0, log(as.numeric(total_debt)), NA_real_),
    log_state_aid = fifelse(as.numeric(fifelse(!is.na(`intergovernmental_revenues-state_government`),
      `intergovernmental_revenues-state_government`, state_govt)) > 0,
      log(as.numeric(fifelse(!is.na(`intergovernmental_revenues-state_government`),
      `intergovernmental_revenues-state_government`, state_govt))), NA_real_)
  )]

  pa_panel <- merge(pa_panel, fiscal_controls,
    by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)
  rm(pa_afr_raw, fiscal_controls)
}

pa_ctrl_vars <- c("log_police", "log_streets", "log_debt", "log_state_aid")


## Helper: extract Sun-Abraham coefficients ----
extract_sa_coefs <- function(model, window = c(-5, 8)) {
  ct <- as.data.table(coeftable(model), keep.rownames = TRUE)
  setnames(ct, c("rn", "est", "se", "t", "p"))
  ct[, rel := as.integer(gsub("year::", "", rn))]
  ct <- ct[rel >= window[1] & rel <= window[2]]
  ref <- data.table(rn = "ref", est = 0, se = 0, t = 0, p = 1, rel = -1L)
  rbind(ct, ref)[order(rel)]
}

## Helper: plot Sun-Abraham event study ----
plot_sa <- function(model, title, subtitle = NULL, window = c(-5, 8),
                    color = "steelblue") {
  ct <- extract_sa_coefs(model, window)
  p <- ggplot(ct, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
      color = color, size = 0.4) +
    geom_line(color = color, linewidth = 0.4) +
    labs(x = "Years Relative to First Rate Change",
      y = expression(hat(mu)[l]),
      title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = seq(window[1], window[2], 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 11),
      plot.subtitle = element_text(size = 9, color = "grey40"))
  p
}


## Treatment groups ----

# PA
pa_inc_cities <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_dec_cities <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_never_changed <- pa_rate_panel[!is.na(cousub_fips),
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = cousub_fips][any_change == FALSE, cousub_fips]

# OH
oh_inc_cities <- oh_rate_panel[event_type == "increase", unique(muni_fips)]
oh_dec_cities <- oh_rate_panel[event_type == "decrease", unique(muni_fips)]
oh_never_changed <- oh_rate_panel[,
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = muni_fips][any_change == FALSE, muni_fips]
oh_no_tax_cities <- setdiff(
  oh_panel[, unique(work_city)],
  oh_rate_panel[rate > 0, unique(muni_fips)])

# Cohort lookups
pa_first_inc <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(first_inc = min(year)), by = cousub_fips]
pa_first_dec <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
  .(first_dec = min(year)), by = cousub_fips]
oh_first_inc <- oh_rate_panel[event_type == "increase",
  .(first_inc = min(year)), by = muni_fips]
oh_first_dec <- oh_rate_panel[event_type == "decrease",
  .(first_dec = min(year)), by = muni_fips]

print(paste("PA:", length(pa_inc_cities), "inc,", length(pa_dec_cities),
  "dec,", length(pa_never_changed), "NC"))
print(paste("OH:", length(oh_inc_cities), "inc,", length(oh_dec_cities),
  "dec,", length(oh_never_changed), "NC,", length(oh_no_tax_cities), "no-tax"))


# PA Binary DiD ----
# (1) Increasers vs never-changed, (2) Decreasers vs NC, (3) Both vs NC

## PA increasers vs never-changed ----
pa_inc_sample <- pa_panel[
  work_city %in% c(pa_inc_cities, pa_never_changed) &
  total_jobs > 50 & !is.na(resident_eit)]
pa_inc_sample <- merge(pa_inc_sample, pa_first_inc,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_inc_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

pa_sa_inc <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_inc_sample, cluster = ~work_city)

## PA decreasers vs never-changed ----
pa_dec_sample <- pa_panel[
  work_city %in% c(pa_dec_cities, pa_never_changed) &
  total_jobs > 50 & !is.na(resident_eit)]
pa_dec_sample <- merge(pa_dec_sample, pa_first_dec,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_dec_sample[, cohort := fifelse(is.na(first_dec), 10000L, as.integer(first_dec))]

pa_sa_dec <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = pa_dec_sample, cluster = ~work_city)

## PA both in one model ----
pa_both_sample <- pa_panel[
  work_city %in% c(pa_inc_cities, pa_dec_cities, pa_never_changed) &
  total_jobs > 50 & !is.na(resident_eit)]
pa_both_sample <- merge(pa_both_sample, pa_first_inc,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_both_sample <- merge(pa_both_sample, pa_first_dec,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_both_sample[, `:=`(
  has_inc = work_city %in% pa_inc_cities,
  has_dec = work_city %in% pa_dec_cities,
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

pa_asym <- feols(
  share_live_in ~ has_inc:post_inc + has_dec:post_dec | work_city + year,
  data = pa_both_sample, cluster = ~work_city)

## PA ATTs and output ----
pa_att_inc <- summary(pa_sa_inc, agg = "ATT")
pa_att_dec <- summary(pa_sa_dec, agg = "ATT")

print(paste("PA Increase ATT:", round(coef(pa_att_inc), 5),
  "(SE", round(se(pa_att_inc), 5), ")"))
print(paste("PA Decrease ATT:", round(coef(pa_att_dec), 5),
  "(SE", round(se(pa_att_dec), 5), ")"))
etable(pa_asym, headers = "PA: Both vs NC")

## PA event study figures ----
p_pa_es_inc <- plot_sa(pa_sa_inc,
  title = "PA: Rate Increases vs Never-Changed Controls",
  subtitle = paste0("N = ", uniqueN(pa_inc_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(pa_inc_sample[cohort == 10000, work_city]), " controls"),
  color = "firebrick")
print(p_pa_es_inc)
ggsave(file.path(fig_dir, "pa_es_increase.png"), p_pa_es_inc,
  width = 7, height = 4.5, dpi = 300)

p_pa_es_dec <- plot_sa(pa_sa_dec,
  title = "PA: Rate Decreases vs Never-Changed Controls",
  subtitle = paste0("N = ", uniqueN(pa_dec_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(pa_dec_sample[cohort == 10000, work_city]), " controls"),
  color = "steelblue")
print(p_pa_es_dec)
ggsave(file.path(fig_dir, "pa_es_decrease.png"), p_pa_es_dec,
  width = 7, height = 4.5, dpi = 300)


# PA Binary DiD with Fiscal Controls ----
# Same specs but adding log_police, log_streets, log_debt, log_state_aid
# Smaller sample (controls available for ~52% of obs) but tighter SEs

pa_inc_ctrl_sample <- pa_inc_sample[
  !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) & !is.na(log_state_aid)]

pa_dec_ctrl_sample <- pa_dec_sample[
  !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) & !is.na(log_state_aid)]

print(paste("PA inc with controls:", nrow(pa_inc_ctrl_sample), "obs,",
  uniqueN(pa_inc_ctrl_sample$work_city), "munis"))
print(paste("PA dec with controls:", nrow(pa_dec_ctrl_sample), "obs,",
  uniqueN(pa_dec_ctrl_sample$work_city), "munis"))

## PA increasers + controls ----
pa_sa_inc_ctrl <- feols(
  share_live_in ~ sunab(cohort, year) + log_police + log_streets +
    log_debt + log_state_aid | work_city + year,
  data = pa_inc_ctrl_sample, cluster = ~work_city)

## PA decreasers + controls ----
pa_sa_dec_ctrl <- feols(
  share_live_in ~ sunab(cohort, year) + log_police + log_streets +
    log_debt + log_state_aid | work_city + year,
  data = pa_dec_ctrl_sample, cluster = ~work_city)

## PA asymmetry + controls ----
pa_both_ctrl_sample <- pa_both_sample[
  !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) & !is.na(log_state_aid)]

pa_asym_ctrl <- feols(
  share_live_in ~ has_inc:post_inc + has_dec:post_dec + log_police +
    log_streets + log_debt + log_state_aid | work_city + year,
  data = pa_both_ctrl_sample, cluster = ~work_city)

pa_att_inc_ctrl <- summary(pa_sa_inc_ctrl, agg = "ATT")
pa_att_dec_ctrl <- summary(pa_sa_dec_ctrl, agg = "ATT")

print(paste("PA Inc + Controls ATT:", round(coef(pa_att_inc_ctrl), 5),
  "(SE", round(se(pa_att_inc_ctrl), 5), ")"))
print(paste("PA Dec + Controls ATT:", round(coef(pa_att_dec_ctrl), 5),
  "(SE", round(se(pa_att_dec_ctrl), 5), ")"))

## Compare with vs without controls ----
etable(pa_sa_inc, pa_sa_inc_ctrl, pa_sa_dec, pa_sa_dec_ctrl,
  headers = c("PA Inc", "PA Inc+Ctrl", "PA Dec", "PA Dec+Ctrl"),
  keep = "ATT")

etable(pa_asym, pa_asym_ctrl,
  headers = c("Asymmetry", "Asymmetry+Ctrl"))

## PA controlled event study figures ----
p_pa_es_inc_ctrl <- plot_sa(pa_sa_inc_ctrl,
  title = "PA: Rate Increases vs NC + Fiscal Controls",
  subtitle = paste0("N = ", uniqueN(pa_inc_ctrl_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(pa_inc_ctrl_sample[cohort == 10000, work_city]),
    " controls; with police, streets, debt, state aid"),
  color = "firebrick")
print(p_pa_es_inc_ctrl)
ggsave(file.path(fig_dir, "pa_es_increase_ctrl.png"), p_pa_es_inc_ctrl,
  width = 7, height = 4.5, dpi = 300)

p_pa_es_dec_ctrl <- plot_sa(pa_sa_dec_ctrl,
  title = "PA: Rate Decreases vs NC + Fiscal Controls",
  subtitle = paste0("N = ", uniqueN(pa_dec_ctrl_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(pa_dec_ctrl_sample[cohort == 10000, work_city]),
    " controls; with police, streets, debt, state aid"),
  color = "steelblue")
print(p_pa_es_dec_ctrl)
ggsave(file.path(fig_dir, "pa_es_decrease_ctrl.png"), p_pa_es_dec_ctrl,
  width = 7, height = 4.5, dpi = 300)


# OH Binary DiD: No-Tax Controls ----

## OH increasers vs no-tax ----
oh_inc_notax_sample <- oh_panel[
  work_city %in% c(oh_inc_cities, oh_no_tax_cities) & total_jobs > 50]
oh_inc_notax_sample <- merge(oh_inc_notax_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_inc_notax_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

oh_sa_inc_notax <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = oh_inc_notax_sample, cluster = ~work_city)

## OH decreasers vs no-tax ----
oh_dec_notax_sample <- oh_panel[
  work_city %in% c(oh_dec_cities, oh_no_tax_cities) & total_jobs > 50]
oh_dec_notax_sample <- merge(oh_dec_notax_sample, oh_first_dec,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_dec_notax_sample[, cohort := fifelse(is.na(first_dec), 10000L, as.integer(first_dec))]

oh_sa_dec_notax <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = oh_dec_notax_sample, cluster = ~work_city)

## OH both vs no-tax ----
oh_both_notax_sample <- oh_panel[
  work_city %in% c(oh_inc_cities, oh_dec_cities, oh_no_tax_cities) & total_jobs > 50]
oh_both_notax_sample <- merge(oh_both_notax_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_both_notax_sample <- merge(oh_both_notax_sample, oh_first_dec,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_both_notax_sample[, `:=`(
  has_inc = work_city %in% oh_inc_cities,
  has_dec = work_city %in% oh_dec_cities,
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

oh_asym_notax <- feols(
  share_live_in ~ has_inc:post_inc + has_dec:post_dec | work_city + year,
  data = oh_both_notax_sample, cluster = ~work_city)

oh_att_inc_notax <- summary(oh_sa_inc_notax, agg = "ATT")
oh_att_dec_notax <- summary(oh_sa_dec_notax, agg = "ATT")

print(paste("OH Inc vs No-Tax ATT:", round(coef(oh_att_inc_notax), 5),
  "(SE", round(se(oh_att_inc_notax), 5), ")"))
print(paste("OH Dec vs No-Tax ATT:", round(coef(oh_att_dec_notax), 5),
  "(SE", round(se(oh_att_dec_notax), 5), ")"))
etable(oh_asym_notax, headers = "OH: Both vs No-Tax")

## OH no-tax event study figures ----
p_oh_es_inc_notax <- plot_sa(oh_sa_inc_notax,
  title = "OH: Rate Increases vs No-Tax Controls",
  subtitle = paste0("N = ", uniqueN(oh_inc_notax_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(oh_inc_notax_sample[cohort == 10000, work_city]), " no-tax"),
  color = "steelblue")
print(p_oh_es_inc_notax)
ggsave(file.path(fig_dir, "oh_es_increase_notax.png"), p_oh_es_inc_notax,
  width = 7, height = 4.5, dpi = 300)

p_oh_es_dec_notax <- plot_sa(oh_sa_dec_notax,
  title = "OH: Rate Decreases vs No-Tax Controls",
  subtitle = paste0("N = ", uniqueN(oh_dec_notax_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(oh_dec_notax_sample[cohort == 10000, work_city]), " no-tax"),
  color = "steelblue")
print(p_oh_es_dec_notax)
ggsave(file.path(fig_dir, "oh_es_decrease_notax.png"), p_oh_es_dec_notax,
  width = 7, height = 4.5, dpi = 300)


# OH Binary DiD: Never-Changed Controls ----

## OH increasers vs never-changed ----
oh_inc_nc_sample <- oh_panel[
  work_city %in% c(oh_inc_cities, oh_never_changed) & total_jobs > 50]
oh_inc_nc_sample <- merge(oh_inc_nc_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_inc_nc_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

oh_sa_inc_nc <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = oh_inc_nc_sample, cluster = ~work_city)

## OH decreasers vs never-changed ----
oh_dec_nc_sample <- oh_panel[
  work_city %in% c(oh_dec_cities, oh_never_changed) & total_jobs > 50]
oh_dec_nc_sample <- merge(oh_dec_nc_sample, oh_first_dec,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_dec_nc_sample[, cohort := fifelse(is.na(first_dec), 10000L, as.integer(first_dec))]

oh_sa_dec_nc <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = oh_dec_nc_sample, cluster = ~work_city)

## OH both vs never-changed ----
oh_both_nc_sample <- oh_panel[
  work_city %in% c(oh_inc_cities, oh_dec_cities, oh_never_changed) & total_jobs > 50]
oh_both_nc_sample <- merge(oh_both_nc_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_both_nc_sample <- merge(oh_both_nc_sample, oh_first_dec,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_both_nc_sample[, `:=`(
  has_inc = work_city %in% oh_inc_cities,
  has_dec = work_city %in% oh_dec_cities,
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

oh_asym_nc <- feols(
  share_live_in ~ has_inc:post_inc + has_dec:post_dec | work_city + year,
  data = oh_both_nc_sample, cluster = ~work_city)

oh_att_inc_nc <- summary(oh_sa_inc_nc, agg = "ATT")
oh_att_dec_nc <- summary(oh_sa_dec_nc, agg = "ATT")

print(paste("OH Inc vs NC ATT:", round(coef(oh_att_inc_nc), 5),
  "(SE", round(se(oh_att_inc_nc), 5), ")"))
print(paste("OH Dec vs NC ATT:", round(coef(oh_att_dec_nc), 5),
  "(SE", round(se(oh_att_dec_nc), 5), ")"))
etable(oh_asym_nc, headers = "OH: Both vs NC")

## OH never-changed event study figures ----
p_oh_es_inc_nc <- plot_sa(oh_sa_inc_nc,
  title = "OH: Rate Increases vs Never-Changed Controls",
  subtitle = paste0("N = ", uniqueN(oh_inc_nc_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(oh_inc_nc_sample[cohort == 10000, work_city]), " NC"),
  color = "steelblue")
print(p_oh_es_inc_nc)
ggsave(file.path(fig_dir, "oh_es_increase_nc.png"), p_oh_es_inc_nc,
  width = 7, height = 4.5, dpi = 300)

p_oh_es_dec_nc <- plot_sa(oh_sa_dec_nc,
  title = "OH: Rate Decreases vs Never-Changed Controls",
  subtitle = paste0("N = ", uniqueN(oh_dec_nc_sample[cohort < 10000, work_city]),
    " treated, ", uniqueN(oh_dec_nc_sample[cohort == 10000, work_city]), " NC"),
  color = "steelblue")
print(p_oh_es_dec_nc)
ggsave(file.path(fig_dir, "oh_es_decrease_nc.png"), p_oh_es_dec_nc,
  width = 7, height = 4.5, dpi = 300)


# Dose-Response ----

## PA treated-only dose (among increasers) ----
pa_dose_sample <- merge(
  pa_panel[work_city %in% pa_inc_cities & total_jobs > 50 & !is.na(resident_eit)],
  pa_first_inc, by.x = "work_city", by.y = "cousub_fips")
pa_dose_sample[, post := as.integer(year >= first_inc)]

pa_dose_post <- feols(
  share_live_in ~ post | work_city + year,
  data = pa_dose_sample, cluster = ~work_city)
pa_dose_rate <- feols(
  share_live_in ~ post + resident_eit:post | work_city + year,
  data = pa_dose_sample, cluster = ~work_city)

## OH treated-only dose (among increasers) ----
oh_dose_sample <- merge(
  oh_panel[work_city %in% oh_inc_cities & total_jobs > 50][year <= max_year],
  oh_first_inc, by.x = "work_city", by.y = "muni_fips")
oh_dose_sample[, post := as.integer(year >= first_inc)]

oh_dose_post <- feols(
  share_live_in ~ post | work_city + year,
  data = oh_dose_sample, cluster = ~work_city)
oh_dose_rate <- feols(
  share_live_in ~ post + rate:post | work_city + year,
  data = oh_dose_sample, cluster = ~work_city)

## PA split dose (full sample) ----
pa_full_dose_sample <- pa_panel[total_jobs > 50 & !is.na(resident_eit)][year <= max_year]
pa_base_rate <- pa_rate_panel[!is.na(cousub_fips),
  .(base_rate = first(na.omit(resident_eit))), by = cousub_fips]
pa_full_dose_sample <- merge(pa_full_dose_sample, pa_base_rate,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_full_dose_sample[, cumul := resident_eit - fifelse(is.na(base_rate), 0, base_rate)]
pa_full_dose_sample[, `:=`(
  pos_change = fifelse(cumul > 0, cumul, 0),
  neg_change = fifelse(cumul < 0, abs(cumul), 0)
)]

pa_dose_split <- feols(
  share_live_in ~ pos_change + neg_change | work_city + year,
  data = pa_full_dose_sample, cluster = ~work_city)

## OH split dose (full sample) ----
oh_full_dose_sample <- oh_panel[total_jobs > 50][year <= max_year]
oh_base_rate <- oh_rate_panel[, .(base_rate = first(rate)), by = muni_fips]
oh_full_dose_sample <- merge(oh_full_dose_sample, oh_base_rate,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_full_dose_sample[is.na(base_rate), base_rate := 0]
oh_full_dose_sample[, cumul := rate - base_rate]
oh_full_dose_sample[, `:=`(
  pos_change = fifelse(cumul > 0, cumul, 0),
  neg_change = fifelse(cumul < 0, abs(cumul), 0)
)]

oh_dose_split <- feols(
  share_live_in ~ pos_change + neg_change | work_city + year,
  data = oh_full_dose_sample, cluster = ~work_city)

## Dose-response combined output ----
etable(pa_dose_post, pa_dose_rate, pa_dose_split,
  oh_dose_post, oh_dose_rate, oh_dose_split,
  headers = c("PA: Post", "PA: Post+Dose", "PA: Split",
    "OH: Post", "OH: Post+Dose", "OH: Split"))

beepr::beep()
