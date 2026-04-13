# Paper-Ready Models: Organized by Research Question
# Each question gets: Sun-Abraham ES + ATT, figures, and table output.
# ============================================================================

library(data.table)
library(fixest)
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
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


## Helper: extract Sun-Abraham coefficients for plotting ----

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
  ggplot(ct, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
      color = color, size = 0.4) +
    geom_line(color = color, linewidth = 0.4) +
    labs(x = "Years Relative to Treatment",
      y = expression(hat(mu)[l]),
      title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = seq(window[1], window[2], 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 11),
      plot.subtitle = element_text(size = 9, color = "grey40"))
}

## Helper: print ATT summary ----

print_att <- function(model, label) {
  att <- summary(model, agg = "ATT")
  print(paste0(label, ": ATT = ", round(coef(att), 5),
    " (SE ", round(se(att), 5), ")"))
}


# ============================================================================
# Treatment Groups ----
# ============================================================================

# OH: extensive margin (adoption)
oh_adopters <- oh_rate_panel[!is.na(rate_lag) & rate_lag == 0 & rate > 0, unique(muni_fips)]
oh_first_adoption <- oh_rate_panel[muni_fips %in% oh_adopters & rate_lag == 0 & rate > 0,
  .(first_adopt = min(year)), by = muni_fips]
# Rate at adoption (for Q1b dose)
oh_adopt_rate <- oh_rate_panel[muni_fips %in% oh_adopters & rate_lag == 0 & rate > 0,
  .SD[which.min(year)], by = muni_fips][, .(muni_fips, adopt_rate = rate)]

# OH: intensive margin (rate changes among already-taxed)
oh_rate_changers <- oh_rate_panel[event_type == "increase" & !is.na(rate_lag) & rate_lag > 0,
  unique(muni_fips)]
oh_first_rate_inc <- oh_rate_panel[muni_fips %in% oh_rate_changers & event_type == "increase" &
  rate_lag > 0, .(first_rate_inc = min(year)), by = muni_fips]

# OH: all increasers (adopters + rate changers, for reference)
oh_inc_cities <- oh_rate_panel[event_type == "increase", unique(muni_fips)]
oh_dec_cities <- oh_rate_panel[event_type == "decrease", unique(muni_fips)]
oh_never_changed <- oh_rate_panel[,
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = muni_fips][any_change == FALSE, muni_fips]
oh_no_tax_cities <- setdiff(
  oh_panel[, unique(work_city)],
  oh_rate_panel[rate > 0, unique(muni_fips)])
oh_stable_taxed <- setdiff(oh_never_changed, oh_no_tax_cities)

# PA groups
pa_inc_cities <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_dec_cities <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_never_changed <- pa_rate_panel[!is.na(cousub_fips),
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = cousub_fips][any_change == FALSE, cousub_fips]

# PA: extensive margin (adoption from 0 -> positive)
pa_rate_by_muni <- pa_rate_panel[!is.na(cousub_fips) & !is.na(resident_eit),
  .(ever_zero = any(resident_eit == 0),
    ever_positive = any(resident_eit > 0)), by = cousub_fips]
pa_adopters <- pa_rate_by_muni[ever_zero == TRUE & ever_positive == TRUE, cousub_fips]
pa_never_taxed <- pa_rate_by_muni[ever_zero == TRUE & ever_positive == FALSE, cousub_fips]

# PA first adoption year (first year with positive rate after having 0)
pa_first_adoption <- pa_rate_panel[!is.na(cousub_fips) & cousub_fips %in% pa_adopters &
  !is.na(resident_eit) & resident_eit > 0,
  .(first_adopt = min(year)), by = cousub_fips]
# Drop those "adopting" in 2000 (first year of data — can't confirm true adoption)
pa_first_adoption <- pa_first_adoption[first_adopt > 2002]
pa_adopters <- pa_first_adoption$cousub_fips

# Cohort lookups
pa_first_inc <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(first_inc = min(year)), by = cousub_fips]
pa_first_dec <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
  .(first_dec = min(year)), by = cousub_fips]
oh_first_inc <- oh_rate_panel[event_type == "increase",
  .(first_inc = min(year)), by = muni_fips]

print(paste("OH adopters (0->tax):", length(oh_adopters)))
print(paste("OH rate changers (already taxed):", length(oh_rate_changers)))
print(paste("OH all increasers:", length(oh_inc_cities)))
print(paste("OH no-tax controls:", length(oh_no_tax_cities)))
print(paste("OH stable-rate taxed:", length(oh_stable_taxed)))
print(paste("OH decreasers:", length(oh_dec_cities)))
print(paste("PA adopters (0->tax, post-2002):", length(pa_adopters)))
print(paste("PA never-taxed (always 0):", length(pa_never_taxed)))
print(paste("PA increasers:", length(pa_inc_cities)))
print(paste("PA decreasers:", length(pa_dec_cities)))
print(paste("PA never-changed:", length(pa_never_changed)))


# ============================================================================
# Q1a: Does adopting a tax cause sorting? (OH extensive margin) ----
# Treated: cities that went from 0 -> positive rate
# Control: cities that never had a tax (no-tax townships)
# ============================================================================

q1a_sample <- oh_panel[work_city %in% c(oh_adopters, oh_no_tax_cities) &
  total_jobs > 50][year <= max_year]
q1a_sample <- merge(q1a_sample, oh_first_adoption,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q1a_sample[, cohort := fifelse(is.na(first_adopt), 10000L, as.integer(first_adopt))]

q1a_model <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q1a_sample, cluster = ~work_city)

print_att(q1a_model, "Q1a: OH Adoption vs No-Tax")

p_q1a <- plot_sa(q1a_model,
  title = "Q1a: Does Adopting a Tax Cause Sorting?",
  subtitle = paste0("OH: ", length(oh_adopters), " adopters (0->tax) vs ",
    length(oh_no_tax_cities), " no-tax controls"),
  color = "steelblue")
print(p_q1a)
ggsave(file.path(fig_dir, "q1a_oh_adoption.png"), p_q1a,
  width = 7, height = 4.5, dpi = 300)

## Q1: PA Adoption (0 -> positive EIT) ----
q1_pa_sample <- pa_panel[work_city %in% c(pa_adopters, pa_never_taxed) &
  total_jobs > 50 & !is.na(resident_eit)][year <= max_year]
q1_pa_sample <- merge(q1_pa_sample, pa_first_adoption,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q1_pa_sample[, cohort := fifelse(is.na(first_adopt), 10000L, as.integer(first_adopt))]

if (uniqueN(q1_pa_sample[cohort < 10000, work_city]) >= 3) {
  q1_pa_model <- feols(
    share_live_in ~ sunab(cohort, year) | work_city + year,
    data = q1_pa_sample, cluster = ~work_city)
  print_att(q1_pa_model, "Q1: PA Adoption vs Never-Taxed")

  p_q1_pa <- plot_sa(q1_pa_model,
    title = "Q1: PA Adoption (0 -> EIT) vs Never-Taxed",
    subtitle = paste0(length(pa_adopters), " adopters vs ",
      length(pa_never_taxed), " never-taxed"),
    color = "firebrick")
  print(p_q1_pa)
  ggsave(file.path(fig_dir, "q1_pa_adoption.png"), p_q1_pa,
    width = 7, height = 4.5, dpi = 300)
} else {
  print("Q1 PA: Too few adopters for Sun-Abraham")
}

## Q1: Pooled OH + PA Adoption ----
# Stack OH and PA adopters + never-taxed controls with state x year FE
oh_q1_stack <- oh_panel[work_city %in% c(oh_adopters, oh_no_tax_cities) &
  total_jobs > 50][year <= max_year]
oh_q1_stack <- merge(oh_q1_stack, oh_first_adoption,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
oh_q1_stack[, `:=`(state = "OH", city_id = paste0("OH_", work_city))]

pa_q1_stack <- pa_panel[work_city %in% c(pa_adopters, pa_never_taxed) &
  total_jobs > 50 & !is.na(resident_eit)][year <= max_year]
pa_q1_stack <- merge(pa_q1_stack, pa_first_adoption,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
pa_q1_stack[, `:=`(state = "PA", city_id = paste0("PA_", work_city))]

pooled_q1 <- rbindlist(list(
  oh_q1_stack[, .(city_id, state, year, share_live_in, first_adopt, total_jobs)],
  pa_q1_stack[, .(city_id, state, year, share_live_in, first_adopt, total_jobs)]
))
pooled_q1[, cohort := fifelse(is.na(first_adopt), 10000L, as.integer(first_adopt))]
pooled_q1[, state_year := paste0(state, "_", year)]

q1_pooled_model <- feols(
  share_live_in ~ sunab(cohort, year) | city_id + state_year,
  data = pooled_q1, cluster = ~city_id)

n_treated_pooled <- uniqueN(pooled_q1[cohort < 10000, city_id])
n_control_pooled <- uniqueN(pooled_q1[cohort == 10000, city_id])
print_att(q1_pooled_model, "Q1: Pooled OH+PA Adoption vs Never-Taxed")

p_q1_pooled <- plot_sa(q1_pooled_model,
  title = "Q1: Pooled Adoption (0 -> tax) vs Never-Taxed",
  subtitle = paste0(n_treated_pooled, " adopters (OH+PA) vs ",
    n_control_pooled, " never-taxed controls; state x year FE"),
  color = "darkgreen")
print(p_q1_pooled)
ggsave(file.path(fig_dir, "q1_pooled_adoption.png"), p_q1_pooled,
  width = 7, height = 4.5, dpi = 300)

## Q1 Summary ----
print("Q1 Summary:")
print_att(q1a_model, "  OH only")
if (exists("q1_pa_model")) print_att(q1_pa_model, "  PA only")
print_att(q1_pooled_model, "  Pooled")


# ============================================================================
# Q1b: Among adopters, does the rate they set matter? ----
# Same sample as Q1a but interact with adoption rate
# Higher adoption rates should cause more sorting
# ============================================================================

q1b_sample <- merge(q1a_sample, oh_adopt_rate,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q1b_sample[is.na(adopt_rate), adopt_rate := 0]
q1b_sample[, post_adopt := as.integer(!is.na(first_adopt) & year >= first_adopt)]

# Binary adoption effect
q1b_binary <- feols(
  share_live_in ~ post_adopt | work_city + year,
  data = q1b_sample[work_city %in% c(oh_adopters, oh_no_tax_cities)],
  cluster = ~work_city)

# Adoption rate dose-response
q1b_dose <- feols(
  share_live_in ~ post_adopt + adopt_rate:post_adopt | work_city + year,
  data = q1b_sample[work_city %in% c(oh_adopters, oh_no_tax_cities)],
  cluster = ~work_city)

# Split by adoption rate: high (>= median) vs low
median_adopt_rate <- median(oh_adopt_rate$adopt_rate)
q1b_sample[, high_adopt := as.integer(adopt_rate >= median_adopt_rate)]

print(paste("Q1b: Median adoption rate:", round(median_adopt_rate, 3)))
print(paste("Q1b: N adopters =", length(oh_adopters),
  "- too few to split by rate. Reporting dose regression only."))

etable(q1b_binary, q1b_dose,
  headers = c("Binary Adoption", "Adoption + Rate Dose"))

# With only 5 adopters, Sun-Abraham split by rate is infeasible.
# The dose regression (post + adopt_rate:post) tests the rate gradient directly.


# ============================================================================
# Q2: Does the rate level matter for sorting? (Intensive margin) ----
# OH: Among already-taxed cities, rate increasers vs stable-rate taxed cities
# PA: Rate increasers vs never-changed
# ============================================================================

## Q2 OH: Rate changers vs stable-rate taxed ----
q2_oh_sample <- oh_panel[work_city %in% c(oh_rate_changers, oh_stable_taxed) &
  total_jobs > 50][year <= max_year]
q2_oh_sample <- merge(q2_oh_sample, oh_first_rate_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q2_oh_sample[, cohort := fifelse(is.na(first_rate_inc), 10000L, as.integer(first_rate_inc))]

q2_oh <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q2_oh_sample, cluster = ~work_city)

print_att(q2_oh, "Q2: OH Rate Increasers vs Stable-Taxed")

p_q2_oh <- plot_sa(q2_oh,
  title = "Q2: Does Raising the Rate Cause Additional Sorting?",
  subtitle = paste0("OH: ", length(oh_rate_changers),
    " rate increasers vs ", length(oh_stable_taxed), " stable-rate taxed"),
  color = "steelblue")
print(p_q2_oh)
ggsave(file.path(fig_dir, "q2_oh_intensive.png"), p_q2_oh,
  width = 7, height = 4.5, dpi = 300)

## Q2 OH: Also with no-tax controls (for comparison) ----
q2_oh_notax_sample <- oh_panel[work_city %in% c(oh_rate_changers, oh_no_tax_cities) &
  total_jobs > 50][year <= max_year]
q2_oh_notax_sample <- merge(q2_oh_notax_sample, oh_first_rate_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q2_oh_notax_sample[, cohort := fifelse(is.na(first_rate_inc), 10000L, as.integer(first_rate_inc))]

q2_oh_notax <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q2_oh_notax_sample, cluster = ~work_city)

print_att(q2_oh_notax, "Q2: OH Rate Increasers vs No-Tax")

## Q2 PA: Rate increasers vs never-changed ----
q2_pa_sample <- pa_panel[work_city %in% c(pa_inc_cities, pa_never_changed) &
  total_jobs > 50 & !is.na(resident_eit)]
q2_pa_sample <- merge(q2_pa_sample, pa_first_inc,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q2_pa_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

q2_pa <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q2_pa_sample, cluster = ~work_city)

print_att(q2_pa, "Q2: PA Rate Increasers vs Never-Changed")

p_q2_pa <- plot_sa(q2_pa,
  title = "Q2: Does Raising the EIT Rate Cause Sorting?",
  subtitle = paste0("PA: ", length(pa_inc_cities),
    " increasers vs ", length(pa_never_changed), " never-changed"),
  color = "firebrick")
print(p_q2_pa)
ggsave(file.path(fig_dir, "q2_pa_intensive.png"), p_q2_pa,
  width = 7, height = 4.5, dpi = 300)

## Q2 PA: Rate increasers vs no-tax ----
q2_pa_notax_sample <- pa_panel[work_city %in% c(pa_inc_cities, pa_never_taxed) &
  total_jobs > 50 & !is.na(resident_eit)]
q2_pa_notax_sample <- merge(q2_pa_notax_sample, pa_first_inc,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q2_pa_notax_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

q2_pa_notax <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q2_pa_notax_sample, cluster = ~work_city)

print_att(q2_pa_notax, "Q2: PA Rate Increasers vs No-Tax")

p_q2_pa_notax <- plot_sa(q2_pa_notax,
  title = "Q2: PA Rate Increases vs No-Tax Municipalities",
  subtitle = paste0(uniqueN(q2_pa_notax_sample[cohort < 10000, work_city]),
    " increasers vs ", uniqueN(q2_pa_notax_sample[cohort == 10000, work_city]),
    " no-tax controls"),
  color = "firebrick")
print(p_q2_pa_notax)
ggsave(file.path(fig_dir, "q2_pa_notax.png"), p_q2_pa_notax,
  width = 7, height = 4.5, dpi = 300)

## Q2 Summary table ----
etable(q2_oh_notax, q2_pa_notax, q2_oh, q2_pa,
  headers = c("OH Inc vs No-Tax", "PA Inc vs No-Tax",
    "OH Inc vs Stable", "PA Inc vs Stable"),
  drop.section = "coef")


# ============================================================================
# Q3: Is the sorting response symmetric? ----
# Do rate decreases reverse the sorting caused by increases?
# PA: Decreasers vs never-changed
# OH: Decreasers vs no-tax
# Joint model: both in one regression
# ============================================================================

## Q3 PA: Decreasers vs never-changed ----
q3_pa_sample <- pa_panel[work_city %in% c(pa_dec_cities, pa_never_changed) &
  total_jobs > 50 & !is.na(resident_eit)]
q3_pa_sample <- merge(q3_pa_sample, pa_first_dec,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q3_pa_sample[, cohort := fifelse(is.na(first_dec), 10000L, as.integer(first_dec))]

q3_pa_dec <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q3_pa_sample, cluster = ~work_city)

print_att(q3_pa_dec, "Q3: PA Decreasers vs NC")

p_q3_pa <- plot_sa(q3_pa_dec,
  title = "Q3: Do Rate Decreases Reverse Sorting?",
  subtitle = paste0("PA: ", length(pa_dec_cities),
    " decreasers vs ", length(pa_never_changed), " never-changed"),
  color = "steelblue")
print(p_q3_pa)
ggsave(file.path(fig_dir, "q3_pa_decrease.png"), p_q3_pa,
  width = 7, height = 4.5, dpi = 300)

## Q3 PA: Asymmetry model (both directions, one regression) ----
q3_pa_both_sample <- pa_panel[
  work_city %in% c(pa_inc_cities, pa_dec_cities, pa_never_changed) &
  total_jobs > 50 & !is.na(resident_eit)]
q3_pa_both_sample <- merge(q3_pa_both_sample, pa_first_inc,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q3_pa_both_sample <- merge(q3_pa_both_sample, pa_first_dec,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q3_pa_both_sample[, `:=`(
  has_inc = work_city %in% pa_inc_cities,
  has_dec = work_city %in% pa_dec_cities,
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

q3_pa_asym <- feols(
  share_live_in ~ has_inc:post_inc + has_dec:post_dec | work_city + year,
  data = q3_pa_both_sample, cluster = ~work_city)

## Q3 PA: Split dose (cumulative + / - on full sample) ----
q3_pa_dose_sample <- pa_panel[total_jobs > 50 & !is.na(resident_eit)][year <= max_year]
pa_base_rate <- pa_rate_panel[!is.na(cousub_fips),
  .(base_rate = first(na.omit(resident_eit))), by = cousub_fips]
q3_pa_dose_sample <- merge(q3_pa_dose_sample, pa_base_rate,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)
q3_pa_dose_sample[, cumul := resident_eit - fifelse(is.na(base_rate), 0, base_rate)]
q3_pa_dose_sample[, `:=`(
  pos_change = fifelse(cumul > 0, cumul, 0),
  neg_change = fifelse(cumul < 0, abs(cumul), 0)
)]

q3_pa_split <- feols(
  share_live_in ~ pos_change + neg_change | work_city + year,
  data = q3_pa_dose_sample, cluster = ~work_city)

## Q3 OH: Decreasers vs no-tax ----
oh_first_dec <- oh_rate_panel[event_type == "decrease",
  .(first_dec = min(year)), by = muni_fips]

q3_oh_sample <- oh_panel[work_city %in% c(oh_dec_cities, oh_no_tax_cities) &
  total_jobs > 50][year <= max_year]
q3_oh_sample <- merge(q3_oh_sample, oh_first_dec,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q3_oh_sample[, cohort := fifelse(is.na(first_dec), 10000L, as.integer(first_dec))]

q3_oh_dec <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q3_oh_sample, cluster = ~work_city)

print_att(q3_oh_dec, "Q3: OH Decreasers vs No-Tax")

## Q3 OH: Asymmetry model ----
q3_oh_both_sample <- oh_panel[
  work_city %in% c(oh_inc_cities, oh_dec_cities, oh_no_tax_cities) &
  total_jobs > 50][year <= max_year]
q3_oh_both_sample <- merge(q3_oh_both_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q3_oh_both_sample <- merge(q3_oh_both_sample, oh_first_dec,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q3_oh_both_sample[, `:=`(
  has_inc = work_city %in% oh_inc_cities,
  has_dec = work_city %in% oh_dec_cities,
  post_inc = as.integer(!is.na(first_inc) & year >= first_inc),
  post_dec = as.integer(!is.na(first_dec) & year >= first_dec)
)]

q3_oh_asym <- feols(
  share_live_in ~ has_inc:post_inc + has_dec:post_dec | work_city + year,
  data = q3_oh_both_sample, cluster = ~work_city)

## Q3 OH: Split dose (cumulative + / - on full sample) ----
q3_oh_dose_sample <- oh_panel[total_jobs > 50][year <= max_year]
oh_base_rate <- oh_rate_panel[, .(base_rate = first(rate)), by = muni_fips]
q3_oh_dose_sample <- merge(q3_oh_dose_sample, oh_base_rate,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q3_oh_dose_sample[is.na(base_rate), base_rate := 0]
q3_oh_dose_sample[, cumul := rate - base_rate]
q3_oh_dose_sample[, `:=`(
  pos_change = fifelse(cumul > 0, cumul, 0),
  neg_change = fifelse(cumul < 0, abs(cumul), 0)
)]

q3_oh_split <- feols(
  share_live_in ~ pos_change + neg_change | work_city + year,
  data = q3_oh_dose_sample, cluster = ~work_city)

## Q3 Summary ----
print("Q3 PA:")
etable(q3_pa_asym, q3_pa_split,
  headers = c("PA Asymmetry", "PA Split Dose"))

print("Q3 OH:")
etable(q3_oh_asym, q3_oh_split,
  headers = c("OH Asymmetry", "OH Split Dose"))

print_att(q3_pa_dec, "Q3: PA Dec Sun-Abraham ATT")
print_att(q3_oh_dec, "Q3: OH Dec Sun-Abraham ATT")


# ============================================================================
# Q4: Combined results — all increasers (adopters + rate changers) ----
# This is the "headline" model for each state
# OH: all increasers vs no-tax
# PA: all increasers vs never-changed
# ============================================================================

## Q4 OH: All increasers vs no-tax ----
q4_oh_sample <- oh_panel[work_city %in% c(oh_inc_cities, oh_no_tax_cities) &
  total_jobs > 50][year <= max_year]
q4_oh_sample <- merge(q4_oh_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q4_oh_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

q4_oh <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q4_oh_sample, cluster = ~work_city)

print_att(q4_oh, "Q4: OH All Increasers vs No-Tax")

p_q4_oh <- plot_sa(q4_oh,
  title = "OH: All Tax Increases vs No-Tax Controls",
  subtitle = paste0(length(oh_inc_cities), " treated vs ",
    length(oh_no_tax_cities), " no-tax controls"),
  color = "steelblue")
print(p_q4_oh)
ggsave(file.path(fig_dir, "q4_oh_all_inc.png"), p_q4_oh,
  width = 7, height = 4.5, dpi = 300)

## Q4 OH: All increasers vs never-changed (for comparison) ----
q4_oh_nc_sample <- oh_panel[work_city %in% c(oh_inc_cities, oh_never_changed) &
  total_jobs > 50][year <= max_year]
q4_oh_nc_sample <- merge(q4_oh_nc_sample, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)
q4_oh_nc_sample[, cohort := fifelse(is.na(first_inc), 10000L, as.integer(first_inc))]

q4_oh_nc <- feols(
  share_live_in ~ sunab(cohort, year) | work_city + year,
  data = q4_oh_nc_sample, cluster = ~work_city)

print_att(q4_oh_nc, "Q4: OH All Increasers vs Never-Changed")

## Q4 PA = same as Q2 PA (all PA variation is intensive) ----
q4_pa <- q2_pa

## Q4 Summary ----
etable(q4_oh, q4_oh_nc, q4_pa,
  headers = c("OH: Inc vs No-Tax", "OH: Inc vs NC", "PA: Inc vs NC"),
  drop.section = "coef")

p_q4_pa <- plot_sa(q4_pa,
  title = "PA: All Rate Increases vs Never-Changed Controls",
  subtitle = paste0(length(pa_inc_cities), " treated vs ",
    length(pa_never_changed), " never-changed"),
  color = "firebrick")
print(p_q4_pa)
ggsave(file.path(fig_dir, "q4_pa_all_inc.png"), p_q4_pa,
  width = 7, height = 4.5, dpi = 300)


# ============================================================================
# Master Summary Table ----
# All key models side by side
# ============================================================================

print("")
print("MASTER SUMMARY: All ATTs")
print_att(q1a_model, "Q1a: OH Adoption (0->tax) vs No-Tax")
print_att(q2_oh, "Q2:  OH Rate Inc vs Stable-Taxed")
print_att(q2_oh_notax, "Q2:  OH Rate Inc vs No-Tax")
print_att(q2_pa, "Q2:  PA Rate Inc vs Never-Changed")
print_att(q3_pa_dec, "Q3:  PA Decreasers vs NC")
print_att(q3_oh_dec, "Q3:  OH Decreasers vs No-Tax")
print_att(q4_oh, "Q4:  OH All Inc vs No-Tax")
print_att(q4_pa, "Q4:  PA All Inc vs NC")


beepr::beep()
