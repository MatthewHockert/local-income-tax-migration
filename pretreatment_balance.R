# Pre-Treatment Balance and Observables
# Reviewer concern: are treated cities deteriorating before the tax increase?
# We check pre-trends in fiscal observables using PA DCED AFR data.
# ============================================================================

library(data.table)
library(fixest)
library(ggplot2)

data_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration/data"

pa_rate <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))
pa_afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))

# Crosswalk and merge
pa_xwalk <- unique(pa_rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
pa_afr[, muni_id := fifelse(!is.na(municipality_id), municipality_id, municipal_code)]
pa_afr <- merge(pa_afr, pa_xwalk, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
pa_afr[, yr := fifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
pa_afr <- pa_afr[yr <= 2019]

# Build fiscal variables
pa_afr[, `:=`(
  eit_rev = as.numeric(fifelse(!is.na(earned_income_tax_revenues),
    earned_income_tax_revenues, earned_income_tax)),
  re_rev = as.numeric(fifelse(!is.na(real_estate_tax_revenues),
    real_estate_tax_revenues, real_estate_tax)),
  tot_rev = as.numeric(fifelse(!is.na(total_revenues), total_revenues, total_revenue)),
  tot_exp = as.numeric(total_expenditures),
  police_exp = as.numeric(fifelse(!is.na(police_expenditures), police_expenditures, police)),
  streets_exp = as.numeric(fifelse(!is.na(`public_works-highways_and_streets_expenditures`),
    `public_works-highways_and_streets_expenditures`, streets_and_roads)),
  pop = as.numeric(population),
  mkt_val = as.numeric(market_val_real_estate),
  debt = as.numeric(total_debt)
)]

pa_afr[, `:=`(
  log_eit = fifelse(eit_rev > 0, log(eit_rev), NA_real_),
  log_rev = fifelse(tot_rev > 0, log(tot_rev), NA_real_),
  log_police = fifelse(police_exp > 0, log(police_exp), NA_real_),
  log_streets = fifelse(streets_exp > 0, log(streets_exp), NA_real_),
  log_pop = fifelse(pop > 0, log(pop), NA_real_),
  log_mkt = fifelse(mkt_val > 0, log(mkt_val), NA_real_),
  log_debt = fifelse(debt > 0, log(debt), NA_real_),
  rev_per_cap = tot_rev / pop,
  exp_per_cap = tot_exp / pop
)]

# Merge rate info
pa_rate_m <- unique(pa_rate[!is.na(cousub_fips), .(cousub_fips, year, resident_eit)])
pa_afr <- merge(pa_afr, pa_rate_m,
  by.x = c("cousub_fips", "yr"), by.y = c("cousub_fips", "year"), all.x = TRUE)

# Cohort info
pa_first_inc <- pa_rate[event_type == "increase" & !is.na(cousub_fips),
  .(first_inc = min(year)), by = cousub_fips]
pa_afr <- merge(pa_afr, pa_first_inc, by = "cousub_fips", all.x = TRUE)
pa_afr[, `:=`(
  treated = !is.na(first_inc),
  cohort = fifelse(is.na(first_inc), 10000L, as.integer(first_inc))
)]

afr_samp <- pa_afr[!is.na(cousub_fips) & tot_rev > 0]


# 1. Pre-Treatment Balance Table ----
# Compare treated vs never-treated municipalities in the PRE period

pre_period <- afr_samp[yr < first_inc | !treated]
# For treated: only pre-treatment years; for never-treated: all years

treated_pre <- afr_samp[treated == TRUE & yr < first_inc]
control_all <- afr_samp[treated == FALSE]

balance_var <- function(var, label) {
  t_mean <- mean(treated_pre[[var]], na.rm = TRUE)
  t_sd <- sd(treated_pre[[var]], na.rm = TRUE)
  c_mean <- mean(control_all[[var]], na.rm = TRUE)
  c_sd <- sd(control_all[[var]], na.rm = TRUE)
  t_n <- sum(!is.na(treated_pre[[var]]))
  c_n <- sum(!is.na(control_all[[var]]))

  # Normalized difference
  norm_diff <- (t_mean - c_mean) / sqrt(t_sd^2 + c_sd^2)

  data.table(Variable = label, Treated_Mean = round(t_mean, 1),
    Treated_SD = round(t_sd, 1), Control_Mean = round(c_mean, 1),
    Control_SD = round(c_sd, 1), Norm_Diff = round(norm_diff, 3),
    T_N = t_n, C_N = c_n)
}

balance_table <- rbind(
  balance_var("pop", "Population"),
  balance_var("tot_rev", "Total Revenue ($)"),
  balance_var("eit_rev", "EIT Revenue ($)"),
  balance_var("re_rev", "RE Tax Revenue ($)"),
  balance_var("police_exp", "Police Expenditure ($)"),
  balance_var("streets_exp", "Streets Expenditure ($)"),
  balance_var("tot_exp", "Total Expenditure ($)"),
  balance_var("debt", "Total Debt ($)"),
  balance_var("rev_per_cap", "Revenue Per Capita ($)"),
  balance_var("exp_per_cap", "Expenditure Per Capita ($)"),
  balance_var("resident_eit", "EIT Rate (%)")
)

print("=== PRE-TREATMENT BALANCE TABLE ===")
print("Treated = municipalities that raised EIT (pre-treatment years)")
print("Control = municipalities that never raised EIT")
print(balance_table)


# 2. Sun-Abraham Pre-Trends in Observables ----
# For each fiscal variable, run Sun-Abraham and check pre-trend coefficients

run_sa_pretrend <- function(outcome_var, label) {
  samp <- afr_samp[!is.na(get(outcome_var))]
  m <- feols(as.formula(paste0(outcome_var, " ~ sunab(cohort, yr) | cousub_fips + yr")),
    data = samp, cluster = ~cousub_fips)

  ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
  setnames(ct, c("rn", "est", "se", "t", "p"))
  ct[, rel := as.integer(gsub("yr::", "", rn))]

  # Test: are pre-treatment coefficients jointly zero?
  pre_coefs <- ct[rel >= -5 & rel <= -1]
  joint_f <- if (nrow(pre_coefs) > 0) {
    mean_pre <- mean(abs(pre_coefs$est))
    max_pre_t <- max(abs(pre_coefs$t))
    any_sig <- any(abs(pre_coefs$t) > 1.96)
    data.table(outcome = label, mean_abs_pre = round(mean_pre, 5),
      max_t_pre = round(max_pre_t, 2), any_sig_5pct = any_sig)
  } else {
    data.table(outcome = label, mean_abs_pre = NA, max_t_pre = NA, any_sig_5pct = NA)
  }
  joint_f
}

pretrend_tests <- rbind(
  run_sa_pretrend("log_rev", "log(Total Revenue)"),
  run_sa_pretrend("log_eit", "log(EIT Revenue)"),
  run_sa_pretrend("log_police", "log(Police Exp)"),
  run_sa_pretrend("log_streets", "log(Streets Exp)"),
  run_sa_pretrend("log_pop", "log(Population)"),
  run_sa_pretrend("log_debt", "log(Total Debt)")
)

print("")
print("=== PRE-TREND TESTS: SUN-ABRAHAM ON FISCAL OBSERVABLES ===")
print("Are pre-treatment coefficients (t-5 to t-1) significant?")
print(pretrend_tests)


# 3. Detailed Sun-Abraham for Key Observables ----

sa_rev <- feols(log_rev ~ sunab(cohort, yr) | cousub_fips + yr,
  data = afr_samp[!is.na(log_rev)], cluster = ~cousub_fips)
sa_police <- feols(log_police ~ sunab(cohort, yr) | cousub_fips + yr,
  data = afr_samp[!is.na(log_police)], cluster = ~cousub_fips)
sa_pop <- feols(log_pop ~ sunab(cohort, yr) | cousub_fips + yr,
  data = afr_samp[!is.na(log_pop) & pop > 0], cluster = ~cousub_fips)

extract_sa <- function(m, label) {
  ct <- as.data.table(coeftable(m), keep.rownames = TRUE)
  setnames(ct, c("rn", "est", "se", "t", "p"))
  ct[, rel := as.integer(gsub("yr::", "", rn))]
  ct[rel >= -5 & rel <= 5, .(label, rel, est = round(est, 5),
    sig = fifelse(p < 0.01, "**", fifelse(p < 0.05, "*", fifelse(p < 0.1, ".", ""))))]
}

print("")
print("=== SUN-ABRAHAM: FISCAL OBSERVABLES (t-5 to t+5) ===")
combined <- rbind(
  extract_sa(sa_rev, "log(Revenue)"),
  extract_sa(sa_police, "log(Police)"),
  extract_sa(sa_pop, "log(Population)"))
print(dcast(combined, rel ~ label, value.var = "est"))


# 4. Pre-Treatment Trends Plot ----
# Visual: treated vs control mean of each observable over time

obs_trends <- afr_samp[, .(
  mean_rev = mean(tot_rev, na.rm = TRUE),
  mean_police = mean(police_exp, na.rm = TRUE),
  mean_pop = mean(pop, na.rm = TRUE),
  mean_eit_rev = mean(eit_rev, na.rm = TRUE),
  n = .N
), by = .(yr, treated)]

obs_long <- melt(obs_trends, id.vars = c("yr", "treated", "n"),
  variable.name = "outcome", value.name = "value")
obs_long[, treated_label := fifelse(treated, "Treated (raised EIT)", "Control (never raised)")]

p_trends <- ggplot(obs_long[outcome %in% c("mean_rev", "mean_police", "mean_pop")],
    aes(x = yr, y = value, color = treated_label)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1) +
  facet_wrap(~outcome, scales = "free_y",
    labeller = labeller(outcome = c(
      mean_rev = "Total Revenue",
      mean_police = "Police Expenditure",
      mean_pop = "Population"))) +
  labs(x = "Year", y = "Mean Value",
    title = "PA: Pre-Treatment Trends in Fiscal Observables",
    subtitle = "Treated = municipalities that raised EIT vs never-raised controls",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_trends)

# Normalized version: index to first year
obs_trends_norm <- afr_samp[yr >= 2002, .(
  mean_rev = mean(tot_rev, na.rm = TRUE),
  mean_police = mean(police_exp, na.rm = TRUE),
  mean_pop = mean(pop, na.rm = TRUE)
), by = .(yr, treated)]

base_vals <- obs_trends_norm[yr == 2002]
obs_trends_norm <- merge(obs_trends_norm, base_vals[, .(treated,
  base_rev = mean_rev, base_police = mean_police, base_pop = mean_pop)],
  by = "treated")
obs_trends_norm[, `:=`(
  idx_rev = mean_rev / base_rev,
  idx_police = mean_police / base_police,
  idx_pop = mean_pop / base_pop
)]

idx_long <- melt(obs_trends_norm[, .(yr, treated, idx_rev, idx_police, idx_pop)],
  id.vars = c("yr", "treated"), variable.name = "outcome", value.name = "index")
idx_long[, treated_label := fifelse(treated, "Treated", "Control")]

p_indexed <- ggplot(idx_long, aes(x = yr, y = index, color = treated_label)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~outcome, labeller = labeller(outcome = c(
    idx_rev = "Total Revenue", idx_police = "Police", idx_pop = "Population"))) +
  labs(x = "Year", y = "Index (2002 = 1)",
    title = "PA: Fiscal Observables Indexed to 2002",
    subtitle = "Parallel trends check: treated and control should track pre-treatment",
    color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_indexed)

beepr::beep()
