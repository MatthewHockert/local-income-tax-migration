# De Chaisemartin & D'Haultfoeuille (2024) Estimator
# Staggered DiD with continuous treatment (municipal income tax rates).
# Split by direction: increasers-only and decreasers-only vs never-changed.
# Includes bootstrap inference for continuous treatment models.
# ============================================================================

library(data.table)
library(dplyr)
library(DIDmultiplegtDYN)
library(fixest)
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))
library(ggplot2)

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data")
fig_dir <- file.path(project_dir, "figures")
max_year <- 2019


## Load data ----

pa_panel <- readRDS(file.path(data_dir, "pa_panel.rds"))[year <= max_year]
oh_panel <- readRDS(file.path(data_dir, "oh_panel.rds"))[year <= max_year]
pa_rate_panel <- readRDS(file.path(data_dir, "pa_rate_panel.rds"))
oh_rate_panel <- readRDS(file.path(data_dir, "oh_rate_panel.rds"))


## Merge fiscal controls into PA panel ----
# From DCED AFR: police, streets, debt, state aid expenditures

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

  print(paste("PA panel with fiscal controls:",
    sum(!is.na(pa_panel$log_police) & !is.na(pa_panel$log_streets) &
      !is.na(pa_panel$log_debt) & !is.na(pa_panel$log_state_aid)),
    "of", nrow(pa_panel)))
  rm(pa_afr_raw, fiscal_controls)
}


## Treatment groups ----

pa_inc_cities <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_dec_cities <- pa_rate_panel[event_type == "decrease" & !is.na(cousub_fips),
  unique(cousub_fips)]
pa_never_changed <- pa_rate_panel[!is.na(cousub_fips),
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = cousub_fips][any_change == FALSE, cousub_fips]

oh_inc_cities <- oh_rate_panel[event_type == "increase", unique(muni_fips)]
oh_dec_cities <- oh_rate_panel[event_type == "decrease", unique(muni_fips)]
oh_never_changed <- oh_rate_panel[,
  .(any_change = any(event_type %in% c("increase", "decrease"), na.rm = TRUE)),
  by = muni_fips][any_change == FALSE, muni_fips]

print(paste("PA:", length(pa_inc_cities), "inc,", length(pa_dec_cities),
  "dec,", length(pa_never_changed), "NC"))
print(paste("OH:", length(oh_inc_cities), "inc,", length(oh_dec_cities),
  "dec,", length(oh_never_changed), "NC"))


## Helper: extract DCDH event study coefficients ----

extract_dcdh <- function(model) {
  res <- model$results
  n_eff <- res$N_Effects
  n_plac <- res$N_Placebos

  eff_df <- as.data.table(res$Effects)
  eff <- data.table(
    rel = seq_len(n_eff),
    est = eff_df$Estimate,
    se = eff_df$SE,
    ci_lo = eff_df[["LB CI"]],
    ci_hi = eff_df[["UB CI"]])

  if (n_plac > 0 && !is.null(res$Placebos)) {
    plac_df <- as.data.table(res$Placebos)
    plac <- data.table(
      rel = -seq_len(n_plac),
      est = plac_df$Estimate,
      se = plac_df$SE,
      ci_lo = plac_df[["LB CI"]],
      ci_hi = plac_df[["UB CI"]])
    eff <- rbindlist(list(plac, eff))
  }

  ref <- data.table(rel = 0L, est = 0, se = 0, ci_lo = 0, ci_hi = 0)
  rbindlist(list(eff[rel < 0][order(rel)], ref, eff[rel > 0]))
}

## Helper: plot DCDH event study ----

plot_dcdh <- function(coefs, title, subtitle = NULL, color = "steelblue") {
  ggplot(coefs, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), color = color, size = 0.4) +
    geom_line(color = color, linewidth = 0.4) +
    labs(x = "Periods Since Treatment Change", y = "Estimated Effect",
      title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = seq(min(coefs$rel), max(coefs$rel), 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 11),
      plot.subtitle = element_text(size = 9, color = "grey40"))
}

## Helper: print DCDH summary ----

print_dcdh <- function(model, label) {
  res <- model$results
  ate <- as.data.table(res$ATE)
  print(paste0(label, ":"))
  print(paste("  ATE:", round(ate$Estimate, 5), "(SE", round(ate$SE, 5), ")"))
  print(paste("  Joint effects p:", round(res$p_jointeffects, 4)))
  print(paste("  Joint placebo p:", round(res$p_jointplacebo, 4)))
  print(paste("  Switchers:", as.data.table(res$Effects)$Switchers[1]))
}

## Helper: build comparison table ----

build_comparison <- function(model_list, labels) {
  cols <- list()
  for (i in seq_along(model_list)) {
    res <- model_list[[i]]$results
    eff <- as.data.table(res$Effects)
    plac <- if (!is.null(res$Placebos)) as.data.table(res$Placebos) else NULL
    ate <- as.data.table(res$ATE)

    est_vec <- c()
    se_vec <- c()
    row_names <- c()

    if (!is.null(plac)) {
      for (j in seq_len(nrow(plac))) {
        star <- ifelse(abs(plac$Estimate[j] / plac$SE[j]) > 2.576, "***",
          ifelse(abs(plac$Estimate[j] / plac$SE[j]) > 1.96, "**",
            ifelse(abs(plac$Estimate[j] / plac$SE[j]) > 1.645, "*", "")))
        est_vec <- c(est_vec, paste0(sprintf("%.5f", plac$Estimate[j]), star))
        se_vec <- c(se_vec, sprintf("(%.5f)", plac$SE[j]))
        row_names <- c(row_names, paste0("Placebo_", j))
      }
    }

    for (j in seq_len(nrow(eff))) {
      star <- ifelse(abs(eff$Estimate[j] / eff$SE[j]) > 2.576, "***",
        ifelse(abs(eff$Estimate[j] / eff$SE[j]) > 1.96, "**",
          ifelse(abs(eff$Estimate[j] / eff$SE[j]) > 1.645, "*", "")))
      est_vec <- c(est_vec, paste0(sprintf("%.5f", eff$Estimate[j]), star))
      se_vec <- c(se_vec, sprintf("(%.5f)", eff$SE[j]))
      row_names <- c(row_names, paste0("Effect_", j))
    }

    ate_star <- ifelse(abs(ate$Estimate / ate$SE) > 1.96, "**",
      ifelse(abs(ate$Estimate / ate$SE) > 1.645, "*", ""))
    est_vec <- c(est_vec, paste0(sprintf("%.5f", ate$Estimate), ate_star))
    se_vec <- c(se_vec, sprintf("(%.5f)", ate$SE))
    row_names <- c(row_names, "Avg Total Effect")

    est_vec <- c(est_vec,
      sprintf("%.4f", res$p_jointeffects),
      sprintf("%.4f", res$p_jointplacebo),
      as.character(eff$Switchers[1]),
      as.character(eff$N[1]))
    se_vec <- c(se_vec, "", "", "", "")
    row_names <- c(row_names, "Joint effects p", "Joint placebo p",
      "Switchers", "N")

    cols[[i]] <- data.table(row = row_names, est = est_vec, se = se_vec)
    setnames(cols[[i]], c("row", paste0("est_", i), paste0("se_", i)))
  }

  out <- cols[[1]]
  for (i in 2:length(cols)) {
    out <- merge(out, cols[[i]], by = "row", all = TRUE, sort = FALSE)
  }
  out <- out[match(cols[[1]]$row, row)]

  header <- paste0(sprintf("%-20s", ""),
    paste0(sprintf("%20s", labels), collapse = ""))
  print(header)
  print(paste0(rep("-", nchar(header)), collapse = ""))

  for (r in seq_len(nrow(out))) {
    est_line <- sprintf("%-20s", out$row[r])
    se_line <- sprintf("%-20s", "")
    for (i in seq_along(labels)) {
      est_line <- paste0(est_line, sprintf("%20s", out[[paste0("est_", i)]][r]))
      se_line <- paste0(se_line, sprintf("%20s", out[[paste0("se_", i)]][r]))
    }
    print(est_line)
    if (out[[paste0("se_", 1)]][r] != "") print(se_line)
  }
  invisible(out)
}


# Build samples ----

# PA samples: filter by direction, keep never-changed as control
pa_base <- pa_panel[total_jobs > 50 & !is.na(resident_eit)]

pa_inc_sample <- pa_base[work_city %in% c(pa_inc_cities, pa_never_changed)]
pa_inc_sample[, work_city_int := as.integer(as.factor(work_city))]

pa_dec_sample <- pa_base[work_city %in% c(pa_dec_cities, pa_never_changed)]
pa_dec_sample[, work_city_int := as.integer(as.factor(work_city))]

pa_all_sample <- copy(pa_base)
pa_all_sample[, work_city_int := as.integer(as.factor(work_city))]

# OH samples
oh_base <- oh_panel[total_jobs > 50]
oh_base[is.na(rate), rate := 0]

oh_inc_sample <- oh_base[work_city %in% c(oh_inc_cities, oh_never_changed)]
oh_inc_sample[, work_city_int := as.integer(as.factor(work_city))]

oh_dec_sample <- oh_base[work_city %in% c(oh_dec_cities, oh_never_changed)]
oh_dec_sample[, work_city_int := as.integer(as.factor(work_city))]

oh_all_sample <- copy(oh_base)
oh_all_sample[, work_city_int := as.integer(as.factor(work_city))]

print(paste("PA inc sample:", nrow(pa_inc_sample), "obs,",
  uniqueN(pa_inc_sample$work_city), "munis"))
print(paste("PA dec sample:", nrow(pa_dec_sample), "obs,",
  uniqueN(pa_dec_sample$work_city), "munis"))
print(paste("OH inc sample:", nrow(oh_inc_sample), "obs,",
  uniqueN(oh_inc_sample$work_city), "munis"))
print(paste("OH dec sample:", nrow(oh_dec_sample), "obs,",
  uniqueN(oh_dec_sample$work_city), "munis"))

# OH samples split by control group (for Q2 table)
oh_no_tax_cities <- setdiff(
  oh_base[, unique(work_city)],
  oh_rate_panel[rate > 0, unique(muni_fips)])
oh_stable_taxed <- setdiff(oh_never_changed, oh_no_tax_cities)

oh_inc_notax_sample <- oh_base[work_city %in% c(oh_inc_cities, oh_no_tax_cities)]
oh_inc_notax_sample[, work_city_int := as.integer(as.factor(work_city))]

oh_inc_stable_sample <- oh_base[work_city %in% c(oh_inc_cities, oh_stable_taxed)]
oh_inc_stable_sample[, work_city_int := as.integer(as.factor(work_city))]

# PA no-tax sample (for Q2 col 2)
pa_rate_by_muni <- pa_rate_panel[!is.na(cousub_fips) & !is.na(resident_eit),
  .(ever_zero = any(resident_eit == 0), ever_positive = any(resident_eit > 0)), by = cousub_fips]
pa_never_taxed <- pa_rate_by_muni[ever_zero == TRUE & ever_positive == FALSE, cousub_fips]

pa_inc_notax_sample <- pa_base[work_city %in% c(pa_inc_cities, pa_never_taxed)]
pa_inc_notax_sample[, work_city_int := as.integer(as.factor(work_city))]

print(paste("OH inc vs no-tax:", nrow(oh_inc_notax_sample), "obs,",
  uniqueN(oh_inc_notax_sample$work_city), "munis"))
print(paste("OH inc vs stable:", nrow(oh_inc_stable_sample), "obs,",
  uniqueN(oh_inc_stable_sample$work_city), "munis"))
print(paste("PA inc vs no-tax:", nrow(pa_inc_notax_sample), "obs,",
  uniqueN(pa_inc_notax_sample$work_city), "munis"))

# PA samples with fiscal controls (subset with complete controls)
pa_ctrl_vars <- c("log_police", "log_streets", "log_debt", "log_state_aid")
pa_has_controls <- complete.cases(pa_base[, ..pa_ctrl_vars])

pa_inc_ctrl_sample <- pa_base[pa_has_controls & work_city %in% c(pa_inc_cities, pa_never_changed)]
pa_inc_ctrl_sample[, work_city_int := as.integer(as.factor(work_city))]

pa_dec_ctrl_sample <- pa_base[pa_has_controls & work_city %in% c(pa_dec_cities, pa_never_changed)]
pa_dec_ctrl_sample[, work_city_int := as.integer(as.factor(work_city))]

print(paste("PA inc w/ controls:", nrow(pa_inc_ctrl_sample), "obs,",
  uniqueN(pa_inc_ctrl_sample$work_city), "munis"))
print(paste("PA dec w/ controls:", nrow(pa_dec_ctrl_sample), "obs,",
  uniqueN(pa_dec_ctrl_sample$work_city), "munis"))


# ============================================================================
# Analytical SE Models ----
# ============================================================================

# PA: Increasers vs never-changed (continuous) ----
pa_dcdh_inc <- did_multiplegt_dyn(
  df = as.data.frame(pa_inc_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(pa_dcdh_inc, "PA Increasers (continuous)")

# PA: Decreasers vs never-changed (continuous) ----
pa_dcdh_dec <- did_multiplegt_dyn(
  df = as.data.frame(pa_dec_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(pa_dcdh_dec, "PA Decreasers (continuous)")

# PA: All pooled (continuous, for reference) ----
pa_dcdh_all <- did_multiplegt_dyn(
  df = as.data.frame(pa_all_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(pa_dcdh_all, "PA All (continuous)")

# OH: Increasers vs never-changed (continuous) ----
oh_dcdh_inc <- did_multiplegt_dyn(
  df = as.data.frame(oh_inc_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(oh_dcdh_inc, "OH Increasers (continuous)")

# OH: Decreasers vs never-changed (continuous) ----
oh_dcdh_dec <- did_multiplegt_dyn(
  df = as.data.frame(oh_dec_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(oh_dcdh_dec, "OH Decreasers (continuous)")

# OH: All pooled (continuous) ----
oh_dcdh_all <- did_multiplegt_dyn(
  df = as.data.frame(oh_all_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(oh_dcdh_all, "OH All (continuous)")


# Q2 Table Models: DCDH by control group ----
# These match the Q2 table columns: inc vs no-tax, inc vs stable-rate

# OH Inc vs No-Tax (continuous)
oh_dcdh_inc_notax <- did_multiplegt_dyn(
  df = as.data.frame(oh_inc_notax_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(oh_dcdh_inc_notax, "OH Inc vs No-Tax (DCDH)")

# OH Inc vs Stable-Taxed (continuous) — same as oh_dcdh_inc above
# (oh_inc_sample uses oh_never_changed which is stable+notax mixed;
#  oh_inc_stable_sample uses only stable-taxed)
oh_dcdh_inc_stable <- did_multiplegt_dyn(
  df = as.data.frame(oh_inc_stable_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(oh_dcdh_inc_stable, "OH Inc vs Stable-Taxed (DCDH)")

# PA Inc vs No-Tax (continuous) — may fail with only 5 controls
tryCatch({
  pa_dcdh_inc_notax <- did_multiplegt_dyn(
    df = as.data.frame(pa_inc_notax_sample),
    outcome = "share_live_in",
    group = "work_city_int",
    time = "year",
    treatment = "resident_eit",
    effects = 8,
    placebo = 5,
    continuous = 1,
    cluster = "work_city_int",
    graph_off = TRUE)
  print_dcdh(pa_dcdh_inc_notax, "PA Inc vs No-Tax (DCDH)")
}, error = function(e) {
  print(paste("PA Inc vs No-Tax DCDH failed:", e$message))
})

# PA Inc vs Stable-Rate (continuous) — same as pa_dcdh_inc above
# (pa_inc_sample already uses pa_never_changed = stable-rate)
print_dcdh(pa_dcdh_inc, "PA Inc vs Stable-Rate (DCDH) [= pa_dcdh_inc]")

# Q2 Summary
print("Q2 DCDH Summary:")
print_dcdh(oh_dcdh_inc_notax, "  OH Inc vs No-Tax")
print_dcdh(oh_dcdh_inc_stable, "  OH Inc vs Stable")
print_dcdh(pa_dcdh_inc, "  PA Inc vs Stable")


# PA Models with Fiscal Controls ----
# Same specs but adding log_police, log_streets, log_debt, log_state_aid

# PA: Increasers vs NC (continuous, with controls) ----
pa_dcdh_inc_ctrl <- did_multiplegt_dyn(
  df = as.data.frame(pa_inc_ctrl_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  controls = pa_ctrl_vars,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(pa_dcdh_inc_ctrl, "PA Increasers + Controls (continuous)")

# PA: Decreasers vs NC (continuous, with controls) ----
pa_dcdh_dec_ctrl <- did_multiplegt_dyn(
  df = as.data.frame(pa_dec_ctrl_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  controls = pa_ctrl_vars,
  cluster = "work_city_int",
  graph_off = TRUE)
print_dcdh(pa_dcdh_dec_ctrl, "PA Decreasers + Controls (continuous)")


# Analytical SE Event Study Figures ----

p_pa_inc <- plot_dcdh(extract_dcdh(pa_dcdh_inc),
  "PA: EIT Increases vs Never-Changed (DCDH Continuous)",
  paste0("Switchers = ", as.data.table(pa_dcdh_inc$results$Effects)$Switchers[1],
    "; joint placebo p = ", round(pa_dcdh_inc$results$p_jointplacebo, 3)),
  "firebrick")
print(p_pa_inc)
ggsave(file.path(fig_dir, "pa_dcdh_inc.png"), p_pa_inc,
  width = 7, height = 4.5, dpi = 300)

p_pa_dec <- plot_dcdh(extract_dcdh(pa_dcdh_dec),
  "PA: EIT Decreases vs Never-Changed (DCDH Continuous)",
  paste0("Switchers = ", as.data.table(pa_dcdh_dec$results$Effects)$Switchers[1],
    "; joint placebo p = ", round(pa_dcdh_dec$results$p_jointplacebo, 3)),
  "steelblue")
print(p_pa_dec)
ggsave(file.path(fig_dir, "pa_dcdh_dec.png"), p_pa_dec,
  width = 7, height = 4.5, dpi = 300)

p_pa_inc_ctrl <- plot_dcdh(extract_dcdh(pa_dcdh_inc_ctrl),
  "PA: EIT Increases vs NC + Fiscal Controls (DCDH)",
  paste0("Switchers = ", as.data.table(pa_dcdh_inc_ctrl$results$Effects)$Switchers[1],
    "; joint placebo p = ", round(pa_dcdh_inc_ctrl$results$p_jointplacebo, 3)),
  "firebrick")
print(p_pa_inc_ctrl)
ggsave(file.path(fig_dir, "pa_dcdh_inc_ctrl.png"), p_pa_inc_ctrl,
  width = 7, height = 4.5, dpi = 300)

p_pa_dec_ctrl <- plot_dcdh(extract_dcdh(pa_dcdh_dec_ctrl),
  "PA: EIT Decreases vs NC + Fiscal Controls (DCDH)",
  paste0("Switchers = ", as.data.table(pa_dcdh_dec_ctrl$results$Effects)$Switchers[1],
    "; joint placebo p = ", round(pa_dcdh_dec_ctrl$results$p_jointplacebo, 3)),
  "steelblue")
print(p_pa_dec_ctrl)
ggsave(file.path(fig_dir, "pa_dcdh_dec_ctrl.png"), p_pa_dec_ctrl,
  width = 7, height = 4.5, dpi = 300)

p_oh_inc <- plot_dcdh(extract_dcdh(oh_dcdh_inc),
  "OH: Rate Increases vs Never-Changed (DCDH Continuous)",
  paste0("Switchers = ", as.data.table(oh_dcdh_inc$results$Effects)$Switchers[1],
    "; joint placebo p = ", round(oh_dcdh_inc$results$p_jointplacebo, 3)),
  "steelblue")
print(p_oh_inc)
ggsave(file.path(fig_dir, "oh_dcdh_inc.png"), p_oh_inc,
  width = 7, height = 4.5, dpi = 300)

p_oh_dec <- plot_dcdh(extract_dcdh(oh_dcdh_dec),
  "OH: Rate Decreases vs Never-Changed (DCDH Continuous)",
  paste0("Switchers = ", as.data.table(oh_dcdh_dec$results$Effects)$Switchers[1],
    "; joint placebo p = ", round(oh_dcdh_dec$results$p_jointplacebo, 3)),
  "steelblue")
print(p_oh_dec)
ggsave(file.path(fig_dir, "oh_dcdh_dec.png"), p_oh_dec,
  width = 7, height = 4.5, dpi = 300)


# Analytical SE Comparison Tables ----

print("")
print("TABLE: PA DCDH — Increases vs Decreases vs All (Analytical SE)")
build_comparison(
  list(pa_dcdh_inc, pa_dcdh_dec, pa_dcdh_all),
  c("PA Inc", "PA Dec", "PA All"))

print("")
print("TABLE: PA DCDH — Without vs With Fiscal Controls")
build_comparison(
  list(pa_dcdh_inc, pa_dcdh_inc_ctrl, pa_dcdh_dec, pa_dcdh_dec_ctrl),
  c("PA Inc", "PA Inc+Ctrl", "PA Dec", "PA Dec+Ctrl"))

print("")
print("TABLE: OH DCDH — Increases vs Decreases vs All (Analytical SE)")
build_comparison(
  list(oh_dcdh_inc, oh_dcdh_dec, oh_dcdh_all),
  c("OH Inc", "OH Dec", "OH All"))


# ACRT Dose-Response Curve ----

pa_first_inc <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(first_inc = min(year)), by = cousub_fips]
oh_first_inc <- oh_rate_panel[event_type == "increase",
  .(first_inc = min(year)), by = muni_fips]

## PA ACRT ----
pa_acrt_data <- pa_panel[total_jobs > 50 & !is.na(resident_eit)][year <= max_year]
pa_acrt_data <- merge(pa_acrt_data, pa_first_inc,
  by.x = "work_city", by.y = "cousub_fips", all.x = TRUE)

pa_year_means <- pa_acrt_data[, .(year_mean = mean(share_live_in, na.rm = TRUE)), by = year]
pa_acrt_data <- merge(pa_acrt_data, pa_year_means, by = "year")
pa_acrt_data[, share_demean := share_live_in - year_mean]

pa_treated_acrt <- pa_acrt_data[!is.na(first_inc)]
pa_pre <- pa_treated_acrt[year < first_inc,
  .(pre_mean = mean(share_demean, na.rm = TRUE)), by = work_city]
pa_post <- pa_treated_acrt[year >= first_inc,
  .(post_mean = mean(share_demean, na.rm = TRUE)), by = work_city]
pa_city_effects <- merge(pa_pre, pa_post, by = "work_city")
pa_city_effects[, effect := post_mean - pre_mean]

pa_rate_changes <- pa_rate_panel[event_type == "increase" & !is.na(cousub_fips),
  .(total_increase = sum(rate_change, na.rm = TRUE)), by = cousub_fips]
pa_city_effects <- merge(pa_city_effects, pa_rate_changes,
  by.x = "work_city", by.y = "cousub_fips")

pa_city_effects[, dose_bin := cut(total_increase,
  breaks = c(0, 0.05, 0.1, 0.15, 0.25, 0.5, 1, Inf),
  labels = c("0-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.25",
    "0.25-0.50", "0.50-1.00", ">1.00"),
  include.lowest = TRUE)]

pa_binned <- pa_city_effects[!is.na(dose_bin), .(
  mean_effect = mean(effect, na.rm = TRUE),
  se = sd(effect, na.rm = TRUE) / sqrt(.N),
  mean_dose = mean(total_increase, na.rm = TRUE),
  n = .N
), by = dose_bin]

print("PA ACRT binned scatter:")
print(pa_binned)

p_pa_acrt <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(data = pa_binned %>% filter(n > 5),
    aes(x = mean_dose, y = mean_effect,
      ymin = mean_effect - 1.96 * se, ymax = mean_effect + 1.96 * se),
    color = "firebrick", size = 0.5) +
  geom_smooth(data = pa_city_effects,
    aes(x = total_increase, y = effect),
    method = "lm", se = TRUE, color = "firebrick", fill = "firebrick",
    alpha = 0.15, linewidth = 0.6) +
  geom_text(data = pa_binned %>% filter(n > 5),
    aes(x = mean_dose, y = mean_effect,
      label = paste0("n=", n)), vjust = -1.5, size = 2.5, color = "grey40") +
  labs(x = "Total EIT Rate Increase (pp)",
    y = "Change in share_live_in (year-demeaned, post - pre)",
    title = "PA: Average Causal Response by Treatment Dose",
    subtitle = "Binned means with 95% CI; line = OLS through city-level effects") +
  theme_minimal()
print(p_pa_acrt)
ggsave(file.path(fig_dir, "pa_acrt_dose_response.png"), p_pa_acrt,
  width = 7, height = 5, dpi = 300)

pa_acrt_reg <- feols(effect ~ total_increase, data = pa_city_effects)
etable(pa_acrt_reg, headers = "PA: ACRT")

## OH ACRT ----
oh_acrt_data <- oh_panel[total_jobs > 50][year <= max_year]
oh_acrt_data <- merge(oh_acrt_data, oh_first_inc,
  by.x = "work_city", by.y = "muni_fips", all.x = TRUE)

oh_year_means <- oh_acrt_data[, .(year_mean = mean(share_live_in, na.rm = TRUE)), by = year]
oh_acrt_data <- merge(oh_acrt_data, oh_year_means, by = "year")
oh_acrt_data[, share_demean := share_live_in - year_mean]

oh_treated_acrt <- oh_acrt_data[!is.na(first_inc)]
oh_pre <- oh_treated_acrt[year < first_inc,
  .(pre_mean = mean(share_demean, na.rm = TRUE)), by = work_city]
oh_post <- oh_treated_acrt[year >= first_inc,
  .(post_mean = mean(share_demean, na.rm = TRUE)), by = work_city]
oh_city_effects <- merge(oh_pre, oh_post, by = "work_city")
oh_city_effects[, effect := post_mean - pre_mean]

oh_rate_changes <- oh_rate_panel[event_type == "increase",
  .(total_increase = sum(rate_change, na.rm = TRUE) * 100), by = muni_fips]
oh_city_effects <- merge(oh_city_effects, oh_rate_changes,
  by.x = "work_city", by.y = "muni_fips")

oh_city_effects[, dose_bin := cut(total_increase,
  breaks = c(0, 0.1, 0.25, 0.5, 0.75, 1.0, Inf),
  labels = c("0-0.10", "0.10-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", ">1.00"),
  include.lowest = TRUE)]

oh_binned <- oh_city_effects[!is.na(dose_bin), .(
  mean_effect = mean(effect, na.rm = TRUE),
  se = sd(effect, na.rm = TRUE) / sqrt(.N),
  mean_dose = mean(total_increase, na.rm = TRUE),
  n = .N
), by = dose_bin]

print("OH ACRT binned scatter:")
print(oh_binned)

p_oh_acrt <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(data = oh_binned,
    aes(x = mean_dose, y = mean_effect,
      ymin = mean_effect - 1.96 * se, ymax = mean_effect + 1.96 * se),
    color = "steelblue", size = 0.5) +
  geom_smooth(data = oh_city_effects,
    aes(x = total_increase, y = effect),
    method = "lm", se = TRUE, color = "steelblue", fill = "steelblue",
    alpha = 0.15, linewidth = 0.6) +
  geom_text(data = oh_binned, aes(x = mean_dose, y = mean_effect,
    label = paste0("n=", n)), vjust = -1.5, size = 2.5, color = "grey40") +
  labs(x = "Total Municipal Rate Increase (pp)",
    y = "Change in share_live_in (year-demeaned, post - pre)",
    title = "OH: Average Causal Response by Treatment Dose",
    subtitle = "Binned means with 95% CI; line = OLS through city-level effects") +
  theme_minimal()
print(p_oh_acrt)
ggsave(file.path(fig_dir, "oh_acrt_dose_response.png"), p_oh_acrt,
  width = 7, height = 5, dpi = 300)

oh_acrt_reg <- feols(effect ~ total_increase, data = oh_city_effects)

etable(pa_acrt_reg, oh_acrt_reg,
  headers = c("PA: ACRT", "OH: ACRT"))


# ============================================================================
# Bootstrap SE Models ----
# Authors recommend bootstrap for continuous treatment.
# Rerun directional models with bootstrap = 100.
# ============================================================================

print("Starting bootstrap models...")

## PA Increasers (bootstrap) ----
pa_dcdh_inc_boot <- did_multiplegt_dyn(
  df = as.data.frame(pa_inc_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  bootstrap = 100,
  graph_off = TRUE)
print_dcdh(pa_dcdh_inc_boot, "PA Increasers (bootstrap)")

## PA Decreasers (bootstrap) ----
pa_dcdh_dec_boot <- did_multiplegt_dyn(
  df = as.data.frame(pa_dec_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  bootstrap = 100,
  graph_off = TRUE)
print_dcdh(pa_dcdh_dec_boot, "PA Decreasers (bootstrap)")

## PA Increasers + Controls (bootstrap) ----
pa_dcdh_inc_ctrl_boot <- did_multiplegt_dyn(
  df = as.data.frame(pa_inc_ctrl_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  controls = pa_ctrl_vars,
  cluster = "work_city_int",
  bootstrap = 100,
  graph_off = TRUE)
print_dcdh(pa_dcdh_inc_ctrl_boot, "PA Increasers + Controls (bootstrap)")

## PA Decreasers + Controls (bootstrap) ----
pa_dcdh_dec_ctrl_boot <- did_multiplegt_dyn(
  df = as.data.frame(pa_dec_ctrl_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "resident_eit",
  effects = 8,
  placebo = 5,
  continuous = 1,
  controls = pa_ctrl_vars,
  cluster = "work_city_int",
  bootstrap = 100,
  graph_off = TRUE)
print_dcdh(pa_dcdh_dec_ctrl_boot, "PA Decreasers + Controls (bootstrap)")

## OH Increasers (bootstrap) ----
oh_dcdh_inc_boot <- did_multiplegt_dyn(
  df = as.data.frame(oh_inc_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  bootstrap = 100,
  graph_off = TRUE)
print_dcdh(oh_dcdh_inc_boot, "OH Increasers (bootstrap)")

## OH Decreasers (bootstrap) ----
oh_dcdh_dec_boot <- did_multiplegt_dyn(
  df = as.data.frame(oh_dec_sample),
  outcome = "share_live_in",
  group = "work_city_int",
  time = "year",
  treatment = "rate",
  effects = 8,
  placebo = 5,
  continuous = 1,
  cluster = "work_city_int",
  bootstrap = 100,
  graph_off = TRUE)
print_dcdh(oh_dcdh_dec_boot, "OH Decreasers (bootstrap)")


# Bootstrap Event Study Figures ----

p_pa_inc_boot <- plot_dcdh(extract_dcdh(pa_dcdh_inc_boot),
  "PA: EIT Increases vs NC (DCDH Bootstrap)",
  paste0("bootstrap = 100; joint placebo p = ",
    round(pa_dcdh_inc_boot$results$p_jointplacebo, 3)),
  "firebrick")
print(p_pa_inc_boot)
ggsave(file.path(fig_dir, "pa_dcdh_inc_boot.png"), p_pa_inc_boot,
  width = 7, height = 4.5, dpi = 300)

p_pa_dec_boot <- plot_dcdh(extract_dcdh(pa_dcdh_dec_boot),
  "PA: EIT Decreases vs NC (DCDH Bootstrap)",
  paste0("bootstrap = 100; joint placebo p = ",
    round(pa_dcdh_dec_boot$results$p_jointplacebo, 3)),
  "steelblue")
print(p_pa_dec_boot)
ggsave(file.path(fig_dir, "pa_dcdh_dec_boot.png"), p_pa_dec_boot,
  width = 7, height = 4.5, dpi = 300)

p_oh_inc_boot <- plot_dcdh(extract_dcdh(oh_dcdh_inc_boot),
  "OH: Rate Increases vs NC (DCDH Bootstrap)",
  paste0("bootstrap = 100; joint placebo p = ",
    round(oh_dcdh_inc_boot$results$p_jointplacebo, 3)),
  "steelblue")
print(p_oh_inc_boot)
ggsave(file.path(fig_dir, "oh_dcdh_inc_boot.png"), p_oh_inc_boot,
  width = 7, height = 4.5, dpi = 300)

p_oh_dec_boot <- plot_dcdh(extract_dcdh(oh_dcdh_dec_boot),
  "OH: Rate Decreases vs NC (DCDH Bootstrap)",
  paste0("bootstrap = 100; joint placebo p = ",
    round(oh_dcdh_dec_boot$results$p_jointplacebo, 3)),
  "steelblue")
print(p_oh_dec_boot)
ggsave(file.path(fig_dir, "oh_dcdh_dec_boot.png"), p_oh_dec_boot,
  width = 7, height = 4.5, dpi = 300)

p_pa_inc_ctrl_boot <- plot_dcdh(extract_dcdh(pa_dcdh_inc_ctrl_boot),
  "PA: EIT Increases + Controls (DCDH Bootstrap)",
  paste0("bootstrap = 100; joint placebo p = ",
    round(pa_dcdh_inc_ctrl_boot$results$p_jointplacebo, 3)),
  "firebrick")
print(p_pa_inc_ctrl_boot)
ggsave(file.path(fig_dir, "pa_dcdh_inc_ctrl_boot.png"), p_pa_inc_ctrl_boot,
  width = 7, height = 4.5, dpi = 300)

p_pa_dec_ctrl_boot <- plot_dcdh(extract_dcdh(pa_dcdh_dec_ctrl_boot),
  "PA: EIT Decreases + Controls (DCDH Bootstrap)",
  paste0("bootstrap = 100; joint placebo p = ",
    round(pa_dcdh_dec_ctrl_boot$results$p_jointplacebo, 3)),
  "steelblue")
print(p_pa_dec_ctrl_boot)
ggsave(file.path(fig_dir, "pa_dcdh_dec_ctrl_boot.png"), p_pa_dec_ctrl_boot,
  width = 7, height = 4.5, dpi = 300)


# Bootstrap Comparison Tables ----

print("")
print("TABLE: PA DCDH — Analytical vs Bootstrap SE (No Controls)")
build_comparison(
  list(pa_dcdh_inc, pa_dcdh_inc_boot, pa_dcdh_dec, pa_dcdh_dec_boot),
  c("PA Inc", "PA Inc Boot", "PA Dec", "PA Dec Boot"))

print("")
print("TABLE: PA DCDH — Analytical vs Bootstrap SE (With Controls)")
build_comparison(
  list(pa_dcdh_inc_ctrl, pa_dcdh_inc_ctrl_boot, pa_dcdh_dec_ctrl, pa_dcdh_dec_ctrl_boot),
  c("Inc+Ctrl", "Inc+Ctrl Boot", "Dec+Ctrl", "Dec+Ctrl Boot"))

print("")
print("TABLE: OH DCDH — Analytical vs Bootstrap SE")
build_comparison(
  list(oh_dcdh_inc, oh_dcdh_inc_boot, oh_dcdh_dec, oh_dcdh_dec_boot),
  c("OH Inc", "OH Inc Boot", "OH Dec", "OH Dec Boot"))

print("")
print("TABLE: Full Comparison — PA Increasers (all variants)")
build_comparison(
  list(pa_dcdh_inc, pa_dcdh_inc_boot, pa_dcdh_inc_ctrl, pa_dcdh_inc_ctrl_boot),
  c("No Ctrl", "No Ctrl Boot", "+Ctrl", "+Ctrl Boot"))

## SE ratio comparison ----
compare_se <- function(ana, boot, label) {
  a <- extract_dcdh(ana)
  b <- extract_dcdh(boot)
  comp <- merge(
    a[rel > 0, .(rel, se_ana = se)],
    b[rel > 0, .(rel, se_boot = se)],
    by = "rel")
  comp[, ratio := round(se_boot / se_ana, 2)]
  print(paste(label, "- SE ratio (boot/analytical):"))
  print(comp[, .(rel, se_ana = round(se_ana, 5), se_boot = round(se_boot, 5), ratio)])
}

compare_se(pa_dcdh_inc, pa_dcdh_inc_boot, "PA Increasers")
compare_se(pa_dcdh_inc_ctrl, pa_dcdh_inc_ctrl_boot, "PA Increasers + Controls")
compare_se(pa_dcdh_dec, pa_dcdh_dec_boot, "PA Decreasers")
compare_se(pa_dcdh_dec_ctrl, pa_dcdh_dec_ctrl_boot, "PA Decreasers + Controls")
compare_se(oh_dcdh_inc, oh_dcdh_inc_boot, "OH Increasers")
compare_se(oh_dcdh_dec, oh_dcdh_dec_boot, "OH Decreasers")

beepr::beep()
