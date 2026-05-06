# run_slide_models.R
# Re-estimates all slide-relevant models in one place after the block-level
# LODES rebuild. Skipped: DCDH (slow, run separately when needed).
#
# Outputs:
#   - figures/q2_event_study_decomp.png   (Q2 ES, muni vs SD cohort decomposed)
#   - figures/q2_clean_groups_es.png      (Q3 muni cohort: Inc-only/Dec-only vs NC)
#   - figures/q3_sd_clean_groups_es.png   (Q3 SD cohort: Inc-only/Dec-only vs NC)
#   - Console: etable() output for the slide tables (Q2, Act 32 break, Q3 clean groups)

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
fig_dir     <- file.path(project_dir, "figures")

max_year <- 2019  # trim COVID years

# ---- Load panels ----
pa_panel      <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))
pa_rate_panel <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
pa_panel      <- pa_panel[year <= max_year]

# ---- Build fiscal controls + revenue shares from AFR ----
pa_afr_raw <- fread(file.path(data_dir, "pa_afr_panel.csv"))
pa_xwalk_afr <- unique(pa_rate_panel[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
pa_afr_raw[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
pa_afr_raw <- merge(pa_afr_raw, pa_xwalk_afr, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
pa_afr_raw[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)

fiscal_controls <- pa_afr_raw[!is.na(cousub_fips), {
  police     <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
  streets    <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                  `public_works-highways_and_streets_expenditures`, streets_and_roads))
  state_aid  <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                                  `intergovernmental_revenues-state_government`, state_govt))
  total_rev  <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
  eit_rev    <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                                  earned_income_tax_revenues, earned_income_tax))
  re_tax     <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                                  real_estate_tax_revenues, real_estate_tax))
  ig_state   <- as.numeric(`intergovernmental_revenues-state_government`)
  ig_fed     <- as.numeric(`intergovernmental_revenues-federal_government`)
  ig_local   <- as.numeric(`intergovernmental_revenues-local_government`)
  intergov   <- rowSums(cbind(ig_state, ig_fed, ig_local), na.rm = TRUE)
  intergov[is.na(ig_state) & is.na(ig_fed) & is.na(ig_local)] <- NA
  list(
    cousub_fips = cousub_fips, year = yr,
    log_police     = log_pos(police),
    log_streets    = log_pos(streets),
    log_debt       = log_pos(as.numeric(total_debt)),
    log_state_aid  = log_pos(state_aid),
    share_eit      = ifelse(total_rev > 0, eit_rev / total_rev, NA_real_),
    share_re       = ifelse(total_rev > 0, re_tax / total_rev, NA_real_),
    share_intergov = ifelse(total_rev > 0, intergov / total_rev, NA_real_)
  )
}]

pa_panel <- merge(pa_panel, fiscal_controls,
  by.x = c("work_city", "year"), by.y = c("cousub_fips", "year"), all.x = TRUE)

# Save for downstream reuse
saveRDS(fiscal_controls, file.path(data_dir, "pa_fiscal_controls.rds"))

cat(sprintf("PA panel: %s rows, %d unique cousubs\n",
            format(nrow(pa_panel), big.mark = ","), uniqueN(pa_panel$work_city)))
cat(sprintf("Fiscal-controls coverage (police): %d of %d rows\n",
            sum(!is.na(pa_panel$log_police)), nrow(pa_panel)))

# ---- Estimation sample: jobs > 50, controls non-missing ----
pa_samp <- pa_panel[total_jobs > 50 & !is.na(resident_eit) & !is.na(sd_eit) &
                    !is.na(log_police) & !is.na(log_streets) & !is.na(log_debt) &
                    !is.na(log_state_aid) & !is.na(share_eit) & !is.na(share_re) &
                    !is.na(share_intergov)]

cat(sprintf("Estimation sample: %s rows, %d cousubs\n",
            format(nrow(pa_samp), big.mark = ","), uniqueN(pa_samp$work_city)))

ctrls_chr <- "log_police + log_streets + log_debt + log_state_aid + share_eit + share_re + share_intergov"
mk <- function(lhs, rhs) as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls_chr,
                                            " | work_city + year"))

# ============================================================================
# Q2 — Headline TWFE (total stack vs muni+SD decomposition)
# ============================================================================
q2_total_sli  <- feols(mk("share_live_in",  "total_eit"), data = pa_samp, cluster = ~work_city)
q2_total_lr   <- feols(mk("log_live_in",    "total_eit"), data = pa_samp, cluster = ~work_city)
q2_total_lj   <- feols(mk("log_total_jobs", "total_eit"), data = pa_samp, cluster = ~work_city)

q2_dec_sli    <- feols(mk("share_live_in",  "resident_eit + sd_eit"), data = pa_samp, cluster = ~work_city)
q2_dec_lr     <- feols(mk("log_live_in",    "resident_eit + sd_eit"), data = pa_samp, cluster = ~work_city)
q2_dec_lj     <- feols(mk("log_total_jobs", "resident_eit + sd_eit"), data = pa_samp, cluster = ~work_city)

cat("\n========== Q2 TOTAL STACK ==========\n")
print(etable(q2_total_sli, q2_total_lr, q2_total_lj,
             headers = c("share_live_in", "log(Resident)", "log(Total Jobs)"),
             keep = "%total_eit"))

cat("\n========== Q2 DECOMPOSITION ==========\n")
print(etable(q2_dec_sli, q2_dec_lr, q2_dec_lj,
             headers = c("share_live_in", "log(Resident)", "log(Total Jobs)"),
             keep = "%resident_eit|%sd_eit"))

# ============================================================================
# Act 32 break — pre/post 2012 split for muni and SD rates
# ============================================================================
pa_samp[, post := as.integer(year >= 2012)]

q2_act32_sli <- feols(mk("share_live_in",  "resident_eit:i(post) + sd_eit:i(post)"),
                      data = pa_samp, cluster = ~work_city)
q2_act32_lr  <- feols(mk("log_live_in",    "resident_eit:i(post) + sd_eit:i(post)"),
                      data = pa_samp, cluster = ~work_city)
q2_act32_lj  <- feols(mk("log_total_jobs", "resident_eit:i(post) + sd_eit:i(post)"),
                      data = pa_samp, cluster = ~work_city)

cat("\n========== ACT 32 BREAK ==========\n")
print(etable(q2_act32_sli, q2_act32_lr, q2_act32_lj,
             headers = c("share_live_in", "log(Resident)", "log(Total Jobs)"),
             keep = "%resident_eit|%sd_eit"))

# ============================================================================
# Q3 — Clean groups (Inc-only / Dec-only / NC; flip-floppers dropped)
# ============================================================================
muni_events <- pa_rate_panel[!is.na(cousub_fips) & event_type %in% c("increase", "decrease"),
  .(any_inc = any(event_type == "increase"),
    any_dec = any(event_type == "decrease")),
  by = cousub_fips]
muni_events[, group := fifelse(any_inc & !any_dec, "Inc",
                       fifelse(!any_inc & any_dec, "Dec",
                       fifelse(any_inc & any_dec, "FF", "NC")))]

# Munis that never appear with a rate change → NC by construction
all_munis <- unique(pa_samp$work_city)
group_dt  <- data.table(work_city = all_munis)
group_dt  <- merge(group_dt, muni_events[, .(work_city = cousub_fips, group)],
                   by = "work_city", all.x = TRUE)
group_dt[is.na(group), group := "NC"]

cat("\nQ3 group counts:\n")
print(group_dt[, .N, by = group])

pa_q3 <- merge(pa_samp, group_dt, by = "work_city")
pa_q3 <- pa_q3[group != "FF"]  # drop flip-floppers
pa_q3[, group := factor(group, levels = c("NC", "Inc", "Dec"))]

q3_sli <- feols(mk("share_live_in",  "i(group, resident_eit, ref = \"NC\") + sd_eit"),
                data = pa_q3, cluster = ~work_city)
q3_lr  <- feols(mk("log_live_in",    "i(group, resident_eit, ref = \"NC\") + sd_eit"),
                data = pa_q3, cluster = ~work_city)
q3_lj  <- feols(mk("log_total_jobs", "i(group, resident_eit, ref = \"NC\") + sd_eit"),
                data = pa_q3, cluster = ~work_city)

cat("\n========== Q3 CLEAN GROUPS ==========\n")
print(etable(q3_sli, q3_lr, q3_lj,
             headers = c("share_live_in", "log(Resident)", "log(Total Jobs)")))

# ============================================================================
# Sun-Abraham event studies (figures)
# ============================================================================
build_first_event <- function(dt, rate_var, direction = "inc") {
  d <- dt[, c("work_city", "year", rate_var), with = FALSE][order(work_city, year)]
  setnames(d, rate_var, "rate")
  d[, lag := shift(rate), by = work_city]
  if (direction == "inc") {
    d <- d[!is.na(lag) & rate - lag > 1e-6,
           .(first_event = min(year)), by = work_city]
  } else {
    d <- d[!is.na(lag) & lag - rate > 1e-6,
           .(first_event = min(year)), by = work_city]
  }
  d
}

pa_sa <- copy(pa_samp)
pa_sa <- merge(pa_sa, build_first_event(pa_samp, "resident_eit", "inc"),
               by = "work_city", all.x = TRUE)
setnames(pa_sa, "first_event", "first_muni_inc")
pa_sa <- merge(pa_sa, build_first_event(pa_samp, "sd_eit", "inc"),
               by = "work_city", all.x = TRUE)
setnames(pa_sa, "first_event", "first_sd_inc")
pa_sa[, cohort_muni := ifelse(is.na(first_muni_inc), 10000L, as.integer(first_muni_inc))]
pa_sa[, cohort_sd   := ifelse(is.na(first_sd_inc),   10000L, as.integer(first_sd_inc))]

# Sun-Abraham coefficient extractor. fixest's sunab() returns coefficients
# already aggregated to relative-time level, with names "year::N" (no cohort).
extract_es <- function(model, label) {
  ct <- model$coeftable
  nms <- rownames(ct)
  is_sa <- grepl("^year::-?[[:digit:]]+$", nms)
  rel  <- as.integer(sub("^year::(-?[[:digit:]]+)$", "\\1", nms[is_sa]))
  data.table(rel_year = rel,
             est      = ct[is_sa, "Estimate"],
             se       = ct[is_sa, "Std. Error"],
             cohort   = label)[order(rel_year)]
}

# Outcome labels for facet titles (must match the originals)
outcome_labs <- c(share_live_in = "share_live_in",
                  log_live_in   = "log(Resident workers)",
                  log_total_jobs = "log(Total jobs)")

empty_es <- data.table(rel_year = integer(), est = numeric(), se = numeric(),
                       cohort = character(), outcome = factor())

fit_es_three <- function(d, cohort_var, control_var, label, max_rel = 8) {
  outs <- c("share_live_in", "log_live_in", "log_total_jobs")
  rhs  <- paste0("sunab(", cohort_var, ", year) + ", control_var)
  parts <- lapply(outs, function(o) {
    m <- tryCatch(feols(mk(o, rhs), data = d, cluster = ~work_city),
                  error = function(e) NULL)
    if (is.null(m)) return(empty_es)
    es <- extract_es(m, label)
    if (nrow(es) == 0) return(empty_es)
    es[, outcome := factor(o, levels = outs, labels = outcome_labs[outs])]
    es[abs(rel_year) <= max_rel]
  })
  rbindlist(parts, fill = TRUE)
}

# Common ggplot theme used in the originals
es_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

# Reds and blues used in the originals
muni_red  <- "#C0392B"
sd_blue   <- "#2C7FB8"

build_es_plot <- function(es_dt, title, subtitle, xlab, color_map) {
  es_dt <- es_dt[!is.na(outcome) & !is.na(est)]
  if (nrow(es_dt) == 0) {
    cat("WARN: no event-study points to plot for '", title, "'\n", sep = "")
    return(invisible(NULL))
  }
  # Order outcomes consistently
  es_dt[, outcome := factor(as.character(outcome),
                            levels = unname(outcome_labs))]
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

# === Q2: muni cohort + SD cohort (each controls for the other rate) ===
pa_sa <- copy(pa_samp)
pa_sa <- merge(pa_sa, build_first_event(pa_samp, "resident_eit", "inc"),
               by = "work_city", all.x = TRUE)
setnames(pa_sa, "first_event", "first_muni_inc")
pa_sa <- merge(pa_sa, build_first_event(pa_samp, "sd_eit", "inc"),
               by = "work_city", all.x = TRUE)
setnames(pa_sa, "first_event", "first_sd_inc")
pa_sa[, cohort_muni := ifelse(is.na(first_muni_inc), 10000L, as.integer(first_muni_inc))]
pa_sa[, cohort_sd   := ifelse(is.na(first_sd_inc),   10000L, as.integer(first_sd_inc))]

es_q2 <- rbind(
  fit_es_three(pa_sa, "cohort_muni", "sd_eit",       "Muni rate increase"),
  fit_es_three(pa_sa, "cohort_sd",   "resident_eit", "SD rate increase")
)
p_q2 <- build_es_plot(
  es_q2,
  title    = "Q2 Sun-Abraham event study (each cohort controls for the other rate)",
  subtitle = "Muni cohort controls sd_eit; SD cohort controls resident_eit; 95% CI",
  xlab     = "Years relative to first rate increase",
  color_map = c("Muni rate increase" = muni_red, "SD rate increase" = sd_blue)
)
ggsave(file.path(fig_dir, "q2_event_study_decomp.png"), p_q2,
       width = 11, height = 4, dpi = 300)

# === Q3 muni-cohort: Inc-only and Dec-only vs NC, controlling for SD rate ===
pa_q3_sa <- merge(pa_q3, build_first_event(pa_q3, "resident_eit", "inc"),
                  by = "work_city", all.x = TRUE)
setnames(pa_q3_sa, "first_event", "first_muni_inc")
pa_q3_sa[, cohort_muni := ifelse(is.na(first_muni_inc), 10000L, as.integer(first_muni_inc))]
pa_q3_sa <- merge(pa_q3_sa, build_first_event(pa_q3, "resident_eit", "dec"),
                  by = "work_city", all.x = TRUE)
setnames(pa_q3_sa, "first_event", "first_muni_dec")
pa_q3_sa[, cohort_muni_dec := ifelse(is.na(first_muni_dec), 10000L, as.integer(first_muni_dec))]

es_q3 <- rbind(
  fit_es_three(pa_q3_sa[group %in% c("NC", "Inc")],
               "cohort_muni",     "sd_eit", "Inc-only"),
  fit_es_three(pa_q3_sa[group %in% c("NC", "Dec")],
               "cohort_muni_dec", "sd_eit", "Dec-only")
)
p_q3 <- build_es_plot(
  es_q3,
  title    = "Q3 muni-cohort event study (clean groups, controlling for SD rate)",
  subtitle = "Inc-only / Dec-only vs never-changed; 95% CI",
  xlab     = "Years relative to first muni rate change",
  color_map = c("Inc-only" = muni_red, "Dec-only" = sd_blue)
)
ggsave(file.path(fig_dir, "q2_clean_groups_es.png"), p_q3,
       width = 11, height = 4, dpi = 300)

# === Q3 SD-cohort: SD Inc-only and SD Dec-only vs SD NC, controlling for muni rate ===
pa_q3_sa <- merge(pa_q3_sa, build_first_event(pa_q3, "sd_eit", "inc"),
                  by = "work_city", all.x = TRUE)
setnames(pa_q3_sa, "first_event", "first_sd_inc")
pa_q3_sa[, cohort_sd_inc := ifelse(is.na(first_sd_inc), 10000L, as.integer(first_sd_inc))]
pa_q3_sa <- merge(pa_q3_sa, build_first_event(pa_q3, "sd_eit", "dec"),
                  by = "work_city", all.x = TRUE)
setnames(pa_q3_sa, "first_event", "first_sd_dec")
pa_q3_sa[, cohort_sd_dec := ifelse(is.na(first_sd_dec), 10000L, as.integer(first_sd_dec))]

sd_events <- pa_q3_sa[, .(any_sd_inc = any(!is.na(first_sd_inc)),
                          any_sd_dec = any(!is.na(first_sd_dec))),
                      by = work_city]
sd_events[, sd_group := fifelse(any_sd_inc & !any_sd_dec, "Inc",
                         fifelse(!any_sd_inc & any_sd_dec, "Dec",
                         fifelse(any_sd_inc & any_sd_dec, "FF", "NC")))]
pa_q3_sa <- merge(pa_q3_sa, sd_events[, .(work_city, sd_group)],
                  by = "work_city", all.x = TRUE)

es_q3sd <- rbind(
  fit_es_three(pa_q3_sa[sd_group %in% c("NC", "Inc")],
               "cohort_sd_inc", "resident_eit", "SD Inc-only"),
  fit_es_three(pa_q3_sa[sd_group %in% c("NC", "Dec")],
               "cohort_sd_dec", "resident_eit", "SD Dec-only")
)
p_q3sd <- build_es_plot(
  es_q3sd,
  title    = "Q3 SD-cohort event study (clean SD groups, controlling for muni rate)",
  subtitle = "SD Inc-only / SD Dec-only vs SD never-changed; 95% CI",
  xlab     = "Years relative to first SD rate change",
  color_map = c("SD Inc-only" = muni_red, "SD Dec-only" = sd_blue)
)
ggsave(file.path(fig_dir, "q3_sd_clean_groups_es.png"), p_q3sd,
       width = 11, height = 4, dpi = 300)

cat("\nFigures saved:\n")
cat("  ", file.path(fig_dir, "q2_event_study_decomp.png"), "\n")
cat("  ", file.path(fig_dir, "q2_clean_groups_es.png"), "\n")
cat("  ", file.path(fig_dir, "q3_sd_clean_groups_es.png"), "\n")
cat("\nDone.\n")
