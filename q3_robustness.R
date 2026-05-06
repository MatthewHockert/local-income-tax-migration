# q3_robustness.R
# Address staggered / multi-event treatment in Q3:
#   1. Visualize the time distribution of muni-rate increases and decreases.
#   2. Single-event TWFE: restrict to inc-only munis with exactly ONE increase
#      (drops the 17 with 2+). No negative-weighting problem under this
#      restriction, so SA is unnecessary -- TWFE on continuous rate is
#      unbiased.
#   3. DCDH (continuous, multi-event) on inc-only + NC. Skip dec-only
#      (only 15 munis -- DCDH would be uninformative).

suppressMessages({
  library(data.table); library(fixest); library(ggplot2)
  library(DIDmultiplegtDYN)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
fig_dir     <- file.path(project_dir, "figures")
max_year    <- 2019

pa_panel      <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
pa_rate_panel <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))

# ============================================================================
# 1. Distribution of muni rate change events over time
# ============================================================================
events <- pa_rate_panel[!is.na(cousub_fips) & year >= 2002 & year <= 2019 &
                        event_type %in% c("increase", "decrease")]
events[, event := factor(event_type, levels = c("decrease", "increase"),
                         labels = c("Rate decrease", "Rate increase"))]

p_events <- ggplot(events, aes(year, fill = event)) +
  geom_bar(position = "dodge", color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 2011.5, linetype = "dotted", color = "grey30") +
  annotate("text", x = 2012, y = Inf, label = "Act 32", hjust = -0.1, vjust = 1.5,
           color = "grey30", size = 3.5) +
  scale_fill_manual(values = c("Rate increase" = "#C0392B",
                               "Rate decrease" = "#2C7FB8")) +
  scale_x_continuous(breaks = seq(2002, 2019, 2)) +
  labs(title = "Muni rate change events over time, 2002--2019",
       subtitle = "Each bar is a (muni, year) rate change. Dotted line: Act 32 effective date.",
       x = "Year", y = "Number of muni rate changes", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))
ggsave(file.path(fig_dir, "rate_events_by_year.png"), p_events,
       width = 9, height = 4.5, dpi = 300)
cat("Saved: rate_events_by_year.png\n")

# Companion: events per muni distribution
inc_per_muni <- events[event_type == "increase", .N, by = cousub_fips]
dec_per_muni <- events[event_type == "decrease", .N, by = cousub_fips]
n_per_muni <- rbind(
  data.table(direction = "Rate increase", n_events = inc_per_muni$N),
  data.table(direction = "Rate decrease", n_events = dec_per_muni$N))

p_per_muni <- ggplot(n_per_muni, aes(factor(n_events), fill = direction)) +
  geom_bar(position = "dodge", color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("Rate increase" = "#C0392B",
                               "Rate decrease" = "#2C7FB8")) +
  labs(title = "Number of rate-change events per muni",
       subtitle = "Among munis that ever changed (2002--2019).",
       x = "Number of events", y = "Number of munis", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))
ggsave(file.path(fig_dir, "events_per_muni.png"), p_per_muni,
       width = 7, height = 4, dpi = 300)
cat("Saved: events_per_muni.png\n")

# ============================================================================
# 2. Single-event TWFE robustness
# ============================================================================
# Build estimation sample (mirror run_slide_models.R)
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

# Group classification (same as Q3)
ev <- pa_rate_panel[!is.na(cousub_fips) & event_type %in% c("increase","decrease"),
                    .(any_inc=any(event_type=="increase"),
                      any_dec=any(event_type=="decrease"),
                      n_inc=sum(event_type=="increase"),
                      n_dec=sum(event_type=="decrease")),
                    by=cousub_fips]
ev[, group := fifelse(any_inc & !any_dec, "Inc",
              fifelse(!any_inc & any_dec, "Dec",
              fifelse(any_inc & any_dec, "FF", "NC")))]
all_munis <- unique(pa_samp$work_city)
gd <- data.table(work_city=all_munis)
gd <- merge(gd, ev[, .(work_city=cousub_fips, group, n_inc, n_dec)],
            by="work_city", all.x=TRUE)
gd[is.na(group), group := "NC"]
gd[is.na(n_inc), n_inc := 0]
gd[is.na(n_dec), n_dec := 0]

cat("\nQ3 group composition:\n")
print(gd[, .N, by=group])

cat("\nInc-only munis by # of increases:\n")
print(gd[group == "Inc", .N, by=n_inc][order(n_inc)])

# Single-event subset: NC + Inc-only with exactly 1 increase
single_munis <- gd[group == "NC" | (group == "Inc" & n_inc == 1), work_city]
multi_munis  <- gd[group == "Inc" & n_inc >= 2, work_city]
cat(sprintf("\nSingle-event sample: %d munis (NC=%d, Inc with n_inc==1: %d)\n",
            length(single_munis),
            nrow(gd[group=="NC"]), nrow(gd[group=="Inc" & n_inc==1])))
cat(sprintf("Excluded multi-event Inc munis: %d\n", length(multi_munis)))

pa_single <- pa_samp[work_city %in% single_munis]

# TWFE: continuous rate on Inc-vs-NC single-event sample
ctrls <- "log_police + log_streets + log_debt + log_state_aid + share_eit + share_re"
mk <- function(lhs, rhs) as.formula(paste0(lhs, " ~ ", rhs, " + ", ctrls,
                                            " | work_city + year"))

cat("\n===== Single-event TWFE: total stack =====\n")
m_se_sli <- feols(mk("share_live_in",  "total_eit"), data=pa_single, cluster=~work_city)
m_se_lr  <- feols(mk("log_live_in",    "total_eit"), data=pa_single, cluster=~work_city)
m_se_lj  <- feols(mk("log_total_jobs", "total_eit"), data=pa_single, cluster=~work_city)
print(etable(m_se_sli, m_se_lr, m_se_lj,
             headers = c("share_live_in", "log(Resident)", "log(Total Jobs)"),
             keep = "%total_eit"))

cat("\n===== Single-event TWFE: muni + SD decomposition =====\n")
m_se_sli2 <- feols(mk("share_live_in",  "resident_eit + sd_eit"), data=pa_single, cluster=~work_city)
m_se_lr2  <- feols(mk("log_live_in",    "resident_eit + sd_eit"), data=pa_single, cluster=~work_city)
m_se_lj2  <- feols(mk("log_total_jobs", "resident_eit + sd_eit"), data=pa_single, cluster=~work_city)
# print full table so SD shows up
for (m in list(sli = m_se_sli2, lr = m_se_lr2, lj = m_se_lj2)) {
  cat("\n  ", deparse(formula(m))[1], "\n")
  print(round(m$coeftable[grepl("eit", rownames(m$coeftable)), c("Estimate","Std. Error","Pr(>|t|)")], 4))
  cat("  N =", nobs(m), "\n")
}

# ============================================================================
# 3. DCDH on inc-only + NC (continuous, multi-event)
# ============================================================================
cat("\n===== DCDH (continuous, multi-event): inc-only + NC =====\n")

inc_or_nc <- gd[group %in% c("NC", "Inc"), work_city]
pa_dcdh <- pa_samp[work_city %in% inc_or_nc]
pa_dcdh[, work_city_int := as.integer(as.factor(work_city))]
pa_dcdh <- as.data.frame(pa_dcdh)

print_dcdh <- function(res, label) {
  cat("\n--- ", label, "---\n")
  if (!is.null(res$results$Effects))    cat("Effects:\n");    if (!is.null(res$results$Effects))    print(round(res$results$Effects, 4))
  if (!is.null(res$results$Placebos))   cat("Placebos:\n");   if (!is.null(res$results$Placebos))   print(round(res$results$Placebos, 4))
  if (!is.null(res$results$ATE))        cat("ATE:\n");        if (!is.null(res$results$ATE))        print(round(res$results$ATE, 4))
}

dcdh_inc_sli <- did_multiplegt_dyn(
  df = pa_dcdh, outcome = "share_live_in",
  group = "work_city_int", time = "year",
  treatment = "resident_eit", effects = 8, placebo = 5,
  continuous = 1, cluster = "work_city_int", graph_off = TRUE)
print_dcdh(dcdh_inc_sli, "PA inc-only + NC: share_live_in")

dcdh_inc_lr <- did_multiplegt_dyn(
  df = pa_dcdh, outcome = "log_live_in",
  group = "work_city_int", time = "year",
  treatment = "resident_eit", effects = 8, placebo = 5,
  continuous = 1, cluster = "work_city_int", graph_off = TRUE)
print_dcdh(dcdh_inc_lr, "PA inc-only + NC: log(Resident)")

dcdh_inc_lj <- did_multiplegt_dyn(
  df = pa_dcdh, outcome = "log_total_jobs",
  group = "work_city_int", time = "year",
  treatment = "resident_eit", effects = 8, placebo = 5,
  continuous = 1, cluster = "work_city_int", graph_off = TRUE)
print_dcdh(dcdh_inc_lj, "PA inc-only + NC: log(Total Jobs)")

cat("\nDone.\n")
