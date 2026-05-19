# q3_unified_heterogeneity.R
# Diagnostic: who are the Inc-bind munis, and does the headline coefficient
# vary by muni type / size?
#
# 1. Classify each PA cousub by LSAD (city / borough / township) and by
#    pre-event size tertile.
# 2. Print composition of Inc-bind cell, ring-1, ring-2 receivers, NC controls.
# 3. Re-run stacked-continuous on log(live_and_work) with cell x type
#    interactions to see whether the -0.388 is driven by one muni type.
# 4. Report magnitudes in absolute jobs (not just log).

suppressMessages({
  library(sf); library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
max_year <- 2019
K_stack <- 5L

# ---------------------------------------------------------------------------
# 1. Build typology
# ---------------------------------------------------------------------------
cousub_geom <- as.data.table(st_drop_geometry(readRDS(file.path(data_dir, "pa_cousub.rds"))))
cousub_geom[, cousub_fips := COUSUBFP]
cousub_geom[, type := fcase(
  LSAD == "44", "township",
  LSAD == "21", "borough",
  LSAD == "25", "city",
  default = "other"
)]
type_panel <- unique(cousub_geom[, .(muni = cousub_fips, type, NAMELSAD)])
cat("Cousub typology:\n"); print(type_panel[, .N, by = type])

# ---------------------------------------------------------------------------
# 2. Build unified panel + cells (using same logic as q3_unified.R)
# ---------------------------------------------------------------------------
wp <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
rp <- as.data.table(readRDS(file.path(data_dir, "pa_residence_panel.rds")))[year <= max_year]
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
rings <- as.data.table(readRDS(file.path(data_dir, "pa_neighbors_rings.rds")))

setnames(wp, "work_city", "muni", skip_absent = TRUE)
setnames(rp, "home_city", "muni", skip_absent = TRUE)
panel <- merge(
  wp[, .(muni, year, live_and_work, live_outside, total_jobs, share_live_in)],
  rp[, .(muni, year, residents_employed, residents_work_in_own,
         residents_work_outside, share_work_outside)],
  by = c("muni", "year"), all = TRUE
)
rate_keep <- rate[!is.na(cousub_fips) & year <= max_year,
                  .(muni = cousub_fips, year, resident_eit, sd_eit, muni_nonres_eit)]
rate_keep <- rate_keep[, .(resident_eit = mean(resident_eit, na.rm = TRUE),
                           sd_eit       = max(sd_eit, na.rm = TRUE),
                           muni_nonres_eit = mean(muni_nonres_eit, na.rm = TRUE)),
                       by = .(muni, year)]
for (c in c("resident_eit","sd_eit","muni_nonres_eit"))
  rate_keep[!is.finite(get(c)), (c) := NA_real_]
panel <- merge(panel, rate_keep, by = c("muni", "year"), all.x = TRUE)
panel[, home_stack := resident_eit + sd_eit]
panel[, total_residents := residents_employed]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
panel[, log_live_and_work := log_pos(live_and_work)]

# Fiscal share controls
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
afr[, muni_id := ifelse(!is.na(municipality_id), municipality_id, municipal_code)]
afr <- merge(afr, xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
afr[, yr := ifelse(!is.na(reporting_year), as.integer(reporting_year), as.integer(year))]
fc <- afr[!is.na(cousub_fips), {
  total_rev <- as.numeric(ifelse(!is.na(total_revenues), total_revenues, revenues))
  total_exp <- as.numeric(total_expenditures)
  eit_rev <- as.numeric(ifelse(!is.na(earned_income_tax_revenues),
                               earned_income_tax_revenues, earned_income_tax))
  re_tax  <- as.numeric(ifelse(!is.na(real_estate_tax_revenues),
                               real_estate_tax_revenues, real_estate_tax))
  fed_ig <- as.numeric(`intergovernmental_revenues-federal_government`)
  sta_ig <- as.numeric(ifelse(!is.na(`intergovernmental_revenues-state_government`),
                               `intergovernmental_revenues-state_government`, state_govt))
  loc_ig <- as.numeric(`intergovernmental_revenues-local_government`)
  intergov <- rowSums(cbind(fed_ig, sta_ig, loc_ig), na.rm = TRUE)
  intergov[is.na(fed_ig) & is.na(sta_ig) & is.na(loc_ig)] <- NA_real_
  police_exp <- as.numeric(ifelse(!is.na(police_expenditures), police_expenditures, police))
  streets_exp <- as.numeric(ifelse(!is.na(`public_works-highways_and_streets_expenditures`),
                                    `public_works-highways_and_streets_expenditures`,
                                    streets_and_roads))
  debt_svc <- as.numeric(debt_service_expenditures)
  list(muni = cousub_fips, year = yr,
       share_police    = ifelse(total_exp > 0, police_exp  / total_exp, NA_real_),
       share_streets   = ifelse(total_exp > 0, streets_exp / total_exp, NA_real_),
       share_debt_svc  = ifelse(total_exp > 0, debt_svc    / total_exp, NA_real_),
       share_eit       = ifelse(total_rev > 0, eit_rev  / total_rev, NA_real_),
       share_re        = ifelse(total_rev > 0, re_tax   / total_rev, NA_real_),
       share_intergov  = ifelse(total_rev > 0, intergov / total_rev, NA_real_))
}]
fc <- fc[!is.na(muni) & !is.na(year)]
fc <- fc[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
         by = .(muni, year)]
panel <- merge(panel, fc, by = c("muni", "year"), all.x = TRUE)
panel <- panel[, lapply(.SD, function(x) {
  if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)
}), by = .(muni, year)]
for (col in names(panel)) {
  if (is.numeric(panel[[col]])) {
    set(panel, which(!is.finite(panel[[col]])), col, NA_real_)
  }
}
ctrls <- c("share_police","share_streets","share_debt_svc",
           "share_eit","share_re","share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

# Cell classification (muni rate only -- focus is muni-rate Inc cells)
classify_treatment <- function(p, rate_var, tag) {
  setorder(p, muni, year)
  p[, home_binds := as.integer(home_stack > muni_nonres_eit)]
  lag_col <- paste0("lag_", tag)
  p[, (lag_col) := shift(get(rate_var)), by = muni]
  ev_col <- paste0("ev_", tag)
  p[, (ev_col) := fifelse(is.na(get(lag_col)), NA_character_,
                  fifelse(get(rate_var) > get(lag_col) + 1e-6, "Inc",
                  fifelse(get(rate_var) < get(lag_col) - 1e-6, "Dec", NA_character_)))]
  ev <- p[!is.na(get(ev_col)), .SD[1], by = muni,
          .SDcols = c("year", ev_col, lag_col, rate_var, "home_binds")]
  setnames(ev, c("year", ev_col, lag_col, rate_var, "home_binds"),
           c("c_var","ev_dir","baseline_var","first_post","postbind"))
  pbind <- p[, .(muni, year, lag_home_binds = shift(home_binds)), by = muni]
  ev <- merge(ev, pbind[, .(muni, year, lag_home_binds)],
              by.x = c("muni","c_var"), by.y = c("muni","year"), all.x = TRUE)
  ev[, prebind := lag_home_binds]; ev[, lag_home_binds := NULL]
  ev[, cell := fcase(
    ev_dir == "Inc" & prebind == 1 & postbind == 1, "Inc-bind",
    ev_dir == "Inc" & prebind == 0 & postbind == 1, "Inc-becomes",
    ev_dir == "Inc" & prebind == 0 & postbind == 0, "Inc-nobind",
    ev_dir == "Dec" & prebind == 1 & postbind == 1, "Dec-bind",
    ev_dir == "Dec" & prebind == 1 & postbind == 0, "Dec-becomes-non",
    ev_dir == "Dec" & prebind == 0 & postbind == 0, "Dec-nobind",
    default = NA_character_
  )]
  setnames(ev, c("c_var","baseline_var","cell"),
           c(paste0("c_", tag), paste0("baseline_", tag), paste0("cell_", tag)))
  ev <- ev[, .SD, .SDcols = c("muni", paste0("c_", tag), paste0("baseline_", tag),
                              paste0("cell_", tag))]
  p2 <- merge(p, ev, by = "muni", all.x = TRUE)
  cell_v <- paste0("cell_", tag); c_v <- paste0("c_", tag)
  base_v <- paste0("baseline_", tag); dose_v <- paste0("dose_", tag)
  # FIX: True NC = no change in rate_var; Uncertain = had changes but cell unclassifiable
  rate_changes <- p2[!is.na(get(rate_var)), .(
    n = sum(abs(diff(get(rate_var))) > 1e-6, na.rm = TRUE)
  ), by = muni]
  truly_stable <- rate_changes[n == 0, muni]
  p2[muni %in% truly_stable,                            (cell_v) := "NC"]
  p2[!muni %in% truly_stable & is.na(get(cell_v)),     (cell_v) := "Uncertain"]
  p2[, (dose_v) := fifelse(!is.na(get(c_v)) & year >= get(c_v),
                           get(rate_var) - get(base_v), 0)]
  cell_levels <- c("NC", "Inc-bind", "Inc-becomes", "Inc-nobind",
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind", "Uncertain")
  p2[, (cell_v) := factor(get(cell_v), levels = cell_levels)]
  p2
}
panel <- classify_treatment(panel, "resident_eit", "muni")
# Drop Uncertain munis
u <- unique(panel[cell_muni == "Uncertain", muni])
cat(sprintf("Heterogeneity: dropping %d uncertain munis\n", length(u)))
panel <- panel[!muni %in% u]
panel[, cell_muni := droplevels(cell_muni)]

# Strip Dec munis (paper-wide policy).
dec_cell_set <- c("Dec-bind","Dec-becomes-non","Dec-nobind","Dec-gains")
dec_munis <- unique(panel[as.character(cell_muni) %in% dec_cell_set, muni])
cat(sprintf("Heterogeneity: dropping %d Dec munis (paper excludes Dec cells)\n",
            length(dec_munis)))
panel <- panel[!muni %in% dec_munis]
panel[, cell_muni := droplevels(cell_muni)]

# Merge typology
panel <- merge(panel, type_panel[, .(muni, type)], by = "muni", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3. Compositional summary
# ---------------------------------------------------------------------------
# Mean residents_employed per muni (pre-event for treated, panel mean for NC)
muni_size <- panel[, .(mean_residents = mean(total_residents, na.rm = TRUE),
                       mean_lw       = mean(live_and_work, na.rm = TRUE)),
                   by = muni]
muni_size <- merge(muni_size, type_panel[, .(muni, type, NAMELSAD)], by = "muni", all.x = TRUE)
muni_cells <- unique(panel[, .(muni, cell_muni)])
muni_size <- merge(muni_size, muni_cells, by = "muni", all.x = TRUE)

cat("\n========== Cell composition by muni type ==========\n")
print(muni_size[!is.na(cell_muni),
                .N, by = .(cell_muni, type)][order(cell_muni, -N)])
cat("\n========== Cell composition: mean size ==========\n")
print(muni_size[!is.na(cell_muni),
                .(N = .N,
                  mean_residents = round(mean(mean_residents, na.rm = TRUE)),
                  mean_lw        = round(mean(mean_lw, na.rm = TRUE))),
                by = .(cell_muni, type)][order(cell_muni, -N)])

cat("\n========== Inc-bind muni list ==========\n")
print(muni_size[cell_muni == "Inc-bind"][order(-mean_residents),
              .(muni, type, NAMELSAD, mean_residents = round(mean_residents),
                mean_lw = round(mean_lw))])

# ---------------------------------------------------------------------------
# 4. Heterogeneity: stacked continuous on log_live_and_work, by muni type
# ---------------------------------------------------------------------------
build_stack <- function(p, side_tag = "muni") {
  cell_v <- paste0("cell_", side_tag); c_v <- paste0("c_", side_tag)
  dose_v <- paste0("dose_", side_tag)
  treated_munis <- unique(p[!is.na(get(c_v)) & get(cell_v) != "NC",
                            .(muni, cohort = get(c_v), cell = get(cell_v))])
  nc_munis <- unique(p[get(cell_v) == "NC", muni])
  cohort_years <- sort(unique(treated_munis$cohort))
  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated_munis[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(get(dose_v)[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := get(dose_v) - base_rate]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                   "Dec-bind","Dec-becomes-non","Dec-nobind")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  stk
}
stk <- build_stack(panel)

# Combined non-city group
stk[, type_collapsed := fifelse(type == "city", "city", "non_city")]
panel[, type_collapsed := fifelse(type == "city", "city", "non_city")]

cat("\n========== Heterogeneity: city vs non_city (borough + township) ==========\n")
for (t in c("city","non_city")) {
  sub <- stk[type_collapsed == t]
  if (nrow(sub) == 0) next
  d <- sub[, c("log_live_and_work","treat_cell","dose_stack","stack_muni",
                "stack_year","muni", ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0 || sum(d$treat_cell == "Inc-bind") < 5) {
    cat("\n--- type =", t, "(insufficient Inc-bind obs)\n"); next
  }
  f <- as.formula(paste0("log_live_and_work ~ i(treat_cell, dose_stack, ref='NC') + ",
                          ctrls_str, " | stack_muni + stack_year"))
  m <- feols(f, data = d, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ct <- ct[grepl("treat_cell::", term)]
  ct[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]
  cat("\n--- type =", t, "; N =", format(nrow(d), big.mark = ","),
      "; Inc-bind munis =", uniqueN(d$muni[d$treat_cell == "Inc-bind"]), "---\n")
  print(ct[, .(cell, est = round(Estimate, 3), se = round(`Std. Error`, 3),
               p = round(`Pr(>|t|)`, 4))])
}

cat("\n========== Three-way split (kept for comparison): ==========\n")
for (t in c("city","borough","township")) {
  sub <- stk[type == t]
  if (nrow(sub) == 0) next
  d <- sub[, c("log_live_and_work","treat_cell","dose_stack","stack_muni",
                "stack_year","muni", ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0 || sum(d$treat_cell == "Inc-bind") < 5) {
    cat("\n--- type =", t, "(insufficient Inc-bind obs)\n"); next
  }
  f <- as.formula(paste0("log_live_and_work ~ i(treat_cell, dose_stack, ref='NC') + ",
                          ctrls_str, " | stack_muni + stack_year"))
  m <- feols(f, data = d, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ct <- ct[grepl("treat_cell::", term)]
  ct[, cell := sub("^.*treat_cell::([^:]+):.*$", "\\1", term)]
  cat("\n--- type =", t, "; N =", format(nrow(d), big.mark = ","),
      "; Inc-bind munis =", uniqueN(d$muni[d$treat_cell == "Inc-bind"]), "---\n")
  print(ct[, .(cell, est = round(Estimate, 3), se = round(`Std. Error`, 3),
               p = round(`Pr(>|t|)`, 4))])
}

# ---------------------------------------------------------------------------
# 5. Magnitudes in absolute jobs
# ---------------------------------------------------------------------------
cat("\n========== City subset: full estimator suite + ES ==========\n")
suppressMessages(library(DIDmultiplegtDYN))

p_city <- panel[type == "city"]
cat(sprintf("City subset: %s muni-years, %d unique munis, Inc-bind = %d\n",
            format(nrow(p_city), big.mark = ","),
            uniqueN(p_city$muni),
            uniqueN(p_city[cell_muni == "Inc-bind", muni])))

run_twfe_pooled_city <- function(p, outcome) {
  needed <- c(outcome, ctrls, "cell_muni", "dose_muni", "muni", "year")
  d <- p[, ..needed]; d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(cell_muni, dose_muni, ref='NC') + ",
       ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}
run_twfe_single_city <- function(p, outcome) {
  count_changes <- function(x) {
    x <- x[!is.na(x)]; if (length(x) < 2) return(0L)
    sum(abs(diff(x)) > 1e-6)
  }
  ec <- p[, .(n_res = count_changes(resident_eit)), by = muni]
  keep <- ec[n_res <= 2, muni]
  needed <- c(outcome, ctrls, "cell_muni", "dose_muni", "muni", "year")
  d <- p[muni %in% keep, ..needed]; d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(cell_muni, dose_muni, ref='NC') + ",
       ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}
build_stack_city <- function(p) {
  treated_munis <- unique(p[!is.na(c_muni) & cell_muni != "NC",
                             .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated_munis$cohort))
  build_one <- function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    t_munis <- treated_munis[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }
  stk <- rbindlist(lapply(cohort_years, build_one), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, base_rate := mean(dose_muni[rel < 0], na.rm = TRUE), by = .(stack, muni)]
  stk[!is.finite(base_rate), base_rate := 0]
  stk[, dose_stack := dose_muni - base_rate]
  stk[, post := as.integer(rel >= 0)]
  stk[, treat := treat_cell != "NC"]
  cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                   "Dec-bind","Dec-becomes-non","Dec-nobind")
  stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
  stk
}

run_stack_binary_city <- function(stk, outcome) {
  d <- stk[, c(outcome, ctrls, "treat_cell", "post", "stack_muni",
                "stack_year", "muni"), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  d[, treat_post := as.integer(treat_cell != "NC" & post == 1)]
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, treat_post, ref='NC') + ",
       ctrls_str, " | stack_muni + stack_year"))
  feols(f, data = d, cluster = ~muni)
}
run_stack_cont_city <- function(stk, outcome) {
  d <- stk[, c(outcome, ctrls, "treat_cell", "dose_stack", "stack_muni",
                "stack_year", "muni"), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_stack, ref='NC') + ",
       ctrls_str, " | stack_muni + stack_year"))
  feols(f, data = d, cluster = ~muni)
}
run_dcdh_city <- function(p, outcome, target_cell) {
  d <- p[cell_muni %in% c("NC", target_cell)]
  cell_lbl <- as.character(d$cell_muni)
  needed <- c(outcome, "resident_eit", "muni", "year", ctrls)
  d <- d[, ..needed]; d[, cell_lbl := cell_lbl]
  d <- d[complete.cases(d[, .SD, .SDcols = c(outcome, "resident_eit", "muni", "year")])]
  if (nrow(d) == 0 || uniqueN(d[cell_lbl != "NC", muni]) < 2) return(NULL)
  d[, muni_int := as.integer(as.factor(muni))]
  res <- tryCatch(suppressMessages(did_multiplegt_dyn(
    df = as.data.frame(d), outcome = outcome,
    group = "muni_int", time = "year",
    treatment = "resident_eit",
    effects = 3, placebo = 2, continuous = 1,
    cluster = "muni_int", graph_off = TRUE
  )), error = function(e) NULL)
  if (is.null(res) || is.null(res$results$ATE)) return(NULL)
  ate <- res$results$ATE
  data.table(cell = target_cell,
             est = as.numeric(ate[1, "Estimate"]),
             se  = as.numeric(ate[1, "SE"]),
             n = as.numeric(ate[1, "N"]))
}

# Run all five on city subset
extract_cell <- function(m) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("(cell_muni|treat_cell)::", term)]
  ct[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  ct[, .(cell, est = round(est, 3), se = round(se, 3), p = round(p, 4))]
}

stk_city <- build_stack_city(p_city)
cat("\n>> Q2 TWFE pooled (city)\n")
m1 <- run_twfe_pooled_city(p_city, "log_live_and_work")
print(extract_cell(m1))
cat("\n>> Single-event TWFE (city)\n")
m2 <- run_twfe_single_city(p_city, "log_live_and_work")
print(extract_cell(m2))
cat("\n>> Stacked binary (city)\n")
m3 <- run_stack_binary_city(stk_city, "log_live_and_work")
print(extract_cell(m3))
cat("\n>> Stacked continuous (city)\n")
m4 <- run_stack_cont_city(stk_city, "log_live_and_work")
print(extract_cell(m4))
cat("\n>> DCDH (city, Inc-bind cell)\n")
m5 <- run_dcdh_city(p_city, "log_live_and_work", "Inc-bind")
print(m5)

# ES: Stacked continuous event study on city subset (per-pp dose-response over event-time)
cat("\n========== City Inc-bind: STACKED CONTINUOUS event study ==========\n")
stk_city_es <- stk_city[treat_cell %in% c("NC","Inc-bind")]
needed_es <- c("log_live_and_work", ctrls, "rel", "dose_stack", "treat_cell",
               "stack_muni", "stack_year", "muni")
d_es_sc <- stk_city_es[, ..needed_es]
d_es_sc <- d_es_sc[complete.cases(d_es_sc) & abs(rel) <= 5]
# Restrict to Inc-bind dose for treated, zero for NC (already handled by dose_stack=0 for NC)
f_sc_es <- as.formula(paste0("log_live_and_work ~ i(rel, dose_stack, ref=-1) + ",
                              ctrls_str, " | stack_muni + stack_year"))
m_sc_es <- feols(f_sc_es, data = d_es_sc, cluster = ~muni)
ct_sc <- as.data.table(m_sc_es$coeftable, keep.rownames = "term")
setnames(ct_sc, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
         skip_absent = TRUE)
ct_sc <- ct_sc[grepl("^rel::", term)]
ct_sc[, rel_year := as.integer(sub("^rel::(-?[[:digit:]]+):.*$", "\\1", term))]
ct_sc <- rbind(ct_sc, data.table(term="ref", est=0, se=0, p=NA_real_,
                                  rel_year=-1L), fill=TRUE)
ct_sc <- ct_sc[order(rel_year)]
print(ct_sc[, .(rel_year, est = round(est,3), se = round(se,3), p = round(p,4))])

# Save figure
suppressMessages(library(ggplot2))
fig_dir <- file.path(project_dir, "figures")
es_plot_data <- ct_sc[, .(rel_year, est, se)]
p_fig_sc <- ggplot(es_plot_data, aes(rel_year, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "#7A0019") +
  geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se),
                width = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.5, color = "#7A0019") +
  geom_point(size = 1.6, color = "#7A0019") +
  labs(title = "City Inc-bind: log(live_and_work) stacked-continuous ES",
       subtitle = "Per-pp dose-response in event-time; 8 PA cities; 95% CI",
       x = "Years relative to first muni-rate event",
       y = "Per-pp coefficient on dose_stack") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"))
ggsave(file.path(fig_dir, "unified_es_city_inc_bind_lw_stackcont.png"),
       p_fig_sc, width = 9, height = 4.5, dpi = 300)
cat("Saved figures/unified_es_city_inc_bind_lw_stackcont.png\n")

# ES: Sun-Abraham on city subset (binary cohort) -- KEPT FOR REFERENCE
cat("\n========== City Inc-bind: Sun-Abraham (binary cohort, reference) ==========\n")
p_city_es <- p_city[cell_muni %in% c("NC","Inc-bind")]
p_city_es[, cohort := ifelse(is.na(c_muni), 10000L, as.integer(c_muni))]
needed <- c("log_live_and_work", ctrls, "cohort", "muni", "year")
d_es <- p_city_es[, ..needed]; d_es <- d_es[complete.cases(d_es)]
f_sa <- as.formula(paste0("log_live_and_work ~ sunab(cohort, year) + ",
                           ctrls_str, " | muni + year"))
m_sa <- feols(f_sa, data = d_es, cluster = ~muni)
ct_sa <- as.data.table(m_sa$coeftable, keep.rownames = "term")
setnames(ct_sa, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
         skip_absent = TRUE)
ct_sa <- ct_sa[grepl("^year::", term)]
ct_sa[, rel_year := as.integer(sub("^year::(-?[[:digit:]]+)$", "\\1", term))]
ct_sa <- ct_sa[abs(rel_year) <= 5]
ct_sa <- rbind(ct_sa, data.table(term="ref", est=0, se=0, p=NA_real_, rel_year=-1L), fill=TRUE)
ct_sa <- ct_sa[order(rel_year)]
print(ct_sa[, .(rel_year, est = round(est,3), se = round(se,3), p = round(p,4))])

# Save ES figure
suppressMessages(library(ggplot2))
fig_dir <- file.path(project_dir, "figures")
es_plot_data <- ct_sa[, .(rel_year, est, se)]
p_fig <- ggplot(es_plot_data, aes(rel_year, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "#7A0019") +
  geom_errorbar(aes(ymin = est - 1.96*se, ymax = est + 1.96*se),
                width = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.5, color = "#7A0019") +
  geom_point(size = 1.6, color = "#7A0019") +
  labs(title = "City Inc-bind: log(live_and_work) Sun-Abraham ES",
       subtitle = "8 PA cities (Wilkes-Barre, Hazleton, Johnstown, Jeannette, Coatesville, Warren, Carbondale, Farrell)",
       x = "Years relative to first muni-rate event",
       y = "Coefficient") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"))
ggsave(file.path(fig_dir, "unified_es_city_inc_bind_lw.png"),
       p_fig, width = 9, height = 4.5, dpi = 300)
cat("Saved figures/unified_es_city_inc_bind_lw.png\n")

cat("\n========== Inc-bind magnitudes in absolute jobs ==========\n")
incbind_munis <- unique(panel[cell_muni == "Inc-bind", muni])
absolute <- panel[muni %in% incbind_munis & year >= c_muni - 1 & year <= c_muni + 5,
                  .(muni, year, rel = year - c_muni, live_and_work, total_residents)]
absolute_summary <- absolute[, .(mean_lw = round(mean(live_and_work, na.rm = TRUE), 1),
                                  median_lw = round(median(live_and_work, na.rm = TRUE), 1),
                                  mean_res = round(mean(total_residents, na.rm = TRUE), 1)),
                              by = rel][order(rel)]
print(absolute_summary)
cat(sprintf("\n  Implied: a -39%% effect on a mean Inc-bind muni with %.0f live_and_work workers = -%.0f workers\n",
            absolute_summary[rel == -1, mean_lw],
            absolute_summary[rel == -1, mean_lw] * 0.39))

cat("\nDone.\n")
