# q3_unified_spillover.R
# Spillover spec: when a treated muni X (Inc-bind or Inc-becomes) raises its
# resident rate, do its first-ring and second-ring neighbors gain residents?
#
# Reuses the unified panel (muni x year) and cell classification from
# q3_unified.R. Adds two spatial-lag indicators:
#   ring1_treated_post_Yt = 1 if any first-ring neighbor of Y is in Inc-bind
#                             or Inc-becomes by year t
#   ring2_treated_post_Yt = same for second ring
#
# Outcomes: log(residents_employed) primary; log(live_and_work) and
# log(out_commuters) for completeness.
#
# Receiver Y's controls:
#   - Own treatment cells (cell_muni, cell_sd, cell_nr) x dose interactions
#   - Own fiscal share controls (share_police etc.)
#   - Y FE + year FE
#   - SE clustered at muni Y

suppressMessages({
  library(data.table); library(fixest)
})
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
dir.create(out_dir, showWarnings = FALSE)
max_year <- 2019
K_stack <- 5L

# ---------------------------------------------------------------------------
# 1. Load unified panel (re-using same construction as q3_unified.R)
# ---------------------------------------------------------------------------
wp <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))[year <= max_year]
rp <- as.data.table(readRDS(file.path(data_dir, "pa_residence_panel.rds")))[year <= max_year]
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
rings <- as.data.table(readRDS(file.path(data_dir, "pa_neighbors_rings.rds")))
hwy_static <- as.data.table(readRDS(file.path(data_dir, "pa_highway_static.rds")))
corridors <- as.data.table(readRDS(file.path(data_dir, "pa_highway_corridors.rds")))

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

panel[, in_commuters    := live_outside]
panel[, out_commuters   := residents_work_outside]
panel[, total_residents := residents_employed]
log_pos <- function(x) ifelse(as.numeric(x) > 0, log(as.numeric(x)), NA_real_)
panel[, log_live_and_work   := log_pos(live_and_work)]
panel[, log_in_commuters    := log_pos(in_commuters)]
panel[, log_out_commuters   := log_pos(out_commuters)]
panel[, log_total_jobs      := log_pos(total_jobs)]
panel[, log_total_residents := log_pos(total_residents)]

# Fiscal controls (share-based)
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
xw  <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
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
  police_exp <- as.numeric(ifelse(!is.na(police_expenditures),
                                   police_expenditures, police))
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

ctrls <- c("share_police", "share_streets", "share_debt_svc",
           "share_eit", "share_re", "share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

# ---------------------------------------------------------------------------
# 2. Cell classification (own treatment cells -- same logic as compare script)
# ---------------------------------------------------------------------------
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
    ev_dir == "Inc" & prebind == 1 & postbind == 0, "Inc-loses",
    ev_dir == "Dec" & prebind == 1 & postbind == 1, "Dec-bind",
    ev_dir == "Dec" & prebind == 1 & postbind == 0, "Dec-becomes-non",
    ev_dir == "Dec" & prebind == 0 & postbind == 0, "Dec-nobind",
    ev_dir == "Dec" & prebind == 0 & postbind == 1, "Dec-gains",
    default = NA_character_
  )]
  setnames(ev, c("c_var","baseline_var","cell"),
           c(paste0("c_", tag), paste0("baseline_", tag), paste0("cell_", tag)))
  ev <- ev[, .SD, .SDcols = c("muni",
                              paste0("c_", tag),
                              paste0("baseline_", tag),
                              paste0("cell_", tag))]
  p2 <- merge(p, ev, by = "muni", all.x = TRUE)
  cell_v <- paste0("cell_", tag)
  c_v    <- paste0("c_", tag)
  base_v <- paste0("baseline_", tag)
  dose_v <- paste0("dose_", tag)
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
                   "Dec-bind", "Dec-becomes-non", "Dec-nobind",
                   "Inc-loses", "Dec-gains", "Uncertain")
  p2[, (cell_v) := factor(get(cell_v), levels = cell_levels)]
  p2
}

panel <- classify_treatment(panel, "resident_eit",   "muni")
panel <- classify_treatment(panel, "sd_eit",          "sd")
panel <- classify_treatment(panel, "muni_nonres_eit", "nr")
# Drop Uncertain munis
u <- Reduce(union, list(unique(panel[cell_muni == "Uncertain", muni]),
                         unique(panel[cell_sd   == "Uncertain", muni]),
                         unique(panel[cell_nr   == "Uncertain", muni])))
cat(sprintf("Spillover: dropping %d uncertain munis\n", length(u)))
panel <- panel[!muni %in% u]
panel[, cell_muni := droplevels(cell_muni)]
panel[, cell_sd   := droplevels(cell_sd)]
panel[, cell_nr   := droplevels(cell_nr)]

# ---------------------------------------------------------------------------
# 3. Build ring-based spatial-lag indicators
# ---------------------------------------------------------------------------
# Identify "binding-treated" munis: cell_muni in {Inc-bind, Inc-becomes}
binding_munis <- unique(panel[cell_muni %in% c("Inc-bind","Inc-becomes"),
                              .(treated_muni = muni, c_treated = c_muni, cell_treated = cell_muni)])
cat(sprintf("\nBinding-treated munis (Inc-bind or Inc-becomes): %d\n", nrow(binding_munis)))

# Same restricted to Inc-bind only (sharper variant)
incbind_munis <- unique(panel[cell_muni == "Inc-bind",
                               .(treated_muni = muni, c_treated = c_muni)])
cat(sprintf("Inc-bind munis only: %d\n", nrow(incbind_munis)))

# For each receiver Y in each year t, compute:
#   ring1_post_bind = 1 if any first-ring neighbor X is in binding_munis & t >= c_treated_X
#   ring2_post_bind = 1 if any second-ring neighbor of Y is binding-treated & t >= c_treated
#   ring1_post_incb / ring2_post_incb = same but restricted to Inc-bind treated

build_ring_indicator <- function(panel, rings, treated_set, ring_num) {
  # Restrict rings to ring_num
  r <- rings[ring == ring_num, .(muni, neighbor)]
  # Merge in treated muni info
  r2 <- merge(r, treated_set, by.x = "neighbor", by.y = "treated_muni",
              all = FALSE, allow.cartesian = TRUE)
  # For each (muni, year), check if any neighbor is treated by that year
  ind <- merge(panel[, .(muni, year)], r2[, .(muni, c_treated)],
               by = "muni", allow.cartesian = TRUE)
  ind <- ind[year >= c_treated, .N, by = .(muni, year)]
  ind[, post := 1L]
  ind[, .(muni, year, post)]
}

# Lower-rate-neighbor variant: receiver Y in ring k of treated X AND Y's
# home_stack at year t is below X's post-event home_stack.
build_ring_indicator_lower <- function(panel, rings, treated_set, treated_postrate,
                                        ring_num) {
  r <- rings[ring == ring_num, .(muni, neighbor)]
  r2 <- merge(r, treated_postrate, by.x = "neighbor", by.y = "treated_muni",
              all = FALSE, allow.cartesian = TRUE)
  rec_hs <- panel[, .(muni, year, home_stack_Y = home_stack)]
  ind <- merge(rec_hs, r2[, .(muni, c_treated, post_homestack_X)],
               by = "muni", allow.cartesian = TRUE)
  ind <- ind[year >= c_treated &
             !is.na(home_stack_Y) & !is.na(post_homestack_X) &
             home_stack_Y < post_homestack_X]
  ind <- unique(ind[, .(muni, year)])
  ind[, post := 1L]
  ind
}

# Corridor variant: receiver Y in ring k of treated X AND Y shares any
# interstate corridor with X (from pa_highway_corridors.rds).
build_ring_indicator_corridor <- function(panel, rings, treated_set, corridors,
                                           ring_num) {
  r <- rings[ring == ring_num, .(muni = muni, neighbor = neighbor)]
  # neighbor here is the treated muni; muni is the receiver.
  # Filter rings to only pairs where (receiver, treated) share corridor.
  r_corr <- merge(r, corridors,
                  by.x = c("muni", "neighbor"),
                  by.y = c("muni_a", "muni_b"),
                  all = FALSE)
  r_corr <- merge(r_corr, treated_set, by.x = "neighbor", by.y = "treated_muni",
                  all = FALSE, allow.cartesian = TRUE)
  ind <- merge(panel[, .(muni, year)], r_corr[, .(muni, c_treated)],
               by = "muni", allow.cartesian = TRUE)
  ind <- ind[year >= c_treated, .(muni, year)]
  ind <- unique(ind)
  ind[, post := 1L]
  ind
}

# Binding cells (Inc-bind + Inc-becomes)
bind_set <- unique(binding_munis[, .(treated_muni, c_treated)])
ring1_bind <- build_ring_indicator(panel, rings, bind_set, 1L)
setnames(ring1_bind, "post", "ring1_post_bind")
ring2_bind <- build_ring_indicator(panel, rings, bind_set, 2L)
setnames(ring2_bind, "post", "ring2_post_bind")

# Inc-bind only
incb_set <- unique(incbind_munis[, .(treated_muni, c_treated)])
ring1_incb <- build_ring_indicator(panel, rings, incb_set, 1L)
setnames(ring1_incb, "post", "ring1_post_incb")
ring2_incb <- build_ring_indicator(panel, rings, incb_set, 2L)
setnames(ring2_incb, "post", "ring2_post_incb")

# Lower-rate variant: receiver Y has home_stack < treated X's post-event home_stack
# Build treated_postrate = (treated_muni, c_treated, post_homestack_X)
post_hs <- panel[, .(muni, year, home_stack)]
treated_postrate <- merge(incbind_munis,
                          post_hs[, .(muni, year, home_stack_post_x = home_stack)],
                          by.x = c("treated_muni","c_treated"),
                          by.y = c("muni","year"), all.x = TRUE)
treated_postrate <- treated_postrate[, .(treated_muni, c_treated,
                                          post_homestack_X = home_stack_post_x)]
ring1_lower <- build_ring_indicator_lower(panel, rings, incbind_munis,
                                          treated_postrate, 1L)
setnames(ring1_lower, "post", "ring1_post_lower")
ring2_lower <- build_ring_indicator_lower(panel, rings, incbind_munis,
                                          treated_postrate, 2L)
setnames(ring2_lower, "post", "ring2_post_lower")

# Corridor variants (Inc-bind treated, receiver shares interstate corridor with X)
ring1_corridor <- build_ring_indicator_corridor(panel, rings, incb_set,
                                                 corridors, 1L)
setnames(ring1_corridor, "post", "ring1_post_corr")
ring2_corridor <- build_ring_indicator_corridor(panel, rings, incb_set,
                                                 corridors, 2L)
setnames(ring2_corridor, "post", "ring2_post_corr")

panel <- Reduce(function(x, y) merge(x, y, by = c("muni","year"), all.x = TRUE),
                list(panel, ring1_bind, ring2_bind, ring1_incb, ring2_incb,
                     ring1_lower, ring2_lower, ring1_corridor, ring2_corridor))
for (v in c("ring1_post_bind","ring2_post_bind","ring1_post_incb","ring2_post_incb",
            "ring1_post_lower","ring2_post_lower",
            "ring1_post_corr","ring2_post_corr")) {
  panel[is.na(get(v)), (v) := 0L]
}

# Add receiver's own highway access for interaction
panel <- merge(panel,
               hwy_static[, .(muni = cousub_fips, has_interstate)],
               by = "muni", all.x = TRUE)
panel[is.na(has_interstate), has_interstate := 0L]

# Drop receivers who are themselves treated munis (so we estimate pure spillover)
panel_recv <- panel[cell_muni == "NC"]
cat(sprintf("\nReceiver pool (cell_muni==NC): %s muni-years, %d unique munis\n",
            format(nrow(panel_recv), big.mark = ","),
            uniqueN(panel_recv$muni)))

cat("\nRing1 post indicator coverage (binding cells):\n")
print(panel_recv[, .(N = .N), by = .(ring1_post_bind, year)][year %in% c(2010, 2015, 2019)])

# ---------------------------------------------------------------------------
# 4. Run spillover spec
# ---------------------------------------------------------------------------
outcomes <- c("log_total_residents", "log_live_and_work",
              "log_out_commuters", "share_work_outside")

run_spill <- function(p, outcome, ring_var = c("bind","incb","lower","corr")) {
  ring_var <- match.arg(ring_var)
  ring1 <- paste0("ring1_post_", ring_var)
  ring2 <- paste0("ring2_post_", ring_var)
  needed <- c(outcome, ctrls, ring1, ring2, "muni", "year")
  d <- p[, ..needed]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) return(NULL)
  f <- as.formula(paste0(outcome, " ~ ", ring1, " + ", ring2, " + ",
                          ctrls_str, " | muni + year"))
  feols(f, data = d, cluster = ~muni)
}

cat("\n========== Spillover: binding cells (Inc-bind + Inc-becomes) ==========\n")
res_bind <- list()
for (o in outcomes) {
  m <- run_spill(panel_recv, o, "bind")
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("^ring", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]; ct[, variant := "bind"]
  res_bind[[length(res_bind) + 1L]] <- ct
}

cat("\n========== Spillover: Inc-bind only ==========\n")
res_incb <- list()
for (o in outcomes) {
  m <- run_spill(panel_recv, o, "incb")
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("^ring", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]; ct[, variant := "incb"]
  res_incb[[length(res_incb) + 1L]] <- ct
}

cat("\n========== Spillover: Inc-bind, LOWER-RATE neighbors only ==========\n")
res_lower <- list()
for (o in outcomes) {
  m <- run_spill(panel_recv, o, "lower")
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("^ring", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]; ct[, variant := "lower"]
  res_lower[[length(res_lower) + 1L]] <- ct
}

cat("\n========== Spillover: Inc-bind, CORRIDOR-SHARED neighbors only ==========\n")
res_corr <- list()
for (o in outcomes) {
  m <- run_spill(panel_recv, o, "corr")
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("^ring", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]; ct[, variant := "corr"]
  res_corr[[length(res_corr) + 1L]] <- ct
}

# Joint spec: Inc-bind ring + ring x corridor interaction
cat("\n========== Spillover: Inc-bind ring x corridor interaction ==========\n")
res_intx <- list()
for (o in outcomes) {
  needed <- c(o, ctrls, "ring1_post_incb","ring2_post_incb",
              "ring1_post_corr","ring2_post_corr","muni","year")
  d <- panel_recv[, ..needed][complete.cases(panel_recv[, ..needed])]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(o,
       " ~ ring1_post_incb + ring2_post_incb + ",
            "ring1_post_corr + ring2_post_corr + ",
            ctrls_str, " | muni + year"))
  m <- feols(f, data = d, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"),
           skip_absent = TRUE)
  ct <- ct[grepl("^ring", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]; ct[, variant := "intx"]
  res_intx[[length(res_intx) + 1L]] <- ct
}

# Coverage
cat("\nLower-rate ring coverage (panel_recv):\n")
print(panel_recv[, .(N1 = sum(ring1_post_lower), N2 = sum(ring2_post_lower)),
                 by = year][year %in% c(2010, 2015, 2019)])
cat("\nCorridor-shared ring coverage (panel_recv):\n")
print(panel_recv[, .(N1 = sum(ring1_post_corr), N2 = sum(ring2_post_corr)),
                 by = year][year %in% c(2010, 2015, 2019)])

results <- rbindlist(c(res_bind, res_incb, res_lower, res_corr, res_intx), fill = TRUE)
results[, sig := fifelse(p < 0.01, "***",
                  fifelse(p < 0.05, "**",
                  fifelse(p < 0.10, "*", "")))]
results[, est_str := sprintf("%+.4f%s", est, ifelse(is.na(sig), "", sig))]
fwrite(results, file.path(out_dir, "spillover_results.csv"))
print(results[, .(variant, outcome, term, est = round(est,4),
                  se = round(se,4), p = round(p,4), n)])
cat("\nSaved spillover_results.csv\n")
