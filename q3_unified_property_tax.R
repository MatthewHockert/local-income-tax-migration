# q3_unified_property_tax.R
# Parallel exercise to the EIT headline: continuous stacked DiD using
# muni property tax millage as the treatment.
#
# Question: do residents sort in response to property tax changes the way
# they do in response to EIT changes? Theory (Oates 1969) predicts NO --
# property tax burden capitalizes into house prices and stays attached to
# the property, not the worker. Empirical prediction: the elasticity of
# log(workers living in block) to property tax should be substantially
# smaller in magnitude than the EIT elasticity (-0.21).
#
# Treatment: cumulative change in mills_on_mkt_val (effective property tax
# rate on market value) from the year before to the year of each muni's
# first material millage change.
# Event threshold: |Δmill| >= 0.5 (about a 5% relative change at the typical
# PA muni millage; smaller changes are noise or assessment drift).
# Direction: Inc if Δmill > 0; Dec if Δmill < 0.
#
# Controls: same fiscal share controls as EIT headline, PLUS resident_eit
# and sd_eit as level controls (absorbs concurrent EIT variation).
# FE/cluster: same as EIT block-level headline -- block^stack + year^stack,
# cluster ~muni + sd_id.
#
# Outputs:
#   data/q3_unified/proptax_block_results.csv
#   data/q3_unified/proptax_first_events.csv
# Prints elasticity comparison vs EIT headline.

suppressMessages({ library(data.table); library(fixest) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack     <- 5L

# ---------------------------------------------------------------------------
# 1. Build muni-year millage panel from AFR
# ---------------------------------------------------------------------------
afr <- fread(file.path(data_dir, "pa_afr_panel.csv"))
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
xw   <- unique(rate[!is.na(cousub_fips),
                     .(municipality_id, cousub_fips)])
xw[, municipality_id := as.character(municipality_id)]
xw[, cousub_fips := as.character(cousub_fips)]

# The AFR has two parallel ID columns: municipality_id (newer rows)
# and municipal_code (older rows where the millage data lives). Same
# underlying 5-digit code; coalesce.
afr[, muni_id := fifelse(!is.na(municipality_id),
                          as.character(municipality_id),
                          as.character(municipal_code))]
afr <- afr[!is.na(muni_id) & !is.na(year)]
afr[, mills_on_mkt_val := as.numeric(mills_on_mkt_val)]
afr[, total_mills      := as.numeric(total_mills)]

mp <- merge(afr[, .(muni_id, year, total_mills, mills_on_mkt_val)],
            xw, by.x = "muni_id", by.y = "municipality_id", all.x = TRUE)
mp <- mp[!is.na(cousub_fips) & year >= 2002 & year <= 2019]
setnames(mp, "cousub_fips", "muni")
# average if multiple rows per (muni, year)
mp <- mp[, .(total_mills = mean(total_mills, na.rm = TRUE),
              mills_on_mkt_val = mean(mills_on_mkt_val, na.rm = TRUE)),
          by = .(muni, year)]
for (cc in c("total_mills","mills_on_mkt_val")) {
  set(mp, which(!is.finite(mp[[cc]])), cc, NA_real_)
}
setkey(mp, muni, year)
mp[, dmill := mills_on_mkt_val - shift(mills_on_mkt_val), by = muni]

cat(sprintf("Muni-year millage observations (non-NA on mills_on_mkt_val): %s\n",
            format(sum(!is.na(mp$mills_on_mkt_val)), big.mark=",")))

# ---------------------------------------------------------------------------
# 2. Define first material millage event per muni
# ---------------------------------------------------------------------------
event_threshold <- 0.5  # mills on market value, ~5% relative change
mp[, event := !is.na(dmill) & abs(dmill) >= event_threshold]

first_evt <- mp[event == TRUE,
                 .SD[1, .(c_mill = year,
                          dose_mill_event = dmill,
                          dir = ifelse(dmill > 0, "Inc", "Dec"))],
                 by = muni]
nc_munis <- mp[, .(any_event = any(event, na.rm = TRUE)), by = muni][
  any_event == FALSE, muni]
cat(sprintf("Munis with property-tax event: %s\n",
            format(nrow(first_evt), big.mark=",")))
cat(sprintf("Inc events: %d ; Dec events: %d\n",
            sum(first_evt$dir == "Inc"), sum(first_evt$dir == "Dec")))
cat(sprintf("Never-changed munis: %s\n", format(length(nc_munis), big.mark=",")))
fwrite(first_evt, file.path(out_dir, "proptax_first_events.csv"))

# Build per-(muni, year) realized dose: cumulative Δmill since c_mill - 1
mp <- merge(mp, first_evt[, .(muni, c_mill, dir)], by = "muni", all.x = TRUE)
mp[, base_mill := mills_on_mkt_val[year == c_mill - 1L], by = muni]
mp[is.na(base_mill), base_mill := 0]
mp[, dose_stack := mills_on_mkt_val - base_mill]

# ---------------------------------------------------------------------------
# 3. Build block panel + merge millage info + EIT level controls
# ---------------------------------------------------------------------------
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
b[, muni := as.character(muni)]
b <- b[year >= 2002 & year <= 2019]
b <- merge(b, mp[, .(muni, year, mills_on_mkt_val, dmill, c_mill, dir,
                      base_mill, dose_stack)], by = c("muni","year"),
            all.x = TRUE)

# SD share controls
sd_ctrl <- as.data.table(readRDS(file.path(data_dir, "pa_sd_controls.rds")))
sd_ctrls <- c("share_state_rev_sd","share_local_rev_sd",
              "share_federal_rev_sd","share_property_tax_sd",
              "share_instructional_sd","share_support_sd",
              "share_capital_sd","share_debt_int_sd")
keep <- intersect(c("sd_id","year", sd_ctrls), names(sd_ctrl))
sd_ctrl <- sd_ctrl[, ..keep]
sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                     if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                   by = .(sd_id, year)]
for (cc in sd_ctrls) {
  set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
}
b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)

# ---------------------------------------------------------------------------
# 4. Build the stacked panel on property-tax cohorts
# ---------------------------------------------------------------------------
b[, treat_dir := dir]  # carry cohort direction onto every block-year
build_stack_proptax <- function(p, K = K_stack) {
  treated <- unique(p[!is.na(c_mill), .(muni, cohort = c_mill,
                                          dir = first(dir)), by = muni][
                                            !is.na(cohort)])
  treated <- unique(treated[, .(muni, cohort, dir)])
  nc <- p[muni %in% nc_munis]
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_m <- unique(treated[cohort == cy, .(muni, dir)])
    if (nrow(t_m) == 0) return(NULL)
    d_t <- p[muni %in% t_m$muni & year %in% win]
    d_t[, treat_dir := dir]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_dir := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]
    d
  }), fill = TRUE)
}
cat("Building property-tax-cohort stacked block panel...\n")
stk <- build_stack_proptax(b)
stk[, treat_dir := factor(treat_dir, levels = c("NC","Inc","Dec"))]
# NC blocks get dose_stack = 0 (they have no property-tax cohort)
stk[is.na(dose_stack), dose_stack := 0]

cat(sprintf("Stack rows: %s ; treated blocks: %s ; NC blocks: %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_dir != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_dir == "NC", block_geoid]), big.mark=",")))

# ---------------------------------------------------------------------------
# 5. Headline regression: same FE + cluster as EIT headline
# ---------------------------------------------------------------------------
muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
# EIT level controls (THIS is the additional control vs the EIT headline)
level_ctrls <- c("resident_eit","sd_eit")
ctrls_str <- paste(c(muni_ctrls, sd_ctrls, level_ctrls), collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")
res <- list()
for (o in outcomes) {
  cat(sprintf("\n[%s]\n", o))
  cols <- intersect(c("block_geoid","muni","sd_id","year","stack",
                        "treat_dir","dose_stack", o,
                        muni_ctrls, sd_ctrls, level_ctrls), names(stk))
  d <- stk[, ..cols]
  d <- d[complete.cases(d)]
  f <- as.formula(paste0(o,
       " ~ i(treat_dir, dose_stack, ref = 'NC') + ",
       ctrls_str, " | block_geoid^stack + year^stack"))
  m <- tryCatch(feols(f, data = d, cluster = ~muni + sd_id),
                 error = function(e) {
                   cat(sprintf("  error: %s\n", conditionMessage(e))); NULL })
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
           c("est","se","p"), skip_absent = TRUE)
  cat("  All term names:\n"); print(head(ct$term, 20))
  ic <- ct[grepl("treat_dir", term)]
  ic[, dir := sub(".*treat_dir::([^:]+):.*$", "\\1", term)]
  ic[, outcome := o]
  ic[, n := nobs(m)]
  ic[, r2 := round(fitstat(m, "r2", simplify = TRUE), 4)]
  res[[length(res)+1L]] <- ic[, .(outcome, dir, est, se, p, n, r2)]
  print(ic[, .(outcome, dir, est = round(est, 4), se = round(se, 4),
                p = round(p, 4))])
}
res_dt <- rbindlist(res, fill = TRUE)
fwrite(res_dt, file.path(out_dir, "proptax_block_results.csv"))

# ---------------------------------------------------------------------------
# 6. Side-by-side comparison vs EIT headline
# ---------------------------------------------------------------------------
cat("\n========= COMPARISON =========\n")
eit_headline_log_home <- -0.208  # block-level Inc-bind on log_home_count
eit_headline_se        <- 0.054
pt_inc <- res_dt[outcome == "log_home_count" & dir == "Inc"]
pt_dec <- res_dt[outcome == "log_home_count" & dir == "Dec"]
cat(sprintf("\nEIT headline (Inc-bind, block-level): %.3f (SE %.3f) per pp of EIT rate\n",
            eit_headline_log_home, eit_headline_se))
if (nrow(pt_inc) == 1) {
  cat(sprintf("Property-tax Inc:                     %.3f (SE %.3f) per mill on market value\n",
              pt_inc$est, pt_inc$se))
}
if (nrow(pt_dec) == 1) {
  cat(sprintf("Property-tax Dec:                     %.3f (SE %.3f) per mill on market value\n",
              pt_dec$est, pt_dec$se))
}

cat("\nDone.\n")
