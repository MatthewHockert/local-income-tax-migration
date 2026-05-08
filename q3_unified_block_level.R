# q3_unified_block_level.R
# Block-level robustness for the unified-design EIT migration analysis.
# Runs four estimators on block x year data with two-way clustering at
# (muni, sd_id):
#   (1) Q2 TWFE pooled (block FE + year FE)
#   (2) Single-event TWFE on Inc-only with <=2 events
#   (3) Stacked binary
#   (4) Stacked continuous (headline)
# DCDH is skipped at this scale (infeasible).
#
# Inputs:  data/pa_block_panel.rds  (built by build_block_panel.R)
# Outputs: data/q3_unified/block_level_results.csv
#          data/q3_unified/block_level_event_studies.rds

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack <- 5L

ctrls <- c("share_police","share_streets","share_debt_svc",
           "share_eit","share_re","share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")

# Run-mode flag: set ONLY_HEADLINE=1 to run only stacked continuous
ONLY_HEADLINE <- Sys.getenv("ONLY_HEADLINE", "0") == "1"

# ---------------------------------------------------------------------------
# Load block panel
# ---------------------------------------------------------------------------
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
cat(sprintf("Rows: %s; blocks: %s; munis: %d; SDs: %d\n",
            format(nrow(b), big.mark=","), format(uniqueN(b$block_geoid), big.mark=","),
            uniqueN(b$muni), uniqueN(b$sd_id)))

# Sort and lag for event detection at muni-year level
muni_year <- unique(b[, .(muni, year, resident_eit, sd_eit, muni_nonres_eit)])
setorder(muni_year, muni, year)
muni_year[, res_lag := shift(resident_eit), by = muni]
muni_year[, dose_muni := resident_eit - res_lag]
muni_year[, has_event := !is.na(dose_muni) & abs(dose_muni) > 1e-6]

# First muni event year per muni (cohort)
cohort <- muni_year[has_event == TRUE,
                     .(c_muni = min(year), dir_muni = sign(dose_muni[which.min(year)])),
                     by = muni]

# ---------------------------------------------------------------------------
# Cell classification at muni level
# Uses muni's first event direction x binding state (post-event home_stack vs nonres)
# Inc-bind:    rate up, post home_stack > nonres -> bill rises with muni rate
# Inc-becomes: rate up, pre home_stack <= nonres, post >  nonres
# Inc-nobind:  rate up, post home_stack <= nonres -> placebo
# Dec-bind:    rate down, post home_stack > nonres
# Dec-becomes-non: rate down, pre > nonres, post <= nonres
# Dec-nobind:  rate down, post home_stack <= nonres
# NC:          no muni-rate event ever in panel (strict)
# ---------------------------------------------------------------------------
my <- merge(muni_year, cohort, by = "muni", all.x = TRUE)
my[, home_stack := resident_eit + sd_eit]
my[, bind := !is.na(home_stack) & !is.na(muni_nonres_eit) & home_stack > muni_nonres_eit]

cell_dt <- my[!is.na(c_muni), {
  pre  <- .SD[year == c_muni - 1L]
  post <- .SD[year == c_muni]
  cell <- "Uncertain"
  if (nrow(pre) == 1 && nrow(post) == 1 &&
      !is.na(pre$home_stack) && !is.na(post$home_stack) &&
      !is.na(pre$muni_nonres_eit) && !is.na(post$muni_nonres_eit)) {
    pre_b  <- pre$home_stack  > pre$muni_nonres_eit
    post_b <- post$home_stack > post$muni_nonres_eit
    if (dir_muni[1] > 0) {
      cell <- if (post_b && pre_b) "Inc-bind"
              else if (post_b && !pre_b) "Inc-becomes"
              else "Inc-nobind"
    } else {
      cell <- if (post_b && pre_b) "Dec-bind"
              else if (!post_b && pre_b) "Dec-becomes-non"
              else "Dec-nobind"
    }
  }
  list(cell_muni = cell)
}, by = muni]

# Strict NC: no event ever
nc_munis <- setdiff(unique(my$muni), unique(cohort$muni))
cell_dt <- rbind(cell_dt,
                  data.table(muni = nc_munis, cell_muni = "NC"))

cat("\nCell distribution (muni level):\n")
print(cell_dt[, .N, keyby = cell_muni])

# Drop Uncertain (had events but unclassifiable)
cell_dt <- cell_dt[cell_muni != "Uncertain"]

# Attach to block panel
b <- merge(b, cell_dt, by = "muni", all.x = TRUE)
b <- merge(b, cohort[, .(muni, c_muni)], by = "muni", all.x = TRUE)
b[, dose_muni := resident_eit - shift(resident_eit), by = .(block_geoid)]
# Use muni-year dose so blocks share the same dose within muni-year
b[, dose_muni := NULL]
my_dose <- unique(my[, .(muni, year, dose_muni_yr = dose_muni)])
b <- merge(b, my_dose, by = c("muni","year"), all.x = TRUE)
setnames(b, "dose_muni_yr", "dose_muni")
b[, cum_dose_muni := cumsum(ifelse(is.na(dose_muni), 0, dose_muni)),
  by = block_geoid]

# Restrict to classified blocks
b <- b[!is.na(cell_muni)]
cat(sprintf("\nAnalysis sample: %s rows; %s blocks\n",
            format(nrow(b), big.mark=","),
            format(uniqueN(b$block_geoid), big.mark=",")))

# ---------------------------------------------------------------------------
# (1) Q2 TWFE pooled: block FE + year FE, treatment = current resident_eit
#     Two-way cluster at (muni, sd_id)
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("(1) Q2 TWFE pooled\n")
cat("=========================================================\n")
twfe_results <- list()
if (!ONLY_HEADLINE) for (o in outcomes) {
  d <- b[, c("block_geoid","year","muni","sd_id","resident_eit","sd_eit",
              o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(o, " ~ resident_eit + sd_eit + ", ctrls_str,
                          " | block_geoid + year"))
  cat(sprintf("  Outcome %s, N=%s ... ", o, format(nrow(d), big.mark=",")))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  for (tt in c("resident_eit","sd_eit")) {
    rr <- ct[term == tt]
    if (nrow(rr) == 1) {
      twfe_results[[length(twfe_results)+1L]] <- data.table(
        estimator = "twfe_pooled", outcome = o, term = tt,
        est = rr$Estimate, se = rr$`Std. Error`, p = rr$`Pr(>|t|)`,
        n = nobs(m))
    }
  }
  cat("done\n")
}
twfe_dt <- rbindlist(twfe_results, fill = TRUE)
print(twfe_dt)

# ---------------------------------------------------------------------------
# (2) Single-event TWFE: muni has exactly 1 increase in panel, classified
#     Inc-bind / Inc-becomes / Inc-nobind. Dose = cum increase.
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("(2) Single-event TWFE (Inc with <=2 events) + NC\n")
cat("=========================================================\n")
n_inc <- my[has_event & dose_muni > 0, .N, by = muni]
single_inc <- n_inc[N <= 2L, muni]
single_munis <- intersect(single_inc, b[grepl("^Inc", cell_muni), unique(muni)])
single_set <- union(single_munis, b[cell_muni == "NC", unique(muni)])
b_single <- b[muni %in% single_set]

single_results <- list()
if (!ONLY_HEADLINE) for (o in outcomes) {
  d <- b_single[, c("block_geoid","year","muni","sd_id","cell_muni",
                     "cum_dose_muni", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  d[, treat_cell := factor(cell_muni,
       levels = c("NC","Inc-bind","Inc-becomes","Inc-nobind"))]
  f <- as.formula(paste0(o, " ~ i(treat_cell, cum_dose_muni, ref='NC') + ",
                          ctrls_str, " | block_geoid + year"))
  cat(sprintf("  Outcome %s, N=%s ... ", o, format(nrow(d), big.mark=",")))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("Inc-", term)]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  single_results[[length(single_results)+1L]] <- data.table(
    estimator = "single_event_twfe", outcome = o,
    cell = ic$cell, est = ic$Estimate, se = ic$`Std. Error`,
    p = ic$`Pr(>|t|)`, n = nobs(m))
  cat("done\n")
}
single_dt <- rbindlist(single_results, fill = TRUE)
print(single_dt)

# ---------------------------------------------------------------------------
# Stack builder used by (3) and (4): for each cohort year cy, combine
# treated munis (cohort=cy) with NC controls in window [cy-K, cy+K].
# ---------------------------------------------------------------------------
build_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t <- merge(d_t, t_munis[, .(muni, treat_cell = cell)], by = "muni", all.x = TRUE)
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := year - cy]
    d
  }), fill = TRUE)
}

cat("\nBuilding stacked panel (this is large)...\n")
stk <- build_stack(b)
stk[, stack_block := paste0(stack, "_", block_geoid)]
stk[, stack_year  := paste0(stack, "_", year)]
# Within-stack baseline rate: average pre-event resident_eit per muni
stk[, base_rate := mean(resident_eit[rel < 0], na.rm = TRUE), by = .(stack, muni)]
stk[!is.finite(base_rate), base_rate := 0]
stk[, dose_stack := resident_eit - base_rate]
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
stk[, treat_cell := factor(treat_cell, levels = cell_levels)]
# Binary post-treatment indicator
stk[, post_treat := as.integer(rel >= 0 & treat_cell != "NC")]

cat(sprintf("Stacked panel: %s rows; treated blocks: %s; NC blocks: %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell == "NC", block_geoid]), big.mark=",")))

# ---------------------------------------------------------------------------
# (3) Stacked binary
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("(3) Stacked binary DiD\n")
cat("=========================================================\n")
stkbin_results <- list()
if (!ONLY_HEADLINE) for (o in outcomes) {
  d <- stk[, c("block_geoid","year","muni","sd_id","stack_block","stack_year",
                "treat_cell","post_treat", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  d[, post_by_cell := interaction(treat_cell, post_treat, drop = TRUE)]
  f <- as.formula(paste0(o, " ~ i(treat_cell, post_treat, ref='NC') + ",
                          ctrls_str, " | stack_block + stack_year"))
  cat(sprintf("  Outcome %s, N=%s ... ", o, format(nrow(d), big.mark=",")))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("treat_cell::", term)]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  stkbin_results[[length(stkbin_results)+1L]] <- data.table(
    estimator = "stacked_binary", outcome = o,
    cell = ic$cell, est = ic$Estimate, se = ic$`Std. Error`,
    p = ic$`Pr(>|t|)`, n = nobs(m))
  cat("done\n")
}
stkbin_dt <- rbindlist(stkbin_results, fill = TRUE)
print(stkbin_dt)

# ---------------------------------------------------------------------------
# (4) Stacked continuous (HEADLINE)
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("(4) Stacked continuous DiD (HEADLINE)\n")
cat("=========================================================\n")
stkcon_results <- list()
es_results <- list()
for (o in outcomes) {
  d <- stk[, c("block_geoid","year","muni","sd_id","stack_block","stack_year",
                "treat_cell","dose_stack","rel", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                          ctrls_str, " | stack_block + stack_year"))
  cat(sprintf("  Outcome %s, N=%s ... ", o, format(nrow(d), big.mark=",")))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("treat_cell::", term)]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  stkcon_results[[length(stkcon_results)+1L]] <- data.table(
    estimator = "stacked_continuous", outcome = o,
    cell = ic$cell, est = ic$Estimate, se = ic$`Std. Error`,
    p = ic$`Pr(>|t|)`, n = nobs(m),
    r2 = fitstat(m, "r2", simplify = TRUE),
    wr2 = fitstat(m, "wr2", simplify = TRUE),
    dep_mean = mean(d[[o]], na.rm = TRUE))
  cat("done\n")
}
stkcon_dt <- rbindlist(stkcon_results, fill = TRUE)
print(stkcon_dt)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
all_results <- rbindlist(list(twfe_dt, single_dt, stkbin_dt, stkcon_dt),
                          fill = TRUE)
fwrite(all_results, file.path(out_dir, "block_level_results.csv"))
cat(sprintf("\nSaved data/q3_unified/block_level_results.csv (%d rows)\n",
            nrow(all_results)))
cat("Done.\n")
