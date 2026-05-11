# q3_unified_block_joint.R
# Block-level joint stacked-continuous DiD. Mirrors the cousub headline
# (q3_unified_full_table.R): both the muni-side cell x dose interaction and
# the SD-side cell x dose interaction enter the same regression, on a
# **stacked** panel with block_geoid^stack + year^stack fixed effects and
# two-way clustering by (muni, sd_id).
#
# Stacking rule (same as cousub): for each muni-side cohort year c, build
# a sub-experiment containing all treated blocks whose primary muni first
# changes in year c, plus all NC blocks (blocks in munis whose muni
# resident rate never changes), restricted to the event window
# [c - K, c + K]. NC blocks are replicated into every stack. dose_stack is
# the muni resident rate net of the within-(stack, muni) pre-event mean.
# SD-side cells (cell_sd, dose_sd) and SD-fiscal share controls are
# inherited from the cousub-level prep, exactly as in
# q3_unified_block_es_full.R.

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack <- 5L

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
sd_ctrls   <- c("share_state_rev_sd","share_local_rev_sd",
                "share_federal_rev_sd","share_property_tax_sd",
                "share_instructional_sd","share_support_sd",
                "share_capital_sd","share_debt_int_sd")
ctrls <- c(muni_ctrls, sd_ctrls)
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

# ---------------------------------------------------------------------------
# 1. Source cousub-level prep for muni + SD + nonres cell labels / doses
#    and the six muni AFR share controls. (Same pattern as
#    q3_unified_block_es_full.R, kept consistent.)
# ---------------------------------------------------------------------------
cat("Sourcing cousub-level full table prep for SD/nonres cells...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
cousub_panel <- e$panel
cousub_meta <- unique(cousub_panel[, .(muni, year,
                                        cell_sd, cell_nr,
                                        dose_sd, dose_nr,
                                        c_muni, cell_muni,
                                        share_police, share_streets, share_debt_svc,
                                        share_eit, share_re, share_intergov)])

# ---------------------------------------------------------------------------
# 2. Load block panel and attach cousub-level cell/dose/control metadata
# ---------------------------------------------------------------------------
cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
drop_cols <- intersect(c("share_police","share_streets","share_debt_svc",
                          "share_eit","share_re","share_intergov"),
                        names(b))
if (length(drop_cols)) b[, (drop_cols) := NULL]
b <- merge(b, cousub_meta, by = c("muni","year"), all.x = TRUE)

sd_ctrls_path <- file.path(data_dir, "pa_sd_controls.rds")
if (file.exists(sd_ctrls_path)) {
  sd_ctrl <- as.data.table(readRDS(sd_ctrls_path))
  sd_ctrl_cols <- c("sd_id","year", sd_ctrls)
  sd_ctrl_cols <- intersect(sd_ctrl_cols, names(sd_ctrl))
  sd_ctrl <- sd_ctrl[, ..sd_ctrl_cols]
  sd_ctrl <- sd_ctrl[, lapply(.SD, function(x)
                       if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                     by = .(sd_id, year)]
  for (cc in sd_ctrls) {
    set(sd_ctrl, which(!is.finite(sd_ctrl[[cc]])), cc, NA_real_)
  }
  b <- merge(b, sd_ctrl, by = c("sd_id","year"), all.x = TRUE)
  cat(sprintf("Merged SD fiscal controls: %d / %d block-years\n",
              sum(!is.na(b$share_state_rev_sd)), nrow(b)))
} else {
  cat("WARNING: data/pa_sd_controls.rds missing; SD controls absent.\n")
  for (cc in sd_ctrls) b[, (cc) := NA_real_]
}

cat("\nMuni cell distribution (block-year):\n")
print(b[, .N, keyby = cell_muni])
cat("\nSD cell distribution (block-year):\n")
print(b[, .N, keyby = cell_sd])

# ---------------------------------------------------------------------------
# 3. Build stacked block panel on muni cohorts
#    Each stack = one muni-cohort year c. Treated blocks = blocks whose muni
#    first changes in year c (treated_cell_muni = block's muni cell). NC
#    blocks (muni cell == NC) are replicated into every stack. Window
#    [c-K, c+K]. dose_stack = resident_eit - pre-event mean within
#    (stack, muni).
# ---------------------------------------------------------------------------
build_block_stack <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC" & !is.na(cell_muni),
                       .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  cat(sprintf("Stacks to build: %d cohorts; %s NC munis to replicate.\n",
              length(cohort_years), format(length(nc_munis), big.mark = ",")))
  build_one <- function(cy) {
    win <- (cy - K):(cy + K)
    t_munis <- treated[cohort == cy]
    if (nrow(t_munis) == 0) return(NULL)
    d_t <- p[muni %in% t_munis$muni & year %in% win]
    d_t[, treat_cell_muni := cell_muni]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell_muni := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]
    d
  }
  rbindlist(lapply(cohort_years, build_one), fill = TRUE)
}

cat("\nBuilding stacked block panel (this is large)...\n")
stk <- build_block_stack(b)
stk[, stack_block := paste0(stack, "_", block_geoid)]
stk[, stack_year  := paste0(stack, "_", year)]
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk[, cell_sd := factor(cell_sd, levels = cell_levels)]

# dose_stack: muni resident rate net of within-(stack, muni) pre-event mean.
# For NC munis the pre-event mean equals the rate itself, so dose_stack == 0;
# for treated munis it's the centered post-event dose. Matches cousub spec.
stk[, base_rate := mean(resident_eit[rel < 0], na.rm = TRUE),
    by = .(stack, muni)]
stk[!is.finite(base_rate), base_rate := 0]
stk[, dose_stack := resident_eit - base_rate]
stk[treat_cell_muni == "NC", dose_stack := 0]
stk[is.na(dose_sd), dose_sd := 0]

cat(sprintf("Stack: %s rows; treated blocks %s; NC blocks %s\n",
            format(nrow(stk), big.mark=","),
            format(uniqueN(stk[treat_cell_muni != "NC", block_geoid]), big.mark=","),
            format(uniqueN(stk[treat_cell_muni == "NC", block_geoid]), big.mark=",")))

# ---------------------------------------------------------------------------
# 4. Joint stacked TWFE per outcome
#    y ~ i(treat_cell_muni, dose_stack, ref='NC')
#       + i(cell_sd, dose_sd, ref='NC')
#       + muni_ctrls + sd_ctrls
#       | block_geoid^stack + year^stack
#    cluster = ~muni + sd_id
# ---------------------------------------------------------------------------
joint_results <- list()
diagnostics   <- list()
for (o in outcomes) {
  cols <- c("block_geoid","year","muni","sd_id","stack",
             "treat_cell_muni","dose_stack","cell_sd","dose_sd",
             o, ctrls)
  cols <- intersect(cols, names(stk))
  d <- stk[, ..cols]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) { cat(sprintf("[%s] no data\n", o)); next }
  cat(sprintf("\n[%s] N=%s ... ", o, format(nrow(d), big.mark=",")))
  f <- as.formula(paste0(o,
       " ~ i(treat_cell_muni, dose_stack, ref='NC')",
       " + i(cell_sd, dose_sd, ref='NC') + ",
       ctrls_str, " | block_geoid^stack + year^stack"))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("treat_cell_muni::|cell_sd::", term)]
  ic[, side := ifelse(grepl("treat_cell_muni::", term), "muni", "sd")]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  joint_results[[length(joint_results)+1L]] <- data.table(
    estimator = "block_joint_stacked", outcome = o,
    side = ic$side, cell = ic$cell,
    est = ic$Estimate, se = ic$`Std. Error`, p = ic$`Pr(>|t|)`,
    n = nobs(m),
    r2  = fitstat(m, "r2",  simplify = TRUE),
    wr2 = fitstat(m, "wr2", simplify = TRUE))
  dep_vec <- m$residuals + m$fitted.values
  diagnostics[[o]] <- data.table(
    outcome = o, n = nobs(m),
    R2 = round(fitstat(m, "r2",  simplify = TRUE), 4),
    Within_R2 = round(fitstat(m, "wr2", simplify = TRUE), 4),
    dep_mean = round(mean(dep_vec, na.rm = TRUE), 4),
    dep_sd   = round(sd(dep_vec,   na.rm = TRUE), 4),
    fe = "block_geoid^stack + year^stack",
    se_type = "cluster: muni + sd_id")
  cat("done.\n")
}
joint_dt <- rbindlist(joint_results, fill = TRUE)
fwrite(joint_dt, file.path(out_dir, "block_joint_results.csv"))
fwrite(rbindlist(diagnostics, fill = TRUE),
       file.path(out_dir, "block_joint_diagnostics.csv"))

cat("\n--- Joint block-level stacked fully-parameterized results ---\n")
print(joint_dt[, .(outcome, side, cell, est = round(est,3), se = round(se,3),
                    p = round(p,4), n = format(n, big.mark=","))])
cat("\nSaved data/q3_unified/block_joint_results.csv\n")
cat("Saved data/q3_unified/block_joint_diagnostics.csv\n")
