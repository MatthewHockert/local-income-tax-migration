# q3_unified_block_joint.R
# Fully-parameterized block-level unified model: both muni-rate cells and
# SD-rate cells enter the same regression. Single TWFE on the unstacked
# block panel (block FE + year FE, two-way cluster at (muni, sd_id)).
#
# Cell decomposition is computed independently for muni events and SD events.
# Dose for each side = current rate - pre-event mean of that rate (per
# muni / per SD), with NC = 0.

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K <- 5L

ctrls <- c("share_police","share_streets","share_debt_svc",
           "share_eit","share_re","share_intergov")
ctrls_str <- paste(ctrls, collapse = " + ")

outcomes <- c("log_home_count","log_work_count",
              "home_in_muni_share","share_work_outside")

cat("Loading block panel...\n")
b <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))

# ---------------------------------------------------------------------------
# Muni-side cohort + cell + dose
# ---------------------------------------------------------------------------
my_m <- unique(b[, .(muni, year, resident_eit, sd_eit, muni_nonres_eit)])
setorder(my_m, muni, year)
my_m[, res_lag := shift(resident_eit), by = muni]
my_m[, dose_muni := resident_eit - res_lag]
my_m[, has_event := !is.na(dose_muni) & abs(dose_muni) > 1e-6]
cohort_m <- my_m[has_event == TRUE,
                  .(c_muni = min(year), dir_muni = sign(dose_muni[which.min(year)])),
                  by = muni]
my_m <- merge(my_m, cohort_m, by = "muni", all.x = TRUE)
my_m[, home_stack := resident_eit + sd_eit]
cell_muni <- my_m[!is.na(c_muni), {
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
nc_munis <- setdiff(unique(my_m$muni), unique(cohort_m$muni))
cell_muni <- rbind(cell_muni, data.table(muni = nc_munis, cell_muni = "NC"))
cell_muni <- cell_muni[cell_muni != "Uncertain"]

# Pre-event mean resident_eit per muni (for dose centering)
mu_pre <- my_m[!is.na(c_muni) & year < c_muni,
                .(pre_mean_muni = mean(resident_eit, na.rm = TRUE)), by = muni]
mu_pre <- rbind(mu_pre, data.table(muni = nc_munis, pre_mean_muni = 0))

# ---------------------------------------------------------------------------
# SD-side cohort + cell + dose
# ---------------------------------------------------------------------------
my_s <- unique(b[!is.na(sd_id), .(sd_id, muni, year, resident_eit, sd_eit, muni_nonres_eit)])
sd_year <- unique(my_s[, .(sd_id, year, sd_eit)])
setorder(sd_year, sd_id, year)
sd_year[, sd_lag := shift(sd_eit), by = sd_id]
sd_year[, dose_sd := sd_eit - sd_lag]
sd_year[, has_event := !is.na(dose_sd) & abs(dose_sd) > 1e-6]
cohort_s <- sd_year[has_event == TRUE,
                     .(c_sd = min(year), dir_sd = sign(dose_sd[which.min(year)])),
                     by = sd_id]
my_s <- merge(my_s, cohort_s, by = "sd_id", all.x = TRUE)
my_s[, home_stack := resident_eit + sd_eit]
cell_sd <- my_s[!is.na(c_sd), {
  pre  <- .SD[year == c_sd - 1L]
  post <- .SD[year == c_sd]
  cell <- "Uncertain"
  if (nrow(pre) == 1 && nrow(post) == 1 &&
      !is.na(pre$home_stack) && !is.na(post$home_stack) &&
      !is.na(pre$muni_nonres_eit) && !is.na(post$muni_nonres_eit)) {
    pre_b  <- pre$home_stack  > pre$muni_nonres_eit
    post_b <- post$home_stack > post$muni_nonres_eit
    if (dir_sd[1] > 0) {
      cell <- if (post_b && pre_b) "Inc-bind"
              else if (post_b && !pre_b) "Inc-becomes"
              else "Inc-nobind"
    } else {
      cell <- if (post_b && pre_b) "Dec-bind"
              else if (!post_b && pre_b) "Dec-becomes-non"
              else "Dec-nobind"
    }
  }
  list(cell_sd = cell)
}, by = .(muni, sd_id)]
nc_sds <- setdiff(unique(b$sd_id), unique(cohort_s$sd_id))
cell_sd <- rbind(cell_sd,
                 unique(b[sd_id %in% nc_sds, .(muni, sd_id)])[, cell_sd := "NC"])
cell_sd <- cell_sd[cell_sd != "Uncertain"]

sd_pre <- my_s[!is.na(c_sd) & year < c_sd,
                .(pre_mean_sd = mean(sd_eit, na.rm = TRUE)), by = sd_id]
sd_pre <- rbind(sd_pre, data.table(sd_id = nc_sds, pre_mean_sd = 0))

# ---------------------------------------------------------------------------
# Attach to block panel and build dose variables
# ---------------------------------------------------------------------------
b <- merge(b, cell_muni, by = "muni", all.x = TRUE)
b <- merge(b, cell_sd,    by = c("muni","sd_id"), all.x = TRUE)
b <- merge(b, mu_pre,     by = "muni", all.x = TRUE)
b <- merge(b, sd_pre,     by = "sd_id", all.x = TRUE)
b <- b[!is.na(cell_muni) & !is.na(cell_sd)]

# Doses (zero for NC by construction since pre_mean = 0 and rate = 0 wouldn't apply,
# so use cell == NC -> dose = 0)
b[, dose_muni := ifelse(cell_muni == "NC", 0, resident_eit - pre_mean_muni)]
b[, dose_sd   := ifelse(cell_sd   == "NC", 0, sd_eit       - pre_mean_sd)]

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
b[, cell_muni := factor(cell_muni, levels = cell_levels)]
b[, cell_sd   := factor(cell_sd,   levels = cell_levels)]

cat("\nMuni cell distribution (block-year):\n"); print(b[, .N, keyby = cell_muni])
cat("\nSD cell distribution (block-year):\n");   print(b[, .N, keyby = cell_sd])

# ---------------------------------------------------------------------------
# Joint TWFE: y ~ i(cell_muni, dose_muni, ref='NC') + i(cell_sd, dose_sd, ref='NC')
#                + ctrls | block_geoid + year, cluster = ~muni + sd_id
# ---------------------------------------------------------------------------
joint_results <- list()
for (o in outcomes) {
  d <- b[, c("block_geoid","year","muni","sd_id","cell_muni","cell_sd",
              "dose_muni","dose_sd", o, ctrls), with = FALSE]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  cat(sprintf("\n[%s] N=%s ... ", o, format(nrow(d), big.mark=",")))
  f <- as.formula(paste0(o,
       " ~ i(cell_muni, dose_muni, ref='NC') + i(cell_sd, dose_sd, ref='NC') + ",
       ctrls_str, " | block_geoid + year"))
  m <- feols(f, data = d, cluster = ~muni + sd_id)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  ic <- ct[grepl("cell_muni::|cell_sd::", term)]
  ic[, side := ifelse(grepl("cell_muni::", term), "muni", "sd")]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  joint_results[[length(joint_results)+1L]] <- data.table(
    estimator = "block_joint_twfe", outcome = o,
    side = ic$side, cell = ic$cell,
    est = ic$Estimate, se = ic$`Std. Error`, p = ic$`Pr(>|t|)`,
    n = nobs(m),
    r2 = fitstat(m, "r2", simplify = TRUE),
    wr2 = fitstat(m, "wr2", simplify = TRUE))
  cat("done.\n")
}
joint_dt <- rbindlist(joint_results, fill = TRUE)
fwrite(joint_dt, file.path(out_dir, "block_joint_results.csv"))

cat("\n--- Joint block-level fully-parameterized results ---\n")
print(joint_dt[, .(outcome, side, cell, est = round(est,3), se = round(se,3),
                    p = round(p,4), n = format(n, big.mark=","))])
cat("\nSaved data/q3_unified/block_joint_results.csv\n")
