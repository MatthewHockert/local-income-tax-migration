# q3_unified_industry_sd.R
# SD-level industry decomposition. Aggregates cousub-level industry job
# counts (pa_industry_panel.rds) to school district via the muni-SD
# assignment, then runs the cell x dose specification at SD level.
#
# Block-level industry is not implemented here -- requires block-resolution
# LODES WAC files which are not currently in the data tree.

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified")
K_stack <- 5L

ind <- as.data.table(readRDS(file.path(data_dir, "pa_industry_panel.rds")))
# muni column in pa_industry_panel.rds is cousub_fips. Need muni->sd_id link.
eit <- fread(file.path(data_dir, "pa_eit_rate_panel.csv"))
muni_sd <- unique(eit[is_resident == 1 & !is.na(sd_id) & !is.na(sd_eit_rate),
                       .(municipality_id, sd_id = sd_id[which.max(sd_eit_rate)]),
                       by = .(municipality_id)])
# pa_industry has cousub_fips (5-digit) but muni is also 5-digit fips here; check
muni_sd_xw <- unique(eit[is_resident == 1 & !is.na(sd_id),
                          .(municipality_id, sd_id, sd_eit_rate)])
muni_sd_xw <- muni_sd_xw[, .(sd_id = sd_id[which.max(sd_eit_rate)]),
                           by = municipality_id]
# Map municipality_id -> cousub_fips (5-digit) via pa_rate_panel
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
muni_to_cousub <- unique(rate[!is.na(cousub_fips), .(municipality_id, cousub_fips)])
muni_sd_xw <- merge(muni_sd_xw, muni_to_cousub, by = "municipality_id", all.x = TRUE)
muni_sd_xw <- muni_sd_xw[!is.na(cousub_fips), .(muni = cousub_fips, sd_id)]

# Aggregate industry job counts to SD-year via muni->sd assignment
ind <- merge(ind, muni_sd_xw, by = "muni", all.x = TRUE)
ind <- ind[!is.na(sd_id)]
sd_ind <- ind[, .(jobs_total      = sum(jobs_total,      na.rm = TRUE),
                   jobs_consumer   = sum(jobs_consumer,   na.rm = TRUE),
                   jobs_tradeable  = sum(jobs_tradeable,  na.rm = TRUE),
                   jobs_health_edu = sum(jobs_health_edu, na.rm = TRUE),
                   jobs_public     = sum(jobs_public,     na.rm = TRUE),
                   jobs_other      = sum(jobs_other,      na.rm = TRUE)),
               by = .(sd_id, year)]
log_pos <- function(x) ifelse(x > 0, log(as.numeric(x)), NA_real_)
for (cc in c("jobs_total","jobs_consumer","jobs_tradeable",
              "jobs_health_edu","jobs_public","jobs_other")) {
  sd_ind[, paste0("log_", cc) := log_pos(get(cc))]
}
sd_ind[, share_consumer := ifelse(jobs_total > 0, jobs_consumer  / jobs_total, NA_real_)]
sd_ind[, share_tradeable := ifelse(jobs_total > 0, jobs_tradeable / jobs_total, NA_real_)]

cat(sprintf("SD industry panel: %d SD-years, %d SDs\n",
            nrow(sd_ind), uniqueN(sd_ind$sd_id)))

# Source SD-level data prep to get rate panel + cell classification + stacks
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e)
stk_sd <- as.data.table(e$stk_sd)
sd_share_ctrls <- e$sd_share_ctrls
ctrls_sd <- intersect(c("mean_muni_rate", sd_share_ctrls), names(stk_sd))
ctrls_str <- paste(ctrls_sd, collapse = " + ")

# Merge industry outcomes onto stacked panel
stk_sd <- merge(stk_sd, sd_ind, by = c("sd_id","year"), all.x = TRUE)
if (!"treat_cell" %in% names(stk_sd) && "cell_sd" %in% names(stk_sd)) {
  stk_sd[, treat_cell := cell_sd]
}
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")
stk_sd[, treat_cell := factor(treat_cell, levels = cell_levels)]

industry_outcomes <- c("log_jobs_total","log_jobs_consumer","log_jobs_tradeable",
                        "share_consumer","share_tradeable")
results <- list()
for (o in industry_outcomes) {
  cols <- intersect(c("sd_id","year","stack_sd","stack_year","treat_cell",
                       "dose_stack", o, ctrls_sd), names(stk_sd))
  d <- stk_sd[, ..cols]; d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                          ctrls_str, " | stack_sd + stack_year"))
  m <- tryCatch(feols(f, data = d, cluster = ~sd_id),
                 error = function(e) NULL)
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
           c("est","se","p"), skip_absent = TRUE)
  ic <- ct[grepl("treat_cell::", term)]
  ic[, cell := sub("^.*::([^:]+):.*$", "\\1", term)]
  ic[, outcome := o]; ic[, n := nobs(m)]
  results[[length(results)+1L]] <- ic[, .(outcome, cell, est, se, p, n)]
}
out <- rbindlist(results, fill = TRUE)
fwrite(out, file.path(out_dir, "industry_sd.csv"))
cat(sprintf("\nSaved data/q3_unified/industry_sd.csv (%d rows)\n", nrow(out)))
print(out[cell %in% c("Inc-bind","Inc-becomes","Inc-nobind"),
          .(outcome, cell, est = round(est,3), se = round(se,3), p = round(p,4))])
