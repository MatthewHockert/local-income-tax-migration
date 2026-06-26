# q3_proptax_layer_split.R
# ---------------------------------------------------------------------------
# Decompose the pooled property-tax treatment (total statutory mills =
# muni + SD + county) into its THREE layers and run them as SEPARATE
# continuous treatments in one regression -- the property-tax analog of the
# EIT design's muni/SD separation. Answers: which layer (city / school /
# county) carries the (near-null) property-tax sorting response?
#
# Construction mirrors q3_proptax_es.R exactly, applied per layer:
#   d_disc_L = mills_L - lag_mills_L            (non-reassessment year)
#            = mills_L - lag_mills_L*(lag_clr/clr) (reassessment: dev. from
#                                                   revenue-neutral)
#   realized_dose_L = CLR_{c-1} * (cum_disc_L_{c+5} - cum_disc_L_{c-1}) / 1000
# Same cohorts (c_prop = largest TOTAL discretionary increase) and same
# stacked sample as the pooled ES, so the three layer doses sum exactly to
# the pooled realized_dose_P and the three coefficients decompose the pooled
# dose-response.
#
# Spec (clean margins): y ~ d_muni:post + d_sd:post + d_cty:post + eit_dose
#                            + X | muni + year,  cluster ~muni
# Output: data/q3_unified/proptax_layer_split.csv
# ---------------------------------------------------------------------------

suppressMessages({ library(data.table); library(fixest) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data"); mt_dir <- file.path(data_dir, "multitax")
out_dir  <- file.path(data_dir, "q3_unified")
K_stack  <- 5L

# --- reuse the pooled property-tax stacked sample --------------------------
e2 <- new.env(); sys.source(file.path(project_dir, "q3_proptax_es.R"), envir = e2)
stk   <- as.data.table(e2$stk)
ctrls <- e2$ctrls; ctrls_str <- paste(ctrls, collapse = " + ")

# cohort (first/largest-disc event year) per muni, from the pooled sample
coh <- unique(stk[, .(muni, c_prop)])[!is.na(c_prop)]

# --- rebuild per-LAYER discretionary millage doses -------------------------
burden <- fread(file.path(mt_dir, "pa_property_burden_panel.csv"))
mp <- fread(file.path(mt_dir, "pa_muni_multitax_panel.csv"))[, .(muni_id, year, cousub_fips)]
mp[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
burden <- merge(burden, mp, by = c("muni_id", "year"), all.x = TRUE)
layers <- c(muni = "muni_re_total_mills", sd = "sd_re_total_mills", cty = "cty_re_total_mills")
burden <- burden[!is.na(cousub_fips) & !is.na(clr) &
                   !is.na(muni_re_total_mills) & !is.na(sd_re_total_mills) &
                   !is.na(cty_re_total_mills)]
bt <- burden[, c(.(clr = mean(clr, na.rm = TRUE), county = first(county)),
                 lapply(.SD, function(x) mean(x, na.rm = TRUE))),
             by = .(muni = cousub_fips, year), .SDcols = unname(layers)]
# mills_<layer> column names (avoid clash with the cousub id column `muni`)
setnames(bt, unname(layers), paste0("mills_", names(layers)))
setorder(bt, muni, year)

reass <- fread(file.path(mt_dir, "pa_reassessment_from_clr.csv"))
reass[, county := toupper(trimws(county))]
bt[, county := toupper(trimws(county))]
bt[, reassess := paste(county, year) %in% paste(reass$county, reass$reassessment_year)]
bt[, lag_clr := shift(clr), by = muni]
bt[, consec  := year == shift(year) + 1L, by = muni]

# per-layer discretionary increment + cumulative
for (L in names(layers)) {
  m_col <- paste0("mills_", L)
  lag_col <- paste0("lag_", L); d_col <- paste0("d_", L); cum_col <- paste0("cum_", L)
  bt[, (lag_col) := shift(get(m_col)), by = muni]
  bt[, (d_col) := fifelse(!consec | is.na(get(lag_col)), NA_real_,
                  fifelse(reassess,
                          get(m_col) - get(lag_col) * (lag_clr / clr),
                          get(m_col) - get(lag_col)))]
  bt[is.na(get(d_col)), (d_col) := 0]
  bt[, (cum_col) := cumsum(get(d_col)), by = muni]
}

# realized per-layer dose at the muni's TOTAL-event cohort c_prop
bt <- merge(bt, coh, by = "muni")
rd <- bt[, {
  base_clr <- clr[year == c_prop[1] - 1L]
  bc <- if (length(base_clr) == 1) base_clr else NA_real_
  yr_max <- min(c_prop[1] + K_stack, max(year))
  out <- list()
  for (L in names(layers)) {
    cb <- get(paste0("cum_", L))[year == c_prop[1] - 1L]
    ce <- get(paste0("cum_", L))[year == yr_max]
    cb <- if (length(cb) == 1) cb else NA_real_
    ce <- if (length(ce) == 1) ce else NA_real_
    out[[paste0("rdose_", L)]] <- bc * (ce - cb) / 1000
  }
  out
}, by = muni]
for (L in names(layers)) rd[is.na(get(paste0("rdose_", L))), (paste0("rdose_", L)) := 0]

stk <- merge(stk, rd, by = "muni", all.x = TRUE)
for (L in names(layers)) stk[is.na(get(paste0("rdose_", L))), (paste0("rdose_", L)) := 0]

# --- sanity: layer doses sum to the pooled total dose ----------------------
chk <- unique(stk[, .(muni, realized_dose_P, rdose_muni, rdose_sd, rdose_cty)])
chk[, layer_sum := rdose_muni + rdose_sd + rdose_cty]
cat(sprintf("\nLayer-sum vs pooled dose: max|diff| = %.2e, corr = %.4f\n",
            max(abs(chk$layer_sum - chk$realized_dose_P)),
            cor(chk$layer_sum, chk$realized_dose_P)))
cat(sprintf("Median realized dose by layer (pp of mkt value): muni %.4f  sd %.4f  cty %.4f\n",
            median(chk$rdose_muni), median(chk$rdose_sd), median(chk$rdose_cty)))
cat(sprintf("Share of munis with non-zero layer dose: muni %.0f%%  sd %.0f%%  cty %.0f%%\n",
            100*mean(chk$rdose_muni != 0), 100*mean(chk$rdose_sd != 0),
            100*mean(chk$rdose_cty != 0)))

# --- joint 3-layer dose-response (post interaction) ------------------------
stk[, post := as.integer(rel >= 0)]
for (L in names(layers)) stk[, (paste0("dp_", L)) := get(paste0("rdose_", L)) * post]
stk[, dose_post := realized_dose_P * post]   # pooled, for reference

clean_margins <- c("share_live_in", "log_total_jobs", "log_in_commuters")
residence_cts <- c("log_live_and_work", "log_total_residents")
rhs_layers <- "dp_muni + dp_sd + dp_cty"

rows <- list()
for (o in c(clean_margins, residence_cts)) {
  cols <- c("muni","year","dp_muni","dp_sd","dp_cty","dose_post","eit_dose", o, ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (!nrow(dd)) next
  # pooled (reference)
  mp0 <- feols(as.formula(paste0(o, " ~ dose_post + eit_dose + ", ctrls_str,
                                 " | muni + year")), dd, cluster = ~muni)
  cp  <- as.data.table(mp0$coeftable, keep.rownames="term")[term=="dose_post"]
  # layer-split
  ml <- feols(as.formula(paste0(o, " ~ ", rhs_layers, " + eit_dose + ", ctrls_str,
                                " | muni + year")), dd, cluster = ~muni)
  ct <- as.data.table(ml$coeftable, keep.rownames="term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"), skip_absent=TRUE)
  for (L in names(layers)) {
    r <- ct[term == paste0("dp_", L)]
    rows[[length(rows)+1L]] <- data.table(outcome=o, layer=L,
      est=r$est, se=r$se, p=r$p, n=nobs(ml),
      pooled_est=cp$Estimate, pooled_p=cp$`Pr(>|t|)`)
  }
  cat(sprintf("\n[%-18s] pooled=%+.4f (p=%.3f) | muni=%+.4f (p=%.2f)  sd=%+.4f (p=%.2f)  cty=%+.4f (p=%.2f)\n",
      o, cp$Estimate, cp$`Pr(>|t|)`,
      ct[term=="dp_muni",est], ct[term=="dp_muni",p],
      ct[term=="dp_sd",  est], ct[term=="dp_sd",  p],
      ct[term=="dp_cty", est], ct[term=="dp_cty", p]))
}
res <- rbindlist(rows, fill=TRUE)
fwrite(res, file.path(out_dir, "proptax_layer_split.csv"))
cat("\nSaved data/q3_unified/proptax_layer_split.csv\n")
