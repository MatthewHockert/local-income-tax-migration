# q3_unified_becomes_marginal.R
# Diagnostic re-run of the cousub muni-side decomposition + realized-dose
# pre-trend ES under a REDEFINED binding-state classification.
#
# Motivation: under the original classification (q3_unified_full_table.R) the
# Inc-becomes cell fails the realized-dose pre-trend test. The original
# prebind/postbind uses fully-lagged rates around the event, so a muni is
# tagged "becomes" whenever the home stack crosses the work floor near the
# event -- even if a simultaneous SD or work-rate move (not the muni's own
# resident hike) is what carried it over. That contamination is a candidate
# explanation for the dirty pre-trend.
#
# Redefinition (counterfactual-marginal): each rate-setter's cell reflects what
# ITS OWN increase does to the worker's bill, holding the other home-stack
# component and the work rate fixed at the event year:
#   prebind*  = (own_{c-1} + other_c  > work_c)     [would it bind w/o my hike]
#   postbind  = (own_c     + other_c  > work_c)      [actual bind at event]
#   becomes  iff prebind* = 0 and postbind = 1       (my hike carries it over)
#   bind     iff prebind* = 1 and postbind = 1       (already over; I add on)
#   nobind   iff postbind = 0                         (stays under; placebo)
# Muni side: own = resident_eit, other = sd_eit. SD side: own = sd_eit,
# other = resident_eit. Work floor = muni_nonres_eit in both. Only the cell
# LABEL changes; event years (c_*) and doses (dose_*) are untouched.
#
# SD edge case: if the muni's resident hike already pushed the stack over the
# floor, a later SD hike adds to an already-binding bill -> SD-bind (default),
# because the SD change still moves the bill. Set SD_ALREADYBIND_AS_NOBIND = TRUE
# to instead label those as SD-nobind (folds them into the SD placebo).
#
# Outputs:
#   data/q3_unified/becomes_marginal_decomp.csv  (static cell coefficients)
#   data/q3_unified/becomes_marginal_es.csv      (ES leads + Wald pre-trend)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack  <- 5L

SD_ALREADYBIND_AS_NOBIND <- FALSE   # see header; default keeps SD-bind

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")

# ---------------------------------------------------------------------------
# 1. Pull the classified panel + estimation helpers from the headline script
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for panel + helpers...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel0 <- as.data.table(e$panel)   # original (classified) panel
ctrls  <- e$ctrls

# ---------------------------------------------------------------------------
# 2. Recompute cell labels with the counterfactual-marginal rule
# ---------------------------------------------------------------------------
classify_marginal <- function(dir, prebind, postbind, alreadybind_as_nobind = FALSE) {
  out <- fcase(
    dir == "Inc" & prebind == 1 & postbind == 1, "Inc-bind",
    dir == "Inc" & prebind == 0 & postbind == 1, "Inc-becomes",
    dir == "Inc" & prebind == 0 & postbind == 0, "Inc-nobind",
    dir == "Inc" & prebind == 1 & postbind == 0, "Inc-loses",
    dir == "Dec" & prebind == 1 & postbind == 1, "Dec-bind",
    dir == "Dec" & prebind == 1 & postbind == 0, "Dec-becomes-non",
    dir == "Dec" & prebind == 0 & postbind == 0, "Dec-nobind",
    dir == "Dec" & prebind == 0 & postbind == 1, "Dec-gains",
    default = NA_character_)
  if (alreadybind_as_nobind)
    out[dir == "Inc" & prebind == 1 & postbind == 1] <- "Inc-nobind"
  out
}

# Build per-muni event-year values and the new label for one side.
remarginal <- function(panel, own_col, other_col, c_col, cell_col,
                       alreadybind_as_nobind = FALSE) {
  ev <- panel[!is.na(get(c_col)), {
    cc <- get(c_col)[1L]
    list(c        = cc,
         own_pre  = get(own_col)[match(cc - 1L, year)],
         own_post = get(own_col)[match(cc, year)],
         other    = get(other_col)[match(cc, year)],
         work     = muni_nonres_eit[match(cc, year)],
         old_cell = as.character(get(cell_col)[1L]))
  }, by = muni]
  ev[, dir := fifelse(grepl("^Inc", old_cell), "Inc",
              fifelse(grepl("^Dec", old_cell), "Dec", NA_character_))]
  ev[, prebind  := as.integer(own_pre  + other > work)]
  ev[, postbind := as.integer(own_post + other > work)]
  ev[, new_cell := classify_marginal(dir, prebind, postbind, alreadybind_as_nobind)]
  # If rates missing (NA prebind/postbind) or non Inc/Dec, keep the old label.
  ev[is.na(new_cell), new_cell := old_cell]
  ev[]
}

panel <- copy(panel0)
ev_muni <- remarginal(panel, "resident_eit", "sd_eit", "c_muni", "cell_muni",
                      alreadybind_as_nobind = FALSE)
ev_sd   <- remarginal(panel, "sd_eit", "resident_eit", "c_sd", "cell_sd",
                      alreadybind_as_nobind = SD_ALREADYBIND_AS_NOBIND)

panel <- merge(panel, ev_muni[, .(muni, nm = new_cell)], by = "muni", all.x = TRUE)
panel[is.na(nm), nm := as.character(cell_muni)]
panel[, cell_muni := factor(nm, levels = cell_levels)]; panel[, nm := NULL]
panel <- merge(panel, ev_sd[, .(muni, ns = new_cell)], by = "muni", all.x = TRUE)
panel[is.na(ns), ns := as.character(cell_sd)]
panel[, cell_sd := factor(ns, levels = cell_levels)]; panel[, ns := NULL]

# ---------------------------------------------------------------------------
# 3. Old -> new cell migration (muni and SD side)
# ---------------------------------------------------------------------------
old_muni <- unique(panel0[, .(muni, old = as.character(cell_muni))])
new_muni <- unique(panel[,  .(muni, new = as.character(cell_muni))])
mig_muni <- merge(old_muni, new_muni, by = "muni")
cat("\n=== Muni-side cell counts (munis): old -> new ===\n")
print(dcast(mig_muni[, .N, by = .(old, new)], old ~ new, value.var = "N", fill = 0))
cat("\nMuni-side switchers (old != new):\n")
print(mig_muni[old != new, .N, by = .(old, new)][order(-N)])

old_sd <- unique(panel0[, .(muni, old = as.character(cell_sd))])
new_sd <- unique(panel[,  .(muni, new = as.character(cell_sd))])
mig_sd <- merge(old_sd, new_sd, by = "muni")
cat("\n=== SD-side cell counts (munis): old -> new ===\n")
print(dcast(mig_sd[, .N, by = .(old, new)], old ~ new, value.var = "N", fill = 0))

# ---------------------------------------------------------------------------
# 4. Static decomposition under new labels (reuse headline build_stack/run)
# ---------------------------------------------------------------------------
key_outcomes <- c("log_live_and_work", "log_in_commuters")
cat("\n=== Static decomposition (new labels) ===\n")
stk_muni <- e$build_stack(panel, "muni", "dose_muni")
decomp <- rbindlist(lapply(key_outcomes, function(o) {
  m <- e$run_headline(stk_muni, o)
  if (is.null(m)) return(NULL)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"), c("est","se","p"), skip_absent = TRUE)
  ct <- ct[grepl("^treat_cell::", term)]
  ct[, cell := sub("treat_cell::([^:]+):.*", "\\1", term)]
  ct[, outcome := o]; ct[, n := nobs(m)]
  ct[, .(outcome, cell, est = round(est,3), se = round(se,3), p = round(p,4), n)]
}), fill = TRUE)
print(decomp)
fwrite(decomp, file.path(out_dir, "becomes_marginal_decomp.csv"))

# ---------------------------------------------------------------------------
# 5. Realized-dose ES + Wald pre-trend under new labels (mirror headline_es)
# ---------------------------------------------------------------------------
# Realized muni dose: cumulative Delta tau between c_muni-1 and c_muni+5
mu_year <- unique(panel[, .(muni, year, resident_eit, c_muni)])
setorder(mu_year, muni, year)
rd <- mu_year[!is.na(c_muni), {
  pre <- resident_eit[year == c_muni - 1L]
  yr_max <- min(c_muni + K_stack, max(year, na.rm = TRUE))
  end <- resident_eit[year == yr_max]
  list(realized_dose_muni = (if (length(end)==1) end else NA_real_) -
                            (if (length(pre)==1) pre else NA_real_))
}, by = muni]
rd[is.na(realized_dose_muni), realized_dose_muni := 0]
rd <- rbind(rd, data.table(muni = unique(panel[cell_muni=="NC", muni]),
                           realized_dose_muni = 0))
panel <- merge(panel, rd, by = "muni", all.x = TRUE)
panel[is.na(realized_dose_muni), realized_dose_muni := 0]

build_stack_es <- function(p, K = K_stack) {
  treated <- unique(p[cell_muni != "NC", .(muni, cohort = c_muni, cell = cell_muni)])
  nc_munis <- unique(p[cell_muni == "NC", muni])
  rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy - K):(cy + K)
    t_m <- treated[cohort == cy]
    if (nrow(t_m) == 0) return(NULL)
    d_t <- p[muni %in% t_m$muni & year %in% win]; d_t[, treat_cell_muni := cell_muni]
    d_c <- p[muni %in% nc_munis & year %in% win];  d_c[, treat_cell_muni := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
}
stk <- build_stack_es(panel)
stk[, stack_muni := paste0(stack, "_", muni)]
stk[, stack_year := paste0(stack, "_", year)]
stk[, treat_cell_muni := factor(treat_cell_muni, levels = cell_levels)]
stk[, cell_sd := factor(as.character(cell_sd), levels = cell_levels)]
stk[, cell_nr := factor(as.character(cell_nr), levels = cell_levels)]
stk[is.na(dose_sd), dose_sd := 0]; stk[is.na(dose_nr), dose_nr := 0]
ctrls_str <- paste(ctrls, collapse = " + ")

es_results <- list(); att_results <- list()
for (o in key_outcomes) {
  for (cl in c("Inc-bind","Inc-becomes","Inc-nobind")) {
    cols <- c("muni","year","stack_muni","stack_year","treat_cell_muni",
              "realized_dose_muni","rel","cell_sd","dose_sd","cell_nr","dose_nr", o, ctrls)
    dd <- stk[treat_cell_muni %in% c("NC", cl), ..cols]
    dd <- dd[complete.cases(dd)]
    if (nrow(dd) == 0 || sum(dd$treat_cell_muni == cl) == 0) next
    f <- as.formula(paste0(o,
         " ~ i(rel, realized_dose_muni, ref = -1) + i(cell_sd, dose_sd, ref = 'NC') ",
         "+ i(cell_nr, dose_nr, ref = 'NC') + ", ctrls_str,
         " | stack_muni + stack_year"))
    m <- tryCatch(feols(f, data = dd, cluster = ~muni), error = function(ee) NULL)
    if (is.null(m)) next
    ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
    if (nrow(ct) == 0) next
    ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
    ic <- ct[, .(rel, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`,
                 outcome = o, cell = cl, n = nobs(m))]
    es_results[[length(es_results)+1L]] <- ic
    wp <- wald_pretrend(m, "^rel::-[2-9]")
    n_munis <- uniqueN(dd[treat_cell_muni == cl, muni])
    att_results[[length(att_results)+1L]] <- data.table(
      outcome = o, cell = cl, n_munis = n_munis,
      post_per_pp = round(mean(ic[rel >= 0 & rel <= 5, est], na.rm = TRUE), 3),
      pre_per_pp  = round(mean(ic[rel <= -2, est], na.rm = TRUE), 3),
      wald_pre_stat = round(wp$stat, 3), wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4))
  }
}
es_dt  <- rbindlist(es_results, fill = TRUE)
att_dt <- rbindlist(att_results, fill = TRUE)
fwrite(es_dt, file.path(out_dir, "becomes_marginal_es.csv"))

# ---------------------------------------------------------------------------
# 6. New vs old pre-trend comparison
# ---------------------------------------------------------------------------
old_att_path <- file.path(out_dir, "headline_es_att.csv")
cat("\n=== Realized-dose ES, NEW (marginal) labels ===\n")
print(att_dt)
if (file.exists(old_att_path)) {
  old_att <- fread(old_att_path)[outcome %in% key_outcomes &
                                 cell %in% c("Inc-bind","Inc-becomes","Inc-nobind"),
                                 .(outcome, cell, old_post = att_post_per_pp,
                                   old_pre = att_pre_per_pp, old_wald_p = wald_pre_p)]
  cmp <- merge(att_dt[, .(outcome, cell, new_post = post_per_pp,
                          new_pre = pre_per_pp, new_wald_p = wald_pre_p, n_munis)],
               old_att, by = c("outcome","cell"), all.x = TRUE)
  cat("\n=== OLD vs NEW (realized-dose ES pre-trend) ===\n")
  print(cmp[order(outcome, cell)])
}
cat("\nDone. Outputs: becomes_marginal_decomp.csv, becomes_marginal_es.csv\n")
