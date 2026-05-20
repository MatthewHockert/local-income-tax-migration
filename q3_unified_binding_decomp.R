# q3_unified_binding_decomp.R
# Causal effects under the binding decomposition, run symmetrically for BOTH
# rate-setters (muni resident rate and SD rate), so we can compare ATTs and
# pre-trends cell-by-cell across the two sides.
#
# Binding rule (per the agreed interpretation): each rate-setter is classified
# by its FIRST rate increase and where the TOTAL home stack (resident + SD)
# sits relative to the nonresident floor at that event:
#   becomes = stack below floor before my first increase, above after (I cross it)
#   bind    = stack already above floor before my first increase (someone else
#             -- the other rate-setter or an earlier event -- crossed it; I add on)
#   nobind  = stack stays below the floor after my increase (placebo)
# Because the stack is resident+SD, ordering matters: if the SD crosses first,
# a muni whose first increase comes later lands on an already-binding stack and
# is recategorized bind (and vice versa). This is exactly what classify_treatment
# in q3_unified_full_table.R computes (prebind uses the total home stack at the
# year before each side's first event), so we reuse those cell labels.
#
# Models (mirroring the paper, at the cousub level):
#   Static ATT  (per side): stacked continuous DiD on that side's cohorts,
#     y ~ i(treat_cell, dose_stack) + i(other_cell, other_dose) + X | stack_muni^ + stack_year^
#   Realized-dose ES (per side): pre-trend test,
#     y ~ i(rel, realized_dose) + i(other_cell, other_dose) + i(cell_nr, dose_nr) + X | FE
#
# Output:
#   data/q3_unified/binding_decomp_compare.csv  (side x cell x outcome: ATT + Wald pre-trend)
#   data/q3_unified/binding_decomp_es.csv       (lead-by-lead ES, both sides)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack  <- 5L
outcomes <- c("log_live_and_work", "log_in_commuters")
cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind","Inc-loses","Dec-gains")
inc_cells <- c("Inc-bind","Inc-becomes","Inc-nobind")

# ---------------------------------------------------------------------------
# Panel + helpers from the headline prep (cell_muni, cell_sd already classified)
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for classified panel + build_stack...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel0 <- as.data.table(e$panel)
ctrls  <- e$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

cat("\nCell counts (munis) by side:\n")
cat("  muni:\n"); print(unique(panel0[, .(muni, cell_muni)])[, .N, by = cell_muni][order(cell_muni)])
cat("  SD:\n");   print(unique(panel0[, .(muni, cell_sd)])[, .N, by = cell_sd][order(cell_sd)])

# ---------------------------------------------------------------------------
# 1. Static ATT per side (stacked continuous DiD; paper's headline spec)
# ---------------------------------------------------------------------------
run_static <- function(side, ctrl_side) {
  dose_col  <- paste0("dose_", side)
  ctrl_cell <- paste0("cell_", ctrl_side)
  ctrl_dose <- paste0("dose_", ctrl_side)
  stk <- e$build_stack(panel0, side, dose_col)          # treat_cell = cell_<side>
  rbindlist(lapply(outcomes, function(o) {
    cols <- c("muni","year","treat_cell","dose_stack", ctrl_cell, ctrl_dose,
              "stack_muni","stack_year", o, ctrls)
    d <- stk[, ..cols]
    d[is.na(get(ctrl_dose)), (ctrl_dose) := 0]
    d <- d[complete.cases(d)]
    f <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + i(",
                           ctrl_cell, ", ", ctrl_dose, ", ref='NC') + ", ctrls_str,
                           " | stack_muni + stack_year"))
    m <- feols(f, data = d, cluster = ~muni)
    ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^treat_cell::", term)]
    ct[, cell := sub("treat_cell::([^:]+):.*", "\\1", term)]
    ct[cell %in% inc_cells,
       .(side, outcome = o, cell, att = round(Estimate, 3),
         att_se = round(`Std. Error`, 3), att_p = round(`Pr(>|t|)`, 4))]
  }), fill = TRUE)
}

# ---------------------------------------------------------------------------
# 2. Realized-dose ES per side (pre-trend test; paper's headline ES spec)
# ---------------------------------------------------------------------------
run_es <- function(side, ctrl_side) {
  rate_col  <- if (side == "muni") "resident_eit" else "sd_eit"
  c_col     <- paste0("c_", side)
  cell_col  <- paste0("cell_", side)
  ctrl_cell <- paste0("cell_", ctrl_side)
  ctrl_dose <- paste0("dose_", ctrl_side)
  p <- copy(panel0)

  # Realized dose for this side: cumulative change c-1 -> min(c+K, last year)
  uy <- unique(p[, c("muni","year",rate_col,c_col), with = FALSE]); setorder(uy, muni, year)
  rd <- uy[!is.na(get(c_col)), {
    cc <- get(c_col)[1L]
    pre <- get(rate_col)[year == cc - 1L]
    yend <- min(cc + K_stack, max(year, na.rm = TRUE)); endv <- get(rate_col)[year == yend]
    list(realized_dose = (if (length(endv)==1) endv else NA_real_) -
                         (if (length(pre)==1) pre else NA_real_))
  }, by = muni]
  rd[is.na(realized_dose), realized_dose := 0]
  ncm <- unique(p[get(cell_col) == "NC", muni])
  rd <- rbind(rd, data.table(muni = ncm, realized_dose = 0))
  p <- merge(p, unique(rd, by = "muni"), by = "muni", all.x = TRUE)
  p[is.na(realized_dose), realized_dose := 0]

  # Stack on this side's cohorts vs that side's NC pool
  treated <- unique(p[get(cell_col) != "NC" & !is.na(get(c_col)),
                      .(muni, cohort = get(c_col), cell = get(cell_col))])
  stk <- rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy - K_stack):(cy + K_stack)
    tm <- treated[cohort == cy]
    d_t <- p[muni %in% tm$muni & year %in% win]; d_t[, tcell := get(cell_col)]
    d_c <- p[muni %in% ncm & year %in% win];     d_c[, tcell := "NC"]
    d <- rbind(d_t, d_c, fill = TRUE); if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
  stk[, stack_muni := paste0(stack, "_", muni)]
  stk[, stack_year := paste0(stack, "_", year)]
  stk[, tcell := factor(tcell, levels = cell_levels)]
  stk[, (ctrl_cell) := factor(as.character(get(ctrl_cell)), levels = cell_levels)]
  stk[, cell_nr := factor(as.character(cell_nr), levels = cell_levels)]
  stk[is.na(get(ctrl_dose)), (ctrl_dose) := 0]; stk[is.na(dose_nr), dose_nr := 0]

  es <- list(); att <- list()
  for (o in outcomes) {
    for (cl in inc_cells) {
      cols <- c("muni","year","stack_muni","stack_year","tcell","realized_dose","rel",
                ctrl_cell, ctrl_dose, "cell_nr","dose_nr", o, ctrls)
      dd <- stk[tcell %in% c("NC", cl), ..cols]; dd <- dd[complete.cases(dd)]
      if (nrow(dd) == 0 || sum(dd$tcell == cl) == 0) next
      f <- as.formula(paste0(o, " ~ i(rel, realized_dose, ref=-1) + i(",
            ctrl_cell, ", ", ctrl_dose, ", ref='NC') + i(cell_nr, dose_nr, ref='NC') + ",
            ctrls_str, " | stack_muni + stack_year"))
      m <- tryCatch(feols(f, data = dd, cluster = ~muni), error = function(ee) NULL)
      if (is.null(m)) next
      ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
      if (nrow(ct) == 0) next
      ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
      ic <- ct[, .(side, outcome = o, cell = cl, rel,
                   est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)]
      es[[length(es)+1L]] <- ic
      wp <- wald_pretrend(m, "^rel::-[2-9]")
      att[[length(att)+1L]] <- data.table(
        side = side, outcome = o, cell = cl,
        n_munis = uniqueN(dd[tcell == cl, muni]),
        es_post = round(mean(ic[rel >= 0 & rel <= 5, est], na.rm = TRUE), 3),
        es_pre  = round(mean(ic[rel <= -2, est], na.rm = TRUE), 3),
        wald_pre_p = round(wp$p, 4))
    }
  }
  list(es = rbindlist(es, fill = TRUE), att = rbindlist(att, fill = TRUE))
}

# ---------------------------------------------------------------------------
# Run both sides and assemble the comparison
# ---------------------------------------------------------------------------
cat("\nRunning static ATTs (muni side, SD side)...\n")
static <- rbind(run_static("muni", "sd"), run_static("sd", "muni"))

cat("Running realized-dose ES (muni side, SD side)...\n")
es_muni <- run_es("muni", "sd")
es_sd   <- run_es("sd", "muni")
es_all  <- rbind(es_muni$es, es_sd$es, fill = TRUE)
att_all <- rbind(es_muni$att, es_sd$att, fill = TRUE)
fwrite(es_all, file.path(out_dir, "binding_decomp_es.csv"))

compare <- merge(static, att_all, by = c("side","outcome","cell"), all = TRUE)
compare[, side := factor(side, levels = c("muni","sd"))]
compare[, cell := factor(cell, levels = inc_cells)]
setorder(compare, outcome, side, cell)
setcolorder(compare, c("outcome","side","cell","n_munis","att","att_se","att_p",
                       "es_post","es_pre","wald_pre_p"))
fwrite(compare, file.path(out_dir, "binding_decomp_compare.csv"))

cat("\n=== Binding decomposition: ATTs and pre-trends by side x cell ===\n")
cat("(att = static per-pp coefficient; es_post/es_pre = realized-dose ES means;\n")
cat(" wald_pre_p = joint pre-trend test, small = fails parallel trends)\n\n")
print(compare)
cat("\nDone. Outputs: binding_decomp_compare.csv, binding_decomp_es.csv\n")
