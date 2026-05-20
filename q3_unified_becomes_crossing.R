# q3_unified_becomes_crossing.R
# PRELIMINARY: measure the effect of pushing the home stack over the
# nonresident (work) floor -- the "becomes binding" margin -- and SEPARATE the
# muni-rate vs SD-rate contributions, grounded in FIRST movements.
#
# Mechanism (per discussion):
#   * The treatment event is the home stack's FIRST crossing of the work floor:
#     home_binds = (resident + SD > nonres) flips 0 -> 1 at year c*.
#   * Attribution is by who RAISED AT the crossing year c* (the first movement
#     that carried it over):
#       muni_becomes_dose = max(0, resident_{c*} - resident_{c*-1})
#       sd_becomes_dose   = max(0, sd_{c*}       - sd_{c*-1})
#     Both can be > 0 (muni and SD both helped push it over -- same pool).
#   * A rate that rises only LATER (stack already binding) is the "bind" group
#     and is NOT part of becomes; here it simply isn't loaded into either dose.
#
# Design: stacked DiD/ES centered on c* (crossers vs never-changer controls),
# with BOTH crossing-year doses entered as continuous treatments interacted
# with event time, so the muni- and SD-becomes effects are identified jointly:
#   y_{it} = a_{stack,muni} + g_{stack,year}
#          + sum_{k!=-1} bM_k * 1{rel=k} * muni_becomes_dose_i
#          + sum_{k!=-1} bS_k * 1{rel=k} * sd_becomes_dose_i
#          + X phi + eps
#
# Outputs:
#   data/q3_unified/becomes_crossing_es.csv   (lead-by-lead, both doses)
#   data/q3_unified/becomes_crossing_att.csv  (post/pre means + Wald, per dose)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
K_stack  <- 5L
EPS      <- 1e-6

# ---------------------------------------------------------------------------
# 1. Panel + controls from the headline prep
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for panel + controls...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)
ctrls <- e$ctrls
ctrls_str <- paste(ctrls, collapse = " + ")

# ---------------------------------------------------------------------------
# 2. First crossing of the home stack over the work floor + crossing-year doses
# ---------------------------------------------------------------------------
setorder(panel, muni, year)
panel[, home_binds := as.integer(home_stack > muni_nonres_eit)]
panel[, lag_binds  := shift(home_binds), by = muni]

# First crossing year c* per muni (home_binds 0 -> 1)
cross <- panel[home_binds == 1 & lag_binds == 0, .(c_star = min(year)), by = muni]

# Crossing-year jumps (first movements that carried the stack over)
cv <- merge(panel, cross, by = "muni")[ , {
  cs <- c_star[1L]
  r_pre  <- resident_eit[match(cs - 1L, year)];  r_post <- resident_eit[match(cs, year)]
  s_pre  <- sd_eit[match(cs - 1L, year)];        s_post <- sd_eit[match(cs, year)]
  dM <- r_post - r_pre; dS <- s_post - s_pre
  list(c_star = cs,
       muni_becomes_dose = ifelse(is.finite(dM) & dM > EPS, dM, 0),
       sd_becomes_dose   = ifelse(is.finite(dS) & dS > EPS, dS, 0))
}, by = muni]

# Universe tags for reporting
ever_binds  <- panel[, .(any_bind = any(home_binds == 1, na.rm = TRUE),
                         start_bind = home_binds[which.min(year)] == 1), by = muni]
# Control pool = TRUE never-changers (BOTH resident and SD rates flat). NOTE:
# cell_muni == "NC" only means the muni rate is flat -- it includes SD-becomes
# crossers (muni flat, SD moved), so it must NOT be used as the control pool.
rate_chg <- panel[, .(res_chg = uniqueN(round(resident_eit[!is.na(resident_eit)], 6)) > 1,
                      sd_chg  = uniqueN(round(sd_eit[!is.na(sd_eit)], 6)) > 1), by = muni]
nc_munis <- setdiff(rate_chg[!res_chg & !sd_chg, muni], cross$muni)
cv[, muni_becomes := muni_becomes_dose > EPS]
cv[, sd_becomes   := sd_becomes_dose   > EPS]

cat("\n=== Crossing universe (munis) ===\n")
cat(sprintf("  Crossers (home stack crosses the floor):      %d\n", nrow(cv)))
cat(sprintf("    muni raised at crossing (muni-becomes):      %d\n", sum(cv$muni_becomes)))
cat(sprintf("    SD raised at crossing  (SD-becomes):         %d\n", sum(cv$sd_becomes)))
cat(sprintf("    BOTH raised at crossing (shared pool):       %d\n", sum(cv$muni_becomes & cv$sd_becomes)))
cat(sprintf("    muni-only / SD-only:                          %d / %d\n",
            sum(cv$muni_becomes & !cv$sd_becomes), sum(!cv$muni_becomes & cv$sd_becomes)))
cat(sprintf("    crossing not from a rate rise (nonres drop):  %d\n",
            sum(!cv$muni_becomes & !cv$sd_becomes)))
cat(sprintf("  Always-binding (start above floor):            %d\n",
            uniqueN(ever_binds[start_bind == TRUE, muni])))
cat(sprintf("  Never-binding:                                  %d\n",
            uniqueN(ever_binds[any_bind == FALSE, muni])))
cat(sprintf("  Never-changer (NC) controls:                    %d\n", length(nc_munis)))

# Keep crossers whose crossing was driven by a rate rise (drop nonres-drop ones)
cv <- cv[muni_becomes | sd_becomes]

# ---------------------------------------------------------------------------
# 3. Stacked panel centered on the crossing year c* (crossers + NC controls)
# ---------------------------------------------------------------------------
panel[, muni_becomes_dose := NULL]; panel[, sd_becomes_dose := NULL]  # safety if rerun
panel <- merge(panel, cv[, .(muni, c_star, muni_becomes_dose, sd_becomes_dose)],
               by = "muni", all.x = TRUE)
panel[muni %in% nc_munis,
      `:=`(muni_becomes_dose = 0, sd_becomes_dose = 0)]

build_stack <- function(p, K = K_stack) {
  treated <- unique(p[!is.na(c_star), .(muni, cohort = c_star,
                                        md = muni_becomes_dose, sdd = sd_becomes_dose)])
  rbindlist(lapply(sort(unique(treated$cohort)), function(cy) {
    win <- (cy - K):(cy + K)
    tm <- treated[cohort == cy]
    d_t <- p[muni %in% tm$muni & year %in% win]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d <- rbind(d_t, d_c, fill = TRUE)
    if (nrow(d) == 0) return(NULL)
    d[, stack := cy]; d[, rel := as.integer(year - cy)]; d
  }), fill = TRUE)
}
stk <- build_stack(panel)
stk[is.na(muni_becomes_dose), muni_becomes_dose := 0]
stk[is.na(sd_becomes_dose),   sd_becomes_dose   := 0]
stk[, stack_muni := paste0(stack, "_", muni)]
stk[, stack_year := paste0(stack, "_", year)]
cat(sprintf("\nStack: %s rows; %s crosser munis; %s NC munis\n",
            format(nrow(stk), big.mark = ","),
            format(uniqueN(stk[muni_becomes_dose > 0 | sd_becomes_dose > 0, muni]), big.mark = ","),
            format(length(intersect(unique(stk$muni), nc_munis)), big.mark = ",")))

# ---------------------------------------------------------------------------
# 4. Joint realized-dose ES: muni-becomes and SD-becomes entered separately
# ---------------------------------------------------------------------------
outcomes <- c("log_live_and_work", "log_in_commuters")
es_list <- list(); att_list <- list()
for (o in outcomes) {
  cols <- c("muni","year","stack_muni","stack_year","rel",
            "muni_becomes_dose","sd_becomes_dose", o, ctrls)
  dd <- stk[, ..cols][complete.cases(stk[, ..cols])]
  if (nrow(dd) == 0) next
  f <- as.formula(paste0(o,
       " ~ i(rel, muni_becomes_dose, ref = -1) + i(rel, sd_becomes_dose, ref = -1) + ",
       ctrls_str, " | stack_muni + stack_year"))
  m <- feols(f, data = dd, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")[grepl("^rel::", term)]
  ct[, rel := as.integer(sub("rel::(-?\\d+):.*", "\\1", term))]
  ct[, side := fifelse(grepl("muni_becomes_dose", term), "muni-becomes",
                fifelse(grepl("sd_becomes_dose", term), "SD-becomes", NA_character_))]
  ic <- ct[, .(rel, side, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`,
               outcome = o, n = nobs(m))]
  es_list[[length(es_list)+1L]] <- ic
  for (sd_side in c("muni-becomes","SD-becomes")) {
    rgx <- if (sd_side == "muni-becomes") "^rel::-[2-9]:muni_becomes_dose"
           else "^rel::-[2-9]:sd_becomes_dose"
    wp <- wald_pretrend(m, rgx)
    sub <- ic[side == sd_side]
    att_list[[length(att_list)+1L]] <- data.table(
      outcome = o, side = sd_side,
      post_per_pp = round(mean(sub[rel >= 0 & rel <= 5, est], na.rm = TRUE), 3),
      pre_per_pp  = round(mean(sub[rel <= -2, est], na.rm = TRUE), 3),
      wald_pre_stat = round(wp$stat, 3), wald_pre_df = wp$df,
      wald_pre_p = round(wp$p, 4), n = nobs(m))
  }
}
es_dt  <- rbindlist(es_list, fill = TRUE)
att_dt <- rbindlist(att_list, fill = TRUE)
fwrite(es_dt,  file.path(out_dir, "becomes_crossing_es.csv"))
fwrite(att_dt, file.path(out_dir, "becomes_crossing_att.csv"))

cat("\n=== Becomes effect at the crossing margin (per pp of crossing-year hike) ===\n")
print(att_dt[order(outcome, side)])
cat("\nDone. Outputs: becomes_crossing_es.csv, becomes_crossing_att.csv\n")
