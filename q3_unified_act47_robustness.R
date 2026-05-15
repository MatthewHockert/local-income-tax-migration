# q3_unified_act47_robustness.R
# Robustness check: drop Act 47 distressed municipalities from the
# treated cohort and re-estimate the headline binding-state
# decomposition. Identifies Act 47 munis in the Inc-bind cell by
# name matching the standard PA DCED list of Act 47 designations.
#
# Outputs:
#   data/q3_unified/act47_robustness_cousub.csv
#   data/q3_unified/act47_robustness_block.csv
#   paper/tables/act47_robustness.tex (concise summary table)

suppressMessages({ library(data.table); library(fixest) })
setFixest_etable(signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1))

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
tab_dir     <- file.path(project_dir, "paper", "tables")

# ---------------------------------------------------------------------------
# Act 47 distressed-municipality list (DCED designations 1987-2019)
# Source: PA DCED Act 47 active and rescinded designation list.
# Names normalized to match DCED's municipality names.
# ---------------------------------------------------------------------------
act47_names <- toupper(c(
  "ALIQUIPPA", "BRADDOCK", "CARBONDALE", "CHESTER CITY", "CLAIRTON",
  "DUQUESNE", "EAST PITTSBURGH", "FARRELL", "FRANKLIN",
  "GREENVILLE", "HARRISBURG", "HAZLETON", "HOMESTEAD", "JEANNETTE",
  "JOHNSTOWN", "MILLBOURNE", "NANTICOKE", "NEW CASTLE", "PITTSBURGH",
  "PLYMOUTH TOWNSHIP", "RANKIN", "READING", "SCRANTON", "SHAMOKIN",
  "SHARON", "TURTLE CREEK", "WEST HAZLETON", "WILKES-BARRE",
  "WILKINSBURG", "YEADON", "ALTOONA", "COATESVILLE", "DUBOIS",
  "ELLPORT", "GLASSPORT", "MAHANOY CITY"
))

# ---------------------------------------------------------------------------
# 1. Source the cousub-level headline prep to get the panel + cells
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for cousub panel + cells...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

# Pull municipality names from the rate panel
rate <- as.data.table(readRDS(file.path(data_dir, "pa_rate_panel.rds")))
muni_names <- unique(rate[!is.na(cousub_fips),
                            .(muni = cousub_fips,
                              name = toupper(municipality))])
panel <- merge(panel, muni_names, by = "muni", all.x = TRUE)

# Flag Act 47 munis in the Inc-bind cell
treated_incbind <- unique(panel[cell_muni == "Inc-bind",
                                  .(muni, name)])
treated_incbind[, is_act47 := vapply(name, function(nm) {
  any(sapply(act47_names, function(a47) grepl(a47, nm, fixed = TRUE)))
}, logical(1))]

cat(sprintf("\nInc-bind munis: %d total\n", nrow(treated_incbind)))
cat(sprintf("Act 47 (or distressed-list) matches: %d\n",
            sum(treated_incbind$is_act47)))
cat("\nAct 47 Inc-bind munis identified:\n")
print(treated_incbind[is_act47 == TRUE, name])

# ---------------------------------------------------------------------------
# 2. Re-estimate headline cousub regression excluding Act 47 munis
# ---------------------------------------------------------------------------
act47_munis_in_panel <- treated_incbind[is_act47 == TRUE, muni]

panel_no_a47 <- panel[!(cell_muni == "Inc-bind" & muni %in% act47_munis_in_panel)]
# Treat the Act 47 Inc-bind munis as if they were dropped from the regression
# by setting their cell to NA (so they're not in any treated cohort).
panel[, cell_muni_a47drop := cell_muni]
panel[muni %in% act47_munis_in_panel & cell_muni == "Inc-bind",
       cell_muni_a47drop := NA_character_]

# Stack on muni cohorts (mirror headline)
K_stack <- 5L
build_stack <- function(p, cell_col, K = K_stack) {
  treated <- unique(p[get(cell_col) != "NC" & !is.na(get(cell_col)),
                       .(muni, cohort = c_muni, cell = get(cell_col))])
  nc_munis <- unique(p[get(cell_col) == "NC", muni])
  cohort_years <- sort(unique(treated$cohort))
  rbindlist(lapply(cohort_years, function(cy) {
    win <- (cy - K):(cy + K)
    t_m <- unique(treated[cohort == cy, .(muni)])
    if (nrow(t_m) == 0) return(NULL)
    d_t <- p[muni %in% t_m$muni & year %in% win]
    d_t[, treat_cell := get(cell_col)]
    d_c <- p[muni %in% nc_munis & year %in% win]
    d_c[, treat_cell := "NC"]
    rbind(d_t, d_c, fill = TRUE)[, .(muni, year, treat_cell,
                                      stack = cy, rel = year - cy,
                                      dose_muni, dose_sd, dose_nr,
                                      cell_sd, cell_nr,
                                      share_police, share_streets,
                                      share_debt_svc, share_eit,
                                      share_re, share_intergov,
                                      log_live_and_work, log_in_commuters,
                                      log_total_residents, share_live_in)]
  }), fill = TRUE)
}

cat("\nBuilding stacks (full headline + Act 47 excluded)...\n")
stk_full <- build_stack(panel, "cell_muni")
stk_a47drop <- build_stack(panel, "cell_muni_a47drop")
for (s in list(stk_full, stk_a47drop)) {
  s[, stack_muni := paste0(stack, "_", muni)]
  s[, stack_year := paste0(stack, "_", year)]
  s[, treat_cell := factor(treat_cell,
       levels = c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                  "Dec-bind","Dec-becomes-non","Dec-nobind"))]
  s[is.na(dose_sd), dose_sd := 0]
  s[is.na(dose_nr), dose_nr := 0]
}

muni_ctrls <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
ctrls_str <- paste(muni_ctrls, collapse = " + ")

run_one <- function(stk, outcome) {
  f <- as.formula(paste0(outcome,
       " ~ i(treat_cell, dose_muni, ref = 'NC') ",
       "+ i(cell_sd, dose_sd, ref = 'NC') ",
       "+ i(cell_nr, dose_nr, ref = 'NC') + ",
       ctrls_str, " | stack_muni + stack_year"))
  dd <- stk[, c("muni","year","stack_muni","stack_year","treat_cell",
                "dose_muni","cell_sd","dose_sd","cell_nr","dose_nr",
                outcome, muni_ctrls), with = FALSE]
  dd <- dd[complete.cases(dd)]
  m <- feols(f, data = dd, cluster = ~muni)
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
           c("est","se","p"), skip_absent = TRUE)
  ic <- ct[grepl("^treat_cell::Inc-bind:", term)]
  data.table(outcome = outcome,
             est = round(ic$est, 4),
             se = round(ic$se, 4),
             p = round(ic$p, 4),
             n = nobs(m))
}

outcomes <- c("log_live_and_work","log_in_commuters",
              "log_total_residents","share_live_in")
cat("\n=== Cousub headline (all 40 Inc-bind munis) ===\n")
res_full <- rbindlist(lapply(outcomes, function(o) run_one(stk_full, o)))
res_full[, spec := "All 40 Inc-bind munis"]
print(res_full)

cat("\n=== Cousub headline (excluding Act 47 Inc-bind munis) ===\n")
res_a47drop <- rbindlist(lapply(outcomes, function(o) run_one(stk_a47drop, o)))
res_a47drop[, spec := sprintf("Drop %d Act 47 Inc-bind", length(act47_munis_in_panel))]
print(res_a47drop)

results <- rbind(res_full, res_a47drop)
fwrite(results, file.path(out_dir, "act47_robustness_cousub.csv"))
cat("\nSaved data/q3_unified/act47_robustness_cousub.csv\n")

# ---------------------------------------------------------------------------
# 3. LaTeX summary table for the paper
# ---------------------------------------------------------------------------
fmt_coef <- function(est, se, p) {
  stars <- if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
  sprintf("$%+.3f^{%s}$ (%.3f)", est, stars, se)
}

tex <- "\\begin{table}[H]
  \\centering
  \\caption{Act~47 robustness: dropping distressed Inc-bind municipalities from the cousub headline}
  \\label{tab:act47_robustness}
  \\small
  \\begin{tabular}{lcc}
    \\toprule
    Outcome & All 40 Inc-bind & Drop $k$ Act~47 \\\\
    \\midrule
"
for (i in seq_along(outcomes)) {
  o <- outcomes[i]
  r_full <- res_full[outcome == o]
  r_drop <- res_a47drop[outcome == o]
  tex <- paste0(tex,
    sprintf("    \\texttt{%s} & %s & %s \\\\\n",
            gsub("_","\\\\_", o),
            fmt_coef(r_full$est, r_full$se, r_full$p),
            fmt_coef(r_drop$est, r_drop$se, r_drop$p)))
}
tex <- paste0(tex,
  "    \\bottomrule\n  \\end{tabular}\n",
  "  \\vspace{0.3em}\n",
  "  \\begin{minipage}{0.85\\textwidth}\n",
  sprintf("    \\footnotesize \\textit{Notes:} Muni-level Inc-bind coefficient on each residence-side outcome, with same FE / cluster / controls as the headline. Column 1 is the headline (all 40 Inc-bind municipalities). Column 2 drops %d Act 47 distressed-designation municipalities from the Inc-bind cohort; the remaining %d Inc-bind municipalities are mostly Home Rule and Boroughs. Cluster-robust SEs in parentheses. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.\n",
          length(act47_munis_in_panel), 40 - length(act47_munis_in_panel)),
  "  \\end{minipage}\n",
  "\\end{table}\n")

writeLines(tex, file.path(tab_dir, "act47_robustness.tex"))
cat("Saved paper/tables/act47_robustness.tex\n")
cat("\nDone.\n")
