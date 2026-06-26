# q3_unified_eit_levels_ptax_controls.R
# ---------------------------------------------------------------------------
# EIT binding decomposition (the causal headline), re-specified per user:
#   * EIT treatment entered as the rate LEVEL (tau), not the dose.
#     (identical to dose under muni^stack FE; baseline absorbed.)
#   * SD rate also as level.
#   * Fiscal controls: DROP EIT-revenue share + real-estate-tax share;
#     ADD individual property-tax LEVELS for city / SD / county
#     (effective rate = fraction of market value). Keep police, streets,
#     debt-service, intergovernmental shares.
#
# Spec:
#   y ~ i(treat_cell, resident_eit, ref='NC') + i(cell_sd, sd_eit, ref='NC')
#       + share_police + share_streets + share_debt_svc + share_intergov
#       + ptax_muni + ptax_sd + ptax_cty
#       | stack_muni + stack_year ,  cluster ~muni
#
# Output: data/q3_unified/eit_levels_ptax_controls_table.csv  (+ .tex fragment)
# ---------------------------------------------------------------------------

suppressMessages({ library(data.table); library(fixest) })
project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir <- file.path(project_dir, "data"); mt_dir <- file.path(data_dir, "multitax")
out_dir  <- file.path(data_dir, "q3_unified")

# --- reuse the headline stacked panel --------------------------------------
e <- new.env(); sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
stk <- as.data.table(e$stk_muni)

# --- property-tax LEVELS (effective rate) per cousub-year ------------------
b  <- fread(file.path(mt_dir, "pa_property_burden_panel.csv"))
mp <- fread(file.path(mt_dir, "pa_muni_multitax_panel.csv"))[, .(muni_id, year, cousub_fips)]
mp[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
b <- merge(b, mp, by = c("muni_id", "year"), all.x = TRUE)
ptax <- b[!is.na(cousub_fips), .(
  ptax_muni = mean(muni_eff_rate,   na.rm = TRUE),
  ptax_sd   = mean(sd_eff_rate,     na.rm = TRUE),
  ptax_cty  = mean(county_eff_rate, na.rm = TRUE)),
  by = .(muni = cousub_fips, year)]
for (c in c("ptax_muni","ptax_sd","ptax_cty")) ptax[!is.finite(get(c)), (c) := NA_real_]
stk <- merge(stk, ptax, by = c("muni","year"), all.x = TRUE)

# --- new control set --------------------------------------------------------
new_ctrls <- c("share_police","share_streets","share_debt_svc","share_intergov",
               "ptax_muni","ptax_sd","ptax_cty")
ctrls_str <- paste(new_ctrls, collapse = " + ")

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
out_labels <- c("Log(work in own muni)","Log(in-commuters)","Log(total jobs)","Share live in muni")

models <- list()
for (o in outcomes) {
  cols <- c("muni","treat_cell","cell_sd","resident_eit","sd_eit",
            "stack_muni","stack_year", o, new_ctrls)
  d <- stk[, ..cols][complete.cases(stk[, ..cols])]
  f <- as.formula(paste0(o,
       " ~ i(treat_cell, resident_eit, ref='NC') + i(cell_sd, sd_eit, ref='NC') + ",
       ctrls_str, " | stack_muni + stack_year"))
  models[[o]] <- feols(f, data = d, cluster = ~muni)
}

# --- assemble table ---------------------------------------------------------
getc <- function(m, tname) {
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  r <- ct[ct$term == tname]
  if (!nrow(r)) return(c(NA, NA, NA))
  c(r$Estimate, r$`Std. Error`, r$`Pr(>|t|)`)
}
star <- function(p) ifelse(is.na(p),"",ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.1,"*",""))))
cell <- function(v) if (is.na(v[1])) "---" else sprintf("%.3f%s\n(%.3f)", v[1], star(v[3]), v[2])

rows <- list(
  list("Muni resident rate (tau^M)", NA),
  list("  Inc-bind",    "treat_cell::Inc-bind:resident_eit"),
  list("  Inc-becomes", "treat_cell::Inc-becomes:resident_eit"),
  list("  Inc-nobind",  "treat_cell::Inc-nobind:resident_eit"),
  list("School district rate (tau^SD)", NA),
  list("  Inc-bind",    "cell_sd::Inc-bind:sd_eit"),
  list("  Inc-becomes", "cell_sd::Inc-becomes:sd_eit"),
  list("Fiscal controls", NA),
  list("  police-services share", "share_police"),
  list("  streets share",         "share_streets"),
  list("  debt-service share",    "share_debt_svc"),
  list("  intergov-revenue share","share_intergov"),
  list("  city property tax (eff)","ptax_muni"),
  list("  SD property tax (eff)",  "ptax_sd"),
  list("  county property tax (eff)","ptax_cty")
)

tab <- data.table(row = sapply(rows, `[[`, 1))
for (j in seq_along(outcomes)) {
  vals <- sapply(rows, function(r) {
    if (is.na(r[[2]])) return("")
    cell(getc(models[[outcomes[j]]], r[[2]]))
  })
  tab[[out_labels[j]]] <- vals
}
diag <- data.table(row = c("N","R2","Within R2","Dep. mean","Dep. SD"))
for (j in seq_along(outcomes)) {
  m <- models[[outcomes[j]]]
  dv <- m$residuals + m$fitted.values
  diag[[out_labels[j]]] <- c(
    format(nobs(m), big.mark=","),
    sprintf("%.3f", fitstat(m,"r2",simplify=TRUE)),
    sprintf("%.3f", fitstat(m,"wr2",simplify=TRUE)),
    sprintf("%.3f", mean(dv,na.rm=TRUE)),
    sprintf("%.3f", sd(dv,na.rm=TRUE)))
}
full <- rbind(tab, diag)
fwrite(full, file.path(out_dir, "eit_levels_ptax_controls_table.csv"))

# --- pretty console print ---------------------------------------------------
cat("\n================= EIT decomposition (LEVELS) + property-tax level controls =================\n\n")
w <- max(nchar(full$row)) + 2
hdr <- sprintf(paste0("%-",w,"s"), "Cell")
for (l in out_labels) hdr <- paste0(hdr, sprintf("%-22s", l))
cat(hdr, "\n"); cat(strrep("-", nchar(hdr)), "\n")
for (i in seq_len(nrow(full))) {
  parts <- strsplit(unlist(full[i, ..out_labels]), "\n")
  est_line <- sprintf(paste0("%-",w,"s"), full$row[i])
  se_line  <- sprintf(paste0("%-",w,"s"), "")
  for (p in parts) {
    est_line <- paste0(est_line, sprintf("%-22s", ifelse(length(p)>=1 && !is.na(p[1]), p[1], "")))
    se_line  <- paste0(se_line,  sprintf("%-22s", ifelse(length(p)>=2, p[2], "")))
  }
  cat(est_line, "\n")
  if (any(grepl("\\(", se_line))) cat(se_line, "\n")
}
cat("\nSE clustered at muni; muni x stack + year x stack FE. Property tax = effective rate (fraction of market value).\n")
cat("Saved data/q3_unified/eit_levels_ptax_controls_table.csv\n")
