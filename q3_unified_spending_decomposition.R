# q3_unified_spending_decomposition.R
# Tiebout-with-public-goods test: among Inc-bind munis, does the residential
# sorting response depend on WHICH expenditure category the new revenue funded?
#
# For each Inc-bind muni, compute:
#   Δshare_X = share of total expenditure on X at year c+5 minus at year c-1
#   Δ log_live_and_work = log home count at c+5 minus at c-1
# Plot one scatter per category. Estimate one interaction regression per
# category at muni level.
#
# Reads:
#   data/pa_afr_panel.csv   (per-muni-year expenditure detail)
#   data/pa_panel.rds       (per-muni-year LODES residence outcomes)
#   q3_unified_full_table.R (cell classification + first-event year)
#
# Emits:
#   figures/spending_decomposition_scatter.png
#   data/q3_unified/spending_decomposition_correlations.csv
#   data/q3_unified/spending_decomposition_interactions.csv
#   paper/tables/spending_decomposition.tex

suppressMessages({
  library(data.table); library(ggplot2); library(fixest); library(scales)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
source(file.path(project_dir, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data")
out_dir  <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir  <- file.path(project_dir, "figures")
tab_dir  <- file.path(project_dir, "paper", "tables")

# ---------------------------------------------------------------------------
# 1. Use the 6 aggregate share controls already in the panel
# AFR per-category detail (police, fire, streets etc. at the 15-category
# level) is sparsely reported and effectively missing for the Inc-bind
# cohort -- those munis file simplified AFR forms. The 6 aggregate shares
# below were built in build_block_panel.R using the AFR fallback chain
# (police_expenditures -> police etc.) and ARE populated for 40/40 Inc-bind
# munis in event years 2007+.
# ---------------------------------------------------------------------------
share_cols <- c("share_police","share_streets","share_debt_svc",
                "share_eit","share_re","share_intergov")
cat_labels <- c("Police (exp.)","Streets & roads (exp.)","Debt service (exp.)",
                 "EIT (rev. share)","Real estate (rev. share)",
                 "Intergovernmental (rev. share)")
names(cat_labels) <- share_cols

# ---------------------------------------------------------------------------
# 2. Cousub LODES panel + cell classification
# ---------------------------------------------------------------------------
cat("Sourcing q3_unified_full_table.R for cell + first-event year...\n")
e <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

# Pull the 6 share controls directly from the panel (already per muni-year)
afr_keep <- unique(panel[, c("muni","year", share_cols), with = FALSE])
setnames(afr_keep, "muni", "muni_id")
afr_keep[, muni_id := as.character(muni_id)]

# Inc-bind munis with their event year c_muni
inc_bind <- unique(panel[cell_muni == "Inc-bind" & !is.na(c_muni),
                          .(muni, c_muni)])
cat(sprintf("Inc-bind munis: %d\n", nrow(inc_bind)))

# Cousub home-count outcome
co <- as.data.table(readRDS(file.path(data_dir, "pa_panel.rds")))
setnames(co, "work_city", "muni", skip_absent = TRUE)
co[, muni := as.character(muni)]
co_keep <- co[, .(muni, year, log_live_in)]

# ---------------------------------------------------------------------------
# 3. Per-muni event-window deltas: Δshare and Δ log_live_and_work
# ---------------------------------------------------------------------------
inc_bind[, muni := as.character(muni)]

# Pre window: c-3 .. c-1; Post window: c+1 .. min(c+5, 2019)
# Average AFR shares within each window (per muni) to stabilize and avoid
# losing observations where a single year is missing or out of coverage.
build_window_means <- function(inc_bind_dt, afr_keep_dt, pre_lo, pre_hi,
                                post_lo, post_hi_cap = 2019L, suffix) {
  rows <- list()
  for (i in seq_len(nrow(inc_bind_dt))) {
    r <- inc_bind_dt[i]
    yrs <- if (suffix == "pre") seq(r$c_muni + pre_lo, r$c_muni + pre_hi)
           else seq(r$c_muni + post_lo,
                    min(r$c_muni + post_hi_cap - r$c_muni,
                        post_hi_cap, r$c_muni + 5L))
    a <- afr_keep_dt[muni_id == r$muni & year %in% yrs]
    if (nrow(a) == 0) {
      means <- as.list(setNames(rep(NA_real_, length(share_cols)), share_cols))
    } else {
      means <- as.list(sapply(share_cols, function(cc)
        mean(a[[cc]], na.rm = TRUE)))
    }
    rows[[i]] <- c(list(muni = r$muni, c_muni = r$c_muni), means)
  }
  out <- rbindlist(rows, fill = TRUE)
  setnames(out, share_cols, paste0(share_cols, "_", suffix))
  out
}
pre_w  <- build_window_means(inc_bind, afr_keep, -3L, -1L,   1L, suffix = "pre")
post_w <- build_window_means(inc_bind, afr_keep, -3L, -1L,   1L, suffix = "post")
# Override post_w with proper post-window logic
post_w <- list()
for (i in seq_len(nrow(inc_bind))) {
  r <- inc_bind[i]
  post_hi <- min(r$c_muni + 5L, 2019L)
  yrs <- seq(r$c_muni + 1L, post_hi)
  if (length(yrs) == 0) {
    means <- as.list(setNames(rep(NA_real_, length(share_cols)), share_cols))
  } else {
    a <- afr_keep[muni_id == r$muni & year %in% yrs]
    if (nrow(a) == 0)
      means <- as.list(setNames(rep(NA_real_, length(share_cols)), share_cols))
    else
      means <- as.list(sapply(share_cols, function(cc)
        mean(a[[cc]], na.rm = TRUE)))
  }
  post_w[[i]] <- c(list(muni = r$muni, c_muni = r$c_muni), means)
}
post_w <- rbindlist(post_w, fill = TRUE)
setnames(post_w, share_cols, paste0(share_cols, "_post"))

ib <- merge(pre_w, post_w, by = c("muni","c_muni"))

# LODES outcome: same pre/post window means of log_live_in
build_lodes_means <- function(inc_bind_dt, co_dt, lo, hi_cap_2019 = TRUE) {
  out <- numeric(nrow(inc_bind_dt))
  for (i in seq_len(nrow(inc_bind_dt))) {
    r <- inc_bind_dt[i]
    if (lo < 0) yrs <- seq(r$c_muni + lo, r$c_muni - 1L)
    else        yrs <- seq(r$c_muni + 1L,
                            min(r$c_muni + 5L, if (hi_cap_2019) 2019L else 9999L))
    v <- co_dt[muni == r$muni & year %in% yrs, log_live_in]
    out[i] <- if (length(v) == 0) NA_real_ else mean(v, na.rm = TRUE)
  }
  out
}
ib[, y_pre  := build_lodes_means(inc_bind, co_keep, lo = -3L)]
ib[, y_post := build_lodes_means(inc_bind, co_keep, lo =  1L)]
ib[, dlog_lw := y_post - y_pre]

for (sc in share_cols) {
  ib[, (paste0("d_", sc)) := get(paste0(sc, "_post")) - get(paste0(sc, "_pre"))]
}
m_pp <- ib

# Also bring in the realized dose for each Inc-bind muni
dose_lookup <- panel[cell_muni == "Inc-bind",
                      .(dose_muni = first(dose_muni)), by = muni]
m_pp <- merge(m_pp, dose_lookup, by = "muni", all.x = TRUE)

cat(sprintf("Sample with both pre/post AFR + LODES: %d\n",
            nrow(m_pp[!is.na(dlog_lw)])))

# ---------------------------------------------------------------------------
# 4. Correlations per category (used by table + per-panel slope)
# ---------------------------------------------------------------------------
cor_rows <- list()
for (sc in share_cols) {
  d <- m_pp[!is.na(get(paste0("d_", sc))) & !is.na(dlog_lw)]
  if (nrow(d) < 5) next
  xvar <- d[[paste0("d_", sc)]]
  if (sd(xvar, na.rm = TRUE) < 1e-9) {
    cor_rows[[length(cor_rows)+1L]] <- data.table(
      category = sc, label = cat_labels[sc], n = nrow(d),
      slope = NA_real_, slope_se = NA_real_, slope_p = NA_real_,
      cor = NA_real_)
    next
  }
  fit <- lm(as.formula(sprintf("dlog_lw ~ d_%s", sc)), data = d)
  s <- summary(fit)$coefficients
  if (nrow(s) < 2) {
    cor_rows[[length(cor_rows)+1L]] <- data.table(
      category = sc, label = cat_labels[sc], n = nrow(d),
      slope = NA_real_, slope_se = NA_real_, slope_p = NA_real_,
      cor = NA_real_)
    next
  }
  cor_rows[[length(cor_rows)+1L]] <- data.table(
    category = sc, label = cat_labels[sc],
    n = nrow(d),
    slope = s[2, 1], slope_se = s[2, 2], slope_p = s[2, 4],
    cor   = cor(xvar, d$dlog_lw))
}
cor_dt <- rbindlist(cor_rows, fill = TRUE)
fwrite(cor_dt, file.path(out_dir, "spending_decomposition_correlations.csv"))
print(cor_dt[order(slope)])

# ---------------------------------------------------------------------------
# 5. Long-form data for facet scatter
# ---------------------------------------------------------------------------
scatter_long <- melt(m_pp,
                     id.vars = c("muni","c_muni","dlog_lw","dose_muni"),
                     measure.vars = paste0("d_", share_cols),
                     variable.name = "category", value.name = "dshare")
scatter_long[, category_clean := sub("^d_share_","", as.character(category))]
scatter_long[, label := cat_labels[paste0("share_", category_clean)]]
scatter_long <- scatter_long[!is.na(dshare) & !is.na(dlog_lw)]

# Order facets by slope magnitude (so the most "voted-against" categories
# appear first)
order_by_slope <- cor_dt[!is.na(slope)][order(slope), label]
remaining <- setdiff(unique(as.character(cor_dt$label)), as.character(order_by_slope))
order_by_slope <- c(as.character(order_by_slope), remaining)
scatter_long[, label := factor(label, levels = order_by_slope)]

g <- ggplot(scatter_long, aes(x = dshare, y = dlog_lw)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_point(alpha = 0.65, size = 1.6, color = PAPER_RED) +
  geom_smooth(method = "lm", se = TRUE, color = "black",
               fill = "grey75", linewidth = 0.6) +
  facet_wrap(~ label, ncol = 5, scales = "free_x") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Change in expenditure share, year $c-1$ to $c+5$",
       y = "Δ log(workers living in muni) over the same window") +
  theme_paper() +
  theme(strip.text = element_text(size = 8.5),
        panel.spacing = unit(0.4, "lines"))
ggsave(file.path(fig_dir, "spending_decomposition_scatter.png"), g,
       width = 13, height = 8, dpi = 200)
cat("Saved spending_decomposition_scatter.png\n")

# ---------------------------------------------------------------------------
# 6. Build LaTeX table of correlations + slopes
# ---------------------------------------------------------------------------
ord <- cor_dt[order(slope)]
star <- function(p) ifelse(is.na(p), "",
                     ifelse(p < 0.01, "$^{***}$",
                     ifelse(p < 0.05, "$^{**}$",
                     ifelse(p < 0.10, "$^{*}$", ""))))

body <- c(
  "\\begin{table}[H]", "  \\centering",
  "  \\caption{Spending composition and residential sorting: Inc-bind cohort.}",
  "  \\label{tab:spending_decomposition}",
  "  \\small",
  "  \\begin{tabular}{lrrrr}",
  "    \\toprule",
  "    Expenditure category & $N$ & Slope & SE & Corr. \\\\",
  "    \\midrule")
for (i in seq_len(nrow(ord))) {
  r <- ord[i]
  body <- c(body, sprintf(
    "    %s & %d & %.3f%s & (%.3f) & %.3f \\\\",
    r$label, r$n, r$slope, star(r$slope_p), r$slope_se, r$cor))
}
body <- c(body, "    \\bottomrule", "  \\end{tabular}",
          "  \\vspace{0.3em}",
          "  \\begin{minipage}{0.95\\textwidth}",
          "    \\footnotesize \\textit{Notes:} For each Inc-bind",
          "    municipality, the outcome is",
          "    $\\Delta\\log(\\text{live-and-work})$ measured from",
          "    event-year $c{-}1$ to $c{+}5$. The covariate is",
          "    $\\Delta s^{X}$, the change in the share of",
          "    total expenditure devoted to category $X$ over the",
          "    same window. Slope is the OLS coefficient in the",
          "    bivariate regression",
          "    $\\Delta\\log N = \\alpha + \\gamma \\, \\Delta s^{X}$.",
          "    A negative slope means the muni grew share-$X$ AND",
          "    lost more residents; a positive slope means residents",
          "    tolerated the hike when more of the new revenue went",
          "    to category $X$. $N$ varies because some munis report",
          "    zero (or no AFR detail) for some categories at one of",
          "    the two event-window endpoints. $^{*}p<0.10$,",
          "    $^{**}p<0.05$, $^{***}p<0.01$.",
          "  \\end{minipage}", "\\end{table}")
writeLines(body, file.path(tab_dir, "spending_decomposition.tex"))
cat("Wrote spending_decomposition.tex\n")

# ---------------------------------------------------------------------------
# 7. (Optional) muni-level interaction regression per category, stored as CSV
# ---------------------------------------------------------------------------
co_panel <- copy(co); setnames(co_panel, "muni", "muni_id", skip_absent = TRUE)
# Use the stacked muni panel from q3_unified_full_table.R for proper FE
stk_muni <- as.data.table(e$stk_muni)

# Bring Δshare values into the stacked panel as muni-level constants
delta_per_muni <- m_pp[, c("muni", paste0("d_", share_cols)), with = FALSE]
stk_x <- merge(stk_muni, delta_per_muni, by = "muni", all.x = TRUE)
ctrls <- e$ctrls
ctrls_str <- paste(intersect(ctrls, names(stk_x)), collapse = " + ")

inter_rows <- list()
for (sc in share_cols) {
  d_col <- paste0("d_", sc)
  if (!d_col %in% names(stk_x)) next
  stk_x[, d_x := get(d_col)]
  d <- stk_x[!is.na(d_x) & !is.na(log_live_and_work)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(
    "log_live_and_work ~ i(treat_cell, dose_stack, ref = 'NC')",
    " + i(treat_cell, dose_stack * d_x, ref = 'NC') + ",
    ctrls_str, " | stack_muni + stack_year"))
  m <- tryCatch(feols(f, data = d, cluster = ~muni), error = function(e) NULL)
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
           c("est","se","p"), skip_absent = TRUE)
  # The interaction Inc-bind coefficient lives on "treat_cell::Inc-bind:dose_stack:d_x"
  ic <- ct[grepl("Inc-bind", term)]
  if (nrow(ic) == 0) next
  inter_rows[[length(inter_rows)+1L]] <- data.table(
    category = sc, label = cat_labels[sc],
    n = nobs(m),
    main_est = ic$est[1], main_se = ic$se[1], main_p = ic$p[1],
    inter_est = if (nrow(ic) >= 2) ic$est[2] else NA_real_,
    inter_se  = if (nrow(ic) >= 2) ic$se[2]  else NA_real_,
    inter_p   = if (nrow(ic) >= 2) ic$p[2]   else NA_real_)
}
if (length(inter_rows)) {
  inter_dt <- rbindlist(inter_rows, fill = TRUE)
  fwrite(inter_dt, file.path(out_dir, "spending_decomposition_interactions.csv"))
  cat("Saved spending_decomposition_interactions.csv\n")
}

# ---------------------------------------------------------------------------
# 8. DiD on the spending mix itself: does the EIT hike CAUSE a shift in
#    share_X? Same stacked-continuous design as the headline, but with
#    share_X on the LHS, drop share_X from the controls.
# ---------------------------------------------------------------------------
cat("\n========= DiD: does Inc-bind CAUSE a shift in share_X? =========\n")
stk_muni <- as.data.table(e$stk_muni)
all_ctrls <- e$ctrls  # share_police, share_streets, share_debt_svc, share_eit, share_re, share_intergov

did_rows <- list()
for (sc in share_cols) {
  rhs_ctrls <- setdiff(all_ctrls, sc)
  ctrls_str <- paste(rhs_ctrls, collapse = " + ")
  cols <- intersect(c("muni","year","stack_muni","stack_year","treat_cell",
                        "dose_stack", sc, rhs_ctrls), names(stk_muni))
  d <- stk_muni[, ..cols]
  d <- d[complete.cases(d)]
  if (nrow(d) == 0) next
  f <- as.formula(paste0(sc,
       " ~ i(treat_cell, dose_stack, ref = 'NC') + ",
       ctrls_str, " | stack_muni + stack_year"))
  m <- tryCatch(feols(f, data = d, cluster = ~muni), error = function(e) NULL)
  if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "term")
  setnames(ct, c("Estimate","Std. Error","Pr(>|t|)"),
           c("est","se","p"), skip_absent = TRUE)
  cell_rows <- ct[grepl("^treat_cell::", term)]
  cell_rows[, cell := sub("^treat_cell::([^:]+):.*$", "\\1", term)]
  cell_rows[, outcome := sc]
  cell_rows[, label := cat_labels[sc]]
  cell_rows[, n := nobs(m)]
  did_rows[[length(did_rows)+1L]] <- cell_rows[, .(outcome, label, cell, est, se, p, n)]
}
did_dt <- rbindlist(did_rows, fill = TRUE)
fwrite(did_dt, file.path(out_dir, "spending_did_results.csv"))
cat("Saved spending_did_results.csv\n\n")

cat("--- Inc-bind dose-response on share_X (DiD) ---\n")
print(did_dt[cell == "Inc-bind",
              .(label, est = round(est, 4), se = round(se, 4),
                p = round(p, 4), n)])

cat("\n--- All cells, all categories ---\n")
print(did_dt[, .(outcome, cell, est = round(est, 4),
                  se = round(se, 4), p = round(p, 4))])

# Build a compact LaTeX table for the Inc-bind row across categories
ib <- did_dt[cell == "Inc-bind"][match(share_cols, outcome)]
star <- function(p) ifelse(is.na(p), "",
                     ifelse(p < 0.01, "$^{***}$",
                     ifelse(p < 0.05, "$^{**}$",
                     ifelse(p < 0.10, "$^{*}$", ""))))
body <- c(
  "\\begin{table}[H]", "  \\centering",
  "  \\caption{Does an Inc-bind rate hike shift the muni's spending mix? DiD on share$^X$.}",
  "  \\label{tab:spending_did}",
  "  \\small",
  "  \\begin{tabular}{lrrr}",
  "    \\toprule",
  "    Outcome (LHS) & Inc-bind $\\beta$ & SE & $N$ \\\\",
  "    \\midrule")
for (i in seq_len(nrow(ib))) {
  r <- ib[i]
  body <- c(body, sprintf(
    "    %s & %.4f%s & (%.4f) & %s \\\\",
    r$label, r$est, star(r$p), r$se, format(r$n, big.mark = ",")))
}
body <- c(body, "    \\bottomrule", "  \\end{tabular}",
          "  \\vspace{0.3em}",
          "  \\begin{minipage}{0.95\\textwidth}",
          "    \\footnotesize \\textit{Notes:} Each row is a separate",
          "    continuous-stacked DiD with share$^X$ as the outcome.",
          "    Same FE structure as the headline regression",
          "    (\\texttt{stack\\_muni + stack\\_year}), clustered at",
          "    the muni level, control set is the other five share",
          "    variables. $\\beta$ is the Inc-bind dose-response: per",
          "    pp of resident-rate hike at an Inc-bind muni, how much",
          "    the outcome share shifts relative to the NC control",
          "    pool over the same calendar years.",
          "    $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.",
          "  \\end{minipage}", "\\end{table}")
writeLines(body, file.path(tab_dir, "spending_did.tex"))
cat("\nWrote spending_did.tex\n")

cat("\nDone.\n")
