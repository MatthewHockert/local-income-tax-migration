# bacon_cont_decomp.R -------------------------------------------------------
# A Bacon-STYLE decomposition of the continuous stacked DiD coefficient.
#
# WHAT THIS IS NOT. Goodman-Bacon (2021) decomposes a TWFE coefficient on a
# BINARY treatment into weighted 2x2 DiD comparisons, some of which use
# already-treated units as controls and can carry negative weights. This design
# has none of that structure: treatment is continuous, the panel is stacked so
# every cohort is compared only to never-changers inside its own window, and the
# coefficients are cell-specific dose slopes. There are no forbidden
# comparisons to find, so the canonical decomposition has nothing to decompose.
#
# WHAT THIS IS. A Frisch-Waugh-Lovell decomposition of each cell's dose
# coefficient into cohort (stack) contributions. For cell k, residualise the
# cell-k dose against EVERYTHING else in the regression -- the other cells'
# doses, the school-district co-treatment, the fiscal and property controls,
# and both stack-interacted fixed effects -- and do the same to the outcome.
# FWL then gives
#       beta_k = sum_it Dtil_it Ytil_it / sum_it Dtil_it^2 .
# Splitting those sums by stack c yields, exactly,
#       beta_k   = sum_c w_kc * beta_kc
#       w_kc     = sum_{c} Dtil^2 / sum_{all} Dtil^2      (weights)
#       beta_kc  = sum_{c} Dtil Ytil / sum_{c} Dtil^2     (stack slopes)
# The weights are sums of squares, so they are NON-NEGATIVE by construction and
# sum to one. That is the point: it shows where the paper's dose-weighted
# average causal response actually comes from, and confirms no stack enters
# with a negative weight.
#
# Note the weights are driven by RESIDUAL dose variation within each stack, not
# by observation counts. A cohort with many municipalities but little residual
# rate variation can carry less weight than a single-municipality cohort whose
# rate moved a lot.
#
# Run:  Rscript bacon_cont_decomp.R
# Out:  data/results/bacon_cont_decomp_{weights,summary}.csv
# ---------------------------------------------------------------------------

source("config.R")
source("_helpers.R")
suppressMessages(library(fixest))

RUN_FWL_GATE   <- TRUE   # chunk 1: prove FWL reproduces feols before decomposing
RUN_DECOMP     <- TRUE   # chunk 2: cohort and cohort-x-dose weights
RUN_BACON_PLOT <- TRUE   # chunk 3: Goodman-Bacon-style weight-vs-estimate scatter
RUN_NEGWEIGHTS <- TRUE   # chunk 4: de Chaisemartin-D'Haultfoeuille negative-weight diagnostic

env   <- build_headline_env()
stk   <- copy(env$stk_muni)
ctrls <- env$ctrls
OUTS  <- c("log_live_and_work", "share_live_in", "log_in_commuters", "log_total_jobs")
INC   <- c("Inc-bind", "Inc-becomes", "Inc-nobind")

# SILENT DECISION 1: dose_sd is NA for municipalities whose school district
# never changed; the headline treats that as zero dose, so do the same here.
stk[is.na(dose_sd), dose_sd := 0]

# SILENT DECISION 2: build the cell x dose regressors EXPLICITLY rather than
# through i(). i() is convenient but hides the design matrix, and FWL needs the
# columns by name. NC is the omitted reference, so it gets no column -- exactly
# as ref='NC' does. The gate below checks the two constructions agree.
mcols <- character(0); scols <- character(0)
for (k in INC) {
  mk <- paste0("d_m_",  gsub("-", "_", k)); sk <- paste0("d_sd_", gsub("-", "_", k))
  stk[, (mk) := dose_stack * (as.character(treat_cell) == k)]
  stk[, (sk) := dose_sd    * (as.character(cell_sd)    == k)]
  mcols <- c(mcols, mk); scols <- c(scols, sk)
}
# Pooled (Eq. 4) analogue: every increaser cell collapsed into one group.
stk[, d_m_pooled  := dose_stack * (as.character(treat_cell) %in% INC)]
stk[, d_sd_pooled := dose_sd    * (as.character(cell_sd)    %in% INC)]

FE <- "stack_muni + stack_year"

# One shared complete-cases sample across every outcome, so the weights are
# comparable across outcomes rather than each resting on its own subsample.
keep <- c("muni", "stack", "stack_muni", "stack_year", mcols, scols,
          "d_m_pooled", "d_sd_pooled", ctrls, OUTS,
          # carried only so the gate can re-run the paper's own i() syntax
          "treat_cell", "cell_sd", "dose_stack", "dose_sd")
dat <- stk[, ..keep]
n_before <- nrow(dat)
dat <- dat[complete.cases(dat)]
message(sprintf("[bacon] shared sample: %s rows (from %s), %d munis, %d stacks",
                format(nrow(dat), big.mark = ","), format(n_before, big.mark = ","),
                uniqueN(dat$muni), uniqueN(dat$stack)))

# FWL residualiser: partial Z (everything except `target`) plus the FE out of
# both the target regressor and the outcome.
# BUG FOUND AND FIXED 2026-10-08: resid(feols(...)) returns residuals only for
# the observations the model USED. fixest drops fixed-effect singletons, so that
# vector is SHORTER than the data and silently misaligns against any column
# taken from `dd` (stack, muni, treat_cell). na.rm = FALSE returns a full-length
# vector with NA on dropped rows; those rows are then removed from the grouping
# columns too, so everything stays aligned. The length gate below enforces it.
resid_on <- function(dd, target, others, y) {
  rhs <- if (length(others)) paste(others, collapse = " + ") else "1"
  fd <- as.formula(paste0(target, " ~ ", rhs, " | ", FE))
  fy <- as.formula(paste0(y,      " ~ ", rhs, " | ", FE))
  rd <- resid(feols(fd, dd, notes = FALSE), na.rm = FALSE)
  ry <- resid(feols(fy, dd, notes = FALSE), na.rm = FALSE)
  stopifnot("residual vector is not aligned with the data" =
              length(rd) == nrow(dd) && length(ry) == nrow(dd))
  list(d = rd, y = ry)
}

# ---- GATE: does FWL reproduce feols, and does the explicit design match i()?
if (RUN_FWL_GATE) {
  gate <- rbindlist(lapply(OUTS, function(o) {
    # (a) explicit columns vs the i() syntax the paper actually runs
    f_i <- as.formula(paste0(o, " ~ i(treat_cell, dose_stack, ref='NC') + ",
                             "i(cell_sd, dose_sd, ref='NC') + ",
                             paste(ctrls, collapse = " + "), " | ", FE))
    f_x <- as.formula(paste0(o, " ~ ", paste(c(mcols, scols, ctrls), collapse = " + "),
                             " | ", FE))
    m_i <- feols(f_i, dat, cluster = ~muni, notes = FALSE)
    m_x <- feols(f_x, dat, cluster = ~muni, notes = FALSE)
    rbindlist(lapply(INC, function(k) {
      mk  <- paste0("d_m_", gsub("-", "_", k))
      b_i <- unname(coef(m_i)[sprintf("treat_cell::%s:dose_stack", k)])
      b_x <- unname(coef(m_x)[mk])
      # (b) FWL on the explicit design
      r   <- resid_on(dat, mk, setdiff(c(mcols, scols, ctrls), mk), o)
      ok  <- !is.na(r$d) & !is.na(r$y)          # drop the FE singletons feols removed
      b_f <- sum(r$d[ok] * r$y[ok]) / sum(r$d[ok]^2)
      data.table(outcome = o, cell = k, beta_i = b_i, beta_explicit = b_x,
                 beta_fwl = b_f,
                 gap_design = abs(b_i - b_x), gap_fwl = abs(b_i - b_f))
    }))
  }))
  print(gate[, .(outcome, cell, beta_i = round(beta_i, 6),
                 gap_design = signif(gap_design, 3), gap_fwl = signif(gap_fwl, 3))])
  stopifnot("explicit design does not match the i() specification" =
              all(gate$gap_design < 1e-8),
            "FWL does not reproduce the feols coefficient" =
              all(gate$gap_fwl < 1e-8))
  message("[bacon] GATE PASSED: explicit design == i(), and FWL == feols")
}


# ---- CHUNK 2: the decomposition -------------------------------------------
# beta_k = sum_g w_kg theta_kg over any partition g of the rows, with
#   w_kg     = sum_g Dtil^2 / sum_all Dtil^2     >= 0,  sum_g w_kg = 1
#   theta_kg = sum_g Dtil Ytil / sum_g Dtil^2
# Two partitions are reported: by cohort (stack), and by cohort x dose, where
# a treated municipality's dose is its own realised rate change and all
# never-changers form a single dose-0 group.
#
# IMPORTANT CAVEAT, carried into the output. D^k is exactly zero for
# never-changers, but Dtil = M_Z D^k is not: residualising a zero column
# against Z leaves minus its projection. Control rows therefore carry weight in
# the denominator. The decomposition is still exact and the weights are still
# non-negative, but only the TREATED groups' theta have a dose-response
# reading. `w_treated` below reports how much of the total weight sits on
# treated rows, which is the share of identification that is actually a dose
# response rather than control-side residual covariance.
if (RUN_DECOMP) {
  dat[, dose_own := fifelse(as.character(treat_cell) %in% INC, dose_stack, 0)]
  dat[, is_treated_k := NA_character_]

  decomp_one <- function(o, k) {
    mk <- paste0("d_m_", gsub("-", "_", k))
    r  <- resid_on(dat, mk, setdiff(c(mcols, scols, ctrls), mk), o)
    w  <- data.table(stack = dat$stack, muni = dat$muni,
                     treated = as.character(dat$treat_cell) == k,
                     dose = dat$dose_own, dt = r$d, yt = r$y)
    # Drop the FE singletons feols removed, from BOTH residuals and the keys.
    w <- w[!is.na(dt) & !is.na(yt)]
    w[, `:=`(num = dt * yt, den = dt^2)]
    tot <- sum(w$den)
    beta <- sum(w$num) / tot

    by_c <- w[, .(w_kc = sum(den)/tot, theta_kc = sum(num)/sum(den),
                  n = .N, n_treated_rows = sum(treated)), by = stack][order(stack)]
    by_c[, contrib := w_kc * theta_kc]

    # cohort x dose: treated munis keep their own dose, controls pool at 0
    w[, grp_dose := fifelse(treated, round(dose, 3), NA_real_)]
    by_cd <- w[, .(w_kcd = sum(den)/tot, theta_kcd = sum(num)/sum(den),
                   n = .N, munis = uniqueN(muni)),
               by = .(stack, treated, grp_dose)][order(-w_kcd)]
    by_cd[, contrib := w_kcd * theta_kcd]

    list(summary = data.table(
           outcome = o, cell = k, beta = beta,
           sum_w = sum(by_c$w_kc), recon = sum(by_c$contrib),
           gap = abs(beta - sum(by_c$contrib)),
           min_w = min(by_c$w_kc), n_neg_w = sum(by_c$w_kc < 0),
           w_treated = w[treated == TRUE, sum(den)]/tot,
           eff_cohorts = 1/sum(by_c$w_kc^2)),
         by_c  = cbind(outcome = o, cell = k, by_c),
         by_cd = cbind(outcome = o, cell = k, by_cd))
  }

  todo <- CJ(o = OUTS, k = INC, sorted = FALSE)
  out  <- lapply(seq_len(nrow(todo)), function(i) decomp_one(todo$o[i], todo$k[i]))
  summ  <- rbindlist(lapply(out, `[[`, "summary"))
  by_c  <- rbindlist(lapply(out, `[[`, "by_c"))
  by_cd <- rbindlist(lapply(out, `[[`, "by_cd"))

  stopifnot("weights do not sum to one"        = all(abs(summ$sum_w - 1) < 1e-10),
            "decomposition does not reconstruct beta" = all(summ$gap < 1e-10),
            "a negative weight appeared"       = all(summ$n_neg_w == 0))
  message("[bacon] DECOMPOSITION VERIFIED: weights non-negative, sum to 1, reconstruct beta")

  fwrite(by_c,  file.path(RESULTS_DIR, "bacon_cont_decomp_weights.csv"))
  fwrite(by_cd, file.path(RESULTS_DIR, "bacon_cont_decomp_cohort_dose.csv"))
  fwrite(summ,  file.path(RESULTS_DIR, "bacon_cont_decomp_summary.csv"))

  cat("\n---- verification ----\n")
  print(summ[, .(outcome, cell, beta = round(beta,4), sum_w = round(sum_w,10),
                 recon = round(recon,4), n_neg_w,
                 w_treated = round(w_treated,4), eff_cohorts = round(eff_cohorts,2))])

  cat("\n---- cohort weights, log_live_and_work ----\n")
  print(dcast(by_c[outcome == "log_live_and_work"],
              stack ~ cell, value.var = "w_kc")[, lapply(.SD, function(x)
                if (is.numeric(x)) round(x,4) else x)])
  cat("\n---- cohort slopes theta_kc, log_live_and_work ----\n")
  print(dcast(by_c[outcome == "log_live_and_work"],
              stack ~ cell, value.var = "theta_kc")[, lapply(.SD, function(x)
                if (is.numeric(x)) round(x,3) else x)])

  cat("\n---- Inc-nobind falsification: the placebo's own decomposition ----\n")
  print(summ[cell == "Inc-nobind", .(outcome, beta = round(beta,4),
             w_treated = round(w_treated,4), eff_cohorts = round(eff_cohorts,2))])
  message("Saved bacon_cont_decomp_{weights,cohort_dose,summary}.csv")
}


# ---- CHUNK 3: the Bacon-style scatter --------------------------------------
# Goodman-Bacon's signature figure plots each 2x2 comparison's estimate against
# its weight, so a reader can see at a glance whether the pooled coefficient is
# carried by a few heavily-weighted comparisons and whether any sit far from the
# aggregate. The analog here: one point per cohort (stack), x = w_kc, y =
# theta_kc, point size = treated municipalities in that cohort, horizontal line
# = the pooled beta_k.
#
# theta is a ratio with sum(Dtil^2) in the denominator, so cohorts carrying
# almost no residual dose variation produce absurd slopes (-179, +268). Those
# are arithmetic noise, not findings. The y-axis is clipped to the range of the
# cohorts that actually matter and the subtitle reports how many points fall
# outside, rather than letting three junk cohorts flatten everything.
if (RUN_BACON_PLOT) {
  suppressMessages(library(ggplot2))
  FIG <- if (exists("OUT_DIR")) OUT_DIR else file.path(ROOT, "figures")
  dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

  bp <- merge(by_c, summ[, .(outcome, cell, beta)], by = c("outcome", "cell"))
  # Drop STRUCTURAL zeros: a cohort that contains no municipality of cell k has
  # no comparison to plot. theta there is 0/0-ish noise, not a small estimate.
  n_struct <- bp[n_treated_rows == 0, .N]
  bp <- bp[n_treated_rows > 0]
  ol <- c(log_live_and_work = "Log(work in own muni)", share_live_in = "Share live in muni",
          log_in_commuters  = "Log(in-commuters)",     log_total_jobs = "Log(total jobs)")
  bp[, out_f  := factor(ol[outcome], levels = unname(ol))]
  bp[, cell_f := factor(cell, levels = INC)]

  # Clip to the cohorts carrying real weight, so the scale is set by them.
  lim <- bp[w_kc >= 0.01, .(lo = min(theta_kc), hi = max(theta_kc)), by = out_f]
  lim[, `:=`(lo = lo - 0.15 * (hi - lo), hi = hi + 0.15 * (hi - lo))]
  bp <- merge(bp, lim, by = "out_f")
  n_off <- bp[theta_kc < lo | theta_kc > hi, .N]

  mk_plot <- function(d, ttl, sub, file, w = 9, h = 8.5) {
    g <- ggplot(d, aes(w_kc, theta_kc)) +
      geom_hline(aes(yintercept = beta), colour = "#2166ac",
                 linetype = "dashed", linewidth = 0.5) +
      geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
      geom_point(aes(size = n_treated_rows), shape = 21, fill = "#b2182b",
                 colour = "white", stroke = 0.4, alpha = 0.9) +
      geom_text(aes(label = stack), size = 2.1, vjust = -1.1, colour = "grey25") +
      facet_grid(if (uniqueN(d$out_f) > 1) out_f ~ cell_f else . ~ cell_f,
                 scales = "free", switch = "y") +
      scale_size_continuous(range = c(1.5, 6), name = "treated rows") +
      labs(title = ttl, subtitle = sub,
           x = expression("cohort weight " * w[kc] * "  (shares of residual dose variance, sum to 1)"),
           y = expression("cohort slope " * theta[kc] * "  (effect per percentage point)")) +
      theme_bw(base_size = 9) +
      theme(legend.position = "bottom", strip.placement = "outside",
            plot.subtitle = element_text(size = 7.5))
    # clip rather than drop, so the axis is set by the cohorts that matter
    g <- g + coord_cartesian(ylim = NULL)
    ggsave(file.path(FIG, file), g, width = w, height = h, dpi = 300)
  }
  bp_in <- bp[theta_kc >= lo & theta_kc <= hi]
  mk_plot(bp_in,
    "Bacon-style decomposition: cohort weight against cohort slope",
    paste0("One point per stack. Blue dashed is the pooled coefficient; weights are shares of residual\n",
           "dose variance and sum to one, so every point enters positively. Point size is treated rows.\n",
           sprintf("%d cohorts hold no municipality of that cell and are omitted; %d of the remaining %d points fall\noutside the plotted range (weight under 1%%, where theta is a ratio with a vanishing denominator).",
                   n_struct, n_off, nrow(bp))),
    "bacon_cont_decomp_scatter.png")

  # Headline outcome alone, larger, for the paper.
  mk_plot(bp_in[outcome == "log_live_and_work"],
    "Bacon-style decomposition, log live-and-work workers",
    paste0("One point per stack, labelled by cohort year. Blue dashed is the pooled coefficient.\n",
           "Two cohorts carry most of the Inc-bind estimate; the Inc-nobind placebo rests on one."),
    "bacon_cont_decomp_scatter_headline.png", w = 8.5, h = 3.6)

  message(sprintf("[bacon] saved scatter figures (%d of %d points off-scale)", n_off, nrow(bp)))
}


# ---- CHUNK 4: the negative-weight diagnostic -------------------------------
# THIS is the de Chaisemartin & D'Haultfoeuille (2020) question, and it is NOT
# what chunks 2-3 answer. Those partition rows, so their weights are sums of
# squares and cannot be negative by construction -- finding none there proves
# nothing.
#
# Write the outcome with a heterogeneous per-unit response,
#     Y_it = Y(0)_it + D_it * tau_it .
# FWL then gives
#     beta = [sum Dtil Y(0)] / sum Dtil^2     (-> 0 under parallel trends)
#          + sum_it w_it tau_it,
#     w_it = Dtil_it * D_it / sum Dtil^2 ,   sum_it w_it = 1.
# These weights are NOT sums of squares, so they CAN be negative: doses are
# non-negative, so w_it < 0 wherever the residualised dose is negative on an
# observation whose actual dose is positive. Control rows have D = 0 and so
# carry no weight at all.
#
# TWO SPLITS MATTER, and reporting the headline number alone is misleading.
#
# (1) LEVEL vs CHANGE. The headline regressor is the rate LEVEL, which is
#     positive (tau_0) even BEFORE the rate change, while the municipality
#     fixed effect makes its residual negative pre-event. Every pre-period
#     treated observation therefore gets a negative weight, mechanically. The
#     cumulative CHANGE is zero pre-event, so those observations drop out.
#     Both give the identical coefficient (verified in RUN_LEVELS_COMPARE), so
#     they are two decompositions of the same number -- but only the change
#     version has interpretable weights.
#
# (2) PRE vs POST. A negative weight multiplies tau_it for that observation.
#     Pre-event, tau is zero by definition (nothing has happened), so negative
#     pre-period weights multiply zero and cannot bias anything. Only
#     POST-period negative mass is a real threat.
if (RUN_NEGWEIGHTS) {
  nw_one <- function(dosevar, lab) {
    mc <- sc <- character(0)
    for (k in INC) {
      a <- paste0("nw_m_", gsub("-", "_", k)); b <- paste0("nw_s_", gsub("-", "_", k))
      stk[, (a) := get(dosevar) * (as.character(treat_cell) == k)]
      stk[, (b) := dose_sd      * (as.character(cell_sd)    == k)]
      mc <- c(mc, a); sc <- c(sc, b)
    }
    kp <- c("muni", "stack", "rel", "treat_cell", "stack_muni", "stack_year",
            mc, sc, ctrls, OUTS[1])
    d <- stk[, ..kp]; d <- d[complete.cases(d)]
    rbindlist(lapply(INC, function(k) {
      a <- paste0("nw_m_", gsub("-", "_", k))
      f <- as.formula(paste0(a, " ~ ", paste(setdiff(c(mc, sc, ctrls), a), collapse = " + "),
                             " | ", FE))
      Dt <- resid(feols(f, d, notes = FALSE), na.rm = FALSE); ok <- !is.na(Dt)
      D <- d[[a]][ok]; Dt <- Dt[ok]; rel <- d$rel[ok]
      w <- Dt * D / sum(Dt^2); tr <- D > 0
      g <- function(sel) data.table(n = sum(sel), n_neg = sum(w[sel] < 0),
                                    sum_w = sum(w[sel]), sum_neg = sum(w[sel][w[sel] < 0]))
      rbind(cbind(dose = lab, cell = k, period = "all",  g(tr)),
            cbind(dose = lab, cell = k, period = "pre",  g(tr & rel <  0)),
            cbind(dose = lab, cell = k, period = "post", g(tr & rel >= 0)))
    }), fill = TRUE)
  }
  negw <- rbind(nw_one("dose_stack", "level"), nw_one("dose_change", "change"))
  negw[, share_neg := round(n_neg / pmax(n, 1), 3)]
  stopifnot("weights do not sum to one" =
              all(abs(negw[period == "all", sum_w] - 1) < 1e-8))
  fwrite(negw, file.path(RESULTS_DIR, "bacon_cont_decomp_negweights.csv"))
  cat("\n---- de Chaisemartin-D'Haultfoeuille negative weights ----\n")
  print(negw[, .(dose, cell, period, n, n_neg, share_neg,
                 sum_w = round(sum_w, 3), sum_neg = round(sum_neg, 3))])
  cat("\nREAD: under the CHANGE specification, post-period negative mass is the\n",
      "only threat. Pre-period negative weights multiply a zero effect.\n")
  message("Saved bacon_cont_decomp_negweights.csv")
}
