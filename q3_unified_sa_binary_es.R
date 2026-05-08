# q3_unified_sa_binary_es.R
# Binary Sun-Abraham IW event study run at all three estimating units
# (cousub, SD, block). Companion to the realized-dose continuous ES.
#
# fixest::sunab() builds the (cohort x event-time) interactions and
# aggregate(model, "ATT") returns IW-aggregated dynamic ATTs.
#
# Spec, run per outcome and per cell c (subset to {NC + cell c}):
#   y ~ sunab(c_unit, year) + ctrls | unit_x_stack + year_x_stack
# where c_unit is the unit's first event year (NA for NC).
#
# Outputs:
#   data/q3_unified/sa_binary_<level>_coefs.csv
#   data/q3_unified/sa_binary_<level>_att.csv
#   figures/sa_binary_<level>_<outcome>.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(data_dir, "q3_unified"); dir.create(out_dir, showWarnings = FALSE)
fig_dir     <- file.path(project_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE)
K_stack <- 5L

cell_levels <- c("NC","Inc-bind","Inc-becomes","Inc-nobind",
                 "Dec-bind","Dec-becomes-non","Dec-nobind")

# ---------------------------------------------------------------------------
# Generic SA binary ES runner.
# Args:
#   panel:       data.table with columns id, year, cell, c_event, outcome..., ctrls...
#   id_col:      "muni" / "sd_id" / "block_geoid"
#   cell_col:    "cell_muni" / "cell_sd"
#   c_col:       "c_muni" / "c_sd" (the first-event year per id; NA for NC)
#   outcomes:    character vector of outcome column names
#   ctrls:       character vector of control column names
#   cluster_var: formula passed to feols(cluster=...) e.g. ~muni or ~muni+sd_id
#   level_tag:   string used in output file names ("cousub", "sd", "block")
# ---------------------------------------------------------------------------
run_sa_binary <- function(panel, id_col, cell_col, c_col,
                           outcomes, ctrls, cluster_var, level_tag) {
  ctrls_str <- paste(ctrls, collapse = " + ")
  es_results <- list(); att_results <- list()

  # NC must have NA cohort to be excluded from sunab interactions.
  panel <- copy(panel)
  panel[get(cell_col) == "NC", (c_col) := NA]

  cluster_cols <- all.vars(cluster_var)

  for (o in outcomes) {
    cat(sprintf("\n[%s | %s]\n", level_tag, o))
    for (cl in setdiff(cell_levels, "NC")) {
      dd <- panel[get(cell_col) %in% c("NC", cl)]
      cols <- unique(c(id_col, "year", cell_col, c_col, cluster_cols, o, ctrls))
      cols <- intersect(cols, names(dd))
      dd <- dd[, ..cols]
      dd <- dd[complete.cases(dd[, ..o])]
      if (sum(dd[[cell_col]] == cl) == 0) {
        cat(sprintf("  %s skip (no treated)\n", cl)); next
      }
      f <- as.formula(paste0(o,
           " ~ sunab(", c_col, ", year) + ", ctrls_str,
           " | ", id_col, " + year"))
      m <- tryCatch(feols(f, data = dd, cluster = cluster_var),
                     error = function(e) {
                       cat(sprintf("  %s feols error: %s\n", cl, conditionMessage(e))); NULL })
      if (is.null(m)) next

      # Aggregate to IW dynamic ATTs by event-time
      iw <- tryCatch(aggregate(m, "(att)|(year::-?[0-9]+)"),
                     error = function(e) NULL)
      if (is.null(iw) || length(iw) == 0) {
        cat(sprintf("  %s aggregate error\n", cl)); next
      }
      iw_dt <- as.data.table(iw, keep.rownames = "term")
      setnames(iw_dt, c("Estimate","Std. Error","Pr(>|t|)"),
               c("est","se","p"), skip_absent = TRUE)
      # Parse rel from "year::k" or sunab post:: etc.
      iw_dt[, rel := suppressWarnings(as.integer(sub(".*::(-?\\d+).*", "\\1", term)))]
      iw_dt <- iw_dt[!is.na(rel), .(rel, est, se, p)]
      if (nrow(iw_dt) == 0) next
      iw_dt[, outcome := o]; iw_dt[, cell := cl]; iw_dt[, n := nobs(m)]
      es_results[[length(es_results)+1L]] <- iw_dt

      post <- iw_dt[rel >= 0 & rel <= 5]
      pre  <- iw_dt[rel <= -2]
      att_results[[length(att_results)+1L]] <- data.table(
        outcome = o, cell = cl,
        att_post = round(mean(post$est, na.rm = TRUE), 3),
        att_post_se = round(sqrt(mean(post$se^2, na.rm = TRUE) / max(nrow(post),1)), 3),
        att_pre = round(mean(pre$est, na.rm = TRUE), 3),
        att_pre_se = round(sqrt(mean(pre$se^2, na.rm = TRUE) / max(nrow(pre),1)), 3),
        n = nobs(m))
      cat(sprintf("  %-16s post=%6.3f (SE %.3f)  pre=%6.3f (SE %.3f)\n",
                  cl, mean(post$est, na.rm=TRUE),
                  sqrt(mean(post$se^2, na.rm=TRUE) / max(nrow(post),1)),
                  mean(pre$est, na.rm=TRUE),
                  sqrt(mean(pre$se^2, na.rm=TRUE) / max(nrow(pre),1))))
    }
  }
  es_dt  <- rbindlist(es_results, fill = TRUE)
  att_dt <- rbindlist(att_results, fill = TRUE)
  fwrite(es_dt,  file.path(out_dir, paste0("sa_binary_", level_tag, "_coefs.csv")))
  fwrite(att_dt, file.path(out_dir, paste0("sa_binary_", level_tag, "_att.csv")))

  # Figures
  if (nrow(es_dt) > 0) {
    es_plot <- copy(es_dt)
    es_plot <- rbind(es_plot,
                      unique(es_plot[, .(outcome, cell)])[, .(outcome, cell, rel = -1L,
                                                              est = 0, se = 0, p = NA, n = NA)],
                      fill = TRUE)
    es_plot[, lo := est - 1.96 * se]
    es_plot[, hi := est + 1.96 * se]
    for (o in outcomes) {
      dd <- es_plot[outcome == o]
      if (nrow(dd) == 0) next
      p <- ggplot(dd, aes(x = rel, y = est)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
        geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) +
        geom_line() +
        facet_wrap(~ cell, ncol = 3, scales = "free_y") +
        labs(title = paste0(level_tag, " Sun-Abraham IW ES (binary): ", o),
             x = "Event time", y = expression(beta[k])) +
        theme_bw(base_size = 9)
      ggsave(file.path(fig_dir, paste0("sa_binary_", level_tag, "_", o, ".png")),
             p, width = 8.5, height = 5, dpi = 300)
    }
  }
  invisible(list(es = es_dt, att = att_dt))
}

# ---------------------------------------------------------------------------
# Cousub level
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("Cousub-level binary Sun-Abraham IW event study\n")
cat("========================================================\n")
e_cousub <- new.env()
sys.source(file.path(project_dir, "q3_unified_full_table.R"), envir = e_cousub)
cousub_panel <- as.data.table(copy(e_cousub$panel))
cousub_outcomes <- c("log_live_and_work","log_in_commuters","log_total_residents",
                      "share_live_in","share_work_outside")
cousub_ctrls <- e_cousub$ctrls
run_sa_binary(cousub_panel, "muni", "cell_muni", "c_muni",
              cousub_outcomes, cousub_ctrls,
              cluster_var = ~muni, level_tag = "cousub")

# ---------------------------------------------------------------------------
# SD level
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("SD-level binary Sun-Abraham IW event study\n")
cat("========================================================\n")
e_sd <- new.env()
sys.source(file.path(project_dir, "q3_unified_sd_level.R"), envir = e_sd)
sd_panel <- as.data.table(copy(e_sd$sd_panel))
sd_outcomes <- c("log_live_and_work","log_in_commuters","log_total_residents",
                  "share_live_in","share_work_outside")
sd_ctrls <- intersect(c("mean_muni_rate", e_sd$sd_share_ctrls), names(sd_panel))
run_sa_binary(sd_panel, "sd_id", "cell_sd", "c_sd",
              sd_outcomes, sd_ctrls,
              cluster_var = ~sd_id, level_tag = "sd")

# ---------------------------------------------------------------------------
# Block level
# ---------------------------------------------------------------------------
cat("\n========================================================\n")
cat("Block-level binary Sun-Abraham IW event study (muni-side cells)\n")
cat("========================================================\n")
# Lightweight block panel: load block panel, attach cell_muni / c_muni
# from cousub_panel via muni key. Avoids the heavy 37M-row stacked panel.
block_panel <- as.data.table(readRDS(file.path(data_dir, "pa_block_panel.rds")))
# cousub_panel was loaded earlier when we sourced q3_unified_full_table
# into e_cousub. Use that for cell_muni / c_muni / muni share controls.
cousub_meta <- unique(cousub_panel[, .(muni, year, cell_muni, c_muni,
                                        share_police, share_streets, share_debt_svc,
                                        share_eit, share_re, share_intergov)])
# Drop existing cell columns from block_panel that conflict
block_drop <- intersect(c("share_police","share_streets","share_debt_svc",
                           "share_eit","share_re","share_intergov","cell_muni"),
                         names(block_panel))
block_panel[, (block_drop) := NULL]
block_panel <- merge(block_panel, cousub_meta, by = c("muni","year"), all.x = TRUE)
# Merge SD fiscal share controls (with dedup)
sd_fc_path <- file.path(data_dir, "pa_sd_controls.rds")
sd_share_cols <- c("share_state_rev_sd","share_local_rev_sd",
                    "share_federal_rev_sd","share_property_tax_sd",
                    "share_instructional_sd","share_support_sd",
                    "share_capital_sd","share_debt_int_sd")
if (file.exists(sd_fc_path)) {
  sd_fc <- as.data.table(readRDS(sd_fc_path))
  keep <- intersect(c("sd_id","year", sd_share_cols), names(sd_fc))
  sd_fc <- sd_fc[, ..keep]
  sd_fc <- sd_fc[, lapply(.SD, function(x)
                  if (is.numeric(x)) mean(x, na.rm = TRUE) else first(x)),
                  by = .(sd_id, year)]
  for (cc in sd_share_cols) {
    if (cc %in% names(sd_fc))
      set(sd_fc, which(!is.finite(sd_fc[[cc]])), cc, NA_real_)
  }
  block_panel <- merge(block_panel, sd_fc, by = c("sd_id","year"), all.x = TRUE)
}

block_outcomes <- c("log_home_count","log_work_count",
                     "home_in_muni_share","share_work_outside")
block_ctrls <- intersect(c("share_police","share_streets","share_debt_svc",
                            "share_eit","share_re","share_intergov",
                            sd_share_cols),
                          names(block_panel))
run_sa_binary(block_panel, "block_geoid", "cell_muni", "c_muni",
              block_outcomes, block_ctrls,
              cluster_var = ~muni + sd_id, level_tag = "block")

cat("\nDone. Tables in data/q3_unified/sa_binary_*; figures in figures/sa_binary_*\n")
