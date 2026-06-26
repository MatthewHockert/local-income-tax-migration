# q3_unified_eit_levels_es.R
# MAIN event study (levels framework): the level form of the decomposition,
#   y ~ i(rel, resident_eit, ref=-1) + i(cell_sd, sd_eit, ref='NC')
#       + property-tax level controls + retained fiscal shares
#       | muni^stack + year^stack ,  cluster ~muni
# Per cell on the {NC, cell} subset. The rate LEVEL is interacted with
# event-time, so pre-event coefficients are identified (level != 0 pre-event)
# and serve as the parallel-trends test; post-event coefficients trace the
# dynamic path. Companion to the levels decomposition headline.
#
# (The realized-DOSE ES lives in the robustness scripts, not here.)
#
# Outputs: data/q3_unified/eit_levels_es_coefs.csv ; figures/eit_levels_es.png

suppressMessages({ library(data.table); library(fixest); library(ggplot2) })
# --- relative-path config (replaces hardcoded absolute project_dir) ---
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
project_dir <- ROOT
source(file.path(SCRIPT_DIR, "paper_helpers.R"))
data_dir <- file.path(project_dir, "data"); mt_dir <- file.path(data_dir, "multitax")
out_dir  <- file.path(data_dir, "q3_unified"); fig_dir <- file.path(project_dir, "figures")

e <- new.env(); sys.source(file.path(SCRIPT_DIR, "q3_unified_full_table.R"), envir = e)
panel <- as.data.table(e$panel)

b  <- fread(file.path(mt_dir, "pa_property_burden_panel.csv"))
mp <- fread(file.path(mt_dir, "pa_muni_multitax_panel.csv"))[, .(muni_id, year, cousub_fips)]
mp[, cousub_fips := sprintf("%05d", as.integer(cousub_fips))]
b <- merge(b, mp, by = c("muni_id","year"), all.x = TRUE)
ptax <- b[!is.na(cousub_fips), .(ptax_muni = mean(muni_eff_rate, na.rm=TRUE),
                                 ptax_sd   = mean(sd_eff_rate, na.rm=TRUE),
                                 ptax_cty  = mean(county_eff_rate, na.rm=TRUE)),
          by = .(muni = cousub_fips, year)]
panel <- merge(panel, ptax, by = c("muni","year"), all.x = TRUE)

ctrls <- c("share_police","share_streets","share_debt_svc","share_intergov",
           "ptax_muni","ptax_sd","ptax_cty")
cs <- paste(ctrls, collapse = " + ")
cl <- levels(panel$cell_muni)

build_stack <- function(p, K = 5) {
  tr <- unique(p[cell_muni != "NC", .(muni, co = c_muni, ce = cell_muni)])
  nc <- unique(p[cell_muni == "NC", muni])
  rbindlist(lapply(sort(unique(tr$co)), function(cy) {
    w <- (cy-K):(cy+K); tm <- tr[co == cy]; if (!nrow(tm)) return(NULL)
    dt <- p[muni %in% tm$muni & year %in% w]; dt[, tc := as.character(cell_muni)]
    dc <- p[muni %in% nc & year %in% w];      dc[, tc := "NC"]
    d <- rbind(dt, dc, fill = TRUE); d[, rel := as.integer(year - cy)]; d[, stk := cy]; d
  }), fill = TRUE)
}
stk <- build_stack(panel)
stk[, smuni := paste0(stk, "_", muni)]; stk[, syear := paste0(stk, "_", year)]
stk[, cell_sd := factor(cell_sd, levels = cl)]; stk[is.na(sd_eit), sd_eit := 0]

outcomes <- c("log_live_and_work","log_in_commuters","log_total_jobs","share_live_in")
res <- list()
for (o in outcomes) for (c in c("Inc-bind","Inc-becomes","Inc-nobind")) {
  d <- stk[tc %in% c("NC", c)]
  cols <- c("muni","rel","smuni","syear","resident_eit","cell_sd","sd_eit", o, ctrls)
  d <- d[, ..cols][complete.cases(d[, ..cols])]; if (!nrow(d)) next
  f <- as.formula(paste0(o, " ~ i(rel, resident_eit, ref=-1) + i(cell_sd, sd_eit, ref='NC') + ",
                         cs, " | smuni + syear"))
  m <- tryCatch(feols(f, d, cluster = ~muni), error = function(e) NULL); if (is.null(m)) next
  ct <- as.data.table(m$coeftable, keep.rownames = "t")[grepl("^rel::", t)]
  ct[, k := as.integer(sub("rel::(-?\\d+):.*", "\\1", t))]
  wp <- tryCatch(wald_pretrend(m, "^rel::-[2-9]")$p, error = function(e) NA_real_)
  res[[length(res)+1L]] <- ct[, .(outcome = o, cell = c, k,
                                   est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`,
                                   wald_pre_p = wp)]
}
ES <- rbindlist(res, fill = TRUE)
fwrite(ES, file.path(out_dir, "eit_levels_es_coefs.csv"))

# figure
ESp <- rbind(ES, unique(ES[, .(outcome, cell)])[, .(outcome, cell, k = -1L,
              est = 0, se = 0, p = NA, wald_pre_p = NA)], fill = TRUE)
ESp[, `:=`(lo = est - 1.96*se, hi = est + 1.96*se)]
lab <- c(log_live_and_work = "Log(work in own muni)", log_in_commuters = "Log(in-commuters)",
         log_total_jobs = "Log(total jobs)", share_live_in = "Share live in muni")
ESp[, ol := lab[outcome]]
g <- ggplot(ESp, aes(k, est, color = cell, fill = cell)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line() + geom_point(size = 1) +
  facet_wrap(~ ol, scales = "free_y") +
  scale_color_manual(values = c("Inc-bind"="#b2182b","Inc-becomes"="#762a83","Inc-nobind"="#2166ac")) +
  scale_fill_manual(values  = c("Inc-bind"="#b2182b","Inc-becomes"="#762a83","Inc-nobind"="#2166ac")) +
  labs(title = "Level-form event study (main spec: levels + property-tax controls)",
       subtitle = "Coefficient on rate LEVEL x event-time, k=-1 omitted; pre-event = parallel-trends test",
       x = "Event time (years from first muni rate change)", y = expression(delta[k]),
       color = "Cell", fill = "Cell") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "eit_levels_es.png"), g, width = 9, height = 6, dpi = 300)

cat("\n--- Level-form ES summary (Inc-bind / Inc-becomes / Inc-nobind) ---\n")
print(ES[, .(pre_wald_p = round(wald_pre_p[1],3),
             post_mean = round(mean(est[k>=0]),3),
             k5 = round(est[k==5],3)), by = .(outcome, cell)])
cat("\nSaved figures/eit_levels_es.png + data/q3_unified/eit_levels_es_coefs.csv\n")
