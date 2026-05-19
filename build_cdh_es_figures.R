# build_cdh_es_figures.R
# Build event-study figures from the CDH (de Chaisemartin & D'Haultfoeuille)
# output for the three Inc cells, on log live-and-work and log in-commuters.
# Uses the project's theme_paper() + ribbon style.

suppressMessages({
  library(data.table)
  library(ggplot2)
})

project_dir <- "/Users/matthewhockert/Desktop/Personal Info/local_income_tax_migration"
fig_dir  <- file.path(project_dir, "figures")
data_dir <- file.path(project_dir, "data", "q3_unified")

PAPER_RED  <- "#C5283D"
PAPER_BLUE <- "#1F5673"

theme_paper <- function(base_size = 11) theme_bw(base_size = base_size) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text       = element_text(face = "bold"))

# Parse a single CDH csv into an ES-ready long table.
# CDH columns:
#   term: "Effect_l" or "Placebo_l"  (l = 1..K)
#   Estimate, SE, LB CI, UB CI
# Map Effect_l -> rel = l, Placebo_l -> rel = -l
read_cdh <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return(NULL)
  d <- fread(path)
  d[, term := trimws(term)]
  d[, type := fifelse(grepl("^Effect", term), "post", "pre")]
  d[, k    := as.integer(sub(".*_", "", term))]
  d[, rel  := fifelse(type == "post", k, -k)]
  setnames(d, c("Estimate","SE","LB CI","UB CI"),
            c("est","se","lo","hi"), skip_absent = TRUE)
  d[, .(rel, est, se, lo, hi, type)]
}

# Cells × outcomes
specs <- list(
  list(cell = "Inc-bind",   outcome = "log_live_and_work",
        csv = "cdh_inc_bind_live_and_work.csv",
        title = "CDH event study: Inc-bind on log live-and-work"),
  list(cell = "Inc-bind",   outcome = "log_in_commuters",
        csv = "cdh_inc_bind_in_commuters.csv",
        title = "CDH event study: Inc-bind on log in-commuters"),
  list(cell = "Inc-becomes", outcome = "log_live_and_work",
        csv = "cdh_inc_becomes_live_and_work.csv",
        title = "CDH event study: Inc-becomes on log live-and-work"),
  list(cell = "Inc-becomes", outcome = "log_in_commuters",
        csv = "cdh_inc_becomes_in_commuters.csv",
        title = "CDH event study: Inc-becomes on log in-commuters"),
  list(cell = "Inc-nobind",  outcome = "log_live_and_work",
        csv = "cdh_inc_nobind_live_and_work.csv",
        title = "CDH event study: Inc-nobind on log live-and-work"),
  list(cell = "Inc-nobind",  outcome = "log_in_commuters",
        csv = "cdh_inc_nobind_in_commuters.csv",
        title = "CDH event study: Inc-nobind on log in-commuters")
)

make_es_figure <- function(spec, save_indiv = TRUE) {
  d <- read_cdh(file.path(data_dir, spec$csv))
  if (is.null(d) || nrow(d) == 0) {
    cat(sprintf("  Skipping %s (csv missing or empty)\n", spec$csv))
    return(NULL)
  }
  ref <- data.table(rel = 0, est = 0, se = 0, lo = 0, hi = 0, type = "ref")
  d <- rbind(d, ref, fill = TRUE)
  setorder(d, rel)
  g <- ggplot(d, aes(x = rel, y = est)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, fill = PAPER_RED, color = NA) +
    geom_line(linewidth = 0.8, color = PAPER_RED) +
    geom_point(size = 2, color = PAPER_RED) +
    scale_x_continuous(breaks = -5:5) +
    labs(title = spec$title,
          subtitle = "Per-pp ATT and pre-event placebo; CDH (de Chaisemartin-D'Haultfoeuille) estimator",
          x = "Event time k", y = expression(hat(beta)[k])) +
    theme_paper()
  out_path <- file.path(fig_dir,
                        sprintf("cdh_es_%s_%s.png",
                                 gsub("-","_", tolower(spec$cell)),
                                 sub("log_", "", spec$outcome)))
  ggsave(out_path, g, width = 7.5, height = 4.5, dpi = 300)
  cat(sprintf("  Saved %s\n", basename(out_path)))
  d[, cell := spec$cell]; d[, outcome := spec$outcome]
  d
}

# Build the six individual figures
cat("Building individual CDH ES figures...\n")
all_dt <- rbindlist(lapply(specs, make_es_figure), fill = TRUE)
fwrite(all_dt, file.path(data_dir, "cdh_es_panel.csv"))

# Combined 3x2 facet figure
cat("\nBuilding combined facet figure...\n")
all_dt[, lo := pmin(lo, est)]
all_dt[, hi := pmax(hi, est)]
all_dt[, cell_f := factor(cell, levels = c("Inc-bind","Inc-becomes","Inc-nobind"))]
all_dt[, outcome_f := factor(outcome,
        levels = c("log_live_and_work","log_in_commuters"),
        labels = c("log(live-and-work)","log(in-commuters)"))]

g_all <- ggplot(all_dt, aes(x = rel, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, fill = PAPER_RED, color = NA) +
  geom_line(linewidth = 0.7, color = PAPER_RED) +
  geom_point(size = 1.6, color = PAPER_RED) +
  facet_grid(outcome_f ~ cell_f, scales = "free_y") +
  scale_x_continuous(breaks = c(-4,-2,0,2,4)) +
  labs(title = "CDH event studies, Inc cells",
        subtitle = "Per-pp ATT (k>=1) and pre-event placebo (k<=-1); k=0 omitted reference",
        x = "Event time k", y = expression(hat(beta)[k])) +
  theme_paper(base_size = 10)
ggsave(file.path(fig_dir, "cdh_es_panel.png"),
        g_all, width = 11.5, height = 6.5, dpi = 300)
cat("Saved cdh_es_panel.png\n\nDone.\n")
