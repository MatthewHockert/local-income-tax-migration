# build_intext_numbers.R
# Reconcile the manuscript's headline IN-TEXT numbers to the script/CSV that
# produces each one, in a single table -> data/q3_unified/intext_numbers.csv
# (workbook sheet "in_text_numbers"). In-text numbers are the exhibits most often
# forgotten and most embarrassing when they don't reconcile, so this makes them a
# first-class, gate-checked artifact like the tables and figures. Validation gates
# (stopifnot) halt loudly if a headline number drifts.
# Run after stage 02. Sources every value from a CSV; nothing is hand-entered
# except the typical event doses, which the abstract itself states (0.24, 0.41 pp,
# the cumulative dose at +5 from the dose-trajectory figures).

suppressMessages({ library(data.table) })
.cfg_args <- commandArgs(trailingOnly = FALSE)
.cfg_fa   <- grep("^--file=", .cfg_args, value = TRUE)
.cfg_dir  <- if (length(.cfg_fa)) dirname(normalizePath(sub("^--file=", "", .cfg_fa[1]))) else getwd()
source(file.path(.cfg_dir, "config.R"))
in_dir <- file.path(ROOT, "data", "q3_unified")

h <- fread(file.path(in_dir, "headline_full_table.csv"))
g <- function(cell) h[term == sprintf("treat_cell::%s:dose_stack", cell) &
                      outcome == "log_live_and_work", est][1]
ib  <- g("Inc-bind"); ibec <- g("Inc-becomes"); inb <- g("Inc-nobind")
inb_p <- h[term == "treat_cell::Inc-nobind:dose_stack" & outcome == "log_live_and_work", p][1]
N <- h[outcome == "log_live_and_work", n][1]
dose_ib <- 0.24; dose_ibec <- 0.41   # cumulative Δτ at +5 (dose-trajectory figs; stated in abstract)

rev <- fread(file.path(in_dir, "eit_revenue_offset.csv"))
spl <- fread(file.path(in_dir, "eit_revenue_split.csv"))
# the paper's "~36% offset" is the STANDARD-STRUCTURE (low-baseline) subgroup, not the
# overall mean offset; the "five high-baseline" munis are Inc-bind-hi.
off_lo <- spl[split == "cap_1pp" & cell == "Inc-bind-lo", offset][1]
off_hi <- spl[split == "cap_1pp" & cell == "Inc-bind-hi", offset][1]

pct <- function(b) round(100 * (exp(b) - 1), 1)
intext <- data.table(
  claim = c(
    "Inc-bind muni-rate effect on log live-and-work (per pp)",
    "Inc-bind full-pp percent change  exp(b)-1",
    "Inc-bind typical-event log change  0.24*b",
    "Inc-bind typical-event percent change",
    "Inc-becomes effect on log live-and-work (per pp)",
    "Inc-becomes typical-event log change  0.41*b",
    "Inc-becomes typical-event percent change",
    "Inc-nobind placebo (per pp; null)",
    "Inc-nobind placebo p-value",
    "Estimation sample N (cousub-years)",
    "EIT revenue net per-pp coefficient (beta_net)",
    "EIT revenue behavioral offset, standard-structure (low-baseline) munis",
    "EIT revenue behavioral offset, high-baseline munis"),
  value = c(
    round(ib, 3), pct(ib), round(dose_ib * ib, 3), pct(dose_ib * ib),
    round(ibec, 3), round(dose_ibec * ibec, 3), pct(dose_ibec * ibec),
    round(inb, 3), round(inb_p, 3), N,
    round(rev$beta_net[1], 3), round(off_lo, 3), round(off_hi, 3)),
  paper_says = c("-0.573", "~ -43%", "-0.14", "~ -13%",
                 "-0.250", "-0.10", "~ -10%", "null", "> 0.1", "—",
                 "1.06", "~ 36%", "(five high-baseline)"),
  source = c(rep("headline_full_table.csv  (sheet headline_decomp)", 9),
             "headline_full_table.csv",
             "eit_revenue_offset.csv  (sheet rev_offset)",
             "eit_revenue_split.csv  (sheet rev_split, cap_1pp Inc-bind-lo)",
             "eit_revenue_split.csv  (sheet rev_split, cap_1pp Inc-bind-hi)"))
fwrite(intext, file.path(in_dir, "intext_numbers.csv"))

# --- validation gates: halt if a headline in-text number drifts -------------
stopifnot(
  "Inc-bind log_LW drifted from -0.573"    = abs(ib   - (-0.573)) < 0.01,
  "Inc-becomes log_LW drifted from -0.250" = abs(ibec - (-0.250)) < 0.01,
  "Inc-nobind placebo no longer null"      = inb_p > 0.10,
  "Estimation N changed from 174,929"      = N == 174929L,
  "EIT revenue net coef drifted from 1.06" = abs(rev$beta_net[1] - 1.06) < 0.02,
  "Low-baseline offset drifted from ~36%"  = abs(off_lo - 0.356) < 0.03)
cat(sprintf("[validation] in-text numbers reconcile: Inc-bind=%.3f (%.0f%%), Inc-becomes=%.3f, placebo p=%.2f, N=%s, rev_net=%.2f, offset_lo=%.0f%%\n",
            ib, pct(ib), ibec, inb_p, format(N, big.mark = ","), rev$beta_net[1], 100*off_lo))
cat(sprintf("Saved %s\n", file.path(in_dir, "intext_numbers.csv")))
