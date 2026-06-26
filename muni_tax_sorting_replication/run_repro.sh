#!/usr/bin/env bash
# Full top-to-bottom reproduction from the VENDORED panels (no LODES needed).
# Clean-rooms the output dirs, regenerates estimation CSVs + every table/figure,
# then compiles the manuscript. Logs each step's status.
set -u
cd "$(dirname "$0")"
export PATH="/Library/TeX/texbin:$PATH"
LOG=run_repro.log; : > "$LOG"
say(){ echo "$@" | tee -a "$LOG"; }

say "=== CLEAN ROOM: wiping output dirs ==="
rm -f tables/*.tex figures/*.png
say "tables/ now: $(ls tables/ 2>/dev/null | wc -l | tr -d ' ') files; figures/ now: $(ls figures/ 2>/dev/null | wc -l | tr -d ' ') files"

run(){ # run a script, record status + duration
  local s="$1"; local t0=$SECONDS
  if [[ "$s" == *.py ]]; then python3 "scripts/$s" >>"$LOG" 2>&1; else Rscript "scripts/$s" >>"$LOG" 2>&1; fi
  local rc=$?; local dt=$((SECONDS-t0))
  if [ $rc -eq 0 ]; then say "  OK   ($dt s)  $s"; else say "  FAIL (rc=$rc) $s   <-- see $LOG"; fi
}

say ""; say "=== STAGE A: estimation (from vendored panels) ==="
for s in q3_unified_full_table.R q3_unified_headline_es.R q3_unified_pooled.R \
         q3_unified_pooled_es.R q3_unified_act32_3level.R q3_unified_act47_robustness.R \
         q3_unified_eit_revenue.R; do run "$s"; done

say ""; say "=== STAGE B: tables + figures ==="
for s in build_summary_stats.R build_paper_tables.R build_appendix_es_figures.R \
         build_uea_aggregate_figures.R build_dose_trajectory_inc_becomes.R; do run "$s"; done

say ""; say "=== STAGE C: compile manuscript ==="
( cd manuscript && pdflatex -interaction=nonstopmode muni_tax_sorting.tex >/dev/null 2>&1 \
  && biber muni_tax_sorting >/dev/null 2>&1 \
  && pdflatex -interaction=nonstopmode muni_tax_sorting.tex >/dev/null 2>&1 \
  && pdflatex -interaction=nonstopmode muni_tax_sorting.tex >/tmp/repro_tex.log 2>&1 )
grep -aoE "Output written on muni_tax_sorting.pdf \([0-9]+ pages.*\)" /tmp/repro_tex.log | tee -a "$LOG"

say ""; say "=== MANIFEST: regenerated outputs ==="
say "tables/  : $(ls tables/*.tex 2>/dev/null | wc -l | tr -d ' ') fragments"
say "figures/ : $(ls figures/*.png 2>/dev/null | wc -l | tr -d ' ') figures"
say ""; say "=== PAPER EXHIBIT CHECK (must all be present) ==="
PT="summary_block summary_muni summary_sd cell_counts_summary pooled_per_pp_cousub es_pooled_cousub_summary headline_muni_combined es_realized_cousub act47_robustness app_full_muni_cell_results act32_cousub"
PF="act32_cdf rate_changes_by_year pa_map support_dose_trajectory_muni_vs_sd support_dose_trajectory_inc_becomes pooled_es_muni_only es_realized_log_live_and_work es_realized_log_in_commuters es_realized_log_total_jobs es_realized_share_live_in"
miss=0
for t in $PT; do [ -f "tables/$t.tex" ] || { say "  MISSING TABLE: $t"; miss=$((miss+1)); }; done
for f in $PF; do [ -f "figures/$f.png" ] || { say "  MISSING FIG:   $f"; miss=$((miss+1)); }; done
say "missing paper exhibits: $miss"
say ""; say "=== EXTRA (non-paper) outputs the builders also produced ==="
for t in tables/*.tex; do b=$(basename "$t" .tex); echo " $PT " | grep -q " $b " || say "  extra table:  $b"; done
say "...and $(ls figures/*.png 2>/dev/null | grep -vE "$(echo $PF | tr ' ' '|')" | wc -l | tr -d ' ') extra figures"
say ""; say "=== DONE ==="
