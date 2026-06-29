#!/usr/bin/env bash
# build_all_figures.sh
# ---------------------------------------------------------------------------
# Regenerate EVERY figure in the package — paper figures + all diagnostic/
# robustness figures — into figures/. Self-sufficient: runs the estimation
# steps the figure builders depend on, then the builders, then the robustness
# figure scripts (which re-estimate internally). Requires the vendored panels
# in data/ (and data/multitax/ for the property-tax figures).
#
# FIGURE -> PRODUCING SCRIPT manifest
#   PAPER FIGURES
#     pa_map, rate_changes_by_year, act32_cdf            build_summary_stats.R
#     support_dose_trajectory_muni_vs_sd, pooled_es_muni_only
#                                                        build_uea_aggregate_figures.R
#     support_dose_trajectory_inc_becomes               build_dose_trajectory_inc_becomes.R
#     es_realized_{live_and_work,in_commuters,total_jobs,share_live_in}
#                                                        build_appendix_es_figures.R
#   DIAGNOSTIC / EXTRA (same builders, also emitted)
#     binding_levels_by_year, pa_rate_trajectories, rate_double_histogram
#                                                        build_summary_stats.R
#     pooled_es_sd_only, support_cohort_timing_agg, support_dose_distribution_agg
#                                                        build_uea_aggregate_figures.R
#     sa_binary_*, sd_es_*, es_realized_share_work_outside
#                                                        build_appendix_es_figures.R
#                                                        (these read vendored CSVs of
#                                                         retired block/SD/SA estimators)
#   ROBUSTNESS-SPEC FIGURES (self-estimating)
#     eit_levels_es                                      q3_unified_eit_levels_es.R
#     eit_levels_ptax_es                                 q3_unified_eit_levels_ptax_es.R
#     proptax_es_scale_vs_comp                           q3_proptax_es.R
#     dcdh_vs_feols_es                                   q3_unified_dcdh_robustness.R
#   DESCRIPTIVE RATE-CHANGE FIGURES (from the rate panel)
#     scatter_level_vs_change_{inc,dec}                  build_extra_figures.R
#     trajectories_spaghetti_{inc,dec}                   build_extra_figures.R
#     change_distribution_mirrored, level_histogram      build_extra_figures.R
# ---------------------------------------------------------------------------
set -u
cd "$(dirname "$0")"
say(){ echo "$@"; }
run(){ local s="$1"; local t0=$SECONDS
  Rscript "scripts/$s" >/tmp/fig_$$.log 2>&1
  local rc=$?; if [ $rc -eq 0 ]; then say "  OK   ($((SECONDS-t0))s)  $s"
  else say "  FAIL (rc=$rc) $s  -- tail:"; tail -3 /tmp/fig_$$.log | sed 's/^/      /'; fi; }

before=$(ls figures/*.png 2>/dev/null | wc -l | tr -d ' ')
say "=== figures/ before: $before png ==="

say ""; say "=== 1. estimation prerequisites the builders read ==="
for s in q3_unified_headline_es.R q3_unified_pooled_es.R; do run "$s"; done

say ""; say "=== 2. figure builders (paper + extras) ==="
for s in build_summary_stats.R build_uea_aggregate_figures.R \
         build_dose_trajectory_inc_becomes.R build_appendix_es_figures.R; do run "$s"; done

say ""; say "=== 3. robustness-spec figures (self-estimating) ==="
for s in q3_unified_eit_levels_es.R q3_unified_eit_levels_ptax_es.R q3_proptax_es.R \
         q3_unified_dcdh_robustness.R; do run "$s"; done

say ""; say "=== 4. descriptive rate-change figures (from rate panel) ==="
run "build_extra_figures.R"

after=$(ls figures/*.png 2>/dev/null | wc -l | tr -d ' ')
say ""; say "=== figures/ after: $after png ==="
say ""; say "=== PAPER-FIGURE CHECK ==="
PF="act32_cdf rate_changes_by_year pa_map support_dose_trajectory_muni_vs_sd support_dose_trajectory_inc_becomes pooled_es_muni_only es_realized_log_live_and_work es_realized_log_in_commuters es_realized_log_total_jobs es_realized_share_live_in"
miss=0; for f in $PF; do [ -f "figures/$f.png" ] || { say "  MISSING: $f"; miss=$((miss+1)); }; done
say "missing paper figures: $miss / $(echo $PF | wc -w | tr -d ' ')"
rm -f /tmp/fig_$$.log
say "DONE"
