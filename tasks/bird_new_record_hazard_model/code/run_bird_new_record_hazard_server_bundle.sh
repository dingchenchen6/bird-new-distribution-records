#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Bird New-Record Hazard Server Bundle
# 中国鸟类新纪录 hazard 模型服务器重跑入口
# ============================================================
#
# This script reruns the main model and threshold sensitivities, then builds
# the visualization bundle for GitHub-ready sharing.
#
# 本脚本用于服务器端一键完成：
# 1. 主模型重跑（threshold = 100）
# 2. 阈值敏感性分析（50 与 200）
# 3. 汇总表、森林图和山脊图输出
# ============================================================

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
R_BIN="${R_BIN:-Rscript}"

cd "$ROOT_DIR"

required_envs=(
  HAZARD_NEW_RECORD_CSV
  HAZARD_EVENT_TABLE_CSV
  HAZARD_SPECIES_YEAR_NATIVE_CSV
  HAZARD_EFFORT_CSV
  HAZARD_EFFORT_LOOKUP_CSV
  HAZARD_SDM_BIRDWATCH_CSV
  HAZARD_SDM_RESCUE_CSV
)

for env_name in "${required_envs[@]}"; do
  if [[ -z "${!env_name:-}" ]]; then
    echo "Missing required environment variable: ${env_name}" >&2
    exit 1
  fi
done

"$R_BIN" code/run_bird_new_record_hazard_model_combined_threshold_test.R 100 2002 2024
"$R_BIN" code/run_bird_new_record_hazard_model_combined_threshold_test.R 50 2002 2024
"$R_BIN" code/run_bird_new_record_hazard_model_combined_threshold_test.R 200 2002 2024
"$R_BIN" code/visualize_bird_new_record_hazard_results.R

echo "Server hazard bundle completed."
