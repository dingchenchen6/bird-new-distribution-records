#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Server launcher for bird range climate shift metrics
# 中国鸟类新纪录历史分布与气候差值分析服务器运行脚本
# ============================================================
#
# Usage / 用法
#   bash code/run_bird_range_climate_shift_metrics_server.sh data/config_paths.csv
#
# Notes / 说明
# - This script is intentionally conservative and does not download data by
#   itself; it assumes the large range-map and climate files are already staged
#   on the server or mounted from shared storage.
# - 本脚本默认大体量的 range map 和气候栅格已经放在服务器目录或共享存储上，
#   不在这里直接强制下载。

CONFIG_PATH="${1:-data/config_paths.csv}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TASK_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
LOG_DIR="${TASK_ROOT}/server_logs"
TIMESTAMP="$(date +"%Y%m%d_%H%M%S")"
LOG_PATH="${LOG_DIR}/bird_range_climate_shift_metrics_${TIMESTAMP}.log"

mkdir -p "${LOG_DIR}"

{
  echo "=== Bird Range Climate Shift Metrics ==="
  echo "Task root: ${TASK_ROOT}"
  echo "Config: ${CONFIG_PATH}"
  echo "Host: $(hostname)"
  echo "Date: $(date)"
  echo
  Rscript "${TASK_ROOT}/code/compute_bird_range_climate_shift_metrics.R" "${CONFIG_PATH}"
} 2>&1 | tee "${LOG_PATH}"

echo
echo "Pipeline finished. Log saved to: ${LOG_PATH}"
