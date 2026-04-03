#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# End-to-end server workflow using WorldClim 2.1 10m
# 基于 WorldClim 2.1 10m 的服务器端到端运行脚本
# ============================================================
#
# Step 1. Download and prepare baseline + annual climate rasters.
# 第 1 步：下载并整理基线和年尺度气候栅格。
# Step 2. Run the bird range climate shift metrics pipeline.
# 第 2 步：运行鸟类新纪录历史分布与气候差值主分析。

PREP_CONFIG="${1:-data/worldclim_10m_prepare.csv}"
MAIN_CONFIG="${2:-data/config_paths.csv}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TASK_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
MICROMAMBA_ENV_ROOT="${HOME}/.local/micromamba/envs/birdsdm"

# Prefer the server's micromamba birdsdm environment because its terra build
# can read WorldClim GeoTIFFs, unlike the system R on the current server.
# 优先使用服务器 birdsdm micromamba 环境中的 Rscript，因为其 terra 可读取
# WorldClim GeoTIFF，而当前系统 R 环境下的 terra/GDAL 不能稳定读取该格式。
if [[ -x "${MICROMAMBA_ENV_ROOT}/bin/Rscript" ]]; then
  RSCRIPT_BIN="${RSCRIPT_BIN:-${MICROMAMBA_ENV_ROOT}/bin/Rscript}"
  [[ -d "${MICROMAMBA_ENV_ROOT}/share/proj" ]] && export PROJ_LIB="${MICROMAMBA_ENV_ROOT}/share/proj"
  [[ -d "${MICROMAMBA_ENV_ROOT}/share/gdal" ]] && export GDAL_DATA="${MICROMAMBA_ENV_ROOT}/share/gdal"
  export PATH="${MICROMAMBA_ENV_ROOT}/bin:${PATH}"
else
  RSCRIPT_BIN="${RSCRIPT_BIN:-Rscript}"
fi

echo "=== Runtime R ==="
echo "${RSCRIPT_BIN}"
echo "=== PROJ_LIB ==="
echo "${PROJ_LIB:-<unset>}"
echo "=== GDAL_DATA ==="
echo "${GDAL_DATA:-<unset>}"

echo "=== Step 1: Prepare WorldClim climate inputs ==="
"${RSCRIPT_BIN}" "${TASK_ROOT}/code/prepare_worldclim_10m_climate_inputs.R" "${PREP_CONFIG}"

echo
echo "=== Step 2: Run range-climate-shift metrics ==="
"${RSCRIPT_BIN}" "${TASK_ROOT}/code/compute_bird_range_climate_shift_metrics.R" "${MAIN_CONFIG}"
