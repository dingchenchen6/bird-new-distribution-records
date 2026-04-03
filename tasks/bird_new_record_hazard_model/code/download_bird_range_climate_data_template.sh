#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# Template downloader for range-map and climate data
# 分布图与气候数据下载模板脚本
# ============================================================
#
# This is a template rather than a fully active downloader because:
# 1. your PKU disk share may require cookies, token renewal, or manual login;
# 2. CHELSA / WorldClim / CRU have different mirror URLs and directory layouts.
# 这是一个模板脚本而不是开箱即跑的强制下载器，因为：
# 1. 北大网盘分享链接可能需要 cookie、token 刷新或手动登录；
# 2. CHELSA / WorldClim / CRU 的镜像地址和目录结构并不统一。
#
# Recommended server directory layout / 推荐服务器目录布局
#   /path/to/project_inputs/
#     range_map/
#     climate/baseline/
#     climate/annual/temp/
#     climate/annual/prec/

DATA_ROOT="${1:-/path/to/project_inputs}"
mkdir -p "${DATA_ROOT}/range_map" \
         "${DATA_ROOT}/climate/baseline" \
         "${DATA_ROOT}/climate/annual/temp" \
         "${DATA_ROOT}/climate/annual/prec"

echo "Created input directories under: ${DATA_ROOT}"
echo
echo "Next steps / 下一步："
echo "1. Manually download the world bird range map from your PKU share and place it in:"
echo "   ${DATA_ROOT}/range_map/"
echo "2. Download baseline climate rasters (1970-2000 mean temp/prec) and place them in:"
echo "   ${DATA_ROOT}/climate/baseline/"
echo "3. Download annual climate rasters and place them in:"
echo "   ${DATA_ROOT}/climate/annual/temp/"
echo "   ${DATA_ROOT}/climate/annual/prec/"
echo "4. Copy data/config_paths_example.csv to data/config_paths.csv and update all paths."
echo
echo "Example placeholder commands / 占位示例命令："
echo "  curl -L '<baseline_temp_url>' -o '${DATA_ROOT}/climate/baseline/temp_1970_2000.tif'"
echo "  curl -L '<baseline_prec_url>' -o '${DATA_ROOT}/climate/baseline/prec_1970_2000.tif'"
echo "  curl -L '<annual_temp_2000_url>' -o '${DATA_ROOT}/climate/annual/temp/temp_2000.tif'"
echo "  curl -L '<annual_prec_2000_url>' -o '${DATA_ROOT}/climate/annual/prec/prec_2000.tif'"
