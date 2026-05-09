#!/usr/bin/env Rscript
# 06_build_grid_infrastructure.R
# 构建 50km 和 100km 网格基础设施
# Grid construction, climate extraction, SDM suitability, effort assignment
# Two resolutions: 50km and 100km

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
  library(exactextractr)
  library(here)
})
sf_use_s2(FALSE)  # 关闭球面几何，避免拓扑错误

TASK_ROOT  <- here::here("tasks", "bird_hazard_model_effort_upgrade")
HAZ_ROOT   <- here::here("tasks", "bird_new_record_hazard_model")
SDM_ROOT   <- here::here("tasks", "bird_sdm_distribution_modeling")
SHP_ROOT   <- here::here("tasks", "bird_spatiotemporal_patterns", "data", "shapefile_base")

YEAR_MIN <- 2002L
YEAR_MAX <- 2024L

# ── 0. 省份边界与名称映射 ──────────────────────────────────────────────────
shp_prov <- st_read(file.path(SHP_ROOT, "省.shp"), quiet = TRUE)
cn_to_en <- c(
  "北京市" = "Beijing", "天津市" = "Tianjin", "河北省" = "Hebei",
  "山西省" = "Shanxi", "内蒙古自治区" = "Inner Mongolia", "辽宁省" = "Liaoning",
  "吉林省" = "Jilin", "黑龙江省" = "Heilongjiang", "上海市" = "Shanghai",
  "江苏省" = "Jiangsu", "浙江省" = "Zhejiang", "安徽省" = "Anhui",
  "福建省" = "Fujian", "江西省" = "Jiangxi", "山东省" = "Shandong",
  "河南省" = "Henan", "湖北省" = "Hubei", "湖南省" = "Hunan",
  "广东省" = "Guangdong", "广西壮族自治区" = "Guangxi", "海南省" = "Hainan",
  "重庆市" = "Chongqing", "四川省" = "Sichuan", "贵州省" = "Guizhou",
  "云南省" = "Yunnan", "西藏自治区" = "Tibet", "陕西省" = "Shaanxi",
  "甘肃省" = "Gansu", "青海省" = "Qinghai", "宁夏回族自治区" = "Ningxia",
  "新疆维吾尔自治区" = "Xinjiang", "台湾省" = "Taiwan",
  "香港特别行政区" = "Hong Kong", "澳门特别行政区" = "Macao"
)
shp_prov$province <- cn_to_en[shp_prov$省名]
shp_prov <- shp_prov[!is.na(shp_prov$province), ]
shp_wgs84 <- st_transform(shp_prov, crs = "EPSG:4326")

# 中国大陆边界（用于裁剪网格，不含港澳台）
shp_mainland <- shp_wgs84[!shp_wgs84$province %in% c("Taiwan", "Hong Kong", "Macao"), ]
# 修复拓扑错误
shp_mainland <- st_buffer(shp_mainland, dist = 0)
shp_mainland_union <- st_union(shp_mainland)

cat("省份边界加载完成\n")

# ── 1. 构建 50km 和 100km 网格 ────────────────────────────────────────────
# 使用 Albers 等面积投影构建规则网格，再转回 WGS84

build_grid <- function(cell_size_km, grid_name) {
  cat(sprintf("\n--- 构建 %s 网格 ---\n", grid_name))

  # 中国 Albers 等面积投影
  albers_crs <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84")

  # 省份边界转 Albers
  shp_albers <- st_transform(shp_mainland, albers_crs)
  bbox <- st_bbox(shp_albers)

  # 网格
  cell_size <- cell_size_km * 1000  # km → m
  x_seq <- seq(bbox["xmin"] - cell_size, bbox["xmax"] + cell_size, by = cell_size)
  y_seq <- seq(bbox["ymin"] - cell_size, bbox["ymax"] + cell_size, by = cell_size)

  # 生成网格质心点（而非多边形，更简单高效）
  grid_pts <- expand.grid(x = x_seq + cell_size / 2,
                          y = y_seq + cell_size / 2)
  grid_pts_sf <- st_as_sf(grid_pts, coords = c("x", "y"), crs = albers_crs)

  # 裁剪：只保留与省界相交的质心
  cat("  裁剪网格到中国范围 ...\n")
  grid_in_china <- st_intersects(grid_pts_sf, shp_albers)
  has_overlap <- sapply(grid_in_china, length) > 0
  grid_pts_sf <- grid_pts_sf[has_overlap, ]
  grid_pts_sf$grid_id <- seq_len(nrow(grid_pts_sf))

  # 转为 WGS84
  grid_wgs84 <- st_transform(grid_pts_sf, "EPSG:4326")
  coords <- st_coordinates(grid_wgs84)
  grid_wgs84$centroid_lon <- coords[, 1]
  grid_wgs84$centroid_lat <- coords[, 2]

  # 分配省份（质心所在省份）
  cat("  分配格点到省份 ...\n")
  grid_with_prov <- st_join(grid_wgs84, shp_wgs84[, "province"],
                            join = st_intersects)
  # 多匹配时取第一个
  grid_with_prov <- grid_with_prov[!duplicated(grid_with_prov$grid_id), ]

  # 转为 data.table
  grid_base <- data.table(
    grid_id = grid_with_prov$grid_id,
    province = grid_with_prov$province,
    centroid_lon = grid_with_prov$centroid_lon,
    centroid_lat = grid_with_prov$centroid_lat
  )
  grid_base <- grid_base[!is.na(province)]

  cat(sprintf("  %s 网格: %d 格点, %d 省份\n",
              grid_name, nrow(grid_base), length(unique(grid_base$province))))

  list(base = grid_base, name = grid_name)
}

# 构建两种分辨率
grid_50  <- build_grid(50,  "50km")
grid_100 <- build_grid(100, "100km")

# 保存基础格点表
fwrite(grid_50$base,  file.path(TASK_ROOT, "data", "grid_50km_base.csv"))
fwrite(grid_100$base, file.path(TASK_ROOT, "data", "grid_100km_base.csv"))

cat("\n网格基础设施保存完成\n")

# ── 2. 提取格点级气候指标 ────────────────────────────────────────────────
wc_dir <- file.path(SDM_ROOT, "data", "climate", "wc2.1_2.5m")

r_bio1  <- rast(file.path(wc_dir, "wc2.1_2.5m_bio_1.tif"))   # °C × 10
r_bio12 <- rast(file.path(wc_dir, "wc2.1_2.5m_bio_12.tif"))  # mm
r_bio15 <- rast(file.path(wc_dir, "wc2.1_2.5m_bio_15.tif"))  # 降水季节性
r_elev  <- rast(file.path(wc_dir, "wc2.1_2.5m_elev.tif"))    # m

extract_grid_climate <- function(grid_base, grid_name) {
  cat(sprintf("\n--- 提取 %s 格点气候 ---\n", grid_name))

  # 用质心坐标提取
  pts <- vect(cbind(grid_base$centroid_lon, grid_base$centroid_lat),
              crs = "EPSG:4326")

  vals <- extract(r_bio1, pts, ID = FALSE)
  grid_clim <- data.table(
    grid_id = grid_base$grid_id,
    province = grid_base$province,
    bio1  = vals[, 1] / 10,  # °C
    bio12 = extract(r_bio12, pts, ID = FALSE)[, 1],
    bio15 = extract(r_bio15, pts, ID = FALSE)[, 1],
    elev  = extract(r_elev,  pts, ID = FALSE)[, 1]
  )

  cat(sprintf("  %s: %d 格点, bio1 范围 [%.1f, %.1f]°C\n",
              grid_name, nrow(grid_clim),
              grid_clim[, min(bio1, na.rm = TRUE)],
              grid_clim[, max(bio1, na.rm = TRUE)]))

  grid_clim
}

grid_50_clim  <- extract_grid_climate(grid_50$base,  "50km")
grid_100_clim <- extract_grid_climate(grid_100$base, "100km")

fwrite(grid_50_clim,  file.path(TASK_ROOT, "data", "grid_50km_climate.csv"))
fwrite(grid_100_clim, file.path(TASK_ROOT, "data", "grid_100km_climate.csv"))

# ── 3. 格点级气候速度和空间梯度 ─────────────────────────────────────────
# 从省级气候指标分配到格点（格点继承其省份的值）
prov_clim <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

# 仅保留省级级别（跨年份恒定的指标）
prov_clim_const <- unique(prov_clim[, .(province, climate_velocity, precip_velocity,
                                          climate_exposure, warming_rate,
                                          spatial_temp_grad, spatial_prec_grad)])
# 如果空间梯度列不存在，从气候指标中计算
if (!"spatial_temp_grad" %in% names(prov_clim_const)) {
  prov_clim_const[, spatial_temp_grad := NA_real_]
  prov_clim_const[, spatial_prec_grad := NA_real_]
}

assign_prov_metrics <- function(grid_clim, prov_clim_const, grid_name) {
  cat(sprintf("\n--- 分配省级气候指标到 %s 格点 ---\n", grid_name))
  grid_clim <- merge(grid_clim,
                     prov_clim_const,
                     by = "province", all.x = TRUE)
  cat(sprintf("  %s: climate_velocity 范围 [%.4f, %.4f] km/yr\n",
              grid_name,
              grid_clim[, min(climate_velocity, na.rm = TRUE)],
              grid_clim[, max(climate_velocity, na.rm = TRUE)]))
  grid_clim
}

grid_50_clim  <- assign_prov_metrics(grid_50_clim,  prov_clim_const, "50km")
grid_100_clim <- assign_prov_metrics(grid_100_clim, prov_clim_const, "100km")

fwrite(grid_50_clim,  file.path(TASK_ROOT, "data", "grid_50km_climate.csv"))
fwrite(grid_100_clim, file.path(TASK_ROOT, "data", "grid_100km_climate.csv"))

# ── 4. 格点级 SDM 适宜性 ─────────────────────────────────────────────────
# 从 SDM 栅格提取每个物种在每个格点的适宜性
cat("\n--- 提取格点级 SDM 适宜性 ---\n")

sdm_raster_dir <- file.path(SDM_ROOT, "data", "rasters")
cat("SDM 栅格目录:", sdm_raster_dir, "\n")

# 物种子目录列表
sdm_species_dirs <- list.dirs(sdm_raster_dir, recursive = FALSE, full.names = FALSE)
cat("SDM 物种目录数:", length(sdm_species_dirs), "\n")

if (length(sdm_species_dirs) > 0) {
  new_records <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                  "derived_inputs", "ndr_supported.csv"))
  target_species <- unique(new_records$species)

  # 标准化物种名匹配（SDM目录用下划线替换空格）
  target_species_underscore <- gsub(" ", "_", tolower(target_species))
  sdm_species_lower <- tolower(sdm_species_dirs)

  available_species <- target_species[tolower(gsub(" ", "_", target_species)) %in% sdm_species_lower]
  cat("有SDM栅格的目标物种:", length(available_species), "\n")

  # 提取前10个物种的适宜性样本
  extract_sdm_sample <- function(grid_base, grid_name, species_sample, raster_dir) {
    cat(sprintf("\n  提取 %s SDM 样本 (前 %d 种) ...\n", grid_name, length(species_sample)))
    pts <- vect(cbind(grid_base$centroid_lon, grid_base$centroid_lat), crs = "EPSG:4326")
    sdm_list <- list()

    for (sp in species_sample) {
      sp_dir_name <- tolower(gsub(" ", "_", sp))
      r_path <- file.path(raster_dir, sp_dir_name,
                           paste0(sp_dir_name, "_current_probability.tif"))
      if (!file.exists(r_path)) next

      r <- tryCatch(rast(r_path), error = function(e) NULL)
      if (is.null(r)) next

      vals <- tryCatch(extract(r, pts, ID = FALSE)[, 1], error = function(e) rep(NA, nrow(grid_base)))
      sdm_list[[sp]] <- data.table(
        grid_id = grid_base$grid_id,
        species = sp,
        sdm_prob = vals
      )
      cat(sprintf("    %s: mean=%.3f\n", sp, mean(vals, na.rm = TRUE)))
    }

    if (length(sdm_list) > 0) rbindlist(sdm_list) else data.table()
  }

  species_sample <- head(available_species, 10)

  sdm_50  <- extract_sdm_sample(grid_50$base,  "50km",  species_sample, sdm_raster_dir)
  sdm_100 <- extract_sdm_sample(grid_100$base, "100km", species_sample, sdm_raster_dir)

  if (nrow(sdm_50) > 0)  fwrite(sdm_50,  file.path(TASK_ROOT, "data", "grid_50km_sdm_sample.csv"))
  if (nrow(sdm_100) > 0) fwrite(sdm_100, file.path(TASK_ROOT, "data", "grid_100km_sdm_sample.csv"))

  cat("SDM 适宜性样本提取完成\n")
} else {
  cat("警告：未找到 SDM 物种目录，跳过适宜性提取\n")
}

# ── 5. 努力分配到格点 ────────────────────────────────────────────────────
cat("\n--- 努力分配到格点 ---\n")

effort_panel <- fread(file.path(TASK_ROOT, "data", "effort_panel_upgraded.csv"))

# 主模型：格点继承省级努力值
assign_effort <- function(grid_base, effort_panel, grid_name) {
  cat(sprintf("  %s: 分配省级努力到格点\n", grid_name))

  grid_effort <- CJ(grid_id = grid_base$grid_id,
                    year = YEAR_MIN:YEAR_MAX)
  grid_effort <- merge(grid_effort,
                       grid_base[, .(grid_id, province)],
                       by = "grid_id", all.x = TRUE)
  grid_effort <- merge(grid_effort,
                       effort_panel[, .(province, year,
                                        log_effort_record_z, log_effort_visits_z,
                                        effort_pc1_z, log_effort_days_z,
                                        n_visits, n_birding_days)],
                       by = c("province", "year"), all.x = TRUE)

  cat(sprintf("  %s: %d 行, effort NA: %d\n",
              grid_name, nrow(grid_effort),
              sum(is.na(grid_effort$log_effort_visits_z))))
  grid_effort
}

grid_50_effort  <- assign_effort(grid_50$base,  effort_panel, "50km")
grid_100_effort <- assign_effort(grid_100$base, effort_panel, "100km")

fwrite(grid_50_effort,  file.path(TASK_ROOT, "data", "grid_50km_effort.csv"))
fwrite(grid_100_effort, file.path(TASK_ROOT, "data", "grid_100km_effort.csv"))

# ── 6. 格点级风险集构建 ──────────────────────────────────────────────────
cat("\n--- 构建格点级风险集 ---\n")

# 读取省级 SDM 候选
sdm_province <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                 "derived_inputs", "sdm_province.csv"))
new_records <- fread(file.path(HAZ_ROOT, "results", "combined_threshold_100_test",
                                "derived_inputs", "ndr_supported.csv"))

# 省级 SDM 候选（物种-省份对）
candidate_sp_prov <- sdm_province[potential == 1L & historical_presence == 0L,
                                   .(species, province)]

# 首次事件
ndr_first <- new_records[order(species, province, year),
                          .SD[1], by = .(species, province)]
setnames(ndr_first, "year", "first_event_year")

build_grid_risk_set <- function(grid_base, candidate_sp_prov, ndr_first, grid_name) {
  cat(sprintf("\n  构建 %s 风险集\n", grid_name))

  # 省份 → 格点映射
  prov_grid <- unique(grid_base[, .(grid_id, province)])

  # 物种-省份候选 → 物种-格点候选
  candidate_grid <- merge(candidate_sp_prov, prov_grid,
                          by = "province", all.x = TRUE, allow.cartesian = TRUE)
  candidate_grid <- candidate_grid[!is.na(grid_id)]
  cat(sprintf("  %s: 候选 物种-格点对: %d\n", grid_name, nrow(candidate_grid)))

  # 估算总行数
  est_rows <- nrow(candidate_grid) * length(YEAR_MIN:YEAR_MAX)
  cat(sprintf("  %s: 预估风险集行数: %.1f M\n", grid_name, est_rows / 1e6))

  if (est_rows > 50e6) {
    cat(sprintf("  %s: 风险集过大(>50M)，仅保存候选表，跳过年份扩展\n", grid_name))
    # 只保存候选表，后续模型按需构建
    return(candidate_grid[, .(species, province, grid_id)])
  }

  # 扩展年份
  year_seq <- YEAR_MIN:YEAR_MAX
  risk_rows <- candidate_grid[, {
    yrs <- year_seq
    .(year = yrs)
  }, by = .(species, province, grid_id)]

  # 合并首次事件
  risk_rows <- merge(risk_rows,
                     ndr_first[, .(species, province, first_event_year)],
                     by = c("species", "province"), all.x = TRUE)

  # 标记事件：省份发生事件 → 该省份所有格点标记事件
  risk_rows[, event := 0L]
  risk_rows[!is.na(first_event_year) & year == first_event_year, event := 1L]

  # 事件后剔除
  risk_rows <- risk_rows[is.na(first_event_year) | year <= first_event_year]
  risk_rows[, first_event_year := NULL]

  cat(sprintf("  %s 风险集: %d 行, 事件: %d\n",
              grid_name, nrow(risk_rows), sum(risk_rows$event)))
  risk_rows
}

risk_50  <- build_grid_risk_set(grid_50$base,  candidate_sp_prov, ndr_first, "50km")
risk_100 <- build_grid_risk_set(grid_100$base, candidate_sp_prov, ndr_first, "100km")

# 50km 风险集过大，仅保存候选表（不展开年份）
if (is.data.table(risk_50) && ncol(risk_50) == 3 && all(c("species", "province", "grid_id") %in% names(risk_50))) {
  fwrite(risk_50, file.path(TASK_ROOT, "data", "grid_50km_candidates.csv"))
  cat("  50km: 保存候选表（未展开年份）\n")
}

# 合并气候
prov_year_clim <- fread(file.path(TASK_ROOT, "data", "climate_metrics_province_year.csv"))

merge_climate_to_risk <- function(risk_rows, prov_year_clim, grid_name) {
  # 省级气候指标分配到格点
  risk_rows <- merge(risk_rows,
                     prov_year_clim[, .(province, year,
                                        temp_grad_prov_z, prec_grad_prov_z,
                                        climate_velocity_z, precip_velocity_z,
                                        climate_exposure_z, warming_rate_z,
                                        mahalanobis_dist_z,
                                        temp_anom, prec_anom)],
                     by = c("province", "year"), all.x = TRUE)
  cat(sprintf("  %s: 合并气候后 %d 行, climate_velocity_z NA: %d\n",
              grid_name, nrow(risk_rows),
              sum(is.na(risk_rows$climate_velocity_z))))
  risk_rows
}

risk_50  <- merge_climate_to_risk(risk_50,  prov_year_clim, "50km")
risk_100 <- merge_climate_to_risk(risk_100, prov_year_clim, "100km")

# 合并努力
merge_effort_to_risk <- function(risk_rows, effort_panel, grid_name) {
  risk_rows <- merge(risk_rows,
                     effort_panel[, .(province, year,
                                      log_effort_record_z, log_effort_visits_z,
                                      effort_pc1_z, log_effort_days_z)],
                     by = c("province", "year"), all.x = TRUE)
  cat(sprintf("  %s: 合并努力后 %d 行, effort NA: %d\n",
              grid_name, nrow(risk_rows),
              sum(is.na(risk_rows$log_effort_visits_z))))
  risk_rows
}

risk_50  <- merge_effort_to_risk(risk_50,  effort_panel, "50km")
risk_100 <- merge_effort_to_risk(risk_100, effort_panel, "100km")

# year_c
risk_50[, year_c  := year - 2013]
risk_100[, year_c := year - 2013]

# 因子化
risk_50[, species  := factor(species)]
risk_50[, province := factor(province)]
risk_100[, species  := factor(species)]
risk_100[, province := factor(province)]

# 保存
fwrite(risk_50,  file.path(TASK_ROOT, "data", "grid_50km_risk_set.csv"))
fwrite(risk_100, file.path(TASK_ROOT, "data", "grid_100km_risk_set.csv"))

# ── 7. 诊断 ──────────────────────────────────────────────────────────────
cat("\n=== 格点风险集诊断 ===\n")
for (gname in c("50km", "100km")) {
  risk_dt <- if (gname == "50km") risk_50 else risk_100
  grid_base <- if (gname == "50km") grid_50$base else grid_100$base

  cat(sprintf("\n%s 网格:\n", gname))
  cat(sprintf("  格点数: %d\n", nrow(grid_base)))
  cat(sprintf("  风险集行数: %d\n", nrow(risk_dt)))
  cat(sprintf("  事件数: %d\n", sum(risk_dt$event)))
  cat(sprintf("  物种数: %d\n", length(unique(risk_dt$species))))
  cat(sprintf("  省份数: %d\n", length(unique(risk_dt$province))))
  cat(sprintf("  事件率: %.4f%%\n", 100 * mean(risk_dt$event)))

  # 完整案例
  complete <- risk_dt[!is.na(temp_grad_prov_z) &
                      !is.na(log_effort_visits_z) &
                      !is.na(climate_velocity_z)]
  cat(sprintf("  完整案例: %d 行, 事件: %d\n", nrow(complete), sum(complete$event)))
}

cat("\n=== 06_build_grid_infrastructure.R 完成 ===\n")
