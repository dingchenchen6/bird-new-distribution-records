#!/usr/bin/env Rscript
# 08_compute_displacement_direction.R
# 计算新纪录的位移距离、方位、经纬度差
# 生成风玫瑰图和方向分布可视化
# Displacement distance, direction, lon/lat delta, wind rose plots

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(openxlsx)
  library(here)
})

sf_use_s2(FALSE)

TASK_ROOT  <- here::here("tasks", "bird_hazard_model_effort_upgrade")
HAZ_ROOT   <- here::here("tasks", "bird_new_record_hazard_model")
SHIFT_ROOT <- here::here("tasks", "bird_new_record_hazard_model")
SHP_ROOT   <- here::here("tasks", "bird_spatiotemporal_patterns", "data", "shapefile_base")

dir.create(file.path(TASK_ROOT, "figures"), recursive = TRUE, showWarnings = FALSE)

# ── 配色 ─────────────────────────────────────────────────────────────────
dir_colors <- c(
  "North" = "#2166ac", "Northeast" = "#67a9cf", "East" = "#d1e5f0",
  "Southeast" = "#fddbc7", "South" = "#ef8a62", "Southwest" = "#b2182b",
  "West" = "#d6604d", "Northwest" = "#4393c3"
)

theme_nature <- theme_bw(base_size = 12, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40", hjust = 0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey70", linewidth = 0.4),
    legend.position = "bottom", legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold")
  )

# ── 1. 读取更新后的新纪录数据 ────────────────────────────────────────────
cat("=== 读取新纪录数据 ===\n")

dt_cbnr <- as.data.table(read.xlsx(file.path(TASK_ROOT, "鸟类新纪录20260509.xlsx"),
                                    sheet = "CBNR（EN）"))

# 标准化列名
setnames(dt_cbnr,
         c("Taxonomy_scientific_name_China2025",
           "New_distribution_province",
           "Source_publication_year"),
         c("species", "province", "pub_year"))

dt_cbnr[, longitude := as.numeric(Longitude)]
dt_cbnr[, latitude  := as.numeric(Latitude)]

# 提取发现年份（优先用 Recordingtime，否则用 Discovery_date）
# Discovery_date 格式多样：2024-09-20, 2010 to 2012, 09, 等
# 用 Recordingtime（原始sheet中有）或从 Discovery_date 提取
dt_cbnr[, discovery_year := {
  # 尝试从 Discovery_date 提取第一个4位数年份
  dates <- as.character(Discovery_date)
  yrs <- as.integer(sub(".*(\\d{4}).*", "\\1", dates))
  # 如果没有匹配到，用 pub_year
  fifelse(!is.na(yrs) & yrs >= 1990 & yrs <= 2025, yrs, as.integer(pub_year))
}]

cat("总记录数:", nrow(dt_cbnr), "\n")
cat("有坐标:", dt_cbnr[!is.na(longitude) & !is.na(latitude), .N], "\n")
cat("发现年份范围:", dt_cbnr[, range(discovery_year, na.rm = TRUE)], "\n")

# ── 2. 读取物种历史分布范围（从CBNR数据中提取）──────────────────────────────
cat("\n=== 读取物种历史分布范围 ===\n")

# CBNR 中的 Original_distribution__province 字段包含物种在中国的历史分布省份
# 格式: "Xinjiang, Yunnan, Guizhou, Guangxi, Guangdong, Hainan, Fujian"
# 特殊值: "No records", "Not mentioned", "无"

# 从原始CBNR数据解析
dt_orig <- as.data.table(read.xlsx(file.path(TASK_ROOT, "鸟类新纪录20260509.xlsx"),
                                    sheet = 2))
setnames(dt_orig, "Taxonomy_scientific_name_China2025", "species")

# 解析历史分布省份
parse_prov_list <- function(prov_str) {
  if (is.na(prov_str) || prov_str == "") return(character(0))
  # 过滤非省份文本
  prov_str <- gsub("No records|Not mentioned|无|\\(.*\\)|central |southern |northern |eastern |western ", "", prov_str, ignore.case = TRUE)
  provs <- trimws(unlist(strsplit(prov_str, ",|;|、")))
  provs <- provs[nchar(provs) > 0]
  # 标准化省份名（首字母大写）
  provs <- sapply(provs, function(p) {
    p <- trimws(p)
    if (nchar(p) > 0) paste0(toupper(substr(p, 1, 1)), tolower(substr(p, 2, nchar(p))))
    else ""
  })
  provs[nchar(provs) > 0]
}

# 合并同一物种的所有历史省份
species_hist_list <- list()
for (sp in unique(dt_orig$species)) {
  sub <- dt_orig[species == sp]
  all_provs <- unique(unlist(lapply(sub$Original_distribution__province, parse_prov_list)))
  if (length(all_provs) > 0) {
    species_hist_list[[sp]] <- all_provs
  }
}

cat("有历史分布的物种数:", length(species_hist_list), "\n")

# 构建data.table
hist_range <- data.table(
  species = names(species_hist_list),
  hist_provinces = lapply(names(species_hist_list), function(sp) species_hist_list[[sp]])
)

# ── 3. 读取省份边界计算分布中心 ───────────────────────────────────────────
cat("\n=== 计算物种历史分布中心 ===\n")

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
shp_prov_wgs84 <- st_transform(shp_prov, "EPSG:4326")

# 等面积投影计算质心
albers_crs <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84")

# 计算每个物种历史分布的质心
compute_range_centroid <- function(provinces, shp) {
  prov_shp <- shp[shp$province %in% provinces, ]
  if (nrow(prov_shp) == 0) return(list(centroid_lon = NA_real_, centroid_lat = NA_real_))
  centroid <- st_transform(prov_shp, albers_crs) %>%
    st_union() %>%
    st_centroid() %>%
    st_transform("EPSG:4326")
  coords <- st_coordinates(centroid)
  list(centroid_lon = coords[1, 1], centroid_lat = coords[1, 2])
}

# 计算所有物种的历史分布质心
cat("计算历史分布质心...\n")
centroids <- lapply(seq_len(nrow(hist_range)), function(i) {
  sp <- hist_range$species[i]
  provs <- unlist(hist_range$hist_provinces[[i]])
  cent <- compute_range_centroid(provs, shp_prov_wgs84)
  if (i %% 50 == 0) cat(sprintf("  %d/%d\n", i, nrow(hist_range)))
  cent
})

hist_range[, centroid_lon := sapply(centroids, `[[`, "centroid_lon")]
hist_range[, centroid_lat := sapply(centroids, `[[`, "centroid_lat")]

cat("质心计算完成, 有效:", sum(!is.na(hist_range$centroid_lon)), "\n")

# 保存
fwrite(hist_range[, .(species, centroid_lon, centroid_lat)],
       file.path(TASK_ROOT, "data", "species_historical_range_centroid.csv"))

# ── 4. 计算位移指标 ──────────────────────────────────────────────────────
cat("\n=== 计算位移指标 ===\n")

# 合并质心到CBNR
dt_disp <- merge(dt_cbnr[!is.na(longitude) & !is.na(latitude)],
                 hist_range[, .(species, centroid_lon, centroid_lat)],
                 by = "species", all.x = TRUE)

cat("有质心匹配的记录:", dt_disp[!is.na(centroid_lon), .N], "\n")

# 经纬度差
dt_disp[, delta_lon := longitude - centroid_lon]
dt_disp[, delta_lat := latitude - centroid_lat]

# 方位角（初始方位角，大圆公式）
initial_bearing_deg <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlon <- (lon2 - lon1) * rad
  y <- sin(dlon) * cos(lat2 * rad)
  x <- cos(lat1 * rad) * sin(lat2 * rad) -
       sin(lat1 * rad) * cos(lat2 * rad) * cos(dlon)
  bearing <- atan2(y, x) * 180 / pi
  (bearing + 360) %% 360
}

dt_disp[, bearing := initial_bearing_deg(centroid_lon, centroid_lat,
                                          longitude, latitude)]

# 8方位分类
classify_direction_8 <- function(angle) {
  ifelse(is.na(angle), NA_character_,
  ifelse(angle >= 337.5 | angle < 22.5, "North",
  ifelse(angle < 67.5, "Northeast",
  ifelse(angle < 112.5, "East",
  ifelse(angle < 157.5, "Southeast",
  ifelse(angle < 202.5, "South",
  ifelse(angle < 247.5, "Southwest",
  ifelse(angle < 292.5, "West", "Northwest"))))))))
}

dt_disp[, direction_8 := classify_direction_8(bearing)]

# 位移距离（Haversine公式）
haversine_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371 * 2 * asin(sqrt(pmin(a, 1)))
}

dt_disp[, dist_to_centroid_km := haversine_km(centroid_lon, centroid_lat,
                                                longitude, latitude)]

# 计算到最近历史分布边缘的距离
# 使用省份边界：找最近的历史分布省份边界点
compute_dist_to_edge <- function(lon, lat, hist_provs, shp) {
  if (is.na(lon) || is.na(lat) || length(hist_provs) == 0) return(NA_real_)
  point <- st_point(c(lon, lat)) %>% st_sfc(crs = "EPSG:4326")
  hist_shp <- shp[shp$province %in% hist_provs, ]
  if (nrow(hist_shp) == 0) return(NA_real_)
  boundary <- st_union(st_transform(hist_shp, albers_crs))
  point_proj <- st_transform(point, albers_crs)
  as.numeric(st_distance(point_proj, boundary)) / 1000
}

# 对有坐标的记录计算到边缘距离（较慢，分批处理）
cat("计算到历史分布边缘距离...\n")
dt_disp_valid <- dt_disp[!is.na(centroid_lon)]
dt_disp_valid[, dist_to_edge_km := {
  hist_provs_list <- lapply(species, function(sp) {
    hr <- hist_range[species == sp]
    if (nrow(hr) > 0) unlist(hr$hist_provinces[[1]]) else character(0)
  })
  dists <- mapply(function(lon, lat, provs) {
    compute_dist_to_edge(lon, lat, provs, shp_prov_wgs84)
  }, longitude, latitude, hist_provs_list)
  dists
}]

# 合并回
dt_disp <- merge(dt_disp, dt_disp_valid[, .(species, province, longitude, latitude,
                                              dist_to_edge_km)],
                  by = c("species", "province", "longitude", "latitude"),
                  all.x = TRUE)

# 负值=在历史分布内，正值=在分布外
dt_disp[, outside_range := dist_to_edge_km > 0]

cat("位移指标计算完成\n")
cat(sprintf("  位移距离范围: %.0f - %.0f km\n",
            dt_disp[, min(dist_to_centroid_km, na.rm = TRUE)],
            dt_disp[, max(dist_to_centroid_km, na.rm = TRUE)]))
cat(sprintf("  方位分布:\n"))
print(dt_disp[, .N, by = direction_8][order(-N)])

# 保存
fwrite(dt_disp[, .(species, province, longitude, latitude, discovery_year,
                    centroid_lon, centroid_lat, delta_lon, delta_lat,
                    bearing, direction_8, dist_to_centroid_km, dist_to_edge_km,
                    outside_range)],
       file.path(TASK_ROOT, "data", "displacement_metrics.csv"))

# ── 5. 风玫瑰图 ─────────────────────────────────────────────────────────
cat("\n=== 生成风玫瑰图 ===\n")

# 准备方向数据
dir_data <- dt_disp[!is.na(direction_8),
                     .(count = .N),
                     by = direction_8]

# 确保8个方向都有
all_dirs <- c("North", "Northeast", "East", "Southeast",
              "South", "Southwest", "West", "Northwest")
dir_data <- merge(data.table(direction_8 = all_dirs), dir_data,
                  by = "direction_8", all.x = TRUE)
dir_data[is.na(count), count := 0]
dir_data[, direction_8 := factor(direction_8, levels = all_dirs)]
dir_data[, pct := count / sum(count) * 100]

# 风玫瑰图（顶刊风格）
p_windrose <- ggplot(dir_data, aes(x = direction_8, y = count, group = 1)) +
  geom_polygon(fill = alpha("#2171b5", 0.25), color = "#2171b5", linewidth = 0.9) +
  geom_line(color = "#2171b5", linewidth = 0.9) +
  geom_point(color = "#2171b5", size = 2.2) +
  annotate("segment", x = 1:8, xend = 1:8, y = 0,
           yend = max(dir_data$count) * 1.05,
           color = "#707070", linewidth = 0.3) +
  scale_y_continuous(
    limits = c(0, max(dir_data$count) * 1.1),
    breaks = c(max(dir_data$count) * 0.5, max(dir_data$count)),
    labels = function(x) paste0(round(x / sum(dir_data$count) * 100), "%")
  ) +
  coord_polar(start = -pi / 8) +
  labs(x = NULL, y = NULL,
       title = "Directional distribution of new records",
       subtitle = "Bearing from historical range centroid to discovery site") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "#79CBE3", linewidth = 0.5, linetype = "22"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7, color = "#6B6B6B"),
    axis.text.x = element_text(size = 8.5, color = "#222222"),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(TASK_ROOT, "figures", "fig_windrose_direction.png"),
       p_windrose, width = 7, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_windrose_direction.pdf"),
       p_windrose, width = 7, height = 7)

# 按目的分组的风玫瑰图
if ("OrderLA" %in% names(dt_cbnr)) {
  setnames(dt_cbnr, "OrderLA", "order_cn")
}

dt_disp[, order_cn := dt_cbnr[match(paste(dt_disp$species, dt_disp$province),
                                      paste(dt_cbnr$species, dt_cbnr$province)),
                               grep("Order", names(dt_cbnr), value=TRUE)[1]]]

# 选取记录数最多的5个目
top_orders <- dt_disp[!is.na(direction_8) & !is.na(order_cn),
                       .N, by = order_cn][order(-N)][1:5, order_cn]

if (length(top_orders) > 0) {
  p_windrose_by_order <- list()
  for (ord in top_orders) {
    sub_dir <- dt_disp[order_cn == ord & !is.na(direction_8),
                        .(count = .N), by = direction_8]
    sub_dir <- merge(data.table(direction_8 = all_dirs), sub_dir,
                     by = "direction_8", all.x = TRUE)
    sub_dir[is.na(count), count := 0]
    sub_dir[, direction_8 := factor(direction_8, levels = all_dirs)]

    p_windrose_by_order[[ord]] <- ggplot(sub_dir, aes(x = direction_8, y = count, group = 1)) +
      geom_polygon(fill = alpha("#d94801", 0.25), color = "#d94801", linewidth = 0.7) +
      geom_line(color = "#d94801", linewidth = 0.7) +
      geom_point(color = "#d94801", size = 1.8) +
      coord_polar(start = -pi / 8) +
      labs(x = NULL, y = NULL, title = ord) +
      theme_minimal(base_size = 9) +
      theme(
        panel.grid.major = element_line(color = "#79CBE3", linewidth = 0.3, linetype = "22"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7),
        plot.title = element_text(size = 9, face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }

  p_combined_windrose <- Reduce(`|`, p_windrose_by_order) +
    plot_annotation(title = "Directional distribution by order",
                    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)))

  ggsave(file.path(TASK_ROOT, "figures", "fig_windrose_by_order.png"),
         p_combined_windrose, width = 14, height = 4, dpi = 300)
  ggsave(file.path(TASK_ROOT, "figures", "fig_windrose_by_order.pdf"),
         p_combined_windrose, width = 14, height = 4)
}

# ── 6. 位移距离分布图 ───────────────────────────────────────────────────
cat("\n=== 生成位移距离分布图 ===\n")

p_dist <- ggplot(dt_disp[!is.na(dist_to_centroid_km)],
                 aes(x = dist_to_centroid_km)) +
  geom_histogram(bins = 40, fill = "#2171b5", alpha = 0.7, color = "white") +
  geom_vline(xintercept = median(dt_disp$dist_to_centroid_km, na.rm = TRUE),
             linetype = "dashed", color = "#d94801", linewidth = 0.8) +
  annotate("text",
           x = median(dt_disp$dist_to_centroid_km, na.rm = TRUE),
           y = Inf, vjust = 2, hjust = -0.1,
           label = sprintf("Median = %.0f km", median(dt_disp$dist_to_centroid_km, na.rm = TRUE)),
           size = 3.5, color = "#d94801") +
  labs(x = "Distance to historical range centroid (km)",
       y = "Count",
       title = "Displacement distance distribution",
       subtitle = "Haversine distance from species historical range centroid to new record site") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures", "fig_displacement_distance.png"),
       p_dist, width = 9, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_displacement_distance.pdf"),
       p_dist, width = 9, height = 5)

# ── 7. 经纬度偏移散点图 ─────────────────────────────────────────────────
p_delta <- ggplot(dt_disp[!is.na(delta_lon) & !is.na(delta_lat)],
                  aes(x = delta_lon, y = delta_lat)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.5, size = 1.5, color = "#2171b5") +
  labs(x = "Delta longitude (°)", y = "Delta latitude (°)",
       title = "Geographic offset from historical range centroid",
       subtitle = "Positive = east/north of centroid") +
  theme_nature

ggsave(file.path(TASK_ROOT, "figures", "fig_lonlat_offset.png"),
       p_delta, width = 8, height = 7, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_lonlat_offset.pdf"),
       p_delta, width = 8, height = 7)

# ── 8. 方位 × 位移距离箱线图 ────────────────────────────────────────────
dir_order <- c("North", "Northeast", "East", "Southeast",
               "South", "Southwest", "West", "Northwest")
dt_disp[, direction_8 := factor(direction_8, levels = dir_order)]

p_box <- ggplot(dt_disp[!is.na(direction_8) & !is.na(dist_to_centroid_km)],
                aes(x = direction_8, y = dist_to_centroid_km, fill = direction_8)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = dir_colors) +
  labs(x = "", y = "Distance to centroid (km)",
       title = "Displacement distance by direction",
       subtitle = "Distance from historical range centroid to new record site") +
  theme_nature +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(TASK_ROOT, "figures", "fig_direction_distance_boxplot.png"),
       p_box, width = 9, height = 5, dpi = 300)
ggsave(file.path(TASK_ROOT, "figures", "fig_direction_distance_boxplot.pdf"),
       p_box, width = 9, height = 5)

# ── 9. 位移指标汇总 ─────────────────────────────────────────────────────
cat("\n=== 位移指标汇总 ===\n")
cat(sprintf("有效记录数: %d\n", dt_disp[!is.na(dist_to_centroid_km), .N]))
cat(sprintf("中位位移距离: %.0f km\n", dt_disp[, median(dist_to_centroid_km, na.rm = TRUE)]))
cat(sprintf("平均位移距离: %.0f km\n", dt_disp[, mean(dist_to_centroid_km, na.rm = TRUE)]))
cat(sprintf("分布外记录比例: %.1f%%\n", 100 * dt_disp[, mean(outside_range, na.rm = TRUE)]))
cat("主方向:", dt_disp[, .N, by = direction_8][order(-N)][1, direction_8], "\n")

cat("\n=== 08_compute_displacement_direction.R 完成 ===\n")
