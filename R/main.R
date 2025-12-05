
# Study                 : Health impacts and socioeconomic inequalities of future outdoor air pollution in an NECP-compliant Europe
# Date                  : Oct. 2025
# Author                : Clàudia Rodés-Bachs
# Institute             : BC3-Basque Centre for Climate Change
# Description           : Main script to produce the analysis and figures present in the Main and SI sections of the study 
# Re-usage instructions : Execute this R script placing the data in the 'data' folder

######################################################################## SET UP ####

# load libraries
require(eurostat)
library(ggpattern)
library(ggplot2)
library(tmap)
library(magrittr)
library(ggpubr)
library(ggnewscale)
library(patchwork)
library(cowplot)
tmap_options(check.and.fix = TRUE) 

# source additional scripts
source("R/utils.R")
source("R/zzz.R")
source('R/machine_learning.R')

# create figures' directory
if (!dir.exists('figures')) dir.create('figures')


######################################################################## LOAD DATA #### 

# constants
normalized <- T
split_num <- 5 #10 deciles, 5 quintiles

map <- T #T if plotted and saved, F otherwise
yy <- 2030

set.seed(300)

source("R/load_data.R")

######################################################################## METHODOLOGY SKETCH #### 

source("R/fig_meth_sketch.R")

######################################################################## ECONOMETRIC CHECK #### 

source("R/econometric_check.R")

######################################################################## PLOTS #### 

## AP  -------------------------------------------------------------------------
ap_nuts3 <- ap %>%
  dplyr::filter(
    nchar(region) > 3,
    !stringr::str_detect(region, 'TR')
  ) %>%
  dplyr::rename(
    geo = region,
    ap = value
  ) %>%
  dplyr::left_join(nuts3_plot_data,
                   by = "geo"
  )
ap_nuts3_sf <- sf::st_sf(ap_nuts3, geometry = ap_nuts3$geometry)
if (nrow(ap_nuts3[rowSums(is.na(ap_nuts3)) > 0, ]) != 0) {
  warning("There are not-matching NUTS3 codes between the rfasst AP results and EUROSTAT NUTS3 data")
}

plot_ap_gg <- ggplot() +
  geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = ap_nuts3_sf, 
          aes(fill = ap), 
          color = "black",
          color = NA,
          size = 0.0001) + 
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       name = "PM2.5\n[ug/m3]") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size, hjust = 0.5),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_ap_gg,
       filename = paste0("figures/plot_ap.pdf"),
       width = 11, height = 10, units = "cm")

# Ranking ctries PM2.5 concentration
dat_rank = ap_nuts3_sf %>% 
  as.data.frame() %>% 
  dplyr::group_by(CNTR_CODE, year) %>% 
  dplyr::summarise(mean_ap = mean(ap)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(mean_ap)
print(tail(dat_rank))
# 1 HU         2030    9.67
# 2 BE         2030   10.7 
# 3 CH         2030   10.7 
# 4 MT         2030   10.7 
# 5 NL         2030   11.0 
# 6 IT         2030   12.4 


## GRID - AP  -------------------------------------------------------------------------
pdf("figures/plot_grid_ap.pdf", width = 11/2.54, height = 10/2.54)
par(mar = c(0,0,0,0))
r <- raster::raster(pm.ap_raster2_europe)
terra::plot(r, 
            col = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100), 
            legend = FALSE, 
            axes = FALSE, 
            box = FALSE)
r.range <- c(raster::minValue(r), raster::maxValue(r))
terra::plot(r, legend.only=TRUE,     
            col = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100), 
            legend.width=1, legend.shrink=0.65,
            frame.plot = F,
            axis.args=list(font=1,
                           cex.axis=legend.text.size.raster),
            legend.args=list(text='PM2.5\n[ug/m3]', side=3, font=1, line=0.5, cex=legend.title.size.raster))
dev.off()



## DEATHS ----------------------------------------------------------------------
deaths_nuts3 <- deaths %>%
  dplyr::filter(
    nchar(region) > 3,
    !stringr::str_detect(region, 'TR'),
    sex == 'Both'
  ) %>%
  dplyr::rename(
    geo = region,
    deaths = value
  ) %>%
  dplyr::filter(!is.na(deaths)) %>% 
  dplyr::left_join(nuts3_plot_data,
                   by = "geo"
  )
deaths_nuts3_sf <- sf::st_sf(deaths_nuts3, geometry = deaths_nuts3$geometry)
if (nrow(deaths_nuts3[rowSums(is.na(deaths_nuts3)) > 0, ]) != 0) {
  warning("There are not-matching NUTS3 codes between the rfasst MORT results and EUROSTAT NUTS3 data")
}

plot_deaths_gg <- ggplot() +
  geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = deaths_nuts3_sf %>% dplyr::filter(sex == "Both"), 
          aes(fill = deaths), 
          # color = NA, 
          color = "black",
          size = 0.05) + 
  scale_fill_distiller(palette = "Oranges", direction = 1, 
                       name = "Premature Deaths\n[deaths per 1M inhabitants]") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size, hjust = 0.5),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_deaths_gg,
       filename = paste0("figures/plot_deaths", normalized_tag, ".pdf"),
       width = 14, height = 10, units = "cm")

# Ranking ctries PM2.5 mortalities
dat_rank = deaths_nuts3_sf %>% 
  as.data.frame() %>% 
  dplyr::group_by(CNTR_CODE, year) %>% 
  dplyr::summarise(mean_deaths = mean(deaths, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(mean_deaths)
print(tail(dat_rank))
#  CNTR_CODE  year mean_deaths
# 1 SK         2030        357.
# 2 ME         2030        382.
# 3 MK         2030        437.
# 4 HU         2030        445.
# 5 RS         2030        495.
# 6 BG         2030        523.


## GRID - DEATHS  -------------------------------------------------------------------------
filtered_raster <- pm.mort_raster2_europe

if (normalized) {
  pdf(paste0("figures/plot_grid_mort",norm_grid_tag,".pdf"), width = 11/2.54, height = 10/2.54)
  r <- raster::raster(filtered_raster)
  r <- raster::mask(r, as(turkey, "Spatial"), inverse = TRUE) # remove Turkey from the map
  base_r <- raster::raster(eu_mask_raster2)
  base_r <- raster::mask(base_r, as(turkey, "Spatial"), inverse = TRUE) # remove Turkey from the map
  colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Oranges"))(100)
  par(mar = c(0, 0, 0, 0))
  raster::plot(base_r,
               col = 'gray90',
               legend = FALSE,
               axes = FALSE,
               box = FALSE,
               useRaster = TRUE)
  raster::plot(r,
               col = colors,
               legend = FALSE,
               axes = FALSE,
               box = FALSE,
               add = TRUE,
               useRaster = TRUE)
  legend_ticks <- c(0, 1, 2, 3, raster::maxValue(r))
  legend_labels <- c("0", "1", "2", "3", paste0(">", floor(max(r[], na.rm = TRUE))))
  raster::plot(r, legend.only = TRUE,
               col = colors,
               legend.width = 1,
               legend.shrink = 0.65,
               frame.plot = FALSE,
               axis.args = list(
                 font = 1,
                 cex.axis = legend.text.size.raster
               ),
               legend.args = list(
                 text = 'Premature mortality rate\n[per 1M inhabitants,\nper grid cell]',
                 side = 3,
                 font = 1,
                 line = 0.5,
                 cex = legend.title.size.raster
               ))
  dev.off()
} else {
  pdf(paste0("figures/plot_grid_mort",norm_grid_tag,".pdf"), width = 11/2.54, height = 10/2.54)
  r <- raster::raster(filtered_raster)
  r <- raster::mask(r, as(turkey, "Spatial"), inverse = TRUE) # remove Turkey from the map
  base_r <- raster::raster(eu_mask_raster2)
  base_r <- raster::mask(base_r, as(turkey, "Spatial"), inverse = TRUE) # remove Turkey from the map
  colors0 <- colorRampPalette(RColorBrewer::brewer.pal(9, "Oranges"))(100)
  col_part1 <- colors0[seq(50, 100, by = 8)]
  col_part2 <- colors0[seq(1, 50, by = 2)]
  colors <- c(col_part2, col_part1)
  par(mar = c(0, 0, 0, 0))
  raster::plot(base_r,
               col = 'gray90',
               legend = FALSE,
               axes = FALSE,
               box = FALSE,
               useRaster = TRUE)
  raster::plot(r,
               col = colors,
               legend = FALSE,
               axes = FALSE,
               box = FALSE,
               add = TRUE,
               useRaster = TRUE)
  raster::plot(r, legend.only = TRUE,
               col = colors,
               legend.width = 1,
               legend.shrink = 0.65,
               frame.plot = FALSE,
               axis.args = list(
                 font = 1,
                 cex.axis = legend.text.size.raster
               ),
               legend.args = list(
                 text = 'Premature deaths\n[absolute number,\nlog scale]',
                 side = 3,
                 font = 1,
                 line = 0.5,
                 cex = legend.title.size.raster
               ))
  dev.off()
}

## AP CTRY -------------------------------------------------------------------------
ap_ctry <- ap %>%
  dplyr::filter(
    nchar(region) == 3
  ) %>%
  dplyr::rename(
    ISO3 = region,
    ap = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>%
                     dplyr::select(geo = ISO2, ISO3) %>% 
                     dplyr::mutate(ISO3 = ifelse(ISO3 == 'ROM', 'ROU', ISO3)) %>% 
                     dplyr::distinct(),
                   by = "ISO3"
  ) %>% 
  dplyr::left_join(ctry_plot_data,
                   by = "geo"
  ) %>% 
  dplyr::filter(LEVL_CODE == 0)

ap_ctry_sf <- sf::st_sf(ap_ctry, geometry = ap_ctry$geometry)

plot_ap_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = ap_ctry_sf, 
          aes(fill = ap), 
          color = "black",
          # color = NA,
          size = 0.0001) + 
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       name = "PM2.5\n[ug/m3]") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_ap_gg,
       filename = paste0("figures/plot_ctry_ap.pdf"),
       width = 11, height = 10, units = "cm")

# Ranking ctries PM2.5 concentration
dat_rank = ap_ctry_sf %>% 
  as.data.frame() %>% 
  dplyr::group_by(ISO3, year) %>% 
  dplyr::summarise(mean_ap = mean(ap, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(mean_ap)
print(tail(dat_rank))
# ISO3  year  mean_ap
# 1 HUN    2030    9.65
# 2 MLT    2030   10.2 
# 3 BEL    2030   10.2 
# 4 NLD    2030   10.7 
# 5 CHE    2030   10.7 
# 6 ITA    2030   12.0 


## AP CTRY LIMIT WHO -------------------------------------------------------------------------
ap_ctry <- ap %>%
  dplyr::filter(
    nchar(region) == 3
  ) %>%
  dplyr::rename(
    ISO3 = region,
    ap = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>%
                     dplyr::select(geo = ISO2, ISO3) %>% 
                     dplyr::mutate(ISO3 = ifelse(ISO3 == 'ROM', 'ROU', ISO3)) %>% 
                     dplyr::distinct(),
                   by = "ISO3"
  ) %>% 
  dplyr::left_join(ctry_plot_data,
                   by = "geo"
  ) %>% 
  dplyr::filter(LEVL_CODE == 0) %>% 
  dplyr::mutate(overWHO = ifelse(ap > 5, TRUE, FALSE))


ap_ctry_sf <- sf::st_sf(ap_ctry, geometry = ap_ctry$geometry)

plot_ap_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = ap_ctry_sf,
          aes(fill = ap),
          color = "black",
          size = 0.0001) +
  geom_sf_pattern(data = ap_ctry_sf %>% 
                    dplyr::filter(overWHO) %>% 
                    dplyr::distinct(),
                  aes(fill = ap),
                  linewidth = NA,
                  pattern = "stripe", 
                  pattern_density = 0.4,
                  pattern_spacing = unit(0.02, "npc"),
                  pattern_angle = 45,
                  pattern_linetype = "solid",
                  pattern_fill = "transparent",
                  pattern_color = "red",
                  pattern_size = unit(0.1, "mm"),
                  pattern_alpha = 0.8,
                  color = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       name = "PM2.5\n[ug/m3]") +
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(plot = plot_ap_gg,
       filename = paste0("figures/plot_ctry_limit_ap.png"),
       width = 11, height = 10, units = "cm")

# Print ctries over WHO guidelines
print(ap_ctry_sf %>% dplyr::filter(overWHO) %>% dplyr::pull(ISO3) %>% unique())
# [1] "ALB" "AUT" "BEL" "BGR" "CHE" "CZE" "DEU" "DNK" "ESP" "FRA" "GBR" "GRC" "HRV" "HUN" 
# "ITA" "LIE" "LTU" "LUX" "MKD" "MLT" "MNE" "NLD" "POL" "PRT" "ROU" "SRB" "SVK" "SVN"

print(ap_ctry_sf %>% dplyr::filter(!overWHO) %>% dplyr::pull(ISO3) %>% unique())
# [1] "EST" "FIN" "IRL" "ISL" "LVA" "NOR" "SWE"

## DEATHS CTRY ----------------------------------------------------------------------
deaths_ctryy <- deaths_ctry %>%
  dplyr::filter(
    nchar(region) == 3,
    sex == 'Both'
  ) %>%
  dplyr::rename(
    ISO3 = region,
    deaths = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>% 
                     dplyr::select(geo = ISO2, ISO3) %>% 
                     dplyr::distinct(),
                   by = "ISO3"
  ) %>% 
  dplyr::group_by(year, sex, scenario, geo, ISO3) %>% 
  dplyr::summarise(deaths = sum(deaths, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_plot_data,
                   by = "geo"
  ) %>% 
  dplyr::filter(LEVL_CODE == 0)

deaths_ctry_sf <- sf::st_sf(deaths_ctryy, geometry = deaths_ctryy$geometry)

# deaths_ctry_sf$deaths[deaths_ctry_sf$deaths > 25] <- 25

plot_deaths_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = deaths_ctry_sf %>% dplyr::filter(sex == "Both"), 
          aes(fill = deaths), 
          # color = NA, 
          color = "black",
          size = 0.05) + 
  scale_fill_distiller(palette = "Oranges", direction = 1, 
                       name = "Premature Deaths\n[deaths per 1M inhabitants]") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_deaths_gg,
       filename = paste0("figures/plot_ctry_deaths", normalized_tag, ".pdf"),
       width = 14, height = 10, units = "cm")


# Ranking ctries premature deaths
dat_rank = deaths_ctry_sf %>% 
  as.data.frame() %>% 
  dplyr::group_by(ISO3, year) %>% 
  dplyr::summarise(mean_deaths = mean(deaths, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(mean_deaths)
print(tail(dat_rank))
# ISO3  year  mean_deaths
# 1 SVK    2030        354.
# 2 MNE    2030        381.
# 3 HUN    2030        445.
# 4 MKD    2030        447.
# 5 SRB    2030        492.
# 6 BGR    2030        510.

## AP vs urbn_type -------------------------------------------------------------
ap_geo_urbn_type <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, ap)
ap_geo_urbn_type <- unique(ap_geo_urbn_type) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>% 
  dplyr::mutate(quintile = urbn_type) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup()

plot_ap_urbn_type <- prob_jitter_plot(ap_geo_urbn_type %>% 
                                        dplyr::rename(item = ap), 
                                      legend_title = 'Settlement type', 
                                      legend_type = 'urbn_type',
                                      ox_text = 'PM2.5 concentration [ug/m3]')
ggsave(
  file = paste0("figures/plot_ap_urbn_type.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_ap_urbn_type[[1]]
)
ggsave(
  file = paste0("figures/plot_ap_urbn_type.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_urbn_type[[1]] + theme(legend.position = 'none',
                                        panel.border = element_blank()), dpi = 300
)
ggsave(
  file = paste0("figures/plot_ap_urbn_type_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_urbn_type[[2]] + theme(legend.position = 'none',
                                        panel.border = element_blank()), dpi = 300
)
leg = ggpubr::get_legend(plot_ap_urbn_type[[1]] + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_urbn_type.RData"))

leg = ggpubr::get_legend(plot_ap_urbn_type[[1]] + theme(
  legend.direction = 'vertical',
  legend.position = 'right',
  legend.key.size = unit(0.75, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+3))
)
save(leg, file = paste0("figures/legend_urbn_type_v.RData"))


## AP vs DEATHS SCATTERPLOT  ---------------------------------------------------
deaths_geo_urbn <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, deaths, income)
deaths_geo_urbn <- unique(deaths_geo_urbn)

ap_geo_urbn <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, ap, income)
ap_geo_urbn <- unique(ap_geo_urbn)

ap_deaths_nuts3 <- merge(ap_geo_urbn, deaths_geo_urbn) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::filter(deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(CNTR_CODE, geo),
                   by = "geo"
  )

# calculate mean values for each quintile
quintile_means <- ap_deaths_nuts3 %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(mean_ap = mean(ap, na.rm = TRUE), 
                   mean_deaths = mean(deaths, na.rm = TRUE))

# define an offset for the reference points
offset_x1 <- min(ap_deaths_nuts3$deaths, na.rm = TRUE) - 0.1 * diff(range(ap_deaths_nuts3$deaths))
offset_y1 <- min(ap_deaths_nuts3$ap, na.rm = TRUE) - 0.1 * diff(range(ap_deaths_nuts3$ap))

# define the start and end point of the segments
a1 <- min(quintile_means$mean_deaths, na.rm = TRUE) -4 * scl  # start of vertical line
b1 <- max(quintile_means$mean_deaths, na.rm = TRUE) +4 * scl  # end of vertical line
c1 <- min(quintile_means$mean_ap, na.rm = TRUE) -0.10 * scl     # start of horizontal line
d1 <- max(quintile_means$mean_ap, na.rm = TRUE) +0.10 * scl     # end of horizontal line


plot_deaths_ap <- ggplot() +
  geom_segment(data = quintile_means[1,], 
               aes(x = a1, xend = b1, y = offset_y1, yend = offset_y1), 
               linetype = "dashed", linewidth = 0.8, alpha = 0.75, color = 'gray60') +
  geom_segment(data = quintile_means[1,], 
               aes(x = offset_x1, xend = offset_x1, y = c1, yend = d1), 
               linetype = "dashed", linewidth = 0.8, alpha = 0.75, color = 'gray60') +
  geom_point(data = ap_deaths_nuts3, 
             aes(x = deaths, y = ap, color = quintile, shape = urbn_type),
             alpha = 0.75, size = 2) +
  geom_point(data = quintile_means, aes(x = offset_x1, y = mean_ap, color = quintile), 
             size = 3, shape = 18) +
  geom_point(data = quintile_means, aes(x = mean_deaths, y = offset_y1, color = quintile), 
             size = 3, shape = 18) + 
  scale_color_manual(
    values = quintiles.color,
    name = "Income quintiles",
    labels = quintiles.labs
  ) +
  theme_minimal() +
  labs(x = "Premature deaths [deaths per 1M inhabitants]", 
       y = "PM2.5 concentration [µg/m³]",
       color = "Income quintiles", 
       shape = "Settlement Type") +
  expand_limits(x = offset_x1, y = offset_y1) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.border = element_blank(),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.text = element_text(size = legend.text.size),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = legend.text.size+2),
    legend.text = element_text(size = legend.text.size)
  ) + coord_flip()



ggsave(
  file = paste0("figures/plot_deaths_ap", normalized_tag, ".pdf"), height = 15, width = 20, units = "cm",
  plot = plot_deaths_ap
)



## AP vs INCOME -------------------------------------------------------------
ap_geo_income <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, income, ap)
ap_geo_income <- unique(ap_geo_income) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup()

plot_ap_income <- prob_jitter_plot(ap_geo_income %>%
                                     dplyr::rename(item = ap), 
                                   legend_title = 'Income quintiles', 
                                   legend_type = 'quintiles',
                                   ox_text = 'PM2.5 concentration [ug/m3]')

ggsave(
  file = paste0("figures/plot_ap_income.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_ap_income[[1]]
)
ggsave(
  file = paste0("figures/plot_ap_income.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_income[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_ap_income_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_income[[2]] + theme(legend.position = 'none'), dpi = 300
)
leg = ggpubr::get_legend(plot_ap_income[[1]] + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_income.RData"))

legend_title = 'Income quintiles'
legend_type = 'quintiles_v'
legend_color = paste0(legend_type, '.color')
legend_labs = paste0(legend_type, '.labs')
leg = ggpubr::get_legend(plot_ap_income[[1]] + 
                           scale_color_manual(
                             values = get(legend_color),
                             name = legend_title,
                             labels = get(legend_labs)
                           ) +
                           scale_fill_manual(
                             values = get(legend_color),
                             name = legend_title,
                             labels = get(legend_labs)
                           ) +
                           guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
                           theme(
                             legend.direction = 'vertical',
                             legend.position = 'right',
                             legend.key.size = unit(0.75, "cm"),
                             legend.title = element_text(size = legend.text.size+5),
                             legend.text = element_text(size = legend.text.size+3))
)
save(leg, file = paste0("figures/legend_income_v.RData"))

ap_geo_income %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_income = min(income, na.rm = TRUE),
    max_income = max(income, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE),
    median_income = median(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_income max_income mean_income median_income sd_income     n
# <fct>         <dbl>      <dbl>       <dbl>         <dbl>     <dbl> <int>
# 1 1             4348.     11243.       9120.         9466.     1555.   236
# 2 2            11245.     15550.      13408.        13107.     1430.   236
# 3 3            15555.     17322.      16443.        16421.      510.   236
# 4 4            17338.     19467.      18424.        18490.      641.   236
# 5 5            19470.     31864.      21229.        20797.     1827.   235


## AP vs GINI -------------------------------------------------------------
ap_geo_gini <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, gini, ap)
ap_geo_gini <- unique(ap_geo_gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup()

plot_ap_gini <- prob_jitter_plot(ap_geo_gini %>% 
                                   dplyr::rename(item = ap), 
                                 legend_title = 'Gini quintiles', 
                                 legend_type = 'quintiles_v3',
                                 ox_text = 'PM2.5 concentration [ug/m3]')
ggsave(
  file = paste0("figures/plot_ap_gini.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_ap_gini[[1]]
)
ggsave(
  file = paste0("figures/plot_ap_gini.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_gini[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_ap_gini_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_gini[[2]] + theme(legend.position = 'none'), dpi = 300
)

leg = ggpubr::get_legend(plot_ap_gini[[1]] + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_gini.RData"))

legend_title = 'Gini quintiles'
legend_type = 'quintiles_v3'
legend_color = paste0(legend_type, '.color')
legend_labs = paste0(legend_type, '.labs')
leg = ggpubr::get_legend(plot_ap_gini[[1]] + 
                           scale_color_manual(
                             values = get(legend_color),
                             name = legend_title,
                             labels = get(legend_labs)
                           ) +
                           scale_fill_manual(
                             values = get(legend_color),
                             name = legend_title,
                             labels = get(legend_labs)
                           ) +
                           guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
                           theme(
                             legend.direction = 'vertical',
                             legend.position = 'right',
                             legend.key.size = unit(0.75, "cm"),
                             legend.title = element_text(size = legend.text.size+5),
                             legend.text = element_text(size = legend.text.size+3))
)
save(leg, file = paste0("figures/legend_gini_v.RData"))


ap_geo_gini %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_gini = min(gini, na.rm = TRUE),
    max_gini = max(gini, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    median_gini = median(gini, na.rm = TRUE),
    sd_gini = sd(gini, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_gini max_gini mean_gini median_gini sd_gini     n
# <fct>       <dbl>    <dbl>     <dbl>       <dbl>   <dbl> <int>
# 1 1           0.204    0.285     0.269       0.271 0.0137    212
# 2 2           0.285    0.298     0.291       0.29  0.00357   212
# 3 3           0.298    0.312     0.305       0.306 0.00395   212
# 4 4           0.312    0.348     0.327       0.326 0.0113    212
# 5 5           0.348    0.553     0.387       0.380 0.0403    211



## AP vs ELDERLY  ----------------------------------------------------------
ap_geo_per_elderly <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, per_elderly, ap)
ap_geo_per_elderly <- unique(ap_geo_per_elderly) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup()


plot_ap_per_elderly <- prob_jitter_plot(ap_geo_per_elderly %>% 
                                          dplyr::rename(item = ap), 
                                        legend_title = 'Elderly proportion\nquintiles', 
                                        legend_type = 'quintiles_v2',
                                        ox_text = 'PM2.5 concentration [ug/m3]')
ggsave(
  file = paste0("figures/plot_ap_per_elderly.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_ap_per_elderly[[1]]
)
ggsave(
  file = paste0("figures/plot_ap_per_elderly.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_per_elderly[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_ap_per_elderly_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_ap_per_elderly[[2]] + theme(legend.position = 'none'), dpi = 300
)

leg = ggpubr::get_legend(plot_ap_per_elderly[[1]] + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_per_elderly.RData"))

legend_title = 'Elderly proportion\nquintiles'
legend_type = 'quintiles_v2'
legend_color = paste0(legend_type, '.color')
legend_labs = paste0(legend_type, '.labs')
leg = ggpubr::get_legend(plot_ap_per_elderly[[1]] + 
                           scale_color_manual(
                             values = get(legend_color),
                             name = legend_title,
                             labels = get(legend_labs)
                           ) +
                           scale_fill_manual(
                             values = get(legend_color),
                             name = legend_title,
                             labels = get(legend_labs)
                           ) +
                           guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
                           theme(
                             legend.direction = 'vertical',
                             legend.position = 'right',
                             legend.key.size = unit(0.75, "cm"),
                             legend.title = element_text(size = legend.text.size+5),
                             legend.text = element_text(size = legend.text.size+3))
)
save(leg, file = paste0("figures/legend_per_elderly_v.RData"))

ap_geo_per_elderly %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_per_elderly = min(per_elderly, na.rm = TRUE),
    max_per_elderly = max(per_elderly, na.rm = TRUE),
    mean_per_elderly = mean(per_elderly, na.rm = TRUE),
    median_per_elderly = median(per_elderly, na.rm = TRUE),
    sd_per_elderly = sd(per_elderly, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly     n
# <fct>              <dbl>           <dbl>            <dbl>              <dbl>          <dbl> <int>
# 1 1                 0.0586           0.182            0.157              0.164        0.0223    280
# 2 2                 0.182            0.206            0.195              0.195        0.00649   280
# 3 3                 0.206            0.224            0.215              0.215        0.00525   280
# 4 4                 0.224            0.246            0.234              0.234        0.00608   279
# 5 5                 0.246            0.354            0.267              0.262        0.0192    279



## Deaths vs URBN_TYPE -------------------------------------------------------------
spacing_factor = 0.5; scl = 25

deaths_geo_urbn_type <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, deaths)
deaths_geo_urbn_type <- unique(deaths_geo_urbn_type) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths > 0.15e+2) %>% 
  dplyr::mutate(quintile = urbn_type) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()


plot_deaths_urbn_type <- prob_jitter_plot(deaths_geo_urbn_type %>% 
                                            dplyr::rename(item = deaths) %>% 
                                            dplyr::filter(item > 0.15e+2), 
                                          legend_title = 'Urban type', 
                                          legend_type = 'urbn_type',
                                          ox_text = 'Premature deaths [deaths per 1M inhabitants]')
ggsave(
  file = paste0("figures/plot_deaths_urbn_type.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_urbn_type[[1]]
)
ggsave(
  file = paste0("figures/plot_deaths_urbn_type.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_urbn_type[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_deaths_urbn_type_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_urbn_type[[2]] + theme(legend.position = 'none'), dpi = 300
)

deaths_geo_urbn_type %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_deaths = min(deaths, na.rm = TRUE),
    max_deaths = max(deaths, na.rm = TRUE),
    mean_deaths = mean(deaths, na.rm = TRUE),
    median_deaths = median(deaths, na.rm = TRUE),
    sd_deaths = sd(deaths, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile    min_deaths max_deaths mean_deaths median_deaths sd_deaths     n
# <fct>            <dbl>      <dbl>       <dbl>         <dbl>     <dbl> <int>
# 1 City              76.8       515.        247.          254.      68.5   368
# 2 Town/Suburb       20.1       610.        276.          275.      98.7   574
# 3 Rural             15.3       624.        252.          261.      91.5   411




## Deaths vs GINI -------------------------------------------------------------
deaths_geo_gini <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, gini, deaths)
deaths_geo_gini <- unique(deaths_geo_gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths > 0.15e+2) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()


plot_deaths_gini <- prob_jitter_plot(deaths_geo_gini %>% 
                                       dplyr::rename(item = deaths) %>% 
                                       dplyr::filter(item > 0.15e+2), 
                                     legend_title = 'Gini quintiles', 
                                     legend_type = 'quintiles_v3',
                                     ox_text = 'Premature deaths [deaths per 1M inhabitants]')
ggsave(
  file = paste0("figures/plot_deaths_gini.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_gini[[1]]
)
ggsave(
  file = paste0("figures/plot_deaths_gini.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_gini[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_deaths_gini_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_gini[[2]] + theme(legend.position = 'none'), dpi = 300
)


deaths_geo_gini %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_gini = min(gini, na.rm = TRUE),
    max_gini = max(gini, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    median_gini = median(gini, na.rm = TRUE),
    sd_gini = sd(gini, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_gini max_gini mean_gini median_gini sd_gini     n
# <fct>       <dbl>    <dbl>     <dbl>       <dbl>   <dbl> <int>
# 1 1           0.204    0.287     0.271       0.273 0.0128    205
# 2 2           0.287    0.3       0.293       0.292 0.00374   205
# 3 3           0.3      0.313     0.306       0.307 0.00391   204
# 4 4           0.313    0.348     0.329       0.329 0.0113    204
# 5 5           0.348    0.553     0.388       0.381 0.0403    204




## Deaths vs INCOME -------------------------------------------------------------
deaths_geo_income <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, income, deaths)
deaths_geo_income <- unique(deaths_geo_income) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths > 0.15e+2) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()

plot_deaths_income <- prob_jitter_plot(deaths_geo_income %>% 
                                         dplyr::rename(item = deaths) %>% 
                                         dplyr::filter(item > 0.15e+2), 
                                       legend_title = 'Income quintiles', 
                                       legend_type = 'quintiles',
                                       ox_text = 'Premature deaths [deaths per 1M inhabitants]')

ggsave(
  file = paste0("figures/plot_deaths_income.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_income[[1]]
)
ggsave(
  file = paste0("figures/plot_deaths_income.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_income[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_deaths_income_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_income[[2]] + theme(legend.position = 'none'), dpi = 300
)

deaths_geo_income %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_income = min(income, na.rm = TRUE),
    max_income = max(income, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE),
    median_income = median(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_income max_income mean_income median_income sd_income     n
# <fct>         <dbl>      <dbl>       <dbl>         <dbl>     <dbl> <int>
# 1 1             4348.     11168.       9085.         9466.     1555.   228
# 2 2            11195.     15666.      13427.        13073.     1522.   228
# 3 3            15678.     17393.      16521.        16540.      501.   228
# 4 4            17397.     19524.      18509.        18563.      626.   228
# 5 5            19530.     31864.      21287.        20920.     1832.   227




## Deaths vs ELDERLY -------------------------------------------------------------
deaths_geo_per_elderly <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, per_elderly, deaths)
deaths_geo_per_elderly <- unique(deaths_geo_per_elderly) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths > 0.15e+2) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()


plot_deaths_per_elderly <- prob_jitter_plot(deaths_geo_per_elderly %>% 
                                              dplyr::rename(item = deaths) %>% 
                                              dplyr::filter(item > 0.15e+2), 
                                            legend_title = 'Elderly proportion\nquintiles', 
                                            legend_type = 'quintiles_v2',
                                            ox_text = 'Premature deaths [deaths per 1M inhabitants]')
ggsave(
  file = paste0("figures/plot_deaths_per_elderly.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_per_elderly[[1]]
)
ggsave(
  file = paste0("figures/plot_deaths_per_elderly.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_per_elderly[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_deaths_per_elderly_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_per_elderly[[2]] + theme(legend.position = 'none'), dpi = 300
)

deaths_geo_per_elderly %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_per_elderly = min(per_elderly, na.rm = TRUE),
    max_per_elderly = max(per_elderly, na.rm = TRUE),
    mean_per_elderly = mean(per_elderly, na.rm = TRUE),
    median_per_elderly = median(per_elderly, na.rm = TRUE),
    sd_per_elderly = sd(per_elderly, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly     n
# <fct>              <dbl>           <dbl>            <dbl>              <dbl>          <dbl> <int>
# 1 1                 0.0586           0.182            0.157              0.165        0.0226    271
# 2 2                 0.182            0.206            0.195              0.195        0.00643   271
# 3 3                 0.206            0.224            0.215              0.215        0.00515   271
# 4 4                 0.224            0.245            0.234              0.234        0.00611   270
# 5 5                 0.245            0.354            0.267              0.261        0.0194    270


## GRID AP vs DEATHS SCATTERPLOT  ---------------------------------------------------
rr_inc <- load_grid_data_inc(type = 'ap')
rr_urb <- load_grid_data_urb(type = 'ap')

ap_values <- rr_inc[[1]]
inc_values <- rr_inc[[2]]
urb_values <- rr_urb[[2]]

rr_mort <- terra::resample(pm.mort_raster2, pm.ap_raster2)
rr_mort <- terra::crop(rr_mort, extent_raster)
# Filter out NA values directly on the rasters
rr_mort_filtered <- terra::mask(rr_mort, rr_mort, maskvalue = NA)
# Convert the filtered rasters to data frames
mort_values <- terra::values(rr_mort_filtered)


# Remove NA values
valid_idx <- !is.na(ap_values) & !is.na(inc_values) & !is.na(urb_values) & !is.na(mort_values)
df_scatter <- data.frame(pm_concentration = ap_values[valid_idx], 
                         pm_deaths = mort_values[valid_idx],
                         urbn_type = urb_values[valid_idx],
                         inc_per_capita = inc_values[valid_idx],
                         ctry_names = ctry_values[valid_idx])

df_scatter_no0 <- df_scatter[df_scatter$pm_concentration > 0,]
df_scatter_no0 <- df_scatter_no0[df_scatter_no0$pm_deaths > 0.15e-4,]
df_scatter_no0 <- df_scatter_no0[df_scatter_no0$urbn_type > 0,]
df_scatter_no0 <- df_scatter_no0[df_scatter_no0$inc_per_capita > 0,]
df_scatter_no0 <- unique(df_scatter_no0) %>% 
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(inc_per_capita, 5))) 

df_scatter_no0_sample <- df_scatter_no0 %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::mutate(urbn_type = dplyr::if_else(urbn_type == 1, 'Rural',
                                           dplyr::if_else(urbn_type == 2, 'Town/\nSuburb',
                                                          dplyr::if_else(urbn_type == 3, 'City', NA)))) %>% 
  dplyr::mutate(urbn_type = forcats::fct_relevel(urbn_type, 'City','Town/\nSuburb','Rural')) 

# calculate mean values for each quintile
binned_data <- df_scatter_no0 %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::group_by(quintile = quintile_5) %>%
  dplyr::summarize(mean_ap = mean(pm_concentration),
                   mean_deaths = mean(pm_deaths)) %>% 
  dplyr::ungroup()

# define an offset for the reference points
offset_x2 <- 0
offset_y2 <- min(binned_data$mean_ap, na.rm = TRUE) - 1.75 * diff(range(binned_data$mean_ap))

# define the start and end point of the segments
a2 <- min(binned_data$mean_deaths, na.rm = TRUE) -5e-6 * scl  # start of vertical line
b2 <- max(binned_data$mean_deaths, na.rm = TRUE) +5e-6 * scl  # end of vertical line
c2 <- min(binned_data$mean_ap, na.rm = TRUE) -0.1 * scl     # start of horizontal line
d2 <- max(binned_data$mean_ap, na.rm = TRUE) +0.1 * scl     # end of horizontal line


plot_deaths_ap_grid <- ggplot() +
  geom_segment(data = binned_data[1,],
               aes(x = a2, xend = b2, y = offset_y2, yend = offset_y2),
               linetype = "dashed", linewidth = 0.8, alpha = 0.75, color = 'gray60') +
  geom_segment(data = binned_data[1,],
               aes(x = offset_x2, xend = offset_x2, y = c2, yend = d2),
               linetype = "dashed", linewidth = 0.8, alpha = 0.75, color = 'gray60') +
  geom_point(data = df_scatter_no0_sample,
             aes(x = pm_deaths, y = pm_concentration, color = quintile_5, shape = urbn_type),
             alpha = 0.75, size = 2) +
  geom_point(data = binned_data, aes(x = offset_x2, y = mean_ap, color = quintile),
             size = 3, shape = 18) +
  geom_point(data = binned_data, aes(x = mean_deaths, y = offset_y2, color = quintile),
             size = 3, shape = 18) +
  scale_color_manual(
    values = quintiles.color,
    name = "Income quintiles",
    labels = quintiles.labs
  ) +
  theme_minimal() +
  labs(x = "Premature mortality rate [per 1M inhabitants, per grid cell]", 
       y = "PM2.5 concentration [µg/m³]",
       color = "Income quintiles", 
       shape = "Settlement Type") +
  expand_limits(x = offset_x2, y = offset_y2) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.border = element_blank(),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.text = element_text(size = legend.text.size),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = legend.text.size+2),
    legend.text = element_text(size = legend.text.size)
  ) + coord_flip()



ggsave(
  file = paste0("figures/plot_deaths_ap", normalized_tag, "_grid.pdf"), height = 15, width = 20, units = "cm",
  plot = plot_deaths_ap_grid
)



## FULL AP vs DEATHS SCATTERPLOT  ---------------------------------------------------

combined_scatterplot <- plot_grid(ggdraw() +
                                    draw_label("a)",
                                               x = 0, hjust = 0, vjust = -23, fontface = "bold", size = 12) +
                                    draw_plot(plot_deaths_ap_grid + theme(legend.position = 'none'), y = 0, height = 1),
                                  ggdraw() +
                                    draw_label("b)",
                                               x = 0, hjust = 0, vjust = -23, fontface = "bold", size = 12) +
                                    draw_plot(plot_deaths_ap + theme(legend.position = 'none'), y = 0, height = 1),
                                  ncol = 1, rel_heights = c(1, 1)
)

combined_scatterplot2 <- cowplot::plot_grid(
  combined_scatterplot,
  cowplot::get_legend(plot_deaths_ap_grid),
  ncol = 2,
  rel_widths = c(1, 0.2)
)

ggsave(
  file = paste0("figures/plot_scatterplot_full", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
  plot = combined_scatterplot2
)



## GRID - AP vs URBN_TYPE ------------------------------------------------------
# data processing --------------------------------------------------------------
rr <- load_grid_data_urb(type = 'ap')

pm_values <- rr[[1]]
urbn_values <- rr[[2]]

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(urbn_values)
df_ap_urbn <- data.frame(pm_concentration = pm_values[valid_idx],
                         urbn_type = urbn_values[valid_idx],
                         ctry_names = ctry_values[valid_idx])

df_ap_urbn_no0 <- df_ap_urbn[df_ap_urbn$pm_concentration > 0,]
df_ap_urbn_no0 <- df_ap_urbn_no0[df_ap_urbn_no0$urbn_type > 0,]
df_ap_urbn_no0 <- unique(df_ap_urbn_no0)

# check nº of settlement cells of Denmark (ctry_value == 8) and Latvia (ctry_value == 22)
df_ap_urbn_no0 %>% 
  dplyr::filter(ctry_names == 8) %>% 
  dplyr::group_by(urbn_type) %>% 
  dplyr::summarise(percentage = 100 * dplyr::n() / nrow(.), .groups = "drop")

df_ap_urbn_no0 %>% 
  dplyr::group_by(ctry_names, urbn_type) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
  dplyr::mutate(percentage = 100 * n / sum(n)) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID')) -> a



# groups and sample
ap_grid_urbn <- unique(df_ap_urbn_no0) %>% 
  dplyr::rename(quintile = urbn_type,
                ap = pm_concentration) %>% 
  dplyr::mutate(quintile = as.factor(quintile)) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup() # 5541267 datapoints
# random 10000 samples (EU+ analysis)
ap_grid_urbn_sample <- ap_grid_urbn %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::mutate(quintile = dplyr::if_else(quintile == 1, 'Rural',
                                          dplyr::if_else(quintile == 2, 'Town/Suburb',
                                                         dplyr::if_else(quintile == 3, 'City', NA)))) %>% 
  dplyr::mutate(quintile = forcats::fct_relevel(quintile, 'City','Town/Suburb','Rural')) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))
# random x% samples by ctry (within ctry analysis)
ap_grid_urbn_sample_ctry <- ap_grid_urbn %>% 
  dplyr::group_by(ctry_names) %>%
  dplyr::slice_sample(prop = sample_ctry_size) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(quintile = dplyr::if_else(quintile == 1, 'Rural',
                                          dplyr::if_else(quintile == 2, 'Town/Suburb',
                                                         dplyr::if_else(quintile == 3, 'City', NA)))) %>% 
  dplyr::mutate(quintile = forcats::fct_relevel(quintile, 'City','Town/Suburb','Rural')) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))


# plots  -----------------------------------------------------------------------
plot_grid_ap_urbn <- prob_jitter_plot(data = ap_grid_urbn_sample %>% 
                                        dplyr::rename(item = ap), 
                                      legend_title = 'Urban type', 
                                      legend_type = 'urbn_type',
                                      ox_text = 'PM2.5 concentration [ug/m3]')

ggsave(
  file = paste0("figures/plot_grid_ap_urbn.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_urbn[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_ap_urbn.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_urbn[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_ap_urbn_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_urbn[[2]] + theme(legend.position = 'none'), dpi = 300
)

# METHODOLOGY SKETCH FIGURE
ggsave(
  file = paste0("figures/meth_sketch/plot_urbntype_density_ap_grid.pdf"), height = 5, width = 10, units = "cm",
  plot = plot_grid_ap_urbn[[1]] + theme(legend.position = 'none',
                                        panel.border = element_blank(),
                                        axis.title = element_blank()),
  dpi = 300
)
# --

df <- data.table::as.data.table(ap_grid_urbn_sample) %>% 
  dplyr::rename("urbn_type" = "quintile") %>% 
  dplyr::mutate(urbn_type = factor(urbn_type, levels = c('City','Rural','Town/Suburb')))
df_medi <- df[, .(medi = quantile(ap, 0.5, na.rm = T)),
              by = c("urbn_type")
]


plot_urbntype_density <- ggplot(df) +
  geom_density(
    data = df, aes(
      x = ap, group = urbn_type,
      color = urbn_type, fill = urbn_type
    ),
    linewidth = 0.8, alpha = 0.25
  ) +
  geom_vline(aes(color = urbn_type, xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_wrap(. ~ urbn_type, nrow = 3) +
  ggpubr::theme_pubr() +
  scale_color_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  labs(x = "PM2.5 concentration [ug/m3]", y = "Probability density") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
    legend.position = "None"
  )
ggsave(
  file = paste0("figures/plot_grid_ap_urbntype_density.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_urbntype_density
)



## GRID - AP vs INCOME ---------------------------------------------------------
# data processing  -------------------------------------------------------------
rr <- load_grid_data_inc(type = 'ap')

pm_values <- rr[[1]]
inc_values <- rr[[2]]

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(inc_values)
df_ap_inc <- data.frame(pm_concentration = pm_values[valid_idx], 
                        inc_per_capita = inc_values[valid_idx],
                        ctry_names = ctry_values[valid_idx])

df_ap_inc_no0 <- df_ap_inc[df_ap_inc$pm_concentration > 0,]
df_ap_inc_no0 <- df_ap_inc_no0[df_ap_inc_no0$inc_per_capita > 0,]
df_ap_inc_no0 <- unique(df_ap_inc_no0) %>% 
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(inc_per_capita, 5))) 

binned_data <- df_ap_inc_no0 %>%
  dplyr::mutate(quintile = as.factor(dplyr::ntile(inc_per_capita, 50000))) %>% 
  dplyr::group_by(quintile, quintile_5) %>%
  dplyr::summarize(avg_gdp = mean(inc_per_capita),
                   avg_pm_q = mean(pm_concentration)) %>% 
  dplyr::ungroup()


# quintiles and sample
ap_grid_income <- unique(df_ap_inc_no0) %>% 
  dplyr::rename(income = inc_per_capita,
                quintile = quintile_5,
                ap = pm_concentration) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup() # 5541267 datapoints
# random 10000 samples (EU+ analysis)
ap_grid_income_sample <- ap_grid_income %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))
# random x% samples by ctry (within ctry analysis)
ap_grid_income_sample_ctry <- ap_grid_income %>% 
  dplyr::group_by(ctry_names) %>%
  dplyr::slice_sample(prop = sample_ctry_size) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

# plots ------------------------------------------------------------------------

plot_grid_ap_income <- prob_jitter_plot(ap_grid_income_sample %>% 
                                          dplyr::rename(item = ap), 
                                        legend_title = 'Income quintiles', 
                                        legend_type = 'quintiles',
                                        ox_text = 'PM2.5 concentration [ug/m3]')
ggsave(
  file = paste0("figures/plot_grid_ap_income.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_income[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_ap_income.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_income[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_ap_income_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_income[[2]] + theme(legend.position = 'none'), dpi = 300
)

ap_grid_income %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_income = min(income, na.rm = TRUE),
    max_income = max(income, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE),
    median_income = median(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_income max_income mean_income median_income sd_income       n
# <fct>         <dbl>      <dbl>       <dbl>         <dbl>     <dbl>   <int>
# 1 1             1586.      6016.       4176.         3894.     1000. 1108254
# 2 2             6016.     10013.       8203.         8421.     1221. 1108254
# 3 3            10013.     14098.      11905.        11743.     1254. 1108253
# 4 4            14098.     16526.      15360.        15377.      674. 1108253
# 5 5            16526.    191019.      18791.        18271.     2203. 1108253

df_medi <- df_ap_inc_no0 %>%
  dplyr::group_by(quintile_5) %>%
  dplyr::summarize(medi = mean(pm_concentration)) %>% 
  dplyr::ungroup()

plot_inc_density <- 
  ggplot(df_ap_inc_no0, aes(x = pm_concentration, 
                            color = quintile_5,
                            fill = quintile_5)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(color = quintile_5, xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_grid(quintile_5 ~ .) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Income Quintiles",
    labels = quintiles.labs
  ) +
  scale_color_manual(
    values = quintiles.color,
    name = "Income Quintiles",
    labels = quintiles.labs
  ) +
  labs(
    x = "PM Concentration",
    y = "Density"
  ) +
  ggpubr::theme_pubr() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
  )

ggsave(
  file = paste0("figures/plot_grid_ap_income_density.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_inc_density
)


## GRID - AP vs ELDERLY --------------------------------------------------------
# data processing --------------------------------------------------------------
rr <- load_grid_data_eld(type = 'ap')

pm_values <- rr[[1]]
pop_elderly <- rr[[2]]

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(pop_elderly)
df_ap_eld <- data.frame(pm_concentration = pm_values[valid_idx],
                        pop_elderly_per = pop_elderly[valid_idx],
                        ctry_names = ctry_values[valid_idx])

df_ap_eld_no0 <- df_ap_eld[df_ap_eld$pm_concentration > 0,]
df_ap_eld_no0 <- df_ap_eld_no0[df_ap_eld_no0$pop_elderly_per > 0,]
df_ap_eld_no0 <- unique(df_ap_eld_no0) %>% 
  dplyr::filter(!ctry_names %in% c(0,2,13,24,25,31)) %>% # remove Albania, Bosnia and Herzegovina, 
  # Macedonia, North Macedonia, Montenegro, Serbia, and UK
  # since not present in the elderly data (but
  # border mismatch, so they appears)
  dplyr::filter(rowSums(is.na(.)) == 0, is.finite(pop_elderly_per), pop_elderly_per < 1) %>% 
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(pop_elderly_per, 5))) 

binned_data <- df_ap_eld_no0 %>%
  dplyr::mutate(quintile = as.factor(dplyr::ntile(pop_elderly_per, 50000))) %>% 
  dplyr::group_by(quintile, quintile_5) %>%
  dplyr::summarize(avg_eld = mean(pop_elderly_per),
                   avg_pm_q = mean(pm_concentration)) %>% 
  dplyr::ungroup()


# quintiles and sample
ap_grid_per_elderly <- unique(df_ap_eld_no0) %>% 
  dplyr::rename(per_elderly = pop_elderly_per,
                quintile = quintile_5,
                ap = pm_concentration) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup() # 3519818 datapoints
# random 10000 samples (EU+ analysis)
ap_grid_per_elderly_sample <- ap_grid_per_elderly %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))
# random x% samples by ctry (within ctry analysis)
ap_grid_per_elderly_sample_ctry <- ap_grid_per_elderly %>% 
  dplyr::group_by(ctry_names) %>%
  dplyr::slice_sample(prop = sample_ctry_size) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

# plots ------------------------------------------------------------------------
plot_grid_ap_per_elderly <- prob_jitter_plot(ap_grid_per_elderly_sample %>% 
                                               dplyr::rename(item = ap), 
                                             legend_title = 'Elderly proportion\nquintiles', 
                                             legend_type = 'quintiles_v2',
                                             ox_text = 'PM2.5 concentration [ug/m3]')
ggsave(
  file = paste0("figures/plot_grid_ap_per_elderly.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_per_elderly[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_ap_per_elderly.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_per_elderly[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_ap_per_elderly_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_ap_per_elderly[[2]] + theme(legend.position = 'none'), dpi = 300
)

ap_grid_per_elderly %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_per_elderly = min(per_elderly, na.rm = TRUE),
    max_per_elderly = max(per_elderly, na.rm = TRUE),
    mean_per_elderly = mean(per_elderly, na.rm = TRUE),
    median_per_elderly = median(per_elderly, na.rm = TRUE),
    sd_per_elderly = sd(per_elderly, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly     n
# <fct>       <dbl>    <dbl>     <dbl>       <dbl>   <dbl> <int>
# 1 1           0.0000000251           0.149            0.103              0.113         0.0378 625414
# 2 2           0.149                  0.196            0.174              0.174         0.0135 625413
# 3 3           0.196                  0.241            0.218              0.218         0.0130 625413
# 4 4           0.241                  0.314            0.273              0.271         0.0205 625413
# 5 5           0.314                  1.000            0.449              0.397         0.143  625413



df_medi <- df_ap_eld_no0 %>%
  dplyr::group_by(quintile_5) %>%
  dplyr::summarize(medi = mean(pm_concentration)) %>% 
  dplyr::ungroup()

plot_eld_density <- 
  ggplot(df_ap_eld_no0, aes(x = pm_concentration, 
                            color = quintile_5,
                            fill = quintile_5)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(color = quintile_5, xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_grid(quintile_5 ~ .) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Elderly Quintiles",
    labels = quintiles.labs
  ) +
  scale_color_manual(
    values = quintiles.color,
    name = "Elderly Quintiles",
    labels = quintiles.labs
  ) +
  labs(
    x = "PM Concentration",
    y = "Density"
  ) +
  ggpubr::theme_pubr() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
  )

ggsave(
  file = paste0("figures/plot_grid_ap_elderly_density.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_eld_density
)


## GRID - Deaths vs URBN_TYPE --------------------------------------------------
# data processing --------------------------------------------------------------
rr <- load_grid_data_urb(type = 'deaths')

pm_values <- rr[[1]]
urbn_values <- rr[[2]]

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(urbn_values)
df_mort_urbn <- data.frame(pm_mort = pm_values[valid_idx],
                           urbn_type = urbn_values[valid_idx],
                           ctry_names = ctry_values[valid_idx])

if (normalized) {
  df_mort_urbn_no0 <- df_mort_urbn[df_mort_urbn$pm_mort > 0.15e+2,]
} else {
  df_mort_urbn_no0 <- df_mort_urbn
}
df_mort_urbn_no0 <- df_mort_urbn_no0[df_mort_urbn_no0$urbn_type > 0,]
df_mort_urbn_no0 <- unique(df_mort_urbn_no0) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

df_mort_urbn_no0$urbn_type <- factor(df_mort_urbn_no0$urbn_type, levels = c('3','2','1'))

df_medi <- df_mort_urbn_no0 %>%
  dplyr::group_by(urbn_type, ctry_names) %>%
  dplyr::summarize(medi = median(pm_mort)) %>% 
  dplyr::ungroup()


# groups and sample
deaths_grid_urbn <- unique(df_mort_urbn_no0) %>% 
  dplyr::rename(quintile = urbn_type,
                deaths = pm_mort) %>% 
  dplyr::mutate(quintile = as.factor(quintile)) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup() # 5541267 datapoints
# random 10000 samples (EU+ analysis)
deaths_grid_urbn_sample <- deaths_grid_urbn %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::mutate(quintile = dplyr::if_else(quintile == 1, 'Rural',
                                          dplyr::if_else(quintile == 2, 'Town/Suburb',
                                                         dplyr::if_else(quintile == 3, 'City', NA)))) %>% 
  dplyr::mutate(quintile = forcats::fct_relevel(quintile, 'City','Town/Suburb','Rural')) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))
# random x% samples by ctry (within ctry analysis)
deaths_grid_urbn_sample_ctry <- deaths_grid_urbn %>% 
  dplyr::group_by(ctry_names) %>%
  dplyr::slice_sample(prop = sample_ctry_size) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(quintile = dplyr::if_else(quintile == 1, 'Rural',
                                          dplyr::if_else(quintile == 2, 'Town/Suburb',
                                                         dplyr::if_else(quintile == 3, 'City', NA)))) %>% 
  dplyr::mutate(quintile = forcats::fct_relevel(quintile, 'City','Town/Suburb','Rural')) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

# plots ------------------------------------------------------------------------
plot_grid_deaths_urbn <- prob_jitter_plot(deaths_grid_urbn_sample %>% 
                                            dplyr::rename(item = deaths) %>% 
                                            { 
                                              if (normalized) {
                                                dplyr::filter(., item > 25)
                                              } else{
                                                dplyr::filter(., TRUE)
                                              }
                                            }, 
                                          legend_title = 'Urban type',
                                          legend_type = 'urbn_type',
                                          ox_text = dplyr::if_else(normalized,
                                                                   'Premature mortality rate [per 1M inhabitants, per grid cell]',# per 1km\u00B2 grid cell]',
                                                                   'Premature deaths [absolute number]'))#Deaths per 1km\u00B2 grid cell]'))
plot_grid_deaths_urbn[[1]] <- plot_grid_deaths_urbn[[1]]
plot_grid_deaths_urbn[[2]] <- plot_grid_deaths_urbn[[2]]

ggsave(
  file = paste0("figures/plot_grid_deaths_urbn",norm_grid_tag,".pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_urbn[[1]],
  bg = 'white'
)
ggsave(
  file = paste0("figures/plot_grid_deaths_urbn",norm_grid_tag,".png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_urbn[[1]] + theme(legend.position = 'none'), dpi = 300,
  bg = 'white'
)
ggsave(
  file = paste0("figures/plot_grid_deaths_urbn_text",norm_grid_tag,".png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_urbn[[2]] + theme(legend.position = 'none'), dpi = 300,
  bg = 'white'
)


# ecf by country

df_mort_urbn_no0_ctry <- df_mort_urbn_no0 %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID')) %>% 
  dplyr::mutate(country_name = countrycode::countrycode(ctry_code,
                                                        origin = "iso2c", destination = "country.name"))

pl <- ggplot(df_mort_urbn_no0_ctry, 
             aes(x = pm_mort, color = factor(urbn_type))) +
  stat_ecdf(size = 0.5) +
  facet_wrap(. ~ country_name) +
  scale_color_manual(
    values = urbn_type.color.num,
    name = "Settlement Type",
    labels = urbn_type.labs.num
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA))) +
  scale_x_continuous(labels = sci_formatter) +
  theme_minimal() +
  labs(y = "ECDF",
       x = "Premature mortality rate [per 1M inhabitants, per grid cell]") +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.title.size),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.title.size),
    legend.text = element_text(size = legend.text.size),
    legend.position = 'right'
  )
ggsave(
  file = paste0("figures/plot_grid_mort_urbntype_facetCtry",norm_grid_tag,".pdf"), 
  height = 17, width = 26, units = "cm",
  plot = pl
)


## GRID - Deaths vs INCOME -----------------------------------------------------
# data processing --------------------------------------------------------------
rr <- load_grid_data_inc(type = 'deaths')

pm_values <- rr[[1]]
inc_values <- rr[[2]]

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(inc_values)
df_mort_inc <- data.frame(pm_mort = pm_values[valid_idx], 
                          inc_per_capita = inc_values[valid_idx],
                          ctry_names = ctry_values[valid_idx])

if (normalized) {
  df_mort_inc_no0 <- df_mort_inc[df_mort_inc$pm_mort > 0.15e+2,]
} else {
  df_mort_inc_no0 <- df_mort_inc[df_mort_inc$pm_mort < 0.4,]
}
df_mort_inc_no0 <- df_mort_inc_no0[df_mort_inc_no0$inc_per_capita > 0,]
df_mort_inc_no0 <- unique(df_mort_inc_no0) %>% 
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(inc_per_capita, 5))) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)


binned_data <- df_mort_inc_no0 %>%
  dplyr::mutate(quintile = as.factor(dplyr::ntile(inc_per_capita, 50000))) %>% 
  dplyr::group_by(quintile, quintile_5) %>%
  dplyr::summarize(avg_gdp = mean(inc_per_capita),
                   avg_pm_q = mean(pm_mort)) %>% 
  dplyr::ungroup()



# quintiles and sample
deaths_grid_income <- unique(df_mort_inc_no0) %>% 
  dplyr::rename(income = inc_per_capita,
                quintile = quintile_5,
                deaths = pm_mort) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup() # 5541267 datapoints
# random 10000 samples (EU+ analysis)
deaths_grid_income_sample <- deaths_grid_income %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))
# random x% samples by ctry (within ctry analysis)
deaths_grid_income_sample_ctry <- deaths_grid_income %>% 
  dplyr::group_by(ctry_names) %>%
  dplyr::slice_sample(prop = sample_ctry_size) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))


# plots ------------------------------------------------------------------------
plot_grid_deaths_income <- prob_jitter_plot(deaths_grid_income_sample %>% 
                                              dplyr::rename(item = deaths) %>% 
                                              { 
                                                if (normalized) {
                                                  dplyr::filter(., item > 25)
                                                } else{
                                                  dplyr::filter(., TRUE)
                                                }
                                              }, 
                                            legend_title = 'Income quintiles',
                                            legend_type = 'quintiles',
                                            ox_text = dplyr::if_else(normalized,
                                                                     'Premature mortality rate [per 1M inhabitants, per grid cell]',# per 1km\u00B2 grid cell]',
                                                                     'Premature deaths [absolute number]'))#Deaths per 1km\u00B2 grid cell]'))
ggsave(
  file = paste0("figures/plot_grid_deaths_income",norm_grid_tag,".pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_income[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_deaths_income",norm_grid_tag,".png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_income[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_deaths_income_text",norm_grid_tag,".png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_income[[2]] + theme(legend.position = 'none'), dpi = 300
)


deaths_grid_income %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_income = min(income, na.rm = TRUE),
    max_income = max(income, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE),
    median_income = median(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_income max_income mean_income median_income sd_income      n
# <fct>         <dbl>      <dbl>       <dbl>         <dbl>     <dbl>  <int>
# 1 1             2039.      9109.       7237.         7538.     1397. 838051
# 2 2             9109.     11697.      10334.        10291.      717. 844552
# 3 3            11697.     15018.      13625.        13815.      990. 845630
# 4 4            15018.     16978.      15944.        15923.      549. 846439
# 5 5            16978.    191019.      19263.        18791.     2198. 846448

df_medi <- df_mort_inc_no0 %>%
  dplyr::group_by(quintile_5) %>%
  dplyr::summarize(medi = median(pm_mort)) %>% 
  dplyr::ungroup()

plot_inc_density <- 
  ggplot(data = df_mort_inc_no0 %>% 
           dplyr::filter(pm_mort < 0.05),
         aes(x = pm_mort, 
             color = quintile_5,
             fill = quintile_5)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(color = quintile_5, xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_grid(quintile_5 ~ .) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Income Quintiles",
    labels = quintiles.labs.income.grid
  ) +
  scale_color_manual(
    values = quintiles.color,
    name = "Income Quintiles",
    labels = quintiles.labs.income.grid
  ) +
  labs(
    x = dplyr::if_else(normalized,
                       'Premature deaths [Population-Normalized per 1km\u00B2 grid cell]',
                       'Premature deaths [Deaths per 1km\u00B2 grid cell]'),
    y = "Density"
  ) +
  ggpubr::theme_pubr() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
  )

ggsave(
  file = paste0("figures/plot_grid_income_density",norm_grid_tag,".pdf"), height = 12, width = 18, units = "cm",
  plot = plot_inc_density
)


## GRID - Deaths vs ELDERLY ----------------------------------------------------
# data processing --------------------------------------------------------------
rr <- load_grid_data_eld(type = 'deaths')

pm_values <- rr[[1]]
pop_elderly <- rr[[2]]

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(pop_elderly)
df_mort_eld <- data.frame(pm_mort = pm_values[valid_idx],
                          pop_elderly_per = pop_elderly[valid_idx],
                          ctry_names = ctry_values[valid_idx])

if (normalized) {
  df_mort_eld_no0 <- df_mort_eld[df_mort_eld$pm_mort > 0.15e+2,]
} else {
  df_mort_eld_no0 <- df_mort_eld[df_mort_eld$pm_mort < 0.4,]
}
df_mort_eld_no0 <- df_mort_eld_no0[df_mort_eld_no0$pop_elderly_per > 0,]
df_mort_eld_no0 <- unique(df_mort_eld_no0) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, is.finite(pop_elderly_per), pop_elderly_per < 1) %>% 
  dplyr::filter(!ctry_names %in% c(0,2,13,24,25,31)) %>% # remove Albania, Bosnia and Herzegovina, 
  # Macedonia, North Macedonia, Montenegro, Serbia, and UK
  # since not present in the elderly data (but
  # border mismatch, so they appears)
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(pop_elderly_per, 5))) 

binned_data <- df_mort_eld_no0 %>%
  dplyr::mutate(quintile = as.factor(dplyr::ntile(pop_elderly_per, 50000))) %>% 
  dplyr::group_by(quintile, quintile_5) %>%
  dplyr::summarize(avg_eld = mean(pop_elderly_per),
                   avg_pm_q = mean(pm_mort)) %>% 
  dplyr::ungroup()

# quintiles and sample
deaths_grid_per_elderly <- unique(df_mort_eld_no0) %>% 
  dplyr::rename(per_elderly = pop_elderly_per,
                quintile = quintile_5,
                deaths = pm_mort) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup() # 3519818 datapoints
# random 10000 samples (EU+ analysis)
deaths_grid_per_elderly_sample <- deaths_grid_per_elderly %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))
# random x% samples by ctry (within ctry analysis)
deaths_grid_per_elderly_sample_ctry <- deaths_grid_per_elderly %>% 
  dplyr::group_by(ctry_names) %>%
  dplyr::slice_sample(prop = sample_ctry_size) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

# plots ------------------------------------------------------------------------
plot_grid_deaths_per_elderly <- prob_jitter_plot(deaths_grid_per_elderly_sample %>% 
                                                   dplyr::rename(item = deaths) %>% 
                                                   { 
                                                     if (normalized) {
                                                       dplyr::filter(., item > 25)
                                                     } else{
                                                       dplyr::filter(., TRUE)
                                                     }
                                                   }, 
                                                 legend_title = 'Elderly proportion\nquintiles', 
                                                 legend_type = 'quintiles_v2',
                                                 ox_text = dplyr::if_else(normalized,
                                                                          'Premature mortality rate [per 1M inhabitants, per grid cell]',# per 1km\u00B2 grid cell]',
                                                                          'Premature deaths [absolute number]'))#Deaths per 1km\u00B2 grid cell]'))
ggsave(
  file = paste0("figures/plot_grid_deaths_per_elderly",norm_grid_tag,".pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_per_elderly[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_deaths_per_elderly",norm_grid_tag,".png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_per_elderly[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_deaths_per_elderly_text",norm_grid_tag,".png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_per_elderly[[2]] + theme(legend.position = 'none'), dpi = 300
)


deaths_grid_per_elderly %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_per_elderly = min(per_elderly, na.rm = TRUE),
    max_per_elderly = max(per_elderly, na.rm = TRUE),
    mean_per_elderly = mean(per_elderly, na.rm = TRUE),
    median_per_elderly = median(per_elderly, na.rm = TRUE),
    sd_per_elderly = sd(per_elderly, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly      n
# <fct>              <dbl>           <dbl>            <dbl>              <dbl>          <dbl>  <int>
# 1 1           0.0000000251           0.150            0.104              0.114         0.0370 578779
# 2 2           0.150                  0.195            0.173              0.174         0.0130 578779
# 3 3           0.195                  0.238            0.216              0.216         0.0124 578779
# 4 4           0.238                  0.305            0.268              0.266         0.0188 578779
# 5 5           0.305                  1.000            0.425              0.378         0.130  578779

df_medi <- df_mort_eld_no0 %>%
  dplyr::group_by(quintile_5) %>%
  dplyr::summarize(medi = median(pm_mort)) %>% 
  dplyr::ungroup()

plot_eld_density <- 
  ggplot(df_mort_eld_no0 %>% 
           dplyr::filter(pm_mort < 0.05),
         aes(x = pm_mort, 
             color = quintile_5,
             fill = quintile_5)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(color = quintile_5, xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_grid(quintile_5 ~ .) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Elderly Quintiles",
    labels = quintiles.labs.per_elderly.grid
  ) +
  scale_color_manual(
    values = quintiles.color,
    name = "Elderly Quintiles",
    labels = quintiles.labs.per_elderly.grid
  ) +
  labs(
    x = dplyr::if_else(normalized,
                       'Premature deaths [Population-Normalized per 1km\u00B2 grid cell]',
                       'Premature deaths [Deaths per 1km\u00B2 grid cell]'),
    y = "Density"
  ) +
  ggpubr::theme_pubr() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
  )

ggsave(
  file = paste0("figures/plot_grid_mort_elderly_density",norm_grid_tag,".pdf"), height = 12, width = 18, units = "cm",
  plot = plot_eld_density
)




## COMBINED PLOTS by SOCIOECONOMIC category ------------------------------------

# URBN TYPE
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_urbn_type.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_urbn.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_urbn_type.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_urbn",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, grob_c, grob_d, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_urbn_type.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_URBNTYPE.pdf')), 
       plot = pl, height = 12, width = 18)


grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_urbn_type_text.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_urbn_text.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_urbn_type_text.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_urbn_text",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, grob_c, grob_d, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_urbn_type.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_URBNTYPE_text.png')), 
       plot = pl, height = 12, width = 18)


# INCOME
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_income.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_income.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_income.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_income",norm_grid_tag,".png"))),
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, grob_c, grob_d, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_income.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_INCOME.pdf')), 
       plot = pl, height = 12, width = 18)


grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_income_text.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_income_text.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_income_text.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_income_text",norm_grid_tag,".png"))),
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, grob_c, grob_d, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_income.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_INCOME_text.png')), 
       plot = pl, height = 12, width = 18)


# ELDERLY
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_per_elderly.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_per_elderly.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_per_elderly.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_per_elderly",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, grob_c, grob_d, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_per_elderly.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_ELDERLY.pdf')), 
       plot = pl, height = 12, width = 18)


grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_per_elderly_text.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_per_elderly_text.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_per_elderly_text.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_per_elderly_text",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, grob_c, grob_d, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_per_elderly.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_ELDERLY_text.png')), 
       plot = pl, height = 12, width = 18)

# GINI
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_gini.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_gini.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_gini.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_GINI.pdf')), 
       plot = pl, height = 6, width = 18)


grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_gini_text.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_gini_text.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))

pl <- gridExtra::grid.arrange(grob_a, grob_b, ncol = 2)
legend_centered <- gridExtra::arrangeGrob(get(load('figures/legend_gini.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

pl <- gridExtra::arrangeGrob(
  pl, legend_centered, ncol = 1,
  heights = grid::unit.c(unit(1, "npc") - unit(2, "cm"), unit(2, "cm"))
)

ggsave(file=file.path(paste0('figures/plot_combined_GINI_text.png')), 
       plot = pl, height = 6, width = 18)



## COMBINED PLOTS by AGGREGATION level -----------------------------------------

# GRID
# settlement
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_urbn.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_urbn",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_urbn <- gridExtra::arrangeGrob(get(load('figures/legend_urbn_type_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# income
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_income.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_income",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_income <- gridExtra::arrangeGrob(get(load('figures/legend_income_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# elderly
grob_e <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_per_elderly.png")), 
                                 top = grid::textGrob("e)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_f <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_per_elderly",norm_grid_tag,".png"))), 
                                 top = grid::textGrob("f)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_elderly <- gridExtra::arrangeGrob(get(load('figures/legend_per_elderly_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

sp = 0.15
ss = 0.25

vp_small <- grid::viewport(width = 0.01, height = 0.01)

legend_urbn_grob <- grid::grobTree(legend_urbn, vp = vp_small)
legend_income_grob <- grid::grobTree(legend_income, vp = vp_small)
legend_elderly_grob <- grid::grobTree(legend_elderly, vp = vp_small)

row1 <- gridExtra::arrangeGrob(
  grob_a, grob_b, grid::nullGrob(), legend_urbn_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_urbn_grob))
)
row2 <- gridExtra::arrangeGrob(
  grob_c, grob_d, grid::nullGrob(), legend_income_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_income_grob))
)
row3 <- gridExtra::arrangeGrob(
  grob_e, grob_f, grid::nullGrob(), legend_elderly_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_elderly_grob))
)
pl_combined <- gridExtra::grid.arrange(row1, row2, row3, ncol = 1)

ggsave(file=file.path(paste0('figures/plot_combined_GRID',norm_grid_tag,'.pdf')), 
       plot = pl_combined, height = 15, width = 18)


# GRID - NO NORM (SI)
# settlement
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_urbn.png"))), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_urbn <- gridExtra::arrangeGrob(get(load('figures/legend_urbn_type_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# income
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_income.png"))), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_income <- gridExtra::arrangeGrob(get(load('figures/legend_income_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# elderly
grob_f <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG(paste0("figures/plot_grid_deaths_per_elderly.png"))), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_elderly <- gridExtra::arrangeGrob(get(load('figures/legend_per_elderly_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

sp = 0.15
ss = 0.25

vp_small <- grid::viewport(width = 0.01, height = 0.01)

legend_urbn_grob <- grid::grobTree(legend_urbn, vp = vp_small)
legend_income_grob <- grid::grobTree(legend_income, vp = vp_small)
legend_elderly_grob <- grid::grobTree(legend_elderly, vp = vp_small)

row1 <- gridExtra::arrangeGrob(
  grob_b, grid::nullGrob(), legend_urbn_grob,
  ncol = 3,
  widths = grid::unit.c(unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_urbn_grob))
)
row2 <- gridExtra::arrangeGrob(
  grob_d, grid::nullGrob(), legend_income_grob,
  ncol = 3,
  widths = grid::unit.c(unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_income_grob))
)
row3 <- gridExtra::arrangeGrob(
  grob_f, grid::nullGrob(), legend_elderly_grob,
  ncol = 3,
  widths = grid::unit.c(unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_elderly_grob))
)
pl_combined <- gridExtra::grid.arrange(row1, row2, row3, ncol = 1)

ggsave(file=file.path(paste0('figures/plot_combined_GRID_noNorm.pdf')), 
       plot = pl_combined, height = 13, width = 12)

# NUTS3
# settlement
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_urbn_type.png")), 
                                 top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_urbn_type.png")), 
                                 top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_urbn <- gridExtra::arrangeGrob(get(load('figures/legend_urbn_type_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# income
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_income.png")), 
                                 top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_income.png")), 
                                 top = grid::textGrob("d)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_income <- gridExtra::arrangeGrob(get(load('figures/legend_income_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# elderly
grob_e <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_per_elderly.png")), 
                                 top = grid::textGrob("e)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_f <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_per_elderly.png")), 
                                 top = grid::textGrob("f)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_elderly <- gridExtra::arrangeGrob(get(load('figures/legend_per_elderly_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))
# gini
grob_g <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_gini.png")), 
                                 top = grid::textGrob("g)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_h <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_gini.png")), 
                                 top = grid::textGrob("h)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
legend_gini <- gridExtra::arrangeGrob(get(load('figures/legend_gini_v.RData')), top = NULL, bottom = NULL, left = NULL, right = NULL, padding = unit(0, "line"))

sp = 0.15
ss = 0.25

vp_small <- grid::viewport(width = 0.01, height = 0.01)

legend_urbn_grob <- grid::grobTree(legend_urbn, vp = vp_small)
legend_income_grob <- grid::grobTree(legend_income, vp = vp_small)
legend_elderly_grob <- grid::grobTree(legend_elderly, vp = vp_small)
legend_gini_grob <- grid::grobTree(legend_gini, vp = vp_small)

row1 <- gridExtra::arrangeGrob(
  grob_a, grob_b, grid::nullGrob(), legend_urbn_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_urbn_grob))
)
row2 <- gridExtra::arrangeGrob(
  grob_c, grob_d, grid::nullGrob(), legend_income_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_income_grob))
)
row3 <- gridExtra::arrangeGrob(
  grob_e, grob_f, grid::nullGrob(), legend_elderly_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_elderly_grob))
)
row4 <- gridExtra::arrangeGrob(
  grob_g, grob_h, grid::nullGrob(), legend_gini_grob,
  ncol = 4,
  widths = grid::unit.c(unit(1, "null"), unit(1, "null"), unit(sp, "cm"), unit(ss, "grobwidth", legend_gini_grob))
)
pl_combined <- gridExtra::grid.arrange(row1, row2, row3, row4, ncol = 1)

ggsave(file=file.path(paste0('figures/plot_combined_NUTS3.pdf')), 
       plot = pl_combined, height = 20, width = 18)




######################################################################## MACHINE LEARNING ####

## AP vs INCOME ----------------------------------------------------------------
ap_income_medi <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(ctry, income, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()

data <- ap_income_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 8, 'withinCtry/ml_income_nuts3',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]",type = 'ap')


## AP vs ELDERLY --------------------------------------------------------------
ap_elderly_medi <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(ctry, per_elderly, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()

data <- ap_elderly_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 2, 'withinCtry/ml_elderly_nuts3',
          fig_legend = "Elderly\nproportion\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]", type = 'ap')  


## AP vs GINI ------------------------------------------------------------------
ap_gini_medi <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(ctry, gini, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()

data <- ap_gini_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 4, 'withinCtry/ml_gini_nuts3',
          fig_legend = "Gini\nindex\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]", type = 'ap')  


## AP vs URBN TYPE -------------------------------------------------------------
ap_urbntype_medi <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(ctry, urbn_type, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(urbn_type, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()


## AP - figure -----------------------------------------------------------------
data_list <- list(
  ap_income_medi %>%
    dplyr::rename_with(~ "income", contains("medi")),
  
  ap_gini_medi %>%
    dplyr::rename_with(~ "gini", contains("medi")),
  
  ap_elderly_medi %>%
    dplyr::rename_with(~ "elderly", contains("medi"))
)

# Merge all datasets by quintile
data <- purrr::reduce(data_list, function(x, y) dplyr::full_join(x, y, by = c("quintile","ctry"))) %>%
  dplyr::full_join(ap_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type)) %>% 
  mutate(country_name = dplyr::case_when(
    ctry == "EL" ~ "Greece",
    ctry == "UK" ~ "United Kingdom",
    TRUE ~ countrycode::countrycode(ctry, origin = "iso2c", destination = "country.name")
  )) %>%
  dplyr::filter(country_name != 'Turkey') # not in EU+
data$variable <- factor(data$variable, levels = c("urbn", "income", "elderly", "gini"))


# Plotting
pl <- do_plot_within(data, "PM2.5 concentration [ug/m3]", type = 'nuts3')
ggsave(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,".pdf"), 
  height = 18, width = 18, units = "cm",
  plot = pl
)
save(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,".RData"),
  plot = pl
)



## DEATHS vs INCOME ----------------------------------------------------------------
deaths_income_medi <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(ctry, income, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()

data <- deaths_income_medi %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 2, 'withinCtry/ml_income_nuts3',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "Premature Deaths [deaths per 1M inhabitants]",
          type = 'deaths')


## DEATHS vs ELDERLY --------------------------------------------------------------
deaths_elderly_medi <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(ctry, per_elderly, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()

data <- deaths_elderly_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 2, 'withinCtry/ml_elderly_nuts3',
          fig_legend = "Elderly\nproportion\nquintile",
          fig_ox_label = "Premature Deaths [deaths per 1M inhabitants]",
          type = 'deaths')

## DEATHS vs GINI ------------------------------------------------------------------
deaths_gini_medi <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(ctry, gini, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()

data <- deaths_gini_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 4, 'withinCtry/ml_gini_nuts3',
          fig_legend = "Gini\nindex\nquintile",type = 'deaths',
          fig_ox_label = "Premature Deaths [deaths per 1M inhabitants]")  


## DEATHS vs URBN TYPE -------------------------------------------------------------
deaths_urbntype_medi <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(ctry, urbn_type, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(urbn_type, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()


## DEATHS - figure -----------------------------------------------------------------
data_list <- list(
  deaths_income_medi %>%
    dplyr::rename_with(~ "income", contains("medi")),
  
  deaths_gini_medi %>%
    dplyr::rename_with(~ "gini", contains("medi")),
  
  deaths_elderly_medi %>%
    dplyr::rename_with(~ "elderly", contains("medi"))
)

# Merge all datasets by quintile
data <- purrr::reduce(data_list, function(x, y) dplyr::full_join(x, y, by = c("quintile","ctry"))) %>%
  dplyr::full_join(deaths_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type)) %>% 
  mutate(country_name = dplyr::case_when(
    ctry == "EL" ~ "Greece",
    ctry == "UK" ~ "United Kingdom",
    TRUE ~ countrycode::countrycode(ctry, origin = "iso2c", destination = "country.name")
  )) %>%
  dplyr::filter(country_name != 'Turkey') # not in EU+
data$variable <- factor(data$variable, levels = c("urbn", "income", "elderly", "gini"))

# Plotting
pl <- do_plot_within(data, "Premature Deaths [deaths per 1M inhabitants]", type = 'nuts3')
ggsave(
  file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,".pdf"), 
  height = 18, width = 18, units = "cm",
  plot = pl
)
save(
  file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,".RData"), 
  plot = pl
)


## AP - DEATHS - figure -------------------------------------------------
pl_ap <- get(load(file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,".RData")))
pl_deaths <- get(load(file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,".RData")))

shared_legend <- get_legend(
  pl_ap + theme(legend.position = "right")
)
pl_ap_noleg <- pl_ap + theme(legend.position = "none")
pl_deaths_noleg <- pl_deaths + theme(legend.position = "none")
pl <- plot_grid(
  plot_grid(pl_ap_noleg, pl_deaths_noleg, 
            ncol = 1, labels = c("a)", "b)"), 
            label_x = 0, label_y = 1,
            label_size = 10),
  shared_legend,
  rel_widths = c(1, 0.2),
  ncol = 2
)

# Save
ggsave(paste0("figures/withinCtry/fig_ap_deaths_var_",yy,"_",split_num_tag,"_nuts3.pdf"),
       plot = pl,
       width = 20, height = 25, units = "cm")

## GRID - AP vs INCOME ----------------------------------------------------------------
ap_income_medi <- data.table::as.data.table(ap_grid_income_sample_ctry) %>%
  dplyr::select(ctry = ctry_code, income, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()

data <- ap_income_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 4, 'withinCtry/ml_income_grid',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]",type = 'ap')


## GRID - AP vs ELDERLY --------------------------------------------------------------
ap_elderly_medi <- data.table::as.data.table(ap_grid_per_elderly_sample_ctry) %>%
  dplyr::select(ctry = ctry_code, per_elderly, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()

data <- ap_elderly_medi %>%
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 3, 'withinCtry/ml_elderly_grid',
          fig_legend = "Elderly\nproportion\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]", type = 'ap')  


## GRID - AP vs URBN TYPE -------------------------------------------------------------
ap_urbntype_medi <- data.table::as.data.table(ap_grid_urbn_sample_ctry) %>%
  dplyr::select(ctry = ctry_code, urbn_type = quintile, ap) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>%
  dplyr::group_by(urbn_type, ctry) %>% 
  dplyr::summarise(medi = quantile(ap, 0.50)) %>% 
  dplyr::ungroup()

## GRID - AP - figure -----------------------------------------------------------------
data_list <- list(
  ap_income_medi %>%
    dplyr::rename_with(~ "income", contains("medi")),
  
  ap_elderly_medi %>%
    dplyr::rename_with(~ "elderly", contains("medi"))
)

# Merge all datasets by quintile
data <- purrr::reduce(data_list, function(x, y) dplyr::full_join(x, y, by = c("quintile","ctry"))) %>%
  dplyr::full_join(ap_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type)) %>% 
  dplyr::mutate(country_name = countrycode::countrycode(ctry, origin = "iso2c", destination = "country.name")) %>% 
  dplyr::filter(country_name != 'Turkey') # not in EU+
data$variable <- factor(data$variable, levels = c("urbn", "income", "elderly"))

pl <- do_plot_within(data, "PM2.5 concentration [ug/m3]", type = 'grid_ap')

ggsave(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,"_grid.pdf"), 
  height = 12, width = 18, units = "cm",
  plot = pl
)
save(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,"_grid.RData"),
  plot = pl
)



## GRID - DEATHS vs INCOME ----------------------------------------------------------------
deaths_income_medi <- data.table::as.data.table(deaths_grid_income_sample_ctry) %>%
  dplyr::select(ctry = ctry_code, income, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()

data <- deaths_income_medi %>%
  dplyr::mutate(medi = medi * 100) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 8, 'withinCtry/ml_income_grid',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "Premature mortality rate [per 1M inhabitants, per grid cell]",
          type = 'deaths')

## GRID - DEATHS vs ELDERLY --------------------------------------------------------------
deaths_elderly_medi <- data.table::as.data.table(deaths_grid_per_elderly_sample_ctry) %>%
  dplyr::select(ctry = ctry_code, per_elderly, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintile, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()

data <- deaths_elderly_medi %>%
  dplyr::mutate(medi = medi * 100) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 3, 'withinCtry/ml_elderly_grid',
          fig_legend = "Elderly\nproportion\nquintile",
          fig_ox_label = "Premature mortality rate [per 1M inhabitants, per grid cell]",
          type = 'deaths')


## GRID - DEATHS vs URBN TYPE -------------------------------------------------------------
deaths_urbntype_medi <- data.table::as.data.table(deaths_grid_urbn_sample_ctry) %>%
  dplyr::select(ctry = ctry_code, urbn_type = quintile, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(urbn_type, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(medi = medi * 100) 


## GRID - DEATHS - figure -----------------------------------------------------------------
data_list <- list(
  deaths_income_medi %>%
    dplyr::mutate(medi = medi * 100) %>% 
    dplyr::rename_with(~ "income", contains("medi")),
  
  deaths_elderly_medi %>%
    dplyr::mutate(medi = medi * 100) %>% 
    dplyr::rename_with(~ "elderly", contains("medi"))
)

# Merge all datasets by quintile
data <- purrr::reduce(data_list, function(x, y) dplyr::full_join(x, y, by = c("quintile","ctry"))) %>%
  dplyr::full_join(deaths_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type)) %>% 
  dplyr::mutate(country_name = countrycode::countrycode(ctry, origin = "iso2c", destination = "country.name")) %>% 
  dplyr::filter(country_name != 'Turkey') # not in EU+
data$variable <- factor(data$variable, levels = c("urbn", "income", "elderly"))

# Plotting
pl <- do_plot_within(data, "Premature mortality rate [per 1M inhabitants, per grid cell]", type = 'grid_deaths')
ggsave(
  file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,"_grid.pdf"), height = 12, width = 18, units = "cm",
  plot = pl
)
save(
  file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,"_grid.RData"),
  plot = pl
)


## GRID - AP - DEATHS - figure -------------------------------------------------
pl_ap <- get(load(file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,"_grid.RData")))
pl_deaths <- get(load(file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,"_grid.RData")))

shared_legend <- get_legend(
  pl_ap + theme(legend.position = "right")
)
pl_ap_noleg <- pl_ap + theme(legend.position = "none")
pl_deaths_noleg <- pl_deaths + theme(legend.position = "none")
pl <- plot_grid(
  plot_grid(pl_ap_noleg, pl_deaths_noleg, 
            ncol = 1, labels = c("a)", "b)"), 
            label_x = 0, label_y = 1,
            label_size = 10),
  shared_legend,
  rel_widths = c(1, 0.2),
  ncol = 2
)

# Save
ggsave(paste0("figures/withinCtry/fig_ap_deaths_var_",yy,"_",split_num_tag,"_grid.pdf"),
       plot = pl,
       width = 20, height = 25, units = "cm")

######################################################################## CELLS COUNT #### 

## GRID
urbn_raster2 <- terra::resample(urbn_raster, pm.mort_raster2)
urbn_raster2 <- terra::crop(urbn_raster2, extent_raster)
urbn_raster2 <- terra::mask(urbn_raster2, pm.mort_raster2, maskvalue = NA)
inc_pc_20152 <- terra::project(inc_pc_2015, pm.mort_raster2)
inc_pc_20152 <- terra::resample(inc_pc_20152, pm.mort_raster2)
inc_pc_20152 <- terra::crop(inc_pc_20152, extent_raster)
pop_ge652 <- terra::project(pop_ge65, pm.mort_raster2)
pop_ge652 <- terra::resample(pop_ge652, pm.mort_raster2)
pop_ge652 <- terra::crop(pop_ge652, extent_raster)
pop_t2 <- terra::project(pop_t, pm.mort_raster2)
pop_t2 <- terra::resample(pop_t2, pm.mort_raster2)
pop_t2 <- terra::crop(pop_t2, extent_raster)
pop_elderly <- pop_ge652/pop_t2


# Apply classification
urbn_raster_classified <- terra::app(urbn_raster2, classify_function)
names(urbn_raster_classified) <- "classification_layer"
urbn_raster_combined <- c(urbn_raster2, urbn_raster_classified)


# Filter out NA values directly on the rasters
pm.mort_raster2_filtered <- terra::mask(pm.mort_raster2, pm.mort_raster2, maskvalue = NA)
inc_pc_20152_filtered <- terra::mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)
urbn_raster_filtered <- urbn_raster_combined$classification_layer
urbn_raster_combined_filtered <- terra::mask(urbn_raster_filtered, urbn_raster_filtered, maskvalue = NA)
elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)

# Convert the filtered rasters to data frames
inc_values <- terra::values(inc_pc_20152_filtered)
urbn_values <- terra::values(urbn_raster_combined_filtered)
pop_elderly_values <- terra::values(elderly_raster_filtered)
pm.mort_values <- terra::values(pm.mort_raster2_filtered)

# Remove NA values
valid_idx <- !is.na(inc_values) & !is.na(urbn_values) & !is.na(pop_elderly_values) & !is.na(pm.mort_values)
df_cells <- data.frame(pm.mort = pm.mort_values[valid_idx],
                       inc_per_capita = inc_values[valid_idx],
                       urbn_type = urbn_values[valid_idx],
                       pop_elderly_per = pop_elderly_values[valid_idx],
                       ctry_names = ctry_values[valid_idx])

df_cells_no0 <- df_cells
df_cells_no0 <- df_cells_no0[df_cells_no0$urbn_type > 0,]
df_cells_no0 <- df_cells_no0[df_cells_no0$inc_per_capita > 0,]
df_cells_no0 <- df_cells_no0[df_cells_no0$pop_elderly_per > 0,]
df_cells_no0 <- df_cells_no0[df_cells_no0$pm.mort > 0.15e+2,]

df_cells_no0 <- unique(df_cells_no0) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, is.finite(pop_elderly_per), pop_elderly_per < 1) %>% 
  dplyr::mutate(quintile_inc = as.factor(dplyr::ntile(inc_per_capita, 5))) %>% 
  dplyr::mutate(quintile_eld = as.factor(dplyr::ntile(pop_elderly_per, 5))) %>%  
  dplyr::mutate(urbn_type = dplyr::if_else(urbn_type == 1, 'Rural',
                                           dplyr::if_else(urbn_type == 2, 'Town/Suburb',
                                                          dplyr::if_else(urbn_type == 3, 'City', NA)))) %>% 
  dplyr::mutate(urbn_type = forcats::fct_relevel(urbn_type, 'City','Town/Suburb','Rural'))


## NUTS3
df_cells_nuts3 <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(urbn_type, inc_per_capita = income, pop_elderly_per = per_elderly)
df_cells_nuts3_no0 <- unique(df_cells_nuts3) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, is.finite(pop_elderly_per), pop_elderly_per < 1) %>% 
  dplyr::mutate(quintile_inc = as.factor(dplyr::ntile(inc_per_capita, 5))) %>% 
  dplyr::mutate(quintile_eld = as.factor(dplyr::ntile(pop_elderly_per, 5))) %>%  
  dplyr::mutate(urbn_type = forcats::fct_relevel(urbn_type, 'City','Town/Suburb','Rural'))


## PLOTS
df_cells_urbn_inc_count_grid <- df_cells_no0 %>% 
  dplyr::group_by(quintile = quintile_inc, urbn_type) %>%
  dplyr::summarise(n_grid = dplyr::n(), .groups = "drop") %>% 
  dplyr::group_by(quintile) %>%
  dplyr::mutate(n_grid = 100 * n_grid / sum(n_grid)) %>% 
  dplyr::ungroup()
df_cells_urbn_inc_count_nuts <- df_cells_nuts3_no0 %>% 
  dplyr::group_by(quintile = quintile_inc, urbn_type) %>%
  dplyr::summarise(n_nuts = dplyr::n(), .groups = "drop") %>% 
  dplyr::group_by(quintile) %>%
  dplyr::mutate(n_nuts = 100 * n_nuts / sum(n_nuts)) %>% 
  dplyr::ungroup()
df_cells_urbn_inc_count <- merge(
  df_cells_urbn_inc_count_nuts,
  df_cells_urbn_inc_count_grid,
  by = c('quintile','urbn_type')
) %>% 
  tidyr::pivot_longer(cols = c('n_nuts','n_grid'), names_to = 'agg_level', values_to = 'n') %>% 
  dplyr::mutate(quintile_num = as.numeric(quintile),
                y_offset = dplyr::case_when(
                  agg_level == "n_grid" ~ quintile_num + 0.05,
                  agg_level == "n_nuts" ~ quintile_num - 0.05
                )
  )

pl <- count_cells_plot(df_cells_urbn_inc_count, axis_title = 'Income quintile')
ggsave(
  file = paste0("figures/plot_count_cells_urbn_income.pdf"), height = 13, width = 18, units = "cm",
  plot = pl,
  bg = 'white'
)


df_cells_urbn_eld_count_grid <- df_cells_no0 %>% 
  dplyr::group_by(quintile = quintile_eld, urbn_type) %>%
  dplyr::summarise(n_grid = dplyr::n(), .groups = "drop") %>% 
  dplyr::group_by(quintile) %>%
  dplyr::mutate(n_grid = 100 * n_grid / sum(n_grid)) %>% 
  dplyr::ungroup()
df_cells_urbn_eld_count_nuts <- df_cells_nuts3_no0 %>% 
  dplyr::group_by(quintile = quintile_eld, urbn_type) %>%
  dplyr::summarise(n_nuts = dplyr::n(), .groups = "drop") %>% 
  dplyr::group_by(quintile) %>%
  dplyr::mutate(n_nuts = 100 * n_nuts / sum(n_nuts)) %>% 
  dplyr::ungroup()
df_cells_urbn_eld_count <- merge(
  df_cells_urbn_eld_count_nuts,
  df_cells_urbn_eld_count_grid,
  by = c('quintile','urbn_type')
) %>% 
  tidyr::pivot_longer(cols = c('n_nuts','n_grid'), names_to = 'agg_level', values_to = 'n') %>% 
  dplyr::mutate(quintile_num = as.numeric(quintile),
                y_offset = dplyr::case_when(
                  agg_level == "n_grid" ~ quintile_num + 0.05,
                  agg_level == "n_nuts" ~ quintile_num - 0.05
                )
  )
pl <- count_cells_plot(df_cells_urbn_eld_count, axis_title = 'Elderly proportion quintiles')
ggsave(
  file = paste0("figures/plot_count_cells_urbn_elderly.pdf"), height = 13, width = 18, units = "cm",
  plot = pl,
  bg = 'white'
)


######################################################################## DISTRIBUTION MAPS ####

## MAP settlement type by grid cell -------------------------------------------
urbn_raster_masked <- terra::classify(urbn_raster_combined_filtered,
                                      rcl = matrix(c(0, NA), ncol = 2, byrow = TRUE))
# classification_layer       n
#                    0     176
#                    1 4938521
#                    2   22661
#                    3   70326


pdf(paste0("figures/","plot_grid_urb_quintiles.pdf"), width = 11/2.54, height = 10/2.54)
par(mar = c(0,0,0,0), xpd = NA)
r <- raster::raster(urbn_raster_masked)
r <- raster::mask(r, as(turkey, "Spatial"), inverse = TRUE) # remove Turkey from the map
terra::plot(r, 
            col = rev(urbn_type.color.num),
            breaks = seq(0.5, 3.5, by = 1),
            legend = FALSE, 
            axes = FALSE, 
            box = FALSE)
terra::plot(countries_iso, 
            add = TRUE, 
            border = "black", 
            lwd = 0.10)
legend('bottom',
       legend = rev(urbn_type.labs.num),
       pch = 21,
       pt.bg = rev(urbn_type.color.num),
       pt.cex = 1,
       bty = "n",
       title = "Settlement type",
       text.font = 1,
       cex = legend.title.size.raster-0.2,
       x.intersp = 1, y.intersp = 1.2,
       horiz = TRUE)
dev.off()

## MAP income quintiles by grid cell -------------------------------------------
inc_pc_20152_filtered <- terra::mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)
inc_values <- terra::values(inc_pc_20152_filtered)

quintiles <- dplyr::ntile(inc_values, 5)
quintile_raster <- inc_pc_20152_filtered
terra::values(quintile_raster) <- quintiles
names(quintile_raster) <- "quintile_inc"

do_map_between_socioecon(quintile_raster,
                         quintiles_v.color,
                         quintiles_v.labs,
                         "Income quintile",
                         "plot_grid_inc_quintiles.pdf")


## MAP income quintile in Urban grid cells -------------------------------------
inc_pc_20152_filtered <- terra::mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)
inc_values <- terra::values(inc_pc_20152_filtered)

quintiles <- dplyr::ntile(inc_values, 5)
quintile_raster <- inc_pc_20152_filtered
terra::values(quintile_raster) <- quintiles
names(quintile_raster) <- "quintile_inc"


urbn_mask <- urbn_raster_combined_filtered == 3 # only cities
inc_pc_2015_CITIES_filtered <- terra::mask(quintile_raster, urbn_mask, maskvalues = 0, updatevalue = NA)
names(inc_pc_2015_CITIES_filtered) <- "inc_cities"

do_map_between_socioecon(inc_pc_2015_CITIES_filtered,
                         quintiles_v.color,
                         quintiles_v.labs,
                         "Income quintile",
                         "plot_grid_inc_quintiles_cities.pdf")

## MAP elderly quintiles by grid cell ------------------------------------------
elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)
pop_elderly_values <- terra::values(elderly_raster_filtered)

quintiles <- dplyr::ntile(pop_elderly_values, 5)
quintile_raster <- elderly_raster_filtered
terra::values(quintile_raster) <- quintiles
names(quintile_raster) <- "quintile_eld"

do_map_between_socioecon(quintile_raster,
                         quintiles_v2.color,
                         quintiles_v2.labs,
                         "Elderly proprotion quintile",
                         "plot_grid_eld_quintiles.pdf")

## MAP elderly quintile in Urban grid cells -------------------------------------
elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)
pop_elderly_values <- terra::values(elderly_raster_filtered)

quintiles <- dplyr::ntile(pop_elderly_values, 5)
quintile_raster <- elderly_raster_filtered
terra::values(quintile_raster) <- quintiles
names(quintile_raster) <- "quintile_eld"


urbn_mask <- urbn_raster_combined_filtered == 3 # only cities
elderly_CITIES_filtered <- terra::mask(quintile_raster, urbn_mask, maskvalues = 0, updatevalue = NA)
names(elderly_CITIES_filtered) <- "eld_cities"

do_map_between_socioecon(elderly_CITIES_filtered,
                         quintiles_v2.color,
                         quintiles_v2.labs,
                         "Elderly proprotion quintile",
                         "plot_grid_eld_quintiles_cities.pdf")

## MAP elderly quintile in Rural grid cells -------------------------------------
elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)
pop_elderly_values <- terra::values(elderly_raster_filtered)

quintiles <- dplyr::ntile(pop_elderly_values, 5)
quintile_raster <- elderly_raster_filtered
terra::values(quintile_raster) <- quintiles
names(quintile_raster) <- "quintile_eld"


urbn_mask <- urbn_raster_combined_filtered == 1 # only rural
elderly_RURAL_filtered <- terra::mask(quintile_raster, urbn_mask, maskvalues = 0, updatevalue = NA)
names(elderly_RURAL_filtered) <- "eld_rural"

do_map_between_socioecon(elderly_RURAL_filtered,
                         quintiles_v2.color,
                         quintiles_v2.labs,
                         "Elderly proprotion quintile",
                         "plot_grid_eld_quintiles_rural.pdf")


## MAP within income quintiles by grid cell  ------------------------------------------
raster_filtered <- inc_pc_20152_filtered
names(raster_filtered) = 'value'
do_map_within_socioecon(
  raster_filtered,
  quintiles_v.color,
  quintiles_v.labs,
  'Income quintiles',
  'plot_grid_wihtin_inc_quintiles.pdf'
)
## MAP within elderly quintiles by grid cell  ------------------------------------------
raster_filtered <- elderly_raster_filtered
names(raster_filtered) = 'value'
do_map_within_socioecon(
  raster_filtered,
  quintiles_v2.color,
  quintiles_v2.labs,
  'Elderly proportion quintiles',
  'plot_grid_wihtin_eld_quintiles.pdf'
)


## MAP within AP by grid cell  -------------------------------------------------

# in relation to the median by ctry, which AP deviation from it, percentage-wise,
# does each grid cell suffer?

pm_raster2_europe2 <- terra::crop(pm.ap_raster2_europe, extent_raster)

# Filter out NA values directly on the rasters
pm_raster2_europe2_filtered <- terra::mask(pm_raster2_europe2, pm_raster2_europe2, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- terra::values(pm_raster2_europe2_filtered)
df_ap_ctry <- data.frame(pm_concentration = pm_values, 
                         ctry_names = ctry_values) %>% 
  dplyr::group_by(ctry_code) %>% 
  dplyr::mutate(
    country_median = median(layer, na.rm = TRUE),
    layer_pct_median = ((layer - country_median) / country_median) * 100
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(layer_pct_median = dplyr::if_else(is.na(ctry_code), NA, layer_pct_median))

pm_raster2_europe2_pct <- pm_raster2_europe2_filtered
terra::values(pm_raster2_europe2_pct) = df_ap_ctry$layer_pct_median
pm_raster2_europe2_pct[pm_raster2_europe2_pct  > 100] = 125

# define palette
n_colors <- 125
breaks <- seq(-100, 125, length.out = n_colors)
custom_palette <- colorRampPalette(c("#0C6291", "#E8DAB2", "#FF66CC"))(n_colors)

zero_index <- which.min(abs(breaks))
colors_left <- colorRampPalette(c("#0C6291", "#E8DAB2"))(zero_index)
colors_right <- colorRampPalette(c("#E8DAB2", "#FF66CC"))(n_colors - zero_index + 1)
custom_palette <- c(colors_left, colors_right[-1])

# plot
pdf("figures/plot_grid_wihtin_ap.pdf", width = 11/2.54, height = 10/2.54)
par(mar = c(0,0,0,0))
r <- raster::raster(pm_raster2_europe2_pct)
terra::plot(r, 
            col = custom_palette,
            legend = FALSE, 
            axes = FALSE, 
            box = FALSE)
terra::plot(countries_iso, 
            add = TRUE, 
            border = "black", 
            lwd = 0.10)
legend_ticks <- c(-100, -50, 0, 50, 100, raster::maxValue(r))
legend_labels <- c("-100%", "-50%", "Ctry median", "+50%", "+100%", ">100%")
raster::plot(r, legend.only = TRUE,
             col = custom_palette,
             legend.width = 1,
             legend.shrink = 0.65,
             frame.plot = FALSE,
             axis.args = list(
               at = legend_ticks,
               labels = legend_labels,
               font = 1,
               cex.axis = legend.text.size.raster
             ),
             legend.args = list(
               text = 'PM2.5 relative\nexposure [%]',
               side = 3,
               font = 1,
               line = 0.5,
               cex = legend.title.size.raster
             ))

dev.off()


## MAP within --------------------------------------------------------------------
# crop to income raster extent to speed up processing
countries_inc <- terra::crop(countries_vect, pm_raster2_europe2_filtered)
countries_inc <- terra::project(countries_inc, pm_raster2_europe2_filtered)
countries_iso <- countries_inc[, c("iso_a3")]
countries_iso <- countries_iso[!countries_iso$iso_a3 %in% c("ALA","DZA","GEO",
                                                            "GRL","IOR","IRQ",
                                                            "ISR","JOR","LBN",
                                                            "LBY","MAR","RUS",
                                                            "SYR","TUN","UKR",
                                                            "BLR","TUR","CYP"), ]

## MAP within DEATHS by grid cell  -------------------------------------------------

# in relation to the median by ctry, which AP deviation from it, percentage-wise,
# does each grid cell suffer?

pm.mort_raster3 <- terra::crop(pm.mort_raster2, extent_raster)

# Filter out NA values directly on the rasters
pm.mort_raster_filtered <- terra::mask(pm.mort_raster3, pm.mort_raster3, maskvalue = NA)

# Convert the filtered rasters to data frames
deaths_values <- terra::values(pm.mort_raster_filtered)
df_deaths_ctry <- data.frame(pm_concentration = deaths_values, 
                             ctry_names = ctry_values) %>% 
  dplyr::group_by(ctry_code) %>% 
  dplyr::mutate(
    country_median = median(layer, na.rm = TRUE),
    layer_pct_median = ((layer - country_median) / country_median) * 100
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(layer_pct_median = dplyr::if_else(is.na(ctry_code) | is.infinite(layer_pct_median), 
                                                  NA, layer_pct_median))

pm.mort_raster_pct <- pm.mort_raster_filtered
terra::values(pm.mort_raster_pct) = df_deaths_ctry$layer_pct_median
pm.mort_raster_pct[pm.mort_raster_pct  > 100] = 125
# pm.mort_raster_pct[pm.mort_raster_pct  > 100] = NA

# define palette
n_colors <- 125
breaks <- seq(-100, 125, length.out = n_colors)
custom_palette <- colorRampPalette(c("#0C6291", "#E8DAB2", "#FF66CC"))(n_colors)

zero_index <- which.min(abs(breaks))
colors_left <- colorRampPalette(c("#0C6291", "#E8DAB2"))(zero_index)
colors_right <- colorRampPalette(c("#E8DAB2", "#FF66CC"))(n_colors - zero_index + 1)
custom_palette <- c(colors_left, colors_right[-1])

# plot
pdf("figures/plot_grid_wihtin_deaths.pdf", width = 11/2.54, height = 10/2.54)
par(mar = c(0,0,0,0))
r <- raster::raster(pm.mort_raster_pct)
terra::plot(r, 
            col = custom_palette,
            legend = FALSE, 
            axes = FALSE, 
            box = FALSE)
terra::plot(countries_iso, 
            add = TRUE, 
            border = "black", 
            lwd = 0.10)
legend_ticks <- c(-100, -50, 0, 50, 100, raster::maxValue(r))
legend_labels <- c("-100%", "-50%", "Ctry median", "+50%", "+100%", ">100%")
raster::plot(r, legend.only = TRUE,
             col = custom_palette,
             legend.width = 1,
             legend.shrink = 0.65,
             frame.plot = FALSE,
             axis.args = list(
               at = legend_ticks,
               labels = legend_labels,
               font = 1,
               cex.axis = legend.text.size.raster
             ),
             legend.args = list(
               text = 'PM2.5 relative\nhealth impact [%]',
               side = 3,
               font = 1,
               line = 0.5,
               cex = legend.title.size.raster
             ))
dev.off()





## MAP settlement type by NUTS3 region -------------------------------------------

urbn_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, urbn_type) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
urbn_nuts3_combined_filtered_sf <- sf::st_sf(urbn_nuts3_combined_filtered, geometry = urbn_nuts3_combined_filtered$geometry)


plot_urbtype <- tm_shape(nuts3_plot_data,
                         projection = "EPSG:3035",
                         xlim = c(2400000, 6500000),
                         ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(urbn_nuts3_combined_filtered_sf) +
  tm_polygons("urbn_type",
              showNA = F,
              title = "Settlement type",
              palette = urbn_type.color,
              labels = urbn_type.labs,
              lwd = 0.1
  ) +
  tm_layout(
    legend.position = c("right", "top"),
    legend.bg.color = NA,
    legend.bg.alpha = 0,
    legend.title.size = legend.title.size.raster - 0.3,
    legend.text.size = legend.title.size.raster - 0.4,
    legend.just = "center",
    legend.width = 0.5,
    legend.height = 0.3,
    frame = F
  )

tmap::tmap_save(tm = plot_urbtype,
                "figures/plot_nuts3_urb_quintiles.pdf",
                width = 4.3, height = 3.93, units = "cm", dpi = 300
                # width = 11/2.54, height = 10/2.54
)


## MAP income quintiles by NUTS3 region -------------------------------------------

inc_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, income) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
inc_nuts3_combined_filtered_sf <- sf::st_sf(inc_nuts3_combined_filtered, geometry = inc_nuts3_combined_filtered$geometry)


do_map_between_socioecon_nuts3(inc_nuts3_combined_filtered_sf,
                               quintiles.color,
                               quintiles.labs,
                               "Income quintiles",
                               "plot_nuts3_inc_quintiles.pdf")

## MAP eldery proportion quintiles by NUTS3 region -----------------------------

eld_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, per_elderly) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
eld_nuts3_combined_filtered_sf <- sf::st_sf(eld_nuts3_combined_filtered, geometry = eld_nuts3_combined_filtered$geometry)


do_map_between_socioecon_nuts3(eld_nuts3_combined_filtered_sf,
                               quintiles_v2.color,
                               quintiles_v2.labs,
                               "Elderly proportion\nquintiles",
                               "plot_nuts3_eld_quintiles.pdf")


## MAP gini index quintiles by NUTS3 region -----------------------------

gini_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, gini) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
gini_nuts3_combined_filtered_sf <- sf::st_sf(gini_nuts3_combined_filtered, geometry = gini_nuts3_combined_filtered$geometry)


do_map_between_socioecon_nuts3(gini_nuts3_combined_filtered_sf,
                               quintiles_v3.color,
                               quintiles_v3.labs,
                               "Gini index\nquintiles",
                               "plot_nuts3_gini_quintiles.pdf")


## MAP income quintiles by NUTS3 region within ctry ----------------------------

inc_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, value = income, ctry) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(value, split_num))) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
inc_nuts3_combined_filtered_sf <- sf::st_sf(inc_nuts3_combined_filtered, geometry = inc_nuts3_combined_filtered$geometry)

do_map_within_socioecon_nuts3(
  inc_nuts3_combined_filtered_sf,
  quintiles_v.color,
  quintiles_v.labs,
  'Income quintiles',
  'plot_nuts3_wihtin_inc_quintiles.pdf'
)

## MAP elderly proportion quintiles by NUTS3 region within ctry ----------------------------

eld_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, value = per_elderly, ctry) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(value, split_num))) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
eld_nuts3_combined_filtered_sf <- sf::st_sf(eld_nuts3_combined_filtered, geometry = eld_nuts3_combined_filtered$geometry)

do_map_within_socioecon_nuts3(
  eld_nuts3_combined_filtered_sf,
  quintiles_v2.color,
  quintiles_v2.labs,
  'Elderly proportion\nquintiles',
  'plot_nuts3_wihtin_eld_quintiles.pdf'
)

## MAP gini index quintiles by NUTS3 region within ctry ----------------------------

gini_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, value = gini, ctry) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(value, split_num))) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
gini_nuts3_combined_filtered_sf <- sf::st_sf(gini_nuts3_combined_filtered, geometry = gini_nuts3_combined_filtered$geometry)

do_map_within_socioecon_nuts3(
  gini_nuts3_combined_filtered_sf,
  quintiles_v3.color,
  quintiles_v3.labs,
  'Gini index\nquintiles',
  'plot_nuts3_wihtin_gini_quintiles.pdf'
)

## MAP within AP by NUTS3 region  -------------------------------------------------
ap_nuts3_combined_filtered <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, value = ap, ctry) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(
    country_median = median(value, na.rm = TRUE),
    layer_pct_median = ((value - country_median) / country_median) * 100
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
ap_nuts3_combined_filtered_sf <- sf::st_sf(ap_nuts3_combined_filtered, geometry = ap_nuts3_combined_filtered$geometry)

# plot
break_vals <- c(-100, -50, 0, 50, max(ap_nuts3_combined_filtered_sf$layer_pct_median))
legend_labels <- c("-100%", "-50%", "Ctry median", "+50%", 
                   paste0("+", round(max(ap_nuts3_combined_filtered_sf$layer_pct_median)), "%"))

plot <- ggplot() +
  geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = ap_nuts3_combined_filtered_sf, 
          aes(fill = layer_pct_median), 
          color = "black",
          size = 0.0001) + 
  scale_fill_gradientn(
    colours = c("#0C6291", "#E8DAB2", "#FF66CC"),
    values = scales::rescale(c(-100, 0, 100)),
    labels = legend_labels,
    breaks = break_vals,
    limits = c(min(ap_nuts3_combined_filtered_sf$layer_pct_median), max(ap_nuts3_combined_filtered_sf$layer_pct_median)),
    name = "PM2.5 relative\nexposure [%]"
  ) +
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size, hjust = 0.5),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
ggsave(plot,
       filename = paste0("figures/plot_nuts3_wihtin_ap.pdf"),
       width = 11, height = 10, units = "cm")


## MAP within DEATHS by NUTS3 region  -------------------------------------------------
deaths_nuts3_combined_filtered <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, value = deaths, ctry) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(
    country_median = median(value, na.rm = TRUE),
    layer_pct_median = ((value - country_median) / country_median) * 100,
    layer_pct_median = dplyr::if_else(country_median == 0, NA, layer_pct_median)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
deaths_nuts3_combined_filtered_sf <- sf::st_sf(deaths_nuts3_combined_filtered, geometry = deaths_nuts3_combined_filtered$geometry)

# plot
break_vals <- c(-100, -50, 0, 50, max(deaths_nuts3_combined_filtered_sf$layer_pct_median, na.rm = T))
legend_labels <- c("-100%", "-50%", "Ctry median", "+50%", 
                   paste0("+", round(max(deaths_nuts3_combined_filtered_sf$layer_pct_median, na.rm = T)), "%"))
plot <- ggplot() +
  geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = deaths_nuts3_combined_filtered_sf, 
          aes(fill = layer_pct_median), 
          color = "black",
          size = 0.0001) + 
  scale_fill_gradientn(
    colours = c("#0C6291", "#E8DAB2", "#FF66CC"),
    values = scales::rescale(c(-100, 0, 100)),
    breaks = break_vals,
    labels = legend_labels,
    limits = c(min(deaths_nuts3_combined_filtered_sf$layer_pct_median), max(deaths_nuts3_combined_filtered_sf$layer_pct_median)),
    name = "PM2.5 relative\nhealth impact [%]"
  ) +
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = legend.title.size, hjust = 0.5),
    legend.text = element_text(size = legend.text.size),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
ggsave(plot,
       filename = paste0("figures/plot_nuts3_wihtin_deaths.pdf"),
       width = 11, height = 10, units = "cm")



######################################################################## DIAGNOSTIC FIGURES ####
# rfasst PM2.5 wegihts
pdf("figures/pm_weights.pdf", width = 14.5/2.54, height = 10/2.54)
par(mar = c(0,0,0,0))
r <- raster::raster(pm.weights_raster2)
terra::plot(r, 
            legend = FALSE, 
            axes = FALSE, 
            box = FALSE)
r.range <- c(raster::minValue(r), raster::maxValue(r))
terra::plot(r, legend.only=TRUE,     
            legend.width=1, legend.shrink=0.65,
            frame.plot = F,
            axis.args=list(font=1,
                           cex.axis=legend.text.size.raster),
            legend.args=list(text='PM2.5\nweight', side=3, font=1, line=0.5, cex=legend.title.size.raster))
dev.off()


# Emissions Figures
emiss_data <- rgcam::getQuery(prj, 'CO2 emissions by sector (excluding resource production)') %>% 
  dplyr::mutate(ghg = 'CO2') %>% 
  rbind(rgcam::getQuery(prj, 'nonCO2 emissions by sector (excluding resource production)')) %>% 
  dplyr::left_join(sector_map, by = 'sector') %>% 
  dplyr::filter(!is.na(agg_sector),
                year >= 2015, year <= 2030) %>% 
  dplyr::left_join(ghg_map, by = 'ghg') %>% 
  dplyr::group_by(scenario, region, sector = agg_sector, Units, year, ghg = agg_ghg) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  dplyr::ungroup() %>% 
  rename_scen() %>% 
  dplyr::mutate(ghg = dplyr::if_else(ghg == 'NMVOC', 'VOC', ghg))

# Baseline vs POLICY55 - by ghg - by timeline
emiss_data_timeline <- emiss_data %>% 
  dplyr::filter(ghg %in% sel_ghg) %>% 
  dplyr::group_by(scenario, year, ghg) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  dplyr::ungroup()

plot_d_timeline <- ggplot(emiss_data_timeline,
                          aes(x = year, y = value, color = scenario)) +
  geom_line(linewidth = 1.1) +
  labs(title = "", y = "Emissions", x = "") +
  scale_color_manual(values = pal_sce, name = 'Scenario') +
  theme_minimal(base_size = 10) +
  ylim(0, NA) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  facet_wrap(~ghg, scales = "free_y")

ggsave(plot_d_timeline,
       filename = paste0("figures/plot_d_timeline.pdf"),
       width = 10,
       height = 8,
       dpi = 300)

# POLICY55 - by ghg - by sector
emiss_data_sec <- emiss_data %>% 
  dplyr::filter(ghg %in% sel_ghg,
                year == 2030) %>% 
  dplyr::group_by(scenario, sector, ghg) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  dplyr::ungroup() %>% 
  rename_sec()

plot_d_sec <- ggplot(emiss_data_sec,
                     aes(x = scenario, y = value, fill = sector)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(title = "", y = "Emissions", x = "") +
  scale_fill_manual(values = pal_sec, name = 'Sector') +
  theme_minimal(base_size = 10) +
  ylim(0, NA) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  facet_wrap(~ghg, scales = "free_y")

ggsave(plot_d_sec,
       filename = paste0("figures/plot_d_sec.pdf"),
       width = 10,
       height = 8,
       dpi = 300)

# joint fig
plot_d_emiss <- plot_grid(ggdraw() +
                            draw_plot(plot_d_timeline, y = 0.05, height = 1),
                          ggdraw() +
                            draw_plot(plot_d_sec, y = 0.005, height = 1) +
                            draw_label("a)", x = 0, hjust = 0, vjust = -92, fontface = "bold", size = 11) +
                            draw_label("b)", x = 0, hjust = 0, vjust = -41, fontface = "bold", size = 11),
                          ncol = 1, rel_heights = c(1, 1.75)
)

ggsave(plot_d_emiss,
       filename = paste0("figures/plot_d_emiss.pdf"),
       width = 10,
       height = 15,
       dpi = 300)

