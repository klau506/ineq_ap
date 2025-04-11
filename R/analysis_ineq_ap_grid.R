require(eurostat)
library(ggplot2)
library(tmap)
library(magrittr)
library(terra)
library(leaflet)

source("R/utils.R")
source("R/zzz.R")

normalized <- T
split_num <- 5 #10 deciles, 5 quintiles
map <- T #T if plotted and saved, F otherwise
yy <- 2030# Load necessary libraries

# Load raster data
pm_raster <- terra::rast("data/rfasst_output/2030_pm25_fin_weighted.tif")
inc_pc_2015 <- terra::rast("data/High-resolution_Downscaling/Europe_disp_inc_2015.tif")
urbn_raster <- terra::rast("data/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0_reproj2.tif")
pop_ge65 <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-Y_GE65_2021_V2.tiff")
pop_t <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-T_2021_V2.tiff")

extent_raster <- ext(-26.276, 40.215, 32.633, 71.141)
pm_raster2 <- terra::crop(pm_raster, extent_raster)
inc_pc_20152 <- terra::crop(inc_pc_2015, extent_raster)

europe_shp <- rnaturalearth::ne_countries(continent = "Europe", returnclass = "sf") %>% 
  dplyr::filter(adm0_a3 != 'RUS')
eu_mask <- vect(europe_shp)
eu_mask <- terra::crop(eu_mask, extent_raster)
eu_mask[!is.na(eu_mask)] <- 1
eu_mask[is.na(eu_mask)] <- 0


# target_crs <- "EPSG:4326"
# urbn_raster_reproj <- project(urbn_raster, target_crs)
# plot(urbn_raster_reproj)
# writeRaster(urbn_raster_reproj, 'data/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0_reproj.tif')
# pop_ge65_reproj <- terra::project(pop_ge65, target_crs)
# plot(pop_ge65_reproj)
# writeRaster(pop_ge65_reproj, 'data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-Y_GE65_2021_V2_reproj.tiff')
# pop_t_reproj <- terra::project(pop_t, target_crs)
# plot(pop_t_reproj)
# writeRaster(pop_t_reproj, 'data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-T_2021_V2_reproj.tiff')


## AP vs URBN TYPE ===============================================================
# urbn_raster2 <- terra::resample(urbn_raster, pm_raster)
# urbn_raster2 <- terra::crop(urbn_raster2, extent_raster)
# writeRaster(urbn_raster2, 'data/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0_reproj2.tif')

# Define classification function
classify_function <- function(x) {
  ifelse(x <= 10, 0, 
         ifelse(x >= 11 & x <= 20, 1, # rural
                ifelse(x >= 21 & x <= 22, 2, # town/suburb
                       ifelse(x >= 23 & x <= 30, 3, NA)))) # urban
}

# Apply classification
urbn_raster_classified <- app(urbn_raster2, classify_function)
names(urbn_raster_classified) <- "classification_layer"
urbn_raster_combined <- c(urbn_raster2, urbn_raster_classified)
plot(urbn_raster_combined$classification_layer)

# Plot only 1 urbn type
# mask_layer <- urbn_raster_combined[["classification_layer"]] == 2
# filtered_raster <- mask(urbn_raster_combined, mask_layer, maskvalue=FALSE)
# plot(filtered_raster$classification_layer)

# Filter out NA values directly on the rasters
pm_raster2 <- crop(pm_raster, extent_raster)
pm_raster_filtered <- mask(pm_raster2, pm_raster2, maskvalue = NA)
urbn_raster_filtered <- urbn_raster_combined$classification_layer
urbn_raster_combined_filtered <- mask(urbn_raster_filtered, urbn_raster_filtered, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- values(pm_raster_filtered)
urbn_values <- values(urbn_raster_combined_filtered)

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(urbn_values)
df_ap_urbn <- data.frame(pm_concentration = pm_values[valid_idx], urbn_type = urbn_values[valid_idx])

df_ap_urbn_no0 <- df_ap_urbn[df_ap_urbn$pm_concentration > 0,]
df_ap_urbn_no0 <- df_ap_urbn_no0[df_ap_urbn_no0$urbn_type > 0,]
df_ap_urbn_no0 <- unique(df_ap_urbn_no0)

df_medi <- df_ap_urbn_no0 %>%
  dplyr::group_by(urbn_type) %>%
  dplyr::summarize(medi = mean(pm_concentration)) %>% 
  dplyr::ungroup()

plot_urbntype_density <- ggplot(df_ap_urbn_no0, aes(x = pm_concentration, 
                   color = as.factor(urbn_type),
                   fill = as.factor(urbn_type))) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(color = as.factor(urbn_type), xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_grid(as.factor(urbn_type) ~ .) +
  scale_fill_manual(
    values = urbn_type.color.num,
    name = "Urban type",
    labels = urbn_type.labs.num
  ) +
  scale_color_manual(
    values = urbn_type.color.num,
    name = "Urban type",
    labels = urbn_type.labs.num
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
  file = paste0("figures/grid/plot_urbntype_density_ap.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_urbntype_density
)


## AP vs INCOME ===============================================================
pm_raster2 <- terra::crop(pm_raster, extent_raster)
inc_pc_20152 <- terra::resample(inc_pc_2015, pm_raster2)
inc_pc_20152 <- terra::crop(inc_pc_20152, extent_raster)

# Filter out NA values directly on the rasters
pm_raster_filtered <- mask(pm_raster2, pm_raster2, maskvalue = NA)
inc_raster_filtered <- mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- values(pm_raster_filtered)
inc_values <- values(inc_raster_filtered)

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(inc_values)
df_ap_inc <- data.frame(pm_concentration = pm_values[valid_idx], inc_per_capita = inc_values[valid_idx])

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

if (map) {
  plot_income_ap <- ggplot(binned_data %>% 
                             dplyr::filter(avg_gdp < 30000), 
                           aes(x = avg_gdp, y = avg_pm_q, color = quintile_5)) +
    geom_point() +
    scale_color_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "", 
         y = "PM2.5 concentration [ug/m3]") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.ontop = FALSE,
      strip.text = element_text(size = 12),
      strip.background = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.key.size = unit(1.5, "cm"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
  
  ggsave(
    file = paste0("figures/grid/plot_income_ap.pdf"), height = 20, width = 20, units = "cm",
    plot = plot_income_ap
  )
}

if (map) {
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
    file = paste0("figures/grid/plot_income_density_ap.pdf"), height = 12, width = 18, units = "cm",
    plot = plot_inc_density
  )
  
}

## AP vs ELDERLY ===============================================================
pm_raster2 <- terra::crop(pm_raster, extent_raster)

pop_ge652 <- terra::crop(pop_ge65, extent_raster)
pop_t2 <- terra::crop(pop_t, extent_raster)
pop_elderly <- pop_ge652/pop_t2
# Plot only 1 urbn type
mask_layer <- pop_elderly[["ESTAT_OBS-VALUE-Y_GE65_2021_V2"]] <= 100 & pop_elderly[["ESTAT_OBS-VALUE-Y_GE65_2021_V2"]] > 0
filtered_raster <- mask(pop_elderly, mask_layer, maskvalue=FALSE)
plot(filtered_raster$`ESTAT_OBS-VALUE-Y_GE65_2021_V2`)

pop_elderly2 <- terra::resample(pop_elderly, pm_raster2)
pop_elderly2 <- terra::crop(pop_elderly2, extent_raster)

# Filter out NA values directly on the rasters
pm_raster_filtered <- mask(pm_raster2, pm_raster2, maskvalue = NA)
elderly_raster_filtered <- mask(pop_elderly2, pop_elderly2, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- values(pm_raster_filtered)
pop_elderly <- values(elderly_raster_filtered)

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(pop_elderly)
df_ap_eld <- data.frame(pm_concentration = pm_values[valid_idx], pop_elderly_per = pop_elderly[valid_idx])

df_ap_eld_no0 <- df_ap_eld[df_ap_eld$pm_concentration > 0,]
df_ap_eld_no0 <- df_ap_eld_no0[df_ap_eld_no0$pop_elderly_per > 0,]
df_ap_eld_no0 <- unique(df_ap_eld_no0) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, is.finite(pop_elderly_per), pop_elderly_per < 1) %>% 
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(pop_elderly_per, 5))) 

binned_data <- df_ap_eld_no0 %>%
  dplyr::mutate(quintile = as.factor(dplyr::ntile(pop_elderly_per, 50000))) %>% 
  dplyr::group_by(quintile, quintile_5) %>%
  dplyr::summarize(avg_eld = mean(pop_elderly_per),
                   avg_pm_q = mean(pm_concentration)) %>% 
  dplyr::ungroup()

if (map) {
  plot_elderly_ap <- ggplot(binned_data %>% 
                              dplyr::filter(avg_eld < 2.5), 
                           aes(x = avg_eld, y = avg_pm_q, color = quintile_5)) +
    geom_point() +
    scale_color_manual(
      values = quintiles.color,
      name = "Elderly Quintiles [%]",
      labels = quintiles.labs
    ) +
    labs(x = "", 
         y = "PM2.5 concentration [ug/m3]") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.ontop = FALSE,
      strip.text = element_text(size = 12),
      strip.background = element_blank(),
      axis.title.x = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.key.size = unit(1.5, "cm"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )
  
  ggsave(
    file = paste0("figures/grid/plot_elderly_ap.pdf"), height = 20, width = 20, units = "cm",
    plot = plot_elderly_ap
  )
}

if (map) {
  df_medi <- df_ap_eld_no0 %>%
    dplyr::group_by(quintile_5) %>%
    dplyr::summarize(medi = mean(pm_concentration)) %>% 
    dplyr::ungroup()
  
  df_ap_eld_no0 %>%
    dplyr::group_by(quintile_5) %>%
    dplyr::summarise(
      min_per_elderly = min(pop_elderly_per, na.rm = TRUE),
      max_per_elderly = max(pop_elderly_per, na.rm = TRUE),
      mean_per_elderly = mean(pop_elderly_per, na.rm = TRUE),
      median_per_elderly = median(pop_elderly_per, na.rm = TRUE),
      sd_per_elderly = sd(pop_elderly_per, na.rm = TRUE),
      n = dplyr::n()
    )
  # # A tibble: 5 x 7
  # quintile_5 min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly      n
  # <fct>                <dbl>           <dbl>            <dbl>              <dbl>          <dbl>  <int>
  # 1 1             0.0000000605           0.146           0.0934              0.105         0.0421 703964
  # 2 2             0.146                  0.194           0.171               0.172         0.0136 703964
  # 3 3             0.194                  0.238           0.215               0.215         0.0127 703964
  # 4 4             0.238                  0.310           0.269               0.267         0.0203 703963
  # 5 5             0.310                  1.00            0.456               0.398         0.157  703963
  
  
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
    file = paste0("figures/grid/plot_elderly_density_ap.pdf"), height = 12, width = 18, units = "cm",
    plot = plot_eld_density
  )
  
}
