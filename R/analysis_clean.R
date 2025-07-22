## 1.- Run data_preprocess.R to obtain all the datasets used in this script
## 2.- Run this script to generate all the figures and additional plots to perform
# the analysis presented in the main manuscript and the supplementary information

################################################################################
#                                LOAD DATA                                     #
################################################################################
# libraries
require(eurostat)
library(ggpattern)
library(ggplot2)
library(tmap)
library(magrittr)
library(ggpubr)

# helper scripts
source("R/utils.R")
source("R/zzz.R")

# constants
normalized <- T
normalized_tag <- dplyr::if_else(normalized, '_norm100k', '')
split_num <- 5 #10 deciles, 5 quintiles
split_num_tag <- dplyr::if_else(split_num == 5, 'quintile', 
                                dplyr::if_else(split_num == 10, 'decile',
                                               paste0('split_num_',split_num)))
map <- T #T if plotted and saved, F otherwise
yy <- 2030

crop_xmin <- -22
crop_xmax <- 32
crop_ymin <- 36
crop_ymax <- 71
legend.title.size <- 9
legend.text.size <- 8
legend.title.size.raster <- 0.6
legend.text.size.raster <- 0.6
scl <- 20
spacing_factor = 0.5


# NUTS3 data
ap_socioecon_sf <- get(load('ap_socioecon_sf.RData'))
deaths_socioecon_sf <- get(load('deaths_socioecon_sf_newdeaths.RData'))

rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 

rfasst_ctry_pop <- rfasst::pop.all.ctry_ctry.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value)  

ap <- get(load("data/rfasst_output/necp_m2_get_conc_pm25.ctry_agg.output.RData")) %>%
  dplyr::filter(year == yy)

deaths <- get(load(paste0("data/rfasst_output/necp_m3_get_mort_pm25.output.RData"))) %>%
  dplyr::select(region, year, age, sex, disease, value = GBD, scenario) %>% 
  dplyr::filter(year == yy,
                sex == 'Both')
deaths_ctry <- deaths %>%
  dplyr::filter(
    nchar(region) > 3
  ) %>%
  dplyr::rename(
    NUTS3 = region,
    deaths = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>% 
                     dplyr::distinct(),
                   by = "NUTS3"
  ) %>% 
  dplyr::group_by(year, sex, scenario, region = ISO3) %>% 
  dplyr::summarise(value = sum(deaths, na.rm = T)) %>% 
  dplyr::ungroup()

count_deaths <- sum(deaths_ctry$value)
print(count_deaths)

if (normalized) {
  deaths <- deaths %>% 
    dplyr::group_by(region, year, sex, scenario) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(data.table::as.data.table(rfasst_pop) %>% 
                       dplyr::filter(year == yy) %>%
                       dplyr::group_by(region = geo, sex) %>% 
                       dplyr::summarise(pop = sum(pop) * 1e6) %>% # popM was in million
                       dplyr::ungroup(),
                     by = c('region','sex')) %>% 
    dplyr::mutate(value = value / pop * 1e6) %>% # deaths per 1000 habitants
    dplyr::select(-pop)
  
  deaths_ctry <- deaths_ctry %>% 
    dplyr::group_by(region, year, sex, scenario) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(data.table::as.data.table(rfasst_ctry_pop) %>% 
                       dplyr::filter(year == yy) %>%
                       dplyr::group_by(region = geo, sex) %>% 
                       dplyr::summarise(pop = sum(pop) * 1e6) %>% # popM was in million
                       dplyr::ungroup(),
                     by = c('region','sex')) %>% 
    dplyr::mutate(value = value / pop * 1e6) %>% # deaths per 1000 habitants
    dplyr::select(-pop)
}

# mapping data
nuts3_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID)
ctry_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 0, year = 2021) %>%
  dplyr::select(-FID)

# GRID data
extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)
pm.pre <- terra::rast(paste0('../../GitHub/rfasst_v2/output/m2/pm25_gridded/raster_grid/', 'Reference_vintage_eur_v2', '_', 2030,'_pm25_fin_weighted.tif'))
pm.pre <- terra::crop(pm.pre, extent_raster)

europe_shp <- rnaturalearth::ne_countries(continent = "Europe", returnclass = "sf") %>% 
  dplyr::filter(!adm0_a3 %in% c('RUS','BLR','UKR')) %>% 
  rbind(rnaturalearth::ne_countries(country = 'Turkey', returnclass = "sf"))
eu_mask <- terra::vect(europe_shp)
eu_mask <- terra::crop(eu_mask, extent_raster)
eu_mask[!is.na(eu_mask)] <- 1
eu_mask[is.na(eu_mask)] <- 0

ctry_raster <- europe_shp %>% 
  dplyr::select(ctry_code = iso_a2_eh, geometry)
ctry_raster <- terra::rasterize(ctry_raster, pm.pre, field = "ctry_code")
ctry_values <- terra::values(ctry_raster)
ctry_raster_values_mapping <- terra::cats(ctry_raster)[[1]]

pm.ap_raster <- terra::rast("data/rfasst_output/EU_NECP_LTT_2030_pm25_fin_weighted.tif")
pm.mort_raster <- get(load("data/rfasst_output/pm.mort_mat_2030_EU_NECP_LTT.RData")); rm(pm.mort_yy); gc()
inc_pc_2015 <- terra::rast("data/High-resolution_Downscaling/Europe_disp_inc_2015.tif")
urbn_raster <- terra::rast("data/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0_reproj2.tif")
pop_ge65 <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-Y_GE65_2021_V2.tiff")
pop_t <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-T_2021_V2.tiff")

pm.ap_raster2 <- terra::crop(pm.ap_raster, extent_raster)
vec <- as.vector(pm.mort_raster[['total']])
pm.mort_raster <- terra::setValues(pm.ap_raster2, vec)

################################################################################
#                                  PLOTS                                       #
################################################################################

## AP  -------------------------------------------------------------------------
ap_nuts3 <- ap %>%
  dplyr::filter(
    nchar(region) > 3
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
 # MT        2030    10.4 
 # NL        2030    10.5 
 # CH        2030    10.9 
 # BE        2030    11.4 
 # TR        2030    11.6 
 # IT        2030    12.9


## GRID - AP  -------------------------------------------------------------------------
pm_raster <- terra::rast("data/rfasst_output/EU_NECP_LTT_2030_pm25_fin_weighted.tif")
extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)
pm_raster2 <- terra::crop(pm_raster, extent_raster)
pm_raster2_europe <- terra::mask(pm_raster2, eu_mask)

pdf("figures/plot_grid_ap.pdf", width = 11/2.54, height = 10/2.54)
par(mar = c(0,0,0,0))
r <- raster::raster(pm_raster2_europe)
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
    sex == 'Both'
  ) %>%
  dplyr::rename(
    geo = region,
    deaths = value
  ) %>%
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
                       name = "Premature Deaths\n[Deaths per 1M inhabitants]") +
  
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
#  HR         2030        375.
#  ME         2030        405.
#  HU         2030        460.
#  MK         2030        475.
#  RS         2030        498.
#  BG         2030        551.


## GRID - DEATHS  -------------------------------------------------------------------------
pm.mort_raster <- get(load("data/rfasst_output/pm.mort_mat_2030_EU_NECP_LTT.RData")); rm(pm.mort_yy); gc()
pm.mort_raster2 <- terra::setValues(pm.pre, pm.mort_raster[['total']])
pm.mort_raster2 <- terra::crop(pm.mort_raster2, extent_raster)

eu_mask_raster <- terra::rast(terra::ext(eu_mask), resolution = 0.01)  # or define your own resolution
eu_mask_raster2 <- terra::rasterize(eu_mask, eu_mask_raster, field = "iso_n3")
eu_mask_raster2 <- terra::crop(eu_mask_raster2, extent_raster)
eu_mask_raster2 <- terra::resample(eu_mask_raster2, pm.mort_raster2)

pm.mort_raster2_europe <- terra::mask(pm.mort_raster2, eu_mask_raster2)

filtered_raster <- pm.mort_raster2_europe
filtered_raster[filtered_raster <= 0] <- NA
filtered_raster[filtered_raster > 3.5] <- 3.5

pdf("figures/plot_grid_mort.pdf", width = 11/2.54, height = 10/2.54)
r <- raster::raster(filtered_raster)
base_r <- raster::raster(eu_mask_raster2)
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
               at = legend_ticks,
               labels = legend_labels,
               font = 1,
               cex.axis = legend.text.size.raster
             ),
             legend.args = list(
               text = 'Premature Deaths\n[Deaths per inhabitants]',
               side = 3,
               font = 1,
               line = 0.5,
               cex = legend.title.size.raster
             ))
dev.off()


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
# NLD   2030     10.2
# HUN   2030     10.2
# LUX   2030     10.3
# BEL   2030     10.9
# CHE   2030     11.0
# ITA   2030     12.5


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
  geom_sf_pattern(data = ap_ctry_sf %>% dplyr::filter(overWHO) %>% 
                    dplyr::distinct(),
                  aes(fill = ap),
                  pattern = "stripe", 
                  pattern_density = 0.4,
                  pattern_spacing = 0.02,
                  pattern_angle = 45,
                  pattern_linetype = "solid",
                  pattern_fill = "transparent",
                  pattern_color = "red",
                  pattern_size = 0.1,
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
print(ap_ctry_sf %>% dplyr::filter(!overWHO) %>% dplyr::pull(ISO3) %>% unique())

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

deaths_ctry_sf$deaths[deaths_ctry_sf$deaths > 25] <- 25

plot_deaths_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = deaths_ctry_sf %>% dplyr::filter(sex == "Both"), 
          aes(fill = deaths), 
          # color = NA, 
          color = "black",
          size = 0.05) + 
  scale_fill_distiller(palette = "Oranges", direction = 1, 
                       name = "Premature Deaths\n[Deaths per 1M inhabitants]") +
  
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
                                      legend_title = 'Urban type', 
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
leg = ggpubr::get_legend(plot_ap_urbn_type + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_urbn_type.RData"))


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
offset_x <- min(ap_deaths_nuts3$deaths, na.rm = TRUE) - 0.1 * diff(range(ap_deaths_nuts3$deaths))
offset_y <- min(ap_deaths_nuts3$ap, na.rm = TRUE) - 0.1 * diff(range(ap_deaths_nuts3$ap))

# define the start and end point of the segments
a <- min(quintile_means$mean_deaths, na.rm = TRUE) -4 * scl  # start of vertical line
b <- max(quintile_means$mean_deaths, na.rm = TRUE) +4 * scl  # end of vertical line
c <- min(quintile_means$mean_ap, na.rm = TRUE) -0.10 * scl     # start of horizontal line
d <- max(quintile_means$mean_ap, na.rm = TRUE) +0.10 * scl     # end of horizontal line


plot_deaths_ap <- ggplot() +
  geom_segment(data = quintile_means[1,], 
               aes(x = a, xend = b, y = offset_y, yend = offset_y), 
               linetype = "dashed", size = 0.8, alpha = 0.75) +
  geom_segment(data = quintile_means[1,], 
               aes(x = offset_x, xend = offset_x, y = c, yend = d), 
               linetype = "dashed", size = 0.8, alpha = 0.75) +
  geom_point(data = ap_deaths_nuts3, 
             aes(x = deaths, y = ap, color = quintile, shape = urbn_type),
             alpha = 0.75, size = 2) +
  geom_point(data = quintile_means, aes(x = offset_x, y = mean_ap, color = quintile), 
             size = 3, shape = 18) +
  geom_point(data = quintile_means, aes(x = mean_deaths, y = offset_y, color = quintile), 
             size = 3, shape = 18) + 
  scale_color_manual(
    values = quintiles.color,
    name = "Income quintiles",
    labels = quintiles.labs
  ) +
  theme_minimal() +
  labs(x = "Premature deaths [Deaths per 1M inhabitants]", 
       y = "PM2.5 concentration [µg/m³]",
       color = "Income quintiles", 
       shape = "Settlement Type") +
  expand_limits(x = offset_x, y = offset_y) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.border = element_blank(),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.text = element_text(size = legend.text.size),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size)
  )



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
leg = ggpubr::get_legend(plot_ap_income + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_income.RData"))

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
# 1 1             2901.     13025.      10839.        11373.     2085.   246
# 2 2            13057.     16203.      14740.        14812.      997.   246
# 3 3            16210.     18104.      17138.        17107.      564.   246
# 4 4            18123.     20570.      19273.        19172.      723.   246
# 5 5            20571.     46866.      22843.        22129.     3192.   246


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
                                 legend_type = 'quintiles',
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

leg = ggpubr::get_legend(plot_ap_gini + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_gini.RData"))


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
                                 legend_type = 'quintiles',
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

leg = ggpubr::get_legend(plot_ap_per_elderly + theme(
  legend.direction = 'horizontal',
  legend.position = 'bottom',
  legend.key.size = unit(1, "cm"),
  legend.title = element_text(size = legend.text.size+5),
  legend.text = element_text(size = legend.text.size+5))
)
save(leg, file = paste0("figures/legend_per_elderly.RData"))

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
# 1 1                 0.0276           0.175            0.135              0.147        0.0365    296
# 2 2                 0.175            0.202            0.190              0.191        0.00779   296
# 3 3                 0.202            0.222            0.212              0.212        0.00562   296
# 4 4                 0.222            0.244            0.233              0.232        0.00630   295
# 5 5                 0.244            0.354            0.266              0.261        0.0193    295



## Deaths vs urbn_type1 -------------------------------------------------------------
deaths_urbntype_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, urbn_type, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

df <- data.table::as.data.table(deaths_urbntype_sf) %>%
  dplyr::select(-geometry)
df_medi <- df[, .(medi = quantile(deaths, 0.5, na.rm = T)),
              by = c("urbn_type")
]

plot_urbntype_density <- ggplot(df) +
  geom_density(
    data = df, aes(
      x = deaths, group = urbn_type,
      color = urbn_type, fill = urbn_type
    ),
    linewidth = 0.8, alpha = 0.25
  ) +
  geom_vline(aes(color = urbn_type, xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_wrap(. ~ urbn_type, nrow = 3) +
  ggpubr::theme_pubr() +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  scale_color_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  labs(x = "Premature deaths [Deaths per 1M inhabitants]", y = "Probability density") +
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
  file = paste0("figures/plot_urbntype_density_deaths.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_urbntype_density
)



## Deaths vs urbn_type2 -------------------------------------------------------------
spacing_factor = 0.5; scl = 25

deaths_geo_urbn_type <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, deaths)
deaths_geo_urbn_type <- unique(deaths_geo_urbn_type) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = urbn_type) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()


plot_deaths_urbn_type <- prob_jitter_plot(deaths_geo_urbn_type %>% 
                                            dplyr::rename(item = deaths), 
                                          legend_title = 'Urban type', 
                                          legend_type = 'urbn_type',
                                          ox_text = 'Premature deaths [Deaths per 1M inhabitants]')
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
# 1 City             20.6        686.        285.          294.      83.8   371
# 2 Town/Suburb       7.88       755.        325.          320.     125.    585
# 3 Rural             2.31       759.        302.          307.     116.    428




## Deaths vs GINI -------------------------------------------------------------
deaths_geo_gini <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, gini, deaths)
deaths_geo_gini <- unique(deaths_geo_gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()


plot_deaths_gini <- prob_jitter_plot(deaths_geo_gini %>% 
                                          dplyr::rename(item = deaths), 
                                        legend_title = 'Gini quintiles', 
                                        legend_type = 'quintiles',
                                        ox_text = 'Premature deaths [Deaths per 1M inhabitants]')
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
# 1 1           0.204    0.285     0.270       0.271 0.0129    629
# 2 2           0.285    0.299     0.292       0.291 0.00355   629
# 3 3           0.299    0.312     0.305       0.306 0.00392   628
# 4 4           0.312    0.348     0.327       0.327 0.0112    628
# 5 5           0.348    0.553     0.387       0.380 0.0402    628




## Deaths vs INCOME -------------------------------------------------------------
deaths_geo_income <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, income, deaths)
deaths_geo_income <- unique(deaths_geo_income) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()

plot_deaths_income <- prob_jitter_plot(deaths_geo_income %>% 
                                        dplyr::rename(item = deaths), 
                                      legend_title = 'Income quintiles', 
                                      legend_type = 'quintiles',
                                      ox_text = 'Premature deaths [Deaths per 1M inhabitants]')

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
# 1 1             2901.     13015.      10823.        11337.     2087.   244
# 2 2            13025.     16210.      14728.        14787.     1006.   244
# 3 3            16223.     18125.      17145.        17120.      567.   244
# 4 4            18153.     20571.      19296.        19241.      719.   243
# 5 5            20585.     46866.      22849.        22122.     3207.   243




## Deaths vs ELDERLY -------------------------------------------------------------
deaths_geo_per_elderly <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, per_elderly, deaths)
deaths_geo_per_elderly <- unique(deaths_geo_per_elderly) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()


plot_deaths_per_elderly <- prob_jitter_plot(deaths_geo_per_elderly %>% 
                                       dplyr::rename(item = deaths), 
                                     legend_title = 'Elderly proportion\nquintiles', 
                                     legend_type = 'quintiles',
                                     ox_text = 'Premature deaths [Deaths per 1M inhabitants]')
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
# 1 1                 0.0586           0.182            0.157              0.164        0.0224    277
# 2 2                 0.182            0.206            0.195              0.195        0.00649   277
# 3 3                 0.206            0.224            0.215              0.215        0.00520   277
# 4 4                 0.224            0.246            0.234              0.234        0.00613   277
# 5 5                 0.246            0.354            0.267              0.262        0.0193    276


## GRID - AP vs URBN_TYPE ------------------------------------------------------
# data processing --------------------------------------------------------------
urbn_raster2 <- terra::resample(urbn_raster, pm.ap_raster2)
urbn_raster2 <- terra::crop(urbn_raster2, extent_raster)

# Define classification function
classify_function <- function(x) {
  ifelse(x <= 10, 0, 
         ifelse(x >= 11 & x <= 20, 1, # rural
                ifelse(x >= 21 & x <= 22, 2, # town/suburb
                       ifelse(x >= 23 & x <= 30, 3, NA)))) # urban
}

# Apply classification
urbn_raster_classified <- terra::app(urbn_raster2, classify_function)
names(urbn_raster_classified) <- "classification_layer"
urbn_raster_combined <- c(urbn_raster2, urbn_raster_classified)
terra::plot(urbn_raster_combined$classification_layer)

# Filter out NA values directly on the rasters
pm.ap_raster_filtered <- terra::mask(pm.ap_raster2, pm.ap_raster2, maskvalue = NA)
urbn_raster_filtered <- urbn_raster_combined$classification_layer
urbn_raster_combined_filtered <- terra::mask(urbn_raster_filtered, urbn_raster_filtered, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- terra::values(pm.ap_raster_filtered)
urbn_values <- terra::values(urbn_raster_combined_filtered)

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(urbn_values)
df_ap_urbn <- data.frame(pm_concentration = pm_values[valid_idx],
                         urbn_type = urbn_values[valid_idx],
                         ctry_names = ctry_values[valid_idx])

df_ap_urbn_no0 <- df_ap_urbn[df_ap_urbn$pm_concentration > 0,]
df_ap_urbn_no0 <- df_ap_urbn_no0[df_ap_urbn_no0$urbn_type > 0,]
df_ap_urbn_no0 <- unique(df_ap_urbn_no0)


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
ap_grid_urbn_sample <- ap_grid_urbn %>% 
  dplyr::slice_sample(n = 10000) %>% 
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

df <- data.table::as.data.table(ap_grid_urbn_sample) %>% 
  dplyr::rename("urbn_type" = "quintile") %>% 
  dplyr::mutate(urbn_type = factor(urbn_type, levels = c('3','1','2')))
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
  facet_wrap(. ~ urbn_type, nrow = 3, labeller = as_labeller(c("3"="City", "2"="Town/Suburb", "1"="Rural"))) +
  ggpubr::theme_pubr() +
  scale_color_manual(
    values = urbn_type.color.num,
    name = "Urban type",
    labels = urbn_type.labs.num
  ) +
  scale_fill_manual(
    values = urbn_type.color.num,
    name = "Urban type",
    labels = urbn_type.labs.num
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
inc_pc_20152 <- terra::resample(inc_pc_2015, pm.ap_raster2)
inc_pc_20152 <- terra::crop(inc_pc_20152, extent_raster)

# Filter out NA values directly on the rasters
pm.ap_raster_filtered <- terra::mask(pm.ap_raster2, pm.ap_raster2, maskvalue = NA)
inc_raster_filtered <- terra::mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- terra::values(pm.ap_raster_filtered)
inc_values <- terra::values(inc_raster_filtered)

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

# plots ------------------------------------------------------------------------
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
ap_grid_income_sample <- ap_grid_income %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))


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
pm.ap_raster2 <- terra::crop(pm.ap_raster, extent_raster)

pop_ge652 <- terra::project(pop_ge65, pm.ap_raster2)
pop_t2 <- terra::project(pop_t, pm.ap_raster2)
pop_ge652 <- terra::crop(pop_ge652, extent_raster)
pop_t2 <- terra::crop(pop_t2, extent_raster)
pop_elderly <- pop_ge652/pop_t2

# Filter out NA values directly on the rasters
pm.ap_raster_filtered <- terra::mask(pm.ap_raster2, pm.ap_raster2, maskvalue = NA)
elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)

# Convert the filtered rasters to data frames
pm_values <- terra::values(pm.ap_raster_filtered)
pop_elderly <- terra::values(elderly_raster_filtered)

# Remove NA values
valid_idx <- !is.na(pm_values) & !is.na(pop_elderly)
df_ap_eld <- data.frame(pm_concentration = pm_values[valid_idx],
                        pop_elderly_per = pop_elderly[valid_idx],
                        ctry_names = ctry_values[valid_idx])

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

# plots ------------------------------------------------------------------------
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
ap_grid_per_elderly_sample <- ap_grid_per_elderly %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

plot_grid_ap_per_elderly <- prob_jitter_plot(ap_grid_per_elderly_sample %>% 
                                              dplyr::rename(item = ap), 
                                            legend_title = 'Elderly proportion\nquintiles', 
                                            legend_type = 'quintiles',
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
# 1 1               2.76e-29           0.144           0.0899              0.103         0.0447 614074
# 2 2               1.44e- 1           0.193           0.170               0.170         0.0139 614074
# 3 3               1.93e- 1           0.237           0.215               0.214         0.0128 614074
# 4 4               2.37e- 1           0.306           0.268               0.266         0.0195 614074
# 5 5               3.06e- 1           1.00            0.437               0.385         0.141  614073



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
urbn_raster2 <- terra::resample(urbn_raster, pm.mort_raster2)
urbn_raster2 <- terra::crop(urbn_raster2, extent_raster)

# Define classification function
classify_function <- function(x) {
  ifelse(x <= 10, 0, 
         ifelse(x >= 11 & x <= 20, 1, # rural
                ifelse(x >= 21 & x <= 22, 2, # town/suburb
                       ifelse(x >= 23 & x <= 30, 3, NA)))) # urban
}

# Apply classification
urbn_raster_classified <- terra::app(urbn_raster2, classify_function)
names(urbn_raster_classified) <- "classification_layer"
urbn_raster_combined <- c(urbn_raster2, urbn_raster_classified)
terra::plot(urbn_raster_combined$classification_layer)

# Filter out NA values directly on the rasters
pm.mort_raster_filtered <- terra::mask(pm.mort_raster2, pm.mort_raster2, maskvalue = NA)
urbn_raster_filtered <- urbn_raster_combined$classification_layer
urbn_raster_combined_filtered <- terra::mask(urbn_raster_filtered, urbn_raster_filtered, maskvalue = NA)

# Convert the filtered rasters to data frames
pm.mort_values <- terra::values(pm.mort_raster_filtered)
urbn_values <- terra::values(urbn_raster_combined_filtered)

# Remove NA values
valid_idx <- !is.na(pm.mort_values) & !is.na(urbn_values)
df_mort_urbn <- data.frame(pm_mort = pm.mort_values[valid_idx],
                           urbn_type = urbn_values[valid_idx],
                           ctry_names = ctry_values[valid_idx])

df_mort_urbn_no0 <- df_mort_urbn[df_mort_urbn$pm_mort > 0,]
df_mort_urbn_no0 <- df_mort_urbn_no0[df_mort_urbn_no0$urbn_type > 0,]
df_mort_urbn_no0 <- unique(df_mort_urbn_no0)

df_mort_urbn_no0$urbn_type <- factor(df_mort_urbn_no0$urbn_type, levels = c('3','2','1'))

df_medi <- df_mort_urbn_no0 %>%
  dplyr::group_by(urbn_type) %>%
  dplyr::summarize(medi = mean(pm_mort)) %>% 
  dplyr::ungroup()

# plots ------------------------------------------------------------------------
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
deaths_grid_urbn_sample <- deaths_grid_urbn %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::mutate(quintile = dplyr::if_else(quintile == 1, 'Rural',
                                          dplyr::if_else(quintile == 2, 'Town/Suburb',
                                                         dplyr::if_else(quintile == 3, 'City', NA)))) %>% 
  dplyr::mutate(quintile = forcats::fct_relevel(quintile, 'City','Town/Suburb','Rural')) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

plot_grid_deaths_urbn <- prob_jitter_plot(deaths_grid_urbn_sample %>% 
                                            dplyr::rename(item = deaths) %>% 
                                            dplyr::filter(item < 4), 
                                          legend_title = 'Urban type',
                                          legend_type = 'urbn_type',
                                          ox_text = 'Premature deaths [Deaths per inhabitants]')
ggsave(
  file = paste0("figures/plot_grid_deaths_urbn.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_urbn[[1]],
  bg = 'white'
)
ggsave(
  file = paste0("figures/plot_grid_deaths_urbn.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_urbn[[1]] + theme(legend.position = 'none'), dpi = 300,
  bg = 'white'
)
ggsave(
  file = paste0("figures/plot_grid_deaths_urbn_text.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_urbn[[2]] + theme(legend.position = 'none'), dpi = 300,
  bg = 'white'
)

plot_urbntype_density <- ggplot(df_mort_urbn_no0 %>% 
                                  dplyr::filter(pm_mort < 5),
                                aes(x = pm_mort, 
                                    color = as.factor(urbn_type),
                                    fill = as.factor(urbn_type))) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(color = as.factor(urbn_type), xintercept = medi),
             data = df_medi, linewidth = 1
  ) +
  facet_grid(as.factor(urbn_type) ~ ., scales = 'free') +
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
    x = "Premature Deaths [Deaths per inhabitants]",
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
    # axis.text.y = element_blank(),
    # axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
  )

ggsave(
  file = paste0("figures/plot_grid_mort_urbntype_density.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_urbntype_density
)

## GRID - Deaths vs INCOME -----------------------------------------------------
# data processing --------------------------------------------------------------
inc_pc_20152 <- terra::project(inc_pc_2015, pm.mort_raster2)
inc_pc_20152 <- terra::resample(inc_pc_20152, pm.mort_raster2)
inc_pc_20152 <- terra::crop(inc_pc_20152, extent_raster)

# Filter out NA values directly on the rasters
pm.mort_raster_filtered <- terra::mask(pm.mort_raster2, pm.mort_raster2, maskvalue = NA)
inc_raster_filtered <- terra::mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)

# Convert the filtered rasters to data frames
pm.mort_values <- terra::values(pm.mort_raster_filtered)
inc_values <- terra::values(inc_raster_filtered)

# Remove NA values
valid_idx <- !is.na(pm.mort_values) & !is.na(inc_values)
df_mort_inc <- data.frame(pm_mort = pm.mort_values[valid_idx],
                          inc_per_capita = inc_values[valid_idx],
                          ctry_names = ctry_values[valid_idx])

df_mort_inc_no0 <- df_mort_inc[df_mort_inc$pm_mort > 0,]
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


# plots ------------------------------------------------------------------------
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
deaths_grid_income_sample <- deaths_grid_income %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))


plot_grid_deaths_income <- prob_jitter_plot(deaths_grid_income_sample %>% 
                                              dplyr::rename(item = deaths) %>% 
                                              dplyr::filter(item < 0.4), 
                                            legend_title = 'Income quintiles',
                                            legend_type = 'quintiles',
                                            ox_text = 'Premature deaths [Deaths per inhabitants]')
ggsave(
  file = paste0("figures/plot_grid_deaths_income.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_income[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_deaths_income.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_income[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_deaths_income_text.png"), height = 10, width = 18, units = "cm",
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
# 1 1             2039.      9051.       7206.         7496.     1382. 832628
# 2 2             9051.     11970.      10394.        10318.      823. 832627
# 3 3            11970.     15090.      13809.        13987.      911. 832627
# 4 4            15090.     17023.      15996.        15972.      541. 832627
# 5 5            17023.    191019.      19302.        18830.     2197. 832627

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
    x = "Premature Deaths [Deaths per inhabitants]",
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
  file = paste0("figures/plot_grid_income_density.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_inc_density
)


## GRID - Deaths vs ELDERLY ----------------------------------------------------
# data processing --------------------------------------------------------------
pop_ge652 <- terra::project(pop_ge65, pm.mort_raster2)
pop_ge652 <- terra::resample(pop_ge652, pm.mort_raster2)
pop_ge652 <- terra::crop(pop_ge652, extent_raster)

pop_t2 <- terra::project(pop_t, pm.mort_raster2)
pop_t2 <- terra::resample(pop_t2, pm.mort_raster2)
pop_t2 <- terra::crop(pop_t2, extent_raster)

pop_elderly <- pop_ge652/pop_t2

# Filter out NA values directly on the rasters
pm.mort_raster_filtered <- terra::mask(pm.mort_raster2, pm.mort_raster2, maskvalue = NA)
elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)

# Convert the filtered rasters to data frames
pm.mort_values <- terra::values(pm.mort_raster_filtered)
pop_elderly <- terra::values(elderly_raster_filtered)

# Remove NA values
valid_idx <- !is.na(pm.mort_values) & !is.na(pop_elderly)
df_mort_eld <- data.frame(pm_mort = pm.mort_values[valid_idx], 
                          pop_elderly_per = pop_elderly[valid_idx],
                          ctry_names = ctry_values[valid_idx])

df_mort_eld_no0 <- df_mort_eld[df_mort_eld$pm_mort > 0,]
df_mort_eld_no0 <- df_mort_eld_no0[df_mort_eld_no0$pop_elderly_per > 0,]
df_mort_eld_no0 <- unique(df_mort_eld_no0) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, is.finite(pop_elderly_per), pop_elderly_per < 1) %>% 
  dplyr::mutate(quintile_5 = as.factor(dplyr::ntile(pop_elderly_per, 5))) 

binned_data <- df_mort_eld_no0 %>%
  dplyr::mutate(quintile = as.factor(dplyr::ntile(pop_elderly_per, 50000))) %>% 
  dplyr::group_by(quintile, quintile_5) %>%
  dplyr::summarize(avg_eld = mean(pop_elderly_per),
                   avg_pm_q = mean(pm_mort)) %>% 
  dplyr::ungroup()

# plots ------------------------------------------------------------------------
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
deaths_grid_per_elderly_sample <- deaths_grid_per_elderly %>% 
  dplyr::slice_sample(n = 10000) %>% 
  dplyr::left_join(ctry_raster_values_mapping, by = c('ctry_names' = 'ID'))

plot_grid_deaths_per_elderly <- prob_jitter_plot(deaths_grid_per_elderly_sample %>% 
                                                   dplyr::rename(item = deaths) %>% 
                                                 dplyr::filter(item < 0.5), 
                                                 legend_title = 'Elderly proportion\nquintiles', 
                                                 legend_type = 'quintiles',
                                                 ox_text = 'Premature Deaths [Deaths per inhabitants]')
ggsave(
  file = paste0("figures/plot_grid_deaths_per_elderly.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_per_elderly[[1]]
)
ggsave(
  file = paste0("figures/plot_grid_deaths_per_elderly.png"), height = 10, width = 18, units = "cm",
  plot = plot_grid_deaths_per_elderly[[1]] + theme(legend.position = 'none'), dpi = 300
)
ggsave(
  file = paste0("figures/plot_grid_deaths_per_elderly_text.png"), height = 10, width = 18, units = "cm",
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
# 1 1               2.43e-40           0.143           0.0898              0.103         0.0445 612460
# 2 2               1.43e- 1           0.191           0.169               0.169         0.0137 612459
# 3 3               1.91e- 1           0.236           0.213               0.213         0.0127 612459
# 4 4               2.36e- 1           0.303           0.265               0.263         0.0191 612459
# 5 5               3.03e- 1           1.00            0.440               0.381         0.158  612459

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
    x = "Premature Deaths [Deaths per inhabitants]",
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
  file = paste0("figures/plot_grid_mort_elderly_density.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_eld_density
)




## COMBINED PLOTS --------------------------------------------------------------

# URBN TYPE
grob_a <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_ap_urbn_type.png")), 
                      top = grid::textGrob("a)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_b <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_ap_urbn.png")), 
                      top = grid::textGrob("b)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_c <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_deaths_urbn_type.png")), 
                      top = grid::textGrob("c)", x = unit(0, "npc"), just = "left", gp = grid::gpar(fontface = "bold", cex = 1)))
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_deaths_urbn.png")), 
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
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_deaths_urbn_text.png")), 
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
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_deaths_income.png")), 
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
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_deaths_income_text.png")), 
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
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_deaths_per_elderly.png")), 
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
grob_d <- gridExtra::arrangeGrob(grid::rasterGrob(png::readPNG("figures/plot_grid_deaths_per_elderly_text.png")), 
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



# ==============================================================================
#                              MACHINE LEARNING                                #
# ==============================================================================
source('R/machine_learning.R')

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
          fig_ox_label = "PM2.5 concentration [ug/m3]",type = 'ap',
          fix = T)


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
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = c("quintile","ctry"))) %>%
  dplyr::left_join(ap_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type))

# Plotting
pl <- ggplot() +
  geom_point(
    data = data %>% 
      dplyr::select(quintile, ctry, variable, value) %>% 
      dplyr::filter(variable != 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, color = quintile), size = 2, alpha = 0.85) + 
  geom_point(
    data = data %>% 
      dplyr::select(urbn_type, ctry, variable, value) %>% 
      dplyr::filter(variable == 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, fill = urbn_type), shape = 21, size = 2) +
  facet_grid(. ~ variable,
             labeller = labeller(variable = c(income_medi = "Income\nper capita",
                                              gini_medi = "Gini index",
                                              elderly_medi = "Elderly\nproportion",
                                              urbn_medi = "Urban\ntype"))) +
  scale_color_manual(
    values = quintiles.color,
    name = "Quintile",
    labels = quintiles.labs
  ) +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  labs(x = "PM2.5 concentration [ug/m3]", y = "") +
  theme_minimal()

ggsave(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,".pdf"), height = 10, width = 15, units = "cm",
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
          fig_ox_label = "Premature Deaths [Deaths per 1M inhabitants]",
          fix = T, type = 'deaths')


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
          fig_ox_label = "Premature Deaths [Deaths per 1M inhabitants]",
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
          fig_ox_label = "Premature Deaths [Deaths per 1M inhabitants]")  


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
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = c("quintile","ctry"))) %>%
  dplyr::left_join(deaths_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type))

# Plotting
pl <- ggplot() +
  geom_point(
    data = data %>% 
      dplyr::select(quintile, ctry, variable, value) %>% 
      dplyr::filter(variable != 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, color = quintile), size = 2, alpha = 0.85) + 
  geom_point(
    data = data %>% 
      dplyr::select(urbn_type, ctry, variable, value) %>% 
      dplyr::filter(variable == 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, fill = urbn_type), shape = 21, size = 2) +
  facet_grid(. ~ variable,
             labeller = labeller(variable = c(income_medi = "Income\nper capita",
                                              gini_medi = "Gini index",
                                              elderly_medi = "Elderly\nproportion",
                                              urbn_medi = "Urban\ntype"))) +
  scale_color_manual(
    values = quintiles.color,
    name = "Quintile",
    labels = quintiles.labs
  ) +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  labs(x = "Premature Deaths [Deaths per 1M inhabitants]", y = "") +
  theme_minimal()

ggsave(
  file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,".pdf"), height = 10, width = 15, units = "cm",
  plot = pl
)




## GRID - AP vs INCOME ----------------------------------------------------------------
ap_income_medi <- data.table::as.data.table(ap_grid_income_sample) %>%
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

ml_do_all(data, 7, 'withinCtry/ml_income_grid',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]",type = 'ap',
          fix = T)


## GRID - AP vs ELDERLY --------------------------------------------------------------
ap_elderly_medi <- data.table::as.data.table(ap_grid_per_elderly_sample) %>%
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
ap_urbntype_medi <- data.table::as.data.table(ap_grid_urbn_sample) %>%
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
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = c("quintile","ctry"))) %>%
  dplyr::left_join(ap_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type))

# Plotting
pl <- ggplot() +
  geom_point(
    data = data %>% 
      dplyr::select(quintile, ctry, variable, value) %>% 
      dplyr::filter(variable != 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, color = quintile), size = 2, alpha = 0.85) + 
  geom_point(
    data = data %>% 
      dplyr::select(urbn_type, ctry, variable, value) %>% 
      dplyr::filter(variable == 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, fill = urbn_type), shape = 21, size = 2) +
  facet_grid(. ~ variable,
             labeller = labeller(variable = c(income_medi = "Income\nper capita",
                                              elderly_medi = "Elderly\nproportion",
                                              urbn_medi = "Urban\ntype"))) +
  scale_color_manual(
    values = quintiles.color,
    name = "Quintile",
    labels = quintiles.labs
  ) +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  labs(x = "PM2.5 concentration [ug/m3]", y = "") +
  theme_minimal()

ggsave(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,"_grid.pdf"), height = 10, width = 15, units = "cm",
  plot = pl
)



## GRID - DEATHS vs INCOME ----------------------------------------------------------------
deaths_income_medi <- data.table::as.data.table(deaths_grid_income_sample) %>%
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
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 8, 'withinCtry/ml_income_grid',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "Premature Deaths [Deaths per 1M inhabitants]",
          fix = T, type = 'deaths')

## GRID - DEATHS vs ELDERLY --------------------------------------------------------------
deaths_elderly_medi <- data.table::as.data.table(deaths_grid_per_elderly_sample) %>%
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
  tibble::as_tibble() %>% 
  dplyr::mutate(quintile = paste0('Q',quintile)) %>%
  dplyr::arrange(quintile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'quintile', values_from = 'medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 3, 'withinCtry/ml_elderly_grid',
          fig_legend = "Elderly\nproportion\nquintile",
          fig_ox_label = "Premature Deaths [Deaths per 1M inhabitants]",
          type = 'deaths')

## GRID - DEATHS vs GINI ------------------------------------------------------------------
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

ml_do_all(data, 4, 'withinCtry/ml_gini_grid',
          fig_legend = "Gini\nindex\nquintile",type = 'deaths',
          fig_ox_label = "Premature Deaths [Deaths per 1M inhabitants]")  


## GRID - DEATHS vs URBN TYPE -------------------------------------------------------------
deaths_urbntype_medi <- data.table::as.data.table(deaths_grid_urbn_sample) %>%
  dplyr::select(ctry = ctry_code, urbn_type = quintile, deaths) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>%
  dplyr::group_by(urbn_type, ctry) %>% 
  dplyr::summarise(medi = quantile(deaths, 0.50)) %>% 
  dplyr::ungroup()


## GRID - DEATHS - figure -----------------------------------------------------------------
data_list <- list(
  deaths_income_medi %>%
    dplyr::rename_with(~ "income", contains("medi")),
  
  deaths_elderly_medi %>%
    dplyr::rename_with(~ "elderly", contains("medi"))
)

# Merge all datasets by quintile
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = c("quintile","ctry"))) %>%
  dplyr::left_join(deaths_urbntype_medi %>%
                     dplyr::rename_with(~ "urbn", contains("medi")) %>% 
                     dplyr::mutate(urbn_type = as.factor(urbn_type)),
                   by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-quintile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(quintile = as.factor(quintile),
                urbn_type = as.factor(urbn_type))

# Plotting
pl <- ggplot() +
  geom_point(
    data = data %>% 
      dplyr::select(quintile, ctry, variable, value) %>% 
      dplyr::filter(variable != 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, color = quintile), size = 2, alpha = 0.85) + 
  geom_point(
    data = data %>% 
      dplyr::select(urbn_type, ctry, variable, value) %>% 
      dplyr::filter(variable == 'urbn') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, fill = urbn_type), shape = 21, size = 2) +
  facet_grid(. ~ variable, scales = 'free',
             labeller = labeller(variable = c(income_medi = "Income\nper capita",
                                              elderly_medi = "Elderly\nproportion",
                                              urbn_medi = "Urban\ntype"))) +
  scale_color_manual(
    values = quintiles.color,
    name = "Quintile",
    labels = quintiles.labs
  ) +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  labs(x = "Premature Deaths [Deaths per 1M inhabitants]", y = "") +
  theme_minimal()

ggsave(
  file = paste0("figures/withinCtry/fig_deaths_var_",yy,"_",split_num_tag,"_grid.pdf"), height = 10, width = 15, units = "cm",
  plot = pl
)

