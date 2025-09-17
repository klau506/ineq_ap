require(eurostat)
library(ggplot2)
library(magrittr)
library(tmap)
tmap_options(check.and.fix = TRUE) 

source("R/utils.R")
source("R/zzz.R")

normalized <- T
split_num <- 5 #10 deciles, 5 quintiles
map <- T #T if plotted and saved, F otherwise
yy <- 2030
# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================
normalized_tag <- dplyr::if_else(normalized, '_norm100k', '')
split_num_tag <- dplyr::if_else(split_num == 5, 'quintile', 
                                dplyr::if_else(split_num == 10, 'decile',
                                               paste0('split_num_',split_num)))
## rfasst + socioeconomid data =================================================
ap_socioecon_sf <- get(load('ap_socioecon_sf3.RData'))
deaths_socioecon_sf <- get(load('deaths_socioecon_sf_newdeaths3.RData'))

rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 

ap <- get(load("data/rfasst_output/necp_m2_get_conc_pm25.ctry_agg.output.RData")) %>%
  dplyr::filter(year == yy)

deaths <- get(load(paste0("data/rfasst_output/necp_m3_get_mort_pm25.output.RData"))) %>%
  dplyr::select(region, year, age, sex, disease, value = GBD, scenario) %>% 
  dplyr::filter(year == yy)
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
}

## load spacial data ===========================================================
nuts3_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID) %>% 
  dplyr::filter(CNTR_CODE != 'TR')

## AP  =========================================================================
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

if (map) {
  plot_ap <- tm_shape(nuts3_plot_data,
                      projection = "EPSG:3035",
                      xlim = c(2400000, 6500000),
                      ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(ap_nuts3_sf) +
    tm_polygons("ap",
                title = "PM2.5\n[ug/m3]",
                palette = "Oranges",
                style = "cont",
                lwd = 0.2
    ) +
    tm_layout(legend.show = FALSE, frame = FALSE)
  
  tmap::tmap_save(plot_ap,
                  filename = paste0("figures/meth_sketch/plot_ap_nuts3.pdf"),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
}


# PM2.5 grid level
pm25_weighted <- terra::rast("data/rfasst_output/EU_NECP_LTT_2030_pm25_fin_weighted.tif")
png(filename = "figures/meth_sketch/plot_ap_grid_w.png",
    width = 100 * 3, height = 50 * 3, units = "mm", res = 300)
terra::plot(pm25_weighted, col = grDevices::terrain.colors(50), axes = FALSE, box = FALSE, legend = FALSE)
dev.off()


# PM2.5 regional level
pm25_regional <- get(load("data/rfasst_output/necp_m2_get_conc_pm25.ctry_agg.output.RData"))

pm25_regional <- as.data.frame(rmap::mapCountries) %>%
  dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
  dplyr::left_join(rfasst::fasst_reg,by="subRegionAlt") %>%
  dplyr::select(-subRegion) %>%
  dplyr::rename(subRegion=fasst_region) %>%
  dplyr::mutate(subRegionAlt=as.factor(subRegionAlt)) %>% 
  dplyr::left_join(get(load("figures/meth_sketch/pm25.map_data.RData")),
                 by = "subRegion",
                 relationship = "many-to-many")
pm25_regional <- sf::st_sf(pm25_regional, geometry = pm25_regional$geometry)

if (map) {
  plot_ap <- 
    tm_shape(pm25_regional %>% 
               dplyr::filter(year == 2005)) +
    tm_polygons("value",
                title = "PM2.5\n[ug/m3]",
                palette = "Oranges",
                style = "cont",
                lwd = 0.2
    ) +
    tm_layout(legend.show = FALSE, frame = FALSE)
  
  
  tmap::tmap_save(plot_ap,
                  filename = paste0("figures/meth_sketch/plot_ap_regional.pdf"),
                  width = 100, height = 50, units = "mm", dpi = 300
  )
}

## DEATHS  =====================================================================
deaths_nuts3 <- deaths %>%
  dplyr::filter(
    nchar(region) > 3,
    !stringr::str_detect(region, 'TR')
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

if (map) {
  plot_deaths <- 
    tm_shape(nuts3_plot_data,
             projection = "EPSG:3035",
             xlim = c(2400000, 6500000),
             ylim = c(1320000, 5650000)
    ) +
    tm_fill("lightgrey") +
    tm_shape(deaths_nuts3_sf %>%
               dplyr::filter(
                 sex == "Both"
               )) +
    tm_polygons("deaths",
                title = "Premature deaths\n[Deaths per 1M inhabitants]",
                palette = "Oranges",
                style = "cont",       
                lwd = 0.2
    ) +
    tm_layout(legend.show = FALSE, frame = FALSE)
  
  tmap::tmap_save(plot_deaths,
                  filename = paste0("figures/meth_sketch/plot_deaths_nuts3", normalized_tag, ".pdf"),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
}

## DEATHS - GRID  ==============================================================

extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)

pm.pre <- terra::rast(paste0('../../GitHub/rfasst_v2/output/m2/pm25_gridded/raster_grid/EU_NECP_LTT_2030_pm25_fin_weighted.tif'))
pm.pre <- terra::crop(pm.pre, extent_raster)

pm.mort_yy <- get(load(paste0('../../GitHub/rfasst_v2/output/m3/pm25_gridded/EUR_grid/pm.mort_mat_2030_norm_EU_NECP_LTT.RData')))
vec <- as.vector(pm.mort_yy[['total']])
pm.mort_rast <- terra::setValues(pm.pre, vec)

eu_mask_raster <- terra::rast(terra::ext(eu_mask), resolution = 0.01)  # or define your own resolution
eu_mask_raster2 <- terra::rasterize(eu_mask, eu_mask_raster, field = "iso_n3")
eu_mask_raster2 <- terra::crop(eu_mask_raster2, extent_raster)
eu_mask_raster2 <- terra::resample(eu_mask_raster2, pm.mort_rast)

pm.mort_raster2_europe <- terra::mask(pm.mort_rast, eu_mask_raster2)

pdf(paste0("figures/meth_sketch/plot_deaths_grid.pdf"), width = 50, height = 50)
r <- raster::raster(pm.mort_raster2_europe)
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
dev.off()

## AP vs urbn_type  ============================================================
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
  file = paste0("figures/meth_sketch/plot_urbntype_density_ap_nuts3.pdf"), height = 5, width = 10, units = "cm",
  plot = plot_ap_urbn_type[[1]] + theme(legend.position = 'none',
                                        panel.border = element_blank(),
                                        axis.title = element_blank()),
  dpi = 300
)


# NOTE: grid figure in the analysis_clear.R script to avoid loading so much data here

#### GCAM - EUR ================================================================
# Load world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Fix invalid geometries and wrap dateline
world <- sf::st_make_valid(world)
world <- sf::st_wrap_dateline(world, options = c("WRAPDATELINE=YES"))

# Define EU country codes based on nuts3_plot_data
eu_countries <- unique(nuts3_plot_data$CNTR_CODE)

# Add a column to classify EU and non-EU countries
world <- world %>%
  dplyr::mutate(eu_status = ifelse(iso_a2 %in% eu_countries, "EU", "Non-EU"))
world <- world %>% dplyr::filter(continent != "Antarctica")
world <- world %>% dplyr::filter(sf::st_area(geometry) > units::set_units(1000, "km^2"))

plot <- tm_shape(world) +
  tm_polygons("eu_status",
              projection = "EPSG:3035",
              palette = c("blue", "lightgrey"),
              title = "Country Type",
              lwd = 0.2) +
  tm_layout(legend.show = FALSE, frame = FALSE)

tmap::tmap_save(plot,
                filename = paste0("figures/meth_sketch/plot_gcameur.pdf"),
                width = 100, height = 50, units = "mm", dpi = 300
)
