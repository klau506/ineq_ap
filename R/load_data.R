###############################################################################

# Study                 : Health impacts and socioeconomic inequalities of future outdoor air pollution in an NECP-compliant Europe
# Date                  : Oct. 2025
# Author                : Clàudia Rodés-Bachs
# Institute             : BC3-Basque Centre for Climate Change
# Description           : Helper script to load all necessary data
# Re-usage instructions : Execute main.R, which will automatically run this script

###############################################################################

# CONSTANTS -------------------------------------------------------------------
normalized_tag <- dplyr::if_else(normalized, '_norm100k', '')
norm_grid_tag <- dplyr::if_else(normalized, '_norm', '')
split_num_tag <- dplyr::if_else(split_num == 5, 'quintile', 
                                dplyr::if_else(split_num == 10, 'decile',
                                               paste0('split_num_',split_num)))
sample_ctry_size <- 0.5

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




# NUTS3 data -------------------------------------------------------------------
ap_socioecon_sf <- get(load('ap_socioecon_sf-Rev1.1.RData')) %>% 
  dplyr::filter(!stringr::str_detect(geo, 'TR'))
deaths_socioecon_sf <- get(load('deaths_socioecon_sf-Rev1.1.RData')) %>% 
  dplyr::filter(!stringr::str_detect(geo, 'TR'))

rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 

rfasst_ctry_pop <- rfasst::pop.all.ctry_ctry.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value)  

ap <- read.csv("data/rfasst_output/EU_NECP_LTT-Rev1.1_2030_WORLD-NUTS3_pm25_avg.csv") %>%
  dplyr::rename(region = id_code,
                value = pm25_avg) %>% 
  dplyr::distinct()

deaths <- get(load(paste0("data/rfasst_output/necp_m3_get_mort_pm25-Rev1.1.output.RData"))) %>%
  dplyr::select(region, year, age, sex, disease, value = GBD, scenario) %>% 
  dplyr::filter(sex == 'Both')

ap <- ap %>% 
  dplyr::filter(year == yy)

deaths <- deaths %>% 
  dplyr::filter(year == yy)

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
  dplyr::select(-FID) %>% 
  dplyr::filter(!CNTR_CODE %in% c('CY','TR'))
ctry_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 0, year = 2021) %>%
  dplyr::select(-FID) %>% 
  dplyr::filter(!geo %in% c('CY','TR'))



# GRID data --------------------------------------------------------------------

# download country boundaries (scale = "medium" is usually good enough)
countries_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
turkey <- countries_sf[countries_sf$name == "Turkey", ]
# convert to SpatVector for terra
countries_vect <- terra::vect(countries_sf)

# reference data
extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)
pm.pre <- terra::rast(paste0('data/rfasst_output/EU_NECP_LTT-Rev1.1_2030_pm25_fin_weighted.tif'))
pm.pre <- terra::crop(pm.pre, extent_raster)

europe_shp <- rnaturalearth::ne_countries(continent = "Europe", returnclass = "sf") %>% 
  dplyr::filter(!adm0_a3 %in% c('RUS','BLR','UKR'))
eu_mask <- terra::vect(europe_shp)
eu_mask <- terra::crop(eu_mask, extent_raster)
eu_mask[!is.na(eu_mask)] <- 1
eu_mask[is.na(eu_mask)] <- 0

eu_mask_raster <- terra::rast(terra::ext(eu_mask), resolution = 0.01)
eu_mask_raster2 <- terra::rasterize(eu_mask, eu_mask_raster, field = "iso_n3")
eu_mask_raster2 <- terra::crop(eu_mask_raster2, extent_raster)
eu_mask_raster2 <- terra::resample(eu_mask_raster2, pm.pre)

ctry_raster <- europe_shp %>% 
  dplyr::select(ctry_code = iso_a2_eh, geometry)
ctry_raster <- terra::rasterize(ctry_raster, pm.pre, field = "ctry_code")
ctry_values <- terra::values(ctry_raster)
ctry_raster_values_mapping <- terra::cats(ctry_raster)[[1]]

# rfasst and socioeconomic data
pm.ap_raster <- terra::rast("data/rfasst_output/EU_NECP_LTT-Rev1.1_2030_pm25_fin_weighted.tif")
pm.mort_raster <- get(load(paste0("data/rfasst_output/pm.mort_mat_2030",norm_grid_tag,"_EU_NECP_LTT-Rev1.1.RData"))); rm(pm.mort_yy); gc()
inc_pc_2015 <- terra::rast("data/High-resolution_Downscaling/Europe_disp_inc_2015.tif")
urbn_raster <- terra::rast("data/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2030_GLOBE_R2023A_54009_1000_V2_0_reproj2.tif")
pop_ge65 <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-Y_GE65_2021_V2.tiff")
pop_t <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-T_2021_V2.tiff")
pm.weights_raster <- terra::rast('data/rfasst_output/pm25_weights_rast_rfasstReg.tif')

pm.ap_raster2 <- terra::crop(pm.ap_raster, extent_raster)
pm.ap_raster2_europe <- terra::mask(pm.ap_raster2, eu_mask)
vec <- as.vector(pm.mort_raster[['total']])
if (normalized) {
  vec <- vec * 1e6 # deaths / 1Mpeople
} else {
  # vec <- log(vec)
}
pm.mort_raster <- terra::setValues(pm.ap_raster2, vec)
pm.mort_raster2 <- terra::crop(pm.mort_raster, extent_raster)
pm.mort_raster2_europe <- terra::mask(pm.mort_raster2, eu_mask_raster2)
pm.weights_raster2 <- terra::crop(pm.weights_raster, extent_raster)


# Diagnostic data
prj <- rgcam::loadProject('data/gcam-eur-necp-Rev1.1.dat')
sector_map <- read.csv('data/sector_map.csv')
ghg_map <- read.csv('data/ghg_map.csv')
sel_ghg <- c('BC', 'OC', 'CO2', 'N2O', 'NH3', 'NOx', 'CH4', 'SO2', 'VOC')
