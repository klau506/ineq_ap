require(eurostat)
require(sf)
library(tmap)

source("R/utils.R")
source("R/zzz.R")

# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================

# population data: https://hub.worldpop.org/doi/10.5258/SOTON/WP00670 // BUILT-POP PROJ R2020 for projections
# age - sex population data: https://hub.worldpop.org/geodata/listing?id=88
## age - sex population data: https://ec.europa.eu/eurostat/web/gisco/geodata/population-distribution/geostat
## urban type data: https://human-settlement.emergency.copernicus.eu/download.php?ds=smod
## disposable income: https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/QRBINB
# gdp data (2030-2100): https://zenodo.org/records/5880037
# cdd data: https://doi.pangaea.de/10.1594/PANGAEA.903123?format=html#download

# TODO - download data and adapt the code below


##### LOAD POPULATION TIF RASTER FILES ====================================
# Load necessary libraries
library(httr)
library(terra)

# List of countries (replace these with your actual list of country codes)
countries <- c("DZA", "USA", "CAN")  # Example: Algeria (DZA), USA, Canada

# Base URL for the data
base_url <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/"

# Function to download and read the .tif file for each country
download_and_read <- function(ctry_code) {
  # Construct the URL for the current country
  url <- paste0(base_url, ctry_code, "/", ctry_code, "_ppp_2020_1km_Aggregated.tif")
  
  # Download the file (you can use httr or curl)
  tmp_file <- tempfile(fileext = ".tif")
  GET(url, write_disk(tmp_file, overwrite = TRUE))
  
  # Read the downloaded .tif file using terra
  rast <- rast(tmp_file)
  return(rast)
}

# List to store the rasters
rasters <- list()

# Loop through countries, download and read .tif files
for (ctry in countries) {
  rast <- download_and_read(ctry)
  rasters <- append(rasters, list(rast))
}

# Stack the rasters together
rasters_stack <- do.call(c, rasters)  # Combine all rasters into one stack

# Aggregate the rasters (if necessary)
# This step depends on how you want to aggregate the data. 
# Example: summing the rasters
aggregated_raster <- sum(rasters_stack)

# Plot the aggregated raster
plot(aggregated_raster)

# Optionally, save the aggregated raster to a file
writeRaster(aggregated_raster, "aggregated_population_2020.tif", overwrite = TRUE)


##### END LOAD POPULATION TIF RASTER FILES ====================================

##### AP vs INCOME ============================================================
inc <- terra::rast('data/dataverse_files/Europe_disp_inc_2015.tif')
terra::plot(inc)
ap <- terra::rast('C:/Users/claudia.rodes/Documents/GitHub/rfasst_v2/output/m2/pm25_gridded/raster_grid/2020_pm25_fin_weighted.tif')

# Crop PM25 to NUTS-3 extent
ctry_nuts_sf <- rfasst::ctry_nuts_sf %>%
  dplyr::filter(id_code %in% (rfasst::nuts_europe_sf %>%
                                dplyr::pull(id_code)))
ap_eur <- terra::crop(ap, terra::ext(ctry_nuts_sf))
terra::plot(ap_eur)

inc_res <- terra::resample(inc, ap_eur, method = "bilinear")
ext <- terra::intersect(terra::ext(inc_res), terra::ext(ap_eur))

ext_inc_res <- terra::crop(inc_res, terra::ext(ext))
ext_ap_eur <- terra::crop(ap_eur, terra::ext(ext))

stacked_rasters <- c(ext_inc_res, ext_ap_eur)

terra::plot(stacked_rasters$Europe_disp_inc_2015)
terra::plot(stacked_rasters$layer)

stacked_rasters_df = data.table::as.data.table(stacked_rasters)
stacked_rasters_df2 <- stacked_rasters_df %>% 
  dplyr::filter(rowSums(is.na(.)) == 0,
                layer != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(Europe_disp_inc_2015, 5)))

a = stacked_rasters_df2 %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::summarise(
    count = n(),                 # Number of observations
    mean = mean(layer, na.rm = TRUE),
    median = median(layer, na.rm = TRUE),
    min = min(layer, na.rm = TRUE),
    max = max(layer, na.rm = TRUE),
    sd = sd(layer, na.rm = TRUE)  # Standard deviation
  )
# # A tibble: 5 x 7
# quintile   count  mean median   min   max    sd
# <fct>      <int> <dbl>  <dbl> <dbl> <dbl> <dbl>
# 1 1        1122571  8.76   8.07     0  22.0  2.23
# 2 2        1122571  9.69   9.58     0  21.3  2.08
# 3 3        1122571  8.83   8.68     0  23.0  2.92
# 4 4        1122571  9.05   8.83     0  23.9  3.43
# 5 5        1122571 10.3   11.1      0  23.7  3.99

plot <- 
  ggplot(stacked_rasters_df2, 
         aes(x = Europe_disp_inc_2015, y = layer, color = quintile)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_minimal() +
  labs(x = "Income pc", y = "PM2.5 concentration [ug/m3]")

ggsave(plot,
       file = paste0("figures/grid/plot_income_ap_scatterplot.pdf"), height = 15, width = 15, units = "cm"
       )
plot2 <- 
  ggplot(stacked_rasters_df2 %>% 
           dplyr::filter(quintile %in% c(1,5)), 
         aes(x = Europe_disp_inc_2015, y = layer, color = quintile)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_minimal() +
  labs(x = "Income pc", y = "PM2.5 concentration [ug/m3]")

ggsave(plot2,
       file = paste0("figures/grid/plot_income_ap_scatterplot2.pdf"), height = 15, width = 15, units = "cm"
)

##### AP vs GDPpc TODO ============================================================
inc <- terra::rast('data/dataverse_files/Europe_disp_inc_2015.tif')
terra::plot(inc)
ap <- terra::rast('C:/Users/claudia.rodes/Documents/GitHub/rfasst_v2/output/m2/pm25_gridded/raster_grid/2020_pm25_fin_weighted.tif')

# Crop PM25 to NUTS-3 extent
ctry_nuts_sf <- rfasst::ctry_nuts_sf %>%
  dplyr::filter(id_code %in% (rfasst::nuts_europe_sf %>%
                                dplyr::pull(id_code)))
ap_eur <- terra::crop(ap, terra::ext(ctry_nuts_sf))
terra::plot(ap_eur)

inc_res <- terra::resample(inc, ap_eur, method = "bilinear")
ext <- terra::intersect(terra::ext(inc_res), terra::ext(ap_eur))

ext_inc_res <- terra::crop(inc_res, terra::ext(ext))
ext_ap_eur <- terra::crop(ap_eur, terra::ext(ext))

stacked_rasters <- c(ext_inc_res, ext_ap_eur)

terra::plot(stacked_rasters$Europe_disp_inc_2015)
terra::plot(stacked_rasters$layer)

stacked_rasters_df = data.table::as.data.table(stacked_rasters)
stacked_rasters_df2 <- stacked_rasters_df %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(Europe_disp_inc_2015, 5)))

a = stacked_rasters_df2 %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::summarise(
    count = n(),                 # Number of observations
    mean = mean(layer, na.rm = TRUE),
    median = median(layer, na.rm = TRUE),
    min = min(layer, na.rm = TRUE),
    max = max(layer, na.rm = TRUE),
    sd = sd(layer, na.rm = TRUE)  # Standard deviation
  )
# # A tibble: 5 x 7
# quintile   count  mean median   min   max    sd
# <fct>      <int> <dbl>  <dbl> <dbl> <dbl> <dbl>
#   1 1        1122571  8.76   8.07     0  22.0  2.23
# 2 2        1122571  9.69   9.58     0  21.3  2.08
# 3 3        1122571  8.83   8.68     0  23.0  2.92
# 4 4        1122571  9.05   8.83     0  23.9  3.43
# 5 5        1122571 10.3   11.1      0  23.7  3.99

plot <- 
  ggplot(stacked_rasters_df2, 
         aes(x = Europe_disp_inc_2015, y = layer, color = quintile)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_minimal() +
  labs(x = "Income pc", y = "PM2.5 concentration [ug/m3]")

ggsave(plot,
       file = paste0("figures/grid/plot_income_ap_scatterplot.pdf"), height = 15, width = 15, units = "cm"
       )
plot2 <- 
  ggplot(stacked_rasters_df2 %>% 
           dplyr::filter(quintile %in% c(1,5)), 
         aes(x = Europe_disp_inc_2015, y = layer, color = quintile)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_minimal() +
  labs(x = "Income pc", y = "PM2.5 concentration [ug/m3]")

ggsave(plot2,
       file = paste0("figures/grid/plot_income_ap_scatterplot2.pdf"), height = 15, width = 15, units = "cm"
)

######





a = sf::st_read('data/grid_accessibility_health.gpkg')
aa = a %>% dplyr::select(health_2023_n3, geom)

ggplot(data = aa) +
  geom_sf(aes(fill = health_2023_n3)) +
  scale_fill_viridis_c() +  # Use a color scale for better visualization
  theme_minimal() +
  labs(title = "Health 2023 N3", fill = "Value")

#### rfasst output =============================================================
## load dummy PM2.5 concentration & premature deaths by NUTS3
ap <- get(load("data/rfasst_output/necp_m2_get_conc_pm25.ctry_nuts.output.RData"))
deaths <- get(load("data/rfasst_output/necp_m3_get_mort_pm25.output.RData")) %>%
  dplyr::select(region, year, age, sex, disease, value = GBD, scenario)


#### spacial data ==============================================================
## spacial data for plots
nuts3_plot_data <- get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID)
sf::st_write(nuts3_plot_data, "data/Replication data/Global_Inputs/nuts3_plot_data.shp",
             delete_layer = TRUE,
             layer_options = "ENCODING=UTF-8"
)


#### eurostat nuts3 data - ONLY HDD-CDD USED ===================================
## read socioeconomic data
gdp <- eurostat::get_eurostat("nama_10r_3gdp") %>% # gdp NUTS3
  dplyr::filter(unit == "PPS_EU27_2020_HAB") %>% # Purchasing power standard (PPS, EU27 from 2020), per inhabitant
  dplyr::filter(grepl("2021", TIME_PERIOD)) %>% # Much more data in 2021
  dplyr::mutate(unit_GDP = "PPS_EU27_2020_HAB") %>%
  dplyr::select(geo, unit_GDP, GDPpc = values)

pop <- eurostat::get_eurostat("nama_10r_3popgdp") %>% # pop NUTS3
  dplyr::filter(grepl("2021", TIME_PERIOD)) %>% # Much more data in 2021
  dplyr::mutate(unit_POP = "THS") %>%
  dplyr::select(geo, unit_POP, POP = values)

pop_ctzha <- eurostat::get_eurostat("cens_21ctzha_r3") %>% # pop_detailed NUTS3
  dplyr::filter(housing == "TOTAL", citizen == "TOTAL") %>%
  dplyr::select(geo, age_group = age, age = values)

hdd_cdd <- eurostat::get_eurostat("nrg_chddr2_a") %>% # HDD & CDD NUTS3
  dplyr::filter(grepl("2023", TIME_PERIOD)) %>%
  tidyr::pivot_wider(names_from = "indic_nrg", values_from = "values") %>%
  dplyr::mutate(
    unit_CDD = "NR",
    unit_HDD = "NR"
  ) %>%
  dplyr::select(geo, CDD, HDD, unit_CDD, unit_HDD)

grid_dt <- eurostat::get_eurostat_geospatial(year = 2021) %>% # 2024 available, but not for URBN_TYPE, which will not change much from 2021 to 2024
  as.data.frame() %>%
  dplyr::select(LEVL_CODE, NUTS_ID, CNTR_CODE, URBN_TYPE, geo)


socioecon_dt <- purrr::reduce(
  list(grid_dt, gdp, pop, pop_ctzha, hdd_cdd),
  full_join,
  by = "geo"
) %>%
  dplyr::select(
    -starts_with("unit"),
    starts_with("unit")
  ) %>%
  dplyr::filter(LEVL_CODE == 3)


pop_employed <- eurostat::get_eurostat("nama_10r_3empers") # pop_employed NUTS3
gva_act <- eurostat::get_eurostat("nama_10r_3gva") # gross value added by act NUTS3
# pop_fhcs <- eurostat::get_eurostat('cens_21fhcs_r3') # pop_detailed NUTS3
income <- eurostat::get_eurostat("nama_10r_2hhinc") # income NUTS2 na_item == 'B6N'
# poverty <- eurostat::get_eurostat('ilc_peps11n') # poverty NUTS2


hdd_cdd_sf <- data.table::as.data.table(hdd_cdd) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
hdd_cdd_sf <- sf::st_sf(hdd_cdd_sf, geometry = hdd_cdd_sf$geometry)
sf::st_write(hdd_cdd_sf, "data/tmp/hdd_cdd_sf.shp", append = FALSE)



#### eurostat grided data ======================================================
# Source: https://ec.europa.eu/eurostat/web/gisco/geodata/population-distribution/geostat
# grided socioeconomic data from EUROSTAT - people by age & sex
temp_dir <- tempdir()
unzip("data/Eurostat_Census-GRID_2021_V2.1.zip", exdir = temp_dir)
eur_data <- sf::st_read(file.path(temp_dir, "ESTAT_Census_2021_V2.gpkg"))
eur_data <- eur_data %>%
  dplyr::select(-ends_with("_CI")) %>%
  dplyr::filter(Y_GE65 >= 0, T >= 0) # avoid wired cell values

# merge socioeconomic gridded data with NUTS3 regions
grid_eur_sf <- sf::st_transform(eur_data, crs = st_crs(nuts3_plot_data))
grid_eur_sf_nuts3 <- sf::st_join(grid_eur_sf, nuts3_plot_data, join = st_intersects)
sf::st_write(grid_eur_sf_nuts3, "data/tmp/grid_eur_sf_nuts3.shp", append = FALSE)


#### harmonized income data ====================================================
# Source: https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/TTIOKI
harm_data <- sf::st_read("data/Replication data/merged_output_2019_wID_clean.shp") %>%
  dplyr::mutate(
    Population = as.numeric(Population),
    Gini = as.numeric(Gini)
  )

# check mismatched data (harmonized socioeconomic data NUTS3 code does not match the corresponding CTRY)
iso2_iso3 <- rfasst::ctry_nuts3_codes %>%
  dplyr::select(ISO2, ISO3) %>%
  dplyr::distinct()

harm_data_repair <- data.table::as.data.table(harm_data) %>%
  dplyr::select("Year", "AU_name", "AU_code", "ISO", "NUTS2", "id" = "id_1", "NUTS_ID", "CNTR_CODE", "geo") %>%
  dplyr::left_join(iso2_iso3,
                   by = c("ISO" = "ISO3")
  ) %>%
  dplyr::left_join(iso_nuts2 = ISO2) %>%
  dplyr::mutate(geo_nuts2 = stringr::str_sub(NUTS_ID, 1, 2)) %>%
  dplyr::mutate(equal = iso_nuts2 == geo_nuts2) %>%
  dplyr::filter(!equal) # OK!!

harm_data_sf <- data.table::as.data.table(harm_data) %>%
  dplyr::select(-geometry) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
harm_data_sf <- sf::st_sf(harm_data_sf, geometry = harm_data_sf$geometry)
sf::st_write(harm_data_sf, "data/tmp/harm_data_sf.shp", append = FALSE)

# ==============================================================================
#                              COMPUTE INDICATORS                              #
# ==============================================================================

#### DISPOSABLE INCOME & GINI ==================================================
harm_data_income_nuts3 <- data.table::as.data.table(harm_data) %>%
  dplyr::mutate(Id = row_number()) %>%
  dplyr::select(Id, Year, ISO, geo, Population, Disp_Inc_P) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # pop-weighted disposable income:
  # Disp_Inc_P_nuts3 = SUM_regInNUTS3(Pop * Disp_Inc_P) / SUM_regInNUTS3(Pop)
  dplyr::mutate(Disp_Inc_P_pw = Disp_Inc_P * Population) %>%
  dplyr::group_by(Year, geo, ISO) %>%
  dplyr::mutate(Population_nuts3 = sum(Population)) %>%
  dplyr::mutate(Disp_Inc_P_nuts3 = sum(Disp_Inc_P_pw) / Population_nuts3) %>%
  dplyr::ungroup() %>%
  dplyr::select(Year, ISO, geo, Population_nuts3, Disp_Inc_P_nuts3) %>%
  dplyr::distinct()

harm_data_gini_nuts3 <- data.table::as.data.table(harm_data) %>%
  dplyr::mutate(Id = row_number()) %>%
  dplyr::select(Id, Year, ISO, geo, Population, Disp_Inc_P, Gini) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  tibble::as_tibble() %>%
  # weighted gini index:
  gcamdata::left_join_error_no_match(compute_gini_by_nuts3(.),
                                     by = c("Year", "geo")
  ) %>%
  dplyr::select(Year, ISO, geo, Gini_nuts3) %>%
  dplyr::distinct()

harm_data_nuts3 <- dplyr::full_join(
  harm_data_income_nuts3,
  harm_data_gini_nuts3
)

harm_data_nuts3_sf <- harm_data_nuts3 %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
harm_data_nuts3_sf <- sf::st_sf(harm_data_nuts3_sf, geometry = harm_data_nuts3_sf$geometry)


plot_income <- tm_shape(nuts3_plot_data,
                        projection = "EPSG:3035",
                        xlim = c(2400000, 6500000),
                        ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(harm_data_nuts3_sf %>%
             dplyr::select(Disp_Inc_P_nuts3, geometry) %>%
             dplyr::filter(rowSums(is.na(.)) == 0)) +
  tm_polygons("Disp_Inc_P_nuts3",
              title = "Disposable Income\n[2015 PPP EU27 €]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_income,
                filename = "figures/plot_income.pdf",
                width = 100, height = 100, units = "mm", dpi = 300
)

plot_gini <- tm_shape(nuts3_plot_data,
                      projection = "EPSG:3035",
                      xlim = c(2400000, 6500000),
                      ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(harm_data_nuts3_sf %>%
             dplyr::select(Gini_nuts3, geometry) %>%
             dplyr::filter(rowSums(is.na(.)) == 0)) +
  tm_polygons("Gini_nuts3",
              title = "Gini\nIndex",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_gini,
                filename = "figures/plot_gini.pdf",
                width = 100, height = 100, units = "mm", dpi = 300
)




#### ELDERLY POPULATION (%) ====================================================
# compute Y_GE65 (elderly people) percentage by NUTS3 region
grid_sf_elderly_v2 <- as.data.frame(grid_eur_sf_nuts3) %>%
  dplyr::filter(T > 0, Y_GE65 >= 0) %>%
  dplyr::select(GRD_ID, Y_GE65, Y_TOT = T, geo)

grid_sf_elderly_v3 <- unique(grid_sf_elderly_v2) %>%
  dplyr::group_by(geo) %>%
  dplyr::summarise(
    Y_GE65 = sum(Y_GE65),
    Y_TOT = sum(Y_TOT)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(per_elderly = ifelse(!is.na(Y_GE65), Y_GE65 / Y_TOT, NA_real_)) %>%
  dplyr::group_by(geo) %>%
  dplyr::summarise(mean_per_elderly = mean(per_elderly, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  as.data.frame() %>%
  # merge with NUTS3 geometries
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )

grid_sf_elderly_v3 <- sf::st_sf(grid_sf_elderly_v3, geometry = grid_sf_elderly_v3$geometry)

plot_elderly <- tm_shape(nuts3_plot_data,
                         projection = "EPSG:3035",
                         xlim = c(2400000, 6500000),
                         ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(grid_sf_elderly_v3) +
  tm_polygons("mean_per_elderly",
              title = "Elderly people [%]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)


tmap::tmap_save(plot_elderly,
                filename = "figures/plot_elderly.pdf",
                width = 100, height = 100, units = "mm", dpi = 300
)



#### URBN_TYPE (1 = cities, 2 = towns and suburbs, 3 = rural) ==================

grid_sf_urbntype_v2 <- as.data.frame(grid_eur_sf_nuts3) %>%
  dplyr::select(URBN_TYPE, geo) %>%
  dplyr::distinct() %>%
  dplyr::mutate(URBN_TYPE = factor(URBN_TYPE,
                                   levels = c(1, 2, 3),
                                   labels = c("City", "Town/Suburb", "Rural")
  )) %>%
  as.data.frame() %>%
  # merge with NUTS3 geometries
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )

grid_sf_urbntype_v2 <- sf::st_sf(grid_sf_urbntype_v2, geometry = grid_sf_urbntype_v2$geometry)

plot_urbtype <- tm_shape(nuts3_plot_data,
                         projection = "EPSG:3035",
                         xlim = c(2400000, 6500000),
                         ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(grid_sf_urbntype_v2) +
  tm_polygons("URBN_TYPE",
              title = "Urban type",
              palette = c("#C288B0", "#C3D2D5", "#92DEC3")
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_urbtype,
                filename = "figures/plot_urbtype.pdf",
                width = 100, height = 100, units = "mm", dpi = 300
)


#### CDD (very hot days when cooling is needed) ================================

grid_sf_cdd_v2 <- hdd_cdd %>%
  dplyr::filter(nchar(geo) == 5) %>% # only NUTS3
  dplyr::select(-starts_with("unit")) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
grid_sf_cdd_v2 <- sf::st_sf(grid_sf_cdd_v2, geometry = grid_sf_cdd_v2$geometry)


plot_cdd <- tm_shape(nuts3_plot_data,
                     projection = "EPSG:3035",
                     xlim = c(2400000, 6500000),
                     ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(grid_sf_cdd_v2) +
  tm_polygons("CDD",
              title = "CDD [NR]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_cdd,
                filename = "figures/plot_cdd.pdf",
                width = 100, height = 100, units = "mm", dpi = 300
)



# ==============================================================================
#                                   JOIN DATA                                  #
# ==============================================================================




harm_socioeconomic_nuts_sf <- data.table::as.data.table(grid_sf_urbntype_v2) %>%
  dplyr::select(-geometry) %>%
  dplyr::full_join(data.table::as.data.table(grid_sf_cdd_v2) %>%
                     dplyr::select(-geometry), by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(grid_sf_elderly_v3) %>%
                     dplyr::select(-geometry), by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(harm_data_nuts3_sf) %>%
                     dplyr::select(-geometry), by = "geo") %>%
  # merge with NUTS3 geometries
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
harm_socioeconomic_nuts_sf <- sf::st_sf(harm_socioeconomic_nuts_sf, geometry = harm_socioeconomic_nuts_sf$geometry)

sf::st_write(harm_socioeconomic_nuts_sf, "data/tmp/harm_socioeconomic_nuts_sf.shp", append = FALSE)
