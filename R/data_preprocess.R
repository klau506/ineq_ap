require(eurostat)
require(sf)
library(tmap)

source('R/utils.R')
source('R/zzz.R')

# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================

#### rfasst output =============================================================
## load dummy PM2.5 concentration & premature deaths by NUTS3
ap <- get(load('data/rfasst_output/tmp_m2_get_conc_pm25.ctry_nuts.output.RData'))
deaths <- get(load('data/rfasst_output/tmp_m3_get_mort_pm25.output.RData')) %>% 
  dplyr::select(region, year, age, sex, disease, value = GBD, scenario)


#### spacial data ==============================================================
## spacial data for plots
nuts3_plot_data <- get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>% 
  dplyr::select(-FID)
sf::st_write(nuts3_plot_data, "data/Replication data/Global_Inputs/nuts3_plot_data.shp", 
         delete_layer = TRUE,
         layer_options = "ENCODING=UTF-8")


#### eurostat nuts3 data - ONLY HDD-CDD USED ===================================
## read socioeconomic data
gdp <- eurostat::get_eurostat('nama_10r_3gdp') %>% # gdp NUTS3
  dplyr::filter(unit == 'PPS_EU27_2020_HAB') %>% # Purchasing power standard (PPS, EU27 from 2020), per inhabitant
  dplyr::filter(grepl('2021', TIME_PERIOD)) %>% # Much more data in 2021
  dplyr::mutate(unit_GDP = 'PPS_EU27_2020_HAB') %>% 
  dplyr::select(geo, unit_GDP, GDPpc = values)

pop <- eurostat::get_eurostat('nama_10r_3popgdp') %>% # pop NUTS3
  dplyr::filter(grepl('2021', TIME_PERIOD)) %>% # Much more data in 2021
  dplyr::mutate(unit_POP = 'THS') %>% 
  dplyr::select(geo, unit_POP, POP = values)

pop_ctzha <- eurostat::get_eurostat('cens_21ctzha_r3') %>% # pop_detailed NUTS3
  dplyr::filter(housing == 'TOTAL', citizen == 'TOTAL') %>% 
  dplyr::select(geo, age_group = age, age = values)

hdd_cdd <- eurostat::get_eurostat('nrg_chddr2_a') %>% # HDD & CDD NUTS3
  dplyr::filter(grepl('2023', TIME_PERIOD)) %>% 
  tidyr::pivot_wider(names_from = 'indic_nrg', values_from = 'values') %>% 
  dplyr::mutate(unit_CDD = 'NR',
         unit_HDD = 'NR') %>% 
  dplyr::select(geo, CDD, HDD, unit_CDD, unit_HDD)

grid_dt <- eurostat::get_eurostat_geospatial(year = 2021) %>% # 2024 available, but not for URBN_TYPE, which will not change much from 2021 to 2024
  as.data.frame() %>% 
  dplyr::select(LEVL_CODE, NUTS_ID, CNTR_CODE, URBN_TYPE, geo)


socioecon_dt <- purrr::reduce(
  list(grid_dt, gdp, pop, pop_ctzha, hdd_cdd),
  full_join,
  by = 'geo'
) %>% 
  dplyr::select(
    -starts_with("unit"),
    starts_with("unit")
  ) %>% 
  dplyr::filter(LEVL_CODE == 3)


pop_employed <- eurostat::get_eurostat('nama_10r_3empers') # pop_employed NUTS3
gva_act <- eurostat::get_eurostat('nama_10r_3gva') # gross value added by act NUTS3
# pop_fhcs <- eurostat::get_eurostat('cens_21fhcs_r3') # pop_detailed NUTS3
income <- eurostat::get_eurostat('nama_10r_2hhinc') # income NUTS2 na_item == 'B6N'
# poverty <- eurostat::get_eurostat('ilc_peps11n') # poverty NUTS2


hdd_cdd_sf <- data.table::as.data.table(hdd_cdd) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(geo, geometry),
                   by = 'geo')
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
  dplyr::mutate(Population = as.numeric(Population),
                Gini = as.numeric(Gini))

# check mismatched data (harmonized socioeconomic data NUTS3 code does not match the corresponding CTRY)
iso2_iso3 <- rfasst::ctry_nuts3_codes %>% 
  dplyr::select(ISO2, ISO3) %>% 
  dplyr::distinct()

harm_data_repair <- data.table::as.data.table(harm_data) %>%
  dplyr::select('Year','AU_name','AU_code','ISO','NUTS2','id' = 'id_1','NUTS_ID','CNTR_CODE','geo') %>% 
  dplyr::left_join(iso2_iso3,
                   by = c('ISO' = 'ISO3')) %>% 
  dplyr::left_join(iso_nuts2 = ISO2) %>% 
  dplyr::mutate(geo_nuts2 = stringr::str_sub(NUTS_ID, 1, 2)) %>% 
  dplyr::mutate(equal = iso_nuts2 == geo_nuts2) %>% 
  dplyr::filter(!equal) # OK!!

harm_data_sf <- data.table::as.data.table(harm_data) %>% 
  dplyr::select(-geometry) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
              dplyr::select(geo, geometry),
            by = 'geo')
harm_data_sf <- sf::st_sf(harm_data_sf, geometry = harm_data_sf$geometry)
sf::st_write(harm_data_sf, "data/tmp/harm_data_sf.shp", append = FALSE)

# ==============================================================================
#                              COMPUTE INDICATORS                              #
# ==============================================================================

#### DISPOSABLE INCOME & GINI ==================================================
harm_data_nuts3 <- data.table::as.data.table(harm_data) %>% 
  dplyr::mutate(Id = row_number()) %>% 
  dplyr::select(Id, Year, ISO, geo, Population, Disp_Inc_P, Gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  # pop-weighted disposable income: 
  # Disp_Inc_P_nuts3 = SUM_regInNUTS3(Pop * Disp_Inc_P) / SUM_regInNUTS3(Pop)
  dplyr::mutate(Disp_Inc_P_pw = Disp_Inc_P * Population) %>% 
  dplyr::group_by(Year, geo, ISO) %>% 
  dplyr::mutate(Population_nuts3 = sum(Population, na.rm = T)) %>% 
  dplyr::mutate(Disp_Inc_P_nuts3 = sum(Disp_Inc_P_pw, na.rm = T) / Population_nuts3) %>% 
  dplyr::ungroup() %>% 
  # weighted gini index: 
  gcamdata::left_join_error_no_match(compute_gini_by_nuts3(.),
                                     by = c('Year', 'geo')) %>% 
  dplyr::select(Year, ISO, geo, Population_nuts3, Disp_Inc_P_nuts3, Gini_nuts3) %>% 
  dplyr::distinct()

harm_data_nuts3_sf <- harm_data_nuts3 %>% 
  dplyr::left_join(nuts3_plot_data %>% 
              dplyr::select(geo, geometry),
            by = 'geo')
harm_data_nuts3_sf <- sf::st_sf(harm_data_nuts3_sf, geometry = harm_data_nuts3_sf$geometry)


plot_income <- tm_shape(nuts3_plot_data,
                        projection = "EPSG:3035",
                        xlim = c(2400000, 6500000),
                        ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(harm_data_nuts3_sf) +
  tm_polygons("Disp_Inc_P_nuts3",
              title = "Disposable Income\n[2015 PPP EU27 €]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_income, filename = "figures/plot_income.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)

plot_gini <- tm_shape(nuts3_plot_data,
                      projection = "EPSG:3035",
                      xlim = c(2400000, 6500000),
                      ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(harm_data_nuts3_sf) +
  tm_polygons("Gini_nuts3",
              title = "Gini\nIndex",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_gini, filename = "figures/plot_gini.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)




#### ELDERLY POPULATION (%) ====================================================
# compute Y_GE65 (elderly people) percentage by NUTS3 region
grid_sf_elderly_v2 <- as.data.frame(grid_eur_sf_nuts3) %>%
  dplyr::filter(T > 0, Y_GE65 >= 0) %>% 
  dplyr::select(GRD_ID, Y_GE65, Y_TOT = T, geo)

grid_sf_elderly_v3 <- unique(grid_sf_elderly_v2) %>% 
  dplyr::group_by(geo) %>%
  dplyr::summarise(Y_GE65 = sum(Y_GE65),
                   Y_TOT = sum(Y_TOT)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(per_elderly = ifelse(!is.na(Y_GE65), Y_GE65 / Y_TOT, NA_real_)) %>% 
  dplyr::group_by(geo) %>%
  dplyr::summarise(mean_per_elderly = mean(per_elderly, na.rm = TRUE)) %>%
  dplyr::ungroup() %>% 
  as.data.frame() %>% 
  # merge with NUTS3 geometries
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(geo, geometry),
                   by = 'geo')

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


tmap::tmap_save(plot_elderly, filename = "figures/plot_elderly.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)



#### URBN_TYPE (1 = cities, 2 = towns and suburbs, 3 = rural) ==================

grid_sf_urbntype_v2 <- as.data.frame(grid_eur_sf_nuts3) %>%
  dplyr::select(URBN_TYPE, geo) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(URBN_TYPE = factor(URBN_TYPE, 
                            levels = c(1, 2, 3), 
                            labels = c("City", "Town/Suburb", "Rural"))) %>%
  as.data.frame() %>% 
  # merge with NUTS3 geometries
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(geo, geometry),
                   by = 'geo')

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

tmap::tmap_save(plot_urbtype, filename = "figures/plot_urbtype.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)


#### CDD (very hot days when cooling is needed) ================================

grid_sf_cdd_v2 <- hdd_cdd %>%   
  dplyr::filter(nchar(geo) == 5) %>% # only NUTS3
  dplyr::select(-starts_with('unit')) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                   dplyr::select(geo, geometry),
                 by = 'geo')
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

tmap::tmap_save(plot_cdd, filename = "figures/plot_cdd.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)



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
  dplyr::left_join(nuts3_plot_data %>% 
              dplyr::select(geo, geometry),
            by = 'geo')
harm_socioeconomic_nuts_sf <- sf::st_sf(harm_socioeconomic_nuts_sf, geometry = harm_socioeconomic_nuts_sf$geometry)

sf::st_write(harm_socioeconomic_nuts_sf, "data/tmp/harm_socioeconomic_nuts_sf.shp", append = FALSE)
