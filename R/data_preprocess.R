require(eurostat)
require(eurostat)
require(sf)
library(tmap)
library(magrittr)

source("R/utils.R")
source("R/zzz.R")
yy = 2030
normalized = T
# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================

#### spacial data ==============================================================
## spacial data for plots
nuts3_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID)
sf::st_write(nuts3_plot_data, "data/Replication data/Global_Inputs/nuts3_plot_data.shp",
             delete_layer = TRUE,
             layer_options = "ENCODING=UTF-8"
)

#### rfasst population data ====================================================
# Source: rfasst pkg, dev_k branch population in million
rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 
save(rfasst_pop, file = "data/tmp/rfasst_pop.RData")

#### rfasst output data ========================================================
# ap <- get(load("data/rfasst_output/necp_m2_get_conc_pm25.ctry_agg.output.RData")) %>%
ap <- read.csv("data/rfasst_output/EU_NECP_LTT-Rev1.1_2030_WORLD-NUTS3_pm25_avg.csv") %>%
  dplyr::filter(year == yy) %>% 
  dplyr::rename(region = id_code,
                value = pm25_avg)
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

deaths <- get(load(paste0("data/rfasst_output/necp_m3_get_mort_pm25-Rev1.1.output.RData"))) %>% 
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
deaths_nuts3 <- deaths %>%
  dplyr::filter(
    nchar(region) > 3
  ) %>%
  dplyr::rename(
    geo = region,
    deaths = value
  ) %>%
  dplyr::left_join(nuts3_plot_data,
                   by = "geo"
  )
deaths_nuts3_sf <- sf::st_sf(deaths_nuts3, geometry = deaths_nuts3$geometry)


#### eurostat urbn-type data ===================================================
# Source: https://ec.europa.eu/eurostat/web/rural-development/methodology, NUTS-2021
urbn_type <- xlsx::read.xlsx('data/NUTS2021.xlsx', sheetName = 'Urban-rural') %>% 
  dplyr::select(geo = NUTS_ID, urbn_type = URBAN.RURAL.CATEGORY.) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(urbn_type = factor(urbn_type,
                                   levels = c(1, 2, 3),
                                   labels = c("City", "Town/Suburb", "Rural")
  ))
save(urbn_type, file = "data/tmp/urbn_type.RData")


#### income pc by nuts3 data ===================================================
pm.ap_raster <- terra::rast("data/rfasst_output/EU_NECP_LTT-Rev1.1_2030_pm25_fin_weighted.tif")
extent_raster <- terra::ext(-26.276, 40.215, 32.633, 71.141)
pm.ap_raster_crop <- terra::crop(pm.ap_raster, extent_raster)

inc_data <- terra::rast("data/High-resolution_Downscaling/Europe_disp_inc_2015.tif")
inc_data2 <- terra::resample(inc_data, pm.ap_raster_crop)
inc_data2 <- terra::crop(inc_data2, extent_raster)
inc_data2 <- terra::mask(inc_data2, inc_data2, maskvalue = NA)

pop_t <- terra::rast("data/Eurostat_Census-GRID_2021_V2-0/ESTAT_OBS-VALUE-T_2021_V2.tiff")
pop_t2 <- terra::project(pop_t, pm.ap_raster_crop)
pop_t2 <- terra::resample(pop_t2, pm.ap_raster_crop)
pop_t2 <- terra::crop(pop_t2, extent_raster)
pop_t2 <- terra::mask(pop_t2, pop_t2, maskvalue = NA)


# convert the filtered rasters to data frames
inc_values <- terra::values(inc_data2)
pop_values <- terra::values(pop_t2)

df_inc <- data.frame('pop' = pop_values, 
                     'inc_pc' = inc_values)
colnames(df_inc) <- c('pop','inc_pc')
df_inc <- df_inc %>% 
  # total income by grid: inc_grid = inc_pc (by grid) * pop (by grid)
  dplyr::mutate(inc_grid = inc_pc * pop)

# convert to raster again
inc_data3 <- inc_data2
terra::values(inc_data3) = df_inc$inc_grid

pop_data3 <- pop_t2
terra::values(pop_data3) = df_inc$pop

# make sure the CRS matches
nuts3_plot_data <- sf::st_transform(nuts3_plot_data, terra::crs(inc_data3))
nuts3_vect <- terra::vect(nuts3_plot_data)

# extract the sum by nuts3 raster polygon of pop and income
inc_sum <- terra::extract(inc_data3, nuts3_vect, fun = sum, na.rm = TRUE); colnames(inc_sum)[2] <- 'inc_nuts3'
pop_sum <- terra::extract(pop_data3, nuts3_vect, fun = sum, na.rm = TRUE); colnames(pop_sum)[2] <- 'pop_nuts3'

# combine with polygon data
nuts3_with_inc <- dplyr::bind_cols(nuts3_plot_data, inc_sum[,2,drop = F], pop_sum[,2,drop = F])

# compute inc_pc by nuts3
nuts3_with_inc <- nuts3_with_inc %>% 
  dplyr::mutate(inc_pc_nuts3 = inc_nuts3 / pop_nuts3) %>% 
  dplyr::filter(!CNTR_CODE %in% c('AL','RS','MK','ME')) # remove countries with population missing data

sf::st_write(nuts3_with_inc, "data/tmp/nuts3_with_inc-Rev1.1.shp", append = FALSE)

#### gini by nuts3 data ========================================================
# Source: https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/TTIOKI
harm_data <- sf::st_read("data/Replication data/merged_output_2019_wID_clean.shp") %>%
  dplyr::mutate(
    Population = as.numeric(Population),
    Gini = as.numeric(Gini)
  ) %>% 
  # fix minor NUTS - ISO pairs
  dplyr::filter(!(NUTS_ID == 'FRF12' & AU_code == 2774 & CNTR_CODE == 'FR')) %>% 
  dplyr::mutate(NUTS_ID = dplyr::if_else(NUTS_ID == 'FRF12' & AU_code == 2774, 'CH032', NUTS_ID)) %>% 
  dplyr::filter(!(NUTS_ID == 'FRK28' & AU_code == 6625 & CNTR_CODE == 'FR')) %>% 
  dplyr::mutate(NUTS_ID = dplyr::if_else(NUTS_ID == 'FRK28' & AU_code == 6625, 'CH013', NUTS_ID)) %>% 
  dplyr::filter(!(NUTS_ID == 'FRI15' & AU_code == 2004502004 & CNTR_CODE == 'FR')) %>% 
  dplyr::mutate(NUTS_ID = dplyr::if_else(NUTS_ID == 'FRI15' & AU_code == 2004502004, 'ES212', NUTS_ID)) %>% 
  dplyr::filter(!(NUTS_ID == 'BE328' & AU_code == 590170201 & CNTR_CODE %in% c('BE',NA))) %>% 
  dplyr::mutate(NUTS_ID = dplyr::if_else(NUTS_ID == 'BE328' & AU_code == 590170201, 'FRE11', NUTS_ID)) %>% 
  dplyr::filter(!(NUTS_ID == 'ITC41' & AU_code == 5213 & CNTR_CODE == 'CH')) %>% 
  dplyr::mutate(NUTS_ID = dplyr::if_else(NUTS_ID == 'ITC41' & AU_code == 5213, 'ITC41', NUTS_ID)) %>% 
  dplyr::mutate(ISO = dplyr::if_else(NUTS_ID == 'ITC41' & AU_code == 5213, 'ITA', ISO))

# # check the "base" gini, before the NUTS3 aggregation
# pdf("plot_check_gini_AU_codes.pdf")
# terra::plot(harm_data %>% dplyr::select(Gini, geometry), border = NA)
# dev.off()

# check mismatched data (harmonized socioeconomic data NUTS3 code does not match the corresponding CTRY)
iso2_iso3 <- rfasst::ctry_nuts3_codes %>%
  dplyr::select(ISO2, ISO3) %>%
  dplyr::distinct()

harm_data_repair <- data.table::as.data.table(harm_data) %>%
  dplyr::select("Year", "AU_name", "AU_code", "ISO", "NUTS2", "id" = "id_1", "NUTS_ID", "CNTR_CODE", "geo") %>%
  dplyr::left_join(iso2_iso3,
    by = c("ISO" = "ISO3")
  ) %>%
  dplyr::rename(iso_nuts2 = ISO2) %>%
  dplyr::mutate(geo_nuts2 = stringr::str_sub(NUTS_ID, 1, 2)) %>%
  dplyr::mutate(equal = iso_nuts2 == geo_nuts2) %>%
  dplyr::filter(!equal)

if(nrow(harm_data_repair) != 0) {
  stop('There is a mismatch between NUTS3 and ISO3 codes. Check the `merged_output_2019_wID_clean.shp` dataset.')
}

harm_data_sf <- data.table::as.data.table(harm_data) %>%
  dplyr::select(-geometry) %>% 
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
harm_data_sf <- sf::st_sf(harm_data_sf, geometry = harm_data_sf$geometry)
sf::st_write(harm_data_sf, "data/tmp/harm_data_sf-Rev1.1.shp", append = FALSE)

# ==============================================================================
#                                 PLOT INDICATORS                              #
# ==============================================================================

#### DISPOSABLE INCOME ==================================================
nuts3_with_inc_sf <- sf::st_sf(nuts3_with_inc, geometry = nuts3_with_inc$geometry)

plot_income <- tm_shape(nuts3_plot_data,
  projection = "EPSG:3035",
  xlim = c(2400000, 6500000),
  ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(nuts3_with_inc_sf %>%
    dplyr::select(inc_pc_nuts3, geometry) %>%
    dplyr::filter(rowSums(is.na(.)) == 0)) +
  tm_polygons("inc_pc_nuts3",
    title = "Disposable Income\n[2015 PPP EU27 €]",
    palette = "Oranges",
    style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_income,
  filename = "figures/plot_income2.pdf",
  width = 100, height = 100, units = "mm", dpi = 300
)

#### GINI ==================================================
harm_data_gini_nuts3 <- data.table::as.data.table(harm_data) %>%
  dplyr::select(Year, ISO, geo = NUTS_ID, Population, Disp_Inc_P, Gini) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  tibble::as_tibble() %>%
  # weighted gini index:
  gcamdata::left_join_error_no_match(compute_gini_by_nuts3(.),
    by = c("Year", "geo")
  ) %>%
  dplyr::select(Year, ISO, geo, Gini_nuts3) %>%
  dplyr::distinct()

harm_data_gini_nuts3_sf <- harm_data_gini_nuts3 %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
harm_data_gini_nuts3_sf <- sf::st_sf(harm_data_gini_nuts3_sf, geometry = harm_data_gini_nuts3_sf$geometry)

plot_gini <- tm_shape(nuts3_plot_data,
  projection = "EPSG:3035",
  xlim = c(2400000, 6500000),
  ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(harm_data_gini_nuts3_sf %>%
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
old_age <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
grid_sf_elderly_v2 <- as.data.frame(rfasst_pop) %>%
  dplyr::mutate(Y_GE65_bool = dplyr::if_else(age %in% old_age, T, F)) %>% 
  dplyr::group_by(geo, year, sex) %>% 
  dplyr::summarise(Y_GE65 = sum(dplyr::if_else(Y_GE65_bool, pop, 0)),
                   Y_TOT = sum(pop)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(per_elderly = Y_GE65 / Y_TOT) %>%
  as.data.frame()
  # merge with NUTS3 geometries
grid_sf_elderly_v3 <- nuts3_plot_data %>%
    dplyr::select(geo, geometry) %>% 
    dplyr::left_join(
      grid_sf_elderly_v2,
      by = "geo"
      ) %>% 
    dplyr::filter(rowSums(is.na(.)) == 0)
      
grid_sf_elderly_v3 <- sf::st_sf(grid_sf_elderly_v3, geometry = grid_sf_elderly_v3$geometry)

plot_elderly <- tm_shape(nuts3_plot_data,
  projection = "EPSG:3035",
  xlim = c(2400000, 6500000),
  ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(grid_sf_elderly_v3 %>% 
             dplyr::filter(year == 2020, sex == 'Both')) +
  tm_polygons("per_elderly",
    title = "Elderly people [%]",
    palette = "Oranges",
    style = "cont",
    size = 0.05
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)


tmap::tmap_save(plot_elderly,
  filename = "figures/plot_elderly.pdf",
  width = 100, height = 100, units = "mm", dpi = 300
)



#### URBN_TYPE (1 = cities, 2 = towns and suburbs, 3 = rural) ==================

grid_sf_urbntype_v2 <- as.data.frame(urbn_type) %>%
  dplyr::select(urbn_type, geo) %>%
  dplyr::distinct() %>%
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
  tm_polygons("urbn_type",
    title = "Urban type",
    palette = c("#C288B0", "#C3D2D5", "#92DEC3")
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_urbtype,
  filename = "figures/plot_urbtype.pdf",
  width = 100, height = 100, units = "mm", dpi = 300
)


# ==============================================================================
#                                   JOIN DATA                                  #
# ==============================================================================



harm_socioeconomic_nuts_sf <- data.table::as.data.table(urbn_type) %>%
  dplyr::full_join(data.table::as.data.table(grid_sf_elderly_v3) %>%
                     dplyr::filter(year == 2020) %>% 
                     dplyr::select(geo, sex, per_elderly), 
                   by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(rfasst_pop %>% 
                                               dplyr::filter(year == 2020,
                                                             geo %in% unique(urbn_type$geo)) %>% 
                                               dplyr::select(geo, age, sex, popM = pop)), 
                   by = c("geo","sex")) %>% 
  dplyr::full_join(data.table::as.data.table(harm_data_gini_nuts3_sf) %>%
    dplyr::select(-geometry), by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(nuts3_with_inc_sf) %>%
    dplyr::select(Inc_nuts3 = inc_pc_nuts3, geo), by = "geo")
harm_socioeconomic_nuts_sf2 <- harm_socioeconomic_nuts_sf %>% 
  # merge with NUTS3 geometries
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
harm_socioeconomic_nuts_sf2 <- sf::st_sf(harm_socioeconomic_nuts_sf2, geometry = harm_socioeconomic_nuts_sf2$geometry)

sf::st_write(harm_socioeconomic_nuts_sf2, "data/tmp/harm_socioeconomic_nuts_sf_POLICY55-Rev1.1.gpkg", append = FALSE)



## AP vs socioeconomic DATA ====================================================
# merge AP & socioeconomic data
ap_socioecon_sf <- sf::st_intersection(
  harm_socioeconomic_nuts_sf2 %>%
    dplyr::filter(sex == 'Both') %>% 
    dplyr::select(geo, urbn_type, per_elderly, income = Inc_nuts3,
                  gini = Gini_nuts3, geometry) %>%
    unique(),
  ap_nuts3_sf %>%
    dplyr::select(ap, ctry = CNTR_CODE, geometry) %>% 
    unique()
)
save(ap_socioecon_sf, file = 'ap_socioecon_sf-Rev1.1.RData')

## DEATHS vs socioeconomic DATA ====================================================
# merge DEATHS & socioeconomic data
a = harm_socioeconomic_nuts_sf2 %>%
  dplyr::filter(sex == 'Both') %>% 
  dplyr::select(geo, urbn_type, per_elderly, income = Inc_nuts3,
                gini = Gini_nuts3, geometry) %>%
  unique()

b = deaths_nuts3_sf %>%
  dplyr::filter(sex == 'Both') %>%
  dplyr::select(deaths, ctry = CNTR_CODE, geometry) %>% 
  unique()

deaths_socioecon_sf <- sf::st_intersection(
  a,b
)
save(deaths_socioecon_sf, file = 'deaths_socioecon_sf-Rev1.1.RData')
