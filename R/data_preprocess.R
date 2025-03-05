require(eurostat)
require(eurostat)
require(sf)
library(tmap)
library(magrittr)

source("R/utils.R")
source("R/zzz.R")

# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================

#### spacial data ==============================================================
## spacial data for plots
nuts3_plot_data <- get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID)
sf::st_write(nuts3_plot_data, "data/Replication data/Global_Inputs/nuts3_plot_data.shp",
             delete_layer = TRUE,
             layer_options = "ENCODING=UTF-8"
)


#### rfasst data ===============================================================
ap <- get(load("data/rfasst_output/tmp_m2_get_conc_pm25.ctry_nuts.output.RData")) %>%
  dplyr::filter(year == yy)
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

deaths <- get(load(paste0("data/rfasst_output/m3_get_mort_pm25.output.RData"))) %>%
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


#### eurostat HDD-CDD nuts3 data ===============================================
hdd_cdd <- eurostat::get_eurostat("nrg_chddr2_a") %>% # HDD & CDD NUTS3
  dplyr::filter(grepl("2023", TIME_PERIOD)) %>%
  tidyr::pivot_wider(names_from = "indic_nrg", values_from = "values") %>%
  dplyr::mutate(
    unit_CDD = "NR",
    unit_HDD = "NR"
  ) %>%
  dplyr::select(geo, CDD, HDD, unit_CDD, unit_HDD)

hdd_cdd_sf <- data.table::as.data.table(hdd_cdd) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
hdd_cdd_sf <- sf::st_sf(hdd_cdd_sf, geometry = hdd_cdd_sf$geometry)
sf::st_write(hdd_cdd_sf, "data/tmp/hdd_cdd_sf.shp", append = FALSE)


#### eurostat GDP nuts3 data ===============================================
gdp <- eurostat::get_eurostat("nama_10r_3gdp") %>% # GDP NUTS3
  dplyr::filter(unit == 'PPS_EU27_2020_HAB',
                grepl("2021", TIME_PERIOD),
                nchar(geo) == 5) %>%
  dplyr::select(geo, gdp = values, unit_gdp = unit)

gdp_sf <- data.table::as.data.table(gdp) %>%
  dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
gdp_sf <- sf::st_sf(gdp_sf, geometry = gdp_sf$geometry)
sf::st_write(gdp_sf, "data/tmp/gdp_sf.shp", append = FALSE)



#### rfasst population data ====================================================
# Source: rfasst pkg, dev_k branch population in million
rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 
save(rfasst_pop, file = "data/tmp/rfasst_pop.RData")


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


#### harmonized income data ====================================================
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

harm_data_sf <- data.table::as.data.table(harm_data_repair) %>%
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

#### GDP =======================================================================
plot_gdp <- tm_shape(nuts3_plot_data,
                        projection = "EPSG:3035",
                        xlim = c(2400000, 6500000),
                        ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(gdp_sf %>%
             dplyr::select(gdp, geometry) %>%
             dplyr::filter(rowSums(is.na(.)) == 0,
                           gdp <= 80000) %>% 
             dplyr::filter(!st_is_empty(geometry))) +
  tm_polygons("gdp",
              title = "GDPpc\n[2020 PPP EU27 €]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_gdp,
                filename = "figures/plot_gdp.pdf",
                width = 100, height = 100, units = "mm", dpi = 300
)

#### DISPOSABLE INCOME & GINI ==================================================
harm_data_income_nuts3 <- data.table::as.data.table(harm_data) %>%
  dplyr::select(Year, ISO, geo = NUTS_ID, Population, Disp_Inc_P) %>%
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
  dplyr::select(Year, ISO, geo = NUTS_ID, Population, Disp_Inc_P, Gini) %>%
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



harm_socioeconomic_nuts_sf <- data.table::as.data.table(urbn_type) %>%
  dplyr::full_join(data.table::as.data.table(grid_sf_cdd_v2) %>%
    dplyr::select(-geometry), by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(gdp_sf) %>%
    dplyr::select(geo, gdp), by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(grid_sf_elderly_v3) %>%
                     dplyr::filter(year == 2020) %>% 
                     dplyr::select(geo, sex, per_elderly), 
                   by = "geo") %>%
  dplyr::full_join(data.table::as.data.table(rfasst_pop %>% 
                                               dplyr::filter(year == 2020,
                                                             geo %in% unique(urbn_type$geo)) %>% 
                                               dplyr::select(geo, age, sex, popM = pop)), 
                   by = c("geo","sex")) %>% 
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



## AP vs socioeconomic DATA ====================================================
# merge AP & socioeconomic data
ap_socioecon_sf <- sf::st_intersection(
  harm_socioeconomic_nuts_sf %>%
    dplyr::filter(sex == 'Both') %>% 
    dplyr::select(geo, urbn_type, CDD, per_elderly, income = Disp_Inc_P_nuts3,
                  gini = Gini_nuts3, gdp, geometry) %>%
    unique(),
  ap_nuts3_sf %>%
    dplyr::select(ap, ctry = CNTR_CODE, geometry) %>% 
    unique()
)
save(ap_socioecon_sf, file = 'ap_socioecon_sf.RData')

## DEATHS vs socioeconomic DATA ====================================================
# merge DEATHS & socioeconomic data
a = harm_socioeconomic_nuts_sf %>%
  dplyr::filter(sex == 'Both') %>% 
  dplyr::select(geo, urbn_type, CDD, per_elderly, income = Disp_Inc_P_nuts3,
                gini = Gini_nuts3, gdp, geometry) %>%
  unique()

b = deaths_nuts3_sf %>%
  dplyr::filter(sex == 'Both') %>%
  dplyr::select(deaths, ctry = CNTR_CODE, geometry) %>% 
  unique()

deaths_socioecon_sf <- sf::st_intersection(
  a,b
)
save(deaths_socioecon_sf, file = 'deaths_socioecon_sf.RData')
