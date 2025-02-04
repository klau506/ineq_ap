require(eurostat)
require(sf)
library(dplyr)
library(ggplot2)
library(tmap)


# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================
## load dummy PM2.5 concentration & premature deaths by NUTS3
ap <- get(load('data/rfasst_output/tmp_m2_get_conc_pm25.ctry_nuts.output.RData'))
deaths <- get(load('data/rfasst_output/tmp_m3_get_mort_pm25.output.RData')) %>% 
  select(region, year, age, sex, disease, value = GBD, scenario)


## spacial data for plots
nuts3_plot_data <- get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>% 
  dplyr::select(-FID)
st_write(nuts3_plot_data, "data/Replication data/Global_Inputs/nuts3_plot_data.shp", 
         delete_layer = TRUE,
         layer_options = "ENCODING=UTF-8")


## read socioeconomic data
gdp <- eurostat::get_eurostat('nama_10r_3gdp') %>% # gdp NUTS3
  filter(unit == 'PPS_EU27_2020_HAB') %>% # Purchasing power standard (PPS, EU27 from 2020), per inhabitant
  filter(grepl('2021', TIME_PERIOD)) %>% # Much more data in 2021
  mutate(unit_GDP = 'PPS_EU27_2020_HAB') %>% 
  select(geo, unit_GDP, GDPpc = values)

pop <- eurostat::get_eurostat('nama_10r_3popgdp') %>% # pop NUTS3
  filter(grepl('2021', TIME_PERIOD)) %>% # Much more data in 2021
  mutate(unit_POP = 'THS') %>% 
  select(geo, unit_POP, POP = values)

pop_ctzha <- eurostat::get_eurostat('cens_21ctzha_r3') %>% # pop_detailed NUTS3
  filter(housing == 'TOTAL', citizen == 'TOTAL') %>% 
  select(geo, age_group = age, age = values)

hdd_cdd <- eurostat::get_eurostat('nrg_chddr2_a') %>% # HDD & CDD NUTS3
  filter(grepl('2023', TIME_PERIOD)) %>% 
  tidyr::pivot_wider(names_from = 'indic_nrg', values_from = 'values') %>% 
  mutate(unit_CDD = 'NR',
         unit_HDD = 'NR') %>% 
  select(geo, CDD, HDD, unit_CDD, unit_HDD)

grid_dt <- eurostat::get_eurostat_geospatial(year = 2021) %>% # 2024 available, but not for URBN_TYPE, which will not change much from 2021 to 2024
  as.data.frame() %>% 
  select(LEVL_CODE, NUTS_ID, CNTR_CODE, URBN_TYPE, geo)


socioecon_dt <- purrr::reduce(
  list(grid_dt, gdp, pop, pop_ctzha, hdd_cdd),
  full_join,
  by = 'geo'
) %>% 
  select(
    -starts_with("unit"),
    starts_with("unit")
  ) %>% 
  filter(LEVL_CODE == 3)


pop_employed <- eurostat::get_eurostat('nama_10r_3empers') # pop_employed NUTS3
gva_act <- eurostat::get_eurostat('nama_10r_3gva') # gross value added by act NUTS3
# pop_fhcs <- eurostat::get_eurostat('cens_21fhcs_r3') # pop_detailed NUTS3
income <- eurostat::get_eurostat('nama_10r_2hhinc') # income NUTS2 na_item == 'B6N'
# poverty <- eurostat::get_eurostat('ilc_peps11n') # poverty NUTS2


# grided socioeconomic data from EUROSTAT - people by age & sex
temp_dir <- tempdir()
unzip("data/Eurostat_Census-GRID_2021_V2.1.zip", exdir = temp_dir)
sf_data <- sf::st_read(file.path(temp_dir, "ESTAT_Census_2021_V2.gpkg"))
sf_data <- sf_data %>%
  dplyr::select(-ends_with("_CI")) %>%
  dplyr::filter(POPULATED == 1, T >= 0)

# merge socioeconomic gridded data with NUTS3 regions
grid_sf <- st_transform(sf_data, crs = st_crs(nuts3_plot_data))
grid_sf_with_nuts3 <- st_join(grid_sf, nuts3_plot_data, join = st_intersects)
# st_write(grid_sf_with_nuts3, "data/tmp_output_grid_with_nuts3.shp", append = FALSE)

# compute Y_GE65 (elderly people) percentage by NUTS3 region
grid_sf_elderly_v2 <- as.data.frame(grid_sf_with_nuts3) %>%
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
                     select(geo, geometry),
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



#### proves - TODO delete ==================================================================

# pl <- ggplot() +
#   geom_sf(data = sf_data, 
#           aes(color = Y_GE65,
#               fill = Y_GE65)) +
#   labs(fill = "Y_GE65") + 
#   ggpubr::theme_pubr() +
#   theme(panel.background = element_rect(fill = 'white'),
#         panel.grid.major = element_line(colour = "grey90"),
#         panel.ontop = FALSE,
#         strip.text = element_text(size = 12),
#         strip.background = element_blank(),
#         axis.title.x = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         legend.key.size = unit(1.5, 'cm'),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.position = "bottom")
# 
# ggsave(file='figures/pl_prova.pdf', height = 15, width = 15, units = 'cm',
#        plot = pl)
#### fi proves =====================

#### Harmonized income - TODO fix =====================
harm_data <- sf::st_read("data/Replication data/merged_output_2019.shp") %>% # TODO check countries' borders. Some NUTS3 are misplaced
  dplyr::mutate(Population = as.numeric(Population),
                Gini = as.numeric(Gini))
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
  dplyr::select(Id, Year, ISO, geo, Population, Disp_Inc_P, Gini, Population_nuts3, Disp_Inc_P_nuts3, Gini_nuts3)



iso2_iso3 <- rfasst::ctry_nuts3_codes %>% 
  dplyr::select(ISO2, ISO3) %>% 
  dplyr::distinct()

harm_data_repair <- data.table::as.data.table(harm_data) %>%
  dplyr::select('Year','AU_name','AU_code','ISO','NUTS2','id','NUTS_ID','CNTR_CODE','geo') %>% 
  dplyr::left_join(iso2_iso3,
                   by = c('ISO' = 'ISO3')) %>% 
  dplyr::rename(iso_nuts2 = ISO2) %>% 
  dplyr::mutate(geo_nuts2 = stringr::str_sub(geo, 1, 2)) %>% 
  dplyr::mutate(equal = iso_nuts2 == geo_nuts2) %>% 
  dplyr::filter(!equal)
  # # remove GRB, SWE, SVN case
  # dplyr::filter(!(ISO == 'GRB' & CNTR_CODE == 'UK'),
  #               !(ISO == 'SWE' & CNTR_CODE == 'SE'),
  #               !(ISO == 'SVN' & CNTR_CODE == 'SI'),
  #               !(ISO == 'PRT' & CNTR_CODE == 'PT'),
  #               !(ISO == 'SRB' & CNTR_CODE == 'RS'))

  # # Gini_nuts3 = SUM_regInNUTS3(i) SUM_regInNUTS3(j) [(Pop_i * Pop_j * |Disp_Inc_P_i - Disp_Inc_P_j|) / 2 * Pop_T^2 * Disp_Inc_P_T], 
  # # Disp_Inc_P_T = SUM_regInNUTS3(Pop * Disp_Inc_P) / SUM_regInNUTS3(Pop)
  # dplyr::group_by(Year, geo, ISO) %>% 
  # dplyr::mutate(Disp_Inc_P_pw_nuts3 = sum(Disp_Inc_P_pw)) %>% # Disp_Inc_P_T: total disposalbe income pop-weighted
  # dplyr::mutate(Disp_Inc_P_T = Disp_Inc_P_pw_nuts3 / Population_nuts3) %>% 
  # 
  


shapefile <- sf::st_read("data/harmonized_income_data/yearly_files/disp_inc_2019.gpkg")
# shapefile <- sf::st_read("data/Replication data/merged_output.shp")
# shapefile <- sf::st_read("data/Replication data/ESP/Disposable_Inc_ESP.shp")
pl <- ggplot(data = aa) +
  geom_sf(aes(fill = NUTS_ID), color = "black", size = 0)
  theme(legend.position = 'none') 
#   scale_fill_viridis_c(option = "magma", na.value = "white") + 
#   theme_minimal() +
#   labs(title = "Gini index",
#        fill = "Gini index")
# ggsave(pl, filename = "figures/pl_prova_harmonized_data_gini.pdf",
#        width = 100, height = 100, units = 'mm')

# merge with NUTS3 geometries
shapefile_sf_with_nuts3_v2 <- as.data.frame(shapefile_sf_with_nuts3) %>%
  dplyr::filter(T > 0, Y_GE65 >= 0) %>% 
  dplyr::select(GRD_ID, Y_GE65, Y_TOT = T, geo)

grid_sf_with_nuts3_v3 <- unique(grid_sf_with_nuts3_v2) %>% 
  dplyr::group_by(geo) %>%
  dplyr::summarise(Y_GE65 = sum(Y_GE65),
                   Y_TOT = sum(Y_TOT)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(per_elderly = ifelse(!is.na(Y_GE65), Y_GE65 / Y_TOT, NA_real_)) %>% 
  dplyr::group_by(geo) %>%
  dplyr::summarise(mean_per_elderly = mean(per_elderly, na.rm = TRUE)) %>%
  dplyr::ungroup() %>% 
  as.data.frame() %>% 
  
  shapefile_sf_with_nuts3 %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     select(geo, geometry),
                   by = 'geo')

shapefile_sf_with_nuts3_v2 <- sf::st_sf(shapefile_sf_with_nuts3_v2, geometry = shapefile_sf_with_nuts3_v2$geometry)

plot_prova_ESP_tmap <- tm_shape(nuts3_plot_data,
                         projection = "EPSG:3035",
                         xlim = c(2400000, 6500000),
                         ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(shapefile_sf_with_nuts3_v2) +
  tm_polygons("mean_per_elderly",
              title = "Elderly people [%]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)


tmap::tmap_save(plot_prova_ESP_tmap, filename = "figures/plot_prova_ESP_tmap.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)


#### fi harmonized income ==============


# ==============================================================================
#                              COMPUTE INDICATORS                              #
# ==============================================================================
## GINI (index) ================================================================


## INCOME PER CAPITA (PPPpc) ===================================================
socioecon_dt2 <- socioecon_dt %>% 
  mutate(total_gdp = GDPpc * POP * 1e3) %>% 
  group_by(CNTR_CODE) %>% 
  summarise(total_gdp = sum(total_gdp))

## ELDERLY PEOPLE (% people >= Y75) ============================================
elderly_age_list <- c("Y75-79","Y80-84","Y85-89","Y90-94","Y95-99","Y_GE100")

socioecon_dt_ind <- socioecon_dt %>% 
  mutate(total = ifelse(age_group == "TOTAL", age, NA)) %>% # use this "total" pop since it's more precise
  tidyr::fill(total, .direction = "down") %>% 
  group_by(geo) %>%
  mutate(NR_elderly = ifelse(age_group %in% elderly_age_list, 
                             sum(age[age_group %in% elderly_age_list]), NA)) %>% 
  tidyr::fill(NR_elderly, .direction = "downup") %>%  
  mutate(per_elderly = NR_elderly / total * 100) %>% 
  ungroup() %>% 
  select(-total, -NR_elderly) %>% 
  left_join(nuts3_plot_data %>% 
              select(geo, geometry),
            by = 'geo')
socioecon_dt_ind_sf <- sf::st_sf(socioecon_dt_ind, geometry = socioecon_dt_ind$geometry)
if(nrow(socioecon_dt_ind[rowSums(is.na(socioecon_dt_ind))>0,]) != 0) {
  warning("There are not-matching NUTS3 codes between the socioecon_dt_ind and EUROSTAT NUTS3 data")
}

plot_elderly <- tm_shape(nuts3_plot_data,
                    projection = "EPSG:3035",
                    xlim = c(2400000, 6500000),
                    ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(socioecon_dt_ind_sf) +
  tm_polygons("per_elderly",
              title = "Elderly people [%]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)


tmap::tmap_save(plot_elderly, filename = "figures/plot_elderly.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)


## URBN_TYPE (1 = cities, 2 = towns and suburbs, 3 = rural) ====================
socioecon_dt_ind_urbntype_sf <- socioecon_dt_ind_sf %>% 
  select(geometry, URBN_TYPE) %>% 
  mutate(URBN_TYPE = factor(URBN_TYPE, 
                            levels = c(1, 2, 3), 
                            labels = c("City", "Town/Suburb", "Rural"))) %>%
  distinct()

plot_urbtype <- tm_shape(nuts3_plot_data,
                         projection = "EPSG:3035",
                         xlim = c(2400000, 6500000),
                         ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(socioecon_dt_ind_urbntype_sf) +
  tm_polygons("URBN_TYPE",
              title = "Urban type",
              palette = c("#C288B0", "#C3D2D5", "#92DEC3")
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_urbtype, filename = "figures/plot_urbtype.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)





# ==============================================================================
#                                    ANALYSE                                   #
# ==============================================================================
## AP  =========================================================================
ap_nuts3 <- ap %>% 
  filter(nchar(region) > 3,
         year == 2020) %>% 
  rename(geo = region,
         ap = value) %>% 
  left_join(nuts3_plot_data,
            by = 'geo')
ap_nuts3_sf <- sf::st_sf(ap_nuts3, geometry = ap_nuts3$geometry)
if(nrow(ap_nuts3[rowSums(is.na(ap_nuts3))>0,]) != 0) {
  warning("There are not-matching NUTS3 codes between the rfasst AP results and EUROSTAT NUTS3 data")
}

plot_ap <- tm_shape(nuts3_plot_data,
                    projection = "EPSG:3035",
                    xlim = c(2400000, 6500000),
                    ylim = c(1320000, 5650000)
  ) +
  tm_fill("lightgrey") +
  tm_shape(ap_nuts3_sf %>% 
             dplyr::filter(year == 2020)) +
  tm_polygons("ap",
              title = "PM2.5\n[ug/m3]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_ap, filename = "figures/plot_ap.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)

## DEATHS  =====================================================================
deaths_nuts3 <- deaths %>% 
  filter(nchar(region) > 3,
         age == '>25') %>% 
  rename(geo = region,
         deaths = value) %>% 
  group_by(geo, year, sex, scenario) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ungroup() %>% 
  left_join(nuts3_plot_data,
            by = 'geo')
deaths_nuts3_sf <- sf::st_sf(deaths_nuts3, geometry = deaths_nuts3$geometry)
if(nrow(deaths_nuts3[rowSums(is.na(deaths_nuts3))>0,]) != 0) {
  warning("There are not-matching NUTS3 codes between the rfasst MORT results and EUROSTAT NUTS3 data")
}

plot_deaths <- tm_shape(nuts3_plot_data,
                    projection = "EPSG:3035",
                    xlim = c(2400000, 6500000),
                    ylim = c(1320000, 5650000)
  ) +
  tm_fill("lightgrey") +
  tm_shape(deaths_nuts3_sf %>% 
             dplyr::filter(year == 2020,
                           sex == 'Both')) +
  tm_polygons("deaths",
              title = "Premature\ndeaths [NR]",
              palette = "Oranges",
              style = "cont"
  ) +
  tm_layout(legend.title.size = 0.8) +
  tm_layout(legend.text.size = 0.6)

tmap::tmap_save(plot_deaths, filename = "figures/plot_deaths.pdf",
                width = 100, height = 100, units = 'mm', dpi = 300)




## AP vs URBN_TYPE  ============================================================
# graph OX URBN_TYPE i OY nivell de PM. Fer punts, i violin plot?
# fer test estadístic per a veure si hi ha alguna relació

# check normality
ap_urbntype_sf <- sf::st_join(
  socioecon_dt_ind_urbntype_sf,
  ap_nuts3_sf %>% 
    dplyr::select(-URBN_TYPE)
) %>% 
  select(geo, URBN_TYPE, ap, geometry)

# Histogram
ggplot(ap_urbntype_sf, aes(x = ap)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ URBN_TYPE)

# Q-Q plot
ggplot(ap_urbntype_sf, aes(sample = ap)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ URBN_TYPE)

# Anderson-Darling Test
test <- nortest::ad.test(ap_urbntype_sf$ap)
# A = 59.643, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(ap ~ URBN_TYPE, data = ap_urbntype_sf)
# Kruskal-Wallis chi-squared = 401.25, df = 2, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(ap_urbntype_sf), ap ~ URBN_TYPE)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   1 ap     8724  0.0458 eta2[H] small    
# Result: the effect of the URBN_TYPE is SIGNIFICANT but small


df <- data.table::as.data.table(ap_urbntype_sf) %>% 
  select(-geometry)
df_medi <- df[, .(medi = quantile(ap, 0.5, na.rm = T)),
              by=c('URBN_TYPE')]

df = data.table::data.table(df)

plot_urbntype_density <- ggplot(df) +
  geom_density(data = df, aes(x = ap, group = URBN_TYPE,
                              color = URBN_TYPE, fill = URBN_TYPE), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = URBN_TYPE, xintercept = medi),
             data = df_medi, linewidth = 1) +
  facet_wrap(. ~ URBN_TYPE, nrow = 3) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = urbn_type.color,
                    name = 'Urban type',
                    labels = urbn_type.labs)+
  scale_color_manual(values = urbn_type.color,
                     name = 'Urban type',
                     labels = urbn_type.labs)+
  labs(x = 'PM2.5 concentration [ug/m3]', y = 'Probability density') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE,
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave(file='figures/plot_urbntype_density.pdf', height = 15, width = 15, units = 'cm',
       plot = plot_urbntype_density)



## AP vs ELDERLY ===============================================================

# check normality
ap_elderly_sf <- sf::st_join(
  grid_sf_elderly_v3,
  ap_nuts3_sf %>% 
    dplyr::select(ap, geometry)
) %>% 
  select(geo, mean_per_elderly, ap, geometry)

ap_elderly <- data.table::as.data.table(ap_elderly_sf) %>% 
  select(-geometry) %>% 
  filter(rowSums(is.na(.)) == 0)

# Histogram
ggplot(ap_elderly, aes(x = ap)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ mean_per_elderly) %>% 
  theme(legend.position = 'none')

# Q-Q plot
ggplot(ap_elderly, aes(sample = ap)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ mean_per_elderly) %>% 
  theme(legend.position = 'none')

# Anderson-Darling Test
test <- nortest::ad.test(ap_elderly$ap)
# A = 65.33, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(ap ~ mean_per_elderly, data = ap_elderly_sf)
# Kruskal-Wallis chi-squared = 6192.6, df = 1199, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(ap_elderly_sf), ap ~ mean_per_elderly)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   ap     7518   0.790 eta2[H] large    
# Result: the effect of the mean_per_elderly is SIGNIFICANT and LARGE



ap_elderly <- ap_elderly %>% 
  dplyr::mutate(mean_per_elderly_decile = as.factor(ntile(mean_per_elderly, 5)))

ap_elderly_medi <- ap_elderly[, .(medi = quantile(ap, 0.5, na.rm = T)),
                              by=c('mean_per_elderly_decile')]

plot_elderly_density <- ggplot(ap_elderly) +
  geom_density(data = ap_elderly, aes(x = ap, group = mean_per_elderly_decile,
                              color = mean_per_elderly_decile, fill = mean_per_elderly_decile), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = mean_per_elderly_decile, xintercept = medi),
             data = ap_elderly_medi, linewidth = 1) +
  facet_wrap(. ~ mean_per_elderly_decile, nrow = 5) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = quintiles.color,
                    name = 'Elderly population [%]',
                    labels = quintiles.labs)+
  scale_color_manual(values = quintiles.color,
                     name = 'Elderly population [%]',
                     labels = quintiles.labs)+
  labs(x = 'PM2.5 concentration [ug/m3]', y = 'Probability density') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE,
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave(file='figures/plot_elderly_density.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_elderly_density)

## DEATHS vs ELDERLY ===============================================================

# check normality
deaths_elderly_sf <- sf::st_join(
  grid_sf_elderly_v3,
  deaths_nuts3_sf %>% 
    dplyr::select(deaths, geometry)
) %>% 
  select(geo, mean_per_elderly, deaths, geometry)

deaths_elderly <- data.table::as.data.table(deaths_elderly_sf) %>% 
  select(-geometry) %>% 
  filter(rowSums(is.na(.)) == 0) %>% 
  # remove outliers(>90%)
  filter(deaths <= quantile(deaths, probs = 0.90))

# Histogram
ggplot(deaths_elderly, aes(x = deaths)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ mean_per_elderly) %>% 
  theme(legend.position = 'none')

# Q-Q plot
ggplot(deaths_elderly, aes(sample = deaths)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ mean_per_elderly) %>% 
  theme(legend.position = 'none')

# Anderson-Darling Test
test <- nortest::ad.test(deaths_elderly$deaths)
# A = 13606, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(deaths ~ mean_per_elderly, data = deaths_elderly_sf)
# Kruskal-Wallis chi-squared = 53064, df = 1169, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(deaths_elderly_sf), deaths ~ mean_per_elderly)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   deaths 108293   0.484 eta2[H] large    
# Result: the effect of the mean_per_elderly is SIGNIFICANT and LARGE



deaths_elderly <- deaths_elderly %>% 
  dplyr::mutate(mean_per_elderly_decile = as.factor(ntile(mean_per_elderly, 5)))

deaths_elderly_medi <- deaths_elderly[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by=c('mean_per_elderly_decile')]

plot_elderly_density_deaths <- ggplot(deaths_elderly) +
  geom_density(data = deaths_elderly, aes(x = deaths, group = mean_per_elderly_decile,
                              color = mean_per_elderly_decile, fill = mean_per_elderly_decile), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = mean_per_elderly_decile, xintercept = medi),
             data = deaths_elderly_medi, linewidth = 1) +
  facet_wrap(. ~ mean_per_elderly_decile, nrow = 5,
             labeller = as_labeller(quintiles.labs)) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = quintiles.color,
                    name = 'Elderly population [%]',
                    labels = quintiles.labs)+
  scale_color_manual(values = quintiles.color,
                     name = 'Elderly population [%]',
                     labels = quintiles.labs)+
  labs(x = 'Premature deaths [NR]', y = 'Probability density') +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey90"),
        panel.ontop = FALSE,
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave(file='figures/plot_elderly_density_deaths.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_elderly_density_deaths)
