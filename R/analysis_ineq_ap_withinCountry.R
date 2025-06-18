require(eurostat)
library(ggplot2)
library(tmap)
library(magrittr)

source("R/utils.R")
source("R/zzz.R")

normalized <- T
split_num <- 5 #10 deciles, 5 quintiles
map <- F #T if plotted and saved, F otherwise
yy <- 2030
# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================
normalized_tag <- dplyr::if_else(normalized, '_norm100k', '')
split_num_tag <- dplyr::if_else(split_num == 5, 'quintile', 
                                dplyr::if_else(split_num == 10, 'decile',
                                               paste0('split_num_',split_num)))

## rfasst + socioeconomid data =================================================
ap_socioecon_sf <- get(load('ap_socioecon_sf.RData'))
deaths_socioecon_sf <- get(load('deaths_socioecon_sf_newdeaths.RData'))

rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 

ap <- get(load("data/rfasst_output/necp_m2_get_conc_pm25.ctry_agg.output.RData")) %>%
  dplyr::filter(year == yy)

deaths <- get(load(paste0("data/rfasst_output/necp_m3_get_mort_pm25.output.RData"))) %>%
  dplyr::select(region, year, age, sex, disease, value = GBD, scenario) %>% 
  dplyr::filter(year == yy,
                sex == 'Both')

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
  dplyr::select(-FID)

# ==============================================================================
#                                    ANALYSIS                                  #
# ==============================================================================
## AP  =========================================================================
ap_nuts3 <- ap %>%
  dplyr::filter(
    nchar(region) > 3
  ) %>%
  dplyr::rename(
    geo = region,
    ap = value
  ) %>%
  dplyr::left_join(data.table::as.data.table(nuts3_plot_data) %>% 
                     dplyr::select(geo, CNTR_CODE),
                   by = "geo"
  ) %>% 
  dplyr::select(geo, year, units, ap, scenario, ctry = CNTR_CODE) %>% 
  unique() %>% 
  # mean concentration by country
  dplyr::group_by(year, units, scenario, ctry) %>% 
  dplyr::mutate(ap_mean = mean(ap)) %>% 
  dplyr::ungroup() %>% 
  # concentration rate by nuts3 over the mean country concentration
  dplyr::mutate(ap_rate = ap / ap_mean) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(geo, URBN_TYPE, geometry),
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
    tm_polygons("ap_rate",
                title = "Regional Pollution\nIndex",
                palette = "Oranges",
                style = "cont",
                lwd = 0.5
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  
  tmap::tmap_save(plot_ap,
                  filename = paste0("figures/withinCtry/plot_ap.pdf"),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
}

## DEATHS  =====================================================================
deaths_nuts3 <- deaths %>%
  dplyr::filter(
    nchar(region) > 3,
    sex == 'Both'
  ) %>%
  dplyr::rename(
    geo = region,
    deaths = value
  ) %>%
  dplyr::left_join(data.table::as.data.table(nuts3_plot_data) %>% 
                     dplyr::select(geo, CNTR_CODE),
                   by = "geo"
  ) %>% 
  dplyr::select(geo, year, deaths, scenario, ctry = CNTR_CODE) %>% 
  unique() %>% 
  # mean deaths by country
  dplyr::group_by(year, scenario, ctry) %>% 
  dplyr::mutate(deaths_mean = mean(deaths, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # deaths rate by nuts3 over the mean country deaths
  dplyr::mutate(deaths_rate = deaths / deaths_mean) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(geo, geometry),
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
               dplyr::filter(deaths_rate < 2)) +
    tm_polygons("deaths_rate",
                title = "Regional Deaths\nIndex",
                palette = "Oranges",
                style = "cont",       lwd = 0.5
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  
  tmap::tmap_save(plot_deaths,
                  filename = paste0("figures/withinCtry/plot_deaths", normalized_tag, ".pdf"),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
}


## AP vs urbn_type  ============================================================
# check normality
ap_urbntype_sf <- ap_socioecon_sf %>%
  dplyr::select(geo, ctry, urbn_type, ap, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

ap_urbntype <- data.table::as.data.table(ap_urbntype_sf) %>%
  dplyr::select(-geometry) %>% 
  dplyr::filter(!is.na(ctry))
ap_urbntype_medi <- ap_urbntype[, .(medi = quantile(ap, 0.5, na.rm = T)),
              by = c("urbn_type","ctry")
]

if (map) {
  plot_urbntype_density <- ggplot(ap_urbntype_medi %>% 
                                    dplyr::arrange(ctry),
                                  aes(y = ctry, x = medi, color = urbn_type)) +
    geom_point() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = urbn_type.color,
      name = "Urban type",
      labels = urbn_type.labs
    ) +
    labs(x = "PM2.5 concentration [ug/m3]", y = "Probability density") +
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
    file = paste0("figures/withinCtry/plot_urbntype_density_ap.pdf"), height = 20, width = 15, units = "cm",
    plot = plot_urbntype_density
  )
}


## AP vs ELDERLY ===============================================================

# check normality
ap_elderly_sf <- ap_socioecon_sf %>%
  dplyr::select(geo, ctry, per_elderly, ap, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

ap_elderly <- data.table::as.data.table(ap_elderly_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(per_elderly_decile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

ap_elderly_medi <- ap_elderly[, .(medi = quantile(ap, 0.5, na.rm = T)),
                              by = c("per_elderly_decile","ctry")
]


if (map) {
  dd <- ap_elderly %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(geo, geometry),
      by = "geo"
    )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_elderly_map <- tm_shape(nuts3_plot_data,
                              projection = "EPSG:3035",
                              xlim = c(2400000, 6500000),
                              ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(per_elderly_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("per_elderly_decile",
                title = "Elderly Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_elderly_map,
                  filename = "figures/withinCtry/plot_elderly_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  plot_elderly_density <- ggplot(ap_elderly_medi %>% 
                                   dplyr::arrange(ctry),
                                 aes(y = ctry, x = medi, color = per_elderly_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "Elderly population [%]",
      labels = quintiles.labs
    ) +
    labs(x = "PM2.5 concentration [ug/m3]", y = "") +
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
    file = paste0("figures/withinCtry/plot_elderly_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_elderly_density
  )
}

## AP vs INCOME ===============================================================

# check normality
ap_income_sf <- ap_socioecon_sf %>%
  dplyr::select(geo, ctry, income, ap, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

ap_income <- data.table::as.data.table(ap_income_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(income_decile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

ap_income_medi <- ap_income[, .(medi = quantile(ap, 0.5, na.rm = T)),
                              by = c("income_decile","ctry")
]


if (map) {
  dd <- ap_income %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(geo, geometry),
      by = "geo"
    )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_income_map <- tm_shape(nuts3_plot_data,
                           projection = "EPSG:3035",
                           xlim = c(2400000, 6500000),
                           ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(income_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("income_decile",
                title = "Income Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_income_map,
                  filename = "figures/withinCtry/plot_income_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  plot_income_density <- ggplot(ap_income_medi %>% 
                                   dplyr::arrange(ctry),
                                 aes(y = ctry, x = medi, color = income_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "PM2.5 concentration [ug/m3]", y = "") +
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
    file = paste0("figures/withinCtry/plot_income_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_income_density
  )
}

## AP vs GDP ===============================================================

# check normality
ap_gdp_sf <- ap_socioecon_sf %>%
  dplyr::select(geo, ctry, gdp, ap, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

ap_gdp <- data.table::as.data.table(ap_gdp_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(gdp_decile = as.factor(dplyr::ntile(gdp, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

ap_gdp_medi <- ap_gdp[, .(medi = quantile(ap, 0.5, na.rm = T)),
                              by = c("gdp_decile","ctry")
]


if (map) {
  dd <- ap_gdp %>% 
    dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_gdp_map <- tm_shape(nuts3_plot_data,
                       projection = "EPSG:3035",
                       xlim = c(2400000, 6500000),
                       ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(gdp_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("gdp_decile",
                title = "GDP Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_gdp_map,
                  filename = "figures/withinCtry/plot_gdp_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
                  
  
  plot_gdp_density <- ggplot(ap_gdp_medi %>% 
                               tidyr::pivot_wider(names_from = 'gdp_decile', values_from = 'medi') %>% 
                               dplyr::mutate(diff = dplyr::if_else(!is.na(`5`), `1` - `5`,
                                                                   dplyr::if_else(!is.na(`4`), `1` - `4`,
                                                                                  dplyr::if_else(!is.na(`3`), `1` - `3`,
                                                                                                 dplyr::if_else(!is.na(`2`), `1` - `2`, 0))))) %>% 
                               tidyr::pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`), 
                                                   names_to = 'gdp_decile', 
                                                   values_to = 'medi') %>% 
                               dplyr::arrange(diff),
                             aes(y = ctry, x = medi, color = gdp_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "PM2.5 concentration [ug/m3]", y = "") +
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
    file = paste0("figures/withinCtry/plot_gdp_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gdp_density
  )
}

## AP vs GINI ===============================================================

# check normality
ap_gini_sf <- ap_socioecon_sf %>%
  dplyr::select(geo, ctry, gini, ap, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

ap_gini <- data.table::as.data.table(ap_gini_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(gini_decile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

ap_gini_medi <- ap_gini[, .(medi = quantile(ap, 0.5, na.rm = T)),
                              by = c("gini_decile","ctry")
]


if (map) {
  dd <- ap_gini %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(geo, geometry),
      by = "geo"
    )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_gini_map <- tm_shape(nuts3_plot_data,
                           projection = "EPSG:3035",
                           xlim = c(2400000, 6500000),
                           ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(gini_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("gini_decile",
                title = "GINI Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_gini_map,
                  filename = "figures/withinCtry/plot_gini_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  
  plot_gini_density <- ggplot(ap_gini_medi %>% 
                                   dplyr::arrange(ctry),
                                 aes(y = ctry, x = medi, color = gini_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "PM2.5 concentration [ug/m3]", y = "") +
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
    file = paste0("figures/withinCtry/plot_gini_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gini_density
  )
}

## DEATHS vs urbn_type  ============================================================
# check normality
deaths_urbntype_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, ctry, urbn_type, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

deaths_urbntype <- data.table::as.data.table(deaths_urbntype_sf) %>%
  dplyr::select(-geometry) %>% 
  dplyr::filter(!is.na(ctry))
deaths_urbntype_medi <- deaths_urbntype[, .(medi = quantile(deaths, 0.5, na.rm = T)),
              by = c("urbn_type","ctry")
]

if (map) {
  plot_urbntype_density <- ggplot(deaths_urbntype_medi %>% 
                                    dplyr::arrange(ctry),
                                  aes(y = ctry, x = medi, color = urbn_type)) +
    geom_point() +
    ggpubr::theme_pubr() +
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
    file = paste0("figures/withinCtry/plot_urbntype_density_deaths.pdf"), height = 20, width = 15, units = "cm",
    plot = plot_urbntype_density
  )
}


## DEATHS vs ELDERLY ===============================================================

# check normality
deaths_elderly_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, ctry, per_elderly, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

deaths_elderly <- data.table::as.data.table(deaths_elderly_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(per_elderly_decile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

deaths_elderly_medi <- deaths_elderly[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by = c("per_elderly_decile","ctry")
]


if (map) {
  dd <- deaths_elderly %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(geo, geometry),
      by = "geo"
    )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_elderly_map <- tm_shape(nuts3_plot_data,
                              projection = "EPSG:3035",
                              xlim = c(2400000, 6500000),
                              ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(per_elderly_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("per_elderly_decile",
                title = "Elderly Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_elderly_map,
                  filename = "figures/withinCtry/plot_elderly_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  plot_elderly_density <- ggplot(deaths_elderly_medi %>% 
                                   dplyr::arrange(ctry),
                                 aes(y = ctry, x = medi, color = per_elderly_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "Elderly population [%]",
      labels = quintiles.labs
    ) +
    labs(x = "Premature deaths [Deaths per 1M inhabitants]", y = "") +
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
    file = paste0("figures/withinCtry/plot_elderly_density_deaths.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_elderly_density
  )
}

## DEATHS vs INCOME ===============================================================

# check normality
deaths_income_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, ctry, income, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

deaths_income <- data.table::as.data.table(deaths_income_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(income_decile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

deaths_income_medi <- deaths_income[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by = c("income_decile","ctry")
]


if (map) {
  dd <- deaths_income %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(geo, geometry),
      by = "geo"
    )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_income_map <- tm_shape(nuts3_plot_data,
                           projection = "EPSG:3035",
                           xlim = c(2400000, 6500000),
                           ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(income_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("income_decile",
                title = "Income Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_income_map,
                  filename = "figures/withinCtry/plot_income_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  plot_income_density <- ggplot(deaths_income_medi %>% 
                                   dplyr::arrange(ctry),
                                 aes(y = ctry, x = medi, color = income_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "Premature deaths [Deaths per 1M inhabitants]", y = "") +
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
    file = paste0("figures/withinCtry/plot_income_density_deaths.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_income_density
  )
}

## DEATHS vs GDP ===============================================================

# check normality
deaths_gdp_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, ctry, gdp, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

deaths_gdp <- data.table::as.data.table(deaths_gdp_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(gdp_decile = as.factor(dplyr::ntile(gdp, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

deaths_gdp_medi <- deaths_gdp[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by = c("gdp_decile","ctry")
]


if (map) {
  dd <- deaths_gdp %>% 
    dplyr::left_join(
    nuts3_plot_data %>%
      dplyr::select(geo, geometry),
    by = "geo"
  )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_gdp_map <- tm_shape(nuts3_plot_data,
                       projection = "EPSG:3035",
                       xlim = c(2400000, 6500000),
                       ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(gdp_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("gdp_decile",
                title = "GDP Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_gdp_map,
                  filename = "figures/withinCtry/plot_gdp_deaths_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
                  
  
  plot_gdp_density <- ggplot(deaths_gdp_medi %>% 
                               tidyr::pivot_wider(names_from = 'gdp_decile', values_from = 'medi') %>% 
                               dplyr::mutate(diff = dplyr::if_else(!is.na(`5`), `1` - `5`,
                                                                   dplyr::if_else(!is.na(`4`), `1` - `4`,
                                                                                  dplyr::if_else(!is.na(`3`), `1` - `3`,
                                                                                                 dplyr::if_else(!is.na(`2`), `1` - `2`, 0))))) %>% 
                               tidyr::pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`), 
                                                   names_to = 'gdp_decile', 
                                                   values_to = 'medi') %>% 
                               dplyr::arrange(diff),
                             aes(y = ctry, x = medi, color = gdp_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "Premature deaths [Deaths per 1M inhabitants]", y = "") +
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
    file = paste0("figures/withinCtry/plot_gdp_density_deaths.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gdp_density
  )
}

## DEATHS vs GINI ===============================================================

# check normality
deaths_gini_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, ctry, gini, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

deaths_gini <- data.table::as.data.table(deaths_gini_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::group_by(ctry) %>% 
  dplyr::mutate(gini_decile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::ungroup() %>% 
  data.table::as.data.table()

deaths_gini_medi <- deaths_gini[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by = c("gini_decile","ctry")
]


if (map) {
  dd <- deaths_gini %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(geo, geometry),
      by = "geo"
    )
  dd <- sf::st_sf(dd, geometry = dd$geometry)
  
  plot_gini_map <- tm_shape(nuts3_plot_data,
                           projection = "EPSG:3035",
                           xlim = c(2400000, 6500000),
                           ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(dd %>%
               dplyr::select(gini_decile, geometry) %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("gini_decile",
                title = "GINI Quintile\nby Ctry",
                palette = "Oranges",
                style = "cont"
    ) +
    tm_layout(legend.title.size = 0.8) +
    tm_layout(legend.text.size = 0.6)
  tmap::tmap_save(plot_gini_map,
                  filename = "figures/withinCtry/plot_gini_map.pdf",
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  
  plot_gini_density <- ggplot(deaths_gini_medi %>% 
                                   dplyr::arrange(ctry),
                                 aes(y = ctry, x = medi, color = gini_decile)) +
    geom_point() +
    ggpubr::theme_pubr() +
    ggpubr::theme_pubr() +
    scale_color_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    labs(x = "Premature deaths [Deaths per 1M inhabitants]", y = "") +
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
    file = paste0("figures/withinCtry/plot_gini_density_deaths.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gini_density
  )
}

# ==============================================================================
#                                    Figures                                   #
# ==============================================================================


## Figure 2 ====================================================================

# AP
data_list <- list(
  ap_income_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "income_medi", contains("medi")),
  
  ap_gini_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "gini_medi", contains("medi")),
  
  # ap_gdp_medi %>%
  #   dplyr::rename_with(~ "decile", contains("decile")) %>%
  #   dplyr::rename_with(~ "gdp_medi", contains("medi")),
  
  ap_elderly_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "elderly_medi", contains("medi"))
)

# Merge all datasets by 'decile'
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = c("decile","ctry"))) %>%
  dplyr::left_join(ap_urbntype_medi %>%
              dplyr::rename_with(~ "urbn_medi", contains("medi")) %>% 
              dplyr::mutate(urbn_type = as.factor(urbn_type)),
            by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-decile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(decile = as.factor(decile),
                urbn_type = as.factor(urbn_type))

# Plotting
pl <- ggplot() +
  geom_point(
    data = data %>% 
      dplyr::select(decile, ctry, variable, value) %>% 
      dplyr::filter(variable != 'urbn_medi') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, color = decile), size = 2, alpha = 0.85) + 
  geom_point(
    data = data %>% 
      dplyr::select(urbn_type, ctry, variable, value) %>% 
      dplyr::filter(variable == 'urbn_medi') %>% 
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
pl
ggsave(
  file = paste0("figures/withinCtry/fig_ap_var_",yy,"_",split_num_tag,".pdf"), height = 10, width = 15, units = "cm",
  plot = pl
)



# Deaths
data_list <- list(
  deaths_income_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "income_medi", contains("medi")),
  
  deaths_gini_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "gini_medi", contains("medi")),
  
  # deaths_gdp_medi %>%
  #   dplyr::rename_with(~ "decile", contains("decile")) %>%
  #   dplyr::rename_with(~ "gdp_medi", contains("medi")),
  
  deaths_elderly_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "elderly_medi", contains("medi"))
)

# Merge all datasets by 'decile'
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = c("decile","ctry"))) %>%
  dplyr::left_join(deaths_urbntype_medi %>%
              dplyr::rename_with(~ "urbn_medi", contains("medi")) %>% 
              dplyr::mutate(urbn_type = as.factor(urbn_type)),
            by = 'ctry'
  ) %>% 
  tidyr::pivot_longer(cols = c(-decile,-urbn_type,-ctry), 
                      names_to = "variable", values_to = "value") %>% 
  dplyr::mutate(decile = as.factor(decile),
                urbn_type = as.factor(urbn_type))

# Plotting
pl <- ggplot() +
  geom_point(
    data = data %>% 
      dplyr::select(decile, ctry, variable, value) %>% 
      dplyr::filter(variable != 'urbn_medi') %>% 
      dplyr::distinct(),
    aes(y = factor(ctry), x = value, color = decile), size = 2, alpha = 0.85) +
  geom_point(
    data = data %>% 
      dplyr::select(urbn_type, ctry, variable, value) %>% 
      dplyr::filter(variable == 'urbn_medi') %>% 
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
  labs(x = "Premture deaths [Deaths per 1M inhabitants]", y = "") +
  theme_minimal()
pl
ggsave(
  file = paste0("figures/withinCtry/fig_deaths_var",yy,"_",split_num_tag,"_",normalized_tag,".pdf"), height = 10, width = 15, units = "cm",
  plot = pl
)



# ==============================================================================
#                              MACHINE LEARNING                                #
# ==============================================================================
source('R/prova_ml.R')

## AP vs INCOME ----------------------------------------------------------------
data <- ap_income_medi %>%
  dplyr::rename_with(~ "decile", contains("decile")) %>%
  dplyr::rename_with(~ "income_medi", contains("medi")) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(decile = paste0('Q',decile)) %>%
  dplyr::arrange(decile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'decile', values_from = 'income_medi') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 8, 'withinCtry/ml_income',
          fig_legend = "Income\nper capita\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]",
          fix = T)


## AP vs ELDERLY --------------------------------------------------------------
data <- ap_elderly_medi %>%
  dplyr::rename_with(~ "decile", contains("decile")) %>%
  dplyr::rename_with(~ "per_elderly_decile", contains("medi")) %>% 
  tibble::as.tibble() %>% 
  dplyr::mutate(decile = paste0('Q',decile)) %>%
  dplyr::arrange(decile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'decile', values_from = 'per_elderly_decile') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)
  
ml_do_all(data, 2, 'withinCtry/ml_elderly_ap',
          fig_legend = "Elderly\nproportion\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]")  


## AP vs GINI ------------------------------------------------------------------
data <- ap_gini_medi %>%
  dplyr::rename_with(~ "decile", contains("decile")) %>%
  dplyr::rename_with(~ "gini_decile", contains("medi")) %>% 
  tibble::as.tibble() %>% 
  dplyr::mutate(decile = paste0('Q',decile)) %>%
  dplyr::arrange(decile, ctry) %>% 
  tidyr::pivot_wider(names_from = 'decile', values_from = 'gini_decile') %>% 
  dplyr::rename(country = ctry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)

ml_do_all(data, 4, 'withinCtry/ml_gini_ap',
          fig_legend = "Gini\nindex\nquintile",
          fig_ox_label = "PM2.5 concentration [ug/m3]")  


  

