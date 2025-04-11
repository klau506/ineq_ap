require(eurostat)
library(ggplot2)
library(tmap)
library(magrittr)

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
ap_socioecon_sf <- get(load('ap_socioecon_sf.RData'))
deaths_socioecon_sf <- get(load('deaths_socioecon_sf_newdeaths.RData'))

rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 

rfasst_ctry_pop <- rfasst::pop.all.ctry_ctry.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value)  

ap <- get(load("data/rfasst_output/tmp_m2_get_conc_pm25.ctry_nuts.output.RData")) %>%
  dplyr::filter(year == yy)

deaths <- get(load(paste0("data/rfasst_output/tmp_m3_get_mort_pm25.output.RData"))) %>%
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

## load spacial data ===========================================================
nuts3_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID)
ctry_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 0, year = 2021) %>%
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
      palette = "Blues",
      style = "cont",
      lwd = 0.5
    ) +
    tm_layout(legend.title.size = 0.8,
              legend.text.size = 0.6,
              frame = FALSE)
  
  tmap::tmap_save(plot_ap,
    filename = paste0("figures/plot_ap.pdf"),
    width = 100, height = 100, units = "mm", dpi = 300
  )
}

## DEATHS  =====================================================================
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
      lwd = 0.5
    ) +
    tm_layout(legend.title.size = 0.8,
              legend.text.size = 0.6,
              legend.position = c("right", "top"),  # Fix legend in the top-right corner
              legend.bg.color = "white",  # Optional: Adds a white background
              legend.bg.alpha = 0.8,      # Optional: Makes background slightly transparent
              legend.width = 1.5,
              frame = FALSE)
  
  tmap::tmap_save(plot_deaths,
    filename = paste0("figures/plot_deaths", normalized_tag, ".pdf"),
    width = 100, height = 100, units = "mm", dpi = 300
  )
}


## AP vs DEATHS  ===============================================================
harm_data_geo_urbn <- data.table::as.data.table(harm_socioeconomic_nuts_sf) %>%
  dplyr::select(geo, urbn_type)
harm_data_geo_urbn <- unique(harm_data_geo_urbn)

ap_deaths_nuts3 <- deaths %>%
  dplyr::filter(
    nchar(region) > 3
  ) %>%
  dplyr::rename(
    geo = region,
    deaths = value
  ) %>%
  dplyr::left_join(ap %>%
                     dplyr::filter(
                       nchar(region) > 3
                     ) %>%
                     dplyr::rename(
                       geo = region,
                       ap = value
                     ) %>% 
                     dplyr::mutate(year = as.numeric(year)) %>%
                     dplyr::select(geo, scenario, ap),
                   by = c('geo','scenario')) %>% 
  dplyr::left_join(harm_data_geo_urbn %>% 
                     dplyr::filter(rowSums(is.na(.)) == 0),
                   by = 'geo') %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(CNTR_CODE, geo),
                   by = "geo"
  )
  
if (map) {
  plot_deaths_ap <- 
    ggplot(ap_deaths_nuts3 %>% 
           dplyr::filter(deaths != 0,
                         sex == 'Both'), 
         aes(x = deaths, y = ap, color = CNTR_CODE, shape = urbn_type)) +
    geom_point(alpha = 0.5, size = 2) +
    theme_minimal() +
    labs(x = "Premature deaths [Deaths per 1M inhabitants]", y = "PM2.5 concentration [ug/m3]")
  
  ggsave(
    file = paste0("figures/plot_deaths_ap", normalized_tag, ".pdf"), height = 15, width = 15, units = "cm",
    plot = plot_deaths_ap
  )
}


## AP vs urbn_type  ============================================================
# check normality
ap_urbntype_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, urbn_type, ap, geometry) %>% 
  unique()

# # Histogram
# ggplot(ap_urbntype_sf, aes(x = ap)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   facet_wrap(~urbn_type)
# 
# # Q-Q plot
# ggplot(ap_urbntype_sf, aes(sample = ap)) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~urbn_type)
# 
# # Anderson-Darling Test
# test <- nortest::ad.test(ap_urbntype_sf$ap)
# # A = 74.25, p-value < 2.2e-16 --> NOT normal distribution
# 
# # Kruskal-Wallis Test (for >2 categories) - non parametric test
# test <- kruskal.test(ap ~ urbn_type, data = ap_urbntype_sf)
# # Kruskal-Wallis chi-squared = 796.57, df = 2, p-value < 2.2e-16
# rstatix::kruskal_effsize(data = as.data.frame(ap_urbntype_sf), ap ~ urbn_type)
# # .y.       n effsize method  magnitude
# # * <chr> <int>   <dbl> <chr>   <ord>
# #   1 ap     8928  0.0890 eta2[H] moderate
# # Result: the effect of the urbn_type is SIGNIFICANT but moderate


df <- data.table::as.data.table(ap_urbntype_sf) %>%
  dplyr::select(-geometry)
df_medi <- df[, .(medi = quantile(ap, 0.5, na.rm = T)),
  by = c("urbn_type")
]

df <- data.table::data.table(df)

if (map) {
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
    file = paste0("figures/plot_urbntype_density_ap.pdf"), height = 15, width = 15, units = "cm",
    plot = plot_urbntype_density
  )
}


## AP vs CDD ===========================================================

# check normality
ap_cdd_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, cdd = CDD, ap, geometry) %>% 
  unique() 

ap_cdd <- data.table::as.data.table(ap_cdd_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(ap <= quantile(ap, probs = 0.95))

# # Histogram
# ggplot(ap_cdd, aes(x = ap)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   theme(legend.position = "none")
# 
# # Q-Q plot
# ggplot(ap_cdd, aes(sample = ap)) +
#   stat_qq() +
#   stat_qq_line() +
#   theme(legend.position = "none")

ap_cdd <- ap_cdd %>%
  dplyr::mutate(mean_cdd_decile = as.factor(dplyr::ntile(cdd, split_num)))

ap_cdd_medi <- ap_cdd[, .(medi = quantile(ap, 0.5, na.rm = T)),
                              by = c("mean_cdd_decile")
]


if (map) {
  plot_cdd_density_ap <- ggplot(ap_cdd) +
    geom_density(
      data = ap_cdd, aes(
        x = ap, group = mean_cdd_decile,
        color = mean_cdd_decile, fill = mean_cdd_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = mean_cdd_decile, xintercept = medi),
               data = ap_cdd_medi, linewidth = 1
    ) +
    facet_wrap(. ~ mean_cdd_decile,
               nrow = 5,
               labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "CDD [NR]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "CDD [NR]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_cdd_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_cdd_density_ap
  )
}

## AP vs ELDERLY ===============================================================

# check normality
ap_elderly_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, per_elderly, ap, geometry) %>% 
  unique() 


ap_elderly <- data.table::as.data.table(ap_elderly_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0)

# # Histogram
# ggplot(ap_elderly, aes(x = ap)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   facet_wrap(~per_elderly) %>%
#   theme(legend.position = "none")
# 
# # Q-Q plot
# ggplot(ap_elderly, aes(sample = ap)) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~per_elderly) %>%
#   theme(legend.position = "none")
# 
# # Anderson-Darling Test
# test <- nortest::ad.test(ap_elderly$ap)
# # A = 73.063, p-value < 2.2e-16 --> NOT normal distribution
# 
# # Kruskal-Wallis Test (for >2 categories) - non parametric test
# test <- kruskal.test(ap ~ per_elderly, data = ap_elderly_sf)
# # Kruskal-Wallis chi-squared = 7255.4, df = 1199, p-value < 2.2e-16
# rstatix::kruskal_effsize(data = as.data.frame(ap_elderly_sf), ap ~ per_elderly)
# # .y.       n effsize method  magnitude
# # * <chr> <int>   <dbl> <chr>   <ord>
# #   ap     8928   0.784 eta2[H] large
# # Result: the effect of the per_elderly is SIGNIFICANT and LARGE

ap_elderly <- ap_elderly %>%
  dplyr::mutate(per_elderly_decile = as.factor(dplyr::ntile(per_elderly, split_num)))

ap_elderly_medi <- ap_elderly[, .(medi = quantile(ap, 0.5, na.rm = T)),
  by = c("per_elderly_decile")
]


if (map) {
  plot_elderly_density <- ggplot(ap_elderly) +
    geom_density(
      data = ap_elderly, aes(
        x = ap, group = per_elderly_decile,
        color = per_elderly_decile, fill = per_elderly_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = per_elderly_decile, xintercept = medi),
      data = ap_elderly_medi, linewidth = 1
    ) +
    facet_wrap(. ~ per_elderly_decile, nrow = 5) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "Elderly population [%]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "Elderly population [%]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_elderly_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_elderly_density
  )
}

## AP vs INCOME ============================================================

# check normality
ap_income_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, income, ap, geometry) %>% 
  unique() 

ap_income <- data.table::as.data.table(ap_income_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(ap <= quantile(ap, probs = 0.95))

# # Histogram
# ggplot(ap_income, aes(x = ap)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   facet_wrap(~income) %>%
#   theme(legend.position = "none")
# 
# # Q-Q plot
# ggplot(ap_income, aes(sample = ap)) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~income) %>%
#   theme(legend.position = "none")
# 

ap_income <- ap_income %>%
  dplyr::mutate(income_decile = as.factor(dplyr::ntile(income, split_num)))

ap_income_medi <- ap_income[, .(medi = quantile(ap, 0.5, na.rm = T)),
                                     by = c("income_decile")
]

if (map) {
  plot_income_density_ap <- ggplot(ap_income) +
    geom_density(
      data = ap_income, aes(
        x = ap, group = income_decile,
        color = income_decile, fill = income_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = income_decile, xintercept = medi),
               data = ap_income_medi, linewidth = 1
    ) +
    facet_wrap(. ~ income_decile,
               nrow = 5,
               labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_income_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_income_density_ap
  )
}

## AP vs GDP ============================================================

# check normality
ap_gdp_sf <- sf::st_intersection(
  harm_socioeconomic_nuts_sf %>%
    dplyr::filter(sex == 'Both') %>% 
    dplyr::select(geo, gdp, geometry) %>%
    dplyr::filter(rowSums(is.na(.)) == 0) %>% 
    unique(),
  ap_nuts3_sf %>%
    dplyr::select(ap, geometry) %>% 
    unique()
) %>%
  dplyr::select(geo, gdp, ap, geometry)

ap_gdp <- data.table::as.data.table(ap_gdp_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(ap <= quantile(ap, probs = 0.95))

# # Histogram
# ggplot(ap_gdp, aes(x = ap)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   facet_wrap(~gdp) %>%
#   theme(legend.position = "none")
# 
# # Q-Q plot
# ggplot(ap_gdp, aes(sample = ap)) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~gdp) %>%
#   theme(legend.position = "none")


ap_gdp <- ap_gdp %>%
  dplyr::mutate(gdp_decile = as.factor(dplyr::ntile(gdp, split_num)))

ap_gdp_medi <- ap_gdp[, .(medi = quantile(ap, 0.5, na.rm = T)),
                                     by = c("gdp_decile")
]

if (map) {
  plot_gdp_density_ap <- ggplot(ap_gdp) +
    geom_density(
      data = ap_gdp, aes(
        x = ap, group = gdp_decile,
        color = gdp_decile, fill = gdp_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = gdp_decile, xintercept = medi),
               data = ap_gdp_medi, linewidth = 1
    ) +
    facet_wrap(. ~ gdp_decile,
               nrow = 5,
               labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_gdp_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gdp_density_ap
  )
}

## AP vs GINI ============================================================

# check normality
ap_gini_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, gini, ap, geometry) %>% 
  unique() 


ap_gini <- data.table::as.data.table(ap_gini_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(ap <= quantile(ap, probs = 0.95))

# # Histogram
# ggplot(ap_gini, aes(x = ap)) +
#   geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#   facet_wrap(~gini) %>%
#   theme(legend.position = "none")
# 
# # Q-Q plot
# ggplot(ap_gini, aes(sample = ap)) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~gini) %>%
#   theme(legend.position = "none")


ap_gini <- ap_gini %>%
  dplyr::mutate(gini_decile = as.factor(dplyr::ntile(gini, split_num)))

ap_gini_medi <- ap_gini[, .(medi = quantile(ap, 0.5, na.rm = T)),
                                   by = c("gini_decile")
]

if (map) {
  plot_gini_density_ap <- ggplot(ap_gini) +
    geom_density(
      data = ap_gini, aes(
        x = ap, group = gini_decile,
        color = gini_decile, fill = gini_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = gini_decile, xintercept = medi),
               data = ap_gini_medi, linewidth = 1
    ) +
    facet_wrap(. ~ gini_decile,
               nrow = 5,
               labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "Gini Quintiles [Index]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "Gini Quintiles [Index]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_gini_density_ap.pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gini_density_ap
  )
}
  


## DEATHS vs urbn_type ===========================================================

# check normality
deaths_urbntype_sf <- deaths_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, urbn_type, deaths, geometry) %>% 
  unique() 

deaths_urbntype <- data.table::as.data.table(deaths_urbntype_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(deaths <= quantile(deaths, probs = 0.95))


deaths_urbntype_medi <- deaths_urbntype[, .(medi = quantile(deaths, 0.5, na.rm = T)),
  by = c("urbn_type")
]

if (map) {
  plot_urbntype_density_deaths <- ggplot(deaths_urbntype) +
    geom_density(
      data = deaths_urbntype, aes(
        x = deaths, group = urbn_type,
        color = urbn_type, fill = urbn_type
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = urbn_type, xintercept = medi),
      data = deaths_urbntype_medi, linewidth = 1
    ) +
    facet_wrap(. ~ urbn_type,
      nrow = 5,
      labeller = as_labeller(urbn_type.labs)
    ) +
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
    file = paste0("figures/plot_urbntype_density_deaths", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
    plot = plot_urbntype_density_deaths
  )
}

## DEATHS vs CDD ===========================================================

# check normality
deaths_cdd_sf <- deaths_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, cdd = CDD, deaths, geometry) %>% 
  unique() 

deaths_cdd <- data.table::as.data.table(deaths_cdd_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(deaths <= quantile(deaths, probs = 0.95)) %>%
  dplyr::mutate(mean_cdd_decile = as.factor(dplyr::ntile(cdd, split_num)))

deaths_cdd_medi <- deaths_cdd[, .(medi = quantile(deaths, 0.5, na.rm = T)),
  by = c("mean_cdd_decile")
]

if (map) {
  plot_cdd_density_deaths <- ggplot(deaths_cdd) +
    geom_density(
      data = deaths_cdd, aes(
        x = deaths, group = mean_cdd_decile,
        color = mean_cdd_decile, fill = mean_cdd_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = mean_cdd_decile, xintercept = medi),
      data = deaths_cdd_medi, linewidth = 1
    ) +
    facet_wrap(. ~ mean_cdd_decile,
      nrow = 5,
      labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "CDD [NR]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "CDD [NR]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_cdd_density_deaths", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
    plot = plot_cdd_density_deaths
  )
}

## DEATHS vs ELDERLY ===========================================================

# check normality
deaths_elderly_sf <- deaths_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, per_elderly, deaths, geometry) %>% 
  unique() 

deaths_elderly <- data.table::as.data.table(deaths_elderly_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(deaths <= quantile(deaths, probs = 0.95)) %>%
  dplyr::mutate(per_elderly_decile = as.factor(dplyr::ntile(per_elderly, split_num)))

deaths_elderly_medi <- deaths_elderly[, .(medi = quantile(deaths, 0.5, na.rm = T)),
  by = c("per_elderly_decile")
]

if (map) {
  plot_elderly_density_deaths <- ggplot(deaths_elderly) +
    geom_density(
      data = deaths_elderly, aes(
        x = deaths, group = per_elderly_decile,
        color = per_elderly_decile, fill = per_elderly_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = per_elderly_decile, xintercept = medi),
      data = deaths_elderly_medi, linewidth = 1
    ) +
    facet_wrap(. ~ per_elderly_decile,
      nrow = 5,
      labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "Elderly population [%]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "Elderly population [%]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_elderly_density_deaths", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
    plot = plot_elderly_density_deaths
  )
}

## DEATHS vs INCOME ============================================================

# check normality
deaths_income_sf <- deaths_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, income, deaths, geometry) %>% 
  unique() 

deaths_income <- data.table::as.data.table(deaths_income_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(deaths <= quantile(deaths, probs = 0.95)) %>%
  dplyr::mutate(income_decile = as.factor(dplyr::ntile(income, split_num)))

deaths_income_medi <- deaths_income[, .(medi = quantile(deaths, 0.5, na.rm = T)),
  by = c("income_decile")
]

if (map) {
  plot_income_density_deaths <- ggplot(deaths_income) +
    geom_density(
      data = deaths_income, aes(
        x = deaths, group = income_decile,
        color = income_decile, fill = income_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = income_decile, xintercept = medi),
      data = deaths_income_medi, linewidth = 1
    ) +
    facet_wrap(. ~ income_decile,
      nrow = 5,
      labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "Income Quintiles [2015 PPP]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_income_density_deaths", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
    plot = plot_income_density_deaths
  )
}

## DEATHS vs GDP ============================================================

# check normality
deaths_gdp_sf <- deaths_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, gdp, deaths, geometry) %>% 
  unique() 

deaths_gdp <- data.table::as.data.table(deaths_gdp_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(deaths <= quantile(deaths, probs = 0.95)) %>%
  dplyr::mutate(gdp_decile = as.factor(dplyr::ntile(gdp, split_num)))

deaths_gdp_medi <- deaths_gdp[, .(medi = quantile(deaths, 0.5, na.rm = T)),
  by = c("gdp_decile")
]

if (map) {
  plot_gdp_density_deaths <- ggplot(deaths_gdp) +
    geom_density(
      data = deaths_gdp, aes(
        x = deaths, group = gdp_decile,
        color = gdp_decile, fill = gdp_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = gdp_decile, xintercept = medi),
      data = deaths_gdp_medi, linewidth = 1
    ) +
    facet_wrap(. ~ gdp_decile,
      nrow = 5,
      labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "GDP Quintiles [2015 PPP]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_gdp_density_deaths", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gdp_density_deaths
  )
}

## DEATHS vs GINI ============================================================

# check normality
deaths_gini_sf <- deaths_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, gini, deaths, geometry) %>% 
  unique() 

deaths_gini <- data.table::as.data.table(deaths_gini_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  # remove outliers(>95%)
  dplyr::filter(deaths <= quantile(deaths, probs = 0.95)) %>%
  dplyr::mutate(gini_decile = as.factor(dplyr::ntile(gini, split_num)))

deaths_gini_medi <- deaths_gini[, .(medi = quantile(deaths, 0.5, na.rm = T)),
  by = c("gini_decile")
]

if (map) {
  plot_gini_density_deaths <- ggplot(deaths_gini) +
    geom_density(
      data = deaths_gini, aes(
        x = deaths, group = gini_decile,
        color = gini_decile, fill = gini_decile
      ),
      linewidth = 0.8, alpha = 0.25
    ) +
    geom_vline(aes(color = gini_decile, xintercept = medi),
      data = deaths_gini_medi, linewidth = 1
    ) +
    facet_wrap(. ~ gini_decile,
      nrow = 5,
      labeller = as_labeller(quintiles.labs)
    ) +
    ggpubr::theme_pubr() +
    scale_fill_manual(
      values = quintiles.color,
      name = "Gini Quintiles [Index]",
      labels = quintiles.labs
    ) +
    scale_color_manual(
      values = quintiles.color,
      name = "Gini Quintiles [Index]",
      labels = quintiles.labs
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
    file = paste0("figures/plot_gini_density_deaths", normalized_tag, ".pdf"), height = 30, width = 20, units = "cm",
    plot = plot_gini_density_deaths
  )
}

# ==============================================================================
#                                    Figures                                   #
# ==============================================================================


## Check urbn_type & elderly ===================================================
check_urbnType_elderly <- data.table::as.data.table(harm_socioeconomic_nuts_sf) %>% 
  dplyr::filter(sex == 'Both') %>% 
  dplyr::select(geo, urbn_type, per_elderly) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)
print(check_urbnType_elderly %>% 
        dplyr::group_by(urbn_type) %>% 
        dplyr::summarise(mean_per_elderly = mean(per_elderly)) %>% 
        dplyr::ungroup())
# urbn_type   mean_per_elderly
# City                   0.196
# Town/Suburb            0.204
# Rural                  0.223


## Check urbn_type & income ====================================================
check_urbnType_income <- data.table::as.data.table(harm_socioeconomic_nuts_sf) %>% 
  dplyr::filter(sex == 'Both') %>% 
  dplyr::select(geo, urbn_type, income = Disp_Inc_P_nuts3) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)
print(check_urbnType_income %>% 
        dplyr::group_by(urbn_type) %>% 
        dplyr::summarise(mean_income = mean(income)) %>% 
        dplyr::ungroup())
# urbn_type         mean_income
# City                   19073
# Town/Suburb            17142
# Rural                  15421


## Check urbn_type & cdd =======================================================
check_urbnType_cdd <- data.table::as.data.table(harm_socioeconomic_nuts_sf) %>% 
  dplyr::filter(sex == 'Both') %>% 
  dplyr::select(geo, urbn_type, cdd = CDD) %>% 
  unique() %>% 
  dplyr::filter(rowSums(is.na(.)) == 0)
print(check_urbnType_cdd %>% 
        dplyr::group_by(urbn_type) %>% 
        dplyr::summarise(mean_cdd = mean(cdd)) %>% 
        dplyr::ungroup())
# urbn_type          mean_cdd
# City                   102.
# Town/Suburb            80.8
# Rural                  89.9

## Figure 2 ====================================================================

# AP
data_list <- list(
  ap_income_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "income_medi", contains("medi")),
  
  ap_gini_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "gini_medi", contains("medi")),

  ap_gdp_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "gdp_medi", contains("medi")),
  
  ap_cdd_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "cdd_medi", contains("medi")),
  
  ap_elderly_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "elderly_medi", contains("medi"))
)

# Merge all datasets by 'decile'
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = "decile")) %>% 
  tidyr::pivot_longer(cols = -decile, names_to = "variable", values_to = "value")

# Plotting
pl <- ggplot(data,
       # %>% 
       #   dplyr::filter(decile %in% c(1,5,split_num)),
       aes(y = factor(variable), x = value, color = factor(decile))) +
  geom_point() +
  # scale_color_manual(values = viridis::viridis(n = split_num, option = "cividis")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = split_num, name = "RdYlBu")) +
  labs(x = "PM2.5 concentration [ug/m3]", y = "", fill = "Decile") +
  theme_minimal()
ggsave(
  file = paste0("figures/fig_ap_var_",yy,"_",split_num_tag,".pdf"), height = 20, width = 20, units = "cm",
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
  
  deaths_gdp_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "gdp_medi", contains("medi")),

  deaths_cdd_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "cdd_medi", contains("medi")),
  
  deaths_elderly_medi %>%
    dplyr::rename_with(~ "decile", contains("decile")) %>%
    dplyr::rename_with(~ "elderly_medi", contains("medi"))
)

# Merge all datasets by 'decile'
data <- purrr::reduce(data_list, function(x, y) merge(x, y, by = "decile")) %>% 
  tidyr::pivot_longer(cols = -decile, names_to = "variable", values_to = "value")

# Plotting

pl <- ggplot(data,
             # %>% 
             #   dplyr::filter(decile %in% c(1,5,split_num)),
             aes(y = factor(variable), x = value, color = factor(decile))) +
  geom_point() +
  # scale_color_manual(values = viridis::viridis(n = split_num, option = "cividis")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = split_num, name = "RdYlBu")) +
  labs(x = "Premture deaths [Deaths per 1M inhabitants]", y = "", fill = "Decile") +
  theme_minimal()
ggsave(
  file = paste0("figures/fig_deaths_var",yy,"_",split_num_tag,"_",normalized_tag,".pdf"), height = 20, width = 20, units = "cm",
  plot = pl
)
