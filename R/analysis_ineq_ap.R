library(ggplot2)
library(tmap)

source('R/utils.R')
source('R/zzz.R')

# ==============================================================================
#                                  LOAD DATA                                   #
# ==============================================================================
## load dummy PM2.5 concentration & premature deaths by NUTS3
ap <- get(load('data/rfasst_output/tmp_m2_get_conc_pm25.ctry_nuts.output.RData'))
deaths <- get(load('data/rfasst_output/tmp_m3_get_mort_pm25.output.RData')) %>% 
  select(region, year, age, sex, disease, value = GBD, scenario)

## load socioeconomic data
new_colnames <- c('URBN_TYPE' = 'URBN_TY', 'geo' = 'geo', 'mean_per_elderly' = 'mn_pr_l',
                  'Year' = 'Year', 'ISO' = 'ISO', 'Population_nuts3' = 'Ppltn_3',
                  'Disp_Inc_P_nuts3' = 'D_I_P_3', 'Gini_nuts3' = 'Gn_nts3',
                  'geometry' = 'geometry')
harm_socioeconomic_nuts_sf <- sf::st_read("data/tmp/harm_socioeconomic_nuts_sf.shp") %>% 
  dplyr::rename(new_colnames)

## load spacial data
nuts3_plot_data <- get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>% 
  dplyr::select(-FID)

# ==============================================================================
#                                    ANALYSIS                                  #
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
# check normality
ap_urbntype_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf,
  ap_nuts3_sf %>% 
    dplyr::select(-URBN_TYPE, -geo)
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
# A = 74.25, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(ap ~ URBN_TYPE, data = ap_urbntype_sf)
# Kruskal-Wallis chi-squared = 796.57, df = 2, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(ap_urbntype_sf), ap ~ URBN_TYPE)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   1 ap     8928  0.0890 eta2[H] moderate    
# Result: the effect of the URBN_TYPE is SIGNIFICANT but moderate


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

ggsave(file='figures/plot_urbntype_density_ap.pdf', height = 15, width = 15, units = 'cm',
       plot = plot_urbntype_density)



## AP vs ELDERLY ===============================================================

# check normality
ap_elderly_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf,
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
# A = 73.063, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(ap ~ mean_per_elderly, data = ap_elderly_sf)
# Kruskal-Wallis chi-squared = 7255.4, df = 1199, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(ap_elderly_sf), ap ~ mean_per_elderly)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   ap     8928   0.784 eta2[H] large    
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

ggsave(file='figures/plot_elderly_density_ap.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_elderly_density)

## DEATHS vs URBN_TYPE ===========================================================

# check normality
deaths_urbntype_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf,
  deaths_nuts3_sf %>% 
    dplyr::select(deaths, geometry)
) %>% 
  select(geo, URBN_TYPE, deaths, geometry)

deaths_urbntype <- data.table::as.data.table(deaths_urbntype_sf) %>% 
  select(-geometry) %>% 
  filter(rowSums(is.na(.)) == 0) %>% 
  # remove outliers(>90%)
  filter(deaths <= quantile(deaths, probs = 0.90))

# Histogram
ggplot(deaths_urbntype, aes(x = deaths)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ URBN_TYPE) %>% 
  theme(legend.position = 'none')

# Q-Q plot
ggplot(deaths_urbntype, aes(sample = deaths)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ URBN_TYPE) %>% 
  theme(legend.position = 'none')

# Anderson-Darling Test
test <- nortest::ad.test(deaths_urbntype$deaths)
# A = 3364.5, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(deaths ~ URBN_TYPE, data = deaths_urbntype_sf)
# Kruskal-Wallis chi-squared = 5273.1, df = 2, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(deaths_urbntype_sf), deaths ~ URBN_TYPE)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   deaths 127621  0.0413 eta2[H] small    
# Result: the effect of the URBN_TYPE is SIGNIFICANT and SMALL


deaths_urbntype_medi <- deaths_urbntype[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by=c('URBN_TYPE')] 

plot_urbntype_density_deaths <- ggplot(deaths_urbntype) +
  geom_density(data = deaths_urbntype, aes(x = deaths, group = URBN_TYPE,
                              color = URBN_TYPE, fill = URBN_TYPE), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = URBN_TYPE, xintercept = medi),
             data = deaths_urbntype_medi, linewidth = 1) +
  facet_wrap(. ~ URBN_TYPE, nrow = 5,
             labeller = as_labeller(urbn_type.labs)) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = urbn_type.color,
                    name = 'Urban type',
                    labels = urbn_type.labs)+
  scale_color_manual(values = urbn_type.color,
                     name = 'Urban type',
                     labels = urbn_type.labs)+
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

ggsave(file='figures/plot_urbntype_density_deaths.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_urbntype_density_deaths)

## DEATHS vs CDD ===========================================================

# check normality
deaths_cdd_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf,
  deaths_nuts3_sf %>% 
    dplyr::select(deaths, geometry)
) %>% 
  select(geo, cdd = CDD, deaths, geometry)

deaths_cdd <- data.table::as.data.table(deaths_cdd_sf) %>% 
  select(-geometry) %>% 
  filter(rowSums(is.na(.)) == 0)

# Histogram
ggplot(deaths_cdd, aes(x = deaths)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme(legend.position = 'none')

# Q-Q plot
ggplot(deaths_cdd, aes(sample = deaths)) +
  stat_qq() +
  stat_qq_line() +
  theme(legend.position = 'none')

# Anderson-Darling Test
test <- nortest::ad.test(deaths_cdd$deaths)
# A = 15155, p-value < 2.2e-16 (all data) --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(deaths ~ cdd, data = deaths_cdd_sf)
# Kruskal-Wallis chi-squared = 52418, df = 1016, p-value < 2.2e-16 (all data)
rstatix::kruskal_effsize(data = as.data.frame(deaths_cdd_sf), deaths ~ cdd)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   deaths 144454   0.358 eta2[H] large    
# Result: the effect of the cdd is SIGNIFICANT and LARGE


deaths_cdd <- deaths_cdd %>% 
  dplyr::mutate(mean_cdd_decile = as.factor(ntile(cdd, 5)))

deaths_cdd_medi <- deaths_cdd[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by=c('mean_cdd_decile')]

plot_cdd_density_deaths <- ggplot(deaths_cdd) +
  geom_density(data = deaths_cdd, aes(x = deaths, group = mean_cdd_decile,
                              color = mean_cdd_decile, fill = mean_cdd_decile), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = mean_cdd_decile, xintercept = medi),
             data = deaths_cdd_medi, linewidth = 1) +
  facet_wrap(. ~ mean_cdd_decile, nrow = 5,
             labeller = as_labeller(quintiles.labs)) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = quintiles.color,
                    name = 'CDD [NR]',
                    labels = quintiles.labs)+
  scale_color_manual(values = quintiles.color,
                     name = 'CDD [NR]',
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

ggsave(file='figures/plot_cdd_density_deaths.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_cdd_density_deaths)

## DEATHS vs ELDERLY ===========================================================

# check normality
deaths_elderly_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf,
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
# A = 3366, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(deaths ~ mean_per_elderly, data = deaths_elderly_sf)
# Kruskal-Wallis chi-squared = 61598, df = 1169, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(deaths_elderly_sf), deaths ~ mean_per_elderly)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   deaths 127621   0.478 eta2[H] large    
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

## DEATHS vs INCOME ============================================================

# check normality
deaths_income_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf %>% 
    dplyr::select(geo, income = Disp_Inc_P_nuts3, geometry),
  deaths_nuts3_sf %>% 
    dplyr::select(deaths, geometry)
) %>% 
  select(geo, income, deaths, geometry)

deaths_income <- data.table::as.data.table(deaths_income_sf) %>% 
  select(-geometry) %>% 
  filter(rowSums(is.na(.)) == 0) %>% 
  # remove outliers(>95%)
  filter(deaths <= quantile(deaths, probs = 0.95))

# Histogram
ggplot(deaths_income, aes(x = deaths)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ income) %>% 
  theme(legend.position = 'none')

# Q-Q plot
ggplot(deaths_income, aes(sample = deaths)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ income) %>% 
  theme(legend.position = 'none')

# Anderson-Darling Test
test <- nortest::ad.test(deaths_income$deaths)
# A = 3883.6, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(deaths ~ income, data = deaths_income_sf)
# Kruskal-Wallis chi-squared = 47227, df = 1124, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(deaths_income_sf), deaths ~ income)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   deaths 127621   0.364 eta2[H] large    
# Result: the effect of the income is SIGNIFICANT and LARGE



deaths_income <- deaths_income %>% 
  dplyr::mutate(income_decile = as.factor(ntile(income, 5)))

deaths_elderly_medi <- deaths_income[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by=c('income_decile')]

plot_income_density_deaths <- ggplot(deaths_income) +
  geom_density(data = deaths_income, aes(x = deaths, group = income_decile,
                              color = income_decile, fill = income_decile), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = income_decile, xintercept = medi),
             data = deaths_elderly_medi, linewidth = 1) +
  facet_wrap(. ~ income_decile, nrow = 5,
             labeller = as_labeller(quintiles.labs)) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = quintiles.color,
                    name = 'Income Quintiles [2015 PPP]',
                    labels = quintiles.labs)+
  scale_color_manual(values = quintiles.color,
                     name = 'Income Quintiles [2015 PPP]',
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

ggsave(file='figures/plot_income_density_deaths.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_income_density_deaths)

## DEATHS vs GINI ============================================================

# check normality
deaths_gini_sf <- sf::st_join(
  harm_socioeconomic_nuts_sf %>% 
    dplyr::select(geo, gini = Gini_nuts3, geometry),
  deaths_nuts3_sf %>% 
    dplyr::select(deaths, geometry)
) %>% 
  select(geo, gini, deaths, geometry)

deaths_gini <- data.table::as.data.table(deaths_gini_sf) %>% 
  select(-geometry) %>% 
  filter(rowSums(is.na(.)) == 0) %>% 
  # remove outliers(>95%)
  filter(deaths <= quantile(deaths, probs = 0.95))

# Histogram
ggplot(deaths_gini, aes(x = deaths)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ gini) %>% 
  theme(legend.position = 'none')

# Q-Q plot
ggplot(deaths_gini, aes(sample = deaths)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ gini) %>% 
  theme(legend.position = 'none')

# Anderson-Darling Test
test <- nortest::ad.test(deaths_gini$deaths)
# A = 3883.6, p-value < 2.2e-16 --> NOT normal distribution

# Kruskal-Wallis Test (for >2 categories) - non parametric test
test <- kruskal.test(deaths ~ gini, data = deaths_gini_sf)
# Kruskal-Wallis chi-squared = 48183, df = 958, p-value < 2.2e-16
rstatix::kruskal_effsize(data = as.data.frame(deaths_gini_sf), deaths ~ gini)
# .y.       n effsize method  magnitude
# * <chr> <int>   <dbl> <chr>   <ord>    
#   deaths 127621   0.373 eta2[H] large       
# Result: the effect of the gini is SIGNIFICANT and LARGE



deaths_gini <- deaths_gini %>% 
  dplyr::mutate(gini_decile = as.factor(ntile(gini, 5)))

deaths_elderly_medi <- deaths_gini[, .(medi = quantile(deaths, 0.5, na.rm = T)),
                              by=c('gini_decile')]

plot_gini_density_deaths <- ggplot(deaths_gini) +
  geom_density(data = deaths_gini, aes(x = deaths, group = gini_decile,
                              color = gini_decile, fill = gini_decile), 
               linewidth = 0.8, alpha = 0.25) +
  geom_vline(aes(color = gini_decile, xintercept = medi),
             data = deaths_elderly_medi, linewidth = 1) +
  facet_wrap(. ~ gini_decile, nrow = 5,
             labeller = as_labeller(quintiles.labs)) +
  ggpubr::theme_pubr() +
  scale_fill_manual(values = quintiles.color,
                    name = 'Gini Quintiles [Index]',
                    labels = quintiles.labs)+
  scale_color_manual(values = quintiles.color,
                     name = 'Gini Quintiles [Index]',
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

ggsave(file='figures/plot_gini_density_deaths.pdf', height = 30, width = 20, units = 'cm',
       plot = plot_gini_density_deaths)
