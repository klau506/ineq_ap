require(sf)
require(tmap)
require(dplyr)
require(rfasst)
require(ggplot2)
require(jsonlite)
require(eurostat)

library(dplyr)
library(eurostat)
library(sf)
library(tmap)

source('R/utils.R')

#####################################################################
#                           NUTS analysis                           #
#####################################################################
year = 2030

### 1. PM2.5 premature mortality data ###############################


### 1.a. run rfasst or load its output

# library(rfasst)
# mort_data <- m3_get_mort_pm25(prj_name = 'output/prova_eur.dat',
#                              gcam_eur = T,
#                              recompute = T,
#                              scen_name = 'Reference_vintage_eur_v2',
#                              final_db_year = 2050,
#                              ssp = "SSP2", saveOutput = T,
#                              map = T, anim = F)

mort_data <- read.csv(paste0('data/rfasst_output/PM25_MORT_AGG_Reference_vintage_eur_v2_',year,'.csv'))


### 1.b. downscale the mort_data to NUTS3
# load data
downscaling_data <- read.csv(paste0('data/mort_NUTS.csv'), skip = 3) %>% 
  dplyr::filter(Degree.Of.Urbanisation == 'All Areas (incl.unclassified)', Air.Pollutant == 'PM2.5') %>% 
  dplyr::select(NUTS1, NUTS3, Population, Premature.Deaths)
  # TODO adapt AD: Andorra, BA: Bosnia and Herzegovina, KS: Kosovo, MC: Monaco, SM: San Marino
map_plot(downscaling_data, 'NUTS3', 'Premature.Deaths', 
         title = 'EEA premature deaths PM2.5 2021', save = T, 
         save_name = 'map_EEA_NUTS3_PM25_premature_deaths.png')
  

nuts3_data <- xlsx::read.xlsx(paste0('data/NUTS2021.xlsx'), sheetName = 'NUTS & SR 2021') %>% 
  dplyr::select(NUTS3 = Code.2021, NUTS.level) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(NUTS.level == 3) %>% 
  dplyr::mutate(iso2_code  = stringr::str_sub(NUTS3,1,2))
iso2_iso3 <- jsonlite::fromJSON('data/iso2-iso3.json') %>% 
  dplyr::select(iso2_code, iso3_code)

# join nuts3_data with iso3 code
nuts3_data <- nuts3_data %>% 
  dplyr::left_join(iso2_iso3, by = 'iso2_code') %>% 
  # fix Greece (EL = GR) and United Kingdom (UK = GB)
  dplyr::mutate(iso3_code = dplyr::if_else(iso2_code == 'EL','GRC',iso3_code)) %>% 
  dplyr::mutate(iso3_code = dplyr::if_else(iso2_code == 'UK','GBR',iso3_code))

# join mort data with iso3 code
mort_data2 <- mort_data %>% 
  dplyr::left_join(rfasst::Regions_EUR, by = c('region' = 'FASST region')) %>% 
  dplyr::left_join(nuts3_data, by = c('ISO3' = 'iso3_code')) %>% 
  # remove non-EUR regions
  dplyr::filter(!is.na(NUTS.level)) %>% 
  dplyr::select(-NUTS.level) %>% 
  dplyr::rename(rfasst_reg = region, iso3_code = ISO3)

# compute population weights by NUTS3 / rfasst_reg
data_mort <- downscaling_data %>% 
  dplyr::left_join(mort_data2, by = 'NUTS3') %>% 
  dplyr::filter(!is.na(iso3_code))
map_plot(data_mort, 'NUTS3', 'GBD',
         title = 'rfasst premature deaths\ndue to PM2.5 in 2030 by rfasst_reg', save = T, 
         save_name = 'map_rfasst_rfasstreg_PM25_premature_deaths.png')

data_mort_NUTS3 <- downscaling_data %>% 
  dplyr::left_join(mort_data2, by = 'NUTS3') %>% 
  dplyr::filter(!is.na(iso3_code)) %>% 
  dplyr::group_by(rfasst_reg) %>% 
  dplyr::mutate(rfasst_reg_Population = sum(Population)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pm_weight = Premature.Deaths / rfasst_reg_Population) %>%
  dplyr::mutate(FUSION = FUSION * 1e3 * pm_weight / Population,
                GBD = GBD * 1e3 * pm_weight / Population,
                GEMM = GEMM * 1e3 * pm_weight / Population) 
map_plot(data_mort_NUTS3, 'NUTS3', 'GBD',
         title = 'rfasst premature deaths population weighted\ndue to PM2.5 in 2030 downscaled NUTS3', save = T, 
         save_name = 'map_rfasst_NUTS3_PM25_premature_deaths.png')


### 2. socioeconomic data #########################################

### 2.a. download data

data_socioecon_NUTS3 <- eurostat::get_eurostat("cens_21agr3", time_format = "raw") %>%
  # subset to have only a single row per NUTS3
  dplyr::filter(TIME_PERIOD == 2021, nchar(geo) == 5) %>% 
  # compute weights (by age and sex)
  dplyr::group_by(geo) %>% 
  dplyr::mutate(total_socio = values[age == "TOTAL" & sex == "T"]) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(w_value = values / total_socio)


### 2.b. merge mort and socioeconomic data

data_NUTS3 <- merge(
  data_mort_NUTS3,
  data_socioecon_NUTS3,
  by.x = 'NUTS3', 
  by.y = 'geo'
) %>% 
  # compute premature deaths by socioeconomic group
  dplyr::mutate(FUSION = FUSION * w_value,
                GBD = GBD * w_value,
                GEMM = GEMM * w_value)
# by sex
map_facet_plot(data_NUTS3 %>% 
                 dplyr::filter(age == 'TOTAL'), 
               'NUTS3', plot_var = 'GBD', facet_var = 'sex',
               title = 'rfasst premature deaths population weighted\ndue to PM2.5 in 2030 downscaled NUTS3 by sex', 
               save = T, save_name = 'map_rfasst_NUTS3_PM25_premature_deaths_by_sex.png')
# by age
map_facet_plot(data_NUTS3 %>% 
                 dplyr::filter(sex == 'T') %>% 
                 dplyr::mutate(age = factor(age, levels = c("Y_LT15","Y15-29","Y30-49","Y50-64","Y65-84","Y_GE85","TOTAL"))), 
               'NUTS3', plot_var = 'GBD', facet_var = 'age',
               title = 'rfasst premature deaths population weighted\ndue to PM2.5 in 2030 downscaled NUTS3 by age', 
               save = T, save_name = 'map_rfasst_NUTS3_PM25_premature_deaths_by_age.png')


# 3. analysis
data_by_sex = data_NUTS3 %>% 
  dplyr::filter(age == 'TOTAL', sex != 'T')

summary_statistics <- 
  data_by_sex %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(
    mean_final = mean(GBD, na.rm = TRUE),
    median_final = median(GBD, na.rm = TRUE),
    sd_final = sd(GBD, na.rm = TRUE),
    count = n()
  )
print(summary_statistics)


ggplot(data_by_sex, aes(x = sex, y = GBD, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of premature deaths by sex",
       x = "Sex",
       y = "Premtaure deaths population weighted")


anova_result <- aov(GBD ~ sex, data = data_by_sex)
residuals <- residuals(anova_result)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(residuals); qqline(residuals, col = "red")
shapiro.test(residuals) # ANOVA assumptions not matched

kruskal.test(GBD ~ sex, data = data_by_sex)
pairwise.wilcox.test(data_by_sex$GBD, data_by_sex$sex, p.adjust.method = "bonferroni")


library(randomForest)
rf_model <- randomForest(GBD ~ ., data = data_by_sex %>% 
                           dplyr::select(GBD,sex,NUTS3))
importance(rf_model)

library(pdp)
partial_plot <- partial(rf_model, pred.var = "sex", plot = TRUE)
partial_plot
