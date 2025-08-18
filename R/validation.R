#' run_validation
#' @description function to validate the rfasst results. The idea is to compare the 
#' raw rfasst output (PM2.5 concentration and premature deaths) in 2020 with 
#' historical values from EEA.
#' The historical data is obtained from 
#' https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.ebd.countries_and_nuts&ScenarioDescription=Baseline%20from%20WHO%202021%20AQG&UrbanisationDegree=All%20Areas%20(incl.unclassified)&Year=2022&Sex=Total
#' @param ap PM2.5 concentration
#' @param deaths premature deaths due to PM2.5
#' @param yy year for the historical data - sensitivity parameter
#' @return comparison charts

run_validation <- function(ap, deaths, yy) {
  hist_data <- read.csv('data/ValidationData/ValidationData.csv') %>% 
    dplyr::group_by(Country = Country.Or.Territory,
                    region = NUTS.Code,
                    year = Year) %>% 
    dplyr::summarise(ap_hist = mean(Air.Pollution.Average..ug.m3.),
                     deaths_hist = sum(Value),
                     deaths_hist.LCI = sum(Value...lower.CI),
                     deaths_hist.HCI = sum(Value...upper.CI),
                     .groups = 'drop') %>% 
    dplyr::filter(year == yy)
  
  ## ap ------------------------------------------------------------------------
  validate_ap <- ap %>% 
    dplyr::mutate(year = as.integer(as.character(year))) %>% 
    # only rfasst 2020 output to check
    dplyr::filter(year == 2020) %>% 
    # only NUTS3
    dplyr::filter(nchar(region) == 5) %>% 
    # merge with hist data
    dplyr::left_join(hist_data %>% 
                       dplyr::select('Country','region','ap_hist'),
                     by = c('region')) %>% # NOTE: only UK and Turkey are not mapped
    dplyr::filter(rowSums(is.na(.)) == 0) %>% 
    # compute abs diff
    dplyr::mutate(abs_diff = value - ap_hist)
  
  # plot abs diff
  validate_ap <- validate_ap %>% 
    dplyr::left_join(nuts3_plot_data,
                   by = c('region' = 'geo')
  )
  validate_ap_sf <- sf::st_sf(validate_ap, geometry = validate_ap$geometry)
  plot_validate_ap <- ggplot() +
    geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
    geom_sf(data = validate_ap_sf, 
            aes(fill = abs_diff), 
            color = "black",
            size = 0.0001) + 
    scale_fill_gradient2(
      low = "yellow",
      mid = "white",
      high = "purple",
      midpoint = 0,
      name = "PM2.5 [µg/m³]\nabsolute\ndifference",
      na.value = "grey90") +
    coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.justification = "top", 
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(1.5, "cm"),
      legend.title = element_text(size = legend.title.size, hjust = 0.5),
      legend.text = element_text(size = legend.text.size),
      panel.border = element_blank(), 
      panel.background = element_rect(fill = "white", color = NA), 
      panel.grid = element_blank(),
      axis.text = element_blank()
    )
  
  ggsave(plot_validate_ap,
         filename = file.path("figures/validation/",paste0('plot_validate_ap_',yy,'.pdf')),
         width = 11, height = 10, units = "cm")
  
  ## deaths ------------------------------------------------------------------------
  validate_deaths <- deaths %>% 
    dplyr::mutate(year = as.integer(as.character(year))) %>% 
    # only rfasst 2020 output to check
    dplyr::filter(year == 2020) %>% 
    # only NUTS3
    dplyr::filter(nchar(region) == 5) %>% 
    # merge with hist data
    dplyr::left_join(hist_data %>% 
                       dplyr::select(-c(ap_hist,year)),
                     by = c('region')) %>% # NOTE: only UK and Turkey are not mapped
    dplyr::filter(rowSums(is.na(.)) == 0) %>% 
    # compute abs diff
    dplyr::mutate(abs_diff = value - deaths_hist) %>% 
    # check if the value is inside the CI
    dplyr::mutate(inside_CI = dplyr::if_else(value >= deaths_hist.LCI &
                                               value <= deaths_hist.HCI, TRUE, FALSE))
  
  # Percentage of values inside CI
  # validate_deaths %>%
  #      dplyr::count(inside_CI) %>%
  #      dplyr::mutate(percentage = 100 * n / sum(n))
  
  
  # plots
  validate_deaths <- validate_deaths %>% 
    dplyr::left_join(nuts3_plot_data,
                   by = c('region' = 'geo')
  )
  validate_deaths_sf <- sf::st_sf(validate_deaths, geometry = validate_deaths$geometry)
  
  # plot abs diff
  plot_validate_deaths <- ggplot() +
    geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
    geom_sf(data = validate_deaths_sf, 
            aes(fill = abs_diff), 
            color = "black",
            size = 0.0001) + 
    scale_fill_gradient2(
      low = "yellow",
      mid = "white",
      high = "purple",
      midpoint = 0,
      name = "Premature Deaths\n[Deaths per inhabitants]\nabsolute difference",
      na.value = "grey90") +
    coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.justification = "top", 
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(1.5, "cm"),
      legend.title = element_text(size = legend.title.size, hjust = 0.5),
      legend.text = element_text(size = legend.text.size),
      panel.border = element_blank(), 
      panel.background = element_rect(fill = "white", color = NA), 
      panel.grid = element_blank(),
      axis.text = element_blank()
    )

  ggsave(plot_validate_deaths,
         filename = file.path("figures/validation/",paste0('plot_validate_deaths_',yy,'.pdf')),
         width = 11, height = 10, units = "cm")
  
  # plot inside CI
  plot_validate_deaths_ci <- ggplot() +
    geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
    geom_sf(data = validate_deaths_sf, 
            aes(fill = inside_CI), 
            color = "black",
            size = 0.0001) + 
    scale_fill_manual(
      values = c(`TRUE` = "green", `FALSE` = "red"),
      labels = c(`TRUE` = "Yes", `FALSE` = "No"),
      name = "Premature Deaths estimate\ninside the historical data CI?",
      na.value = "grey90"
    ) +
    coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.justification = "top", 
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(1.5, "cm"),
      legend.title = element_text(size = legend.title.size, hjust = 0.5),
      legend.text = element_text(size = legend.text.size),
      panel.border = element_blank(), 
      panel.background = element_rect(fill = "white", color = NA), 
      panel.grid = element_blank(),
      axis.text = element_blank()
    )
  
  ggsave(plot_validate_deaths_ci,
         filename = file.path("figures/validation/",paste0('plot_validate_deaths_ci_',yy,'.pdf')),
         width = 11, height = 10, units = "cm")
  
}