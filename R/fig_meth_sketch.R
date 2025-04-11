require(eurostat)
library(ggplot2)
library(magrittr)
library(tmap)
tmap_options(check.and.fix = TRUE) 

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
deaths_socioecon_sf <- get(load('deaths_socioecon_sf.RData'))

rfasst_pop <- rfasst::pop.all.ctry_nuts3.str.SSP2 %>% 
  dplyr::select(geo = region, year, age, sex, unit, pop = value) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo == 'CYP', 'CY000', geo)) 

ap <- get(load("data/rfasst_output/tmp_m2_get_conc_pm25.ctry_nuts.output.RData")) %>%
  dplyr::filter(year == yy)

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

## load spacial data ===========================================================
nuts3_plot_data <- eurostat::get_eurostat_geospatial(resolution = 3, nuts_level = 3, year = 2021) %>%
  dplyr::select(-FID)

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
                palette = "Oranges",
                style = "cont",
                lwd = 0.2
    ) +
    tm_layout(legend.show = FALSE, frame = FALSE)
  
  tmap::tmap_save(plot_ap,
                  filename = paste0("figures/meth_sketch/plot_ap_nuts3.pdf"),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
}


# PM2.5 grid level
pm25_weighted <- terra::rast("C:/Users/claudia.rodes/Documents/GitHub/rfasst_v2/output/m2/pm25_gridded/raster_grid/2005_pm25_fin_weighted.tif")
png(filename = "figures/meth_sketch/plot_ap_grid_w.png",
    width = 100 * 3, height = 50 * 3, units = "mm", res = 300)
terra::plot(pm25_weighted, col = grDevices::terrain.colors(50), axes = FALSE, box = FALSE, legend = FALSE)
dev.off()


# PM2.5 regional level
pm25_regional <- get(load("figures/meth_sketch/pm25.map_data.RData"))

pm25_regional <- as.data.frame(rmap::mapCountries) %>%
  dplyr::mutate(subRegionAlt=as.character(subRegionAlt)) %>%
  dplyr::left_join(rfasst::fasst_reg,by="subRegionAlt") %>%
  dplyr::select(-subRegion) %>%
  dplyr::rename(subRegion=fasst_region) %>%
  dplyr::mutate(subRegionAlt=as.factor(subRegionAlt)) %>% 
  dplyr::left_join(get(load("figures/meth_sketch/pm25.map_data.RData")),
                 by = "subRegion",
                 relationship = "many-to-many")
pm25_regional <- sf::st_sf(pm25_regional, geometry = pm25_regional$geometry)

if (map) {
  plot_ap <- 
    tm_shape(pm25_regional %>% 
               dplyr::filter(year == 2005)) +
    tm_polygons("value",
                title = "PM2.5\n[ug/m3]",
                palette = "Oranges",
                style = "cont",
                lwd = 0.2
    ) +
    tm_layout(legend.show = FALSE, frame = FALSE)
  
  
  tmap::tmap_save(plot_ap,
                  filename = paste0("figures/meth_sketch/plot_ap_regional.pdf"),
                  width = 100, height = 50, units = "mm", dpi = 300
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
                lwd = 0.2
    ) +
    tm_layout(legend.show = FALSE, frame = FALSE)
  
  tmap::tmap_save(plot_deaths,
                  filename = paste0("figures/meth_sketch/plot_deaths_nuts3", normalized_tag, ".pdf"),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
}

## AP vs urbn_type  ============================================================
ap_urbntype_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, urbn_type, ap, geometry) %>% 
  unique()

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
    labs(x = "", y = "") +
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
      legend.position = "None"
    )
  
  ggsave(
    file = paste0("figures/meth_sketch/plot_urbntype_density_ap.pdf"), height = 5, width = 10, units = "cm",
    plot = plot_urbntype_density
  )
}


#### GCAM - EUR ================================================================
# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Fix invalid geometries and wrap dateline
world <- st_make_valid(world)
world <- st_wrap_dateline(world, options = c("WRAPDATELINE=YES"))

# Define EU country codes based on nuts3_plot_data
eu_countries <- unique(nuts3_plot_data$CNTR_CODE)

# Add a column to classify EU and non-EU countries
world <- world %>%
  mutate(eu_status = ifelse(iso_a2 %in% eu_countries, "EU", "Non-EU"))
world <- world %>% filter(continent != "Antarctica")
world <- world %>% filter(st_area(geometry) > units::set_units(1000, "km^2"))

plot <- tm_shape(world) +
  tm_polygons("eu_status",
              projection = "EPSG:3035",
              palette = c("blue", "lightgrey"),
              title = "Country Type",
              lwd = 0.2) +
  tm_layout(legend.show = FALSE, frame = FALSE)

tmap::tmap_save(plot,
                filename = paste0("figures/meth_sketch/plot_gcameur.pdf"),
                width = 100, height = 50, units = "mm", dpi = 300
)
