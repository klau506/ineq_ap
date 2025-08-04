load_grid_data_urb <- function() {
  urbn_raster2 <- terra::resample(urbn_raster, pm.ap_raster2)
  urbn_raster2 <- terra::crop(urbn_raster2, extent_raster)
  
  # Define classification function
  classify_function <- function(x) {
    ifelse(x <= 10, 0, 
           ifelse(x >= 11 & x <= 20, 1, # rural
                  ifelse(x >= 21 & x <= 22, 2, # town/suburb
                         ifelse(x >= 23 & x <= 30, 3, NA)))) # urban
  }
  
  # Apply classification
  urbn_raster_classified <- terra::app(urbn_raster2, classify_function)
  names(urbn_raster_classified) <- "classification_layer"
  urbn_raster_combined <- c(urbn_raster2, urbn_raster_classified)
  terra::plot(urbn_raster_combined$classification_layer)
  
  # Filter out NA values directly on the rasters
  pm.ap_raster_filtered <- terra::mask(pm.ap_raster2, pm.ap_raster2, maskvalue = NA)
  urbn_raster_filtered <- urbn_raster_combined$classification_layer
  urbn_raster_combined_filtered <- terra::mask(urbn_raster_filtered, urbn_raster_filtered, maskvalue = NA)
  
  # Convert the filtered rasters to data frames
  pm_values <- terra::values(pm.ap_raster_filtered)
  urbn_values <- terra::values(urbn_raster_combined_filtered)
  
  
  rr <- list(pm_values, urbn_values)
  
  return(rr)
}


load_grid_data_inc <- function() {
  inc_pc_20152 <- terra::resample(inc_pc_2015, pm.ap_raster2)
  inc_pc_20152 <- terra::crop(inc_pc_20152, extent_raster)
  
  # Filter out NA values directly on the rasters
  pm.ap_raster_filtered <- terra::mask(pm.ap_raster2, pm.ap_raster2, maskvalue = NA)
  inc_raster_filtered <- terra::mask(inc_pc_20152, inc_pc_20152, maskvalue = NA)
  
  # Convert the filtered rasters to data frames
  pm_values <- terra::values(pm.ap_raster_filtered)
  inc_values <- terra::values(inc_raster_filtered)
  
  rr <- list(pm_values, inc_values)
  
  return(rr)
}


load_grid_data_eld <- function() {
  pm.ap_raster2 <- terra::crop(pm.ap_raster, extent_raster)
  
  pop_ge652 <- terra::project(pop_ge65, pm.ap_raster2)
  pop_t2 <- terra::project(pop_t, pm.ap_raster2)
  pop_ge652 <- terra::crop(pop_ge652, extent_raster)
  pop_t2 <- terra::crop(pop_t2, extent_raster)
  pop_elderly <- pop_ge652/pop_t2
  
  # Filter out NA values directly on the rasters
  pm.ap_raster_filtered <- terra::mask(pm.ap_raster2, pm.ap_raster2, maskvalue = NA)
  elderly_raster_filtered <- terra::mask(pop_elderly, pop_elderly, maskvalue = NA)
  
  # Convert the filtered rasters to data frames
  pm_values <- terra::values(pm.ap_raster_filtered)
  pop_elderly <- terra::values(elderly_raster_filtered)
  
  rr <- list(pm_values, pop_elderly)
  
  return(rr)
}



prob_jitter_plot <- function(data, legend_position = c(0.87,0.87), legend_title = '', 
                             legend_type = NULL, ox_text = '') {
  # Find the maximum y value
  pl <- ggplot(data) +
    geom_density(mapping = aes(x = item, fill = quintile, color = quintile),
                 inherit.aes = FALSE,
                 alpha = 0.1,
                 linewidth = 0.8)
  plot_data <- ggplot_build(pl)$data[[1]]
  max_density <- max(plot_data$y)
  
  # Do the plot
  spacing_factor = max_density*77
  scl = max_density/17
  
  
  legend_color = paste0(legend_type, '.color')
  legend_labs = paste0(legend_type, '.labs')
  
  pl <- ggplot(data) +
    geom_density(mapping = aes(x = item,
                               fill = quintile,
                               color = quintile),
                 inherit.aes = FALSE,
                 alpha = 0.1,
                 linewidth = 0.8) +
    geom_jitter(mapping = aes(x = item,
                              y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                              color = quintile),
                size = 0.5,
                alpha = 0.3,
                height = max_density/14) +
    geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 + scl,
                     yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 - scl,
                     x = c05, xend = c05, color = quintile),
                 linewidth = 0.8) +
    geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 + scl,
                     yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 - scl,
                     x = c95, xend = c95, color = quintile),
                 linewidth = 0.8) +
    geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                     yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                     x = c05, xend = c33, color = quintile),
                 linewidth = 0.8) +
    geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                     yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                     x = c66, xend = c95, color = quintile),
                 linewidth = 0.8) +
    geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 + scl,
                  ymax = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 - scl,
                  xmin = c33, xmax = c66,
                  color = quintile),
              fill = "white", alpha = 0.01, linewidth = 0.8) +
    geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 + scl,
                     yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025 - scl,
                     x = c50, xend = c50,
                     color = quintile),
                 linewidth = 0.8) +
    scale_color_manual(
      values = get(legend_color),
      name = legend_title,
      labels = get(legend_labs)
    ) +
    scale_fill_manual(
      values = get(legend_color),
      name = legend_title,
      labels = get(legend_labs)
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
    theme_minimal() +
    labs(x = ox_text,
         y = "",
         color = legend_title) +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.ontop = FALSE,
      strip.text = element_text(size = legend.text.size),
      strip.background = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = legend.text.size),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.key.size = unit(0.6, "cm"),
      legend.title = element_text(size = legend.text.size),
      legend.text = element_text(size = legend.text.size),
      legend.position = legend_position
    )
  
  if (legend_type == 'urbn_type') {
    pl2 <- pl +
      geom_text(mapping = aes(x = max(c95) + (max(item) - max(c95))*3/4, 
                              y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                              color = quintile, label = quintile))
  } else {
    pl2 <- pl +
      geom_text(mapping = aes(x = max(c95) + (max(item) - max(c95))*3/4, 
                              y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.025,
                              color = quintile, label = paste0('Q',quintile)))
  }
  
  pl1 <- pl
  
  pl <- list(pl1, pl2)
  
  return(pl)
}


#' do_plot_within
#' 
#' @param data dataset
#' @param ox_label label for the OX axis
#' @param type either 'grid' or 'nuts3'
do_plot_within <- function(data, ox_label, type) {
  
  # nuts3
  if (type != 'grid') {
    pl <- ggplot() +
      # income
      geom_point(
        data = data %>%
          dplyr::select(quintile, country_name, variable, value) %>%
          dplyr::filter(variable == 'income') %>%
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, color = quintile), size = 2, alpha = 0.85) +
      scale_color_manual(
        values = quintiles_v.color,
        name = "Income quintile",
        labels = quintiles_v.labs
      ) +
      new_scale_colour() +
      # elderly
      geom_point(
        data = data %>%
          dplyr::select(quintile, country_name, variable, value) %>%
          dplyr::filter(variable == 'elderly') %>%
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, color = quintile), size = 2, alpha = 0.85) +
      scale_color_manual(
        values = quintiles_v2.color,
        name = "Elderly proportion\nquintile",
        labels = quintiles_v2.labs
      ) +
      # gini
      geom_point(
        data = data %>%
          dplyr::select(quintile, country_name, variable, value) %>%
          dplyr::filter(variable == 'gini') %>%
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, fill = quintile), size = 2, shape = 21) +
      scale_fill_manual(
        values = quintiles_v3.color,
        name = "Gini index\nquintile",
        labels = quintiles_v3.labs
      ) +
      new_scale_fill() +
      # settlement
      geom_point(
        data = data %>% 
          dplyr::select(urbn_type, country_name, variable, value) %>% 
          dplyr::filter(variable == 'urbn') %>% 
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, fill = urbn_type), size = 2, shape = 21) +
      scale_fill_manual(
        values = urbn_type.color,
        name = "Settlement type",
        labels = urbn_type.labs
      ) +
      # rest of the plot
      facet_grid(. ~ variable,
                 labeller = labeller(variable = c(income = "Income\nper capita",
                                                  elderly = "Elderly\nproportion",
                                                  gini = "Gini index\n",
                                                  urbn = "Settlement\ntype")),
                 scales = 'fixed')
  } else {
    pl <- ggplot() +
      # income
      geom_point(
        data = data %>%
          dplyr::select(quintile, country_name, variable, value) %>%
          dplyr::filter(variable == 'income') %>%
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, color = quintile), size = 2, alpha = 0.85) +
      scale_color_manual(
        values = quintiles_v.color,
        name = "Income quintile",
        labels = quintiles_v.labs
      ) +
      new_scale_colour() +
      # elderly
      geom_point(
        data = data %>%
          dplyr::select(quintile, country_name, variable, value) %>%
          dplyr::filter(variable == 'elderly') %>%
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, color = quintile), size = 2, alpha = 0.85) +
      scale_color_manual(
        values = quintiles_v2.color,
        name = "Elderly proportion\nquintile",
        labels = quintiles_v2.labs
      ) +
      # settlement
      geom_point(
        data = data %>% 
          dplyr::select(urbn_type, country_name, variable, value) %>% 
          dplyr::filter(variable == 'urbn') %>% 
          dplyr::distinct(),
        aes(y = factor(country_name), x = value, fill = urbn_type), size = 2, shape = 21) +
      scale_fill_manual(
        values = urbn_type.color,
        name = "Settlement type",
        labels = urbn_type.labs
      ) +
      # rest of the plot
      facet_grid(. ~ variable,
                 labeller = labeller(variable = c(income = "Income\nper capita",
                                                  elderly = "Elderly\nproportion",
                                                  urbn = "Settlement\ntype")),
                 scales = 'fixed') +
      # beautiful ox scale
      scale_x_continuous(labels = sci_formatter2)
  }
  
  pl <- pl +
    labs(x = ox_label, y = "") +
    guides(
      color = guide_legend(override.aes = list(alpha = 1), order = 0),
      fill = guide_legend(order = 3)
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.ontop = FALSE,
      strip.text = element_text(size = legend.text.size),
      strip.background = element_blank(),
      axis.title = element_text(size = legend.title.size),
      axis.title.y = element_blank(),
      axis.text = element_text(size = legend.text.size),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.key.size = unit(0.6, "cm"),
      legend.title = element_text(size = legend.text.size),
      legend.text = element_text(size = legend.text.size),
      legend.position = 'right'
    )
  
  return(pl)
}

# scientific formatter: 0 appears as "0" and we remove the extra 0s (e.g., 05+e04 is 5e+4)
sci_formatter <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) {
      NA
    } else if (val == 0) {
      "0"
    } else {
      # format to scientific, then clean up exponent
      formatted <- format(val, scientific = TRUE, digits = 1)
      formatted <- gsub("e([-+]?)(0+)([0-9]+)", "e\\1\\3", formatted)  # removes e+0, e-0, etc.
      formatted
    }
  })
}


# scientific formatter: 0 appears as "0" and we remove the extra 0s (e.g., 05+e04 is 5e+4)
sci_formatter2 <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) {
      NA
    } else if (val == 0) {
      "0"
    } else {
      # # format to scientific, then clean up exponent
      # formatted <- format(val, scientific = TRUE, digits = 1)
      # formatted <- gsub("e([-+]?)(0+)([0-9]+)", "e\\1\\3", formatted)  # removes e+0, e-0, etc.
      # formatted
      val
    }
  })
}


count_cells_plot <- function(data, axis_title = '') {
  legend_title = 'Urban type'
  legend_type = 'urbn_type'
  legend_color = paste0(legend_type, '.color')
  legend_labs = paste0(legend_type, '.labs')
  
  pl <- ggplot(data,
               mapping = aes(x = n, y = y_offset, shape = agg_level,
                             fill = urbn_type, color = urbn_type)) +
    geom_hline(data = dplyr::distinct(data, y_offset),
               aes(yintercept = y_offset),
               color = "grey90") +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(
      values = get(legend_color),
      name = legend_title,
      labels = get(legend_labs)
    ) +
    scale_fill_manual(
      values = get(legend_color),
      name = legend_title,
      labels = get(legend_labs)
    ) +
    scale_shape_manual(
      values = agg.level_shape,
      name = 'Aggregation level',
      labels = agg.level_labs
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA, size = 2)),
           shape = guide_legend(override.aes = list(alpha = 1, size = 2)))  +
    scale_y_continuous(
      breaks = 1:5,
      labels = paste0("Q", 1:5),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                       labels = function(x) paste0(x, "%")) +
    theme_minimal() +
    labs(x = "Share of spatial units [%]",
         y = axis_title,
         color = legend_title) +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid.major.y = element_line(color = "white"),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor.x = element_blank(),
      panel.ontop = FALSE,
      axis.text = element_text(size = legend.text.size),
      axis.title = element_text(size = legend.title.size),
      legend.key.size = unit(0.6, "cm"),
      legend.title = element_text(size = legend.title.size),
      legend.text = element_text(size = legend.text.size),
      legend.position = 'bottom',
      legend.direction = 'horizontal'
    )
  
  return(pl)
}


do_map_between_socioecon <- function(raster_filtered,
                                    legend_color,
                                    legend_labs,
                                    legend_title,
                                    plot_title) {
  
  pdf(paste0("figures/",plot_title), width = 11/2.54, height = 10/2.54)
  par(mar = c(0,0,0,0), xpd = NA)
  r <- raster::raster(raster_filtered)
  terra::plot(r, 
              col = legend_color, 
              breaks = 1:5,
              legend = FALSE, 
              axes = FALSE, 
              box = FALSE)
  legend('bottom',
         legend = legend_labs,
         pch = 21,
         pt.bg = legend_color,
         pt.cex = 1,
         bty = "n",
         title = legend_title,
         text.font = 1,
         cex = legend.title.size.raster-0.2,
         x.intersp = 1, y.intersp = 1.2,
         horiz = TRUE)
  dev.off()
  
}


do_map_between_socioecon_nuts3 <- function(data_sf,
                                    legend_color,
                                    legend_labs,
                                    legend_title,
                                    plot_title) {
  
  # limit the extension of the sf object  
  data_proj <- sf::st_transform(nuts3_plot_data, crs = 3035)
  bbox <- sf::st_bbox(c(xmin = 2400000, xmax = 6500000,
                        ymin = 1320000, ymax = 5650000),
                  crs = sf::st_crs(3035)) %>%
    sf::st_as_sfc()
  data_filtered <- data_proj %>%
    sf::st_filter(bbox)
  
  
  bbox_new <- sf::st_bbox(data_filtered) # current bounding box
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
  bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top
  
  bbox_new <- bbox_new %>%  # take the bounding box ...
    sf::st_as_sfc() # ... and make it a sf polygon

  plot <- tm_shape(nuts3_plot_data,
                       projection = "EPSG:3035",
                       bbox = bbox_new
  ) +
    tm_fill("lightgrey") +
    tm_shape(data_sf,
             bbox = bbox_new) +
    tm_polygons("quintile",
                showNA = F,
                title = legend_title,
                palette = legend_color,
                labels = legend_labs,
                lwd = 0.1
    ) +
    tm_layout(
      legend.position = c("right", "top"),
      legend.bg.color = NA,
      legend.bg.alpha = 0,
      legend.title.size = legend.title.size.raster - 0.3,
      legend.text.size = legend.title.size.raster - 0.4,
      legend.just = "center",
      legend.width = 0.5,
      legend.height = 0.3,
      frame = F
    )
  
  tmap::tmap_save(tm = plot,
                  paste0("figures/",plot_title),
                  width = 4.3, height = 3.93, units = "cm", dpi = 300
                  # width = 11/2.54, height = 10/2.54
  )
  
}

do_map_within_socioecon <- function(raster_filtered,
                                    legend_color,
                                    legend_labs,
                                    legend_title,
                                    plot_title) {
  
  # crop to income raster extent to speed up processing
  countries_inc <- terra::crop(countries_vect, raster_filtered)
  countries_inc <- terra::project(countries_inc, raster_filtered)
  countries_iso <- countries_inc[, c("iso_a3")]
  countries_iso <- countries_iso[!countries_iso$iso_a3 %in% c("ALA","DZA","GEO",
                                                              "GRL","IOR","IRQ",
                                                              "ISR","JOR","LBN",
                                                              "LBY","MAR","RUS",
                                                              "SYR","TUN","TUR"), ]
  
  # extract raster values by country
  vals_by_country <- terra::extract(raster_filtered, countries_iso,
                                    cells = TRUE, 
                                    xy = TRUE
  )
  vals_by_country$iso_a3 <- countries_iso$iso_a3[vals_by_country$ID]
  
  # compute quintiles per country
  vals_quintiles <- vals_by_country %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(iso_a3) %>%
    dplyr::mutate(quintile = dplyr::ntile(value, 5)) %>%
    dplyr::ungroup()
  
  # convert back to spatial points and rasterize 
  spat_points <- terra::vect(vals_quintiles, geom = c("x", "y"), crs = terra::crs(raster_filtered))
  quintile_raster <- terra::rasterize(spat_points, raster_filtered, field = "quintile")
  
  centroids <- terra::centroids(countries_iso)
  
  pdf(paste0("figures/",plot_title), width = 11/2.54, height = 10/2.54)
  par(mar = c(0,0,0,0), xpd = NA)
  r <- raster::raster(quintile_raster)
  terra::plot(r, 
              col = legend_color, 
              breaks = 1:5,
              legend = FALSE, 
              axes = FALSE, 
              box = FALSE)
  terra::plot(countries_iso, 
              add = TRUE, 
              border = "black", 
              lwd = 0.1)
  # text(centroids, labels = countries_iso$iso_a3, cex = 0.6, font = 2)
  legend('bottom',
         legend = legend_labs,
         pch = 21,
         pt.bg = legend_color,
         pt.cex = 1,
         bty = "n",
         title = legend_title,
         text.font = 1,
         cex = legend.title.size.raster-0.2,
         x.intersp = 1, y.intersp = 1.2,
         horiz = TRUE)
  dev.off()
  
}


agg.level_shape = c(16,15)
agg.level_labs = c('n_grid' = '1km\u00B2 grid cell',
                   'n_nuts' = 'NUTS3')

urbn_type.color = c('City'='#9253a6',
                    'Town/Suburb'='#819499',
                    'Rural'='#49a383')
urbn_type.labs = c('City','Town/Suburb','Rural')
names(urbn_type.labs) <- c('City','Town/Suburb','Rural')

urbn_type.color.num = c('3'='#9253a6',
                        '2'='#819499',   
                        '1'='#49a383')
urbn_type.labs.num = c('City','Town/Suburb','Rural')
names(urbn_type.labs.num) <- c('3','2','1')

long.discrete.color = c(
  "#001219",
  "#005f73",
  "#0a9396",
  "#94d2bd",
  "#e9d8a6",
  "#ee9b00",
  "#ca6702",
  "#9b2226"
  )
# quintiles.color = c('1'='#e3d26f',
#                     '2'='#ca895f',
#                     '3'='#a15e49',
#                     '4'='#4e3822',
#                     '5'='#2f1b25')
quintiles_v3.color = c('1' = '#008080',
                       '2' = '#66b2b2',
                       '3' = '#f4d58d',
                       '4' = '#e07a5f',
                       '5' = '#6a0572')
quintiles_v3.labs = c('Q1 - more egalitarian','Q2','Q3','Q4','Q5 - more unequal')
names(quintiles_v3.labs) <- c('1','2','3','4','5')

quintiles_v2.color = c('1' = '#3b4994',
                       '2' = '#7c91c4',
                       '3' = '#b2d5ba',
                       '4' = '#f2b5d4',
                       '5' = '#93003a')
quintiles_v2.labs = c('Q1 - less elderly','Q2','Q3','Q4','Q5 - more elderly')
names(quintiles_v2.labs) <- c('1','2','3','4','5')

quintiles_v.color = c('1'='#335c67',
                      '2'='#a7c957',
                      '3'='#e09f3e',
                      '4'='#9e2a2b',
                      '5'='#540b0e')
quintiles_v.labs = c('Q1 - poorest','Q2','Q3','Q4','Q5 - wealthiest')
names(quintiles_v.labs) <- c('1','2','3','4','5')

quintiles.color = c('1'='#335c67',
                    '2'='#a7c957',
                    '3'='#e09f3e',
                    '4'='#9e2a2b',
                    '5'='#540b0e')
quintiles.labs = c('Q1','Q2','Q3','Q4','Q5')
names(quintiles.labs) <- c('1','2','3','4','5')
quintiles.labs.gini.nuts3 = c('Q1 [0.204 - 0.285]',
                        'Q2 [0.285 - 0.299]',
                        'Q3 [0.299 - 0.312]',
                        'Q4 [0.312 - 0.348]',
                        'Q5 [0.348 - 0.553]')
quintiles.labs.per_elderly.nuts3 = c('Q1 [0.0276 - 0.175]',
                                     'Q2 [0.175 - 0.202]',
                                     'Q3 [0.202 - 0.222]',
                                     'Q4 [0.222 - 0.244]',
                                     'Q5 [0.244 - 0.354]')
quintiles.labs.income.nuts3 = c('Q1 [2901 - 13015]',
                                'Q2 [13015 - 16210]',
                                'Q3 [16210 - 18125]',
                                'Q4 [18125 - 20585]',
                                'Q5 [20585 - 46866]')
names(quintiles.labs.gini.nuts3) <- c('1','2','3','4','5')

quintiles.labs.per_elderly.grid = c('Q1 [0.000 - 0.144]',
                                    'Q2 [0.144 - 0.193]',
                                    'Q3 [0.193 - 0.237]',
                                    'Q4 [0.237 - 0.306]',
                                    'Q5 [0.306 - 1]')
quintiles.labs.income.grid = c('Q1 [2039 - 9051]',
                               'Q2 [9051 - 11970]',
                               'Q3 [11970 - 15090]',
                               'Q4 [15090 - 17023]',
                               'Q5 [17023 - 191019]')
