
#' map_plot
#' 
#' @param data data to plot
#' @param nuts_var column name containing the NUTS3 codes
#' @param plot_var column name with the variable to be plotted
#' @param title plot title
#' @param save saving figure parameter
#' @param save_name saving figure name
map_plot <- function(data, nuts_var, plot_var, title, save = T, save_name) {
  geodata <- eurostat::get_eurostat_geospatial(nuts_level = 3, year = 2021)
  
  map_data <- dplyr::inner_join(geodata, data, by = c("geo" = nuts_var))
  map <- tm_shape(geodata,
                  projection = "EPSG:3035",
                  xlim = c(2400000, 7800000),
                  ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(map_data) +
    tm_polygons(plot_var,
                title = title,
                palette = "Oranges"
    )
  tmap_mode("plot")
  tmap_save(map, filename = paste0("figures/",save_name), width = 10, height = 8, units = "in", dpi = 300)
  
  return(map)
}


#' map_plot
#' 
#' @param data data to plot
#' @param nuts_var column name containing the NUTS3 codes
#' @param plot_var column name with the variable to be plotted
#' @param facet_var column name with the facet variable names to be plotted
#' @param title plot title
#' @param save saving figure parameter
#' @param save_name saving figure name
map_facet_plot <- function(data, nuts_var, plot_var, facet_var, title, save = T, save_name) {
  geodata <- eurostat::get_eurostat_geospatial(nuts_level = 3, year = 2021)
  n_facets <- length(unique(data[[facet_var]]))
  map_data <- dplyr::inner_join(geodata, data, by = c("geo" = nuts_var))
  map <- tm_shape(geodata,
                  projection = "EPSG:3035",
                  xlim = c(2400000, 7800000),
                  ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(map_data) +
    tm_polygons(plot_var,
                title = title,
                palette = "Oranges"
    ) +
    tm_facets(by = facet_var, nrow = 1)
  
  tmap_mode("plot")
  tmap_save(map, filename = paste0("figures/",save_name), width = n_facets*5, height = 5, units = "in", dpi = 300)
  
  return(map)
}
