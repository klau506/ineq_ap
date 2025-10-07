###############################################################################

# Study                 : Health impacts and socioeconomic inequalities of future outdoor air pollution in an NECP-compliant Europe
# Date                  : Oct. 2025
# Author                : Clàudia Rodés-Bachs
# Institute             : BC3-Basque Centre for Climate Change
# Description           : Helper script to load all necessary data
# Re-usage instructions : Execute main.R, which will automatically run this script

###############################################################################


#' ml_clustering
#' 
#' @description ML clustering and dendograms creation algorithm
#' @param data dataset
#' @param cluster_number desired number of clusters
#' @param fig_name name of the figure
#' @param type 'ap' or 'deaths'
ml_clustering <- function(data, cluster_number, fig_name, type) {
  # Convert each country's values to ranks
  ranked_data <- data %>%
    dplyr::select(-country) %>%
    apply(1, rank) %>%
    t() %>%
    as.data.frame()
  
  rownames(ranked_data) <- data$country  # Assign row names
  
  # Compute Spearman correlation matrix
  cor_matrix <- cor(t(ranked_data), method = "spearman")
  
  # Convert to distance matrix (1 - correlation)
  dist_matrix <- as.dist(1 - cor_matrix)
  
  # Perform hierarchical clustering
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Plot dendrogram
  png(file.path("figures", paste0(fig_name, "_", type, ".png")), width = 800, height = 600)
  plot(hc, main = "Hierarchical Clustering of Countries by Category Rankings")
  dev.off()
  
  
  # Cut tree into clusters
  clusters <- cutree(hc, k = cluster_number)
  
  # Add cluster labels to the original data
  data$cluster <- clusters

  return(data)
}


#' ml_do_all
#' 
#' @description function to run all the ML analysis
#' @param data dataset
#' @param cluster_number desired number of clusters
#' @param fig_name name of the figure
#' @param fig_legend figure's legend
#' @param fig_ox_label figure's ox label
#' @param type 'ap' or 'deaths'
ml_do_all <- function(data, cluster_number, fig_name, 
                      fig_legend = NULL, fig_ox_label = NULL,
                      type = '') {
  ml_data <- ml_clustering(data, cluster_number, fig_name, type)  
  
  to_analyze = ml_data %>% 
    dplyr::select(country, cluster) %>% 
    dplyr::distinct()
  
  data_ml <- ml_data %>% 
    tidyr::pivot_longer(cols = 2:6, names_to = 'decile', values_to = 'value') %>% 
    dplyr::mutate(cluster = as.factor(cluster)) %>% 
    dplyr::arrange(cluster) %>% 
    dplyr::mutate(country = factor(country, levels = unique(country)))
  
  
  pl <- ggplot(data_ml %>%
                 dplyr::mutate(decile = substr(decile, 2, 2)),
               aes(y = factor(country), x = value, color = factor(decile))) +
    geom_point(size = 5, alpha = 0.7) +
    scale_color_manual(
      values = quintiles.color,
      name = fig_legend,
      labels = quintiles.labs
    ) +
    labs(x = fig_ox_label, y = "", fill = "Decile") +
    theme(axis.text = element_text(size = 12))
  ggsave(
    file = file.path("figures",paste0(fig_name,"_",type,"_ordered.pdf")), 
    height = 30, width = 20, units = "cm",
    plot = pl
  )
  
  
  
  data_ml_sf <- to_analyze %>% 
    dplyr::left_join(
      nuts3_plot_data %>%
        dplyr::select(country = CNTR_CODE, geometry),
      by = "country"
    )
  data_ml_sf <- sf::st_sf(data_ml_sf, geometry = data_ml_sf$geometry)
  
  pl2 <- tm_shape(nuts3_plot_data,
                  projection = "EPSG:3035",
                  xlim = c(2400000, 6500000),
                  ylim = c(1320000, 5650000)
  ) +
    tm_fill("lightgrey") +
    tm_shape(data_ml_sf %>%
               dplyr::select(cluster, geometry) %>% 
               dplyr::distinct() %>% 
               dplyr::filter(!sf::st_is_empty(geometry))) +
    tm_polygons("cluster",
                title = "Region\ncluster",
                palette = rev(long.discrete.color), 
                style = "cat",
                lwd = 0.01) +
    tm_layout(legend.title.size = 0.8,
              legend.text.size = 0.6,
              frame = FALSE)
  tmap::tmap_save(pl2,
                  filename = file.path("figures",paste0(fig_name,"_",type,"_clusters_map.pdf")),
                  width = 100, height = 100, units = "mm", dpi = 300
  )
  
  
}
