

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
