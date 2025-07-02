

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
                alpha = 0.5,
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
  
  return(pl)
}



urbn_type.color = c('City'='#C288B0',
                    'Town/Suburb'='#C3D2D5',
                    'Rural'='#92DEC3')
urbn_type.labs = c('City','Town/Suburb','Rural')
names(urbn_type.labs) <- c('City','Town/Suburb','Rural')

urbn_type.color.num = c('3'='#C288B0',
                        '2'='#C3D2D5',   
                        '1'='#92DEC3')
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
