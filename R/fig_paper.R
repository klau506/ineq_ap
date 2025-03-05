library(ggpattern)
crop_xmin <- -22
crop_xmax <- 32
crop_ymin <- 36
crop_ymax <- 71
legend.text.size <- 12
scl <- 20

#### RESULTS NDC CONSEQ ========================================================
## AP  -------------------------------------------------------------------------
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

plot_ap_gg <- ggplot() +
  geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
    geom_sf(data = ap_nuts3_sf, 
          aes(fill = ap), 
          # color = "black",
          color = NA,
          size = 0.0001) + 
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       name = "PM2.5\n[ug/m3]") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 8),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_ap_gg,
       filename = paste0("figures/plot_ap.pdf"),
       width = 11, height = 10, units = "cm")



## DEATHS ----------------------------------------------------------------------
deaths_nuts3 <- deaths %>%
  dplyr::filter(
    nchar(region) > 3,
    sex == 'Both'
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

plot_deaths_gg <- ggplot() +
  geom_sf(data = nuts3_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = deaths_nuts3_sf %>% dplyr::filter(sex == "Both"), 
          aes(fill = deaths), 
          color = NA, 
          # color = "black", 
          size = 0.05) + 
  scale_fill_distiller(palette = "Oranges", direction = 1, 
                       name = "Premature Deaths\n(Normalized in Millions)") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 8),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_deaths_gg,
       filename = paste0("figures/plot_deaths", normalized_tag, ".pdf"),
       width = 14, height = 10, units = "cm")



## AP CTRY -------------------------------------------------------------------------
ap_ctry <- ap %>%
  dplyr::filter(
    nchar(region) == 3
  ) %>%
  dplyr::rename(
    ISO3 = region,
    ap = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>%
                     dplyr::select(geo = ISO2, ISO3) %>% 
                     dplyr::mutate(ISO3 = ifelse(ISO3 == 'ROM', 'ROU', ISO3)) %>% 
                     dplyr::distinct(),
                   by = "ISO3"
  ) %>% 
  dplyr::left_join(ctry_plot_data,
                   by = "geo"
  ) %>% 
  dplyr::filter(LEVL_CODE == 0)

ap_ctry_sf <- sf::st_sf(ap_ctry, geometry = ap_ctry$geometry)

plot_ap_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
    geom_sf(data = ap_ctry_sf, 
          aes(fill = ap), 
          color = "black",
          # color = NA,
          size = 0.0001) + 
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       name = "PM2.5\n[ug/m3]") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 8),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_ap_gg,
       filename = paste0("figures/plot_ctry_ap.pdf"),
       width = 11, height = 10, units = "cm")



## AP CTRY LIMIT WHO -------------------------------------------------------------------------
ap_ctry <- ap %>%
  dplyr::filter(
    nchar(region) == 3
  ) %>%
  dplyr::rename(
    ISO3 = region,
    ap = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>%
                     dplyr::select(geo = ISO2, ISO3) %>% 
                     dplyr::mutate(ISO3 = ifelse(ISO3 == 'ROM', 'ROU', ISO3)) %>% 
                     dplyr::distinct(),
                   by = "ISO3"
  ) %>% 
  dplyr::left_join(ctry_plot_data,
                   by = "geo"
  ) %>% 
  dplyr::filter(LEVL_CODE == 0) %>% 
  dplyr::mutate(overWHO = ifelse(ap > 5, TRUE, FALSE))
  

ap_ctry_sf <- sf::st_sf(ap_ctry, geometry = ap_ctry$geometry)

plot_ap_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = ap_ctry_sf,
          aes(fill = ap),
          color = "black",
          size = 0.0001) +
  geom_sf_pattern(data = ap_ctry_sf %>% dplyr::filter(overWHO) %>% 
                    dplyr::distinct(),
                  aes(fill = ap),
                  pattern = "stripe", 
                  pattern_density = 0.4,
                  pattern_spacing = 0.02,
                  pattern_angle = 45,
                  pattern_linetype = "solid",
                  pattern_fill = "transparent",
                  pattern_color = "red",
                  pattern_size = 0.1,
                  pattern_alpha = 0.8,
                  color = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       name = "PM2.5\n[ug/m3]") +
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 8),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(plot = plot_ap_gg,
       filename = paste0("figures/plot_ctry_limit_ap.png"),
       width = 11, height = 10, units = "cm")



## DEATHS CTRY ----------------------------------------------------------------------
deaths_ctryy <- deaths_ctry %>%
  dplyr::filter(
    nchar(region) == 3,
    sex == 'Both'
  ) %>%
  dplyr::rename(
    ISO3 = region,
    deaths = value
  ) %>%
  dplyr::left_join(rfasst::ctry_nuts3_codes %>% 
                     dplyr::filter(ISO2 != 'TR') %>% 
                     dplyr::select(geo = ISO2, ISO3) %>% 
                     dplyr::distinct(),
                   by = "ISO3"
  ) %>% 
  dplyr::group_by(year, sex, scenario, geo, ISO3) %>% 
  dplyr::summarise(deaths = sum(deaths, na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(ctry_plot_data,
                   by = "geo"
  ) %>% 
  dplyr::filter(LEVL_CODE == 0)

deaths_ctry_sf <- sf::st_sf(deaths_ctryy, geometry = deaths_ctryy$geometry)

plot_deaths_gg <- ggplot() +
  geom_sf(data = ctry_plot_data, fill = "lightgrey", color = NA) +
  geom_sf(data = deaths_ctry_sf %>% dplyr::filter(sex == "Both"), 
          aes(fill = deaths), 
          # color = NA, 
          color = "black",
          size = 0.05) + 
  scale_fill_distiller(palette = "Oranges", direction = 1, 
                       name = "Premature Deaths\n(Normalized in Millions)") +
  
  coord_sf(xlim = c(crop_xmin, crop_xmax), ylim = c(crop_ymin, crop_ymax)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "top", 
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 8),
    panel.border = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank()
  )


ggsave(plot_deaths_gg,
       filename = paste0("figures/plot_ctry_deaths", normalized_tag, ".pdf"),
       width = 14, height = 10, units = "cm")



#### RESULTS NUTS3 URBAN =====================================================================
## AP vs urbn_type -------------------------------------------------------------
ap_urbntype_sf <- ap_socioecon_sf %>%
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::select(geo, urbn_type, ap, geometry) %>% 
  unique()

df <- data.table::as.data.table(ap_urbntype_sf) %>%
  dplyr::select(-geometry)
df_medi <- df[, .(medi = quantile(ap, 0.5, na.rm = T)),
              by = c("urbn_type")
]

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
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
    legend.position = "None"
  )

ggsave(
  file = paste0("figures/plot_urbntype_density_ap.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_urbntype_density
)


## AP vs DEATHS SCATTERPLOT  ---------------------------------------------------
deaths_geo_urbn <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, deaths, income)
deaths_geo_urbn <- unique(deaths_geo_urbn)

ap_geo_urbn <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, ap, income)
ap_geo_urbn <- unique(ap_geo_urbn)

ap_deaths_nuts3 <- merge(ap_geo_urbn, deaths_geo_urbn) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  dplyr::filter(deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::left_join(nuts3_plot_data %>% 
                     dplyr::select(CNTR_CODE, geo),
                   by = "geo"
  )

# calculate mean values for each quintile
quintile_means <- ap_deaths_nuts3 %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(mean_ap = mean(ap, na.rm = TRUE), 
            mean_deaths = mean(deaths, na.rm = TRUE))

# define an offset for the reference points
offset_x <- min(ap_deaths_nuts3$deaths, na.rm = TRUE) - 0.1 * diff(range(ap_deaths_nuts3$deaths))
offset_y <- min(ap_deaths_nuts3$ap, na.rm = TRUE) - 0.1 * diff(range(ap_deaths_nuts3$ap))

# define the start and end point of the segments
a <- min(quintile_means$mean_deaths, na.rm = TRUE) -4 * scl  # start of vertical line
b <- max(quintile_means$mean_deaths, na.rm = TRUE) +4 * scl  # end of vertical line
c <- min(quintile_means$mean_ap, na.rm = TRUE) -0.10 * scl     # start of horizontal line
d <- max(quintile_means$mean_ap, na.rm = TRUE) +0.10 * scl     # end of horizontal line


plot_deaths_ap <- ggplot() +
  geom_segment(data = quintile_means[1,], 
               aes(x = a, xend = b, y = offset_y, yend = offset_y), 
               linetype = "dashed", size = 0.8, alpha = 0.75) +
  geom_segment(data = quintile_means[1,], 
               aes(x = offset_x, xend = offset_x, y = c, yend = d), 
               linetype = "dashed", size = 0.8, alpha = 0.75) +
  geom_point(data = ap_deaths_nuts3, 
             aes(x = deaths, y = ap, color = quintile, shape = urbn_type),
             alpha = 0.75, size = 2) +
  geom_point(data = quintile_means, aes(x = offset_x, y = mean_ap, color = quintile), 
             size = 3, shape = 18) +
  geom_point(data = quintile_means, aes(x = mean_deaths, y = offset_y, color = quintile), 
             size = 3, shape = 18) + 
  scale_color_manual(
    values = quintiles.color,
    name = "Income quintiles",
    labels = quintiles.labs
  ) +
  theme_minimal() +
  labs(x = "Premature deaths [normalized million]", 
       y = "PM2.5 concentration [µg/m³]",
       color = "Income quintiles", 
       shape = "Settlement Type") +
  expand_limits(x = offset_x, y = offset_y) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.border = element_blank(),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.text = element_text(size = legend.text.size),
    legend.key.size = unit(1.5, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size)
  )



ggsave(
  file = paste0("figures/plot_deaths_ap", normalized_tag, ".pdf"), height = 13, width = 18, units = "cm",
  plot = plot_deaths_ap
)








## AP vs GINI -------------------------------------------------------------
ap_geo_gini <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, gini, ap)
ap_geo_gini <- unique(ap_geo_gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup()

plot_ap_gini <- ggplot(ap_geo_gini) +
  geom_density(mapping = aes(x = ap,
                             fill = quintile,
                             color = quintile),
               inherit.aes = FALSE,
               alpha = 0.1,
               size = 0.8) +
  geom_jitter(mapping = aes(x = ap,
                            y = -as.numeric(quintile) * spacing_factor * 1e-1,
                            color = quintile),
              size = 0.5,
              alpha = 0.5,
              height = 0.02) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                   x = c05, xend = c05, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                   x = c95, xend = c95, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1,
                   x = c05, xend = c33, color = quintile),
               size = 0.8) +
    geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1,
                     yend = -as.numeric(quintile) * spacing_factor * 1e-1,
                     x = c66, xend = c95, color = quintile),
               size = 0.8) +
  geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                ymax = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                xmin = c33, xmax = c66,
                color = quintile),
            fill = "white", alpha = 0.01, size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                   x = c50, xend = c50,
                   color = quintile),
               size = 0.8) +
  scale_color_manual(
    values = quintiles.color,
    name = "Gini quintiles",
    labels = quintiles.labs.gini
  ) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Gini quintiles",
    labels = quintiles.labs.gini
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
  theme_minimal() +
  labs(x = "PM2.5 concentration [ug/m3]", 
       y = "",
       color = "Gini quintiles") +
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
    legend.position = c(0.87,0.87)
  )

plot_ap_gini

ggsave(
  file = paste0("figures/plot_ap_gini.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_ap_gini
)

ap_geo_gini %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_gini = min(gini, na.rm = TRUE),
    max_gini = max(gini, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    median_gini = median(gini, na.rm = TRUE),
    sd_gini = sd(gini, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_gini max_gini mean_gini median_gini sd_gini     n
# <fct>       <dbl>    <dbl>     <dbl>       <dbl>   <dbl> <int>
# 1 1           0.204    0.285     0.269       0.271 0.0137    212
# 2 2           0.285    0.298     0.291       0.29  0.00357   212
# 3 3           0.298    0.312     0.305       0.306 0.00395   212
# 4 4           0.312    0.348     0.327       0.326 0.0113    212
# 5 5           0.348    0.553     0.387       0.380 0.0403    211



## AP vs ELDERLY  ----------------------------------------------------------
ap_geo_per_elderly <- data.table::as.data.table(ap_socioecon_sf) %>%
  dplyr::select(geo, per_elderly, ap)
ap_geo_per_elderly <- unique(ap_geo_per_elderly) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, ap != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(ap, 0.05),
                c33 = quantile(ap, 0.33),
                c50 = quantile(ap, 0.50),
                c66 = quantile(ap, 0.66),
                c95 = quantile(ap, 0.95)) %>% 
  dplyr::ungroup()
spacing_factor = 0.5
plot_ap_per_elderly <- ggplot(ap_geo_per_elderly) +
  geom_density(mapping = aes(x = ap,
                             fill = quintile,
                             color = quintile),
               inherit.aes = FALSE,
               alpha = 0.1,
               size = 0.8) +
  geom_jitter(mapping = aes(x = ap,
                            y = -as.numeric(quintile) * spacing_factor * 1e-1,
                            color = quintile),
              size = 0.5,
              alpha = 0.5,
              height = 0.02) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                   x = c05, xend = c05, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                   x = c95, xend = c95, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1,
                   x = c05, xend = c33, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1,
                   x = c66, xend = c95, color = quintile),
               size = 0.8) +
  geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                ymax = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                xmin = c33, xmax = c66,
                color = quintile),
            fill = "white", alpha = 0.01, size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 + 1e-3 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 - 1e-3 * scl,
                   x = c50, xend = c50,
                   color = quintile),
               size = 0.8) +
  scale_color_manual(
    values = quintiles.color,
    name = "Elderly proportion quintiles",
    labels = quintiles.labs.per_elderly
  ) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Elderly proportion quintiles",
    labels = quintiles.labs.per_elderly
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
  theme_minimal() +
  labs(x = "PM2.5 concentration [ug/m3]", 
       y = "",
       color = "Elderly proportion quintiles") +
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
    legend.position = c(0.87,0.87)
  )

plot_ap_per_elderly

ggsave(
  file = paste0("figures/plot_ap_per_elderly.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_ap_per_elderly
)

ap_geo_per_elderly %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_per_elderly = min(per_elderly, na.rm = TRUE),
    max_per_elderly = max(per_elderly, na.rm = TRUE),
    mean_per_elderly = mean(per_elderly, na.rm = TRUE),
    median_per_elderly = median(per_elderly, na.rm = TRUE),
    sd_per_elderly = sd(per_elderly, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly     n
# <fct>              <dbl>           <dbl>            <dbl>              <dbl>          <dbl> <int>
# 1 1                 0.0645           0.187            0.166              0.173        0.0192    223
# 2 2                 0.187            0.209            0.199              0.200        0.00636   223
# 3 3                 0.209            0.227            0.218              0.218        0.00513   222
# 4 4                 0.227            0.247            0.237              0.237        0.00561   222
# 5 5                 0.247            0.344            0.269              0.263        0.0195    222



## Deaths vs urbn_type1 -------------------------------------------------------------
deaths_urbntype_sf <- deaths_socioecon_sf %>%
  dplyr::select(geo, urbn_type, deaths, geometry) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>%
  unique()

df <- data.table::as.data.table(deaths_urbntype_sf) %>%
  dplyr::select(-geometry)
df_medi <- df[, .(medi = quantile(deaths, 0.5, na.rm = T)),
              by = c("urbn_type")
]

plot_urbntype_density <- ggplot(df) +
  geom_density(
    data = df, aes(
      x = deaths, group = urbn_type,
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
  labs(x = "Premature deaths [Normalized in Million]", y = "Probability density") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.ontop = FALSE,
    strip.text = element_text(size = legend.text.size),
    strip.background = element_blank(),
    axis.title = element_text(size = legend.text.size),
    axis.title.y = element_blank(),
    axis.text = element_text(size = legend.text.size),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.key.size = unit(0.6, "cm"),
    legend.title = element_text(size = legend.text.size),
    legend.text = element_text(size = legend.text.size),
    legend.position = "None"
  )

plot_urbntype_density

ggsave(
  file = paste0("figures/plot_urbntype_density_deaths.pdf"), height = 12, width = 18, units = "cm",
  plot = plot_urbntype_density
)



## Deaths vs urbn_type2 -------------------------------------------------------------
deaths_geo_urbn_type <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, urbn_type, deaths)
deaths_geo_urbn_type <- unique(deaths_geo_urbn_type) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = urbn_type) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()

plot_deaths_urbn_type <- ggplot(deaths_geo_urbn_type) +
  geom_density(mapping = aes(x = deaths,
                             fill = quintile,
                             color = quintile),
               inherit.aes = FALSE,
               alpha = 0.1,
               size = 0.8) +
  geom_jitter(mapping = aes(x = deaths,
                            y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                            color = quintile),
              size = 0.5,
              alpha = 0.5,
              height = 0.0002) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c05, xend = c05, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c95, xend = c95, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   x = c05, xend = c33, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   x = c66, xend = c95, color = quintile),
               size = 0.8) +
  geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                ymax = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                xmin = c33, xmax = c66,
                color = quintile),
            fill = "white", alpha = 0.01, size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c50, xend = c50,
                   color = quintile),
               size = 0.8) +
  scale_color_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  scale_fill_manual(
    values = urbn_type.color,
    name = "Urban type",
    labels = urbn_type.labs
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
  theme_minimal() +
  labs(x = "Premature deaths [Normalized in Million]",
       y = "",
       color = "Urban type") +
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
    legend.position = c(0.87,0.87)
  )

plot_deaths_urbn_type

ggsave(
  file = paste0("figures/plot_deaths_urbn_type.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_urbn_type
)

deaths_geo_urbn_type %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_deaths = min(deaths, na.rm = TRUE),
    max_deaths = max(deaths, na.rm = TRUE),
    mean_deaths = mean(deaths, na.rm = TRUE),
    median_deaths = median(deaths, na.rm = TRUE),
    sd_deaths = sd(deaths, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile    min_deaths max_deaths mean_deaths median_deaths sd_deaths     n
# <fct>            <dbl>      <dbl>       <dbl>         <dbl>     <dbl> <int>
# 1 City             20.6        860.        309.          312.      102.   371
# 2 Town/Suburb       7.88       949.        360.          345.      154.   585
# 3 Rural             2.31       883.        340.          336.      144.   428




## Deaths vs GINI -------------------------------------------------------------
deaths_geo_gini <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, gini, deaths)
deaths_geo_gini <- unique(deaths_geo_gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(gini, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()

plot_deaths_gini <- ggplot(deaths_geo_gini) +
  geom_density(mapping = aes(x = deaths,
                             fill = quintile,
                             color = quintile),
               inherit.aes = FALSE,
               alpha = 0.1,
               size = 0.8) +
  geom_jitter(mapping = aes(x = deaths,
                            y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                            color = quintile),
              size = 0.5,
              alpha = 0.5,
              height = 0.0002) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c05, xend = c05, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c95, xend = c95, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   x = c05, xend = c33, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   x = c66, xend = c95, color = quintile),
               size = 0.8) +
  geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                ymax = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                xmin = c33, xmax = c66,
                color = quintile),
            fill = "white", alpha = 0.01, size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c50, xend = c50,
                   color = quintile),
               size = 0.8) +
  scale_color_manual(
    values = quintiles.color,
    name = "Gini quintiles",
    labels = quintiles.labs.gini
  ) +
  scale_fill_manual(
    values = quintiles.color,
    name = "Gini quintiles",
    labels = quintiles.labs.gini
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
  theme_minimal() +
  labs(x = "Premature deaths [Normalized in Million]",
       y = "",
       color = "Gini quintiles") +
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
    legend.position = c(0.87,0.87)
  )

plot_deaths_gini

ggsave(
  file = paste0("figures/plot_deaths_gini.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_gini
)

deaths_geo_gini %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_gini = min(gini, na.rm = TRUE),
    max_gini = max(gini, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    median_gini = median(gini, na.rm = TRUE),
    sd_gini = sd(gini, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_gini max_gini mean_gini median_gini sd_gini     n
# <fct>       <dbl>    <dbl>     <dbl>       <dbl>   <dbl> <int>
# 1 1           0.204    0.285     0.270       0.271 0.0129    629
# 2 2           0.285    0.299     0.292       0.291 0.00355   629
# 3 3           0.299    0.312     0.305       0.306 0.00392   628
# 4 4           0.312    0.348     0.327       0.327 0.0112    628
# 5 5           0.348    0.553     0.387       0.380 0.0402    628




## Deaths vs INCOME -------------------------------------------------------------
deaths_geo_income <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, income, deaths)
deaths_geo_income <- unique(deaths_geo_income) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(income, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()

plot_deaths_income <- ggplot(deaths_geo_income) +
  geom_density(mapping = aes(x = deaths,
                             fill = quintile,
                             color = quintile),
               inherit.aes = FALSE,
               alpha = 0.1,
               size = 0.8) +
  geom_jitter(mapping = aes(x = deaths,
                            y = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001,
                            color = quintile),
              size = 0.5,
              alpha = 0.5,
              height = 0.0004) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 + 1.5e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 - 1.5e-5 * scl,
                   x = c05, xend = c05, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 - 1e-5 * scl,
                   x = c95, xend = c95, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001,
                   x = c05, xend = c33, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001,
                   x = c66, xend = c95, color = quintile),
               size = 0.8) +
  geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 + 1.5e-5 * scl,
                ymax = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 - 1.5e-5 * scl,
                xmin = c33, xmax = c66,
                color = quintile),
            fill = "white", alpha = 0.01, size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 + 1.5e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 2e-1 * 0.001 - 1.5e-5 * scl,
                   x = c50, xend = c50,
                   color = quintile),
               size = 0.8) +
  scale_color_manual(
    values = quintiles.color,
    name = "income quintiles",
    labels = quintiles.labs.income
  ) +
  scale_fill_manual(
    values = quintiles.color,
    name = "income quintiles",
    labels = quintiles.labs.income
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
  theme_minimal() +
  labs(x = "Premature deaths [Normalized in Million]",
       y = "",
       color = "income quintiles") +
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
    legend.position = c(0.87,0.87)
  )

plot_deaths_income

ggsave(
  file = paste0("figures/plot_deaths_income.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_income
)

deaths_geo_income %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_income = min(income, na.rm = TRUE),
    max_income = max(income, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE),
    median_income = median(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_income max_income mean_income median_income sd_income     n
# <fct>         <dbl>      <dbl>       <dbl>         <dbl>     <dbl> <int>
# 1 1             2901.     12981.      10814.        11317.     2083.   729
# 2 2            13015.     16223.      14715.        14764.     1010.   729
# 3 3            16223.     18125.      17147.        17122.      564.   729
# 4 4            18125.     20571.      19293.        19241.      717.   729
# 5 5            20571.     46866.      22851.        22122.     3204.   728




## Deaths vs ELDERLY -------------------------------------------------------------
deaths_geo_per_elderly <- data.table::as.data.table(deaths_socioecon_sf) %>%
  dplyr::select(geo, per_elderly, deaths)
deaths_geo_per_elderly <- unique(deaths_geo_per_elderly) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0, deaths != 0) %>% 
  dplyr::mutate(quintile = as.factor(dplyr::ntile(per_elderly, split_num))) %>% 
  dplyr::group_by(quintile) %>% 
  dplyr::mutate(c05 = quantile(deaths, 0.05),
                c33 = quantile(deaths, 0.33),
                c50 = quantile(deaths, 0.50),
                c66 = quantile(deaths, 0.66),
                c95 = quantile(deaths, 0.95)) %>% 
  dplyr::ungroup()

plot_deaths_per_elderly <- ggplot(deaths_geo_per_elderly) +
  geom_density(mapping = aes(x = deaths,
                             fill = quintile,
                             color = quintile),
               inherit.aes = FALSE,
               alpha = 0.1,
               size = 0.8) +
  geom_jitter(mapping = aes(x = deaths,
                            y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                            color = quintile),
              size = 0.5,
              alpha = 0.5,
              height = 0.0002) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c05, xend = c05, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c95, xend = c95, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   x = c05, xend = c33, color = quintile),
               size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001,
                   x = c66, xend = c95, color = quintile),
               size = 0.8) +
  geom_rect(aes(ymin = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                ymax = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                xmin = c33, xmax = c66,
                color = quintile),
            fill = "white", alpha = 0.01, size = 0.8) +
  geom_segment(aes(y = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 + 1e-5 * scl,
                   yend = -as.numeric(quintile) * spacing_factor * 1e-1 * 0.001 - 1e-5 * scl,
                   x = c50, xend = c50,
                   color = quintile),
               size = 0.8) +
  scale_color_manual(
    values = quintiles.color,
    name = "per_elderly quintiles",
    labels = quintiles.labs.per_elderly
  ) +
  scale_fill_manual(
    values = quintiles.color,
    name = "per_elderly quintiles",
    labels = quintiles.labs.per_elderly
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, fill = NA) ) ) +
  theme_minimal() +
  labs(x = "Premature deaths [Normalized in Million]",
       y = "",
       color = "per_elderly quintiles") +
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
    legend.position = c(0.87,0.87)
  )

plot_deaths_per_elderly

ggsave(
  file = paste0("figures/plot_deaths_per_elderly.pdf"), height = 10, width = 18, units = "cm",
  plot = plot_deaths_per_elderly
)

deaths_geo_per_elderly %>%
  dplyr::group_by(quintile) %>%
  dplyr::summarise(
    min_per_elderly = min(per_elderly, na.rm = TRUE),
    max_per_elderly = max(per_elderly, na.rm = TRUE),
    mean_per_elderly = mean(per_elderly, na.rm = TRUE),
    median_per_elderly = median(per_elderly, na.rm = TRUE),
    sd_per_elderly = sd(per_elderly, na.rm = TRUE),
    n = dplyr::n()
  )
# quintile min_per_elderly max_per_elderly mean_per_elderly median_per_elderly sd_per_elderly     n
# <fct>       <dbl>    <dbl>     <dbl>       <dbl>   <dbl> <int>
# 1 1           0.204    0.285     0.270       0.271 0.0129    629
# 2 2           0.285    0.299     0.292       0.291 0.00355   629
# 3 3           0.299    0.312     0.305       0.306 0.00392   628
# 4 4           0.312    0.348     0.327       0.327 0.0112    628
# 5 5           0.348    0.553     0.387       0.380 0.0402    628

