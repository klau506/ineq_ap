
# Study                 : Health impacts and socioeconomic inequalities of future outdoor air pollution in an NECP-compliant Europe
# Date                  : Oct. 2025
# Author                : Clàudia Rodés-Bachs
# Institute             : BC3-Basque Centre for Climate Change
# Description           : Check the variables' correlation indices
# Re-usage instructions : Execute this R script through the main.R script (to properly load all data and helper functions)

######################################################################## NUTS3 ##
dat = ap_socioecon_sf %>% 
  dplyr::select(ctry, ap, urbn_type, per_elderly, income, gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(log_income = log(income),
                log_per_elderly = log(per_elderly))

dat2 <- dat %>% 
  dplyr::mutate(urbn_type = dplyr::if_else(urbn_type == 'Rural', 1,
                                           dplyr::if_else(urbn_type == 'Town/Suburb', 2, 3)))
cor(as.data.frame(dat2)[, c("urbn_type", "log_income", "log_per_elderly", "gini")], use = "complete.obs")
#                  urbn_type  log_income log_per_elderly        gini
# urbn_type        1.0000000  0.24337703      -0.2924739  0.19616027
# log_income       0.2433770  1.00000000       0.2984748 -0.05218798
# log_per_elderly -0.2924739  0.29847479       1.0000000 -0.34195670
# gini             0.1961603 -0.05218798      -0.3419567  1.00000000
cor(as.data.frame(dat2)[, c("urbn_type", "income", "per_elderly", "gini")], use = "complete.obs")
#              urbn_type       income per_elderly         gini
# urbn_type    1.0000000 0.241311634  -0.2992467  0.196160271
# income       0.2413116 1.000000000   0.2283344  0.003599785
# per_elderly -0.2992467 0.228334428   1.0000000 -0.335870752
# gini         0.1961603 0.003599785  -0.3358708  1.000000000
# note: |everything| < 0.8 => no correlation

######################################################################### GRID ##
rr1 <- load_grid_data_urb()
rr2 <- load_grid_data_inc()
rr3 <- load_grid_data_eld()

valid_idx <- !is.na(rr1[[1]]) & !is.na(rr1[[2]]) &
  !is.na(rr2[[2]]) & !is.na(rr3[[2]]) & !is.infinite(rr3[[2]])
dat_grid <- data.frame(ap = pm_values[valid_idx],
                       urbn_type = rr1[[2]][valid_idx],
                       income = rr2[[2]][valid_idx],
                       per_elderly = rr3[[2]][valid_idx],
                       ctry = ctry_values[valid_idx]) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::filter(rowSums(dplyr::across(where(is.numeric), ~ is.infinite(.x))) == 0) %>% 
  dplyr::distinct()

cor(as.data.frame(dat_grid)[, c("urbn_type", "income", "per_elderly")], use = "complete.obs")
#                 urbn_type       income   per_elderly
# urbn_type    1.0000000000 0.1008084873 -0.0006760665
# income       0.1008084873 1.0000000000  0.0004751926
# per_elderly -0.0006760665 0.0004751926  1.0000000000

dat_grid_log <- dat_grid %>% 
  dplyr::mutate(log_income = log(income),
                log_per_elderly = log(per_elderly)) %>% 
  dplyr::filter(rowSums(dplyr::across(where(is.numeric), ~ is.infinite(.x))) == 0)

cor(as.data.frame(dat_grid_log)[, c("urbn_type", "log_income", "log_per_elderly")], use = "complete.obs")
#                   urbn_type    log_income  log_per_elderly
# urbn_type        1.00000000    0.08875467      -0.02543213
# log_income       0.08875467    1.00000000       0.05054584
# log_per_elderly -0.02543213    0.05054584       1.00000000


