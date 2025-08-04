library(easystats)
library(fixest)
library(splines)

# | Metric             | Definition                                                        | Good? (↑ or ↓)   |
# |--------------------|-------------------------------------------------------------------|------------------|
# | AIC                | Akaike Information Criterion – penalizes model complexity + fit   | Lower is better  |
# | AICc               | Corrected AIC for small sample sizes                              | Lower is better  |
# | BIC                | Bayesian Information Criterion – more strict penalty than AIC     | Lower is better  |
# | R²                 | Proportion of variance in the response explained by the model     | Higher is better |
# | Adjusted R²        | Like R² but adjusted for number of predictors                     | Higher is better |
# | RMSE               | Root Mean Square Error – avg. error magnitude                     | Lower is better  |
# | Sigma              | Residual standard deviation                                       | Lower is better  |
# | Nagelkerke’s R²    | Pseudo R² for logistic or non-linear models                       | Higher is better |
# | Score_log          | Logarithmic score (used in probabilistic models)                  | Higher is better |
# | Score_spherical    | Another scoring rule for probabilistic models                     | Higher is better |
  

### NUTS3
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

### GRID
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
dat_grid->dat
model7a <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly), data = dat)
model7b <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly)| ctry, data = dat) # fixed effects
model12e <- feols(ap ~ factor(urbn_type) + log(income)*log(per_elderly) | ctry, data = dat) # interactions
model12i <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) | ctry, data = dat) # interactions





dat_no0 <- dat %>% 
  dplyr::filter(ap != 0)
model1 = lm(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + log(gini), data = dat_no0)
model2 = lm(ap ~ factor(urbn_type) + log(income) + log(per_elderly), data = dat_no0)
model3 = glm(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini, data = dat,
             family = poisson(link = 'inverse')) # inverse better than log/identity
model4 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini, data = dat) # no fixed effects
model5 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) | ctry, data = dat) # fixed effects
model6 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + log(gini)| ctry, data = dat) # fixed effects
model7a <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini, data = dat)
model7b <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini| ctry, data = dat) # fixed effects
model8 <- feols(ap ~ factor(urbn_type) + log(income) + per_elderly + gini| ctry, data = dat) # fixed effects
model9 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly)| ctry, data = dat) # fixed effects
model10 <- fenegbin(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini | ctry, data = dat) # Very high value of theta (10000). There is no sign of overdispersion, you may consider a Poisson model.
model11 <- feglm(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini | ctry,
                     family = Gamma(link = "log"), data = dat) # Error in eval(family$initialize) non-positive values not allowed for the 'Gamma' family
model12a <- feols(ap ~ factor(urbn_type)*log(income) + log(per_elderly) + gini | ctry, data = dat) # interactions
model12b <- feols(ap ~ factor(urbn_type):log(income) + log(per_elderly) + gini | ctry, data = dat) # interactions
model12d <- feols(ap ~ factor(urbn_type) + log(income) + log(income):log(per_elderly) + gini | ctry, data = dat) # interactions
model12e <- feols(ap ~ factor(urbn_type) + log(income)*log(per_elderly) + gini | ctry, data = dat) # interactions
model12f <- feols(ap ~ factor(urbn_type) + log(income):log(per_elderly) + gini | ctry, data = dat) # interactions
model12h <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + log(per_elderly):gini | ctry, data = dat) # interactions
model12i <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly)*gini | ctry, data = dat) # interactions
model12j <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly):gini | ctry, data = dat) # interactions
model13 <- feols(ap ~ factor(urbn_type) + bs(log(income), 3) + log(per_elderly) + gini | ctry, data = dat) # splines
model14 <- fepois(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini | ctry, data = dat)


model7a <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini, data = dat)



# R2
r2(model1)
r2(model2)
r2(model3)
r2(model4)
r2(model5)
r2(model6)

# Over dispersion
check_overdispersion(model3)

# Zero inflation
check_zeroinflation(model3)

# Singularity
check_singularity(model3)  #OK
check_singularity(model5)  #OK
check_singularity(model6)  #OK
check_singularity(model7b)  #OK
check_singularity(model8)  #OK

# Heteroscedasticity
check_heteroscedasticity(model1)  #OK
check_heteroscedasticity(model2)  #ERROR!
check_heteroscedasticity(model3)
check_heteroscedasticity(model4)  #OK
check_heteroscedasticity(model5)  #OK
check_heteroscedasticity(model6)  #OK
check_heteroscedasticity(model12i)  #OK
check_heteroscedasticity(model8)  #OK
check_heteroscedasticity(model9)  #OK
check_heteroscedasticity(model12b)  #OK

# Multicollinearity
check_collinearity(model5)  #Low
check_collinearity(model6)  #Moderate: log(gini) 9.99; log(income) 6.02; log(per_elderly) 3.89
check_collinearity(model7a)  #Low
check_collinearity(model7b)  #Moderate: gini 5.22; log(income) 2.96; log(per_elderly) 4.41
check_collinearity(model8)  #Low
check_collinearity(model14) #Moderate: gini 5.38
check_collinearity(model13) #Moderate: gini 8.73; log(per_elderly) 9.14
check_collinearity(model12b) #Moderate: gini 8.73; log(per_elderly) 9.14

# ICC
icc(model1)  #OK
icc(model2)  #OK
icc(model3)  #OK
icc(model4)  #OK
icc(model5)  #OK
icc(model6)  #OK
icc(model7)  #OK
icc(model8)  #OK

# General check
pl1 <- check_model(model1); pl1
pl2 <- check_model(model1); pl2
pl3 <- check_model(model1); pl3
pl4 <- check_model(model4); pl4
pl5 <- check_model(model5); pl5
pl6 <- check_model(model6); pl6 # WARNING: some collineality
pl7 <- check_model(model7); pl7 # WARNING: some collineality
pl8 <- check_model(model8); pl8
pl9 <- check_model(model9); pl9
check_model(model7b)

# Model performance
model_performance(model1)
model_performance(model2)
model_performance(model3)
model_performance(model4)
model_performance(model5)
model_performance(model6)
model_performance(model7)
model_performance(model8)
model_performance(model9)
model_performance(model12i)

# Model comparison
compare_performance(model7a, model7b, verbose = FALSE)
compare_performance(model12e, model7b, verbose = FALSE)
plot(compare_performance(model12e, model12i, model7b, rank = TRUE, verbose = FALSE))

# note: model2 Error heteroscedasticity; model3 Error score_log (-inf);
# note: model4 < model1 < model5 < model14 < model8/model9 < model13 < model6 < model7 < model12i 

