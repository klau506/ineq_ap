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
  

dat = ap_socioecon_sf %>% 
  dplyr::select(ctry, ap, urbn_type, per_elderly, income, gini) %>% 
  dplyr::filter(rowSums(is.na(.)) == 0) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(log_income = log(income),
                log_per_elderly = log(per_elderly))

cor(as.data.frame(dat)[, c("log_income", "log_per_elderly", "gini")], use = "complete.obs")
#                   log_income log_per_elderly        gini
# log_income       1.00000000       0.2984748 -0.05218798
# log_per_elderly  0.29847479       1.0000000 -0.34195670
# gini            -0.05218798      -0.3419567  1.00000000
# note: |everything| < 0.8 => no correlation



dat_no0 <- dat %>% 
  dplyr::filter(ap != 0)
model1 = lm(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + log(gini), data = dat_no0)
model2 = lm(ap ~ factor(urbn_type) + log(income) + log(per_elderly), data = dat_no0)
model3 = glm(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini, data = dat,
             family = poisson(link = 'inverse')) # inverse better than log/identity
model4 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini, data = dat) # no fixed effects
model5 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) | ctry, data = dat) # fixed effects
model6 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + log(gini)| ctry, data = dat) # fixed effects
model7 <- feols(ap ~ factor(urbn_type) + log(income) + log(per_elderly) + gini| ctry, data = dat) # fixed effects
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
check_singularity(model7)  #OK
check_singularity(model8)  #OK

# Heteroscedasticity
check_heteroscedasticity(model1)  #OK
check_heteroscedasticity(model2)  #ERROR!
check_heteroscedasticity(model3)
check_heteroscedasticity(model4)  #OK
check_heteroscedasticity(model5)  #OK
check_heteroscedasticity(model6)  #OK
check_heteroscedasticity(model7)  #OK
check_heteroscedasticity(model8)  #OK
check_heteroscedasticity(model9)  #OK
check_heteroscedasticity(model12b)  #OK

# Multicollinearity
check_collinearity(model5)  #Low
check_collinearity(model6)  #Moderate: log(gini) 9.99; log(income) 6.02; log(per_elderly) 3.89
check_collinearity(model7)  #Moderate: gini 5.22; log(income) 2.96; log(per_elderly) 4.41
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
check_model(model12i)

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
compare_performance(model12e, model12i, model7, verbose = FALSE)
plot(compare_performance(model12e, model12i, model7, rank = TRUE, verbose = FALSE))

# note: model2 Error heteroscedasticity; model3 Error score_log (-inf);
# note: model4 < model1 < model5 < model14 < model8/model9 < model13 < model6 < model7 < model12i 

