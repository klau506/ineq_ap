###############################################################################

# Study                 : Health impacts and socioeconomic inequalities of future outdoor air pollution in an NECP-compliant Europe
# Date                  : Oct. 2025
# Author                : Clàudia Rodés-Bachs
# Institute             : BC3-Basque Centre for Climate Change
# Description           : Helper script with palettes and labels
# Re-usage instructions : Execute main.R, which will automatically run this script

###############################################################################

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
