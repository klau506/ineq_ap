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
quintiles.labs.per_elderly.nuts3 = c('Q1 [0.0586 - 0.182]',
                                     'Q2 [0.182 - 0.206]',
                                     'Q3 [0.206 - 0.224]',
                                     'Q4 [0.224 - 0.246]',
                                     'Q5 [0.246 - 0.354]')
quintiles.labs.income.nuts3 = c('Q1 [2901 - 12981]',
                                'Q2 [12981 - 16223]',
                                'Q3 [16223 - 18125]',
                                'Q4 [18125 - 20571]',
                                'Q5 [20571 - 46866]')
names(quintiles.labs.gini.nuts3) <- c('1','2','3','4','5')

quintiles.labs.per_elderly.grid = c('Q1 [0.605e-7 - 0.146]',
                                    'Q2 [0.146 - 0.194]',
                                    'Q3 [0.194 - 0.238]',
                                    'Q4 [0.238 - 0.310]',
                                    'Q5 [0.310 - 1]')
quintiles.labs.income.grid = c('Q1 [1586 - 6016]',
                               'Q2 [6016 - 10013]',
                               'Q3 [10013 - 14098]',
                               'Q4 [14098 - 16526]',
                               'Q5 [16526 - 191019]')
