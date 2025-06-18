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
