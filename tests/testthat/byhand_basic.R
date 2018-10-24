library(eflows)




# e_production class ------------------------------------------------------

prod <- e_production$new(fixed = list(solar = sept$solar, 
                                      wind = sept$d_ev))
