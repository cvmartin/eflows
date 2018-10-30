library(eflows)


# FAILS -------------------------------------------------------------------

# The ncol of the data and the length of steps should match. 
flex_mtx$new(as.matrix(sept$d_ev), c(1, 2), "ev1")

# all the objects of flex must have a flex_mtx class
p_demand <- e_demand$new(fixed = as.vector(sept$d_house_smooth),
                         flex = list(flex_mtx$new(as.matrix(sept$d_ev), c(1), "ev1"),
                                     "avocado"
                         )
)

# demand must inherit from e_demand
proto <- e_frame$new(sept$datetime)
p2_demand <- e_demand$new(fixed = as.vector(sept$d_house_smooth),
                         flex = list(flex_mtx$new(as.matrix(sept$d_ev), c(1), "ev1"),
                                  flex_mtx$new(as.matrix(sept$d_ev), c(7), "ev7"),
                                     flex_mtx$new(as.matrix(sept$d_ev), c(8), "ev8")
                         )
)
# proto$set_demand(p2_demand)
proto$set_demand(mtcars)


# e_production class ------------------------------------------------------

prod <- e_production$new(fixed = list(solar = sept$solar, 
                                      wind = sept$d_ev))


assertError(sqrt("abc"))
assertWarning(matrix(1:8, 4,3))

assertCondition( ""-1 ) # ok, any condition would satisfy this

try( assertCondition(sqrt(2), "warning") )
## .. Failed to get warning in evaluating sqrt(2)
assertCondition(sqrt("abc"), "error")   # ok
try( assertCondition(sqrt("abc"), "warning") )# -> error: had no warning
assertCondition(sqrt("abc"), "error")
## identical to assertError() call above