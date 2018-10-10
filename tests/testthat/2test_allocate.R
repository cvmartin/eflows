library(eflows)
context("work of allocate()")

test_that("basic working", {
  expect_equal(allocate(23, c(0.5, 0.5), c(2,0), c(20, 1)),
               c(18,1,4))
  expect_equal(allocate(130, c(9, 2, 5, 1), c(30, 30, 20, 10), c(90,40, 50, 80)),
               c(60, 10, 30, 30, 0))
})

# Basic
allocate(100, c(25,50), c(75, 75))
allocate(60, c(25,50), c(75, 75))

# Using variable share
allocate(100, c(25,50), c(75, 75), c(0.7, 0.3))
allocate(60, c(25,50), c(75, 75), c(0.7, 0.3))

# efficiency
allocate(100, c(25,50), c(75, 75), c(0.7, 0.3), eff = c(0.9,0.9))

# Giving preference at the same level, and charging?:
allocate(75, c(25,50), c(0, 0), c(0.5,0), eff = c(0.9,0.9))

#work with discharge
allocate(-100, c(25,50), c(0, 0), c(0.5, 0.5), eff = c(0.9,0.9))

microbenchmark(allocate(-100, c(25,50), c(0, 0), c(0.5, 0.5), eff = c(0.9,0.9)),times = 1000)
microbenchmark(allocate(-100, c(25,50), c(0, 0), c(0.5, 0.5)),times = 1000)

###
allocate(100, c(25,50), c(75, 75), c(0.5, 0.5))

# When the battery is filled and there is no preference, just make it zero
allocate(100, c(75,75), c(75, 75), c(0, 0),eff = c(0.9,0.9))

# Should provide an error
allocate(40, c(30, 50, 10), c(0, 70, 50)) # Cannot mix charge and discharge

# This is how you combine charge and discharge:
allocate(40, c(30, 50, 10), c(0, 70, 50), active = c(TRUE, FALSE, FALSE))
allocate(70, c(0, 50, 10), c(0, 70, 50), active = c(FALSE, TRUE, TRUE))


allocate(50, c(25,50), c(75, 75), c(0.5, 0.5))
allocate(100, c(25,50), c(75, 75), c(5, 0.5))


allocate(100, c(25,50), c(75, 75), c(5, 0.5))



allocate(130, c(9, 2, 5, 1), c(30, 30, 20, 10), c(90,40, 50, 80))
allocate(100, c(9, 2, 5, 1), c(30, 30, 20, 10), c(90,40, 50, 80))
allocate(60, c(9, 2, 5, 1), c(30, 30, 20, 10), c(90,40, 50, 80), eff = c(0.9, 1, 0.8, 1))

allocate(120, c(9, 2, 5, 1), c(30, 30, 50, 30), c(90,40, 50, 80), level = c(3, 2, 1, 3))
##
soc <- c(20,34,50)
vol <- c(24, 34, 60)
level <- c(1,1,1)
m <-  matrix(c(soc, vol, level), ncol = length(soc), byrow = TRUE,
             dimnames = list(c("soc", "vol", "level")))
print(m)

sel <- c(TRUE,FALSE,TRUE)
sel <- as.logical(c(1,0,1))

faa <- m[,sel]
foo <- faa * 234

m[,sel] <- foo
print(m)

m[which(sel)]

v1[which(v2)] <- v1[v2]*10


## Megatest

vol <- runif(100, 50, 100)
soc <- vol * runif(100, 0.1,0.9)
share <- runif(100)
level <- sample(seq(1,4), 100, replace = TRUE)
active <- sample(seq(FALSE,TRUE), 100, replace = TRUE)
eff <- runif(100, 0.8,1)


# eff still causes a bug. What is it?
allocate(10000, soc, vol, share, level, active, eff)
