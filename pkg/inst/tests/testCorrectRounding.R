library(testthat)
library(editrules)

R <- editmatrix("a + b == 2")
Q <- editmatrix("b >= 0")

dat <- data.frame(a=2,b=1)

test_that("correctRounding works",{
   sol <- correctRounding(R,Q,dat)
   #print(sol)
})