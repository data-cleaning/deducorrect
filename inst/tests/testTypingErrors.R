library(testthat)
library(editrules)

A.csv <- read.csv(textConnection(
"name, edit
e1, x1 + x2 == x3
e2, x2 == x4
e3, x5 + x6 + x7 == x8
e4, x3 + x8 == x9
e5, x9 - x10 == x11"
))

A <- editmatrix(A.csv)

dat <- read.csv(textConnection(
"x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
1452,116,1568,161,323,76,12,411,1979,1842,137
1452,116,1568,161,323,76,12,411,19979,1842,137"
))


test_that("Comparing equal strings works",{
   cor <- typingErrors(A,dat)
   print(cor)
})

