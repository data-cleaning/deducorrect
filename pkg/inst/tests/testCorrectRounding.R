library(testthat)
library(editrules)

R <- editmatrix("a + b == 2")
Q <- editmatrix("b >= 0")

dat <- data.frame(a=2,b=1)

test_that("correctRounding works",{
   sol <- correctRounding(R,dat,Q)
   expect_equivalent(sol$corrected[1,], data.frame(a=1,b=1))
   #print(sol)
})


test_that("correctRounding works with Scholtus 2008 example",{
   E <- editmatrix(c( "x1 + x2 == x3"
                    , "x2 == x4"
                    , "x5 + x6  + x7 == x8"
                    , "x3 + x8 == x9"
                    , "x9 - x10 == x11"
                    )
                  )

   dat <- data.frame( x1=12
                    , x2=4
                    , x3=15
                    , x4=4
                    , x5=3
                    , x6=1
                    , x7=8
                    , x8=11
                    , x9=27
                    , x10=41
                    , x11=-13
                    )
   sol <- correctRounding(E,dat)
   #print(sol)
   expect_equal(as.character(sol$status$status), "corrected")
})