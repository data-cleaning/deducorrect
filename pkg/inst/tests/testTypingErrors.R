library(testthat)
library(editrules)


E <- editmatrix( c("x1 + x2 == x3"
                  ,"x2 == x4"
                  ,"x5 + x6 + x7 == x8"
                  ,"x3 + x8 == x9"
                  ,"x9 - x10 == x11"
                  )
               )

dat <- read.csv(textConnection(
"    , x1, x2 , x3  , x4 , x5 , x6, x7, x8 , x9   , x10 , x11
4  , 1452, 116, 1568, 116, 323, 76, 12, 411,  1979, 1842, 137
4.1, 1452, 116, 1568, 161, 323, 76, 12, 411,  1979, 1842, 137
4.2, 1452, 116, 1568, 161, 323, 76, 12, 411, 19979, 1842, 137
4.3, 1452, 116, 1568, 161,   0,  0,  0, 411, 19979, 1842, 137
4.4, 1452, 116, 1568, 161, 323, 76, 12,   0, 19979, 1842, 137"
))

test_that("typingErrors works",{   
   cor <- typingErrors(E,dat)
   #print(cor)
   corrected <- cor$corrected
   expect_equal(corrected[1,], dat[1,-1])
   expect_equal(corrected[2,], dat[1,-1])
   expect_equal(corrected[3,], dat[1,-1])
   #TODO add checks
})

