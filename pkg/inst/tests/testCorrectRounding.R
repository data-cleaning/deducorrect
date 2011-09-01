library(testthat)
library(editrules)

context("CorrectRounding")

test_that("correctRounding works",{
   R <- editmatrix(c("a == 1"))
   dat <- data.frame(a=2)
   sol <- correctRounding(R,dat)
   expect_equivalent(sol$corrected[1,], data.frame(a=1))
})



test_that("correctRounding with fixate works",{
   R <- editmatrix(c("a + b == 2"))
   dat <- data.frame(a=2,b=1)
   sol <- correctRounding(R,dat, fixate="b")
   expect_equivalent(sol$corrected[1,], data.frame(a=1,b=1))
})

test_that("correctRounding with Q works",{
   set.seed(1)
   R <- editmatrix(c("a + b == 2", "b>0"))
   dat <- data.frame(a=2,b=1)
   sol <- correctRounding(R,dat)
   expect_equivalent(sol$corrected[1,], data.frame(a=1,b=1))
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
   expect_equal(as.character(sol$status$status), "corrected")
})


# smoke test
context("Smoke test of correctrounding")
s <- sample(.Machine$integer.max,1)
cat("Randseed is ",s,":")
set.seed(s)
E <- editmatrix(c("x+y==z","x>=0","y>=0"))
for ( i in 1:10 ){
    cat(i); flush.console()
    x <- sample(0:2,10,replace=TRUE)
    y <- sample(0:2,10,replace=TRUE)
    z <- x + y + sample(c(-1,1),10,replace=TRUE) 
    v <- correctRounding(E,data.frame(x=x,y=y,z=z))
    test_that("No extra inequalities are generated",{
        expect_equal(sum(violatedEdits(E,v$corrected)[,2:3]),0)
    })
}












