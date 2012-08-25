
context("Deterministic corrections")
library(editrules)
.onLoad(0,0)
test_that("parser",{
   e <- correctionRules(expression(
      if ( x > y ) x <- y,
      if ( is.na(z) ) x <- 0
   ))
   expect_true(all(sapply(e,is.language)))
   expect_equal(length(e),2)
   expect_equal(sort(getVars(e)),c('x','y','z'))
   expect_error(correctionRules("if (is.na(x)) x <- mean(y)"))
   expect_equal(
      as.character(correctionRules(expression(if ( x < 0 ) x <- 0))),
      as.character(correctionRules("if ( x < 0 ) x <- 0",file=FALSE))
   )
   e <- correctionRules("if ( x < 0 ) x <- 0",file=FALSE)
   expect_equal(as.character(e), as.character(correctionRules(as.character(e),file=FALSE)))

})



test_that("correction",{
   expect_equal(
      correctWithRules(
         correctionRules("if ( x < 0 ) x <- 0",file=FALSE),
         data.frame(x=c(-1,0,1))
      )$corrected,
      data.frame(x=c(0,0,1))
   )
       
})

test_that("logging",{
   b <- correctionRules("if ( x < 0 ) x <- 0", file=FALSE)
   expect_equivalent(
      correctWithRules(
         b,
         data.frame(x=-1,0,1)
      )$corrections
    , data.frame(row=1,variable='x',old=-1,new=0,how=as.character(b),stringsAsFactors=FALSE)
   )
})


