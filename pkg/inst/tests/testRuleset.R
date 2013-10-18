
context("Ruleset")
library(editrules)
.onLoad(0,0)
test_that("parser",{
   e <- ruleset(expression(
      if ( x > y ) x <- y,
      if ( is.na(z) ) x <- 0
   ))
   expect_true(all(sapply(e,is.language)))
   expect_equal(length(e),2)
   expect_equal(sort(getVars(e)),c('x','y','z'))
   expect_error(ruleset("if (is.na(x)) x <- mean(y)"))
   expect_equal(
      as.character(ruleset(expression(if ( x < 0 ) x <- 0))),
      as.character(ruleset("if ( x < 0 ) x <- 0",file=FALSE))
   )
   e <- ruleset("if ( x < 0 ) x <- 0",file=FALSE)
   expect_equal(as.character(e), as.character(ruleset(as.character(e),file=FALSE)))

})



test_that("correction",{
   expect_equal(
      applyRules(
         ruleset("if ( x < 0 ) x <- 0",file=FALSE),
         data.frame(x=c(-1,0,1))
      )$dat,
      data.frame(x=c(0,0,1))
   )
       
})

test_that("logging",{
   b <- ruleset("if ( x < 0 ) x <- 0", file=FALSE)
   how <- "x <- 0 because x < 0"
   expect_equivalent(
      applyRules(
         b,
         data.frame(x=-1,0,1)
      )$log
    , data.frame(row=1,variable='x',old=format(-1),new=format(0),how=how,stringsAsFactors=FALSE)
   )
})


test_that("logging for changing a variable twice",{
   df <- data.frame(
      x = 1,
      y = NA,
      z = 2)

   u <- ruleset(expression(
      if ( x == 1 ) z <- NA,
      if ( is.na(z) ) z <- 1
   ))
   x <- applyRules(u,df)

   expect_equal(x$log[2,'old'],format(NA))
   expect_equal(x$log[2,'new'],format(1))
})


test_that("changing multiple variables in a record with different rules",{

   df <- data.frame(
    x = 1,
   y = NA,
   z = 2)


   w <- ruleset(expression(
      if ( x == 1 ) x <- NA,
      if ( is.na(y) ) y <- 1
   ))
   x <- applyRules(w,df)
   expect_equal(nrow(x$log),2)
   expect_equal(x$log$old,sapply(c(1,NA),format))
   expect_equal(x$log$new,sapply(c(NA,1),format))

})

