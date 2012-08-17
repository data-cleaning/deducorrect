
# some faulty data


source('../pkg/R/deterministic.R')

v <- substitute({ 
   a + b = c
   if ( is.na(x) ) x <- 0 
})


w <- substitute("if ( is.na(x) ) y=0")

u <- imputationRules({
   if ( is.na(x) ) y <- 3
   if ( is.na(y) ){ 
      y <- 0
   } else {
      y <- 10
   }
})

dat <- data.frame(x=c(NA,1),y=c(NA,NA))
imputeWithRules(u,dat)

