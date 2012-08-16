


source("../pkg/R/deterministic.R")

u <- imputationRules(c(
     "y <- ifelse( x > 3 || x < 1 , 5, y)"
   , "y <- ifelse( x < 3 , 0, y)"
   , "if ( 0 < 1){ x<-1} else {'aap'}"
   , "if (m>0) print('fiets')"
))



d <- data.frame(
   x = 1:5,
   y = 5:1
)

imputeWithRules(u,d)


source("../pkg/R/deterministic.R")
e <- expression(
   if ( x == 3 ) y <- mean(x)
)

v <- getvrs(e,L=character(0))


f <- function(m,...){
   if (!m){
      x <- 1:3
   }
   if (length(x)>0){
      print(x)
      f(TRUE,x[-1])
   }

}



source("../pkg/R/deterministic.R")
checkSymbols(expression(mean(x)))


