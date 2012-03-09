require(editrules)

library(editrules)
for ( d in dir("../pkg/R",full.names=TRUE)) dmp <- source(d)

E <- editmatrix(c( "x1 + x2 == x3"
                 , "x2 == x4"
                 , "x5 + x6  + x7== x8"
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

sol <- correctRounding(E, dat)


E <- editmatrix(expression(
    x + y == z
))
dat <- data.frame(
    x = sample(10),
    y = sample(10))
dat$z = dat$x + dat$y + sample(c(-1,0,1),10,replace=TRUE)

for ( d in dir("../pkg/R",full.names=TRUE)) dmp <- source(d)
d <- correctRounding(E,dat)

k <- c(3,6)
irws <- d$corrections$row %in% k
cls <- as.character(d$corrections[irws,'variable'])
cls <- match(cls,names(dat))
rws <- d$correction$row[irws]

A <- array(FALSE,dim=dim(d$corrected))
A[cbind(rws,cls)] <- TRUE
d$corrected[A] <- d$corrections[irws,'old']
d$corrections <- d$corrections[-irws,]
d$status$status[k] <- 'invalid'




