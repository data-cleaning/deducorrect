# load sources
library(editrules)

src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
#examples <- sapply(list.files("../examples", full.names=TRUE), source)

dat <- data.frame(x1=c(10,100,100), x2=c(200,20,200), x3=c(300,300,30))
E <- editmatrix("x1+x2==x3")