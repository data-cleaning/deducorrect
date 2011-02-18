
require(editrules)
# Example of scholtus (2008)
edits <- c(
    "x0 == x0r - x0c",
    "x1 == x1r - x1c",
    "x2 == x2r - x2c",
    "x3 == x3r - x3c",
    "x4 == x0 + x1 + x2 + x3")

E <- editmatrix(edits) 
isTotallyUnimodular(E)

dat <- data.frame(
   case = c("a","b","c","d"),
   x0r = c(2100,5100,3250,5726),
   x0c = c(1950,4650,3550,5449),
   x0  = c( 150, 450, 300, 276),
   x1r = c(   0,   0, 110,  17),
   x1c = c(  10, 130,  10,  26),
   x1  = c(  10, 130, 100,  10),
   x2r = c(  20,  20,  50,   0),
   x2c = c(   5,   0,  90,  46),
   x2  = c(  15,  20,  40,  46),
   x3r = c(  50,  15,  30,   0),
   x3c = c(  10,  25,  10,   0),
   x3  = c(  40,  10,  20,   0),
   x4  = c( 195, 610,-140, 221))


source("correctsigns.r")
dat2 <- correctSigns(E,dat, eps=2)

checkRows(E,dat2)
listErrors(E, dat)















   
   