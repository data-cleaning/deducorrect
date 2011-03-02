# load sources
src <- sapply(list.files("../pkg/R", full.names=TRUE), source)

# load demos
#examples <- sapply(list.files("../examples", full.names=TRUE), source)


E <- editmatrix( c( "x1 == x2"
                     , "9*x1 == x2"
                     )
                  )

   #print(E)
   data <- data.frame(
    x1 = 10,
    x2 = 99,
    x3 = 91)

   # fail:
   cor <- correctTypos(E,data)
   