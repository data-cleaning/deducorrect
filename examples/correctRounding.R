require(editrules)

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

#sol <- correctRounding(E, dat)
