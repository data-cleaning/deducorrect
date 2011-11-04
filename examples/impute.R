

# This example is taken from De Waal et al (2011) (Example 9.1.)
E <- editmatrix(c(
    "x1 + x2      == x3",
    "x2           == x4",
    "x5 + x6 + x7 == x8",
    "x3 + x8      == x9",
    "x9 - x10     == x11",
    "x6 >= 0",
    "x7 >= 0"
))


# 
x <- c(x1=145,x2=NA,x3=155,x4=NA,x5=NA,x6=NA,x7=NA,x8=86,x9=NA,x10=217,x11=NA)

s <- solSpace(E,x)
s

# some values are uniquely determined and may be imputed directly:
u <- rowSums(abs(s$C)) == 0
imiss <- is.na(x)
# impute unique values
x[imiss][u] <- s$x0[u]

# choose z=1 (arbitrary) to impute the other values 
z <- rep(1,sum(!u))
x[imiss][!u] <- s$x0[!u] +  s$C[!u,!u]%*%z

# did it work? (use a tolerance in checking to account for machine rounding)
violatedEdits(E,x,tol=1e-8)

# Find out which values can be deductively imputed with 0:
# (Example 9.2 of De Waal et al. (2011))
x <- c(x1=145, x2=NA, x3=155, x4=NA, x5=86, x6=NA, x7=NA, x8=86, x9=NA, x10=217,x11=NA)

I <- deductiveZeros(E,x)
I
# we may impute x deductively with
x[I] <- 0




