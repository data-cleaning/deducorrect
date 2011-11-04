# These examples are taken from De Waal et al (2011) (Examples 9.1-9.2)
E <- editmatrix(c(
    "x1 + x2      == x3",
    "x2           == x4",
    "x5 + x6 + x7 == x8",
    "x3 + x8      == x9",
    "x9 - x10     == x11",
    "x6 >= 0",
    "x7 >= 0"
))


dat <- data.frame(
    x1=c(145,145),
    x2=c(NA,NA),
    x3=c(155,155),
    x4=c(NA,NA),
    x5=c(NA, 86),
    x6=c(NA,NA),
    x7=c(NA,NA),
    x8=c(86,86),
    x9=c(NA,NA),
    x10=c(217,217),
    x11=c(NA,NA)
)


d <- deduImpute(E,dat)
d$corrected
d$status
d$corrections

# example with solSpace method for editmatrix
# example 9.1 of De Waal et al (2011).
x <-t(dat)[,1]
s <- solSpace(E,x)
s

# some values are uniquely determined and may be imputed directly:
u <- rowSums(abs(s$C)) == 0
imiss <- is.na(x)
# impute unique values
x[imiss][u] <- s$x0[u]

# choose z=1 (arbitrary) to impute the other values 
z <- rep(1,sum(!u))
x[imiss][!u] <- s$x0[!u] +  s$C[!u,!u]\%*\%z

# did it work? (use a tolerance in checking to account for machine rounding)
violatedEdits(E,x,tol=1e-8)

# Find out which values can be deductively imputed with 0:
# (Example 9.2 of De Waal et al. (2011))
x <- t(dat)[,2]

I <- deductiveZeros(E,x)
I
# we may impute x deductively with
x[I] <- 0




