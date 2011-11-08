
#############################################
# IMPUTATION OF NUMERIC DATA
#############################################

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
imputess(x,s$x0,s$C)


# To impute everything, we choose z=1 (arbitrary)
z <- rep(1,sum(is.na(x)))
(y <- imputess(x,s$x0,s$C,z))

# did it work? (use a tolerance in checking to account for machine rounding)
# (FALSE means an edit is not violated)
any(violatedEdits(E,y,tol=1e-8))


# here's an example showing that solSpace only looks at missing values
# unless told otherwise.
Ey <- editmatrix(c(
    "yt == y1 + y2 + y3",
    "y4 == 0"))
y <- c(yt=10, y1=NA, y2=3, y3=7,y4=12)

# without further direction, y4 is left alone (although is is clearly wrong).
(s <- solSpace(Ey,y))
imputess(y, s$x0, s$C)

# by setting 'adapt' we can include y4 in the imputation
# (it does not matter how we set the value for y1, since is is empty, and
# it occurs in E it will be imputed)
(s <- solSpace(Ey,y,adapt=c(FALSE,FALSE,FALSE,FALSE,TRUE)))
imputess(y,s$x0,s$C)



# Find out which values can be deductively imputed with 0:
# (Example 9.2 of De Waal et al. (2011))
x <- t(dat)[,2]

I <- deductiveZeros(E,x)
I
# we may impute x deductively with
x[I] <- 0


#############################################
# IMPUTATION OF CATEGORICAL DATA
#############################################


# a simple example. We know the subject is pregnant. What is the gender?
E <- editarray(c(
"gender %in% c('male','female')",
"pregnant %in% c(TRUE,FALSE)",
"if ( gender=='male') !pregnant"))

x <- c(gender=NA,pregnant=TRUE)
(ximp <- deductiveLevels(E,x))

# imputation can be done as follows:
x[names(ximp)] <- ximp

# Here's an example from Katrika (2001) [but see De Waal et al (2011), ex. 9.3)]
E <- editarray(c(
    "D1 %in% as.character(1:4)",
    "D2 %in% as.character(1:3)",
    "D3 %in% as.character(1:3)",
    "D4 %in% as.character(1:2)",
    "if (D2 == '3'  & D3 != '3' & D4 == '1' ) FALSE",
    "if (D2 != '1'  & D4 == '2') FALSE",
    "if (D1 != '3'  & D2 != '2' & D3 != '1') FALSE",
    "if (D1 == '3'  & D3 != '1' & D4 == '1' ) FALSE"
))

#for ( f in F ) dmp <- source(f)
x <- c(D1='3',D2='2',D3=NA,D4=NA)
deductiveLevels(E,x)


# F <- dir('../pkg/R',full.names=TRUE)
# for ( f in F ) dmp <- source(f)
# library(editrules)








