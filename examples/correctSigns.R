
require(editrules)

# some data 
dat <- data.frame(
    x = c( 3,14,15),
    y = c(13,-4, 5),
    z = c(10,10,-10))
# ... which has to obey
E <- editmatrix("z == x-y")

# All signs may be flipped, no swaps.
correctSigns(E, dat)

# fix z, flip everything else
correctSigns(E, dat,fix="z")

# the same result is achieved with
correctSigns(E, dat, flip=c("x","y"))

# make x and y swappable, if both x and y are flipped, it is interpreted as a swap.
correctSigns(E, dat,flip=c(), swap=list(c("x","y")))

# make x and y swappable, swap a counts as one flip
correctSigns(E, dat, flip="z", swap=list(c("x","y")), swapIsOneFlip=TRUE)

# make x and y swappable, swap counts as one flip, flipping z gets higher penalty
correctSigns(E, dat, flip="z", swap=list(c("x","y")), swapIsOneFlip=TRUE, weight=c(2,1))















   
   
