


# reduces to 0x0 matrix
A <- matrix(c(
    1,-1, 0, 0,
    1, 0, 0, 0,
    0, 0, 1, 0,
    0, 1, 1, 1,
    0, 0, 0, 1),
    nrow=5, byrow=TRUE)
 reduceMatrix(A)

# does not reduce
A <- matrix(c(
    1,1,0,0,
    0,0,1,1,
    1,0,1,0,
    0,1,0,1),
    nrow=4, byrow=TRUE)
reduceMatrix(A)


