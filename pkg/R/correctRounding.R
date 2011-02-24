resample <- function(x, ...) {
   x[sample.int(length(x), ...)]
}

#' Scapegoat algorithm
#'
#' @nord
scapegoat <- function(R0, a0, x,krit=NULL) {
	 r0 <- nrow(R0)
    v <- ncol(R0)
    
    if (v < r0){
       stop("...")
    }
    
    if (!is.null(krit)){
        krit <- colnames(R0) %in% krit
    }
    else krit <- logical(ncol(R0))
    p <- 1:v
    perm <- c(resample(p[!krit]),resample(p[krit]))

    R0t <- R0[, perm, drop=FALSE]
    xt <- x[perm]
    
    #TODO check if R0 is totalunimodular, if so, then QR decomposition isn't necessary
	 ks <- qr(R0t)$pivot;
    p1 <- ks[1:r0]
    p2 <- ks[(r0+1):v]
    
    R1 <- R0t[,p1, drop=FALSE]
    #x1 <- x[p1]
    R2 <- R0t[,p2, drop=FALSE]
    x2 <- xt[p2]
    
	 c <- a0 - (R2 %*% x2)
    x1 <- solve(R1, c)[,1]
	 sol <- c(x1, x2)
    #restore original order
    m <- match(names(x), names(sol))
    sol[m]
}

#' Correct rounding errors Scholtus (2009)
#'
#' Try to find and correct for rounding errors.
#'
#'
#' @example examples/correctRounding.R
#'
#' @export
#' @param R editmatrix \eqn{Rx = a}
#' @param dat \code{data.frame} with the data to be corrected
#' @param Q optional editmatrix \eqn{Qx => b}
#' @param delta tolerance on checking for rounding error
#' @param K number of trials per records see details
#' @param round should the solution be a rounded, default TRUE
#' @return list with....
correctRounding <- function(R, dat, Q = NULL, delta=2, K=10, round=TRUE){
   stopifnot(is.editmatrix(R), is.data.frame(dat))
   krit <- character(0)
   
   if (!missing(Q)){
     stopifnot(is.editmatrix(Q))
     krit <- colnames(Q)
     b <- getC(Q)
   }
   
   m <- as.matrix(dat[getVars(R)])
   n <- nrow(m)
   status <- status(n)
   attempts <- integer(n)
   
   corrections <- NULL
   a <- getC(R)
   cc <- which(complete.cases(m))
   for (i in 1:n){
      x <- m[i,]
      E0 <- abs(a - (R %*% x)) <= delta
      R0 <- R[E0,,drop=FALSE]
      a0 <- a[E0]
      
      if (all((R0 %*% x) == a0)){
         status[i] <- if (all(E0)) "valid"
                      else "invalid"
         next
      }
      k <- 0
      while (k < K){
        k <- k + 1
        sol <- scapegoat(R0, a0, x, krit)
        if (round) 
            sol <- round(sol,0)
        #TODO make this step more generic (so Q can be any inequality matrix)
        if ( is.null(Q) 
          || Q %*% x >= b
           ){
           break
        }
      }
      
      if (k < K){
        # detect if vars are changed
        vars <- which((sol-x) != 0)
        m[i,] <- sol
        cor <- cbind( row=i
                    , var=colnames(R)[vars]
                    , old=x[vars]
                    , new=sol[vars]
                    )
        corrections <- rbind(corrections, cor)
        status[i] <- if (all(E0)) "corrected"
                     else "partial"
      }
      else {
        status[i] <- "invalid"
      }
      attempts[i] <- k
   }
   
   corrected <- dat
   corrected[colnames(m)] <- as.data.frame(m)[]
   
   list( status=data.frame(status=status, attempts=attempts)
       , corrected=corrected
       , corrections=as.data.frame(corrections)
       )
}
