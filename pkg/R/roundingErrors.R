#' Scapegoat algorithm
#'
#' @nord
scapegoat <- function(R0, x,krit=NULL) {
	r0 <- nrow(R0)
    v <- ncol(R0)
    
    if (v < r0){
       stop("...")
    }
    
    if (!is.null(krit)){
        krit <- colnames(R0) %in% krit
    }
    else krit <- logical(ncol(R0))
    
    if ((k <- sum(krit))==0){
        perm <- c(sample(which(!krit)))
    } 
    else if (k==v){
       perm <- c(sample(which(krit)))
    }
    else {
       perm <- c(sample(which(!krit)), sample(which(krit)))
    }
    
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
    
	b <- -R2 %*% x2
	x1 <- solve(R1, b)[,1]
	sol <- c(x1, x2)
    
    #restore original order
    m <- match(names(x), names(sol))
    sol[m]
}

#' Correct rounding errors Scholtus (2009)
#'
#' Try to find and correct for rounding errors.
#' @nord
#' @param R editmatrix \eqn{Rx = a}
#' @param Q editmatrix \eqn{Qx => b}
#' @param dat \code{data.frame} with the data to be corrected
#' @param delta tolerance on checking for rounding error
#' @param K number of trials
#' @param round \code{logical}, should the solution be a rounded?
#' @return list with....
roundingErrors <- function(R, Q, dat, delta=2, K=10, round=TRUE){
   stopifnot(is.editmatrix(R), is.data.frame(dat))
   krit <- character(0)
   
   if (!missing(Q)){
     stopifnot(is.editmatrix(Q))
     krit <- colnames(Q)
   }   
   
   m <- as.matrix(dat[colnames(R)])
   n <- nrow(m)
   status <- factor(integer(n), levels=c("valid", "corrected", "partial","invalid"), ordered=TRUE)
   
   corrections <- NULL
   
   for (i in 1:n){
      x <- m[i,]
      #TODO add C to R matrix
      E0 <- abs(R %*% x) <= delta
      R0 <- R[E0,,drop=FALSE]
      
      if (all((R0 %*% x) == 0)){
         status[i] <- if (all(E0)) "valid"
                      else "invalid"
         next
      }
      
      k <- 0
      while (k <- K){
        k <- k + 1
        sol <- scapegoat(R0, x, krit)
        if (round) 
            sol <- round(sol,0)
        #TODO check Qx >= b
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
      
   }
   
   corrected <- dat
   corrected[colnames(m)] <- as.data.frame(m)[]
   
   list( status=status
       , corrected=corrected
       , corrections=as.data.frame(corrections)
       )
}