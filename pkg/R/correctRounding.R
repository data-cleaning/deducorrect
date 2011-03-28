resample <- function(x, ...) {
   if (length(x) <= 1) x
   else sample(x)
}

#' Scapegoat algorithm
#'
#' @nord
scapegoat <- function(R0, a0, x,krit=NULL) {
	  r0 <- nrow(R0)
    v <- ncol(R0)
    
    if (v < r0){
       return(x)
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

#' Correct records under linear restrictions for rounding errors
#'
#' This algorithm tries to detect and repair records that violate linear (in)equality constraints by correcting possible rounding errors as described by Scholtus(2008).
#' Typically data is constrainted by \eqn{Rx=a} and \eqn{Qx \ge b}.
#' 
#' The algorithm first finds violated constraints
#' \eqn{|r'_{i}x-a_i| > 0} , and selects edits that may be due to a rounding error \eqn{0 < |r'_{i}x-a_i| \leq \delta}. 
#' The algorithm then makes a correction suggestion where the errors are attributed to randomly selected variables under the lineair equality constraints. 
#' It checks if the suggested correction 
#' does not violate the inequality matrix \eqn{Q}. If it does, it will try to generate a different solution up till \code{K} times.
#'
#' @export
#' @example examples/correctRounding.R
#' @references 
#' Scholtus S (2008). Algorithms for correcting some obvious
#' inconsistencies and rounding errors in business survey data. Technical
#' Report 08015, Statistics Netherlands.
#'
#' @param R editmatrix \eqn{Rx = a}
#' @param dat \code{data.frame} with the data to be corrected
#' @param Q *deprecated* Inequalities can be entered via the first argument
#' @param delta tolerance on checking for rounding error
#' @param K number of trials per record. See details
#' @param round should the solution be a rounded, default TRUE
#' @return A \code{\link[=deducorrect-object]{deducorrrect}} object.
#' @seealso \code{\link{deducorrect-object}} \code{\link{status}}
#'
#'
correctRounding <- function(R, dat, Q = NULL, delta=2, K=10, round=TRUE){
   stopifnot(is.editmatrix(R), is.data.frame(dat))
   #TODO add fixate
   krit <- character(0)
 
   if (!missing(Q)){
     stopifnot(is.editmatrix(Q))
     krit <- colnames(Q)
     b <- getC(Q)
   }
   else {
     q <- getOps(R) == ">=" 
     if (any(q)){
       Q <- as.editmatrix(R[q,,drop=FALSE])
       krit <- colnames(Q)
       b <- getC(Q)
     }
   }
   if ( ! is.null(Q) ) v <- violatedEdits(Q,dat)

   eq <- getOps(R) == "=="
   if (!all(eq)){
      R <- as.editmatrix(R[eq,,drop=FALSE])
   }
   
   m <- as.matrix(dat[,getVars(R),drop=FALSE])
   n <- nrow(m)
   status <- status(n)
   attempts <- integer(n)
   
   corrections <- NULL
   a <- getC(R)
   cc <- which(complete.cases(m))
   for (i in cc){
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
        if ( R0 %*% sol == a0
          && (  is.null(Q) 
             || all(which(violatedEdits(Q,as.data.frame(t(sol)))) %in% which(v[i,]))
             )
           ){
           break
        }
      }
      
      if (k < K){
        # detect if vars are changed
        vars <- which((sol-x) != 0)
        m[i,] <- sol
        cor <- cbind( row=i
                    , variable=colnames(R)[vars]
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
    
   if ( is.null(corrections) ){
      corrections <- data.frame(
            row=integer(0), 
            variable=factor(levels=colnames(R)),
            old=numeric(0), new=numeric(0)
        )   
   }
   return(
      newdeducorrect(
         corrected   = corrected, 
         corrections = as.data.frame(corrections), 
         status      = data.frame(status=status, attempts=attempts)
      )
   )
}
