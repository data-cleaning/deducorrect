#' Based only equality rules, impute as many values as possible.
#'
#' Given (equality) rules and a number of values to impute or adapt, in some cases
#' unique solutions can be derived. The solution space is derived as described in
#' De Waal et al. (2011). See also the package vignette (TODO).
#'
#'
#' @references 
#' T. De Waal, J. Pannekoek and S. Scholtus (2011) Handbook of statistical data editing 
#' Chpt 9.2.1
#'
#' @param E an editmatrix
#' @param dat a data.frame
#' @param adapt a boolean array of dim(dat), e.g. the result editrules::localizeErrors(E,dat)
#' @param ... parameters to pass to \code{\link{solSpace.matrix}}. 
#' @example ../examples/impute.R
#' 
#' @export
deduImpute <- function(E, dat, adapt=NULL,...){
    X <- t(dat)
    vars <- getVars(E)
    a <- logical(length(vars))
    Xi <- array(0,dim=c(length(vars),ncol(X)))
    for ( i in 1:ncol(X) ){
        x <- X[vars,i]
        if ( !is.null(adapt) ) a <- adapt[i,vars]
        s <- solSpace(E, x, adapt=a, ...)
        u <- rowSums(abs(s$C)) == 0
        x[is.na(x) | a][u] <- s$x0[u]
        Xi[,i] <- x
    }
    X[vars,] <- Xi
    as.data.frame(t(X))
}






