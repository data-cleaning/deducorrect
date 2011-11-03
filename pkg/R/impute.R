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
#' @param ... parameters to pass to \code{\link{solSpace}}. 
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



#' Solution space for missing values under equality constraints
#'
#' Consider a record x subject to linear restrictions \eqn{Ax = b}. If x has
#' missing values, we may write \eqn{x = (x_{obs}, x_{mis})}. The solution space
#' for \eqn{x_{miss}} can be written as \eqn{x_{miss} = x_0 + Cz}, where \eqn{x_0}
#' is a constant vector, \eqn{C} a constant matrix and \eqn{z} any real vector. 
#' This function computes \eqn{x_0} and \eqn{C}. 
#'
#' If one or more rows of \eqn{C} equal 0, then the solution for that variable is 
#' unique. 
#'
#' @references 
#' T. De Waal, J. Pannekoek and S. Scholtus (2011) Handbook of statistical data editing 
#' Chpt 9.2.1
#'
#' @param E editmatrix
#' @param x a named numeric vector. Every name in x must occur as a variable in E.
#' @param adapt A named logical vector with variables in the same order as in x
#' @param tol tolerance used to determine 0-singular values when determining generalized inverse and to round coefficients of C to zero.
#' @return A \code{list} with elements \eqn{x0} and \eqn{C}
#'
#' @example ../examples/impute.R
#' @export
solSpace <- function(E, x, adapt=logical(length(x)), tol=sqrt(.Machine$double.eps)){
    eq <- getOps(E) == '=='
    m <- is.na(x) | adapt
    
    v <- match(getVars(E),names(x))
    

    A <- getA(E[eq,])[,v,drop=FALSE]
    b <- getb(E[eq,])
    Amis <- A[,m,drop=FALSE]
    Aobs <- A[,!m,drop=FALSE]
    Ainv <- ginv(Amis)
    x0  <- Ainv%*%(b-Aobs%*%x[!m])
    rownames(x0) <-  names(x)[m]
    C         <-  Ainv%*%Amis - diag(1,nrow(Ainv))
    C[abs(C) < sqrt(.Machine$double.eps)] <- 0
    dimnames(C) <- list(names(x)[m],names(x)[m])
   
    list(x0=x0, C=C)
}



# Generalized matrix inverse. Code copied from MASS:::ginv 
#
#
ginv <- function (X, tol = sqrt(.Machine$double.eps)) {
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X)) 
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    if (all(Positive)) 
        Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
        t(Xsvd$u[, Positive, drop = FALSE]))
}




