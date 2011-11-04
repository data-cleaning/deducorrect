#' Based only equality rules, impute as many values as possible.
#'
#' Given (equality) rules and a number of values to impute or adapt, in some cases
#' unique solutions can be derived. This function uses \code{\link{solSpace}} and
#' \code{\link{deductiveZeros}} (iteratively) to determine which values can be imputed
#' deductively. Solutions which causing new violations of inequality rules are rejected.
#'
#' @references 
#' T. De Waal, J. Pannekoek and S. Scholtus (2011) Handbook of statistical data editing 
#' Chpt 9.2.1 - 9.2.2
#'
#' @param E an editmatrix
#' @param dat a data.frame
#' @param adapt a boolean array of dim(dat), e.g. the result editrules::localizeErrors(E,dat)
#' @param tol tolerance to use in \code{violatedEdits} (if checkConsistency=TRUE), in \code{\link{solSpace}} 
#'      and in \code{\link{deductiveZeros}} 
#' @param checkConsistency if the imputed values cause any new violations in inequalities? Use this 
#'      when the assumption that all nonmissings are correct cannot be justified. Turn off when passing
#'      an \code{adapt} vector, determined by error localization (e.g. with editrules::localizeErrors).
#' @param ... parameters to pass to \code{\link{solSpace.matrix}}. 
#' @example ../examples/deduImpute.R
#' 
#' @export
deduImpute <- function(E, dat, adapt=NULL, tol=sqrt(.Machine$double.eps), checkConsistency=TRUE,...){

    V <- rowSums(is.na(dat))
    X <- t(dat)
    vars <- getVars(E)
    a <- logical(length(vars))
    Xi <- array(0,dim=c(length(vars),ncol(X)))
    for ( i in 1:ncol(X) ){
        x <- X[vars,i]
        nMiss <- sum(is.na(x)) + 1
        if ( !is.null(adapt) ) a <- adapt[i,vars]
        if ( checkConsistency ) ve <-which(violatedEdits(E,x,tol=tol))

        while( sum(is.na(x)) < nMiss ){
            nMiss <- sum(is.na(x))
            I <- deductiveZeros(E,x)
            if ( any(I) ) x[I] <- 0
            s <- solSpace(E, x, adapt=a, tol=tol, ...)
            if ( !is.null(s) ){
                u <- rowSums(abs(s$C)) == 0
                if ( checkConsistency ){
                    xi <- x
                    xi[rownames(s$x0)[u]] <- s$x0[u]
                    vx <- which(violatedEdits(E, xi, tol=tol))
                    
                    if ( length(setdiff(vx,ve)) == 0 ) x <- xi
                } else {
                    x[rownames(s$x0)[u]] <- s$x0[u]
                }
            }
        }
        Xi[,i] <- x
    }

    ii <- which(is.na(X[vars,]) & !is.na(Xi))

    corrections <- data.frame(
            row     = (ii-1) %/% nrow(Xi) + 1,   
            variable= vars[(ii-1) %% nrow(Xi) + 1],
            old     = rep(NA,length(ii)),
            new     = Xi[ii]
    )

    X[vars,] <- Xi
    dd <- as.data.frame(t(X))

    npre <- rowSums(is.na(dat))
    npost <- rowSums(is.na(dd))

    nImp <- npre - npost
    stat <- status(length(nImp))
    stat[npre  == 0 ]              <- 'valid'
    stat[npost == 0 & npre > 0]    <- 'corrected'
    stat[0 < nImp   & nImp < V ]     <- 'partial'
    stat[npre==npost & npre > 0]       <- 'invalid'



    newdeducorrect(
        corrected   = dd,
        corrections=corrections,    
        status = data.frame(status=stat, imputations=nImp)
    )
}






