#' Deductive imputation of numerical or categorical values
#'
#' Based on observed values and edit rules, impute as many variables deductively as possible.
#'
#' @references 
#' T. De Waal, J. Pannekoek and S. Scholtus (2011) Handbook of statistical data editing 
#' Chpt 9.2.1 - 9.2.2
#'
#' @param E An \code{editmatrix} or \code{editarray}
#' @param dat A \code{data.frame}
#' @param adapt (optional) A boolean array of dim(dat), e.g. the result editrules::localizeErrors(E,dat). 
#'      Column names must match those of \code{dat}.
#' @param ... arguments to be passed to \code{\link{solSpace}} (numerical data) or \code{\link{deductiveLevels}} (categorical data)
#'
#' @return A \code{\link{deducorrect-object}}
#'
#' @seealso \code{\link{deductiveZeros}}, \code{\link{solSpace}}, \code{\link{deductiveLevels}}
#'
#' @example ../examples/deduImpute.R
#' @export
deduImpute <- function(E, dat, adapt=NULL, ...){
    UseMethod('deduImpute')
}


#' Deductive imputation of categorical data
#'
#' \bold{For categorical data:} The funcion \code{\link{deductiveLevels}} is used to derive
#' deductive imputations for as many fields as possible
#'
#'
#' @method deduImpute editarray
#' @rdname deduImpute
#' @export
deduImpute.editarray <- function(E, dat, adapt=NULL, ...){
   
    vars <- getVars(E)
    if ( is.null(adapt) ){
        a <- logical(length(vars))
        nCandidates <- rowSums(is.na(dat[,vars,drop=FALSE]))    
    } else {
        nCandidates <- rowSums(is.na(dat[,vars,drop=FALSE]) | adapt)
    }

    nImp <- numeric(nrow(dat))
    X <- t(dat[,vars,drop=FALSE])
    imp <- vector(mode='list',length=nrow(dat))
    for ( i in 1:ncol(X) ){
        x <- X[ ,i]
        if ( !is.null(adapt) ) a <- adapt[i,vars]
        L <- deductiveLevels(E,x,adapt=a)
        X[names(L),i] <- L
        imp[[i]] <- L
    }
    dat[,vars] <- t(X)

    nImp <- sapply(imp,length)
    # derive deducorrect object
    stat <- status(nrow(dat))
    stat[ nCandidates == 0] <- 'valid'
    stat[ nImp > 0 & nImp < nCandidates] <- 'partial'
    stat[ nImp == nCandidates & nCandidates > 0] <- 'corrected'
    stat[ nImp == 0 & nCandidates > 0 ] <- 'invalid'
    # corrections
    
    xi <- do.call(c,imp)

    corrections <- data.frame(
        row = rep(1:nrow(dat),times=nImp),
        variable = names(xi),
        old = rep(NA,length(xi)), #TODO take old value!
        new =  xi)
    newdeducorrect(
        corrected = dat,
        corrections = corrections,
        status = data.frame(status=stat,imputations=nImp)
    )
}






#' Based only equality rules, impute as many values as possible.
#'
#' \bold{For numerical data:} Given (equality) rules and a number of values to impute or adapt, in some cases
#' unique solutions can be derived. This function uses \code{\link{solSpace}} and
#' \code{\link{deductiveZeros}} (iteratively) to determine which values can be imputed
#' deductively. Solutions causing new violations of (in)equality rules are rejected by default by testing
#' if the observed values can lead to a feasible record. This may be switched off by passing
#' \code{checkFeasibility=FALSE}. This may be desirable for performance reasons. If \code{adapt}
#' was computed with an error localization algorithm, such as \code{editrules::localizeErrors}, the 
#' feasibility check is also not nessecary.
#'
#'
#' @method deduImpute editmatrix
#'
#' @param tol tolerance to use in \code{\link{solSpace}} 
#'      and in \code{\link{deductiveZeros}} 
#'
#' @rdname deduImpute
#' @export 
deduImpute.editmatrix <- function(E, dat, adapt=NULL, tol=sqrt(.Machine$double.eps),...){

    X <- t(dat)
    vars <- getVars(E)
    a <- logical(length(vars))
    Xi <- array(0,dim=c(length(vars),ncol(X)))
    for ( i in 1:ncol(X) ){
        x <- X[vars,i]
        nMiss <- sum(is.na(x)) + 1
        if ( !is.null(adapt) ) a <- adapt[i,vars]

        while( sum(is.na(x)) < nMiss ){
            nMiss <- sum(is.na(x))
            I <- deductiveZeros(E,x)
            if ( any(I) ) x[I] <- 0
            s <- solSpace(E, x, adapt=a, tol=tol, ...)
            if ( !is.null(s) ){
                u <- rowSums(abs(s$C)) == 0
                x[rownames(s$x0)[u]] <- s$x0[u]
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
# TODO: keep track of 'adapt'
    npre <- rowSums(is.na(dat))
    npost <- rowSums(is.na(dd))

    nImp <- npre - npost
    stat <- status(nrow(dat))
    stat[npre  == 0 ]            <- 'valid'
    stat[npost == 0 & npre > 0]  <- 'corrected'
    stat[0 < nImp   & nImp < npre ] <- 'partial'
    stat[npre==npost & npre > 0] <- 'invalid'



    newdeducorrect(
        corrected   = dd,
        corrections=corrections,    
        status = data.frame(status=stat, imputations=nImp)
    )
}






