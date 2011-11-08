#' Derive imputation values for categorical data
#'
#' Deduce imputation values for categorical data. By substituting all known
#' values and interatively eliminating the unknowns from the set of edits,
#' unique imputation values are derived where possible.
#' 
#' Imputation values are derived for missing variables (NA) and for variables indicated by 'adapt'.
#'
#' @references 
#' T. De Waal, J. Pannekoek and S. Scholtus (2011) Handbook of statistical data editing 
#' Chpt 9.2.1 - 9.2.2
#'
#'
#' @param E editarray
#' @param x a named \code{character} vector 
#' @param adapt boolean vector indicating which variables may be adapted.
#'
#' @return A named vector with imputation values for \code{x}
#'
#' @example ../examples/deductiveLevels.R
#' @export
deductiveLevels <- function(E, x, adapt=rep(FALSE,length(x)) ){
    if ( !is.editarray(E) ) stop('E must be object of class editarray')
    iM <-  (is.na(x) | adapt)  & names(x) %in% getVars(E)
    iN <- !(is.na(x) | adapt)  & names(x) %in% getVars(E)
    
    M  <- names(x)[iM]
    E0 <- reduce(editrules:::substValue.editarray(E,names(x)[iN],x[iN]))
    T <- c()
    nT <- 0
    nM <- length(M)
    xi <- character(0)

    while ( nT < nM  ){
        E1 <- E0

        for ( m in setdiff(M,T)[-1] ) E1 <- reduce(editrules:::eliminate.editarray(E1,m))
        val <- colSums(!editrules:::getArr(E1)) > 0
        if( nrow(E1) > 0 && sum(val) == 1 ){ #deductive imputation possible
            ind <- editrules:::getInd(E1)
            variable <- names(ind)
            level <- names(ind[[1]][val])
            xi[variable] <- level
            E0 <- reduce(editrules:::substValue.editarray(E0,variable,level))
            M <- M[-1]
        } else {
            T <- c(T,M[1])
        }
        nT <- nT + 1
    }

    xi
}






