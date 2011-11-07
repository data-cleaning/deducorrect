#' Deductively impute categorical data.
#'
#'
#' @param E editarray
#' @param x a named \code{character} vector 
#' @export
deductiveLevels <- function(E,x){
    iM <- is.na(x) & names(x) %in% getVars(E)
    iN <- !is.na(x) & names(x) %in% getVars(E)
    
    M  <- names(x)[iM]
    E0 <- reduce(substValue(E,names(x)[iN],x[iN]))
    treated <- logical(length(M))
    names(treated) <- M
    T <- c()
    nT <- 0
    nM <- length(M)
    xi <- c()

    while ( nT < nM  ){
        E1 <- E0
        for ( m in setdiff(M[-1],T) ) E1 <- reduce(editrules:::eliminate.editarray(E1,m))
        
        val <- colSums(!editrules:::getArr(E1)) > 0
        if( nrow(E1) > 0 && sum(val) == 1 ){ #deductive imputation possible
            ind <- editrules:::getInd(E1)
            xi[names(ind)] <- names(ind[[1]])[val]
            M <- M[-1]
        } else {
            T <- c(T,M[1])
        }
        nT <- nT + 1
    }

    xi
}






