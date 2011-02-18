#' Check wether a matrix is totally unimodular.
#'
#'
#' A matrix for which the determinant of every square submatrix is -1, 0 or 1
#' is called _totally unimodular_. This function tests if a matrix with 
#' coefficients in \eqn{\{-1,0,1\}} is unimodular. It tries to reduce the matrix
#' using the reduction method described in Scholtus (2008). Next, a test based
#' on Heller and Tompkins (1956) or Raghavachari is performed.
#'
#' @title Total unimodularity of a -1, 0, 1 matrix.
#'
#' @param A An object of class \code{\link{matrix}} in \eqn{\{-1,0,n\}^{n\times m}}
#' @return logical
#' 
#' @example ../../examples/unimodular.R
#'
#'
#' @seealso \code{\link{reducematrix}}
#' @export
#' @references
#' @cite scholtus:2008
#' @cite raghavachari:1976
#' @cite heller:1956
isTotallyUnimodular <- function(A) {
    
    # A matrix with elements not in {-1,0,1} cannot be totally unimodular.
    if ( !all(A %in% c(-1,0,1)) ){
        value <- FALSE
    }

    # After reduction, A has no rows or columns containing less than 2 elements.
    A <- reduceMatrix(A)
    if ( length(A) == 0 ){ # if reduced to nothingness, we are ready.
        value <- TRUE
    # HT-criterium, by rows or columns
    } else if (max(colSums(abs(A))) == 2){ 
        value <- hellerTompkins(A)
    } else if (max(rowSums(abs(A))) == 2){
        value <- hellerTompkins(t(A))
    # raghavachari criterium recurses over columns. Minimize effort by
    # transposition when possible.    
    } else {                        
        if ( nrow(A) >= ncol(A) ){ 
            value <- raghavachari(A)
        } else {
            value <- raghavachari(t(A))
        }
    }
    return(value)
}


#' Apply reduction method from Scholtus (2008)
#'
#' Apply the reduction method in the appendix of Scholtus (2008) to a matrix.
#' Let \eqn{A} be matrix containing only -1, 0 and 1 
#' as coefficients. If, after a possible permutation of columns it can be written 
#' in the form \eqn{A=[B,C]} where each column in \eqn{B} has at most 1 nonzero
#' element, the \eqn{A} is totally unimodular if and only if \eqn{C} is totally
#' unimodular. By transposition, a similar theorem holds for the rows of A. This
#' function iteratively removes rows and columns with only 1 nonzero element
#' from \eqn{A} and returns the reduced result.
#'
#' @param A An object of class \code{matrix}, in \eqn{\{-1,0,1}^{n\times m}}
#' @return 
#'
#' @example ../../examples/reduceMatrix.R
#'
#' @references
#' @cite scholtus:2008
reduceMatrix <- function(A){
    d1 <- c(0,0)
    d <- dim(A)
    while ( !all(d1==d) ){
        A <- A[, colSums(abs(A)) >= 2,drop=FALSE]
        A <- A[rowSums(abs(A)) >= 2, ,drop=FALSE]
        d1 <- d
        d <- dim(A)
    }
    return(A)
}

#' Determine if a matrix is totally unimodular using Heller and Tompkins criterium.
#'
#' @param A An object of class Matrix in \eqn{\-1,0,1\}^{m\times n}}. Each column 
#'      must have exactly 2 nonzero elements. (This is tested by 
#'      \code{\link{isTotallyUnimodular}}).
#'
#' @return \code{TRUE} if matrix is unimodular, otherwise \code{FALSE}
#'
#' @references
#' @cite heller:1956
hellerTompkins <- function(A){
    # If the matrix has columns with two elements, and those elements differ in
    # sign for all those columns, the matrix is unimodular. 
    if ( !any(abs(colSums(A))==2) ){ 
        return(TRUE)
    }
    # Loop over ways to split a matrix in 2 by rows.
    # Return TRUE when HT criterium is met, FALSE otherwise.
    for ( m in 1:(nrow(A)%/%2) ){
        I <- combn(1:nrow(A), m)
        for ( i in 1:ncol(I)){
            M1 <- A[I[ , i], ,drop=FALSE]
            M2 <- A[-I[ , i], ,drop=FALSE]
            if ( !any(abs(colSums(M1)) == 2) && !any(abs(colSums(M2)) == 2) ){
                return(TRUE)
            }
        }
    }
    return(FALSE)
}

#' Test if a list of matrices are all unimodular
#'
#' Helper function for \code{\link{raghavachani}}
#' @param L A list of objects of class matrix.
#' @return logical vector of length \code{length(L)}
allTotallyUnimodular <- function(L){
    for ( i in 1:length(L) ){
        if ( !isTotallyUnimodular(L[[i]]) ){
            return(FALSE)
        }
    }
    return(TRUE)
}

#' Determine if a matrix is unimodular using recursive Raghavachari criterium
#' 
#' @param A An object of class Matrix in \eqn{\-1,0,1\}^{m\times n}}. 
#' @return \code{TRUE} or \code{FALSE}
#' @references
#' @cite raghavachari:1976  
raghavachari <- function(A){
    if ( ncol(A) == 1 ){
        return(TRUE)
    } else {
        L <- vector(mode="list", length=ncol(A))
        for ( i in 1:ncol(A) ){
            L[[i]] <- A[ ,-i, drop=FALSE]
        }
        return(allTotallyUnimodular(L))
    }
}
    

