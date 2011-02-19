
#' Try to solve balance edit violations by sign changes
#' 
#' This is the workhorse function for \code{\link{correctSigns}}. It performs
#' a breadth-first tree search to resolve (near) equality edit violations.
#' The solution(s), if any, with as little as possible sign changes are returned.
#'
#'
#' @param A The \code{matrix} part of an \code{editmatrix}
#' @param r A numerical record.
#' @param adapt A logical vector of length \code{ncol(A)} indicating which variables may be changed.
#' @param maxSigns how many signs may maximally be flipped? 
#' @param eps Tolerance for equality restriction checking
#' 
#' @return A \code{list} with vectors of length \code{r} with coefficients 
#'      in \eqn{\{-1,1\}}. Empty \code{list} if no solution is found.
#'
getSignCorrection <- function(A, r, adapt, maxSigns, eps){

    v <- rep(1,length(r))
    if (all(abs(A%*%r) <= eps)) return(list(v))
    adapt <- which(adapt)
    nViolated <- length(adapt)
    S <- list()
    j <- 1
    for ( nsigns in 1:min(maxSigns, max(nViolated%/%2,1)) ){
        I <- combn(1:nViolated, nsigns)
        for ( i in 1:ncol(I) ){
           s <- v
           s[adapt[I[, i]]] <- -1
           if ( all(abs(A %*% (r*s)) <= eps) ){
                S[[j]] <- s
                j <- j+1
           }
        }
        if ( length(S) > 0 ) break
    }
    return(S)
}

#' Try to resolve linear edits by adapting signs
#'
#' This algorithm tries to repair records that violate linear equality constraints by
#' switching signs or swapping variable values. The option to swap variables is only meaningfull
#' when they have oposite signs in the equality restriction. For example if \eqn{x=y-z}
#' is violated, one could try to swap the values of \eqn{y} and \eqn{z}, which is equal to
#' changing the signs of y and z. The user has to indicate explicitly which variable pairs
#' may be switched. 
#'
#' The algorithm searches for the least possible number of sign switches which lead to a solution.
#' If multiple solutions with the same number of sign switches are found, the solution with the
#' minimum weight (computed by summing over the reliability weights of the variables to be
#' switched) is chosen. If there are still more than one, the first one is chosen. A vector
#' with the number of degenerate solutions (if any) is returned.
#'
#' 
#' @param E An object of class \code{editmatrix} 
#' @param dat The data to correct
#' @param maxSigns Maximum number of signs that may be changed. Defaults to \code{ncol(E)} modulo 2.
#' @param eps Tolerance on edit check. Defaults to \code{sqrt(.Machine.double.eps)}
#' @param weight Positive numeric vector of length ncol(E). Variables with heigher 
#'      reliability weight are less likely to be changed. Defaults to \code{rep(1,ncol(E))}
#' @param fix character vector. Names of variables which may not be changed.
#' @param swap \code{list} of 2-vectors containing pairs of swappable variable names.
#'
#' @return a list containing repaired data and a vector of length \code{nrow(dat)} denoting 
#'      the number of degenerate solutions found.
#'
#' \tabular{ll}{
#' value \tab meaning \cr
#' 0     \tab record contains an error that cannot be repaired by sign corrections \cr
#' n     \tab there were $n$ solutions with equal reliability weights. The first one was chosen.\cr
#' }
#' @example examples/correctSigns.R
#' 
#' @export
correctSigns <- function(
    E, 
    dat,
    maxSigns = floor(ncol(E)/2),
    eps=sqrt(.Machine$double.eps),
    weight = rep(1,ncol(E)),
    fix = NA,
    swap = NA ){
#TODO check if swap-pairs have opposite signs in edits.
    # Flip signs and swaps variables if allowed. Register swaps. 
    swappit <- function(sw){
        if ( all(s[sw]==-1) || any(abs(r[sw]) < eps) & any(s[sw] == -1) ){
            r[sw] <<- r[sw[2:1]]
            s[sw] <<- 1
        }
    }

    # which variables may be changed?
    notFixed <- rep(TRUE,ncol(E))
    if ( !identical(fix, NA) ){
        notFixed <- !(colnames(E) %in% fix)
    }

    # swap-names to swap-indices
    haveSwaps <- FALSE
    if (!identical(swap, NA)){
        iSwap <- lapply(swap, function(sw) which(names(dat) %in% sw) )
        haveSwaps <- TRUE
    }

    cn <- colnames(E)
    D <- as.matrix(dat[ ,cn])
    A <- as.matrix(E)
    w <- matrix(weight,nrow=1)
    degeneracy <- integer(nrow(dat))
    for ( i in 1:nrow(dat) ){
        r <- D[i,]
        violated <- abs(A %*% r) > eps            
        adapt <- notFixed & colSums(abs(E[violated, ,drop=FALSE])) > 0
        S <- getSignCorrection(A, r, adapt, maxSigns, eps)
        if ( length(S) > 0 ){
            wvec <- w %*% (sapply(S,matrix) < 0)
            s <- S[[which.max(wvec)]]
            degeneracy[i] <- sum(wvec==max(wvec))
            if ( haveSwaps ) lapply(iSwap, swappit)
            D[i, ] <- r*s
        }
    }
    dat[,cn] <- D
    return(list(data=dat, degeneracy=degeneracy))
}






