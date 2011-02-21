
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
#' @param w positive weight vector of \code{length(ncol(A))}
#' 
#' @return A \code{list} with vectors of length \code{r} with coefficients 
#'      in \eqn{\{-1,1\}}. Empty \code{list} if no solution is found.
#'
getSignCorrection <- function(A, r, adapt, maxSigns, eps, w){

    v <- rep(1,length(r))
    if (all(abs(A%*%r) <= eps)) return(list(v))
    adapt <- which(adapt)
    nViolated <- length(adapt)
    S <- list()
    weights <- NA 
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
        if ( length(S) > 0 ){ 
            weights <- w %*% (sapply(S,matrix) < 0)
            break
        } 
    }
    return(list(signs=S, weights=weights))
}

#' Try to solve balance edit violations by sign changes and/or variable swaps.
#'
#' @param A The \code{matrix} part of an \code{editmatrix}
#' @param r A numerical record.
#' @param flip vector of  indices (integers) in r who's values may be swapped.
#' @param swap nx2 array of index combinations of variables wich may be swapped.
#' @param eps Tolerance for equality restriction checking
#' @param w weights vector of length length(flip)+nrow(swap), w[1:lenght(flip)] are penalties for changing variables 
#'      corresponding to indices in \code{flip}, the remaining entries are penalties for the value interchanges in the
#'      rows of \code{swap}.
#' @return A \code{list} with vectors of length \code{r} with coefficients 
#'      in \eqn{\{-1,1\}}. Empty \code{list} if no solution is found.
#'
flipAndSwap <- function(A, r, flip, swap, eps, w){
    II <- rbind(cbind(flip,NA),swap)
   
    v <- rep(1,ncol(A))
    if (all(abs(A%*%r) <= eps)) return(list(v))
    
    S <- list()
    weights <- NA
    j <- 1
    for ( nsigns in 1:(nrow(II))){    
        I <- combn(1:nrow(II), nsigns)
        for ( i in 1:ncol(I) ){
           s <- v
           k <- as.numeric(II[I[,j],])
           k <- k[!is.na(k)]
           s[k] <- -1
           if ( all(abs(A %*% (r*s)) <= eps) ){
                S[[j]] <- s
                j <- j+1
           }
        }
        if ( length(S) > 0 ){ 
            weights <- w %*% (sapply(S,matrix) < 0)
            break
        } 
    }
    return(list(signs=S, weights=weights))
}


#' Try to resolve linear edits by adapting signs
#'
#' TODO edit DESCRIPTION after adding flipAndSwap
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
#' @param maxSigns Maximum number of signs that may be changed. Defaults to 
#'      \code{ncol(E)} modulo 2 if \code{swapIsOneFlip==FALSE}. Ignored otherwise.
#' @param eps Tolerance on edit check. Defaults to \code{sqrt(.Machine.double.eps)}
#' @param flip Names of variables whos signs may be flipped, defaults to \code{colnames(E)}
#' @param swap \code{list} of 2-vectors containing pairs of variable names who's values may 
#'      be interchanged. Defaults to \code{NA}.
#' @param swapIsOneFlip \code{logical}. Count a value interchange as 1 or 2 sign changes? 
#'  If \code{TRUE}, a value swap is counted as 1 sign interchange, which corresponds with the 
#'  formulation in Scholtus (2008). If \code{FALSE}, a swap counts as two sign changes,
#'  (since \eqn{y-x=-(x-y))}). See also the details.
#' @param weight Positive numeric vector of length ncol(E). Variables with heigher 
#'      reliability weight are less likely to be changed. Defaults to \code{rep(1,ncol(E))}
#' @param fix character vector. Names of variables which may not be changed. Ignored when \code{swapIsOneFlip==TRUE}
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
    flip = colnames(E),
    swap = NULL,
    swapIsOneFlip = TRUE,
    weight = NA,
    fix = NA){
# TODO check if swap-pairs have opposite signs in at least one edit.
# TODO check that flip and are mutually exclusive
# TODO consistency checks on arguments
    # Flip signs and swaps variables if allowed. Register swaps. 
    swappit <- function(sw){
        if ( all(s[sw]==-1) || any(abs(r[sw]) < eps) & any(s[sw] == -1) ){
            r[sw] <<- r[sw[2:1]]
            s[sw] <<- 1
        }
    }

    # which variables may be changed?
    notFixed <- rep(TRUE,ncol(E))
    if ( is.character(fix) & !swapIsOneFlip  ){
        notFixed <- !(colnames(E) %in% fix)
    }

    # swap-names to swap-indices
    haveSwaps <- FALSE
    if (!identical(swap, NULL)){
        iSwap <- lapply(swap, function(sw) which(names(dat) %in% sw) )
        haveSwaps <- TRUE
    }
        
    if ( swapIsOneFlip & haveSwaps ){
        swapArray <- t(sapply(iSwap, array))
        flipVec <- sapply(flip, function(fl) which(names(dat) %in% fl))
    }

    # default weights if necessary
    if (is.na(weight)){
        if (swapIsOneFlip) {
            weight <- matrix(rep(1,length(flip) + length(swap)), nrow=1)
        } else {
            weight <- matrix(rep(1,ncol(E)), nrow=1)
        }
    }

        

    cn <- colnames(E)
    D <- as.matrix(dat[ ,cn])
    A <- as.matrix(E)
    degeneracy <- integer(nrow(dat))
    for ( i in 1:nrow(dat) ){
        r <- D[i,]
        violated <- abs(A %*% r) > eps            
        adapt <- notFixed & colSums(abs(E[violated, ,drop=FALSE])) > 0
        if ( swapIsOneFlip ){
          S <-  flipAndSwap(A, r, flipVec, swapArray, eps, weight)
        } else {
          S <- getSignCorrection(A, r, adapt, maxSigns, eps, weight)
        }
        if ( length(S$signs) > 0 ){
            s <- S$signs[[which.min(S$weights)]]
            degeneracy[i] <- sum(S$weights==max(S$weights))
            if ( haveSwaps ) lapply(iSwap, swappit)
                D[i, ] <- r*s
        }
    }
    dat[,cn] <- D
    return(list(data=dat, degeneracy=degeneracy))
}





