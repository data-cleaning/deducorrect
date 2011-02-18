
#' Solve balance edit violations by sign changes
#' 
#' This is the workhorse function for \code{\link{correctSigns}}. It performs
#' a breadth-first tree search to resolve (near) equality edit violations.
#' The solution(s), if any, with as little as possible sign changes are returned.
#'
#'
#' @param A The \code{matrix} part of an \code{editmatrix}
#' @param r A numerical record.
#' @param inViolatedEdits An integer vector containing indices in the columns 
#'      of \code{A}, alocating the variables which occur in the edits violated 
#'      by \code{r}
#' @param maxSigns how many signs may maximally be flipped? 
#' @param eps Tolerance for equality restriction checking
#' 
#' @return A \code{list} with vectors of length \code{r} with coefficients 
#'      in \eqn{\{-1,1\}}. Empty \code{list} if no solution is found.
#'
getSignCorrection <- function(A, r, inViolatedEdits, maxSigns, eps){

    v <- rep(1,nrow(r))
    if (all(abs(A%*%r) <= eps)) return(list(v))
    nViolated <- length(inViolatedEdits)
    S <- list()
    j <- 1
    for ( nsigns in 1:min(maxSigns, max(nViolated%/%2,1)) ){
        I <- combn(1:nViolated, nsigns)
        for ( i in 1:ncol(I) ){
           s <- v
           s[inViolatedEdits[I[, i]]] <- -1
           if ( all(abs(A %*% (r*s)) <= eps) ){
                S[[j]] <- s
                j <- j+1
           }
        }
        if ( length(S) > 0 ) break
    }
    return(S)
}

correctSigns <- function(
    E, 
    dat,
    maxSigns = ncol(E)%/%2,
    eps=sqrt(.Machine$double.eps),
    weight = rep(1,ncol(E))
    ){
# TODO swapping and fix variables    
    cn <- colnames(E)
    D <- as.matrix(dat[ ,cn])
    A <- as.matrix(E)
    w <- matrix(weight,nrow=1)
    degeneracy <- integer(nrow(dat))
    for ( i in 1:nrow(dat) ){
        r <- t(D[i,,drop=FALSE])
        violated <- abs(A %*% r) > eps            
        inViolatedEdits <- which(colSums(abs(E[violated, ,drop=FALSE])) > 0)
        S <- getSignCorrection(A, r, inViolatedEdits, maxSigns, eps)
        if ( length(S) > 0 ){
            wvec <- w %*% (sapply(S,matrix)<0)
            s <- S[[which.max(wvec)]]
            D[i, ] <- D[i, ]*s
            degeneracy[i] <- sum(wvec==max(wvec))
        }
    }
    dat[,cn] <- D
    return(list(data=dat, degeneracy=degeneracy))
}










