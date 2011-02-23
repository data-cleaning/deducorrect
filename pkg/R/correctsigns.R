
#' Try to solve balance edit violations by sign changes
#' 
#' This is the workhorse function for \code{\link{correctSigns}}. It performs
#' a breadth-first tree search to resolve (near) equality edit violations.
#' The solution(s), if any, with as little as possible sign changes are returned.
#'
#'
#' @param A The \code{matrix} part of an \code{editmatrix}
#' @param C The \code{CONSTANT} part of an \code{editmatrix}
#' @param r A numerical record.
#' @param adapt A logical vector of length \code{ncol(A)} indicating which variables may be changed.
#' @param maxSigns how many signs may maximally be flipped? 
#' @param eps Tolerance for equality restriction checking
#' @param w positive weight vector of \code{length(ncol(A))}
#' 
#' @return A \code{list} with vectors of length \code{r} with coefficients 
#'      in \eqn{\{-1,1\}}. Empty \code{list} if no solution is found.
#'
getSignCorrection <- function(A, C, r, adapt, maxSigns, eps, w){

    v <- rep(1,length(r))
    if (all(abs(A%*%r - C) <= eps)) return(list(v))
    adapt <- which(adapt)
    nViolated <- length(adapt)
    S <- list()
    weights <- NA 
    j <- 1
    for ( nsigns in 1:min(maxSigns, nViolated) ){
        I <- combn(1:nViolated, nsigns)
        for ( i in 1:ncol(I) ){
           s <- v
           s[adapt[I[, i]]] <- -1
           if ( all(abs(A %*% (r*s) - C) <= eps) ){
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
#' @param C The \code{CONSTANT} part of an \code{editmatrix}
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
flipAndSwap <- function(A, C, r, flip, swap, eps, w){
    II <- rbind(cbind(flip,NA),swap)
   
    v <- rep(1,ncol(A))
    if (all(abs(A%*%r - C) <= eps)) return(list(v))
    
    S <- list()
    weights <- numeric(0)
    j <- 1
    for ( nsigns in 1:(nrow(II))){    
        I <- combn(1:nrow(II), nsigns)
        for ( i in 1:ncol(I) ){
           s <- v
           k <- as.integer(II[I[,i],])
           k <- k[!is.na(k)]
           s[k] <- -1
           if ( all(abs(A %*% (r*s) - C) <= eps) ){
                S[[j]] <- s
                weights[j] <- sum(w[I[,j]])
                j <- j+1
           }
        }
        if ( length(S) > 0 ) break
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
    maxSigns = length(unique(c(flip,unlist(swap)))),
    eps=sqrt(.Machine$double.eps),
    flip = colnames(E),
    swap = NULL,
    swapIsOneFlip = FALSE,
    weight = NA,
    fix = NA){
# TODO check if swap-pairs have opposite signs in at least one edit.
# TODO use only equality constraints.

    # check that flip and swap are disjunct
    lapply(swap, function(sw){ 
        if (any(flip %in% sw)) 
            stop("Variables in flip and swap must be mutually exclusive")
    })
    
    # remove flips and swaps containing fixed variables.
    if (!identical(fix,NA)){
        flip <- setdiff(flip, fix)
        swap <- lapply(swap, function(sw){
            if ( !any(sw %in% fix)) return(sw)
        })
    }

    # default weights if necessary
    if (identical(weight,NA)){
        if (swapIsOneFlip){
            weight <- matrix(rep(1,length(flip) + length(swap)), nrow=1)
        } else {
            weight <- matrix(rep(1,ncol(E)), nrow=1)
            weight[!(colnames(E) %in% flip)] <- 0 
        }
    # check valitity of weights
    } else {
        if (swapIsOneFlip){
            if (length(weight) != length(flip) + length(swap)){
                cat("Problem with weigth vector:\n")
                cat(paste(" flip variables (",length(flip),"): ",paste(flip,collapse=", "),"\n",sep=""))
                cat(paste(" swaps          (",length(swap),"): ",paste(sapply(swap, paste,collapse="<->"),sep=", "),"\n",sep=""))
                cat(paste(" weight vector  (",length(weight),"): ",paste(weight, collapse=" "),"\n",sep=""))
                stop("Length of Weight vector not equal to number of flips + number of swaps")
            } 
        } else {
            if (length(weight) != ncol(E)){
                stop("Length of weight vecor must equal the number of columns in E")
            }
        }
    }
    
    
    # convenient subvectors
    if (swapIsOneFlip){
        wflip <- if ( length(flip)>0 ) weight[1:length(flip)] else numeric(0)
        wswap <- if ( length(swap)>0 ) weight[(length(flip)+1):length(weight)] else numeric(0)
    }
    # which variables may be changed?
    notFixed <- rep(TRUE,ncol(E))
    if ( !swapIsOneFlip  ){
        notFixed <- colnames(E) %in% c(flip,unlist(swap))
    }

    # Prepare matrices for correctSigns ans flipAndSwap
    cn <- colnames(E)
    D <- as.matrix(dat[ ,cn])
    A <- as.matrix(E)[getOps(E) == "==", ,drop=FALSE]
    C <- getC(E)
    # swap-names to swap-indices
    haveSwaps <- FALSE
    if (!identical(swap, NULL)){
        iSwap <- lapply(swap, function(sw) which(colnames(D) %in% sw) )
        haveSwaps <- TRUE
    }
        
    if ( swapIsOneFlip & haveSwaps ){
        flipable <- colnames(D) %in% flip
        swapable <- t(sapply(iSwap, array))
    }

    # Flip signs and swaps variables if allowed. 
    swappit <- function(sw){
        if ( all(s[sw]==-1) || any(abs(r[sw]) < eps) & any(s[sw] == -1) ){
            r[sw] <<- r[sw[2:1]]
            s[sw] <<- 1
            nswap[i] <<- nswap[i] + 1
        }
    }

    # do the actual work
    degeneracy <- integer(nrow(dat))
    nflip <- nswap <- weights <- numeric(nrow(dat))
    status <- factor(1:nrow(D),levels=c("valid","corrected","partial","invalid"))
    corrections <- data.frame(row=integer(0), var=factor(levels=colnames(D)), old=numeric(0), new=numeric(0))

    for ( i in which(complete.cases(D)) ){
        r <- D[i,]
        violated <- abs(A%*%r - C) > eps            
        adapt <- notFixed & colSums(abs(E[violated, ,drop=FALSE])) > 0
        if ( swapIsOneFlip ){
            fl    <- which(adapt & flipable)
            adapt <- which(adapt)
            w1    <- wflip[which(flipable) %in% adapt]
            iSw   <- which(swapable[,1] %in% adapt & swapable[,2] %in%  adapt)
            w2    <- wswap[iSw]
            S <- flipAndSwap(A, C, r, fl, swapable[iSw,], eps, c(w1,w2))
        } else {
            S <- getSignCorrection(A, C, r, adapt, maxSigns, eps, weight)
        }
        if ( length(S$signs) > 0 ){ # solution found
            s <- S$signs[[which.min(S$weights)]]
            degeneracy[i] <- sum(S$weights==min(S$weights))
            weights[i] <- min(S$weights)
            adapted <- s == -1
            if ( !any(adapted) ){
                status[i] <- "valid"
            } else {
                oldrec <- r
                if ( haveSwaps ) lapply(iSwap, swappit)
                nflip[i] <- sum(adapted) - 2*nswap[i]
                D[i, ] <- r*s
                status[i] <- "corrected"
                corrections <- rbind(corrections,
                    data.frame(
                        row = rep(i,sum(adapted)),
                        col = colnames(D)[adapted],
                        old = oldrec[adapted],
                        new = D[i,adapted]))
            }
        } else { # no solution
            status[i] <- "invalid"
        }
    }
    dat[,cn] <- D
    rownames(corrections) <- NULL
    return(list(
        corrected=dat, 
        corrections=corrections, 
        status=data.frame(
            status=status, 
            degeneracy=degeneracy,
            weight=weights,
            nflip=nflip,
            nswap=nswap)))
}





