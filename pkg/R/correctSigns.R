#' Try to solve balance edit violations by sign changes
#' 
#' This is one of the workhorse function for \code{\link{correctSigns}}, the other being \code{\link{flipAndSwap}} 
#' It performs a breadth-first tree search to resolve (near) equality edit violations. The difference with \code{\link{flipAndSwap}}
#' is that variable swaps are interpreted as two actions (two sign flips).
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
#' This is one of the workhorse function for \code{\link{correctSigns}}, the other being \code{\link{flipAndSwap}} 
#' It performs a breadth-first tree search to resolve (near) equality edit violations. The difference with 
#' \code{\link{getSignCorrection}} is that variable swaps are interpreted as a single action.
#' The solution(s), if any, with as little as possible sign changes are returned.
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


#' Correct records under linear restrictions using sign flips and variable swaps
#'
#' This algorithm tries to repair records that violate linear equality constraints by
#' switching signs or swapping variable values occuring in violated edits. Possible rounding errors
#' can be taken into account. Specifically, it tries to solve
#' 
#' \eqn{(1)\quad \min_{s\in\{-1,1\}^n\backslash V}(\sum_{i=1}^n\delta_{s_i,-1}) \textrm{ under } |\sum_{i=1}^nE_{ji}x_is_i - b_j| \leq \varepsilon,\quad \forall j},
#'
#' where \eqn{\delta} is the Kronecker delta function (the sum counts the number of occurences where \eqn{s_i=1}). Furthermore, 
#' \eqn{E} is an \code{editmatrix}, \eqn{x} is a the subset of a record of \code{dat} corresponding to
#' columns in \code{E}, and \eqn{b}  a vecor of constants. \eqn{V} is the set of vectors in \eqn{\{-1,1\}^n} excluded from the 
#' search space because they flip signs of variables not occuring in violated restrictions.
#' 
#' When a set \eqn{S} of equivalent solutions is found, the solution minimizing a certain
#' weight will be chosen. Rounding errors which mask sign  errors in \eqn{x} can be circumvented by setting \eqn{\varepsilon} to 2 or more
#' units of measure. 
#'
#' Note that when two elements of \eqn{x} have coefficients with opposite signs in one of the rows of \eqn{E},
#' flipping the sign of both elements is equal to changing their order (\emph{i.e.} \eqn{a-b=-(b-a)}). We will
#' call this a variable swap.
#'
#' The algorithm has two modes: one where a variable swap is counted as two sign flips (the default) and one where 
#' a variable swap is counted as one sign flip. This can be set with the option  \code{swapIsOneFlip}. 
#'
#' If \code{swapIsOneFlip=FALSE}, the default, the algorithm tries to solve the minimization of Eqn. (1). When a set \eqn{S} of
#' multiple solutions is found the solution satisfying
#'
#' \eqn{(2)\quad \min_{s\in S}\sum_{i=1}^n w_is_i\delta_{s_i,-1}}
#'
#' is chosen (\eqn{n} is the number of variables in the record). 
#' If this still doesn't single out one solution, the first one encountered is used. If the user passes a list
#' of variable pairs which may be interchanged (the \code{swap} argument), the solution will be checked for
#' swaps and if so, the swaps are applied.
#'
#' When \code{swapIsOneFlip=TRUE}, a value interchange counts as one sign change. The algorithm still searches for
#' the minimum of Eqn. (1). However, signs of swap-pairs are always changed simultaneously. Therefore the list of 
#' variables who's signs may be changed (\code{flip} argument) must be disjunct from the list of variable pairs
#' that may be swapped (\code{swap}). When more then one solution is found, the solution satisfying
#'
#' 
#' \eqn{(3)\quad \min_{s\in S}\sum_{i\in {\tt flips}}^{m} w_i  +\sum_{i\in{\tt swaps}} w_i }
#'
#' is chosen. Here, \eqn{w} is a vector of length \code{length{flip}+length{swap}}, so a weight is assigned to every 
#' action, not to every variable. The case where \code{swapIsOneFlip=TRUE}, is can be used in the the profit-loss account example
#' in \cite{Scholtus, 2008}. 
#'
#'
#'
#'
#' @title Correct records violating linear restrictions with sign flips and value swaps
#'
#'
#' @param E An object of class \code{editmatrix}. It may contain equality as well as inequality constraints, 
#'      but only the equality constraints will be used.
#' @param dat The data to correct
#' @param maxSigns Maximum number of signs that may be changed. Defaults to 
#'      the number of variables that occur in violated edits if \code{swapIsOneFlip==FALSE}. Ignored otherwise.
#' @param eps Tolerance on edit check. Defaults to \code{sqrt(.Machine.double.eps)}. Increase this to correct sign errors masked by rounding.
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
#' @return A \code{list} with the elements
#' \itemize{
#'  \item{\code{corrected}, the data in \code{dat}, corrected for sign flips and variable swaps against the linear equality restrictions in \code{E}, when possible.}
#'  \item{\code{corrections}, a \code{data.frame} containing the row, column name, old value and new value for every correction.}
#'  \item{\code{status}, a \code{data.frame} with the same number of rows as \code{dat} containing more information on applied corrections.
#'   Row numbers in \code{status} correspond to row numbers in \code{dat}. The content of the columns is given in the table below.} 
#' }
#'
#'  \tabular{ll}{
#'      \code{status}\tab a \code{\link{status}} factor, showing the status of the treated record.\cr
#'      \code{degeneracy}\tab the number of solutions found, \emph{after} applying the weight\cr
#'      \code{weight}\tab the weight of the chosen solution\cr
#'      \code{nflip}\tab the number of applied sign flips\cr
#'      \code{nswap}\tab the number of applied value interchanges\cr
#'  }
#' @example examples/correctSigns.R
#' @references
#' Scholtus S (2008). Algorithms for correcting some obvious
#' inconsistencies and rounding errors in business survey data. Technical
#' Report 08015, Netherlands.
#' @seealso \code{\link{deducorrect-object}}
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
    
    # check that flip and swap are disjunct
    if (swapIsOneFlip){
        lapply(swap, function(sw){ 
            if (any(flip %in% sw)) 
                stop("Variables in flip and swap must be mutually exclusive")
        })
    }
    
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
    status <- status(nrow(D))
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
                # TODO - this data.frame stuff can slow things down and should be speeded up.
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
    return(newdeducorrect(
        corrected  = dat,
        corrections=corrections,
        status=data.frame(
            status     = status, 
            degeneracy = degeneracy,
            weight     = weights,
            nflip      = nflip,
            nswap      = nswap)))
        
}





