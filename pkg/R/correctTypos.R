#' detect and correct typing errors in \code{dat} based on the \code{editmatrix} E. 
#'
#' This algorithm tries to repair records that violate linear equality constraints by correcting simple typo's
#' It detects and corrects simple typing errors as described in Scholtus (2009). The implemention of the detection of typing errors 
#' differs in that it uses the Damerau-Levensthein distance. Furthermore it solves a broader class of problems: the original  #' paper describes the class of inequalites: \eqn{Ex=0} and the implementation allows for  \eqn{Ex=a}.
#' 
#' For each row in \code{dat} the correction algorithm first detects if row \code{x} violates the equality constraints of \code{E}. 
#' In mathematical terms the matrix equation \eqn{Ex=a} should hold. The implementation checks these 
#' constraints within rounding errors.
#'
#' \code{correctTypos} returns a list with (amongst others) \code{corrections}. Each corrected
#' \tabular{lll}{
#'       row   \tab \code{integer}   \tab row number of \code{dat} \cr
#'       var   \tab \code{character} \tab variable name \cr
#'       old   \tab \code{numeric}   \tab old value of var in row \cr
#'       new   \tab \code{numeric}   \tab new value of var in row \cr
#'     }
#'
#' Please note that if the returned status of a record is "partial" the corrected record is still not valid.
#' The partially corrected record will contain less errors and will violate less constraints. 
#' Also note that the status "valid" and "corrected" have to be interpreted in combination with \code{eps}.
#' A common case is first to correct for typo's and then correct for rounding errors. This means that in the first
#' step the algorithm should allow for typo's (e.g. \code{eps==2}). The returned "valid"  record therefore may still contain 
#' rounding errors.
#'
#' @export
#' @example examples/correctTypos.R
#' @seealso \code{\link{damerauLevenshteinDistance}}
#'
#' @param E \code{\link{editmatrix}} that constrains \code{x} 
#' @param dat \code{data.frame} with data to be corrected.
#' @param cost for an deletion, insertion, substition or transposition.
#' @param eps \code{numeric}, tolerance on edit check. Default value is \code{sqrt(.Machine$double.eps)}. Set to 2 
#' to allow for rounding errors. Set this parameter to 0 for exact checking.
#' @param maxdist \code{numeric}, tolerance used in finding typographical corrections. Default value 1 allows for one error. Used in combination with \code{cost}.
#'
#' @return list with members
#' \tabular{ll}{
#' status      \tab an ordered \code{factor} with the status for each row: \code{valid, corrected, partial, invalid} \cr
#' corrected   \tab corrected \code{data.frame}: the original \code{dat} with the \code{corrections} applied \cr
#' corrections \tab \code{data.frame} with all corrections. see details
#' } 
#' 
#' @references see
#' 
#' Scholtus S (2008). Algorithms for correcting some obvious
#' inconsistencies and rounding errors in business survey data. Technical
#' Report 08015, Netherlands.
#' 
#' Damerau F (1964). A technique for computer detection and correction of
#' spelling errors. Communications of the ACM, 7,issue 3
#'
#' Levenshtein VI (1966). Binary codes capable of correcting deletions, insertions, 
#' and reversals. Soviet Physics Doklady 10: 707-10
correctTypos <- function( E
                        , dat
                        , cost = c(1,1,1,1)
                        , eps = sqrt(.Machine$double.eps)
                        , maxdist = 1
                        ){
                        
   stopifnot(is.editmatrix(E), is.data.frame(dat))
   
   eq <- getOps(E) == "=="
   if (!all(eq)){
      stop("E must be an equality edit matrix. Edits ", which(!eq)," are inequalities.")
   }
   
   #align names of E and dat, beware m contains only constrained, numeric variables at this point
   m <- as.matrix(dat[colnames(E)])
   n <- nrow(m)
   
   status <- factor(integer(n), levels=c("valid","corrected", "partial","invalid"), ordered=TRUE)   
   corrections <- NULL

   # only loop over complete records
   cc <- which(complete.cases(m))
	for (i in cc){
	   chk <- getTypoCorrection(E,m[i,], eps, maxdist)
      
      status[i] <- chk$status
      
      if (chk$status %in% c("valid", "invalid")){
         #nothing we can do...
         next
      }

      cor <- chk$cor
      #sol <- tree(chk$B, cor[,"kappa"])
      sol <- tree(chk$B, cor[,5])
      if (nrow(sol) > 1){
         # if a correction is valid for all found solutions, then it can be applied
         partialsol <- colSums(sol) == nrow(sol)
         if (any(partialsol)){
            sol[1,] <- partialsol
            status[i] <- "partial"
         }
         else {
            status[i] <- "invalid"
            next
         }
      }
      cor <- cor[sol[1,],,drop=FALSE]
      
      #m[i, cor[,"var"]]  <- cor[,"new"]      
      m[i, cor[,1]]  <- cor[,3]
      
      cor <- cbind(row=rep(i, nrow(cor)), cor)
      corrections <- rbind(corrections, cor)      
	}
   
   vars <- getVars(E)
   
   # recreate data.frame dat in original column order, but with the corrections applied
   corrected <- dat   
   corrected[vars] <- as.data.frame(m)[]
   
   cdf <- data.frame( row=corrections[,1]
                    , var=vars[corrections[,2]]
                    , old=corrections[,3]
                    , new=corrections[,4]
                    )
                    
   list( status = data.frame(status=status)
       , corrected = corrected
       , corrections = cdf
       )
}

#' Check record validity and suggest typo corrections
#'
#' This function is the working horse for \code{\link{correctTypos}}
#' @nord
#' @param E editmatrix
#' @param x numerical record to be checked
#' @param eps tolerance for an edit to be valid
#' @param maxdist maximum edit distance to be valid as a correction suggestion
#' @return list with members
#' \tabular{ll}{
#' status \tab \cr
#' cor    \tab suggested corrections \cr
#' B      \tab reduced binary editmatrix with violated edits, needed for choosing the suggested corrections\cr
#'}
getTypoCorrection <- function( E, x, eps=sqrt(.Machine$double.eps), maxdist=1){
   ret <- list(status=NA)
   
   a <- getC(E)
   
   #violated edits (ignoring rounding errors)
   E1 <- (abs(a-E%*%x) > eps)
   
   #non violated edits
   E2 <- !E1
   
   if (all(E2)){
      #record is valid ignoring rounding errors
      ret$status <- "valid"
      return(ret)
   }
   
   # set of variables that are involved in the violated edits
   V1 <- if (any(E1)) colSums(abs(E[E1,,drop=FALSE])) != 0
         else FALSE
               
   # set of variables that are not involved in the non-violated edits and therefore can be edited
   I0 <- if (any(E2)) colSums(abs(E[E2,,drop=FALSE])) == 0
         else TRUE

   # restrict I0 to the set of variables involved in violated edits that can be changed
   I0 <- V1 & I0
   
   if (sum(I0) == 0){
		# cannot correct this error
      ret$status <- "invalid"
		return(ret)
   }
   
   names(I0) <- colnames(E)
   # retrieve correction canditates for variables that can be changed
   cor <- lapply( which(I0)
                , function(i){
                     # edits valid for current variable v_i
                     edits <- E1 & (E[,i] != 0)
                     
                     # correction candidates
                     x_i_c <- ( (a[edits]-(E[edits,-i] %*% x[-i])) / (E[edits,i]));
                     # count their numbers
                     kap <- table(x_i_c)
                     x_i_c <- as.integer(rownames(kap))
                     kap <- as.integer(kap)
                     # and retrieve their distance from the current x[i]
                     sapply( seq_along(kap)
                           , function(j){
                                c( var = i
                                 , old = x[i]
                                 , new = x_i_c[j]
                                 , dist = damerauLevenshteinDistance(x_i_c[j], x[i])
                                 , kappa = kap[j]
                                 )
                             }
                           )
                  }
                )
   cor <- t(do.call(cbind,cor))
   
   # filter out the corrections that have dist > 1
   valid <- cor[,4] <= maxdist
   
   cor <- cor[valid,,drop=FALSE]
   # optimization matrix
   B <- E[E1,cor[,1], drop=FALSE] != 0
   ret$cor <- cor
   ret$B <- B
   ret$status <- "corrected"
   ret
}

#' Solve an optimization problem using a tree algorithm as described in Scholtus (2009)
#' @nord
#' @param B binary matrix with suggested corrections per violated edit
#' @param kappa frequency of suggested corrections
#' @param delta \code{logical} vector with partial solution (starts with NA)
#' @param sol current best solution. (starts with null)
#' 
#' @return sol
tree <- function( B
                , kappa
                , delta=as.logical(rep(NA, ncol(B)))
                , sol = NULL
                ) {
   if (any(is.na(delta))){
      i_t <- match(NA,delta); # first element of partial solution that is not determined
      
      # leftnode delta_i_t == FALSE
      delta[i_t] <- FALSE
      sol <- tree(B, kappa, delta, sol)
      
      # rightnode  delta_i_t == TRUE
      # set other corrections involved in this edit to FALSE
      # edits involved in i_t
      E2 <- B[,i_t]
      delta[colSums(B[E2,,drop=FALSE]) > 0] <- FALSE
      delta[i_t] <- TRUE
      sol <- tree(B, kappa, delta, sol)
   }
   else {
      value = kappa%*%delta
      delta  <- matrix(delta, nrow=1)
      if (is.null(sol)){
         sol <- delta
      }
      else {
         vals <- kappa %*% sol[1,]
         if (vals < value){
            sol <- delta
         }
         else if (vals == value){
            sol <- rbind(sol, delta)
         }
      }
   }
   sol
}