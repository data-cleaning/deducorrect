#' detect and correct typing errors in \code{dat} based on the \code{editmatrix} E. 
#'
#' This algorithm tries to repair records that violate linear equality constraints by correcting simple typo's
#' It detects and corrects simple typing errors as described in Scholtus (2009). The implemention of the detection of typing errors 
#' differs in that it uses the Damerau-Levensthein distance.
#' 
#' For each row in \code{dat} the correction algorithm first detects if row \code{x} violates the equality constraints of \code{E}. In mathematical terms the matrix equation \eqn{Ex=0} should hold. The implementation checks these 
#' constraints within rounding errors.
#'
#' Please note that if the returned status of a record is "partial" the corrected record is still not valid.
#' The partially corrected record will contain less errors and will violate less constraints. 
#'
#' @export
#' @example examples/typingErrors.R
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
#' status      \tab a \code{factor} with the status for each row: \code{valid, invalid, corrected, partial} \cr
#' corrected   \tab \code{data.frame} where correction are applied to original data.frame \code{dat} \cr
#' corrections \tab \code{matrix} with all corrections. Per correction the row number, column number, 
#' original value, corrected value and frequency are given. \cr
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
#' and reversals. Soviet Physics Doklady 10: 707–10
typingErrors <- function( E
                        , dat
                        , cost = c(1,1,1,1)
                        , eps = sqrt(.Machine$double.eps)
                        , maxdist = 1
                        ){
   stopifnot(is.editmatrix(E), is.data.frame(dat))
   
   #TODO add check on E, are all ops "=="?
   
   #align names of E and dat, beware dat contains only constrained variables at this point
   dat <- dat[colnames(E)]
   
   # looping might be inefficient so we may rewrite this
   n <- nrow(dat)
   status <- factor(integer(n), levels=c("valid","corrected", "partial","invalid"))
   #names(status) <- rownames(dat)
   
   corrections <- NULL

   m <- as.matrix(dat)   
	for (i in 1:n){
	   chk <- suggestCorrections(E,t(dat[i,]), eps, maxdist)
      
      status[i] <- chk$status
      
      if (chk$status %in% c("valid", "invalid")){
         next
      }

      cor <- chk$cor
      # try corrections
      #cat("Row ",i,":\n")
      sol <- tree(chk$B, cor[,"kappa"])
      if (nrow(sol) > 1){
         # if a correction is valid for all found solutions, then it can be applied
         partialsol <- colSums(sol) == nrow(sol)
         #names(vars) <- colnames(chk$B)
         if (any(partialsol)){
            warning("Multiple solutions in row ", i, ". Applying partial correction.")
            sol[1,] <- partialsol
            status[i] <- "partial"
         }
         else {
            warning("Multiple solutions in row ", i, ". Marking record as invalid")
            status[i] <- "invalid"
         }
      }
      cor <- cor[sol[1,],,drop=FALSE]
      
      #make flexible?
      m[i, cor[,"i"]]  <- cor[,"x_r"]
      
      cor <- cbind(row=rep(i, nrow(cor)), cor)
      corrections <- rbind(corrections, cor)      
	}
   
   #TODO reconstruct corrected original data.frame
   corrected <- as.data.frame(m)
   
   list( status = status
       , corrected = corrected
       , corrections = corrections
       )
}

#' Check record validity and suggest corrections
#'
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
suggestCorrections <- function( E, x, eps=sqrt(.Machine$double.eps), maxdist=1){
   ret <- list(status=NA)
   #violated edits (ignoring rounding errors)
   E1 <- (abs(E%*%x) > eps)
   
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
                     x_i_c <- ( (E[edits,-i] %*% x[-i]) / (-E[edits,i]));
                     # count their numbers
                     kap <- table(x_i_c)
                     x_i_c <- as.integer(rownames(kap))
                     kap <- as.integer(kap)
                     # and retrieve their distance from the current x[i]
                     sapply( seq_along(kap)
                           , function(j){
                                c( i = i
                                 , x = x[i]
                                 , x_r = x_i_c[j]
                                 , dist = damerauLevenshteinDistance(x_i_c[j], x[i])
                                 , kappa = kap[j]
                                 )
                             }
                           )
                  }
                )
   cor <- t(do.call(cbind,cor))
   
   # filter out the corrections that have dist > 1
   valid <- cor[,"dist"] <= maxdist
   
   cor <- cor[valid,,drop=FALSE]
   # optimization matrix
   B <- E[E1,cor[,"i"], drop=FALSE] != 0
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
      i_t <- match(NA,delta); # eerste element van delta dat nog niet is bepaald
      
      #leftnode delta_i_t == FALSE
      delta[i_t] <- FALSE
      sol <- tree(B, kappa, delta, sol)
      
      #rightnode  delta_i_t == TRUE
      # set other corrections involved in this edit to FALSE
      #edits involved in i_t
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
