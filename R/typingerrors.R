#' \code{typingErrors} detects and corrects typing errors in \code{dat} based on the \code{editmatrix} E. 
#'
#' Typing errors detects and corrects typing errors as described in Scholtus (2009). The implemention of the detection of typing errors 
#' differs in that it uses the Damerau-Levensthein distance ()
#' TODO explain Ax=0
#'
#' \cite{scholtus:2009}
#' \cite{levenshtein:1966}
#' \cite{damerau:1964}
#'
#' @export
#' @seealso damerauLevenshteinDistance
#'
#' @param A \code{\link{editmatrix} that constrains \code{x} 
#' @param dat data.frame with data to be checked.
#' @param cost for an deletion, insertion, substition or transposition
typingErrors <- function( E
                        , dat
                        , cost = c(1,1,1,1)
                        ){
   stopifnot(is.matrix(A), is.data.frame(dat))
   
   #align names of A and dat, beware dat contains only constrained variables at this point
   dat <- dat[colnames(A)]
   
   # looping might be inefficient so we may rewrite this
   n <- nrow(dat)
   status <- factor(integer(n), levels=c("valid","correction", "invalid"))
   #names(status) <- rownames(dat)
   
   corrections <- NULL

   m <- as.matrix(dat)   
	for (i in 1:n){
	  chk <- checkRecord(A,t(dat[i,]))
      
      status[i] <- chk$status
      
      if (chk$status %in% c("valid", "invalid")){
         next
      }

      cor <- chk$cor
      # try corrections
      #cat("Row ",i,":\n")
      #print(chk)
      sol <- tree(chk$B, cor[,"kappa"])
      if (nrow(sol > 1)){
         warning("Multiple solutions in row ", i, ". Taking first solution found.")
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

#' check record if its is valid and suggest corrections
#' @param A editmatrix
#' @param x numerical record to be checked
#' @param eps tolerance for an edit to be valid
#' @return list with members
#' \itemize
#' \item \code{status}
#' \item \cor suggested corrections
#' \item \B reduced binary editmatrix with violated edits, needed for choosing the suggested corrections
checkRecord <- function( A, x, eps=2){
   ret <- list(status=NA)
   #violated edits (ignoring rounding errors)
   E1 <- (abs(A%*%x) > eps)
   
   #non violated edits
   E2 <- !E1
   
   if (all(E2)){
      #record is valid ignoring rounding errors
      ret$status <- "valid"
      return(ret)
   }
   
   # set of variables that are involved in the violated edits
   V1 <- if (any(E1)) colSums(abs(A[E1,,drop=FALSE])) != 0
         else FALSE
               
   # set of variables that are not involved in the non-violated edits and therefore can be edited
   I0 <- if (any(E2)) colSums(abs(A[E2,,drop=FALSE])) == 0
         else TRUE

   # restrict I0 to the set of variables involved in violated edits that can be changed
   I0 <- V1 & I0
   
   if (sum(I0) == 0){
		# cannot correct this error
      ret$status <- "invalid"
		return(ret)
   }
   
   names(I0) <- colnames(A)
   # retrieve correction canditates for variables that can be changed
   cor <- lapply( which(I0)
                , function(i){
                     # edits valid for current variable v_i
                     edits <- E1 & (A[,i] != 0)
                     
                     # correction candidates
                     x_i_c <- ( (A[edits,-i] %*% x[-i]) / (-A[edits,i]));
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
   valid <- cor[,"dist"] <= 1
   
   cor <- cor[valid,,drop=FALSE]
   # optimization matrix
   B <- A[E1,cor[,"i"], drop=FALSE] != 0
   # convert to 0 and 1
   #mode(B) <- "integer"
   ret$cor <- cor
   ret$B <- B
   ret$status <- "correction"
   ret
}

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