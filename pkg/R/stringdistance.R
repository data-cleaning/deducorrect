#' Damerau Levenshtein Distance calculates the difference between two strings
#' used for typografiphical errors (typo's) 
#'
#' \cite{levenshtein:1966}
#' \cite{damerau:1964}
#'
#' @param sa character vector
#' @param sb character vector of equal \code{length(sa)}
#' @param w integer vector for cost of deletion, insertion, substitution 
#' and transposition.
#'
#' @return integer vector with all edit distances
damerauLevenshteinDistance <- function(sa,sb, w=c(1,1,1,1)){
   #TODO edit this so it calculates the distance for multiple   characters.
   
   mapply(function(a,b){      
      a <- c("",a)
      b <- c("",b)
      
      la <- length(a)
      lb <- length(b)
      d <- matrix(nrow = la, ncol = lb)
    
      
      d[,1] <- 0:(la-1)
      d[1,] <- 0:(lb-1)
      for(i in 2:la){
         for(j in 2:lb) {
            cost <- if (a[i] == b[j]) 0
                    else w[3]
            d[i,j] <- min( (d[i-1, j  ] + w[1])      # deletion
                         , (d[i  , j-1] + w[2])      # insert
                         , (d[i-1, j-1] + cost)   # substitution
                         )
            if (a[i]==b[j-1] && a[i-1]==b[j]){
             cost <- if (a[i] == b[j]) 0
                   else w[4]
               d[i,j] <- min( d[i  ,j  ] 
                            , d[i-2,j-2] + cost   # transposition
                            )
            }
         }
      }
      d[la,lb]
   }
   , strsplit(as.character(sa), NULL)
   , strsplit(as.character(sb), NULL)
   )
}