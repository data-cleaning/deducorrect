#' Rules for deterministic imputation
#' @param x Rules, in \code{character} or \code{expression} form.
#' 
#' @export
imputationRules <- function(x, strict=TRUE, ...){
   UseMethod('imputationRules')
}


#' @method imputationRules character
#' @rdname imputationRules
imputationRules.character <- function(x, strict=TRUE, ...){
   i <- 0
   L <- lapply(x, function(y){
      i <<- i+1
      tryCatch(parse(text=y), 
            error = function(e){
               msg <- sprintf("\nCould not process rule [%d]:\n '%s'\nparser returned:\n%s\n",i,y,e$message)
               stop(msg)
            })
   }
   )
   if (strict) checkRules(L)
   structure(L,class='imputationRules')
}


#' @method imputationRules expression
#' @rdname imputationRules
imputationRules.expression <- function(x,strict=TRUE, ...){
   if (strict){ 
      I <- checkRules(x)
      if (!all(I)){
         m1 <- "The following rules contain forbidden symbols or functions:"
         m2 <- sprintf("\n[%d] %s",which(!I),sapply(L[!I], as.character) )
         stop(sprintf("%s%s",m1,m2))
      }
   }
   structure(x,class='imputationRules')
}

#' @method imputationRules print
#' @export
#' @rdname imputationRules
print.imputationRules <- function(x,...){
   cat("Object of class 'imputationRules'")
   v <- sapply(x,as.character)
   v <- gsub("^","  ",v)
   v <- gsub("\n","\n  ",v)
   cat(sprintf("\n## %2d-------\n%s",1:length(v),v),'\n')
}


isconditional <- function(r){
   r[[1]][[3]][[1]] == "ifelse" || r[[1]][[1]] == 'if'
}

ALLOWEDSYMBOLS <- c(
   'ifelse',
   'if',
   'else',
   '==','<','<=','=','>=','>', '%in%',
   '||', '|', '&&', '&', 
   '(','{','<-','=',
   '+', '-', '*', '^', '/', '%%', '%/%'
)

checkSymbols <- function(x, b=TRUE, ...){
   if (is.expression(x)){ 
      x <- x[[1]]
   }

   if (length(x) == 1 ) return(TRUE)
   if (  is.symbol(x[[1]]) && 
         !(as.character(x[[1]]) %in% ALLOWEDSYMBOLS) ){
            return(FALSE)
   }
   for ( i in 1:length(x) ){b <- b & checkSymbols(x[[i]],b)}
   return(b)
}

checkRules <- function(x,...){
   sapply(x,checkSymbols)
}


getvrs <- function(x, L=character(0), ...){
   if ( is.expression(x) ){
      L <- getvrs(x[[1]],L)
   }  
   if ( length(x) == 1){
      if ( is.symbol(x) ){ 
         return(c(L,as.character(x)))
      } else {
         return(L)
      } 
   }

   for ( i in 2:length(x) ) L <- getvrs(x[[i]],L)
   unique(L)
}

#'
#' @method getVars imputationRules
#' @rdname imputationRules
#' @param E object of class \code{\link{imputationRules}}
getVars.imputationRules <- function(E, ...){
   unique(do.call(c,sapply(E,getvrs)))
}



#' Deterministic imputation
#'
#' @param rules object of class \code{\link{imputationRules}} 
#' @param dat \code{data.frame}
#' @param strict If \code{TRUE}, an error is produced when the imputation rules use variables other than in the \code{data.frame}.
imputeWithRules <- function(rules, dat, strict=TRUE){
   if (strict){
      vars <- getVars(rules)
      I <- vars %in% names(dat)
      if (!all(I)) stop(
         sprintf("Variables '%s' in imputation rules do not occur in data",paste(vars[!I],sep=", "))
      )
   }

   out <- dat
   m <- nrow(dat)
   n <- length(rules)
   row <- numeric(0)
   variable <- character(0)
   old <- numeric(0)
   new <- numeric(0)
   how <- character(0)
   vars <- colnames(dat)
   tr <- sapply(rules, function(r) gsub("\n","",as.character(r)))
   tr <- sapply(tr, function(r) gsub("[ ]+"," ",r))
   for ( i in 1:m ){
      for ( j in 1:n ){
         d <- out[i,,drop=FALSE]
         d <- within(d,eval(rules[[j]]))
         if ( !all(d==out[i,]) ){
            out[i,] <- d
            rule <- tr[j]
            w <- which(d != dat[i,])
            row <- c(row, rep(i,length(w)))
            variable <- c(old,vars[w])
            old <- c(old,do.call(c,as.list(dat[i,w])))
            new <- c(new,do.call(c,as.list(out[i,w])))
            how <- c(how,rep(rule,length(w)))
         }
      }
   }
   list(
      corrected=out, 
      corrections=data.frame(
            row=row,
            variable=variable,
            old=old,
            new=new,
            how=how,
            stringsAsFactors=FALSE
         )
   )
}



