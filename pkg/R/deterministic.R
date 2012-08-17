
##-------------------------------------------------------------------------
# default symbols allowed to define imputation rules.
.onLoad <- function(libname,pkgname){
   options(allowedSymbols = c(
      'ifelse',
      'if', 'else', 'is.na','is.finite',
      '==','<','<=','=','>=','>','!', '%in%',
      '||', '|', '&&', '&', 
      '(','{','<-','=',
      '+', '-', '*', '^', '/', '%%', '%/%'
      )
   )
}

##-------------------------------------------------------------------------
# define imputation rules.



#' Rules for deterministic imputation
#' @param x Rules, in \code{character} or \code{expression} form. 
#' @param strict If \code{TRUE} an error is thrown if any forbidden symbol is used (see details).
#' @param allowed A \code{character} vector of allowed symbols
#' @export
#' @seealso \code{\link{imputeWithRules}}
imputationRules <- function(x, strict=TRUE, allowed=getOption('allowedSymbols'), ...){
   UseMethod('imputationRules')
}


#' @method imputationRules character
#' @param file If \code{file=TRUE}, \code{x} is treated as a filename.
#' @rdname imputationRules
imputationRules.character <- function(x, strict=TRUE, allowed=getOption('allowedSymbols'), file=TRUE, ...){
   if ( file ){ 
      x <- parse(file=x)
      return(imputationRules.expression(x,strict,allowed))
   }
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
   if (strict){ 
      M <- checkRules(L,allowed=allowed)
      if ( any(M$error) ){ 
         printErrors(x,M)
         stop('Forbidden symbols found')
      }
   }
   structure(L,class='imputationRules')
}


#' @method imputationRules expression
#' @rdname imputationRules
imputationRules.expression <- function(x,strict=TRUE, allowed=getOption('allowedSymbols'), ...){
   if (strict){ 
      M <- checkRules(x,allowed=allowed)
      if ( any(M$error) ){ 
         printErrors(x,M)
         stop("Forbidden symbols found in imputation rules")
      }
   }
   structure(x,class='imputationRules')
}

#' @method print imputationRules 
#' @export
#' @rdname imputationRules
print.imputationRules <- function(x,...){
   cat("Object of class 'imputationRules'")
   v <- as.character(x)
   v <- gsub("^","  ",v)
   v <- gsub("\n","\n  ",v)
   cat(sprintf("\n## %2d-------\n%s",1:length(v),v),'\n')
}

#' @method as.character imputationRules
#' @param oneliner coerce to oneliner
#' @export
#' @rdname imputationRules
as.character.imputationRules <- function(x, oneliner=FALSE,...){
   # this seems to be the easiest way to retain formatting information (mvdl)
   v <- sapply(x,function(r) as.character(noquote(list(r))))
   if ( oneliner ){
      v <- gsub("\n"," ",v)      # remove line end
      v <- gsub("[ ]+"," ",v)    # collapse multiple spaces to one
   }   
   v
}

isconditional <- function(r){
   r[[1]][[3]][[1]] == "ifelse" || r[[1]][[1]] == 'if'
}

##-------------------------------------------------------------------------
# check if expression contains forbidden symbols (boolean)
checkSymbols <- function(x, b=TRUE, allowed=getOption('allowedSymbols'), ...){
   if (is.expression(x)){ 
      x <- x[[1]]
   }

   if (length(x) == 1 ) return(TRUE)
   if (  is.symbol(x[[1]]) && 
         !(as.character(x[[1]]) %in% allowed) ){
            return(FALSE)
   }
   for ( i in 1:length(x) ){b <- b & checkSymbols(x[[i]],b)}
   b
}

##-------------------------------------------------------------------------
# extract forbidden symbols from an expression
extractSymbols <- function(x, allowed, L=character(0), ...){
   if ( is.expression(x)){
      x <- x[[1]]
   }
   if ( length(x) == 1 ) return(NULL)
   if ( is.symbol(x[[1]]) && 
      !( as.character(x[[1]]) %in% allowed ) ){
      return(as.character(x[[1]]))
   }
   for ( i in 1:length(x) )  L <- c(L, extractSymbols(x[[i]], allowed, L))
   L
}

checkRules <- function(x, allowed, ...){
   M <- lapply(x,extractSymbols, allowed=allowed,...)
   list(error = sapply(M,function(m) length(m) == 0), symbols=M)
}


##-------------------------------------------------------------------------
# print rules and their errors. 
# x : list of rules, M result of checkRules
printErrors <- function(x, M){
   ix <- which(!M$error)
   v <- sapply(x[ix], function(r){
            r <- gsub("^","  ",as.character(r))
            gsub("\n","\n  ",r)
         })
   S <- lapply(M$symbols[ix], paste, collapse=", ")
   cat('\nForbidden symbols found in imputation rules:')
   cat(sprintf('\n## ERR %2d ------\nForbidden symbols: %s\n%s',ix,S,v),'\n')
}





#' @method getVars imputationRules
#' @rdname imputationRules
#' @param E object of class \code{\link{imputationRules}}
getVars.imputationRules <- function(E, ...){
   unique(do.call(c,lapply(E,getvrs)))
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



#' Deterministic imputation
#'
#' @param rules object of class \code{\link{imputationRules}} 
#' @param dat \code{data.frame}
#' @param strict If \code{TRUE}, an error is produced when the imputation rules use variables other than in the \code{data.frame}.
#' @export
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
   tr <- as.character(rules,oneliner=TRUE)
   
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



