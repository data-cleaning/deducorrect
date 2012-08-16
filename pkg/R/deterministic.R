
imputationRules <- function(x, strict=TRUE, ...){
   UseMethod('imputationRules')
}


# TODO: re
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

imputationRules.expression <- function(x,strict=TRUE, ...){
   if (strict) checkRules(L)
   structure(x,class='imputationRules')
}


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
   '||', '|', '&&', '&', 
   '(',
   '+', '-', '*', '^', '/', '%%', '%/%'
)

checkRules <- function(...) TRUE

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


getVars.imputationRules <- function(E, ...){
   unique(sapply(E,getvrs))
}




imputeWithRules <- function(rules, dat){
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



