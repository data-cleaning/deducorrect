
##-------------------------------------------------------------------------
# default symbols allowed to define correction rules.
.onLoad <- function(libname,pkgname){
   options(allowedSymbols = c(
      'if', 'else', 'is.na','is.finite',
      '==','<','<=','=','>=','>','!=','!', '%in%',
      'identical','sign','abs',
      '||', '|', '&&', '&', 
      '(','{','<-','=',
      '+', '-', '*', '^', '/', '%%', '%/%'
      )
   )
}

.onUnload <- function(libpath){
   options(allowedSymbols=NULL)
}


##-------------------------------------------------------------------------
# define correction rules.


#' Rules for deterministic correction
#' 
#' These functions are deprecated and will be defunct as of 01.01.2015. Please see \code{\link{ruleset}} instead.
#'
#' @param x \code{character} or \code{expression} vector. 
#' @param strict If \code{TRUE} an error is thrown if any forbidden symbol is used (see details).
#' @param allowed A \code{character} vector of allowed symbols
#' @param ... Currently unused.
#' @return \code{correctionRules} returns an object of class \code{correctionRules}
#' 
#'
#' @export
#' @seealso \code{\link{correctWithRules}}
correctionRules <- function(x, strict=TRUE, allowed=getOption('allowedSymbols'), ...){
   .Deprecated(new='ruleset')
   UseMethod('correctionRules')
}

#' Rules for deterministic correction or deriving new variables
#'
#' @section Details:
#' This function, together with \code{\link{applyRules}} allows for easy definition and execution 
#' of simle deterministic replacement rules and variable derivation rules where all actions are
#' logged on the variable level.
#'
#' These functions are ment to support very simple rules, such as \emph{if variable x is missing, then
#' set it to zero}. Such actions usually basically model-free corrections stemming from subject-matter knowledge.
#' Given the nature of such rules, the type of rules are by default limited to R-statements containing
#' conditionals (\code{if}-\code{else}), arithmetic and logical operators, and brackets and assignment operators.
#' see \code{getOption('allowedSymbols')} for a complete list.
#'
#' If you cannot execute your 'simple' corrections with just these functions, we strongly recommend to 
#' write a separate imputation or correction routine. However, it's a free world, so you may alter the list of allowed symbols
#' as you wish. 
#' 
#' @section Note:
#' \code{getVars} is overloaded from the \code{editrules} package.
#'
#' @param x \code{character} or \code{expression} vector. 
#' @param strict If \code{TRUE} an error is thrown if any forbidden symbol is used (see details).
#' @param allowed A \code{character} vector of allowed symbols
#' @param ... Currently unused.
#' @return \code{ruleset} returns an object of class \code{ruleset}
#' 
#' @example ../examples/ruleset.R
#' @export
#' @seealso \code{\link{applyRules}}
ruleset <- function(x, strict=TRUE, allowed=getOption('allowedSymbols'), ...){
  UseMethod('ruleset')
}


#' @method correctionRules character
#' @param file If \code{file=TRUE}, \code{x} is treated as a filename from which the rules are read.
#' @rdname correctionRules
#' @export
correctionRules.character <- function(x, strict=TRUE, allowed=getOption('allowedSymbols'), file=TRUE, ...){
  ruleset.character(x,strict,allowed,file,...)
}

#' @method ruleset character
#' @param file If \code{file=TRUE}, \code{x} is treated as a filename from which the rules are read.
#' @rdname ruleset
#' @export
ruleset.character <- function(x, strict=TRUE, allowed=getOption('allowedSymbols'), file=TRUE, ...){
  if ( file ){ 
    x <- parse(file=x)
    return(correctionRules.expression(x,strict,allowed))
  } 
  
  i <- 0
  L <- sapply(x, function(y){
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
  structure(L,class='ruleset')
}



#' @method correctionRules expression
#' @rdname correctionRules
#' @export
correctionRules.expression <- function(x,strict=TRUE, allowed=getOption('allowedSymbols'), ...){
  ruleset.expression(x,strict,allowed,...)
}

#' @method ruleset expression
#' @rdname ruleset
#' @export
ruleset.expression <- function(x,strict=TRUE, allowed=getOption('allowedSymbols'), ...){
  
  if (strict){ 
    M <- checkRules(x,allowed=allowed)
    if ( any(M$error) ){ 
      printErrors(x,M)
      stop("Forbidden symbols found")
    }
  }
  structure(x,class='ruleset')
}



#' @method print correctionRules 
#' @export
#' @rdname correctionRules
print.correctionRules <- function(x,...){
   cat("Object of class 'correctionRules'")
   v <- as.character(x)
   v <- gsub("^","  ",v)
   v <- gsub("\n","\n  ",v)
   cat(sprintf("\n## %2d-------\n%s",1:length(v),v),'\n')
}

#' @method print ruleset 
#' @export
#' @rdname ruleset
print.ruleset <- function(x,...){
  cat("Object of class 'ruleset'")
  v <- as.character(x)
  v <- gsub("^","  ",v)
  v <- gsub("\n","\n  ",v)
  cat(sprintf("\n## %2d-------\n%s",1:length(v),v),'\n')
}



#' @method as.character correctionRules
#' @param oneliner Coerce to oneliner
#' @export
#' @rdname correctionRules
as.character.correctionRules <- function(x, oneliner=FALSE,...){
   # this seems to be the easiest way to retain formatting information (mvdl)
   v <- sapply(x,function(r) as.character(noquote(list(r))))
   if ( oneliner ){
      v <- gsub("\n"," ",v)      # remove line end
      v <- gsub("[ ]+"," ",v)    # collapse multiple spaces to one
   }
   names(v) <- NULL
   v
}

#' @method as.character ruleset
#' @param oneliner Coerce to oneliner
#' @export
#' @rdname ruleset
as.character.correctionRules <- function(x, oneliner=FALSE,...){
  # this seems to be the easiest way to retain formatting information (mvdl)
  v <- sapply(x,function(r) as.character(noquote(list(r))))
  if ( oneliner ){
    v <- gsub("\n"," ",v)      # remove line end
    v <- gsub("[ ]+"," ",v)    # collapse multiple spaces to one
  }
  names(v) <- NULL
  v
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
   list(error = sapply(M,function(m) length(m) > 0), symbols=M)
}


##-------------------------------------------------------------------------
# print rules and their errors. 
# x : list of rules, M result of checkRules
printErrors <- function(x, M){
   ix <- which(M$error)
   v <- as.character(x[ix])
   v <- gsub("^","  ",v)
   v <- gsub("\n","\n  ",v)
   S <- lapply(M$symbols[ix], paste, collapse=", ")
   cat('\nForbidden symbols found:')
   cat(sprintf('\n## ERR %2d ------\nForbidden symbols: %s\n%s',ix,S,v),'\n')
}





#' @method getVars correctionRules
#' @rdname correctionRules
#' @param E object of class \code{\link{correctionRules}}
#'
#' @return \code{getVars} returns a character vector of variable names.
#' @export
getVars.correctionRules <- function(E, ...){
   unique(do.call(c,lapply(E,getvrs)))
}

#' @method getVars ruleset
#' @rdname ruleset
#' @param E object of class \code{\link{ruleset}}
#'
#' @return \code{getVars} returns a character vector of variable names.
#' @export
getVars.ruleset <- function(E, ...){
  unique(do.call(c,lapply(E,getvrs)))
}



getvrs <- function(x, L=character(0), ...){
   if ( is.expression(x) ){
      x <- x[[1]]
   }  
   if ( length(x) == 1){
      if ( is.symbol(x) ) return(as.character(x))
      return(NULL)
   }

   for ( i in 2:length(x) ) L <- c(L, getvrs(x[[i]],L))
   unique(L)
}



#' Deterministic correction
#' 
#' These functions are deprecated and will be defunct as of 01.01.2015. Please see \code{\link{applyRules}} instead.
#'
#'
#' @param rules object of class \code{\link{correctionRules}} 
#' @param dat \code{data.frame}
#' @param strict If \code{TRUE}, an error is produced when the rules use variables other than in the \code{data.frame}.
#' @seealso \code{\link{correctionRules}}
#'
#' @return list with altered data (\code{$corrected}) and a list of alterations (\code{$corrections}).
#'
#' @export
correctWithRules <- function(rules, dat, strict=TRUE){
  .Deprecated(new='applyRules')
  rowbyrow(rules,dat,strict)
}

##-------------------------------------------------------------------------'
# Code of the old 'correctWithRules function'
rowbyrow <- function(rules, dat, strict=TRUE){
  if (strict){
    vars <- getVars(rules)
    I <- vars %in% names(dat)
    if (!all(I)) stop(
      sprintf("Variables '%s' in rules do not occur in data",paste(vars[!I],sep=", "))
    )
  }
  
  out <- dat
  m <- nrow(dat)
  n <- length(rules)
  row <- numeric(0)
  variable <- character(0)
  old <- character(0)
  new <- character(0)
  how <- character(0)
  vars <- colnames(dat)
  tr <- as.character(rules,oneliner=TRUE)
  
  
  
  for ( i in 1:m ){
    for ( j in 1:n ){
      d <- within(out[i,,drop=FALSE],eval(rules[[j]]))
      
      if ( !all(equal(d,out[i,])) ){
        rule <- tr[j]
        w <- which(!equal(d,out[i,]))
        row <- c(row, rep(i,length(w)))
        variable <- c(variable,vars[w])
        old <- c(old, format(unlist(out[i,w])) )
        new <- c(new, format(unlist(d[1,w])) )
        how <- c(how,rep(rule,length(w)))
        out[i,] <- d
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




##-------------------------------------------------------------------------'
# NA-robust pairwise comparison of data.frames
equal <- function(d,e){
   dNA <- is.na(d)
   eNA <- is.na(e)
   d == e & !(dNA != eNA) | (dNA & eNA) 
}

##-------------------------------------------------------------------------'
# helper function for vectorize 
set_guards <- function(x){
  e <- x[[1]]
  if ( class(e) == "{" ){
    v <- lapply(e[min(2,length(e)):length(e)], function(ex){
      ex <- as.expression(ex)
      attr(ex,'guard') <- guard(x)
      ex
    })
    return(lapply(v,set_guards))
  }
  
  if(class(e) == 'if'){
    v <- as.expression(e[[3]]) # expression
    attr(v,'guard') <- guard(x) %&% condition(e)
    v <- list(v)
    if (length(e)==4){ # there is an 'else'
      w <- as.expression(e[[4]])
      attr(w,'guard') <- guard(x) %&% not(condition(e))
      v <- list(v[[1]],w)
    }
    return(lapply(v,set_guards))
  }
  return(x) 
}

##-------------------------------------------------------------------------'
# vectorize an expression.
vectorize <- function(x){
  L <- lapply(x,function(e) set_guards(as.expression(e)))  
  expr <- unlist(L)
  guard <- rapply(L, function(e) if (is.null(attr(e,'guard'))) expression(TRUE) else attr(e,'guard') )
  list(rule=as.expression(expr), guard=as.expression(guard))
}

##-------------------------------------------------------------------------'
# apply a single vectorized rule to a dataset
apply_rule <- function(rule, dat, subset=expression(TRUE)){
  I <- eval(subset,dat)
  dat[I, ] <- within(dat[I,,drop=FALSE],eval(rule))
  dat
}  

##-------------------------------------------------------------------------'
# replace shortcircuited operators by vectorized ones
replace_shortcircuit <- function(x){
  y <- as.character(x)
  if ( any(grepl('&&|\\|\\|',y)) ){
    warning("Short-circuited operators '&&' and/or '||' replaced by vectorized operators '&' and/or '|'")
    y <- gsub('&&','&',y,fixed=TRUE)
    y <- gsub('||','|',y,fixed=TRUE)
    x <- parse(text=y)
  } 
  x
}

##-------------------------------------------------------------------------'
# apply rules and log

#' Execute a ruleset in data environment
#'
#' Apply simple replacement and derivation rules to a \code{data.frame}.
#'
#' @section Details:
#' This function applies the the \code{rules} one by one to \code{dat} and logs
#' their actions. Rules are excuted in order of occurrence in the \code{\link{ruleset}}
#' so order may matter for the final result. Rules are vectorized by default for speedy execution.
#' Deriving new variables is only possible in vectorized mode.
#' See \code{\link{ruleset}} for details on the type of rules that are allowed.
#'
#' @param rules object of class \code{\link{ruleset}} 
#' @param dat \code{data.frame}
#' @param strict If \code{TRUE}, an error is produced when the rules involve variables other than in the \code{data.frame}.
#' @param vectorize Vectorize rules before applying them? If \code{FALSE}, the rules are applied row-by-row, which can be significantly slower.
#' @seealso \code{\link{ruleset}}
#'
#' @return list with altered data (\code{$dat}) and a list of modifications (\code{$log}).
#' @example ../examples/ruleset.R
#' @export
applyRules <- function(rules, dat, strict=TRUE, vectorize=TRUE, ...){
  
  if (!vectorize) return(rowbyrow(rules,dat,strict))

  if (strict){
    vars <- getVars(rules)
    I <- vars %in% names(dat)
    if (!all(I)) stop(
      sprintf("Variables '%s' in rules do not occur in data",paste(vars[!I],sep=", "))
    )
  }
  
  rules <- replace_shortcircuit(rules)
  R <- vectorize(rules)
  
  # init log
  log <- logframe(dat,dat,'')

  # apply rules
  for ( i in 1:length(R[[1]]) ){
    dat1 <- apply_rule(rule=R$rule[i], dat=dat, subset=R$guard[i])
    if ( TRUE ){
      log <- rbind(log,logframe(
        dat
        , dat1
        , paste(R$rule[i],'because',R$guard[i])
        , as.character(R$rule[i])
      ))
    }
    dat <- dat1
  }
  list(dat=dat,log=log)
}

##-------------------------------------------------------------------------'
# Get guarding expression
guard <- function(x) attr(x,'guard')

##-------------------------------------------------------------------------'
# Conjugate two expressions
`%&%` <- function(e,f){
  if ( is.null(e) ) return(f)
  if ( is.null(f) ) return(e)
  parse(text=paste('(',e,') & (',f,')'))
}

##-------------------------------------------------------------------------'
# Negate an expression
not <- function(e){
  parse(text=paste0('!(',e,')'))
}

##-------------------------------------------------------------------------'
# return the conditional expression from an 'if' object
condition <- function(e){
  stopifnot(class(e)=='if')
  as.expression(e[[2]])
}

##-------------------------------------------------------------------------'
# row-variable-old-new-remark log
logframe <- function(dat1, dat2, remark,...){
  A <- !equal(dat1,dat2)
  rc <- which(A,arr.ind=TRUE)
  data.frame(
      row = rc[,'row']
    , variable = names(dat1)[rc[,'col']]
    , old = format(as.character(dat1[A]))
    , new = format(as.character(dat2[A]))
    , how=rep(remark,nrow(rc))
    , stringsAsFactors=FALSE
  )
}




