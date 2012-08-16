


imputationRules <- function(x,...){
	UseMethod('imputationRules')
}


# TODO: add code referencing
imputationRules.character <- function(x, ...){
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

	structure(L,class='imputationRules')
}

imputationRules.expression <- function(x,...){
	structure(x,class='imputationRules')
}


print.imputationRules <- function(x,...){
	cat("Object of class 'imputationRules'")
	v <- sapply(x,as.character)
	v <- gsub("^","  ",v)
	v <- gsub("\n","\n  ",v)
	cat(sprintf("\n## %2d-------\n%s",1:length(v),v),'\n')
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
	for ( i in 1:m ){
		for ( r in rules ){
			d <- out[i,,drop=FALSE]
			d <- within(d,eval(r))
			if ( !all(d==out[i,]) ){
				out[i,] <- d
				rule <- gsub("\n","",as.character(r))
				rule <- gsub("[ ]+"," ",rule)
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



