
#' Create empty status vector
#' 
#'
#' Every function in \code{\link[=deducorrect-package]{deducorrect}} returns the status of every row after treatment.
#' The status vector is an \code{ordered} factor with levels
#'
#' \tabular{ll}{
#'  \code{invalid}      \tab record is invalid but could not be corrected\cr
#'  \code{partial}    \tab record violates less edits then before entering the function\cr
#'  \code{corrected}    \tab record satisfies all edit restrictions after correction\cr
#'  \code{valid}        \tab record violates no edit restrictions\cr
#' }
#' where \code{invalid < partial < corrected < valid}
#' 
#'
#'
#' This function is \code{deducorrect} internal.
#'
#' @title Create empty status vector
#' @param n length of status vector
#' @param ini initial value, defaults to \code{NA}
#' @return an ordered factor with levels mentioned under details
#'
#' @example examples/status.R
#'
status <- function(n, ini=NA){
    st <- c("invalid", "partial", "corrected", "valid")
    if ( ! ini %in% c(st,NA) ){
        stop(paste("invalid status level, allowed are NA, ",paste(st,collapse=", ")))
    }
    return(ordered(rep(ini,n), levels=st))
}


#' Get name of R user.
#'
#' This function tries to get the name of the user from the system on which R is running.
#' It first tries the \code{whoami} command, which will work on a \code{*nix} like system, and probably on OSX as well (not tested).
#' If this fails, it looks for the \code{USER} (first) and \code{USERNAME} environment variables. Please note that this may
#' or may not work as expected when running R on a server, or as webservice.
#' 
#' This function is not exported.
#' 
#' @title Get name of R user. 
#' 
#' @return The username or \code{NA}
#' @nord
getUsername <- function(){
    
    # the *nix case
    name <- tryCatch(system("whoami", intern=TRUE), warning=function(w) NA, error=function(NA) )
    # the other case(s)
    if ( is.na(name)) {
        v <- Sys.getenv()
        names(v) <- toupper(names(v))
        if ( "USER" %in% names(v) ){
            name <- v["USER"]
        } else if ( "USERNAME" %in% names(v) ){
            name <- v["USERNAME"]
        } 
        names(name) <- NULL
    }
    return(name)
}





