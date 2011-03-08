#' Deductive correction methods for sign, rounding and typing errors
#' 
#' Deductive correction methods for sign, rounding and typing errors
#' It also contains functionality to check if a matrix of linear restrictions is
#' totally unimodular. See also
#'
#' \itemize{
#' \item{\code{\link{correctRounding}}}
#' \item{\code{\link{old.correctSigns}}}
#' \item{\code{\link{correctTypos}}}
#' \item{\code{\link{deducorrect-object}} and \code{\link{status}} for output specification}
#' \item{\code{\link{isTotallyUnimodular}}}
#' }
#'
#'
#'
#'
#' @example examples/pkg.R
#' @name deducorrect-package 
#' @docType package 
{}


.onLoad <- function(libname, pkgname){
    cat("\nBE CAREFUL:\n")
    cat("  As of version 0.9-2, the function correctSigns is no longer backwards compatible.\n")
    cat("  Use old.correctSigns for the old interface. This function will dissapear before the release of 1.0.\n")

}

