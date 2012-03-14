
# revert corrections.
# NOTE: it is assumed that actual corrections have taken place. The re
# d     : deducorrect object
# rows  : logical or integer vector indexing records to be reverted
#
revert <- function(d, rows){
    if (missing(rows)) rows <- 1:nrow(d$corrected)
    if ( is.logical(rows) )  rows <- which(rows)

    rows <- 1:nrow(d$corrected)
    status <- d$status
    rows <- rows[status$status[rows] %in% c('corrected','partial')]
    corr <- d$corrections
    cord <- d$corrected
    irws <- corr$row %in% rows
    irow <- corr$row[irws]

    cls <- as.character(corr[irws,'variable'])
    vars <- unique(cls)
    icol <- match(cls,vars)
    A <- as.matrix(cord[vars])

    A[cbind(irow,icol)] <- corr[irws,'old']
    cord[vars] <- A[,vars,drop=FALSE]

    status[rows,'status'] <- "invalid"

    newdeducorrect(
        corrected = cord,
        corrections=corr[!irws,,drop=FALSE],
        status=status
    )
}



