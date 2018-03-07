#' Elementwise difference between potential and optimal solutions.
#'
#' @aliases diff.optproj diff.optrefit diff.summary.optproj diff.summary.optrefit
#' @param x An object of class \code{optproj}, \code{optrefit}, \code{summary.optproj}, or \code{summary.optrefit}.
#' @return The minimum of the absolute and relative difference between each element of the potential solution (\code{xsol}) and the optimial solution (\code{xopt}) obtained from either \code{\link{optim_proj}} or \code{\link{optim_refit}}.
#' @export
diff.optcheck <- function(x) {
  xerr <- abs(summary(x)$xdiff) # absolute and relative differences
  xerr <- pmin(xerr[,"abs"], xerr[,"rel"]) # elementwise min between the two
  if(is.null(names(x$xsol))) {
    # only add names if they are provided
    names(xerr) <- NULL
  }
  xerr
}

#' @export
diff.summary.optcheck <- function(x) {
  xerr <- abs(summary(x)$xdiff) # absolute and relative differences
  xerr <- pmin(xerr[,"abs"], xerr[,"rel"]) # elementwise min between the two
  xerr
}
