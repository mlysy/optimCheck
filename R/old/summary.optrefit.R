#' `summary` method for `optrefit` objects.
#'
#' @name summary.optrefit
#' @aliases print.optrefit print.summary.optrefit
#' @param object An `optrefit` object, i.e., output from the function [optim_refit()].
#' @param xnames Optional vector of names for the elements of the potential solution.
#' @return A list with elements:
#' \describe{
#'   \item{`xsol`}{The potential solution vector.}
#'   \item{`ysol`}{The value of the objective function at `xsol`.}
#'   \item{`xopt`}{A vector containing the argmax/argmin in each projection plot.}
#'   \item{`yopt`}{A vector containing the max/min in each projection plot.}
#'   \item{`xdiff`}{A two-column matrix containing the differences between `xsol` and `xopt`.  The first column is the absolute difference `D = xopt - xsol`, the second is the relative difference `R = D/|xsol|`.}
#'   \item{`ydiff`}{A length-two vector containing the absolute and relative difference between `ysol` and `yopt`.}
#' }
#' @export
summary.optrefit <- function(object, xnames) {
  xsol <- object$xsol
  ysol <- object$ysol
  nx <- length(xsol)
  if(missing(xnames)) {
    xnames <- names(xsol)
    if(is.null(xnames)) xnames <- paste0("x",1:nx)
  }
  # optimal value
  xopt <- object$xopt
  yopt <- object$yopt
  # differences in solution
  xdiff <- xopt - xsol
  xdiff <- cbind(abs = xdiff, rel = xdiff/abs(xsol))
  # differences in solution value
  ydiff <- yopt - ysol
  ydiff <- c(abs = ydiff, rel = ydiff/abs(ysol))
  ans <- list(xsol = xsol, ysol = ysol,
              xopt = xopt, yopt = yopt,
              xdiff = xdiff, ydiff = ydiff)
  class(ans) <- c("summary.optrefit", "summary.optcheck")
  ans
}

#--- print methods -------------------------------------------------------------

## #' @rdname summary.optrefit
## #' @export
## print.summary.optrefit <- function(x,
##                                    digits = max(3L, getOption("digits")-3L)) {
##   res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
##   colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
##   print(signif(res, digits = digits))
## }

## #' @rdname summary.optrefit
## #' @export
## print.optrefit <- function(x,
##                             digits = max(3L, getOption("digits")-3L)) {
##   osum <- summary(x)
##   print(osum, digits = digits)
## }

