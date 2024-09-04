#' `summary` method for `optrefit` objects.
#'
#' @param object An `optrefit` object, i.e., output from the function [optim_refit()].
#' @param xnames Optional vector of names for the elements of the potential solution.
#' @param ... Further arguments to be passed to or from other methods.
#' @return An object of class `summary.optrefit` inheriting from `summary.optcheck`, with elements:
#' \describe{
#'   \item{`xsol`}{The potential solution vector.}
#'   \item{`ysol`}{The value of the objective function at `xsol`.}
#'   \item{`maximize`}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{`xopt`}{A vector containing the argmax/argmin in each projection plot.}
#'   \item{`yopt`}{The scalar value of the max/min found by `optim_refit`.}
#'   \item{`xdiff`}{A two-column matrix containing the differences between `xsol` and `xopt`.  The first column is the absolute difference `D = xopt - xsol`, the second is the relative difference `R = D/|xsol|`.}
#'   \item{`ydiff`}{A length-two vector containing the absolute and relative difference between `ysol` and `yopt`.}
#' }
#' @seealso [print.summary.optcheck()] for `print` method.
#' @export
summary.optrefit <- function(object, xnames, ...) {
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
              maximize = object$maximize,
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

