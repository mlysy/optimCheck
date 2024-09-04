#' `summary` method for projection plots.
#'
#' @param object An `optproj` object, i.e., output from the function [optim_proj()].
#' @template param_xnames
#' @template param_dots
#' @return An object of class `summary.optproj` inheriting from `summary.optcheck`, with elements:
#' \describe{
#'   \item{`xsol`}{The potential solution vector.}
#'   \item{`ysol`}{The value of the objective function at `xsol`.}
#'   \item{`maximize`}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{`xopt`}{A vector containing the argmax/argmin in each projection plot.}
#'   \item{`yopt`}{A vector containing the max/min in each projection plot.}
#'   \item{`xdiff`}{A two-column matrix containing the differences between `xsol` and `xopt`.  The first column is the absolute difference `D = xopt - xsol`, the second is the relative difference `R = D/|xsol|`.}
#'   \item{`ydiff`}{Same thing, but between `ysol` and `yopt`.}
#' }
#' @details The `print` methods for `summary.optproj` and `optproj` objects themselves both return a three-column matrix, consisting of the potential solution (`xsol`), the optimal solution in each projection plot (`xopt`), and the relative difference between the two (`R = (xopt - xsol)/|xsol|`).
#' @seealso [print.summary.optproj()] for `print` method.
#' @export
summary.optproj <- function(object, xnames, ...) {
  xsol <- object$xsol
  ysol <- object$ysol
  nx <- length(xsol)
  maximum <- object$maximize
  opt.fun <- if(maximum) max else min
  which.opt <- if(maximum) which.max else which.min
  if(missing(xnames)) {
    xnames <- names(xsol)
    if(is.null(xnames)) xnames <- paste0("x",1:nx)
  }
  # store argmax/argmin and max/min in each projection plot
  opt.res <- matrix(NA, nx, 2)
  for(ii in 1:nx) {
    iopt <- which.opt(object$yproj[,ii])
    opt.res[ii,] <- c(object$xproj[iopt,ii], object$yproj[iopt,ii])
  }
  # differences in solution
  xdiff <- opt.res[,1] - xsol
  xdiff <- cbind(abs = xdiff, rel = xdiff/abs(xsol))
  # differences in solution value
  ydiff <- opt.res[,2] - ysol
  ydiff <- cbind(abs = ydiff, rel = ydiff/abs(ysol))
  # add names
  names(xsol) <- xnames
  rownames(opt.res) <- xnames
  rownames(xdiff) <- xnames
  rownames(ydiff) <- xnames
  ans <- list(xsol = xsol, ysol = ysol,
              maximize = maximum,
              xopt = opt.res[,1], yopt = opt.res[,2],
              xdiff = xdiff, ydiff = ydiff)
  class(ans) <- c("summary.optproj", "summary.optcheck")
  ans
}

#--- print methods -------------------------------------------------------------

## #' @rdname summary.optproj
## #' @export
## print.summary.optproj <- function(x,
##                                    digits = max(3L, getOption("digits")-3L)) {
##   res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
##   colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
##   print(signif(res, digits = digits))
## }

## #' @rdname summary.optproj
## #' @export
## print.optproj <- function(x,
##                            digits = max(3L, getOption("digits")-3L)) {
##   osum <- summary(x)
##   print(osum, digits = digits)
## }

