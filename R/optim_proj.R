#' Projection plot test.
#'
#' Given the objective function of an optimization problem and a potential solution, calculates "projection plots" along each coordinate of the solution vector, with all other coordinates being fixed at the input values.
#'
#' @template param_xsol
#' @template param_fun
#' @template param_maximize
#' @param xrng Optional specification of the range of each projection plot.  Can be: (i) a `2 x nx` matrix giving the endpoints of the range, (ii) a scalar or vector of length `nx`, such that the range in each plot is `theta +/- xrange * abs(theta)`.
#' @param npts Number of points in each projection plot.
#' @param plot Logical, whether or not to display the projection plots or just return their contents.
#' @param ... Further arguments to pass to the `plot` method (see [plot.optproj()]).
#' @return An object of class `optproj` inheriting from `optcheck` (returned invisibly if `plot = TRUE`, with elements:
#' \describe{
#'   \item{`xsol`}{The potential solution.}
#'   \item{`ysol`}{The value of `fun(xsol)`.}
#'   \item{`maximize`}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{`xproj`}{An `npts x nx` matrix where each column is the `x`-axis of the projection plot along the given component of `theta`.}
#'   \item{`yproj`}{An `npts x nx` matrix where each column is the `y`-axis of the corresponding projection plot.}
#' }
#' @seealso `plot`, `summary`, `print`, and `diff` methods for projection plots are available; see [plot.optproj()], [summary.optproj()], [print.optproj()], and [diff.optproj()].
#' @export
optim_proj <- function(xsol, fun, maximize = TRUE, xrng = .1,
                       npts = 100, plot = TRUE, ...) {
  nx <- length(xsol) # number of parameters
  xproj <- matrix(NA, npts, nx) # x-axis of plots
  yproj <- matrix(NA, npts, nx) # y-axis of plots
  ## equalize <- FALSE # disabled
  if(!is.matrix(xrng)) {
    # default range is +/- .1 * max(abs(xsol))
    xrng <- xrng * abs(xsol)
    xrng <- rbind(xsol - xrng, xsol + xrng)
  } else {
    if(!all(dim(xrng) == c(2,nx))) {
      stop("xrng must be a scalar, vector of length(xsol), or a 2 x length(xsol) matrix.")
    }
  }
  for(ii in 1:nx) {
    xseq <- seq(from = xrng[1,ii],
                to = xrng[2,ii], len = npts)
    ## for(jj in 1:2) {
    # evaluate likelihood fixing all components except one
    yval <- sapply(xseq, function(xi) {
      x <- xsol
      x[ii] <- xi
      fun(x)
    })
    ## if(!maximize) yval <- -yval
    ##   if(jj == 1 && equalize) {
    ##     xseq <- .equalize_xlim(xseq, yval, xsol[ii])
    ##     xseq <- seq(xseq[1], xseq[2], len = npts)
    ##   } else break
    ## }
    ## if(!maximize) yval <- -yval
    # store calcs
    xproj[,ii] <- xseq
    yproj[,ii] <- yval
  }
  ans <- list(xsol = xsol, ysol = fun(xsol), maximize = maximize,
              xproj = xproj, yproj = yproj)
  class(ans) <- c("optproj", "optcheck")
  if(plot) {
    plot.optproj(ans, ...)
    return(invisible(ans))
  } else return(ans)
}
