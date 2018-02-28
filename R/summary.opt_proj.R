#' \code{summary} method for projection plots.
#'
#' @name summary.opt_proj
#' @aliases print.opt_proj print.summary.opt_proj
#' @param object An \code{opt_proj} object, i.e., output from the function \code{\link{optim_proj}}.
#' @param xnames Optional vector of names for the elements of the potential solution.
#' @return A list with elements:
#' \describe{
#'   \item{\code{xsol}}{The potential solution vector.}
#'   \item{\code{ysol}}{The value of the objective function at \code{x}.}
#'   \item{\code{opt}}{A two-row matrix of optimal values of the projection plots.  The first row corresponds to the maxizing/minimizing value of \code{x_i}, and the second is the value of the maximum/minimum itself.}
#'   \item{\code{diff}}{A two-row matrix of differences between the potential solution and the optimal value.  The first row corresponds to differences in each element of theta, and the second to differences in the objective function.}
#'   \item{\code{rel}}{A two-row matrix of relative differences between the potential solution and the optimal value (the denominator being the potential solution).  The first row corresponds to relative differences in each element of theta, and the second to relative differences in the objective function.}
#' }
#' @details The \code{print} methods for the summary and \code{opt_proj} objects themselves both return a three-column matrix, consisting of the potential solution, the optimal solution in each projection plot, and the relative difference between the two.
#' @export
summary.opt_proj <- function(object, xnames) {
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
  opt.res <- matrix(NA, 2, nx)
  for(ii in 1:nx) {
    iopt <- which.opt(object$yproj[,ii])
    opt.res[,ii] <- c(object$xproj[iopt,ii], object$yproj[iopt,ii])
  }
  diff.res <- rbind(opt.res[1,] - xsol,
                    opt.res[2,] - ysol)
  rel.res <- rbind(diff.res[1,]/abs(xsol),
                   diff.res[2,]/abs(ysol))
  # name things
  names(xsol) <- xnames
  rnames <- c("x", "y")
  rownames(opt.res) <- rnames
  rownames(diff.res) <- rnames
  rownames(rel.res) <- rnames
  colnames(opt.res) <- xnames
  colnames(diff.res) <- xnames
  colnames(rel.res) <- xnames
  ans <- list(xsol = xsol, ysol = ysol,
              opt = opt.res, diff = diff.res, reldiff = rel.res)
  class(ans) <- "summary.opt_proj"
  ans
}

#--- print methods -------------------------------------------------------------

#' @rdname summary.opt_proj
#' @export
print.summary.opt_proj <- function(x,
                                   digits = max(3L, getOption("digits")-3L)) {
  res <- cbind(x$xsol, x$diff["x",], x$reldiff["x",])
  colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
  print(signif(res, digits = digits))
}

#' @rdname summary.opt_proj
#' @export
print.opt_proj <- function(x,
                           digits = max(3L, getOption("digits")-3L)) {
  osum <- summary(x)
  print(osum, digits = digits)
}

