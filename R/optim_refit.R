#' Refined optimization test.
#'
#' If the potential solution is indeed a local optimum of the objective function, and if it is used to initialize a second optimization, then original and "refined" solutions ought to be close.
#'
#' @template param_xsol
#' @template param_fun
#' @template param_maximize
#' @param maxit Maximum number of iterations for [stats::optim()] refit (see Details).
#' @param reltol Relative tolerance for convergence of [stats::optim()] refit (see Details).
#' @param xopt Optional refit solution calculated externally from an optimization algorithm of choice (see Details).
#' @return An object of class `optrefit` inheriting from `optcheck`, with elements:
#' \describe{
#'   \item{`xsol`}{The potential solution.}
#'   \item{`ysol`}{The value of `fun(xsol)`.}
#'   \item{`maximize`}{Logical indicating whether the potential solution should maximize or minimize the objective function.}
#'   \item{`xopt`}{The solution found by the general-purpose optimizer.}
#'   \item{`yopt`}{The function value at the optimal solution, i.e., `fun(xopt)`.}
#' }
#' @details By default, a so-called **refi**ned op(**t**)imization (or refit) test is performed by running the default Nelder-Mead simplex method provided by [stats::optim()], initialized by the potential solution `xsol`.  Only a simplified interface to [stats::optim()]'s control parameters are provided here.
#'
#' Alternatively, the refit test can be performed with any optimization algorithm of choice.  This is done externally, with the refined solution passed to `optim_refit()` via the argument `xopt`.
#' @seealso `summary`, `print`, and `diff` for `optrefit` objects are available; see [summary.optrefit()], [print.optrefit()], and [diff.optrefit()].
#' @export
optim_refit <- function(xsol, fun, maximize = TRUE,
                        maxit = 5e3, reltol = 1e-8, xopt) {
  if(missing(xopt)) {
    ans <- optim(par = xsol, fn = fun, method = "Nelder-Mead",
                 control = list(fnscale = ifelse(maximize, -1, 1),
                                parscale = abs(xsol),
                                maxit = maxit, reltol = reltol))
    if(ans$convergence == 1) {
      warning("Iteration limit maxit has been reached.")
    }
    if(ans$convergence == 10) {
      warning("Degeneracy of the Nelder-Mead simplex.")
    }
  } else {
    ans <- list(par = xopt, value = fun(xopt))
  }
  ans <- list(xsol = xsol, ysol = fun(xsol),
              maximize = maximize,
              xopt = ans$par, yopt = ans$value)
  class(ans) <- c("optrefit", "optcheck")
  ans
}
