#' Calculates projection plots for a given objective function.
#'
#' Given the objective function of an optimization problem and a potential solution, calculates "projection plots" along each coordinate of the solution vector, with all other coordinates being fixed at the input values.
#'
#' @param theta Potential solution vector of length \code{ntheta}.
#' @param fun Objective function to be maximized (or minimized), with first argument the length-\code{ntheta} parameter vector over which optimization is to take place.  Should return a scalar result.
#' @param maximize Logical, whether a maximum or a minimum of the objective function is sought.
#' @param theta.rng Optional specification of the range of each projection plot.  Can be: (i) a \code{2 x ntheta} matrix giving the endpoints of the range, (ii) a scalar or vector of length \code{ntheta}, such that the range in each plot is \code{theta +/- theta.range * abs(theta)}.
#' @param npts Number of points in each projection plot.
#' @param equalize If \code{TRUE}, narrows the range of each plot so that the y-value in each plot is more or less the same at either endpoint.
#' @return An object of class \code{optimCheck} consisting of the elements:
#' \describe{
#'   \item{\code{theta}}{The potential solution.}
#'   \item{\code{value}}{The value of \code{fun(theta)}.}
#'   \item{\code{maximize}}{Logical; whether the potential solution should maximize or minimize the objective function.}
#'   \item{\code{x}}{An \code{npts x ntheta} matrix where each column is the \code{x}-axis of the projection plot along the given component of \code{theta}.}
#'   \item{\code{y}}{An \code{npts x ntheta} matrix where each column is the \code{y}-axis of the corresponding projection plot.}
#' }
#' @export
optim_proj <- function(theta, fun, maximize = TRUE, theta.rng = .1,
                       npts = 100, equalize = TRUE) {
  theta.sol <- theta
  ntheta <- length(theta.sol) # number of parameters
  xout <- matrix(NA, npts, ntheta) # x-axis of plots
  yout <- matrix(NA, npts, ntheta) # y-axis of plots
  ll.max <- fun(theta.sol) # maximum value
  itheta <- 1:ntheta # previous option disabled
  ## if(missing(itheta)) itheta <- 1:ntheta
  ## if(is.logical(itheta)) itheta <- which(itheta) # convert T/F's to indices
  ## if(missing(theta.names)) {
  ##   theta.names <- paste0("theta[",1:ntheta,"]")
  ##   # converts to expression so symbol "theta_i" is plotted
  ##   theta.names <- parse(text = theta.names)
  ## }
  if(!is.matrix(theta.rng)) {
    # default range is +/- .1 * max(abs(theta))
    theta.rng <- theta.rng * abs(theta.sol)
    theta.rng <- rbind(theta.sol - theta.rng, theta.sol + theta.rng)
    ## theta.rng <- rbind(theta.sol - .5 * abs(theta.sol),
    ##                    theta.sol + .5 * abs(theta.sol))
  } else {
    if(!identical(dim(theta.rng), c(2,ntheta))) {
      stop("theta.rng must be a scalar, vector of length(theta), or a 2 x length(theta) matrix.")
    }
  }
  # shorten theta.names and theta.rng if necessary
  ntheta2 <- length(itheta)
  ## if(length(theta.names) > ntheta2) theta.names <- theta.names[itheta]
  if(ncol(theta.rng) > ntheta2) theta.rng <- theta.rng[,itheta,drop=FALSE]
  ## if(plot) {
  ##   # set up plot
  ##   opar <- par(no.readonly = TRUE) # save specs of current plot
  ##   # plot size
  ##   if(missing(layout)) {
  ##     layout <- floor(sqrt(ntheta2))
  ##     layout <- c(layout, ceiling(ntheta2/layout))
  ##   }
  ##   par(mfrow = layout, mar = c(2,2.5,2.5,0), oma = c(3, 3, .5, .5))
  ##   on.exit(par(opar)) # restore plot parameters when exiting function
  ## }
  # for loop for plotting
  for(ii in 1:ntheta2) {
    ith <- itheta[ii]
    theta.seq <- seq(from = theta.rng[1,ii],
                     to = theta.rng[2,ii], len = npts)
    for(jj in 1:2) {
      # evaluate likelihood fixing all components except one
      theta.ll <- sapply(theta.seq, function(thetai) {
        theta <- theta.sol
        theta[ith] <- thetai
        fun(theta)
      })
      if(!maximize) theta.ll <- -theta.ll
      if(jj == 1 && equalize) {
        vth <- !is.na(theta.ll) & theta.ll > -Inf # valid values
        lth <- theta.seq < theta.sol[ith] # on the left of mle
        rth <- theta.seq > theta.sol[ith] # on the right
        # larger of the min value on each size
        lbd <- max(min(theta.ll[vth & lth]), min(theta.ll[vth & rth]))
        # rescale theta.seq to be on this range
        ibd <- c(which.min(ifelse(vth & lth, abs(theta.ll-lbd), Inf)),
                 which.min(ifelse(vth & rth, abs(theta.ll-lbd), Inf)))
        theta.seq <- seq(theta.seq[ibd[1]], theta.seq[ibd[2]], len = npts)
      } else break
    }
    if(!maximize) theta.ll <- -theta.ll
    # store calcs
    xout[,ith] <- theta.seq
    yout[,ith] <- theta.ll
    ## # numerical check
    ## is.mle[ii] <- ll.max >= max(theta.ll)
    ## if(plot) {
    ##   # plot loglikelihood and add estimated value
    ##   graphics::plot(theta.seq, theta.ll, type = "l",
    ##                  xlab = "", ylab = "")
    ##   title(main = theta.names[ii], cex.main = 2)
    ##   abline(v = theta.sol[ith], col = "red")
    ## }
  }
  ans <- list(theta = theta.sol, value = ll.max, maximize = maximize,
              x = xout, y = yout)
  class(ans) <- "opt_proj"
  ans
  ## if(plot) {
  ##   plot.optimCheck
  ##   ## # labels in margin
  ##   ## mtext(side = 2, text = "Log-Likelihood",
  ##   ##       line = 1, outer = TRUE)
  ##   ## mtext(side = 1, text = "Parameter",
  ##   ##       line = 1, outer = TRUE)
  ##   return(invisible(ans))
  ## } else {
  ##   return(ans)
  ## }
}

