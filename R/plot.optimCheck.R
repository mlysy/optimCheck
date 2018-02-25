#' Projection plots for optimization routines.
#'
#' @param x An \code{optimCheck} object, i.e., output from function \code{\link{optim_check}}.
#' @param theta.names Optional vector of parameter names for plotting.
#' @param itheta indices of one dimensional functions to evaluate and plot.  Defaults to all parameters.
#' @param layout Optional vector giving the number of rows and columns in the plot.  For \code{ntheta} parameters, defaults to \code{c(nr, nc)}, where \code{nr = floor(ntheta)} and \code{nc = ceiling(ntheta/nr)}.
#' @return A grid of projection plots, with vertical lines at the potential solution.
#' @export
plot.optimCheck <- function(x, theta.names, itheta, layout) {
  theta.sol <- x$theta
  ntheta <- length(theta.sol)
  xout <- x$x
  yout <- x$y
  # which projections to plot
  if(missing(itheta)) itheta <- 1:ntheta
  if(is.logical(itheta)) itheta <- which(itheta) # convert T/F's to indices
  ntheta2 <- length(itheta)
  # plot titles
  if(missing(theta.names)) {
    theta.names <- paste0("theta[",1:ntheta,"]")
    # converts to expression so symbol "theta_i" is plotted
    theta.names <- parse(text = theta.names)
  }
  # set up plot region
  opar <- par(no.readonly = TRUE) # save specs of current plot
  on.exit(par(opar)) # restore plot parameters when exiting function
  # plot size
  if(missing(layout)) {
    layout <- floor(sqrt(ntheta2))
    layout <- c(layout, ceiling(ntheta2/layout))
  }
  par(mfrow = layout, mar = c(2,2.5,2.5,0), oma = c(3, 3, .5, .5))
  # plot itself
  for(ii in 1:ntheta2) {
    ith <- itheta[ii]
    plot(xout[,ith], yout[,ith], type = "l",
         xlab = "", ylab = "")
    title(main = theta.names[ith], cex.main = 2)
    abline(v = theta.sol[ith], col = "red")
  }
  # labels in margin
  mtext(side = 2, text = "Objective Function",
        line = 1, outer = TRUE)
  mtext(side = 1, text = "Parameter",
        line = 1, outer = TRUE)
}
