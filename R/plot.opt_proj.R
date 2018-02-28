#' Projection plots for optimization routines.
#'
#' @param x An \code{opt_proj} object, i.e., output from function \code{\link{optim_proj}}.
#' @param xnames Optional vector of element names of potential solution for plot titles.
#' @param xind Integer or logical vector of indices indicating which projections should be plotted.  Defaults to all projection plots.
#' @param layout Optional vector giving the number of rows and columns in the plot layout.  For \code{nx} plots, defaults to \code{c(nr, nc)}, where \code{nr = floor(nx)} and \code{nc = ceiling(nx/nr)}.
#' @param xlab,ylab Outer x-axis and y-axis labels.
#' @return A grid of projection plots, with vertical lines at the potential solution.
#' @export
plot.opt_proj <- function(x, xnames, xind, layout, xlab, ylab) {
  xsol <- x$xsol
  nx <- length(xsol)
  xout <- x$xproj
  yout <- x$yproj
  # which projections to plot
  if(missing(xind)) xind <- 1:nx
  if(is.logical(xind)) xind <- which(xind) # convert T/F's to indices
  nx2 <- length(xind)
  # plot titles
  if(missing(xnames)) {
    xnames <- paste0("x[",1:nx,"]")
    # converts to expression so symbol "theta_i" is plotted
    xnames <- parse(text = xnames)
  }
  # set up plot region
  opar <- par(no.readonly = TRUE) # save specs of current plot
  on.exit(par(opar)) # restore plot parameters when exiting function
  # plot size
  if(missing(layout)) {
    layout <- floor(sqrt(nx2))
    layout <- c(layout, ceiling(nx2/layout))
  }
  par(mfrow = layout, mar = c(2,2.5,2.5,0), oma = c(3, 3, .5, .5))
  # plot itself
  for(ii in 1:nx2) {
    ix <- xind[ii]
    plot(xout[,ix], yout[,ix], type = "l",
         xlab = "", ylab = "")
    title(main = xnames[ix], cex.main = 2)
    abline(v = xsol[ix], col = "red")
  }
  # labels in margin
  if(missing(xlab)) xlab <- "Parameter"
  if(missing(ylab)) ylab <- "Objective Function"
  mtext(side = 2, text = ylab, line = 1, outer = TRUE)
  mtext(side = 1, text = xlab, line = 1, outer = TRUE)
}
