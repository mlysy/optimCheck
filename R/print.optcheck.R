#--- print methods for optcheck objects ----------------------------------------

#' @export
print.summary.optcheck <- function(x,
                                   digits = max(3L, getOption("digits")-3L)) {
  res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
  colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
  print(signif(res, digits = digits))
}

#' @export
print.optcheck <- function(x,
                           digits = max(3L, getOption("digits")-3L)) {
  osum <- summary(x)
  print(osum, digits = digits)
}
