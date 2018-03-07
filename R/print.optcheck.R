#--- print methods for optcheck objects ----------------------------------------

# 1. same display for summary.optcheck and optcheck, but return is different.
# 2. documentation displayed with summary.optproj and summary.optcheck.

#' @export
print.summary.optcheck <- function(x,
                                   digits = max(3L, getOption("digits")-3L)) {
  nx <- length(x$xsol)
  nmax <- min(nx, 5)
  otype <- ifelse(x$maximize, "maximization", "minimization")
  ctype <- switch(class(x)[1],
                  summary.optproj = "\'optim_proj\'",
                  summary.optrefit = "\'optim_refit\'")
  cat("\n", ctype, " check on ", nx, "-variable ", otype, " problem.\n\n",
      "Top ", nmax, " relative errors in potential solution:\n\n",
      sep = "")
  res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
  colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
  ord <- order(round(abs(res[,3]), 6), decreasing = TRUE)[1:nmax]
  print(signif(res[ord,], digits = digits))
  cat("\n")
  invisible(x)
}

#' @export
print.optcheck <- function(x,
                           digits = max(3L, getOption("digits")-3L)) {
  osum <- summary(x)
  print(osum, digits = digits)
  invisible(x)
}
