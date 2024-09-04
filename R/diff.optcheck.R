#' Elementwise difference between potential and optimal solutions.
#'
#' @aliases diff.summary.optcheck diff.optproj diff.summary.optproj diff.optrefit  diff.summary.optrefit
#'
#' @template param_x
#' @template param_dots
#' @return A two-column matrix consisting of the absolute and relative differences between the potential and optimal solutions (`xsol` and `xopt`).
#' @details This function is simply a wrapper to `summary(x)$xdiff` and `x$xdiff`, for `optcheck` and `summary.optcheck` objects respectively.
#' @export
diff.optcheck <- function(x, ...) {
  xdiff <- summary(x)$xdiff
  ## if(is.null(names(x$xsol))) {
  ##   # only add names if they are provided
  ##   rownames(xdiff) <- NULL
  ## }
  xdiff
}

#' @rdname diff.optcheck
#' @export
diff.summary.optcheck <- function(x, ...) {
  x$xdiff
}
