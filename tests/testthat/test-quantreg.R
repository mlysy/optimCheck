context("Quantile regression")

# quantile regression objective function
qr.obj <- function(y, X, beta, tau) {
  u <- y - c(X %*% beta)
  sum(u * (tau - (u < 0)))
}

# automated tests
ntest <- 20
test_that("quantreg::rq converges to local mode", {
  skip_if_not(requireNamespace("quantreg", quietly = TRUE),
              "quantreg package required to run this test.")
  require(quantreg)
  replicate(ntest, expr = {
    n <- sample(100:1000, 1)
    p <- sample(1:20, 1)
    X <- matrix(rnorm(n*p), n, p)
    colnames(X) <- paste0("x", 1:p)
    beta <- rnorm(p)
    y <- c(X %*% beta) + rnorm(n)
    ds <- data.frame(y = y, X)
    tau <- runif(1)
    M <- rq(y ~ . - 1, tau = tau, data = ds)
    beta.hat <- coef(M)
    ocheck <- optim_proj(fun = function(beta) {
      qr.obj(y = y, X = X, beta = beta, tau = tau)
    }, xsol = beta.hat, maximize = FALSE)
    # minimum of relative and absolute error
    err <- summary(ocheck)$xdiff
    err <- max(pmin(abs(err[,"abs"]), abs(err[,"rel"])))
    expect_less_than(err, .01) # check that its less than
    ## expect_true(all(apply(ocheck$yproj, 2, max) >= ocheck$ysol))
  })
})
