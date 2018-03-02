#--- test mclust ---------------------------------------------------------------

context("Gaussian mixture models")

source("mclust-testfunctions.R")

# loglikelihood on flattened parameter scale
loglik <- function(theta, d, G, ydata) {
  parameters <- theta2par(theta, d, G)
  # simplex-valued probabilities
  if(any(parameters$pro < 0)) return(-Inf) # always sums to 1
  ## # positive-definite variance matrices
  if(any(diag(parameters$variance$cholSigma) <= 0)) return(-Inf)
  sum(dens("EEE", data = ydata,
           parameters = parameters, logarithm = TRUE))
}

ntest <- 5
test_that("mclust::emEEE converges to local mode.", {
  skip_if_not(requireNamespace("mclust", quietly = TRUE),
              "mclust package required to run this test.")
  require(mclust)
  replicate(n = ntest, expr = {
    G <- sample(2:4, 1) # number of components
    d <- sample(2:4, 1) # number of dimensions
    n <- sample(400:600,1) # number of observations
    # simulate data
    # true parameter values
    parameters <- list(pro = rDirichlet(1, rep(1, G)),
                       mean = rMnorm(d, G) + matrix(10 * 1:G, d, G,
                                                    byrow = TRUE),
                       variance = list(modelName = "EEE", d = d, G = G,
                                       cholSigma = chol(crossprod(rMnorm(d)))))
    y <- simEEE(parameters, n = n) # data
    # calculate MLE
    fit <- emEEE(data = y[,-1], parameters = parameters) # fit model
    par.mle <- fit$parameters # convert parameters to MLE
    par.mle$variance$cholSigma <- chol(par.mle$variance$Sigma)
    theta.mle <- par2theta(par.mle)
    # projection plots
    ocheck <- optim_proj(fun = function(theta) {
      loglik(theta, d, G, y[,-1])
    }, xsol = theta.mle, xrng = .1, npts = 50)
    # minimum of relative and absolute error
    err <- summary(ocheck)$xdiff
    err <- max(pmin(abs(err["abs",]), abs(err["rel",])))
    expect_lt(err, .01)
  })
})

## all.equal(parameters, theta2par(par2theta(parameters), d, G))
## parameters2 <- theta2par(par2theta(parameters), d, G)
## identical(parameters$pro, parameters2$pro)
## range(parameters$mean - parameters2$mean)
## range(parameters$variance$cholsigma - parameters2$variance$cholsigma)

# simulate data


## theta.mle <- par2theta(par.mle)
## loglik(theta.mle)

## system.time({
##   ocheck <- optim_refit(xsol = theta.mle, fun = loglik, reltol = 1e-7)
## })

## err <- summary(ocheck)$xdiff
## max(pmin(abs(err["abs",]), abs(err["rel",])))

## system.time({
##   ocheck <- optim_proj(fun = loglik, xsol = theta.mle, npts = 50, xrng = .5)
## })

## plot(ocheck, xnames = parse(text = theta.names(d, G)), equalize = FALSE)

## system.time({
##   ocheck2 <- optim_refit(xsol = theta.mle, fun = loglik, reltol = 5e-8)
## })
