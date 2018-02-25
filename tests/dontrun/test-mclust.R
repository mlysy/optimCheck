require(mclust)

# simulate random vector(s) from dirichlet distribution
rDirichlet <- function(n, alpha) {
  p <- length(alpha)
  X <- matrix(rgamma(n*p, shape = alpha), p, n)
  drop(t(X)/colSums(X))
}

# generate an n x p matrix of iid N(0,1)
rMnorm <- function(n, p) {
  if(missing(p)) p <- n
  matrix(rnorm(n*p), n, p)
}

# convert back and forth between "flattened" parameter representation,
# i.e., theta = c(pro[-G], mean, upper.tri(variance))
par2theta <- function(parameters) {
  c(parameters$pro[-length(parameters$pro)],
    parameters$mean,
    apply(parameters$variance$cholsigma, 3,
          function(cs) cs[upper.tri(cs, diag = TRUE)]))
}

theta2par <- function(theta, d, G) {
  pro <- theta[1:(G-1)]
  pro <- c(pro, 1 - sum(pro))
  nchol <- d*(d+1)/2
  cholsigma <- matrix(theta[(G-1)+d*G + 1:(G*nchol)], nchol, G)
  cholsigma <- apply(cholsigma, 2, function(cs) {
    CS <- matrix(0, d, d)
    CS[upper.tri(CS, diag = TRUE)] <- cs
    CS
  })
  cholsigma <- array(cholsigma, dim = c(d,d,G))
  list(pro = pro,
       mean = matrix(theta[(G-1) + 1:(d*G)], d, G),
       variance = list(modelName = "VVV", d = d, G = G,
                       cholsigma = cholsigma))
}

G <- 5 # number of components
d <- 3 # number of dimensions
n <- 200 # number of observations

# model parameters
parameters <- list(pro = rDirichlet(1, rep(1, G)),
                   mean = rMnorm(d, G),
                   variance = list(modelName = "VVV", d = d, G = G,
                                   cholsigma = replicate(G, {
                                     chol(crossprod(rMnorm(d)))
                                   })))

all.equal(parameters, theta2par(par2theta(parameters), d, G))
## parameters2 <- theta2par(par2theta(parameters), d, G)
## identical(parameters$pro, parameters2$pro)
## range(parameters$mean - parameters2$mean)
## range(parameters$variance$cholsigma - parameters2$variance$cholsigma)

# simulate data
y <- simVVV(parameters, n = n)

# fit model
fit <- emVVV(data = y[,-1], parameters = parameters)

# convert parameters to MLE
par.mle <- fit$parameters
par.mle$variance$cholsigma <- array(apply(par.mle$variance$sigma, 3, chol),
                                    dim = c(d, d, G))

loglik <- function(theta) {
  parameters <- theta2par(theta, d, G)
  # simplex-valued probabilities
  if(any(parameters$pro < 0)) return(-Inf) # always sums to 1
  # positive-definite variance matrices
  if(any(apply(parameters$variance$cholsigma, 3, diag) <= 0)) return(-Inf)
  sum(dens("VVV", data = y[,-1],
           parameters = parameters, logarithm = TRUE))
}

theta.mle <- par2theta(par.mle)
loglik(theta.mle)

theta.names <- function(d, G) {
  rho <- paste0("rho[", 1:(G-1), "]")
  ind <- as.matrix(expand.grid(d = 1:d, G = 1:G))
  mu <- paste0("mu[", ind[,"d"], ind[,"G"], "]")
  idd <- as.matrix(expand.grid(d1 = 1:d, d2 = 1:d))
  ind2 <- as.matrix(expand.grid(dd = which(upper.tri(diag(d), diag = TRUE)),
                                G = 1:G))
  Sigma <- paste0("Sigma[",
                  idd[ind2[,"dd"],"d1"],
                  idd[ind2[,"dd"],"d2"], "]^(", ind2[,"G"], ")")
  c(rho, mu, Sigma)
}


system.time({
  ocheck <- optim_check(fun = loglik, theta = theta.mle)
})

plot(ocheck, theta.names = parse(text = theta.names(d, G)),
     itheta = (G-1) + 1:(G*d))

# absolute error
aerr <- ocheck$value - apply(ocheck$y, 2, max)
