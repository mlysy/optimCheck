#--- test optim on a quadratic function ----------------------------------------

require(optimCheck)

# generate an n x p matrix of iid N(0,1)
rMnorm <- function(n, p) {
  if(missing(p)) p <- n
  matrix(rnorm(n*p), n, p)
}

d <- 10 # number of dimensions

A <- crossprod(rMnorm(d))
b <- rnorm(d)
loglik <- function(x) c(-crossprod(x, A %*% x) + 2 * crossprod(b, x))

x.mle <- solve(A, b)

fit <- optim(par = x.mle * 1.1, fn = loglik,
             control = list(maxit = 1e5,
                            fnscale = -1,
                            parscale = abs(x.mle)))
fit$convergence
x.hat <- fit$par

plot(optim_proj(xsol = x.hat, fun = loglik, xrng = .1))


fit$par - mu

fit2 <- optim(par = fit$par, fn = loglik, control = list(maxit = 1e5))

plot(optim_proj(theta = fit$par, fun = loglik, theta.rng = .1))
