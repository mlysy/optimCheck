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

# x'Ax - 2 b'x
objfun <- function(x) c(crossprod(x, A %*% x) - 2 * crossprod(b, x))

xopt <- solve(A, b) # analytic solution

# numerical solution using optim
xfit <- optim(par = xopt * 5, fn = objfun,
              control = list(maxit = 1e5))
xfit$convergence

oproj <- optim_proj(xsol = xfit$par, fun = objfun,
                    maximize = FALSE, xrng = .5)
plot(oproj)

oproj

#-----------------------------------------------------------------------------------------------------------------------------------------------------------

x.mle <- solve(A, b)

fit <- optim(par = rep(0, d), fn = loglik,
             control = list(maxit = 1e5,
                            fnscale = -1,
                            parscale = abs(x.mle)))
fit$convergence
x.hat <- fit$par

plot(optim_proj(xsol = x.hat, fun = loglik, xrng = .1))


fit$par - mu

fit2 <- optim(par = fit$par, fn = loglik, control = list(maxit = 1e5))

plot(optim_proj(theta = fit$par, fun = loglik, theta.rng = .1))
