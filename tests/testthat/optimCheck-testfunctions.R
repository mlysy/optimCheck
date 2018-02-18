
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
