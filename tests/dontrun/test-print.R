# simple check

# likelihood function
loglik <- function(beta, y, X) {
  sum(dbinom(y, size = 1,
             prob = binomial()$linkinv(X %*% beta), log = TRUE))
}

# generate data
n <- sample(100:200,1)
p <- sample(2:10,1)
X <- matrix(rnorm(n*p),n,p)
beta0 <- rnorm(p, sd = .1)
# response
y <- rbinom(n, size = 1, prob = binomial()$linkinv(X %*% beta0))

# fit glm
beta.hat <- coef(glm(y ~ X - 1, family = binomial))

oproj <- optim_proj(fun = function(beta) loglik(beta, y, X),
                    xsol = beta.hat)

orefit <- optim_refit(fun = function(beta) loglik(beta, y, X),
                      xsol = beta.hat)
