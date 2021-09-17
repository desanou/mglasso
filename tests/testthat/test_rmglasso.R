# library(Rmglasso)

library(Matrix)
library(mvtnorm)

n = 50
K = 3
p = 9
rho = 0.85
blocs <- list()
for (j in 1:K) {
  bloc <- matrix(rho, nrow = p/K, ncol = p/K)
  for(i in 1:(p/K)) { bloc[i,i] <- 1 }
  blocs[[j]] <- bloc
}

mat.covariance <- bdiag(blocs)
mat.covariance
## Sparsity
## Sim
set.seed(11)
X <- rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
X <- scale(X)

res <- conesta_rwrapper(X, 0.1, 0.1)
