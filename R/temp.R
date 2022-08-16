# #install_conesta(py_version = '3.8')
# # Sys.which("python")
# py_version()
# use_python_version(version = '3.8.')
# # Error in pyenv_python(version) :
# #   Python 3.8.0 does not appear to be installed.
# # Try installing it with install_python(version = "3.8.0")
# install_python(version = "3.8.0")
# use_python_version(version = '3.8.0')
# reticulate::py_config()
#

install_conesta
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

mat.covariance <- Matrix::bdiag(blocs)
mat.covariance

set.seed(11)
X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
X <- scale(X)

res <- mglasso(X, 0.1, lambda2_start = 0.1)
res$out[[1]]$clusters
res$out[[1]]$beta
