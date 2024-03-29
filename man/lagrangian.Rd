% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmpt-cost.R, R/cost.R
\name{lagrangian}
\alias{lagrangian}
\title{lagrangian function}
\usage{
lagrangian(Beta, X, lambda1 = 0, lambda2 = 0)

lagrangian(Beta, X, lambda1 = 0, lambda2 = 0)
}
\arguments{
\item{Beta}{numeric matrix. In rows, regression vectors coefficients following of node-wise regression. diag(Beta) = 0}

\item{X}{numeric matrix. Data with variables in columns.}

\item{lambda1}{numeric scalar. Lasso penalization parameter.}

\item{lambda2}{numeric scalar. Fused-group Lasso penalization parameter.}
}
\value{
numeric scalar. The lagrangian

numeric scalar. The lagrangian
}
\description{
Beta and X must have the same number of variables

Beta and X must have the same number of variables
}
\examples{

## Generation of K block partitions
n = 50
K = 3
p = 6
rho = 0.85
blocs <- list()
for (j in 1:K) {
   bloc <- matrix(rho, nrow = p/K, ncol = p/K)
   for(i in 1:(p/K)) { bloc[i,i] <- 1 }
   blocs[[j]] <- bloc
   }
mat.covariance <- bdiag(blocs)
set.seed(11)
X <- rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
X <- scale(X)

## Initialization for Beta
Beta1 <- matrix(0, nrow = p, ncol = p)
for(i in 1:p){
  Beta1[i,-i] <- solve(t(X[,-i])\%*\%X[,-i]) \%*\% t(X[,-i]) \%*\% X[,i]
  }
lagrangian(Beta, X, 0, 0)

## Generation of K block partitions
n = 50
K = 3
p = 6
rho = 0.85
blocs <- list()
for (j in 1:K) {
   bloc <- matrix(rho, nrow = p/K, ncol = p/K)
   for(i in 1:(p/K)) { bloc[i,i] <- 1 }
   blocs[[j]] <- bloc
   }
mat.covariance <- bdiag(blocs)
set.seed(11)
X <- rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
X <- scale(X)

## Initialization for Beta
Beta1 <- matrix(0, nrow = p, ncol = p)
for(i in 1:p){
  Beta1[i,-i] <- solve(t(X[,-i])\%*\%X[,-i]) \%*\% t(X[,-i]) \%*\% X[,i]
  }
lagrangian(Beta, X, 0, 0)
}
