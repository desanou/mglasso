---
title: "Multiscale GLasso"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Multiscale GLasso}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---


Inference of Multiscale Gaussian Graphical Models.  

## Installation  

```r
remotes::install_github("desanou/mglasso")
library(mglasso)
```


```r
library(mglasso)
install_conesta()
```


## Basic Usage

1. Simulate some block diagonal model  

```r
library(Matrix)
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
Matrix::image(mat.covariance)
```

1.1 True cluster partition

```r
rep(1:3, each = 3)
```


1.2. Simulate gaussian data from the covariance matrix 

```r
set.seed(11)
X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
```


2. Launch algorithm  

```r
X <- scale(X)    
res <- mglasso(X, lambda1 = 0.1, lambda2_start = 0.1, fuse_thresh = 1e-3)
```
3. Plot results compact version

3.1 Estimated regression vectors

```r
plot_mglasso(res)
```

`level9` denotes a partition with `9` clusters.
We observe a shrinkage effect in the estimated coefficients due to the fuse-group lasso penalty parameter.

3.2 Estimated clustering partitions  

```r
res$out$level9$clusters
res$out$level7$clusters
res$out$level4$clusters
res$out$level3$clusters
res$out$level1$clusters
```
The uncovered partition obtained while increasing $\lambda_2$ is a hierarchical partition under some constraints.

# Reference 
Edmond, Sanou; Christophe, Ambroise; Geneviève, Robin; (2022): Inference of Multiscale Gaussian Graphical Model. ArXiv. Preprint. https://doi.org/10.48550/arXiv.2202.05775
