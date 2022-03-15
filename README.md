
[![R-CMD-check](https://github.com/desanou/mglasso/actions/workflows/basic.yml/badge.svg)](https://github.com/desanou/mglasso/actions/workflows/basic.yml)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Method

This repository provides an implementation of the `MGLasso` (Multiscale
Graphical Lasso) algorithm: a novel approach for estimating sparse
Gaussian Graphical Models with the addition of a group-fused Lasso
penalty.

`MGLasso` is described in the paper [Inference of Multiscale Gaussian
Graphical Model](https://desanou.github.io/multiscale_glasso/).
`MGLasso` has three major contributions:

-   We simultaneously infer a network and estimate a clustering
    structure by combining the [neighborhood selection](https://arxiv.org/abs/math/0608017)
    approach (Meinshausen and Bühlman, 2006)
    and [convex clustering](https://www.di.ens.fr/~fbach/419_icmlpaper.pdf)
    (Hocking et al. 2011).

-   We use a continuation with Nesterov smoothing in a
    shrinkage-thresholding algorithm
    ([`CONESTA`](https://arxiv.org/abs/1605.09658), Hadj-Selem et
    al. 2018) to solve the optimization problem .

-   We show numerically that `MGLasso` performs better than
    [`GLasso`](https://arxiv.org/abs/0708.3517)(Friedman et al. 2007) in
    terms of support recovery.

`MGLasso` package available on
[CRAN](https://CRAN.R-project.org/package=mglasso) is based on the
python implementation of the solver `CONESTA` available in
[pylearn-parsimony](https://github.com/neurospin/pylearn-parsimony)
library.

## Package requirements

-   Install the `reticulate` package.

``` r
install.packages('reticulate')
```

-   Check if Python engine is available on the system

``` r
reticulate::py_available()
```

-   If not available

``` r
reticulate::install_miniconda()
```

-   Install `MGLasso`

``` r
install.packages('mglasso')
```

-   Install `MGLasso` python dependencies

``` r
mglasso::install_conesta()
```

An example of use is given below.

## Vignette

### Installation

``` r
remotes::install_github("desanou/mglasso")
library(mglasso)
```

``` r
library(mglasso)
#> Welcome to MGLasso.
#> Run install_conesta() to finalize the package python dependencies installation.
install_conesta()
#> mglasso requires the r-reticulate virtual environment. Attempting to create...
#> virtualenv: r-reticulate
#> Using virtual environment 'r-reticulate' ...
#> Installing pylearn-parsimony
#> pylearn-parsimony is installed.
```

### Basic Usage

1.  Simulate some block diagonal model

``` r
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

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" />

-   True cluster partition

``` r
rep(1:3, each = 3)
#> [1] 1 1 1 2 2 2 3 3 3
```

-   Simulate gaussian data from the covariance matrix

``` r
set.seed(11)
X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
```

2.  Launch algorithm

``` r
X <- scale(X)    
res <- mglasso(X, lambda1 = 0.1, lambda2_start = 0.1, fuse_thresh = 1e-3)
#> [1] 0.1
#> nclusters = 9 lambda2 0 cost = 85.41681 
#> [1] 0.1
#> nclusters = 9 lambda2 0.1 cost = 85.73135 
#> [1] 0.1
#> nclusters = 9 lambda2 0.15 cost = 85.98468 
#> [1] 0.1
#> nclusters = 9 lambda2 0.225 cost = 86.45847 
#> [1] 0.1
#> nclusters = 9 lambda2 0.3375 cost = 87.3288 
#> [1] 0.1
#> nclusters = 9 lambda2 0.50625 cost = 88.86085 
#> [1] 0.1
#> nclusters = 9 lambda2 0.759375 cost = 91.41304 
#> [1] 0.1
#> nclusters = 9 lambda2 1.139063 cost = 95.42645 
#> [1] 0.1
#> nclusters = 9 lambda2 1.708594 cost = 101.5021 
#> [1] 0.1
#> nclusters = 9 lambda2 2.562891 cost = 111.5274 
#> [1] 0.1
#> nclusters = 7 lambda2 3.844336 cost = 130.5629 
#> [1] 0.1
#> nclusters = 4 lambda2 5.766504 cost = 170.3529 
#> [1] 0.1
#> nclusters = 3 lambda2 8.649756 cost = 254.4898 
#> [1] 0.1
#> nclusters = 1 lambda2 12.97463 cost = 380.9522 
#> niter ==  14
```

3.  Plot results compact version

-   Estimated regression vectors

``` r
plot_mglasso(res)
```

<img src="README_files/figure-gfm/unnamed-chunk-12-1.png" width="100%" />

`level9` denotes a partition with `9` clusters. We observe a shrinkage
effect in the estimated coefficients due to the fuse-group lasso penalty
parameter.

-   Estimated clustering partitions

``` r
res$out$level9$clusters
#> [1] 1 2 3 4 5 6 7 8 9
res$out$level7$clusters
#> [1] 1 2 3 4 5 6 7 7 7
res$out$level4$clusters
#> [1] 1 1 1 2 3 2 4 4 4
res$out$level3$clusters
#> [1] 1 1 1 2 2 2 3 3 3
res$out$level1$clusters
#> [1] 1 1 1 1 1 1 1 1 1
```

The uncovered partition obtained while increasing *λ*<sub>2</sub> is a
hierarchical partition under some constraints.

## Reference

Edmond, Sanou; Christophe, Ambroise; Geneviève, Robin; (2022): Inference
of Multiscale Gaussian Graphical Model. ArXiv. Preprint.
<https://doi.org/10.48550/arXiv.2202.05775>
