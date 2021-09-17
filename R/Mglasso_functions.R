conesta_rwrapper <- NULL


.onLoad <- function(libname, pkgname) {

  # setup environment
  reticulate::py_config()
  path <- system.file("python", package = "Rmglasso")

  if (!reticulate::py_module_available("scipy")) {
    reticulate::py_install("scipy")
  }

  if (!reticulate::py_module_available("scikit-learn")) {
    reticulate::py_install("scikit-learn")
  }

  if (!reticulate::py_module_available("parsimony.estimators")) {
    text <- paste("sys.path.insert(0,'", path, "/pylearn-parsimony')",
                  sep = "",
                  collapse = NULL)
    reticulate::py_run_string("import sys")
    reticulate::py_run_string(text)
  }

  # loading conesta solver
  the_module <- reticulate::import_from_path("conesta_solver", path = path)
  conesta_rwrapper <<- the_module$conesta_rwrapper
}

#' weighted sum/difference of two regression vectors
#'
#' `fun_lines` applies function `fun` to regression vectors while reordering the coefficients,
#' such that the `j`-th coefficient in `beta[j, ]` is permuted with the `i`-th coefficient.
#'
#' @param i integer scalar. Indice of the first vector.
#' @param j integer scalar. Indice of the second vector.
#' @param beta p by p numeric matrix. In rows, regression vectors coefficients after node-wise regression. `diag(beta) = 0`.
#' @fun function. Applied on lines.
#' @ni integer scalar. Weight for vector `i`.
#' @nj integer scalar. Wweight for vector `j`.
#'
#' @return numeric scalar.
#'
#' @examples
#'beta <- matrix(round(rnorm(9),2), ncol = 3)
#'diag(beta) <- 0
#'beta
#'fun_lines(1, 2, beta)
#'fun_lines(2, 1, beta)
fun_lines <- function(i, j, beta, fun = `-`, ni = 1, nj = 1) {
  y_coeffs <- beta[i, ]
  x_coeffs <- beta[j, ]
  x_i <- x_coeffs[i]
  x_coeffs[i] <- x_coeffs[j]
  x_coeffs[j] <- x_i

  fun(ni * y_coeffs, nj * x_coeffs)
}

#' `Mglasso` cost function
#'
#' `cost` computes the cost function of `Mglasso` method.
#'
#' @param beta p by p numeric matrix. In rows, regression vectors coefficients after node-wise regression. `diag(beta) = 0`.
#' @param x n by p numeric matrix. Data with variables in columns.
#' @param lambda1 numeric scalar. Lasso penalization parameter.
#' @param lambda2 numeric scalar. Fused-group Lasso penalization parameter.
#'
#' @return numeric scalar. The cost.
#'
#' @examples
#'
#'## Generation of K block partitions
#'n = 50
#'K = 3
#'p = 6
#'rho = 0.85
#'blocs <- list()
#'for (j in 1:K) {
#'    bloc <- matrix(rho, nrow = p/K, ncol = p/K)
#'    for(i in 1:(p/K)) { bloc[i,i] <- 1 }
#'    blocs[[j]] <- bloc
#'    }
#'mat.covariance <- bdiag(blocs)
#'set.seed(11)
#'X <- rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
#'X <- scale(X)
#'
#'## Initialization for beta
#'beta1 <- matrix(0, nrow = p, ncol = p)
#'for(i in 1:p){
#'   beta1[i,-i] <- solve(t(X[,-i])%*%X[,-i]) %*% t(X[,-i]) %*% X[,i]
#'   }
#'cost(beta, X, 0, 0)
cost <- function(beta, x, lambda1 = 0, lambda2 = 0) {
  p <- ncol(x)
  l2_norm <- 0
  least_squares <- 0

  if (length(p) != 0) {
    least_squares <- sum(sapply(1:p,
                                function(i) {
                                  norm(x[, i] - x %*% beta[i, ],
                                       type = "2")^2})
    )

    # I can vectorize it later
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        l2_norm <- l2_norm + norm(fun_lines(i, j, beta, `-`), type = "2")
      }
    } ## fuse-group lasso penalty
  }


  l1_norm <- sum(abs(beta))  ## lasso penalty

  return(least_squares + lambda1 * l1_norm + lambda2 * l2_norm)
}

#' Multiscale Graphical lasso
#'
#' Estimates a graphical model structure using Lasso and fused-group Lasso penalties.
#' Belongs to neighborhood selection inference methods.
#'
#' @param x n by p numeric matrix. p-multivariate normal sample with n independent observations.
#' @param lambda1 positive numeric scalar. Lasso penalty.
#' @param fuse_thresh positive numeric scalar. Threshold for cluster fusion.
#' @param maxit integer scalar. Maximum number of iterations.
#' @param distance character. Distance between regression vectors. Default euclidean;
#' @param solver character. Optimization algorithm. Default conesta.
#' @param lambda2_start numeric scalar. starting value for fused-group Lasso penalty (clustering penalty).
#' @param lambda2_factor numeric scalar. Step used to update fused-group Lasso penalty. `lambda2_factor*lambda2_start`.
#'
#' @return A list.
#' \item{out}{list with matrix of regression vectors and clusters for each level.
#' Each element of the list contains two elements: `beta` and `clusters`}
#' \item{tree}{clustering tree}
#'
#' @examples
#' n = 50
#' K = 3
#' p = 9
#' rho = 0.85
#' blocs <- list()
#' for (j in 1:K) {
#'   bloc <- matrix(rho, nrow = p/K, ncol = p/K)
#'   for(i in 1:(p/K)) { bloc[i,i] <- 1 }
#'   blocs[[j]] <- bloc
#' }
#'
#' mat.covariance <- bdiag(blocs)
#' mat.covariance
#'
#' set.seed(11)
#' X <- rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
#' X <- scale(X)
#'
#' res <- mglasso(X, 0.1, lambda2_start = 0.1)
#' print(res$tree)
#' res$out$clusters
#' res$out$beta
mglasso <- function(x,
                    lambda1 = 0,
                    fuse_thresh = 10^-3,
                    maxit = 1000,
                    distance = "euclidean",
                    solver = "conesta",
                    lambda2_start = 1e-4,
                    lambda2_factor = 1.5) {

  ## Initialisations
  p        <- ncol(x)
  gains    <- rep(0, p - 1)
  merge    <- matrix(0, nrow = (p - 1), ncol = 2) # clusters merged
  level    <- 0
  labels   <- -1:-p                         # vector of clusters labels
  clusters <- 1:p

  lambda2     <- lambda2_start
  old_lambda2 <- 0
  beta <- conesta_rwrapper(x, lambda1, old_lambda2)

  old_costf <- costf <-  cost(beta, x, lambda1, old_lambda2)


  t               <- 2 # index for the out list.
  iter            <- 0
  out             <- list()
  out[[1]]        <- list("beta" = beta, "clusters" = clusters)
  names(out)[[1]] <- "level0"
  prev            <- length(unique(clusters))
  ## End Initialisations


  ## Loop until all the variables merged
  while (length(unique(clusters)) > 1) {
    iter <- iter + 1


    beta <- conesta_rwrapper(x, lambda1, lambda2); print(lambda1)

    ## Update distance matrix
    diffs <- dist_beta(beta, distance = distance)

    ## Clustering starts here
    to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)

    if (nrow(to_merge) != 0) {
      gain_level     <- costf - old_costf
      out_mergeproc <- merge_proc(to_merge, clusters, x, beta,
                                  level, gain_level, gains, labels,
                                  merge)

      x        <- out_mergeproc$x
      beta     <- out_mergeproc$beta
      clusters <- out_mergeproc$clusters

      level    <- out_mergeproc$level
      gains    <- out_mergeproc$gains
      merge    <- out_mergeproc$merge
      labels   <- out_mergeproc$labels
    }
    ## Clustering ends here

    costf <- cost(beta, x, lambda1, lambda2)
    cat("nclusters =", length(unique(clusters)),
        "lambda2", lambda2,
        "cost =", costf, "\n")

    gains[is.na(gains)] <- old_costf - costf
    old_costf <- costf

    if (length(unique(clusters)) != prev) {
      out[[t]] <- list("beta" = beta, "clusters" = clusters)
      names(out)[[t]] <- paste0("level", (p - length(unique(clusters))))
      prev <- length(unique(clusters))
      t <- t + 1

    }

    old_lambda2 <- lambda2

    lambda2 <- lambda2 * lambda2_factor
  }

  height      <- cumsum(gains)
  tree        <- list(merge = merge,
                      height = height,
                      order = rev(clusters),
                      labels = paste("", 1:p),
                      gains = gains)
  class(tree) <- "hclust"
  tree <- vegan:::reorder.hclust(tree, 1:p)


  result <- list("out" = out, "tree" = tree)

  return(result)
}

#' merge clusters from table
merge_proc <- function(to_merge,
                       clusters,
                       x,
                       beta,
                       level,
                       gain_level,
                       gains,
                       labels,
                       merge) {
  for (l in seq_len(nrow(to_merge))) {
    pair_to_merge <- to_merge[l, ]

    i         <- min(pair_to_merge)
    j         <- max(pair_to_merge)

    if (i != j) {
      level <- level + 1

      # merge lines/cols in beta and x
      beta <- merge_beta(beta, pair_to_merge, clusters)
      x <- merge_x(x, pair_to_merge, clusters)

      # update dendrogram
      merge[level, ] <- c(labels[i], labels[j])
      labels        <- merge_labels(pair_to_merge, labels, level)
      gains[level] <- ifelse(l > 1, 0, NA)

      # merge clusters
      clusters[clusters == j] <- i
      clusters[clusters > j] <- clusters[clusters > j] - 1

      # update the rest of the table with the new clusters
      to_merge[to_merge == j] <- i
      to_merge[to_merge > j] <- to_merge[to_merge > j] - 1
    }

  }

  out_mergeproc <- list("clusters" = clusters,
                        "beta" = beta,
                        "x" = x,
                        "level" = level,
                        "gains" = gains,
                        "merge" = merge,
                        "labels" = labels)
  return(out_mergeproc)
}

#' distances beta
dist_beta <- function(beta, distance = "euclidean") {
  k <- ncol(beta)
  if (k != 1) {
    diffs <- matrix(NA, nrow = k, ncol =  k)
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        diffs[i, j] <- norm(fun_lines(i, j, beta, `-`), type = "2")
      }
    }

    if (distance == "relative") {
      dsum <- matrix(NA, nrow = k, ncol = k)
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          dsum[i, j] <- norm(beta[i, ], type = "2") +
            norm(beta[j, ], type = "2")
        }
      }
      diffs <- diffs / dsum
    }
  }else{
    diffs <- matrix(0, nrow = 1, ncol = 1)
  }

  return(diffs)
}

#' Merge x
#'
#' weighted mean
merge_x <- function(x, pair_to_merge, clusters) {
  i        <- min(pair_to_merge)
  j        <- max(pair_to_merge)

  ni       <- sum(clusters == i)
  nj       <- sum(clusters == j)

  x[, i] <- (ni * x[, i] + nj * x[, j]) / (ni + nj)
  x <- x[, -j]

  return(x)
}

#' Merge beta lines
merge_beta <- function(beta, pair_to_merge, clusters) {
  i        <- min(pair_to_merge)
  j        <- max(pair_to_merge)

  ni       <- sum(clusters == i)
  nj       <- sum(clusters == j)

  beta[i, ] <- fun_lines(i, j, beta, `+`, ni, nj) / (ni + nj)
  beta     <- beta[-j, -j]

  return(beta)
}

#' Merge labels
merge_labels <- function(merged_pair, labels, level) {
  i         <- min(merged_pair)
  j         <- max(merged_pair)
  labels[i] <- level
  labels    <- labels[-j]
  labels
}
