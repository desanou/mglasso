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
#' @param lambda2_start numeric scalar. starting value for fused-group Lasso penalty (clustering penalty).
#' @param lambda2_factor numeric scalar. Step used to update fused-group Lasso penalty. `lambda2_factor*lambda2_start`.
#' @param precision precision of estimation algorithm.
#' @param weights_ matrix of weights
#' @param type if "initial" use classical version of MGLasso without weights.
#' @param compact if TRUE, only save results when previous clusters are different from current
#'
#' @return A list.
#' \item{out}{list with matrix of regression vectors and clusters for each level.
#' Each element of the list contains two elements: `beta` and `clusters`}
#' \item{tree}{clustering tree}
#'
#' @examples
#' \dontrun{
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
#' mat.covariance <- Matrix::bdiag(blocs)
#' mat.covariance
#'
#' set.seed(11)
#' X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
#' X <- scale(X)
#'
#' res <- mglasso(X, 0.1, lambda2_start = 0.1)
#' res$out[[1]]$clusters
#' res$out[[1]]$beta
#' }

mglasso <- function(x, lambda1 = 0, fuse_thresh = 10^-3, maxit = NULL,
                    distance = "euclidean", lambda2_start = 1e-04, lambda2_factor = 1.5,
                    precision = 0.01, weights_ = NULL, type = "initial", compact = TRUE) {
  p <- ncol(x)
  x <- scale(x)
  clusters <- 1:p

  t <- 1  # index for the out list.
  iter <- 0
  out <- list()
  clusters_prev <- NULL

  if (type == "pcor") {
    weights_ <- weight_mat(x, "pcor")
  } else if (type == "adapt") {
    weights_ <- weight_mat(x, "adapt")
  }

  ## Loop until all the variables merged
  while (length(unique(clusters)) > 1) {
    clusters <- 1:p

    if (iter == 0) {
      beta_old <- beta_to_vector(beta_ols(x))  ## init OLS
      lambda2 <- 0
    }
    if (iter == 1) {
      lambda2 <- lambda2_start
    }

    beta <- conesta_rwrapper(x, lambda1, lambda2, beta_old, prec_ = precision,
                             type_ = type, W_ = weights_)
    print(lambda1)
    beta_old <- beta_to_vector(beta)

    diffs <- dist_beta(beta, distance = distance)  ## Update distance matrix

    pairs_to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)  ## Clustering starts
    if (nrow(pairs_to_merge) != 0)
    {
      clusters <- merge_clusters(pairs_to_merge, clusters)  # merge clusters
    }  ## Clustering ends here

    cost_ <- cost(beta, x)
    cat("nclusters =", length(unique(clusters)), "lambda2", lambda2,
        "cost =", cost_, "\n")

    if (compact) {
      if (!identical(clusters, clusters_prev)) {
        out[[t]] <- list(beta = beta, clusters = clusters)
        names(out)[[t]] <- paste0("level", length(unique(clusters)))
        clusters_prev <- clusters
        t <- t + 1
      }
    } else {
      out[[t]] <- list(beta = beta, clusters = clusters)
      names(out)[[t]] <- paste0("level", length(unique(clusters)))
      t <- t + 1
    }

    lambda2 <- lambda2 * lambda2_factor
    iter <- iter + 1
  }

  result <- list(out = out, l1 = lambda1)
  cat("niter == ", iter)

  return(result)
}


#' Generate a weight matrix for fuse-group lasso term
weight_mat <- function(x_, type_) {
  beta <- beta_ols(x_)

  if (type_ == "pcor") {
    w <- (1/(1 - abs(beta)))^2  # squared because of conesta configuration
  }
  if (type_ == "adapt") {
    w <- (1/dist_beta(beta))^2
  }

  return(w)
}
