### TODO
### return :: penalties (l1 tv), sparsity, n_edges for each model varying tv
###           path (list of adjacency mats), beta (list of betas l1 fixed)
### input :: symmetrization rule for path
### make maxit visible

#' Inference of Multiscale Gaussian Graphical Model.
#'
#' Cluster variables using L2 fusion penalty and simultaneously estimates a
#' gaussian graphical model structure with the addition of L1 sparsity penalty.
#'
#' Estimates a gaussian graphical model structure while hierarchically grouping
#' variables by optimizing a pseudo-likelihood function combining Lasso and
#' fuse-group Lasso penalties. The problem is solved via the \emph{COntinuation
#' with NEsterov smoothing in a Shrinkage-Thresholding Algorithm} (Hadj-Selem et
#' al. 2018). Varying the fusion penalty \eqn{\lambda_2} in a multiplicative
#' fashion allow to uncover a seemingly hierarchical clustering structure. For
#' \eqn{\lambda_2 = 0}, the approach is theoretically equivalent to the
#' Meinshausen-Bühlmann (2006) \emph{neighborhood selection} as resuming to the
#' optimization of \emph{pseudo-likelihood} function with \eqn{\ell_1} penalty
#' (Rocha et al., 2008). The algorithm stops when all the variables have merged
#' into one cluster. The criterion used to merge clusters is the
#' \eqn{\ell_2}-norm distance between regression vectors.
#'
#' For each iteration of the algorithm, the following function is optimized :
#' \deqn{1/2 \sum_{i=1}^p || X^i - X^{\ i} \beta^i ||_2 ^2  + \lambda_1 \sum_{i
#' = 1}^p || \beta^i ||_1 + \lambda_2 \sum_{i < j} || \beta^i -
#' \tau_{ij}(\beta^j) ||_2.}
#'
#' where \eqn{\beta^i} is the vector of coefficients obtained after regression
#' \eqn{X^i} on the others and \eqn{\tau_{ij}} is a permutation exchanging
#' \eqn{\beta_j^i} with \eqn{\beta_i^j}.
#'
#' @param x Numeric matrix (\eqn{n x p}). Multivariate normal sample with
#'   \eqn{n} independent observations.
#' @param lambda1 Positive numeric scalar. Lasso penalty.
#' @param fuse_thresh Positive numeric scalar. Threshold for clusters fusion.
#' @param maxit Integer scalar. Maximum number of iterations.
#' @param distance Character. Distance between regression vectors with
#'   permutation on symmetric coefficients.
#' @param lambda2_start Numeric scalar. Starting value for fused-group Lasso
#'   penalty (clustering penalty).
#' @param lambda2_factor Numeric scalar. Step used to update fused-group Lasso
#'   penalty in a multiplicative way..
#' @param precision Tolerance for the stopping criterion (duality gap).
#' @param weights_ Matrix of weights.
#' @param type If "initial" use classical version of \bold{MGLasso} without
#'   weights.
#' @param compact Logical scalar. If TRUE, only save results when previous
#'   clusters are different from current.
#' @param verbose Logical scalar. Print trace. Default value is FALSE.
#'
#' @return A list-like object of class \code{mglasso} is returned.
#'   \item{out}{List of lists. Each element of the list corresponds to a
#'   clustering level. An element named \code{levelk} contains the regression
#'   matrix \code{beta} and clusters vector \code{clusters} for a clustering in
#'   \code{k} clusters. When \code{compact = TRUE} \code{out} has as many
#'   elements as the number of unique partitions. When set to \code{FALSE}, the
#'   function returns as many items as the the range of values taken by
#'   \code{lambda2}.} \item{l1}{the sparsity penalty \code{lambda1} used in the
#'   problem solving.}
#'
#' @export
#'
#' @seealso \code{\link{conesta}} for the problem solver,
#'   \code{\link{plot_mglasso}} for plotting the output of \code{mglasso}.
#'
#' @examples
#' \donttest{
#' install_conesta
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

mglasso <- function(x, lambda1 = 0, fuse_thresh = 1e-3, maxit = NULL,
                    distance = c("euclidean", "relative"), lambda2_start = 1e-4, lambda2_factor = 1.5,
                    precision = 1e-2, weights_ = NULL, type = c("initial"), compact = TRUE,
                    verbose = FALSE) {

  ## Check input
  stopifnot(lambda1 >= 0 | fuse_thresh >= 0 | lambda2_start >= 0 | lambda2_factor >= 0)
  distance = match.arg(distance)
  type = match.arg(type)

  p <- ncol(x)
  x <- scale(x)
  clusters <- 1:p
  t <- 1  # index for the out list.
  iter <- 0
  out <- list()
  clusters_prev <- NULL


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

    beta <- conesta(x, lambda1, lambda2, beta_old, prec_ = precision,
                    type_ = type, W_ = weights_)
    if(verbose) cat(lambda1)
    beta_old <- beta_to_vector(beta) ## For warm start

    diffs <- dist_beta(beta, distance = distance)  ## Update distance matrix

    pairs_to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)  ## Clustering starts
    if (nrow(pairs_to_merge) != 0)
    {
      clusters <- merge_clusters(pairs_to_merge, clusters)  # merge clusters
    }  ## Clustering ends here

    cost_ <- cost(beta, x)
    if(verbose) cat("nclusters =", length(unique(clusters)), "lambda2", lambda2,
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

  result <- list(out = out, lambda1 = lambda1)
  if(verbose) cat("niter == ", iter)

  class(result) <- "mglasso"

  return(result)
}
