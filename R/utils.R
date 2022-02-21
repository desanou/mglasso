#' `Mglasso` cost function
#'
#' `cost` computes the cost function of `Mglasso` method.
#'
#' @param beta p by p numeric matrix. In rows, regression vectors coefficients after node-wise regression. \code{diag(beta) = 0}.
#' @param x n by p numeric matrix. Data with variables in columns.
#' @param lambda1 numeric scalar. Lasso penalization parameter.
#' @param lambda2 numeric scalar. Fused-group Lasso penalization parameter.
#'
#' @return numeric scalar. The cost.

cost <- function(beta, x, lambda1 = 0, lambda2 = 0) {
    p <- ncol(x)
    l2_norm <- 0
    least_squares <- 0

    if (length(p) != 0) {
        least_squares <- sum(sapply(1:p, function(i) {
            norm(x[, i] - x %*% beta[i, ], type = "2")^2
        }))

        for (i in 1:(p - 1)) {
            for (j in (i + 1):p) {
                l2_norm <- l2_norm + norm(fun_lines(i, j, beta, `-`),
                  type = "2")
            }
        }  ## fuse-group lasso penalty
    }


    l1_norm <- sum(abs(beta))  ## lasso penalty

    return(least_squares + lambda1 * l1_norm + lambda2 * l2_norm)
}

#' Apply symmetrization on estimated graph
#'
#' @param mat graph or precision matrix
#' @param rule "and" or "or" rule
#' @return A numeric matrix.
symmetrize <- function(mat, rule = "and") {
    diag(mat) <- 0
    if (rule == "and") {
        mat <- sign(mat) * pmin(abs(mat), t(abs(mat)))
    } else {
        ## or rule
        mat <- pmax(mat, t(mat)) - pmax(-mat, -t(mat))
    }
    return(mat)
}

#' Compute precision matrix from regression vectors
#'
#' @param K precision matrix
#' @return A numeric matrix.
precision_to_regression <- function(K) {
    p <- ncol(K)
    mat <- matrix(0, p, p)

    for (i in 1:p) {
        for (j in 1:p) {
            if (i != j)
                mat[i, j] <- -K[i, j]/K[i, i]
        }
    }

    mat
}


#' Transform a matrix of regression coefficients to vector removing the diagonal
#'
#' @param beta_mat matrix of regressions vectors
#' @return A numeric vector of all regression coefficients.
beta_to_vector <- function(beta_mat){
  beta_mat <- as.matrix(beta_mat)
  diag(beta_mat) <- NA
  beta_mat <- as.vector(t(beta_mat))
  beta_mat <- beta_mat[which(!is.na(beta_mat))]
  beta_mat
}

#' Initialize regression matrix
#'
#' @param X data
#' @return A zero-diagonal matrix of regression vectors.
beta_ols <- function(X){
  X <- as.matrix(X)
  p <- ncol(X)
  Beta <- matrix(0, nrow = p, ncol = p)

  foo <- function(i){
    bi <- as.vector(corpcor::pseudoinverse(t(X[,-i])%*%X[,-i]) %*% t(X[,-i]) %*% X[,i])
    R.utils::insert(bi, ats = i, values = 0)
  }

  Beta <- sapply(1:p, foo)
  t(Beta)
}


# PLOT --------------------------------------------------------------------
#' Plot the image of a matrix
#'
#' @param matrix matrix of regression coefficients
#' @param main_ title
#' @param sub_ subtitle
#' @param col_names columns names
#' @return No return value.
image_sparse <- function(matrix, main_ = "", sub_ = "", col_names = FALSE) {
  main_ <- paste0(c(sub_, main_), collapse = " ")

  nn <- 100
  plt <- Matrix::image(methods::as(matrix, "sparseMatrix"), main = main_, sub = "",
                       xlab = "", ylab = "", useAbs = FALSE)

  nn <- rownames(matrix)
  nn <- c(nn[length(nn)], nn)
  nn <- nn[1:(length(nn) - 1)]
  labs <- c(nn, nn)
  if (col_names) {
    stats::update(plt, scales = list(labels = labs))
  }

  plt
}

# CLUSTERING --------------------------------------------------------------
#' Compute distance matrix between regression vectors
#'
#' @param beta matrix of regression vectors
#' @param distance euclidean or relative distance
#' @return A numeric matrix of distances.
dist_beta <- function(beta, distance = "euclidean") {
  k <- ncol(beta)

  if (k != 1) {
    diffs <- matrix(NA, nrow = k, ncol = k)
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        diffs[i, j] <- norm(fun_lines(i, j, beta, `-`), type = "2")
      }
    }

    if (distance == "relative") {
      dsum <- matrix(NA, nrow = k, ncol = k)
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          dsum[i, j] <- norm(beta[i, ], type = "2") + norm(beta[j,
          ], type = "2")
        }
      }
      diffs <- diffs / dsum
    }
  } else {
    diffs <- matrix(0, nrow = 1, ncol = 1)
  }

  return(diffs)
}

#' compute clusters partition from pairs of variables to merge
#' @param pairs_to_merge table of the indices of variables to be merge
#' @param clusters numeric vector. By default 1:p where p is the number of variables
#' @return A numeric vector.
merge_clusters <- function(pairs_to_merge, clusters) {

  for (l in 1:nrow(pairs_to_merge)) {
    pair_to_merge <- pairs_to_merge[l, ]

    # for a upper-triangular matrix
    i <- min(pair_to_merge)
    j <- max(pair_to_merge)

    if (i != j) {
      # merge clusters
      clusters[clusters == j] <- i
      clusters[clusters > j] <- clusters[clusters > j] - 1

      # update the rest of the table with the new clusters
      pairs_to_merge[pairs_to_merge == j] <- i
      pairs_to_merge[pairs_to_merge > j] <- pairs_to_merge[pairs_to_merge >
                                                             j] - 1
    }
  }

  return(clusters)
}
