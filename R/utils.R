#' weighted sum/difference of two regression vectors
#'
#' `fun_lines` applies function `fun` to regression vectors while reordering the coefficients,
#' such that the `j`-th coefficient in `beta[j, ]` is permuted with the `i`-th coefficient.
#'
#' @param i integer scalar. Indice of the first vector.
#' @param j integer scalar. Indice of the second vector.
#' @param fun function. Applied on lines.
#' @param ni integer scalar. Weight for vector `i`.
#' @param nj integer scalar. Wweight for vector `j`.
#' @param beta p by p numeric matrix. In rows, regression vectors coefficients after node-wise regression. `diag(beta) = 0`.
#'
#' @return numeric vector
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

#' Compute precison matrix from regression vectors
#'
#' @param K precision matrix
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
