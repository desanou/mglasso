#' weighted sum/difference of two regression vectors
#'
#' `fun_lines` applies function `fun` to regression vectors while reordering the coefficients,
#' such that the `j`-th coefficient in `beta[j, ]` is permuted with the `i`-th coefficient.
#'
#' @param i integer scalar. Index of the first vector.
#' @param j integer scalar. Index of the second vector.
#' @param fun function. Applied on lines.
#' @param ni integer scalar. Weight for vector `i`.
#' @param nj integer scalar. Weight for vector `j`.
#' @param beta p by p numeric matrix. In rows, regression vectors coefficients after node-wise regression. `diag(beta) = 0`.
#'
#' @export
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
