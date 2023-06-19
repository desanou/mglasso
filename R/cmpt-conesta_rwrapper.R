#' CONESTA solver for numerical experiments.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{# because of installation of external packages during checks
#' mglasso::install_pylearn_parsimony(envname = "rmglasso", method = "conda")
#' reticulate::use_condaenv("rmglasso", required = TRUE)
#' reticulate::py_config()
#
#' n = 30
#' K = 2
#' p = 4
#' rho = 0.85
#' blocs <- list()
#' for (j in 1:K) {
#'  bloc <- matrix(rho, nrow = p/K, ncol = p/K)
#'    for(i in 1:(p/K)) { bloc[i,i] <- 1 }
#'    blocs[[j]] <- bloc
#'    }
#'
#' mat.covariance <- Matrix::bdiag(blocs)
#' mat.covariance
#' set.seed(11)
#' X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
#' X <- scale(X)
#' res <- conesta_rwrapper(X, 0.1, 0.1)
#' }

conesta_rwrapper <- function(X, lam1, lam2, beta_warm=c(0), type_="initial", W_=NULL, mean_ = FALSE, max_iter_=1e4, prec_=1e-2) {

  solver_module$conesta_rwrapper(X=X, lam1=lam1, lam2=lam2, beta_warm=beta_warm, type_=type_, W_=W_, mean_ = mean_, max_iter_=max_iter_, prec_=prec_)

}
