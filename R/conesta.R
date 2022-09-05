#' CONESTA solver.
#'
#' Solve the MGLasso optimization problem using CONESTA algorithm. Interface to
#' the pylearn.parsimony python library.
#'
#' *COntinuation with NEsterov smoothing in a Shrinkage-Thresholding
#' Algorithm* (CONESTA, Hadj-Selem et al. 2018) <doi:10.1109/TMI.2018.2829802>
#' is an algorithm design for solving optimization problems including group-wise
#' penalties. This function is an interface with the python solver. The MGLasso
#' problem is first reformulated in a problem of the form \deqn{argmin 1/2 ||Y -
#' \tilde{X} \tilde{\beta}||_2^2 + \lambda_1 ||\tilde{\beta}||_1 + \lambda_2
#' \sum_{i<j} ||\boldsymbol A_{ij} \tilde{\beta}||_2} where vector \eqn{Y} is
#' the vectorized form of matrix \eqn{X}.
#'
#' @param X Data matrix nxp.
#' @param lam1 Sparsity penalty.
#' @param lam2 Total variation penalty.
#' @param beta_warm Warm initialization vector.
#' @param type_ Character scalar. By default set to initial version which doesn't
#'   use weights
#' @param W_ Weights matrix for total variation penalties.
#' @param mean_ Logical scalar. If TRUE weights the optimization function by the
#'   inverse of sample size.
#' @param max_iter_ Numeric scalar. Maximum number of iterations.
#' @param prec_ Numeric scalar. Tolerance for the stopping criterion (duality gap).
#'
#' @return Numeric matrix of size pxp. Line `k` of the matrix represents
#'   the coefficients obtained from the L1-L2 penalized regression of variable
#'   `k` on the others.
#'
#' @export
#'
#' @seealso [mglasso()] for the MGLasso model estimation.
#'
#' @examples
#' \donttest{
#' #library(mglasso)
#'
#' #reticulate::conda_create(envname = 'rmglasso', python_version = 3.8)
#' #path_to_python <- reticulate::conda_python("rmglasso")
#' #Sys.setenv(RETICULATE_PYTHON = path_to_python)
#'
#' #install_conesta()
#' reticulate::use_condaenv("rmglasso", required = TRUE)
#'
#' reticulate::conda_list()
#'
#' #temp <- reticulate::py_discover_config()
#' #temp
#' #Sys.setenv(RETICULATE_PYTHON = temp$python)
#' #mglasso_initialize()
#'
#' reticulate::py_discover_config()
#'
#' n = 30
#' K = 2
#' p = 4
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
#' set.seed(11)
#' X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
#' X <- scale(X)
#' res <- conesta(X, 0.1, 0.1)
#' }
#'
conesta <- function(X, lam1, lam2, beta_warm = c(0), type_="initial", W_ = NULL, mean_ = FALSE, max_iter_=1e4, prec_=1e-2) {
  # the_module()$conesta(X=X, lam1=lam1, lam2=lam2, beta_warm=beta_warm, type_=type_, W_=W_, mean_ = FALSE,
  #                      max_iter_=1e4, prec_=1e-2)

  path_python <-system.file("python", package = "mglasso")
  # modules <- reticulate::py_run_file(paste0(path_python, "/conesta_solver.py"))
  ##modules <- reticulate::import_from_path("conesta_solver", path = path_python, delay_load = TRUE)
  reticulate::source_python(file = paste0(path_python, "/conesta_solver.py"))
  conesta_py(X=X, lam1=lam1, lam2=lam2, beta_warm=beta_warm, type_=type_, W_=W_, mean_ = FALSE,
                       max_iter_=1e4, prec_=1e-2)
}
