#' #' Install pylearn-parsimony library
#' #'
#' #' @param path path to local pylearn_parsimony parent folder
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' install_pylearn_parsimony <- function(path) {
#'   text <- paste("sys.path.insert(0,'", path, "/pylearn-parsimony')",
#'                 sep = "",
#'                 collapse = NULL)
#'   reticulate::py_run_string("import sys")
#'   reticulate::py_run_string(text)
#'
#'   message('pylearn-parsimony sucessfully insert in system.path')
#'
#'   invisible(TRUE)
#' }
#'
#' #' Install python library
#' #'
#' #' @param libname
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' install_library <- function(libname,
#'                             method = c("auto", "virtualenv", "conda"),
#'                             conda = "auto",
#'                             envname = NULL) {
#'
#'   reticulate::py_install(packages = libname,
#'                          envname = envname,
#'                          method = method,
#'                          conda = conda,
#'                          pip = TRUE)
#'
#'   invisible(TRUE)
#' }
#'
#' #' Title
#' #'
#' #' @param flag
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' restart_session <- function(flag = TRUE){
#'   if (flag &&
#'       rstudioapi::isAvailable() &&
#'       rstudioapi::hasFun("restartSession"))
#'     rstudioapi::restartSession()
#' }
#'
#'
#' #' Title
#' #'
#' #' @param libname
#' #' @param pkgname
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .onLoad <- function(libname, pkgname) {
#'   reticulate::configure_environment(pkgname)
#'
#'   mglasso_init()
#'
#'   # assign("conesta_rwrapper",
#'   #        reticulate::import_from_path("conesta_solver", path = path, delay_load = TRUE)$conesta_rwrapper,
#'   #        mglasso_env)
#'
#'   path <- "./inst/python/"
#'   the_module <- reticulate::import_from_path("conesta_solver", path = path, delay_load = TRUE)
#'   conesta_rwrapper <<- the_module$conesta_rwrapper
#'
#'   invisible(TRUE)
#' }
#'
#'
#' #' Title
#' #'
#' #' @param libname
#' #' @param pkgname
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .onAttach <- function(libname, pkgname) {
#'   packageStartupMessage('Welcome to MGLasso.')
#'   packageStartupMessage('By default, this package will install miniconda and create a "mglasso" conda environment.')
#' }
#'
#' mglasso_init <- function(miniconda = TRUE){
#'   mglasso_env <- new.env(parent = emptyenv())
#'
#'   path <- "./inst/python/"
#'   extra_packages <- list("scipy", "scikit-learn") # test first and then complete if needed
#'
#'   if (miniconda) {
#'     message('Checking if miniconda is installed...')
#'     tryCatch(reticulate::install_miniconda(),
#'              error = function (e) {return()})
#'   }
#'
#'   is_mglasso_env_installed = tryCatch(reticulate::use_miniconda(condaenv = 'mglasso', required = TRUE),
#'                                       error = function (e) {'not installed'})
#'
#'   if (!is.null(is_mglasso_env_installed)) {
#'     message('MGLasso requires the mglasso conda environment. Attempting to create...')
#'     reticulate::conda_create(envname = 'mglasso')
#'   }
#'
#'   reticulate::use_miniconda(condaenv = 'mglasso', required = TRUE)
#'
#'   if (!reticulate::py_module_available("parsimony.estimators")) {
#'     message('pylearn-parsimony is not available')
#'     install_library(extra_packages)
#'     install_pylearn_parsimony(path)
#'   } else {
#'     message('pylearn is available')
#'   }
#'
#'   message('Importing scipy')
#'   mglasso_env$scipy <- reticulate::import('scipy', delay_load = TRUE)
#'   message('Importing numpy')
#'   mglasso_env$numpy <- reticulate::import('numpy', delay_load = TRUE)
#'   message('Importing pylearn-parsimony')
#'   mglasso_env$parsimony <- reticulate::import('pylearn-parsimony', delay_load = TRUE)
#'
#'   invisible(TRUE)
#' }
