# Inspired from https://github.com/OscarKjell/text/blob/master/R/0_0_text_install.R and
# https://github.com/OscarKjell/text/blob/master/R/0_0_1_text_initialize.R
# https://github.com/mrchypark/theeuh/blob/5dbe086967ba46d62f0ef3ca8c8efa9f33dcf66c/R/install.R

#' Install the python library pylearn-parsimony and other required libraries
#'
#' pylearn-parsimony contains the solver CONESTA used for the mglasso problem
#' and is available on github at https://github.com/neurospin/pylearn-parsimony
#' It is advised to use a python version ">=3.7,<3.10".
#' Indeed, the latest version of scipy under which mglasso was developped is scipy 1.7.1 which is based on python ">=3.7,<3.10".
#' In turn, this version of scipy can only be associated with a version of numpy ">=1.16.5,<1.23.0"
#'
#' @inheritParams reticulate::py_install
#' @param extra_pack Character vector. Extra-packages to be installed.
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio)
#' @param ... additionnal arguments passed to [`reticulate::py_install()`]
#'
#' @return No return value.
#' @export
#'
install_pylearn_parsimony <- function(method = c("auto", "virtualenv", "conda"),
                                      conda = "auto",
                                      extra_pack = c("scipy == 1.7.1",
                                                     "scikit-learn",
                                                     "numpy == 1.22.4",
                                                     "six",
                                                     "matplotlib"),
                                      python_version = '3.8',
                                      restart_session = TRUE,
                                      envname = NULL,
                                      ...) {


  method <- match.arg(method)

  if (.Machine$sizeof.pointer != 8) {
    stop("Binary installation is only available for 64-bit platforms.")
  }

  reticulate::py_install(packages = c("pylearn-parsimony", extra_pack),
                         envname = envname,
                         method = method,
                         conda = conda,
                         python_version = python_version,
                         pip = TRUE,
                         pip_options = 'git+https://github.com/neurospin/pylearn-parsimony.git',
                         ...)

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)

  # Check if conda available on the system
  # conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  # have_conda <- !is.null(conda)
  #
  # if (!have_conda) {
  #   cat("No conda was found in the system. ")
  #   ans <- utils::menu(c("No", "Yes"), title = "Do you want mglasso to download
  #                          miniconda using reticulate::install_miniconda()?")
  #   if (ans == 2) {
  #     reticulate::install_miniconda()
  #     conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)
  #   } else {
  #     stop("Conda environment installation failed (no conda binary found)\n", call. = FALSE)
  #   }
  # }

  # setup environment
  # is_rmglasso_env_installed = tryCatch(reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE),
  #                                      error = function (e) {'not installed'})
  # if (!is.null(is_rmglasso_env_installed)) {
  #   packageStartupMessage('mglasso requires the rmglasso conda environment. Attempting to create...')
  #   reticulate::conda_create(envname = 'rmglasso', python_version = conda_py_version)
  #   message("Env created.")
  # }
  #
  # reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE)
  #
  # check_install <- sapply(extra_pack, reticulate::py_module_available)
  #
  # pack_to_install <- extra_pack[!check_install]
  #
  # if(!identical(pack_to_install, character(0))){
  #   reticulate::conda_install(envname = "rmglasso",
  #                             pack_to_install,
  #                             python_version = conda_py_version)
  # }else{
  #   message("required packages are already available.")
  # }
  #
  # if (!reticulate::py_module_available("parsimony")) {
  #   message('Installing pylearn-parsimony')
  #
  #   config <- reticulate::py_discover_config()
  #   system2(config$python, c("-m", "pip", "install", "--quiet",
  #                            shQuote("git+https://github.com/neurospin/pylearn-parsimony.git")))
  # }else{
  #   message("pylearn-parsimony is already available")
  # }

  # path_to_python <- reticulate::conda_python("rmglasso")
  # Sys.setenv(RETICULATE_PYTHON = path_to_python)
  # writeLines(sprintf("RETICULATE_PYTHON=%s", path_to_python),
  #            Sys.getenv("GITHUB_ENV"))
}

# the_module <- function()
#   try(reticulate::import_from_path("conesta_solver",
#                                    path = path_python(),
#                                    delay_load = TRUE))
#
# path_python <- function(){
#   system.file("python", package = "mglasso")
# }

#' Initialize mglasso required python packages
#'
#' Initialize mglasso required python packages to call from R.
#' @return NULL
#' @param condaenv character. conda virtual environment name
#' @export
# mglasso_initialize <- function(condaenv = "rmglasso") {
#
#   reticulate::use_condaenv(condaenv, required = TRUE)
#
#   message("Successfully initialized mglasso required modules.")
# }
