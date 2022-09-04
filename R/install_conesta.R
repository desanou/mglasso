# Inspired from https://github.com/OscarKjell/text/blob/master/R/0_0_text_install.R and
# https://github.com/OscarKjell/text/blob/master/R/0_0_1_text_initialize.R

#' Install the python library pylearn-parsimony and other required libraries
#'
#' pylearn-parsimony contains the solver CONESTA used for the mglasso problem
#' and is available on github at https://github.com/neurospin/pylearn-parsimony
#' At this stage, we only support conda virtual environments.
#'
#'
#' @param conda Character. Path to conda executable. "auto" finds the path automatically.
#' @param extra_pack Character vector. Extra-packages to be installed.
#' @param conda_py_version Character. Python version. It is advised to use a version ">=3.7,<3.10".
#' The latest version under which mglasso was developped is scipy 1.7.1 which is based on python ">=3.7,<3.10".
#' In turn, this version of scipy can only be associated with a version of numpy ">=1.16.5,<1.23.0"
#'
#' @return No return value.
#' @export
#'
install_conesta <- function(conda = "auto",
                            extra_pack = c("scipy == 1.7.1",
                                           "scikit-learn",
                                           "numpy == 1.22.4",
                                           "six",
                                           "matplotlib"),
                            conda_py_version = '3.8') {

  reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE)

  check_install <- sapply(extra_pack, reticulate::py_module_available)

  pack_to_install <- extra_pack[!check_install]

  if(!identical(pack_to_install, character(0))){
    reticulate::py_install(pack_to_install,
                           envname = "rmglasso",
                           python_version = conda_py_version)
  }else{
    message("required packages are already available.")
  }

  if (!reticulate::py_module_available("parsimony")) {
    message('Installing pylearn-parsimony')

    config <- reticulate::py_config()
    system2(config$python, c("-m", "pip", "install",
                             shQuote("git+https://github.com/neurospin/pylearn-parsimony.git")))
  }else{
    message("pylearn-parsimony is already available")
  }

  # if (rstudioapi::hasFun("restartSession"))
  #   rstudioapi::restartSession()

}

the_module <- function()
  try(reticulate::import_from_path("conesta_solver",
                                   path = path_python()), silent = TRUE)

path_python <- function(){
  system.file("python", package = "mglasso")
}

#' Initialize mglasso required python packages
#'
#' Initialize mglasso required python packages to call from R.
#' @return NULL
#' @param condaenv character. conda virtual environment name
#' @export
mglasso_initialize <- function(condaenv = "rmglasso") {

  reticulate::use_condaenv(condaenv, required = TRUE)

  the_module()

  message("Successfully initialized mglasso required python packages.")
}
