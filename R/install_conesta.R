#' Install mglasso
#'
#' @param extra_pack Character vector. Extra-packages to be installed.
#'
#' @export
#'
install_conesta <- function(extra_pack = c("scipy == 1.7.1", "scikit-learn", "numpy >= 1.6", "six")) {
  # conestaa <- NULL

  miniconda = TRUE

  if (miniconda) {
    message("Checking if miniconda is installed...")
    tryCatch(reticulate::install_miniconda(), error = function(e) {
      return()
    })
  }

  is_mglasso_env_installed = tryCatch(reticulate::use_miniconda(condaenv = 'mglasso', required = TRUE),
                                          error = function (e) {'not installed'})

  # setup environment
  if (!is.null(is_mglasso_env_installed)) {
    packageStartupMessage('MGLasso requires the mglasso conda environment. Attempting to create...')
    reticulate::conda_create(envname = 'mglasso', python_version = '3.8')
  }

  reticulate::use_miniconda(condaenv = 'mglasso', required = TRUE)
  reticulate::py_config()

  check_install <- sapply(extra_pack, reticulate::py_module_available)

  pack_to_install <- extra_pack[!check_install]

  if(!identical(pack_to_install, character(0))){
    reticulate::py_install(pack_to_install,
                           pip=TRUE,
                           envname = "mglasso")
  }



  if (!reticulate::py_module_available("pylearn-parsimony")) {
    reticulate::py_install("https://github.com/neurospin/pylearn-parsimony/releases/download/v0.3.1/pylearn-parsimony-0.3.1.tar.gz",
                           envname = "mglasso",
                           pip = TRUE)
    # reticulate::use_condaenv(condaenv = 'mglasso', required = TRUE)
    # message('Installing pylearn-parsimony')
    # text <- "pip install git+git://github.com/neurospin/pylearn-parsimony.git@master --quiet"
    # system(text)
    # system("python3 -m pip install numpy")
  }

  the_module()

  # the_module <- reticulate::import_from_path("conesta_solver", path = path_python(), delay_load = TRUE)
  # conestaa <<- the_module$conesta

  message("pylearn-parsimony is installed.")
}

path_python <- function(){
  system.file("python", package = "mglasso")
}

the_module <- function() try(reticulate::import_from_path("conesta_solver", path = path_python()), silent = TRUE)
