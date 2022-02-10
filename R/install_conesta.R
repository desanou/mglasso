#' Install conesta
#'
#' @param extra_pack Character vector. Extra-packages to be installed.
#'
#' @export
#'
install_conesta <- function(extra_pack = c("scipy == 1.7.1", "scikit-learn", "numpy >= 1.6", "six", "passlib==1.7.2")) {
  # conestaa <- NULL

  is_rreticulate_env_installed = tryCatch(reticulate::use_condaenv(condaenv = 'r-reticulate', required = TRUE),
                                          error = function (e) {'not installed'})

  # setup environment
  if (!is.null(is_rreticulate_env_installed)) {
    packageStartupMessage('mglasso requires the r-reticulate conda environment. Attempting to create...')
    reticulate::conda_create(envname = 'r-reticulate')
  }

  reticulate::use_condaenv(condaenv = 'r-reticulate', required = TRUE)
  #reticulate::py_config()

  check_install <- sapply(extra_pack, reticulate::py_module_available)

  pack_to_install <- extra_pack[!check_install]

  if(!identical(pack_to_install, character(0))){
    reticulate::py_install(pack_to_install,
                           pip=TRUE,
                           envname = "r-reticulate")
  }



  if (!reticulate::py_module_available("pylearn-parsimony")) {
    reticulate::py_install("https://github.com/neurospin/pylearn-parsimony/releases/download/v0.3.1/pylearn-parsimony-0.3.1.tar.gz",
                           envname = "r-reticulate",
                           pip = TRUE)
    # reticulate::use_condaenv(condaenv = 'r-reticulate', required = TRUE)
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
