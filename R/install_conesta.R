
#' Install mglasso
#'
#' @param extra_pack Character vector. Extra-packages to be installed.
#'
#' @export
#'
install_conesta <- function(extra_pack = c("scipy", "scikit-learn", "numpy")) {

  if (is.null(reticulate::conda_binary())) { # Check for anaconda
    stop("You need to install Anaconda or add it in the system path.")
  }

  is_rreticulate_env_installed = tryCatch(reticulate::use_condaenv(condaenv = 'r-reticulate', required = TRUE),
                                          error = function (e) {'not installed'})

  # setup environment
  if (!is.null(is_rreticulate_env_installed)) {
    packageStartupMessage('MGLasso requires the r-reticulate conda environment. Attempting to create...')
    reticulate::conda_create(envname = 'r-reticulate')
  }

  reticulate::use_condaenv(condaenv = 'r-reticulate', required = TRUE)

  check_install <- sapply(extra_pack, reticulate::py_module_available)

  pack_to_install <- extra_pack[!check_install]

  if(!identical(pack_to_install, character(0))){
    reticulate::py_install(pack_to_install,
                           method = "auto",
                           conda = "auto",
                           pip=TRUE,
                           envname = "r-reticulate")
  }

  if (!reticulate::py_module_available("pylearn-parsimony")) {
    message('Installing pylearn-parsimony')
    text <- "pip install git+git://github.com/neurospin/pylearn-parsimony.git@master --quiet"
    system(text)
  }

  message("pylearn-parsimony is installed.")
}

path_python <- function(){
  system.file("python", package = "mglasso")
}

the_module <- function() try(reticulate::import_from_path("conesta_solver", path = path_python()), silent = TRUE)
