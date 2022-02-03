.onLoad <- function(libname, pkgname) {

  path <- system.file("python", package = "mglasso")

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
  #reticulate::py_config()

  library_list <- c("scipy", "pandas", "scikit-learn", "six")

  reticulate::py_install(library_list,
                         method = "auto",
                         conda = "auto",
                         pip=TRUE,
                         envname = "r-reticulate")

  if (!reticulate::py_module_available("parsimony.estimators")) {
    message('Installing parsimony.estimators')
    text <- "pip install git+git://github.com/neurospin/pylearn-parsimony.git@master"
    system(text)
  }

  the_module <- reticulate::import_from_path("conesta_solver", path = path)
  conesta <<- the_module$conesta
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to MGLasso.")
}
