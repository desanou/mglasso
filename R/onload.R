conesta_rwrapper <- NULL

install_libs <- function(path) {
  if (!reticulate::py_module_available("scipy")) {
    reticulate::py_install("scipy", method = "auto",
                           conda = "auto", pip=TRUE, envname = "mglasso")
  }

  if (!reticulate::py_module_available("scikit-learn")) {
    reticulate::py_install("scikit-learn", method = "auto",
                           conda = "auto", pip=TRUE, envname = "mglasso")
  }

  if (!reticulate::py_module_available("parsimony.estimators")) {
    # text <- paste("sys.path.insert(0,'", path, "/pylearn-parsimony')",
    #               sep = "",
    #               collapse = NULL)

    text <- "pip install git+git://github.com/neurospin/pylearn-parsimony.git@master"
    system(text)

    #reticulate::py_run_string("import sys")
    #reticulate::py_run_string(text)

  }
}

.onLoad <- function(libname, pkgname) {

  miniconda = TRUE
  remove_existing_env = TRUE
  path <- system.file("python", package = "mglasso")
  #path <- "/inst/python/"
  envname = "mglasso"

  if (is.null(reticulate::conda_binary())) { # Check for anaconda
    stop("You need to install Anaconda or add it in the system path.")
  }

  if (miniconda) {
    message('Checking if miniconda is installed...')
    tryCatch(reticulate::install_miniconda(),
             error = function (e) {return()})
  }

  reticulate::py_config()

  is_mglasso_env_installed = tryCatch(reticulate::use_miniconda(condaenv = 'mglasso', required = TRUE),
                                      error = function (e) {'not installed'})

  # remove the conda environment if needed
  envs <- reticulate::conda_list()
  env_exists <- envname %in% envs$name
  if (env_exists && remove_existing_env) {
    msg <- sprintf(paste("Environment \"%s\" already exists.",
                         "Remove the environment..."),
                   envname)
    message(msg)
    reticulate::conda_remove(envname)
    env_exists <- FALSE
  }

  # setup environment
  if (!is.null(is_mglasso_env_installed)) {
    message('MGLasso requires the mglasso conda environment. Attempting to create...')
    reticulate::conda_create(envname = 'mglasso')
  }
  reticulate::use_condaenv(condaenv = 'mglasso')

  install_libs(path)

  #
  reticulate::py_config()
  #reticulate::source_python(paste0(path,"conesta_solver.py"))
  # reticulate::source_python("conesta_solver.py")
  # loading conesta solver
  the_module <- reticulate::import_from_path("conesta_solver", path = path)
  conesta_rwrapper <<- the_module$conesta_rwrapper
}

# git submodule add https://github.com/neurospin/pylearn-parsimony.git ./inst/python/pylearn-parsimony
# https://github.com/rstudio/tensorflow/blob/701d402efa78fea25c95c1b3394c22b27aabd466/R/install.R
# source https://github.com/Azure/azureml-sdk-for-r/blob/master/R/install.R
