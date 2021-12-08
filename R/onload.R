conesta_rwrapper <- NULL

.onLoad <- function(libname, pkgname) {
  # Check for anaconda
  if (is.null(reticulate::conda_binary())) {
    stop("You need to install Anaconda or add it in the system path.")
  }

  # setup environment
  reticulate::py_config()
  #path <- system.file("python", package = "mglasso") ### !!!!!!!!!
  path <- "./inst/python/"

  ##########################
  ######################
  #### source https://github.com/Azure/azureml-sdk-for-r/blob/master/R/install.R

  envname = "r-reticulate"

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

  if (!env_exists) {
    msg <- paste("Creating environment: ", envname)
    message(msg)
    #py_version <- paste("python=", conda_python_version, sep = "")
    reticulate::conda_create(envname)
  }
  ########################
  ##########################

  #if (!reticulate::py_module_available("scipy")) {
  reticulate::py_install("scipy", method = "auto", conda = "auto", pip=TRUE, envname = "r-reticulate")
  #}

  #if (!reticulate::py_module_available("scikit-learn")) {
  reticulate::py_install("scikit-learn", method = "auto", conda = "auto", pip=TRUE, envname = "r-reticulate")
  #}

  #if (!reticulate::py_module_available("parsimony.estimators")) {
  text <- paste("sys.path.insert(0,'", path, "/pylearn-parsimony')",
                sep = "",
                collapse = NULL)
  reticulate::py_run_string("import sys")
  reticulate::py_run_string(text)
  #}

  if (rstudioapi::isAvailable() &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  # loading conesta solver
  the_module <- reticulate::import_from_path("conesta_solver", path = path)
  conesta_rwrapper <<- the_module$conesta_rwrapper
}

## git submodule add https://github.com/neurospin/pylearn-parsimony.git ./inst/python/pylearn-parsimony

## https://github.com/rstudio/tensorflow/blob/701d402efa78fea25c95c1b3394c22b27aabd466/R/install.R
