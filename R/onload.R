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

  if (!reticulate::py_module_available("scipy")) {
    reticulate::py_install("scipy", method = "auto", conda = "auto", pip=TRUE, envname = "r-reticulate")
  }

  if (!reticulate::py_module_available("scikit-learn")) {
    reticulate::py_install("scikit-learn", method = "auto", conda = "auto", pip=TRUE, envname = "r-reticulate")
  }

  if (!reticulate::py_module_available("parsimony.estimators")) {
    text <- paste("sys.path.insert(0,'", path, "/pylearn-parsimony')",
                  sep = "",
                  collapse = NULL)
    reticulate::py_run_string("import sys")
    reticulate::py_run_string(text)
  }

  # loading conesta solver
  the_module <- reticulate::import_from_path("conesta_solver", path = path)
  conesta_rwrapper <<- the_module$conesta_rwrapper
}

## git submodule add https://github.com/neurospin/pylearn-parsimony.git ./inst/python/pylearn-parsimony

## https://github.com/rstudio/tensorflow/blob/701d402efa78fea25c95c1b3394c22b27aabd466/R/install.R
