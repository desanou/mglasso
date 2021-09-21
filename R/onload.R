conesta_rwrapper <- NULL

.onLoad <- function(libname, pkgname) {

  # setup environment
  reticulate::py_config()
  path <- system.file("python", package = "Rmglasso")

  if (!reticulate::py_module_available("scipy")) {
    reticulate::py_install("scipy")
  }

  if (!reticulate::py_module_available("scikit-learn")) {
    reticulate::py_install("scikit-learn")
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
