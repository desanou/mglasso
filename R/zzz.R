numpy <- NULL
.onLoad <- function(libname, pkgname) {
  reticulate::conda_create(envname = 'r-reticulate', python_version = "3.7")
  reticulate::use_condaenv(condaenv = 'r-reticulate', required = TRUE)

  reticulate::py_config()
  reticulate::py_install("numpy")
  numpy <<- reticulate::import("numpy")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to MGLasso.")
  packageStartupMessage("Run install_conesta() to finalize the package python dependencies installation.")
}
