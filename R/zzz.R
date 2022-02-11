numpy <- NULL
.onLoad <- function(libname, pkgname) {
  reticulate::py_install("numpy")
  numpy <<- reticulate::import("numpy")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to MGLasso.")
  packageStartupMessage("Run install_conesta() to finalize the package python dependencies installation.")
}
