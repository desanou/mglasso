.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to MGLasso.")
  packageStartupMessage("Run install_conesta() to finalize the package python dependencies installation.")
}

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname, force = TRUE)
}
