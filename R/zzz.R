.onAttach <- function(libname, pkgname) {

}


.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname, force = TRUE)
}
