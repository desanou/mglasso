solver_module <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname, force = TRUE)

  solver_module <<- reticulate::import_from_path("conesta_solver",
                                                 path = system.file("python", package = "mglasso"),
                                                 delay_load = TRUE)
}
