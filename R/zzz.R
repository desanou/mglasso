solver_module <- NULL

.onLoad <- function(libname, pkgname) {
   solver_module <<- reticulate::import_from_path("conesta_solver",
                                                 path = system.file("python",
                                                                    package = "mglasso"),
                                                 delay_load = TRUE)
}
