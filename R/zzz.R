.onAttach <- function(libname, pkgname) {
}

.onLoad <- function(libname, pkgname) {

  # Check if conda available on the system
  conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  have_conda <- !is.null(conda)

  if (!have_conda) {
    cat("No conda was found in the system. ")
    ans <- utils::menu(c("No", "Yes"), title = "Do you want mglasso to download
                           miniconda using reticulate::install_miniconda()?")
    if (ans == 2) {
      reticulate::install_miniconda()
      conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)
    } else {
      stop("Conda environment installation failed (no conda binary found)\n", call. = FALSE)
    }
  }

  # setup environment
  is_rmglasso_env_installed = tryCatch(reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE),
                                       error = function (e) {'not installed'})
  if (!is.null(is_rmglasso_env_installed)) {
    packageStartupMessage('mglasso requires the rmglasso conda environment. Attempting to create...')
    reticulate::conda_create(envname = 'rmglasso', python_version = conda_py_version)
    message("Env created.")
  }

  reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE)

}
