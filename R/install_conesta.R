# Inspired from https://github.com/OscarKjell/text/blob/master/R/0_0_text_install.R

conda_args <- reticulate:::conda_args

#' Install the python library pylearn-parsimony and other required libraries
#'
#' pylearn-parsimony contains the solver CONESTA used for the mglasso problem.
#'
#' @param conda Character. Path to conda executable. "auto" finds the path automatically.
#' @param extra_pack Character vector. Extra-packages to be installed.
#' @param py_version Character. Python version. It is advised to use a version ">=3.7,<3.10".
#' The latest version under which mglasso was developped is scipy 1.7.1 which is based on python ">=3.7,<3.10".
#' In turn, this version of scipy can only be associated with a version of numpy ">=1.16.5,<1.23.0"
#'
#' @return No return value.
#' @export
#'
install_conesta <- function(conda = "auto",
                            extra_pack = c("scipy == 1.7.1", "scikit-learn", "numpy == 1.22.4", "six",
                                           "matplotlib"), py_version = '3.8') {

  is_rmglasso_env_installed = tryCatch(reticulate::use_condaenv(envname = 'rmglasso', required = TRUE),
                                       error = function (e) {'not installed'})

  # Check if conda available on the system
  conda <- tryCatch(reticulate::conda_binary(conda), error = function(e) NULL)
  have_conda <- !is.null(conda)

  # Mac and linux
  if (is_unix()) {
    # check for explicit conda method
    # validate that we have conda
    if (!have_conda) {
      cat("No conda was found in the system. ")
      ans <- utils::menu(c("No", "Yes"), title = "Do you want mglassp to download
                           miniconda using reticulate::install_miniconda()?")
      if (ans == 2) {
        reticulate::install_miniconda(update = update_conda)
        conda <- tryCatch(reticulate::conda_binary("auto"), error = function(e) NULL)
      } else {
        stop("Conda environment installation failed (no conda binary found)\n", call. = FALSE)
      }
    }
    # Windows installation
  }

  # setup environment
  if (!is.null(is_rmglasso_env_installed)) {
    packageStartupMessage('mglasso requires the rmglasso conda environment. Attempting to create...')
    #reticulate::use_condaenv(envname = 'rmglasso', python = py_version)
    reticulate::conda_create(envname = 'rmglasso', python_version = py_version)
    message("Env created.")
  }

  reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE)

  check_install <- sapply(extra_pack, reticulate::py_module_available)

  pack_to_install <- extra_pack[!check_install]

  if(!identical(pack_to_install, character(0))){
    reticulate::py_install(pack_to_install,
                           envname = "rmglasso",
                           python_version = py_version)
  }

  if (!reticulate::py_module_available("pylearn-parsimony")) {
    # message('Installing pylearn-parsimony')
    message('Configuring the env')
    reticulate::use_condaenv(condaenv = 'rmglasso', required = TRUE)


    config <- reticulate::py_config()
    message('Rmglasso env configured ')
    system2(config$python, c("-m", "pip", "install",
                             shQuote("git+https://github.com/neurospin/pylearn-parsimony.git")))

    message("pylearn-parsimony is installed.")
  }
}

the_module <- function()
  try(reticulate::import_from_path("conesta_solver",
                                   path = path_python()), silent = TRUE)

path_python <- function(){
  system.file("python", package = "mglasso")
}



