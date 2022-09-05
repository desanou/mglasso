# Inspired from
# https://github.com/mrchypark/theeuh/blob/5dbe086967ba46d62f0ef3ca8c8efa9f33dcf66c/R/install.R

#' Install the python library pylearn-parsimony and other required libraries
#'
#' pylearn-parsimony contains the solver CONESTA used for the mglasso problem
#' and is available on github at https://github.com/neurospin/pylearn-parsimony
#' It is advised to use a python version ">=3.7,<3.10".
#' Indeed, the latest version of scipy under which mglasso was developped is scipy 1.7.1 which is based on python ">=3.7,<3.10".
#' In turn, this version of scipy can only be associated with a version of numpy ">=1.16.5,<1.23.0"
#'
#' @inheritParams reticulate::py_install
#' @param extra_pack Character vector. Extra-packages to be installed.
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio)
#' @param ... additionnal arguments passed to [`reticulate::py_install()`]
#'
#' @return No return value.
#' @export
#'
install_pylearn_parsimony <- function(method = c("auto", "virtualenv", "conda"),
                                      conda = "auto",
                                      extra_pack = c("scipy == 1.7.1",
                                                     "scikit-learn",
                                                     "numpy == 1.22.4",
                                                     "six",
                                                     "matplotlib"),
                                      python_version = '3.8',
                                      restart_session = TRUE,
                                      envname = NULL,
                                      ...) {


  method <- match.arg(method)

  if (.Machine$sizeof.pointer != 8) {
    stop("Binary installation is only available for 64-bit platforms.")
  }

  reticulate::py_install(packages = c("pylearn-parsimony", extra_pack),
                         envname = envname,
                         method = method,
                         conda = conda,
                         python_version = python_version,
                         pip = TRUE,
                         pip_options = 'git+https://github.com/neurospin/pylearn-parsimony.git',
                         ...)

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}
