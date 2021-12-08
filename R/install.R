# install_conesta <- function(envname = "r-reticulate",
#                             restart_session = TRUE) {
#
#   # Check for anaconda
#   if (is.null(reticulate::conda_binary())) {
#     stop("You need to install Anaconda or add it in the system path.")
#   }
#
#   # setup environment
#   reticulate::py_config()
#   #path <- system.file("python", package = "mglasso") ### !!!!!!!!!
#   path <- "./inst/python/"
#
#   #if (!reticulate::py_module_available("scipy")) {
#   reticulate::py_install("scipy", method = "auto", conda = "auto", pip=TRUE, envname = "r-reticulate")
#   #}
#
#   if (!reticulate::py_module_available("scikit-learn")) {
#     reticulate::py_install("scikit-learn", method = "auto", conda = "auto", pip=TRUE, envname = "r-reticulate")
#   }
#
#   if (!reticulate::py_module_available("parsimony.estimators")) {
#     text <- paste("sys.path.insert(0,'", path, "/pylearn-parsimony')",
#                   sep = "",
#                   collapse = NULL)
#     reticulate::py_run_string("import sys")
#     reticulate::py_run_string(text)
#   }
#
#   # loading conesta solver
#   the_module <- reticulate::import_from_path("conesta_solver", path = path)
#   conesta_rwrapper <<- the_module$conesta_rwrapper
#
# }
