
skip_if_no_pylearn_parsimony <- function() {
  if (!"pylearn-parsimony" %in% reticulate::py_list_packages()$package)
    skip("pylearn_parsimony not available for testing")
}

skip_if_no_python <- function(verbose = FALSE) {
  if (!reticulate::py_available())
    skip("Python not available for testing")
}
