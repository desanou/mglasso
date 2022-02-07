testthat::test_that("pylearn-parsimony library is installed", {
  install_conesta()
  skip_if_no_pylearn <- function() {
    have_pylearn <- py_module_available("pylearn-parsimony")
    if (!have_pylearn)
      skip("scipy not available for testing")
  }
})
