testthat::test_that("pylearn-parsimony library is installed", {
  install_conesta()

  testthat::expect_true(reticulate::py_module_available("pylearn-parsimony"))

})
