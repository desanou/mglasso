testthat::test_that("pylearn-parsimony library is installed", {
  install_conesta()

  testthat::expect_true("pylearn-parsimony" %in% reticulate::py_list_packages()$package)
})
