testthat::test_that("pylearn-parsimony library is installed", {
  reticulate::conda_create(envname = 'r-reticulate')
  reticulate::use_condaenv(condaenv = 'r-reticulate')
  install_conesta()
  testthat::expect_true("pylearn-parsimony" %in% reticulate::py_list_packages("r-reticulate")$package)
})
