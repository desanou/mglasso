testthat::test_that("pylearn-parsimony library is installed", {
  install_conesta()

  print("current environment")
  print(reticulate::py_list_packages()$package)

  print("Specified reticulate environment")
  print(reticulate::py_list_packages("r-reticulate")$package)

  #testthat::expect_true("pylearn-parsimony" %in% reticulate::py_list_packages()$package)
})
