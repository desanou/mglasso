testthat::test_that("pylearn-parsimony library is installed", {
  install_conesta()

  print("current environment")
  print(reticulate::py_list_packages()$package)

  print("Specified reticulate virtual environment")
  print(reticulate::py_list_packages(envname = "r-reticulate",
                                     type = "virtualenv")$package)

  print("base")
  print(reticulate::py_list_packages("base")$package)

  testthat::expect_true("pylearn-parsimony" %in% reticulate::py_list_packages("r-reticulate", "virtualenv")$package)
})
