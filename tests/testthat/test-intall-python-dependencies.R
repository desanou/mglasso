


test_that("All python dependencies are installed", {
  skip_if_no_python()

  install_pylearn_parsimony(envname = "rmglasso", method = "conda")
  reticulate::use_condaenv("rmglasso")

  expect_true(reticulate::py_available("numpy"))
  expect_true(reticulate::py_available("scipy"))
  expect_true(reticulate::py_available("six"))
  expect_true(reticulate::py_available("matplotlib"))
  expect_true(reticulate::py_available("scikit-learn"))
  expect_true("pylearn-parsimony" %in% reticulate::py_list_packages()$package)

})
