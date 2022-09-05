

test_that("All python dependencies are installed", {

  skip_on_cran()

  install_pylearn_parsimony(envname = "rmglasso", method = "conda")
  reticulate::use_condaenv("rmglasso")

  expect_true(reticulate::py_module_available("numpy"))
  expect_true(reticulate::py_module_available("scipy"))
  expect_true(reticulate::py_module_available("six"))
  expect_true(reticulate::py_module_available("matplotlib"))
  expect_true("scikit-learn" %in% reticulate::py_list_packages()$package)
  expect_true("pylearn-parsimony" %in% reticulate::py_list_packages()$package)

})
