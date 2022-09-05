testthat::test_that("conesta solver is loaded and works.", {

  skip_on_cran()

  print("Testing conesta solver on a block diagonal model ...")

  mglasso::install_pylearn_parsimony(envname = "rmglasso", method = "conda")
  reticulate::use_condaenv("rmglasso", required = TRUE)
  reticulate::py_config()
  n = 30
  K = 2
  p = 4
  rho = 0.85
  blocs <- list()
  for (j in 1:K) {
   bloc <- matrix(rho, nrow = p/K, ncol = p/K)
     for(i in 1:(p/K)) { bloc[i,i] <- 1 }
     blocs[[j]] <- bloc
     }
  mat.covariance <- Matrix::bdiag(blocs)
  mat.covariance
  set.seed(11)
  X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
  X <- scale(X)
  res <- conesta(X, 0.1, 0.1)

  testthat::expect_true(is.matrix(res))

})


