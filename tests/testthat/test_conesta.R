testthat::test_that("conesta returns a matrix.", {

  print("Testing conesta solver on a block diagonal model ...")

  print(reticulate::conda_list())

  install_conesta()

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

  set.seed(11)
  X <- mvtnorm::rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
  X <- scale(X)

  res <- conesta(X = X, lam1 = 0.1, lam2 = 0.1)

  testthat::expect_true(is.matrix(res))
})
