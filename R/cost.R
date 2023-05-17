#' apply function FUN to lines while reordering the coefficients
#' 
#' the j-th coefficient in Beta[j,] is permuted with the i-th coefficient
#' 
#'@param i indice of the line
#'@param j indice of the line
#'
#'@examples
#'Beta <- matrix(round(rnorm(9),2), ncol = 3)
#'diag(Beta) <- 0
#'Beta
#'FUN_lines(1, 2, Beta)
#'FUN_lines(2, 1, Beta)
FUN_lines <- function(i, j, Beta, FUN = `-`, ni = 1, nj = 1){
  Y_coeffs <- Beta[i,]
  X_coeffs <- Beta[j,]
  X_i <- X_coeffs[i]
  X_coeffs[i] <- X_coeffs[j]
  X_coeffs[j] <- X_i
  
  FUN(ni*Y_coeffs, nj*X_coeffs)
}

FUN_lines_wr <- function(i, j, Beta, FUN = `-`, ni = 1, nj = 1){
  Y_coeffs <- Beta[i,]
  X_coeffs <- Beta[j,]
  Y_coeffs[j] <- 0
  X_coeffs[i] <- 0
  X_i <- X_coeffs[i]
  X_coeffs[i] <- X_coeffs[j]
  X_coeffs[j] <- X_i
  
  FUN(ni*Y_coeffs, nj*X_coeffs)
}

#' cost function
cost <- function(Beta, X){
  p <- ncol(X)
  L <- 0
  
  if(length(p) != 0){
    L <- sum(sapply(1:p, function(i){norm(X[,i] - X %*% Beta[i,], type = "2")^2})) ## loss term
  }
  
  return(0.5*L)
}


#' lagrangian function
#' 
#' Beta and X must have the same number of variables
#' 
#'@param Beta numeric matrix. In rows, regression vectors coefficients following of node-wise regression. diag(Beta) = 0 
#'@param X numeric matrix. Data with variables in columns.
#'@param lambda1 numeric scalar. Lasso penalization parameter.
#'@param lambda2 numeric scalar. Fused-group Lasso penalization parameter.
#'
#'@return numeric scalar. The lagrangian
#'
#'@examples
#'
#'## Generation of K block partitions 
#'n = 50
#'K = 3
#'p = 6
#'rho = 0.85
#'blocs <- list()
#'for (j in 1:K) {
#'    bloc <- matrix(rho, nrow = p/K, ncol = p/K) 
#'    for(i in 1:(p/K)) { bloc[i,i] <- 1 } 
#'    blocs[[j]] <- bloc
#'    }
#'mat.covariance <- bdiag(blocs)
#'set.seed(11)
#'X <- rmvnorm(n, mean = rep(0,p), sigma = as.matrix(mat.covariance))
#'X <- scale(X)
#'
#'## Initialization for Beta
#'Beta1 <- matrix(0, nrow = p, ncol = p)
#'for(i in 1:p){
#'   Beta1[i,-i] <- solve(t(X[,-i])%*%X[,-i]) %*% t(X[,-i]) %*% X[,i]
#'   }
#'lagrangian(Beta, X, 0, 0)
lagrangian <- function(Beta, X, lambda1 = 0, lambda2 = 0){
  p <- ncol(X)
  P2 <- 0
  L <- 0
  
  if(length(p) != 0){
    L <- sum(sapply(1:p, function(i){norm(X[,i] - X %*% Beta[i,], type = "2")^2})) ## loss term
    
    for(i in 1:(p-1)){
      for(j in (i+1):p){
        P2 = P2 + norm(FUN_lines(i, j, Beta, `-`), type = "2")
      }
    } ## fuse-group lasso penalty
  }
  
  
  P1 <- sum(abs(Beta))  ## lasso penalty
  
  return(0.5*L + lambda1*P1 + lambda2*P2)
}

#' vectorize beta matrix
beta_to_vector <- function(beta_mat){
  beta_mat <- as.matrix(beta_mat)
  diag(beta_mat) <- NA
  beta_mat <- as.vector(t(beta_mat))
  beta_mat <- beta_mat[which(!is.na(beta_mat))]
  beta_mat
}



precision_to_regression <- function(K){
  p <-  ncol(K)
  mat <- matrix(0,p,p)
  
  for (i in 1:p) {
    for (j in 1:p) {
      if(i != j)
        mat[i, j] <- - K[i,j]/K[i,i]
    }
  }
  
  mat
}

#' Init Beta via OLS
beta_ols <- function(X){
  X <- as.matrix(X)
  p <- ncol(X)
  Beta <- matrix(0, nrow = p, ncol = p)
  
  # if(p <= nrow(X)){
  #   f <- function(i){
  #     bi <- as.vector(solve(t(X[,-i])%*%X[,-i]) %*% t(X[,-i]) %*% X[,i])
  #     R.utils::insert(bi, ats = i, values = 0)
  #   }
  # }else{
  f <- function(i){
    bi <- as.vector(corpcor::pseudoinverse(t(X[,-i])%*%X[,-i]) %*% t(X[,-i]) %*% X[,i])
    R.utils::insert(bi, ats = i, values = 0)
  }
  # }
  
  Beta <- sapply(1:p, f)
  t(Beta)
}

#' Init Beta 1 matrix
beta_idty <- function(p){
  Beta <- matrix(1, nrow = p, ncol = p)
  diag(Beta) <- 0
  
  return(Beta)
}

#' symmetrize matrix of regression vectors pxp
symmetrize <- function(mat, rule = "and") {
  diag(mat) <- 0
  if (rule == "and") {
    mat <- sign(mat) * pmin(abs(mat), t(abs(mat)))
  } else{
    ## or rule
    mat <- pmax(mat, t(mat)) - pmax(-mat, -t(mat))
  }
  return(mat)
}
