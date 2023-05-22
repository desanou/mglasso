## to do
## recuperer sd des erreurs
#' compute TPR, FPR, SHD given estimated and true precision matrices
#'
#' @details SHD: structural hamming distance
perf_one <- function (omega_hat, omega)
{
  omega_hat <- as.matrix(omega_hat)
  omega <- as.matrix(omega)

  upper <- upper.tri(omega_hat)

  true_nzero <- which(omega[upper] != 0)
  true_zero  <- which(omega[upper] == 0)

  nzero <- which(omega_hat[upper] != 0)
  zero  <- which(omega_hat[upper] == 0)

  TP <- sum(nzero %in% true_nzero)
  TN <- sum(zero %in%  true_zero)
  FP <- sum(nzero %in% true_zero)
  FN <- sum(zero %in%  true_nzero)

  tpr    <- TP/(TP + FN) ## also recall and sensitivity
  fpr   <- FP/(FP + TN) ## also 1 - specificit
  prec <- TP/(TP+FP)
  tpr[TP + FN == 0] <- NA
  fpr[TN + FP == 0] <- NA
  prec[TP + FP == 0] <- NA

  # Hamming distance
  omega[omega != 0] = 1
  omega_hat[omega_hat != 0] = 1

  shd <- sum(omega[upper] != omega_hat[upper]) / length(omega[upper])

  return(list(tpr = tpr, fpr = fpr, prec = prec, shd = shd))
}

#' get performances from list of estimations
perf_vec <- function(omega_hat_list, omega){
  pf <- tpr <- fpr <- prec <- shd <- NULL

  for(i in 1:length(omega_hat_list)){
    pf <- perf_one(omega_hat_list[[i]], omega)
    tpr <- c(tpr, pf$tpr)
    fpr <- c(fpr, pf$fpr)
    prec <- c(prec, pf$prec)
    shd <- c(shd, pf$shd)
  }

  fpr <- c(0, fpr, 1)
  tpr <- c(0, tpr, 1)
  prec <- c(1, prec, 0)
  shd <- c(0, shd, 1)

  res <- as.data.frame(tpr)
  res$fpr <- fpr
  res$prec <- prec
  res$shd <- shd

  return(res)
}

#' Plot ROC curve and calculate AUC
#'
#' @param type Classical ROC curve tpr = f(FPR) or TPR = f(precision) adjusted version.
#' Compute AUC and partial  AUC
#'
#' @import DescTools
get_auc <- function(omega_hat_list, omega, to = to_){
  pf_vec <- perf_vec(omega_hat_list, omega)
  x   <- pf_vec$fpr
  tpr <- pf_vec$tpr

  auc <- AUC(x, tpr, to = to_)

  return(auc)
}

#' cah_glasso
cah_glasso <- function(num_clusters, data, lam1, hclust_obj) {
  data <- scale(data)

  cah_cut <- cutree(hclust_obj, k = num_clusters)

  X_grouped <- t(aggregate(t(data), list(cah_cut), "mean"))
  X_grouped <- X_grouped[-1,]

  # cah_glasso_obj <- huge(x      = scale(X_grouped),
  #                        lambda = lam1)

  beta0  <- beta_to_vector(beta_ols(scale(X_grouped))) ## init OLS train
  beta_cah_glasso <- glasso(cov(scale(X_grouped)), rho = lam1, thr = 1e-2, approx = TRUE,
                            wi.init = beta0)$wi

  #beta_cah_glasso <- cah_glasso_obj$beta[[1]]
  #beta_cah_glasso <- symmetrize(beta_cah_glasso, "and")
  beta_cah_glasso <- expand_beta(beta_cah_glasso, cah_cut)

  return(list(beta_cah_glasso=beta_cah_glasso, nclusters =  num_clusters))
}

#'neighbor_select
#'@export
neighbor_select <- function(data = data$X, config, lambda_min_ratio = 1e-2, nlambda = 10,
                            nresamples = 20, lambdas = NULL, model = NULL,
                            verbose = FALSE, estim_var = NULL){

  p = ncol(data)
  if(is.null(lambdas)) {
    S <- cov(scale(data))
    lambda_max = max(max(S-diag(p)), -min(S-diag(p)))
    lambda_min = lambda_min_ratio*lambda_max
    lambdas = exp(seq(log(lambda_max), log(lambda_min), length = nlambda))
  }

  if (is.null(estim_var)) {
    if(!is.null(model)) {
      estim_var <- estimate_variability(nresamples, config$n, config$p, config$dnsty,
                                        config$pi, config$alpha, config$rho, model)
    }
  }

  mb_out <- huge.select(est       = huge(x       = scale(data),
                                         method  = "mb",
                                         verbose = verbose,
                                         sym     = "and",
                                         lambda = lambdas
  ),
  criterion = "stars",
  verbose   = verbose,
  stars.thresh = estim_var,
  rep.num = nresamples)

  lambda1 <- mb_out$opt.lambda

  beta_glasso <- mb_out$beta[[which(mb_out$lambda == lambda1)]]
  #beta_glasso <- symmetrize(beta_glasso, "and")

  return(list(beta_glasso = beta_glasso, lambda_opt = lambda1, lambdas = lambdas))
}

cah_glasso_loop <- function(lam1, data_){
  data_ = scale(data_)
  p = ncol(data_)
  cah <- hclust(dist(t(data_)), method = "ward.D")

  one_run <- function(num_clusters){
    cah_cut <- cutree(cah, k = num_clusters)
    X_grouped <- t(aggregate(t(data_), list(cah_cut), "mean"))
    X_grouped <- X_grouped[-1,]

    # cah_glasso_obj <- huge(x      = scale(X_grouped),
    #                        lambda = lam1,
    #                        verbose = FALSE)

    beta0  <- beta_to_vector(beta_ols(scale(X_grouped))) ## init OLS train
    beta_cah_glasso <- glasso(cov(scale(X_grouped)), rho = lam1, thr = 1e-2, approx = TRUE,
                              wi.init = beta0)$wi

    #beta_cah_glasso <- cah_glasso_obj$beta[[1]]
    #beta_cah_glasso <- symmetrize(beta_cah_glasso, "and")
    beta_cah_glasso <- expand_beta(beta_cah_glasso, cah_cut)
    return(beta_cah_glasso)
  }

  out_list <- lapply(2:p, one_run)

  return(out_list)
}


