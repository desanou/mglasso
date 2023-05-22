#' def sequences for lambda1s and lambda2s
#' not sure if max of lambda1 s still the same as in the lasso case. But if find better equivalence will update this part
#' @param mean in conesta_rwrapper is the mean criterion used ie averaged by np
seq_l1l2 <- function(X, nlam1 = 2, nlam2 = 2, logscale = TRUE, mean = FALSE,
                     lambda1_min_ratio = 0.01, require_non_list = FALSE,
                     l2_max = NULL){

  p <- ncol(X)
  n <- nrow(X)
  S <- cov(scale(X))

  lambda1_max = max(max(S-diag(p)), -min(S-diag(p)))
  lambda1_min = lambda1_min_ratio*lambda1_max

  if(logscale){
    lambda1s = exp(seq(log(lambda1_max), log(lambda1_min), length = nlam1))
  }else{
    lambda1s <- seq(lambda1_min, lambda1_max, length.out = nlam1)
  }
  if(!mean)
    lambda1s = lambda1s*n ## due to mean = False

  if(is.null(l2_max))
    l2_max <- max(eigen(S)$values) ####### I think it should be max of absolute value, to be re-checked
  lambda2s <- seq(0, l2_max, length.out = nlam2)

  repl1 <- rep(lambda1s, each = nlam2)
  repl2 <- rep(lambda2s, nlam1)

  pen_params <- cbind(repl1, repl2)
  pen_params <- split(pen_params, rep(1:nrow(pen_params), ncol(pen_params)))

  if(!require_non_list){
    return(pen_params)
  }else{
    out <- list(pen_params = pen_params, lambda1s = lambda1s, lambda2s = lambda2s)

    return(out)
  }
}

#'@export
seq_l2_l1_fixed <- function(dt, l1, l2_start, l2_factor, nl2, mean = FALSE, l2_max = TRUE) {

  S <- cov(scale(dt))
  n = nrow(dt)
  p = ncol(dt)

  if(l2_max){
    l2_max <- max(eigen(S)$values)
    lambda2s <- seq(0, l2_max, length.out = nl2)
  }

  else{
    lambda2s <- l2_start * (l2_factor^seq(0, nl2, by = 1))
    lambda2s <- c(0, lambda2s)
  }

  lambda1s <- rep(l1, length(lambda2s))
  if(!mean)
    lambda1s = lambda1s*n ## due to mean = False

  pen_params <- cbind(lambda1s, lambda2s)
  pen_params <- split(pen_params, rep(1:nrow(pen_params), ncol(pen_params)))

  return(pen_params)
}

#'
criterion <- function(Theta, X){
  GSG <- t(Theta) %*% cov(X) %*% Theta
  GS <- t(Theta %*% cov(X))
  return(- 0.5 * sum(diag(GSG)) + sum(diag(GS)))
}

#' @param pair_param vector of l1 and l2 penalties
#' @export
mglasso_pair_param <- function(pair_param = NULL, X_, type = "initial",
                               fuse_thresh = 1e-6, prec = 1e-2) {
  clusters <- 1:ncol(X_)
  beta0  <- beta_to_vector(beta_ols(X_))

  selected_Theta <- conesta_rwrapper(X = X_, lam1 = pair_param[1], lam2 = pair_param[2],
                                     beta_warm = beta0, type_ = type,
                                     W_ = NULL, prec_ = prec)

  diffs <- dist_beta(selected_Theta, distance = "euclidean") ## Update distance matrix

  pairs_to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)
  if(nrow(pairs_to_merge) != 0){
    clusters <- merge_clusters(pairs_to_merge, clusters)  # merge clusters
  }

  nclusters <- length(unique(clusters))

  return(list(selected_Theta = selected_Theta, clusters = clusters, nclusters = nclusters))
}

pseudo_likelihood_mglasso <- function(X, nl1, nl2, lambda1s = NULL, lambda2s = NULL){
  n <- nrow(X)
  p <- ncol(X)
  X <- scale(X)

  if(is.null(lambda1s) & is.null(lambda2s))
    pen_params <- seq_l1l2(X, nlam1 = nl1, nlam2 = nl2)

  mglasso <- function(l1l2){
    l1_ <- l1l2[1]
    l2_ <- l1l2[2]
    beta0  <- beta_to_vector(beta_ols(X)) ## init OLS train

    beta <- conesta_rwrapper(X, lam1 = l1_, lam2 = l2_, beta_warm = beta0,
                             type_ = "initial", W_ = NULL, prec_ = 1e-2)

    return(beta)
  }

  Thetas <- mclapply(pen_params,
                     mglasso,
                     mc.cores = min(50, length(pen_params)))

  n_edges <- sapply(Thetas,
                    function(Theta_){
                      sum(Theta_ !=0 & col(Theta_) < row(Theta_))
                    }
  )

  ploglik <- sapply(Thetas, criterion, X)

  return(list(ploglik = ploglik, n_edges = n_edges, Thetas = Thetas, pen_params = pen_params,
              n = n, p = p))
}

which_params <- function(pl_, beta_){
  return(pl_$pen_params[[which(sapply(pl_$Thetas, function(e) identical(beta_, e)))]])
}

#' K-fold cross validation mglasso
select_kfold_mglasso <- function(X, lambda1s = NULL, lambda2s = NULL, K_fold = 5, nl1=1, nl2=1, lam1_min_ratio,
                                 verbose = TRUE){
  n                = nrow(X)
  p                = ncol(X)
  X                = scale(X)
  X_train          = X_test = X
  shuffled_indices = sample(n)
  clusters <- 1:p

  if(is.null(lambda1s) & is.null(lambda2s)) {
    pen_params <- seq_l1l2(X, nlam1 = nl1, nlam2 = nl2, lambda1_min_ratio = lam1_min_ratio)
    errors   <- matrix(NA, nrow = nl1*nl2, ncol = K_fold)
  }else{
    nl1 <- length(lambda1s)
    nl2 <- length(lambda2s)
    errors   <- matrix(NA, nrow = nl1*nl2, ncol = K_fold)
  }

  for (k in 1:K_fold) {
    if(K_fold > 1){
      fold_size     = n/K_fold
      first_element = 1 + floor((k-1)*fold_size)
      last_element  = floor(k*fold_size)

      to_leave = shuffled_indices[first_element:last_element]
      X_train  = X[-to_leave, , drop = FALSE]
      X_test   = X[to_leave, , drop = FALSE]
    }

    error_classic <- function(Theta, X){
      n <- nrow(X)
      p <- ncol(X)

      error_prediction <- function(rvn){
        norm(X[,rvn] - X %*% Theta[rvn,], "2")^2/n #control the solve by column or line thing before
      }
      return(sum(sapply(1:p, error_prediction))/p)
    }

    mglasso <- function(l1l2){
      l1_ <- l1l2[1]
      l2_ <- l1l2[2]
      beta0  <- beta_to_vector(beta_ols(X_train)) ## init OLS train

      Theta_train <- conesta_rwrapper(X = X_train, lam1 = l1_, lam2 = l2_, beta_warm = beta0,
                                      type_ = "initial", W_ = NULL,
                                      max_iter_=1000, prec_=1e-4)

      errors_ik = error_classic(Theta_train, X_test)

      return(errors_ik)
    }

    errors[ ,k] <- sapply(pen_params, mglasso)
    # get the i somewhere or fuse the list in a vector and input it the matrix column

    if(verbose) message(paste0(k, "-th"), "fold done")
  }

  mean_errors <- apply(errors, 1, mean)
  best_lambdas_pair <- pen_params[[which.min(mean_errors)]]
  errors_min  <- min(mean_errors)

  ### Evaluate model on FULL data with selected parameters
  eval_mgl <- mglasso_pair_param(best_lambdas_pair, X)

  res <- list(selected_Theta = eval_mgl$selected_Theta, clusters = eval_mgl$clusters,
              pen_params = pen_params, errors = errors,
              best_lambdas_pair = best_lambdas_pair, errors_min = errors_min)

  return(res)
}

#' stability selection mglasso
select_stab_mglasso <- function(X, l1_, l2_, subsample_ratio, nrep, stab_thresh){
  n <- nrow(X)
  p <- ncol(X)

  variability = 0
  merge = matrix(0, p, p)

  if(is.null(subsample_ratio))
  {
    if(n>144) subsample_ratio = 10*sqrt(n)/n
    if(n<=144) subsample_ratio = 0.8
  }

  for(i in 1:nrep){
    ind_sample = sample(c(1:n), floor(n*subsample_ratio), replace=FALSE)

    beta0  <- beta_to_vector(beta_ols(X[ind_sample, ]))
    Beta <- conesta_rwrapper(X =X[ind_sample, ], lam1 =l1_, lam2 = l2_,
                             beta_warm = beta0, type_ = "initial", W_ = NULL,
                             max_iter_=1000, prec_=1e-3)

    graph <- abs(sign(Beta))
    graph <- symmetrize(graph, "and")
    diag(graph) <- 0

    merge <- merge + graph
  }

  merge_prop <- merge / nrep

  # variability <- 4 * sum(merge_prop * (1 - merge_prop) / (p * (p - 1)))
  #
  # variability

  return(merge_prop)
}

#' stability selection mglasso II stars way
select_stars_mglasso <- function(X, lambda1s = NULL, lambda2s = NULL, subsample_ratio = NULL,
                                 nrep = 1, stars_thresh = 0.1, nl1 = 1, nl2 = 1){
  n <- nrow(X)
  p <- ncol(X)
  ncores <- detectCores() - 1

  if(is.null(subsample_ratio))
  {
    if(n>144) subsample_ratio = 10*sqrt(n)/n
    if(n<=144) subsample_ratio = 0.8
  }

  if(is.null(lambda1s) & is.null(lambda2s))
    pen_params <- seq_l1l2(X, nlam1 = nl1, nlam2 = nl2)
  rep_pen_params <- rep(pen_params, nrep)


  conesta_rwrap <- function(param) {
    ind_sample = sample(c(1:n), floor(n*subsample_ratio), replace=FALSE)

    beta0  <- beta_to_vector(beta_ols(X[ind_sample, ]))
    Beta <- conesta_rwrapper(X = X[ind_sample, ], lam1 = param[1], lam2 = param[2],
                             beta_warm = beta0, type_ = "initial", W_ = NULL,
                             prec_=1e-2)

    graph <- abs(sign(Beta))
    graph <- symmetrize(graph, "and")
    diag(graph) <- 0

    graph
  }

  merges_all <- mclapply(rep_pen_params, conesta_rwrap, mc.cores = min(ncores, length(rep_pen_params)))

  all_index <- rep(1:length(pen_params), nrep)
  list_index_to_sum <- lapply(1:length(pen_params),
                              function(e) {
                                which(all_index == e)
                              })

  merges <- lapply(list_index_to_sum,
                   function(index_to_sum) {
                     Reduce(`+`, merges_all[index_to_sum])/nrep
                   })

  variability <- sapply(1:length(pen_params),
                        function(k) {
                          4 * sum(merges[[k]] * (1 - merges[[k]]) / (p * (p - 1)))
                        })

  #opt_index = max(which.max(variability >= stars_thresh)[1]-1,1)
  opt_index = max(which.max(variability >= stars_thresh)[1]-1,1)
  opt_pair = pen_params[[opt_index]]

  ### Evaluate model on FULL data with selected parameters
  eval_mgl <- mglasso_pair_param(opt_pair, X)

  res <- list(merges = merges, variability = variability, opt_pair=opt_pair, opt_index=opt_index,
              selected_Theta = eval_mgl$selected_Theta, clusters = eval_mgl$clusters,
              merges_all = merges_all, pen_params = pen_params)

  return(res)
}

proc_select_real_data <- function(X, nl1, nl2, l1_min_ratio, verbose = TRUE){
  beta_ebic = beta_capushe = NULL
  if(verbose) cat("Conducting Kfold selection")
  beta_cv   <- select_kfold_mglasso(X, nl1 = nl1, nl2 = nl2, lam1_min_ratio = l1_min_ratio)
  if(verbose) cat("Conducting pseudo-likelihood calculations")
  pl        <- pseudo_likelihood_mglasso(X, nl1 = nl1, nl2 = nl2)
  if(sum(pl$n_edges >= 1))
    beta_ebic <- select_ebic_weighted(pl$Thetas, pl$ploglik, pl$n_edges, pl$n, pl$p, 0.5, pl$pen_params)
  if(nl1*nl2 >=10 & sum(pl$n_edges >= 1))
    beta_capushe <- select_capushe(pl$Thetas, pl$ploglik, pl$n_edges, pl$n, pl$pen_params)
  if(verbose) cat("Conducting stars selection")
  beta_stars <- select_stars_mglasso(mathmarks, nrep = 20, stars_thresh = 0.2, nl1 = nl1, nl2 = nl2)

  res <- list(beta_cv = beta_cv, beta_ebic = beta_ebic, beta_capushe = beta_capushe, beta_stars = beta_stars, pl = pl)

  return(res)
}
