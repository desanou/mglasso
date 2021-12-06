#' simulate data with given graph structure
#'
#' @return A list:
#' tree_true: an object of class `phylo`. The true dendrogram, returned if the structure is simulated from a tree.
sim_data <- function(p = 20,
                     np_ratio = 2,
                     structure = c("from_tree", "scale_free", "stochastic_block", "planted", "block_diagonal"),
                     sparsity = 0,
                     alpha,
                     prob_mat,
                     members,
                     K, rho, prop){

  structure <- match.arg(structure)
  n = round(np_ratio * p)
  tree = NULL
  if(structure != "block_diagonal"){

    zz       <- simulate_graph(type = structure, p = p, alpha, prob_mat, members = members)
    graph       <- zz$adjm
    tree <- zz$tree

    correlation <- graph.to.correlation(graph, sparsity.prob = sparsity)$correlation
  }else{
    K <- bloc_diag(p, prob_mat, alpha, rho)
    correlation <- solve(K)
    graph = K
  }

  X           <- mvtnorm::rmvnorm(n, sigma = correlation)

  return(list(X=X, graph=graph, correlation=correlation, tree_true=tree))
}

#'return precision matrix
bloc_diag <- function(n_vars, connectivity_mat, prop_clusters, rho) {

  network <- rNetwork(n_vars, connectivity_mat, prop_clusters)

  precision_mat <- network$A[order(network$clusters), order(network$clusters)]

  eff_clusters <- table(network$clusters)

  b = -rho/(1+rho*(eff_clusters-2) -rho^2*(eff_clusters-1))
  d = (1+rho*(eff_clusters-2))/(1+rho*(eff_clusters-2) -rho^2*(eff_clusters-1))


  for (i in 1:length(eff_clusters)) {
    #bloc <- matrix(b[i], eff_clusters[i], eff_clusters[i])
    temp <- precision_mat[which(row.names(precision_mat) == i), which(row.names(precision_mat) == i)]
    temp <- as.matrix(temp)

    temp[temp != 0] = b[i]
    diag(temp) = d[i]

    precision_mat[which(row.names(precision_mat) == i), which(row.names(precision_mat) == i)] = temp

  }

  flag <- min(svd(precision_mat)$d)
  if(flag < 0){
    diag(precision_mat) <- 1 - flag

  }

  return(precision_mat)
}

one_simu <- function(list_ii_rho){
  ii    = list_ii_rho[1]
  n     = list_ii_rho[2]
  rho   = list_ii_rho[3]
  dnsty = list_ii_rho[4]

  pi   <-  diag(dnsty, 3)
  data <- sim_data(p, n/p, "block_diagonal", prob_mat = pi, alpha = alpha, rho = rho)

  lambda_max <- max(abs(cov(scale(data$X))))
  lambda_min <- 1e-3
  lambdas    <- seq(lambda_min, lambda_max, length.out = 100)

  estim_var <- estimate_variability(100, n, p, dnsty, pi, alpha, rho)

  mb_out <- huge.select(est       = huge(x       = scale(data$X),
                                         method  = "mb",
                                         verbose = FALSE,
                                         sym     = "and",
                                         lambda = rev(lambdas)
  ),
  criterion = "stars",
  verbose   = FALSE,
  stars.thresh = estim_var)

  # mb_out <- simone(X       = scale(data$X),
  #                  control = setOptions(edges.steady   = "neighborhood.selection",
  #                                       edges.sym.rule = "AND",
  #                                       verbose        = FALSE))

  lambda1 = mb_out$opt.lambda
  # lambda1 = mb_out$penalties[which.max(mb_out$AIC)]

  beta_glasso <- t(mb_out$beta[[which(mb_out$lambda == lambda1)]])
  # beta_glasso = - mb_out$networks[[which.max(mb_out$BIC)]]
  diag(beta_glasso) = 0
  beta_glasso <- sign(beta_glasso) * pmin(abs(beta_glasso),t(abs(beta_glasso))) ## AND RULE

  beta_true <- precision_to_regression(data$graph)

  lambda2_start = 0.01
  lambda2_factor = 1.4

  mglasso_obj <- hggm_no_merge(scale(data$X), lambda1 = lambda1*n, fuse_thresh = 1e-3,
                               lambda2_start = lambda2_start,  lambda2_factor = lambda2_factor)


  error_glasso = error_mglasso = error_cahglasso = nclust = NULL
  fpr_glasso = fpr_mglasso = fpr_cahglasso = NULL
  tpr_glasso = tpr_mglasso = tpr_cahglasso = NULL
  shd_glasso = shd_mglasso = shd_cahglasso = NULL


  rand_cah = rand_mglasso = rand_kmeans = NA

  cah = hclust(dist(t(data$X)))

  for (j in 1:length(mglasso_obj$out)){
    num_clusters <- length(unique(mglasso_obj$out[[j]]$clusters))

    if(num_clusters != 1){
      ##### CAH followed by GLASSO
      cah_cut <- cutree(cah, k = num_clusters)
      X_grouped <- t(aggregate(t(data$X), list(cah_cut), "mean"))
      X_grouped <- X_grouped[-1,]
      cah_glasso_obj <- huge(x      = scale(X_grouped),
                             lambda = lambda1)
      # cah_glasso_obj <- simone(X       = scale(X_grouped),
      #                          control = setOptions(penalties      = lambda1,
      #                                               edges.steady   = "neighborhood.selection",
      #                                               edges.sym.rule = "NO"))
      beta_cah_glasso <- t(cah_glasso_obj$beta[[1]])
      # beta_cah_glasso <- - cah_glasso_obj$networks[[1]]
      diag(beta_cah_glasso) = 0
      beta_cah_glasso <- sign(beta_cah_glasso) * pmin(abs(beta_cah_glasso),t(abs(beta_cah_glasso))) ## AND RULE

      ## symmetrize
      beta_cah_glasso <- expand_beta(beta_cah_glasso, cah_cut)

      ###### MGLASSO
      beta_mglasso <- mglasso_obj$out[[j]]$Beta

      error_glasso <- c(error_glasso, norm(beta_glasso - beta_true, type = "F"))
      error_mglasso <- c(error_mglasso, norm(beta_mglasso - beta_true, type = "F"))
      error_cahglasso <- c(error_cahglasso, norm(beta_cah_glasso - beta_true, type = "F"))


      ####### AND OR Rule
      beta_mglasso.and <- sign(beta_mglasso) * pmin(abs(beta_mglasso),t(abs(beta_mglasso)))
      #temp.or <- pmax(temp,t(temp)) - pmax(-temp,-t(temp))

      tpr_glasso <- c(tpr_glasso, perf(beta_glasso, beta_true)$tpr)
      fpr_glasso <- c(fpr_glasso, perf(beta_glasso, beta_true)$fpr)
      shd_glasso <- c(shd_glasso, perf(beta_glasso, beta_true)$shd)

      tpr_mglasso <- c(tpr_mglasso, perf(beta_mglasso.and, beta_true)$tpr)
      fpr_mglasso <- c(fpr_mglasso, perf(beta_mglasso.and, beta_true)$fpr)
      shd_mglasso <- c(shd_mglasso, perf(beta_mglasso.and, beta_true)$shd)

      tpr_cahglasso <- c(tpr_cahglasso, perf(beta_cah_glasso, beta_true)$tpr)
      fpr_cahglasso <- c(fpr_cahglasso, perf(beta_cah_glasso, beta_true)$fpr)
      shd_cahglasso <- c(shd_cahglasso, perf(beta_cah_glasso, beta_true)$shd)

      nclust <- c(nclust, num_clusters)
    }

    if(num_clusters == 3){
      cah_cut <- cutree(cah, k = 3)
      mglasso_cut <- mglasso_obj$out[[j]]$clusters
      kmeans_cut <- kmeans(t(data$X), centers = 3)$cluster

      rand_cah <- adjustedRandIndex(cah_cut, as.numeric(colnames(data$correlation)))
      rand_mglasso <- adjustedRandIndex(mglasso_cut, as.numeric(colnames(data$correlation)))
      rand_kmeans <- adjustedRandIndex(kmeans_cut, as.numeric(colnames(data$correlation)))
    }

  }

  # res <- as.data.frame(cbind(error_glasso, error_mglasso, error_cahglasso,
  #                            tpr_glasso, tpr_mglasso, tpr_cahglasso,
  #                            fpr_glasso, fpr_mglasso, fpr_cahglasso,
  #                            shd_glasso, shd_mglasso, shd_cahglasso,
  #                            nclust))

  ####
  ####
  perfs <- c(error_glasso, error_mglasso, error_cahglasso,
             tpr_glasso, tpr_mglasso, tpr_cahglasso,
             fpr_glasso, fpr_mglasso, fpr_cahglasso,
             shd_glasso, shd_mglasso, shd_cahglasso,
             rand_cah, rand_kmeans, rand_mglasso)
  res <- data.frame(perfs)

  ncluster <- c(rep(nclust, 12), rep(3, 3))
  kk <- length(nclust)
  rep1 <- rep(c("glasso", "mglasso", "cah_glasso"), each = kk)

  res$perfs <- perfs
  res$crit <- rep(c("error", "tpr", "fpr", "shd", "rand"),
                  times = c(3*kk, 3*kk, 3*kk, 3*kk, 3))
  res$method <- c(rep(rep1, 4), c("cah_clust", "kmeans_clust", "mglasso_clust"))
  res$ncluster <- ncluster

  res$simu_label <- ii
  res$np <- n/p
  res$cor <- rho
  res$density <- dnsty
  ####
  ####


  # res$simu_label <- ii
  # res$cor <- rho
  # res$rand_cah <- rand_cah
  # res$rand_mglasso <- rand_mglasso
  # res$rand_kmeans <- rand_kmeans

  path = paste0("~/These/res_simu/","res_simu", ii, "_rho", rho, "_n", n, "_dnst", dnsty,".RData")

  file_name <- paste0("res_simu", ii, "_rho", rho)
  assign(file_name, res)
  do.call(save, c(lapply(file_name, as.name), file=path))
  #save_here

  return(list(res = res, mglasso_obj = mglasso_obj, data = data, mb_out = mb_out, cah_glasso_obj = cah_glasso_obj))
}
