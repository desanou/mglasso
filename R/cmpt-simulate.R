#' simulate data with given graph structure
#'
#' @param p
#' @param np_ratio
#' @param structure
#' @param alpha
#' @param prob_mat
#' @param rho
#' @param g
#' @param inter_cluster_edge_prob
#' @param p_erdos
#' @param verbose
#'
#' @export
#'
#' @return A list:
#' graph : precision
sim_data <- function(p = 20,
                     np_ratio = 2,
                     structure = c("block_diagonal", "hub", "scale_free", "erdos"),
                     alpha,
                     prob_mat,
                     rho,
                     g,
                     inter_cluster_edge_prob = 0.01,
                     p_erdos = 0.1,
                     verbose = FALSE) {
  structure <- match.arg(structure)
  n = round(np_ratio * p)

  switch (
    structure,
    erdos = {
      network <- simone::rNetwork(p, p_erdos, 1)
      correlation <- solve(network$Theta)
      X           <- mvtnorm::rmvnorm(n, sigma = correlation)
      graph <- network$Theta

      if (verbose)
        message("Data, precision matrix and graph generated from Erdos structure.")
    },

    hub = {
      L = huge::huge.generator(
        graph = "hub",
        g = 5,
        d = p,
        n = n,
        verbose = FALSE
      )

      X = L$data
      graph = L$omega
      correlation = L$sigma

      if (verbose)
        message("Data, precision matrix and graph generated from Hub structure.")
    },

    scale_free = {
      L = huge::huge.generator(
        graph = "scale-free",
        d = p,
        n = n,
        verbose = FALSE
      ) ## d edges graph

      X = L$data
      graph = L$omega
      correlation = L$sigma

      if (verbose)
        message("Data, precision matrix and graph generated from Scale free structure.")
    },

    block_diagonal = {
      if (inter_cluster_edge_prob != 0) {
        # inter-clusters edges added in the flipped way
        flag <- TRUE

        while (flag) {
          K <- bloc_diag(p, prob_mat, alpha, rho)

          target_indices <-
            which(K[upper.tri(K)] == 0) # Random selection of edges to be set to 1

          select_len <-
            round(length(target_indices) * inter_cluster_edge_prob)
          selected_indices <-
            sample(target_indices, select_len)

          precision_level <- unique(K[upper.tri(K)])
          precision_level <-
            max(precision_level[precision_level != 0])

          K[upper.tri(K)][selected_indices] <- precision_level
          K <-
            as.matrix(Matrix::forceSymmetric(K, uplo = "U"))

          flag <-
            any(eigen(K)$values <= 0) # Control of positive definiteness
        }

        correlation <- solve(K)
        graph = K
        X           <-
          mvtnorm::rmvnorm(n, sigma = correlation)

        if (verbose)
          message(
            "Data, precision matrix and graph generated from block-diagonal
                                  structure with inter-clusters edges."
          )
      }
      else {
        # Only intra-cluster edges while approximately controlling correlation level
        K <- bloc_diag(p, prob_mat, alpha, rho)
        correlation <- solve(K)
        graph = K
        X           <-
          mvtnorm::rmvnorm(n, sigma = correlation)

        if (verbose)
          message(
            "Data, precision matrix and graph generated from block-diagonal
                                  structure with only intra-clusters edges."
          )
      }
    }
  )
  return(list(
    X = X,
    graph = graph,
    correlation = correlation
  ))
}

#'return precision matrix
bloc_diag <- function(n_vars,
                      connectivity_mat,
                      prop_clusters,
                      rho) {
  true_nclusters <- 0

  while (true_nclusters != length(prop_clusters)) {
    ## To make sure we have the required number of clusters
    network <-
      simone::rNetwork(n_vars, connectivity_mat, prop_clusters)
    true_nclusters <- length(unique(network$clusters))
  }

  precision_mat <-
    network$A[order(network$clusters), order(network$clusters)]

  eff_clusters <- table(network$clusters)

  b = -rho / (1 + rho * (eff_clusters - 2) - rho ^ 2 * (eff_clusters - 1))
  d = (1 + rho * (eff_clusters - 2)) / (1 + rho * (eff_clusters - 2) -
                                          rho ^ 2 * (eff_clusters - 1))


  for (i in 1:length(eff_clusters)) {
    temp <-
      precision_mat[which(row.names(precision_mat) == i), which(row.names(precision_mat) == i)]
    temp <- as.matrix(temp)
    temp[temp != 0] = b[i]
    diag(temp) = d[i]
    precision_mat[which(row.names(precision_mat) == i), which(row.names(precision_mat) == i)] = temp
  }

  flag <- min(svd(precision_mat)$d)
  if (flag < 0) {
    diag(precision_mat) <- 1 - flag
  }

  return(precision_mat)
}

#'@export
configs_simu <- function(n_simu, seq_rho, seq_dnsty, seq_n, type){
  L = length(seq_n)*length(seq_rho)*length(seq_dnsty)*length(type)

  v1 <- rep(1:n_simu, each = L)
  v2 <- rep(seq_n, each = length(seq_rho)*length(seq_dnsty)*length(type), times = n_simu)
  v3 <- rep(seq_rho, each = length(seq_dnsty)*length(type), times = n_simu*length(seq_n))
  v4 <- rep(seq_dnsty, each = length(type), times = n_simu*length(seq_rho)*length(seq_n))
  v5 <- rep(type, length(seq_n)*length(seq_rho)*length(seq_dnsty))

  xx <- cbind(v1, v2, v3, v4, v5)
  list_ii_rho <- split(xx, rep(1:nrow(xx), ncol(xx)))

  return(list_ii_rho)
}

#' One simulation configuration
one_config <- function(n, p, pi, alpha, rho){
  return(list(n = n, p = p, pi  = pi, alpha = alpha, rho = rho))
}

#'
tuning_l1 <- function(l1_, dt_, lambda2_start_, lambda2_factor_, version = "no-merge"){
  if(version == "no-merge")
    mglasso_ <- hggm_no_merge(scale(dt_$X), lambda1 = l1_, fuse_thresh = 1e-3,
                              lambda2_start  = lambda2_start_,
                              lambda2_factor = lambda2_factor_, precision = 1e-2)
  else
    mglasso_ <- hggm_merge(scale(dt_$X), lambda1 = l1_, fuse_thresh = 1e-3,
                           lambda2_start  = lambda2_start_,
                           lambda2_factor = lambda2_factor_, precision = 1e-2)

  return(mglasso_)
}

#'

tuning_l1_all <- function(l1_){
  mb <- huge(x       = scale(data),
             method  = "mb",
             verbose = FALSE,
             sym     = "and",
             lambda = rev(lambdas)
  )

  cah_glasso(data, num_clusters, lambda1)


  mglasso_ <- hggm_no_merge(scale(dt_$X), lambda1 = l1_, fuse_thresh = 1e-3,
                            lambda2_start  = lambda2_start_,
                            lambda2_factor = lambda2_factor_,
                            precision = 1e-2)

  MGL <- lapply(seq_l1, tuning_l1, dt_ = dt)
}

#'@export
one_simu_ROC <- function(list_ii_rho, verbose = FALSE, model, p, pi, alpha, path_roc){
  ii    = list_ii_rho[1]
  n     = list_ii_rho[2]
  rho   = list_ii_rho[3]
  dnsty = list_ii_rho[4]

  KK        <- length(alpha)
  config    <- one_config(n, p, pi, alpha, rho)

  data      <- sim_data(p, n/p, model, prob_mat = pi, alpha = alpha, rho = rho)
  beta_true <- precision_to_regression(data$graph)

  pen_params <- seq_l1l2(X = data$X, nlam1 = 10, nlam2 = 4, require_non_list = TRUE, l2_max = 10)
  #pen_params <- seq_l1l2(X = data$X, nlam1 = 2, nlam2 = 2, require_non_list = TRUE, l2_max = 10) # for testing purpose
  lambda1s   <- pen_params$lambda1s/n

  # ESTIMATION
  ## GLASSO PATH
  mb_out      <- huge(x = scale(data$X), method = "mb", verbose = FALSE, sym = "and", lambda = lambda1s)$path
  if(verbose) cat("mb_out done \n")

  ## MGLASSO PATH
  pen_params  <- pen_params$pen_params
  mglasso_out <- lapply(pen_params, FUN = mglasso_pair_param, X_ = data$X, type = "initial", fuse_thresh = 1e-6)
  if(verbose) cat("mglasso_out done \n")

  res <- list(mb_out = mb_out, mglasso_out = mglasso_out, beta_true = beta_true,
              pen_params = pen_params, data = data)

  file_name <- paste0("ROC_simu_", ii, "_rho", rho, "_n", n, "_p", p, "_dnsty", dnsty, model, ".RData")
  path <- paste0(path_roc, file_name)

  assign(file_name, res)
  do.call(save, c(lapply(file_name, as.name), file=path))

  return(res)
}

ROC_mb_readjustment <- function(roc_object, n = 80){
  pen_params <- convert_pen_params_to_dataframe(roc_object$pen_params)
  l1 <- unique(pen_params$l1)/n
  mb_out <- huge(x = scale(roc_object$data$X), method = "mb", verbose = FALSE, sym = "and", lambda = l1)$path

  return(mb_out)
}

#'@export
one_simu_extended <- function(list_ii_rho, verbose = FALSE, model = "block_diagonal", p, pi, alpha, path_extended){
  ii    = list_ii_rho[1]
  n     = list_ii_rho[2]
  rho   = list_ii_rho[3]
  dnsty = list_ii_rho[4]

  KK        <- length(alpha)
  pi        <- diag(dnsty, KK)
  config    <- one_config(n, p, pi, alpha, rho)

  data      <- sim_data(p, n/p, model, prob_mat = pi, alpha = alpha, rho = rho)
  beta_true <- precision_to_regression(data$graph)
  true_clusters <- as.numeric(colnames(data$correlation))

  mb_out      <- neighbor_select(data$X, config, lambda_min_ratio = 1e-2, nlambda = 20, nresamples = 50, model = model)
  lambda1     <- mb_out$lambda_opt
  #lambda1 = 0
  if(verbose) cat("selection of lambda1 done \n")

  cah_data <- hclust(dist(t(scale(data$X))), method = "ward.D")

  pen_params <- seq_l2_l1_fixed(dt = data$X, l1 = lambda1, nl2 = 20, l2_max = TRUE) #set nl2 to 2 for testing purpose
  mgl <- lapply(pen_params, FUN = mglasso_pair_param, X_ = data$X, type = "initial")
  if(verbose) cat("mglasso done \n")

  ## CAH
  cah_out <- lapply(2:(p-1), function(ncl){cutree(cah_data, k = ncl)})
  if(verbose) cat("cah_out done \n")
  ## K-MEANS
  kmeans_out <- lapply(2:(p-1), function(ncl) {kmeans(scale(t(data$X)), centers = ncl)$cluster})
  if(verbose) cat("kmeans_out done \n")
  res <- list(simu_label = ii, config = config, data = data,
              beta_true = beta_true, true_clusters = true_clusters, cah_data = cah_data,
              pen_params = pen_params, mgl = mgl, cah_out = cah_out, kmeans_out = kmeans_out)

  # save
  file_name <- paste0("simu", ii, "_rho", rho, "_n", n, "_dnsty", dnsty, ".RData")
  path <- paste0(path_extended, file_name)
  assign(file_name, res)
  do.call(save, c(lapply(file_name, as.name), file = path))

  return(res)
}

#'
#'@export
get_clusters_mgl <- function(beta_mgl, fuse_thresh = 1e-3, p) {
  p <- ncol(beta_mgl)

  clusters <- 1:p
  diffs <- dist_beta(beta_mgl, distance = "euclidean") ## Update distance matrix
  pairs_to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)

  if(nrow(pairs_to_merge) != 0)
    clusters <- merge_clusters(pairs_to_merge, clusters)  # merge clusters

  return(clusters)
}

#'@export
get_perf_from_raw <- function(criterion, out, thresh_fuse = 1e-7, cah_kmeans_available = FALSE) {
  # mgl outs
  mgl       <- out$mgl

  # MGL clusters
  nclusters_mgl   <- sapply(mgl, function(e){length(unique(get_clusters_mgl(e$selected_Theta, fuse_thresh = thresh_fuse, p = out$config$p)))})



  #### RAND
  if(criterion == "rand") {
    method <- c("cah", "kmeans", "mglasso")
    true_clusters <- out$true_clusters

    ncluster_cah <- ncluster_kmeans <- 2:(out$config$p-1)

    if(!cah_kmeans_available) {
      cah   <- lapply(2:(out$config$p -1), function(ncl){cutree(out$cah_data, k = ncl)})
      kmean <- lapply(2:(out$config$p -1), function(ncl) {kmeans(scale(t(out$data$X)), centers = ncl)$cluster})
    } else {
      cah   <- out$cah_out
      kmean <- out$kmeans_out
    }

    nclusters <- c(ncluster_cah, ncluster_kmeans, nclusters_mgl)
    vec_rep_num <- c(length(ncluster_cah), length(ncluster_kmeans), length(nclusters_mgl))

    methods <- rep(method, vec_rep_num)

    perfs_cah   <- sapply(cah, function(e){adjustedRandIndex(e, true_clusters)})
    perfs_kmean <- sapply(kmean, function(e){adjustedRandIndex(e, true_clusters)})

    perfs_mgl   <- sapply(mgl,
                          function(e){
                            mglasso_cut <- get_clusters_mgl(e$selected_Theta, fuse_thresh = thresh_fuse, p = out$config$p)
                            return(adjustedRandIndex(mglasso_cut, true_clusters))
                          })

    perfs       <- c(perfs_cah, perfs_kmean, perfs_mgl)

    res             <- data.frame(perfs)
    res$simu_label  <- out$simu_label
    res$crit        <- "rand"
    res$np          <- out$config$n/out$config$p
    res$cor         <- out$config$rho
    res$dnsty       <- max(out$config$pi)

    res$method    <- methods
    res$ncluster  <- nclusters

    return(res)
  }

}

load_specific <- function(chars, path_){
  files <- list.files(path=path_)
  tmp <- sapply(files, function(a) {grepl(chars, a, fixed = TRUE)})
  tmp <- files[tmp]
  setwd(path_)
  results <- sapply(tmp, function(x) mget(load(x)), simplify = TRUE)

  obj_len <- sapply(results, length)
  results <- results[which(obj_len>1)]

  return(results)
}

reorder_mglasso_roc_calculations <- function(out){

  pen_params <- convert_pen_params_to_dataframe(out$pen_params)

  l2 <- unique(pen_params$l2)
  list_of_list <- list()

  for (i in 1:length(l2)) {
    indices <- which(pen_params$l2 == l2[i])

    sublist <- list()
    for (j in 1:length(indices)){
      k = indices[j]
      sublist[[j]] <- out$mglasso_out[[k]]$selected_Theta ## will update out result in ROC simus function to discard $selected Theta and just select inices in list
    }

    list_of_list[[i]] <- sublist #out$mglasso_out[indices]
  }

  res <- list(path_per_tv = list_of_list, pen_params = pen_params, l2 = l2)

  return(res)
}


convert_pen_params_to_dataframe <- function(pen_params) {
  pen_params <- matrix(unlist(pen_params), ncol = 2, byrow = TRUE)
  colnames(pen_params) <- c("l1", "l2")
  pen_params <- data.frame(pen_params)

  return(pen_params)
}

#'
#'@export
get_mean_ROC_stat <- function(out){

  nlam1 = 10
  mean_pf_mb <- matrix(0, ncol = 4, nrow = nlam1+2)
  colnames(mean_pf_mb) <- c("tpr", "fpr", "prec", "shd")
  mean_pf_mb <- as.data.frame(mean_pf_mb)

  for (i in 1:length(out)) {
    ### if dealing with erdos model, the estimated precision matrix returned by huge doesn't map the adjacency matrix
    ### thresholding needed (set to 1e-10)
    ### same goes for all the simulated model from huge, thesholding needed
    ### Adding hub

    beta_true <- out[[i]]$beta_true
    beta_true <- abs(beta_true) >= 1e-10
    pf_vec <- perf_vec(out[[i]]$mb_out, beta_true)

    mean_pf_mb$tpr <- mean_pf_mb$tpr + pf_vec$tpr/length(out)
    mean_pf_mb$fpr <- mean_pf_mb$fpr + pf_vec$fpr/length(out)
    mean_pf_mb$prec <- mean_pf_mb$prec + pf_vec$prec/length(out)
    mean_pf_mb$shd <- mean_pf_mb$shd + pf_vec$shd/length(out)
  }

  ### MGLasso Step
  l2 <- reorder_mglasso_roc_calculations(out[[1]])$l2

  mglasso_outs <- lapply(out, reorder_mglasso_roc_calculations)

  mean_one_tv <- function(tv_index){
    perfs <- list()

    for (i in 1:length(mglasso_outs)) {
      beta_true <- out[[i]]$beta_true
      beta_true <- abs(beta_true) >= 1e-10
      perfs[[i]] <- perf_vec(mglasso_outs[[i]]$path_per_tv[[tv_index]], beta_true)
    }

    mean_mat <- Reduce("+", perfs) / length(perfs)
    mean_mat
  }

  mean_pf_mglasso <- lapply(1:length(l2), mean_one_tv)

  res <- list(mean_pf_mglasso = mean_pf_mglasso, mean_pf_mb = mean_pf_mb, l2 = l2)
  return(res)
}


#' compute range of number of clusters from ROC outputs
#' take in parameter an object from reorder_mglasso_roc_calculations
get_range_nclusters <- function(out, thresh_fuse = 1e-6, p = 40){
  nclusters_per_tv <- list()
  list_of_nclusters_per_tv <- list()

  #compute range for one simu roc
  ### compute range for one tv parameter
  get_range_nclusters_tv <- function(one_roc) {
    for (j in 1:length(one_roc$path_per_tv)) {
      nclusters_per_tv[[j]] <- lapply(one_roc$path_per_tv[[j]], function(k){
        length(unique(get_clusters_mgl(k, thresh_fuse, p = p)))
      })

      nclusters_per_tv[[j]] <- unique(unlist(nclusters_per_tv[[j]]))
    }

    nclusters_per_tv
  }

  mglasso_outs <- lapply(out, reorder_mglasso_roc_calculations)


  list_of_nclusters_per_tv <- lapply(mglasso_outs, get_range_nclusters_tv)

  res <- NULL
  for(l in 1:5) { # need to be updated by the length of total variation param vector
    res[[l]] <- unique(unlist(map(list_of_nclusters_per_tv, l)))
  }

  res
}

# adj_mat <- function(mat, sym_rule = "and") {
#   mat <- symmetrize(as.matrix(mat), rule = sym_rule)
#   mat[abs(mat) < 1e-10] <- 0
#   mat[mat != 0] <- 1
#   mat <- as(mat, "sparseMatrix")
#   return(mat)
# }
