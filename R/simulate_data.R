
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

  if (!requireNamespace("simone", quietly = TRUE)) {
    stop("Package \"simone\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  network <- simone::rNetwork(n_vars, connectivity_mat, prop_clusters)

  precision_mat <- network$A

  eff_clusters <- table(network$clusters)

  b = -rho/(1+rho*(eff_clusters-2) -rho^2*(eff_clusters-1))
  d = (1+rho*(eff_clusters-2))/(1+rho*(eff_clusters-2) -rho^2*(eff_clusters-1))


  for (i in 1:length(eff_clusters)) {
    bloc <- matrix(b[i], eff_clusters[i], eff_clusters[i])
    diag(bloc) = d[i]
    precision_mat[which(row.names(precision_mat) == i), which(row.names(precision_mat) == i)] = bloc
  }
  #diag(K) <- 1 - min(svd(K)$d)

  return(precision_mat)
}
