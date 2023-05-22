#' merge clusters from table
merge_proc <- function(pairs_to_merge,
                       clusters,
                       X,
                       Beta,
                       level,
                       gain_level,
                       gains,
                       labels,
                       merge){
  for (l in 1:nrow(pairs_to_merge)) {
    pair_to_merge <- pairs_to_merge[l,]

    # can also take the 1st element cause it's always the min for a upper-triangular matrix
    i         <- min(pair_to_merge)
    j         <- max(pair_to_merge)

    if(i != j){
      level <- level + 1

      # merge lines/cols in Beta and X
      Beta <- merge_beta(Beta, pair_to_merge, clusters)
      X <- mergeX(X, pair_to_merge, clusters)

      # update dendrogram
      merge[level,] <- c(labels[i], labels[j])
      labels        <- merge_labels(pair_to_merge, labels, level)
      gains[level] <- ifelse(l > 1, 0, NA)

      # merge clusters
      clusters[clusters == j] <- i
      clusters[clusters > j] <- clusters[clusters > j] - 1

      # update the rest of the table with the new clusters
      pairs_to_merge[pairs_to_merge == j] <- i
      pairs_to_merge[pairs_to_merge > j] <- pairs_to_merge[pairs_to_merge > j] - 1
    }
  }

  out_mergeproc <- list("clusters" = clusters, "Beta" = Beta, "X" = X, "level" = level,
                        "gains" = gains, "merge" = merge , "labels" = labels)
  return(out_mergeproc)
}

#' distances Beta
dist_beta <- function(Beta, distance = "euclidean"){
  K <- ncol(Beta)

  if(distance == "wr"){
    diffs <- matrix(NA, nrow = K, ncol =  K)
    for(i in 1:(K-1)){
      for(j in (i+1):K){
        diffs[i,j] <- norm(FUN_lines_wr(i, j, Beta, `-`), type = "2")
      }
    }
  }else{
    if(K != 1){
      diffs <- matrix(NA, nrow = K, ncol =  K)
      for(i in 1:(K-1)){
        for(j in (i+1):K){
          diffs[i,j] <- norm(FUN_lines(i, j, Beta, `-`), type = "2")
        }
      }

      if(distance == "relative"){
        Dsum <- matrix(NA, nrow = K, ncol = K)
        for(i in 1:(K-1)){
          for(j in (i+1):K){
            Dsum[i,j] <- norm(Beta[i,], type = "2") + norm(Beta[j,], type = "2")
          }
        }
        diffs <- diffs/Dsum
      }
    }else{
      diffs <- matrix(0, nrow = 1, ncol = 1)
    }
  }

  return(diffs)
}

#' Merge X
#'
#' weighted mean
mergeX <- function(X, pair_to_merge, clusters){ #####
  i        <- min(pair_to_merge)
  j        <- max(pair_to_merge)

  ni       <- sum(clusters == i)
  nj       <- sum(clusters == j)

  X[,i] <- (ni*X[,i] + nj*X[,j])/(ni + nj)
  X <- X[,-j]

  return(X)
}

#' Merge Beta
#' Different types of merging and their effect
merge_beta <- function(Beta, pair_to_merge, clusters){
  i        <- min(pair_to_merge)
  j        <- max(pair_to_merge)

  ni       <- sum(clusters == i)
  nj       <- sum(clusters == j)

  Beta[i,] <- FUN_lines(i, j, Beta, `+`, ni, nj) / (ni + nj)
  Beta     <- Beta[-j, -j]

  return(Beta)
}

#' Merge labels
merge_labels <- function(merged_pair, labels, level){
  i         <- min(merged_pair)
  j         <- max(merged_pair)
  labels[i] <- level
  labels    <- labels[-j]
  labels
}

#'
merge_clusters <- function(pairs_to_merge,
                           clusters){

  for (l in 1:nrow(pairs_to_merge)) {
    pair_to_merge <- pairs_to_merge[l,]

    # can also take the 1st element cause it's always the min for a upper-triangular matrix
    i         <- min(pair_to_merge)
    j         <- max(pair_to_merge)

    if(i != j){
      # merge clusters
      clusters[clusters == j] <- i
      clusters[clusters > j] <- clusters[clusters > j] - 1

      # update the rest of the table with the new clusters
      pairs_to_merge[pairs_to_merge == j] <- i
      pairs_to_merge[pairs_to_merge > j] <- pairs_to_merge[pairs_to_merge > j] - 1
    }
  }

  return(clusters)
}


#' doesn't work when dealing with matrix where diagonal of zero should be adjusted
expand_beta_deprecated <- function(beta_level, clusters){
  beta_level = as.matrix(beta_level)
  beta_exp <- t(sapply(1:nrow(beta_level), function(i){rep(beta_level[i,], table(clusters))}))

  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }

  if(nrow(beta_exp) == 1){
    beta_exp = rep.row(beta_exp[1,], table(clusters)[1])
    return(beta_exp)
  }else{
    beta_exp <- lapply(1:nrow(beta_exp), function(i){rep.row(beta_exp[i,], table(clusters)[i])})
  }

  if(!is.list(beta_exp)){
    beta_exp = list(beta_exp)
  }
  do.call(rbind, beta_exp)
}


#'
#' TO DO: Fill upper triangular matrix then sum up with the transpose to have full matrix
expand_beta <- function(beta_level, clusters){
  p <- length(clusters)
  beta_level = as.matrix(beta_level)
  beta_exp <- matrix(NA, p, p)

  for (i in 1:p) {
    for (j in 1:p) {
      ilevel <- clusters[i]
      jlevel <- clusters[j]
      beta_exp[i, j] <- beta_level[ilevel, jlevel]
    }
  }

  return(beta_exp)
}

#' extracts meta-variables indices
#' @export
extract_meta <- function(full_graph=NULL, clusters) {
  p <- length(clusters)

  meta_groups <- split(1:p, clusters)

  index_meta_vars <- sapply(meta_groups, function(a){
    return(a[[1]])
  })

  #meta_graph <- full_graph[index_meta_vars, index_meta_vars]

  #return(meta_graph)
  return(index_meta_vars)
}
