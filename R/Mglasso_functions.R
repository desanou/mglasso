
conesta_rwrapper <- NULL

.onLoad <- function(libname, pkgname) {

  # setup environment
  reticulate::py_config()
  path = system.file("python", package = "Rmglasso")

  if (!reticulate::py_module_available("scipy")){
    reticulate::py_install("scipy")
  }

  if (!reticulate::py_module_available("scikit-learn")){
    reticulate::py_install("scikit-learn")
  }

  if (!reticulate::py_module_available("parsimony.estimators")){
    text = paste("sys.path.insert(0,'", path, "/pylearn-parsimony')", sep="", collapse=NULL)
    reticulate::py_run_string("import sys")
    reticulate::py_run_string(text)
  }

  # loading conesta solver
  the_module <- reticulate::import_from_path("conesta_solver", path = path)
  conesta_rwrapper <<- the_module$conesta_rwrapper
}


FUN_lines <- function(i, j, Beta, FUN = `-`, ni = 1, nj = 1){
  Y_coeffs <- Beta[i,]
  X_coeffs <- Beta[j,]
  X_i <- X_coeffs[i]
  X_coeffs[i] <- X_coeffs[j]
  X_coeffs[j] <- X_i

  FUN(ni*Y_coeffs, nj*X_coeffs)
}


cost <- function(Beta, X, lambda1 = 0, lambda2 = 0){
  p <- ncol(X)
  P2 <- 0
  L <- 0

  #L <- sum(sapply(1:p, function(i){norm(X[,i] - X[,-i] %*% Beta[i,-i], type = "2")^2})) ## loss term

  if(length(p) != 0){
    L <- sum(sapply(1:p, function(i){norm(X[,i] - X %*% Beta[i,], type = "2")^2})) ## loss term

    # I can vectorize it later
    for(i in 1:(p-1)){
      for(j in (i+1):p){
        P2 = P2 + norm(FUN_lines(i, j, Beta, `-`), type = "2")
      }
    } ## fuse-group lasso penalty
  }


  P1 <- sum(abs(Beta))  ## lasso penalty



  #cat("\n LS = ", L, ", l1*P1 = ", lambda1*P1, ", l2*P2 = ", lambda2*P2, "\n")
  return(L + lambda1*P1 + lambda2*P2)
}

hggm2 <- function(X,
                  lambda1=0,
                  fuse_thresh = 10^-3,
                  maxit = 1000,
                  silent = TRUE,
                  distance = "euclidean",
                  solver = "conesta",
                  lambda2_start = 1e-4,
                  lambda2_factor = 1.5){

  ## Initialisations
  p        <- ncol(X)
  gains    <- rep(0, p-1)
  merge    <- matrix(0, nrow=(p-1), ncol=2) # matrix of merging clusters at each level
  level    <- 0
  labels   <- -1:-p                         # vector of clusters labels
  clusters <- 1:p

  lambda2     <- lambda2_start
  old_lambda2 <- 0
  old_lambda1 <- lambda1
  Beta <- conesta_rwrapper(X, lambda1, old_lambda2)

  old_costf <- costf <-  cost(Beta, X, lambda1, old_lambda2)


  t               <- 2 # index for the out list.
  iter            <- 0
  out             <- list()
  out[[1]]        <- list("Beta" = Beta, "clusters" = clusters)
  names(out)[[1]] <- "level0"
  prev            <- length(unique(clusters))
  ## End Initialisations


  ## Loop until all the variables merged
  while (length(unique(clusters)) > 1) {
    oldp <- ncol(X)
    iter = iter + 1


    Beta <- conesta_rwrapper(X, lambda1, lambda2); print(lambda1)

    ## Update distance matrix
    diffs <- dist_beta(Beta, distance = distance)

    ## Clustering starts here
    pairs_to_merge <- which(diffs <= fuse_thresh, arr.ind = TRUE)

    if(nrow(pairs_to_merge) != 0){
      gain_level     <- costf - old_costf
      out_mergeproc <- merge_proc(pairs_to_merge, clusters, X, Beta, level, gain_level, gains, labels, merge)

      X        <- out_mergeproc$X
      Beta     <- out_mergeproc$Beta
      clusters <- out_mergeproc$clusters

      level    <- out_mergeproc$level
      gains    <- out_mergeproc$gains
      merge    <- out_mergeproc$merge
      labels   <- out_mergeproc$labels
    }
    ## Clustering ends here

    costf <- cost(Beta, X, lambda1, lambda2)
    cat("nclusters =", length(unique(clusters)), "lambda2", lambda2, "cost =", costf, "\n")

    gains[is.na(gains)] <- old_costf - costf
    old_costf = costf

    if(length(unique(clusters)) != prev){
      out[[t]] <- list("Beta" = Beta, "clusters" = clusters)
      names(out)[[t]] <- paste0("level", (p-length(unique(clusters))))
      prev <- length(unique(clusters))
      t <- t + 1

    }

    old_lambda2 = lambda2

    lambda2 = lambda2*lambda2_factor
  }

  height      <- cumsum(gains)
  tree        <- list(merge = merge,
                      height = height,
                      order = rev(clusters),
                      labels = paste("",1:p),
                      gains=gains)
  class(tree) <- "hclust"
  tree <- vegan:::reorder.hclust(tree, 1:p)


  result <- list("out" = out, "tree" = tree)

  return(result)
}

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

  out_mergeproc <- list("clusters" = clusters, "Beta" = Beta, "X" = X, "level" = level, "gains" = gains, "merge" = merge , "labels" = labels)
  return(out_mergeproc)
}

#' distances Beta
dist_beta <- function(Beta, distance = "euclidean"){
  K <- ncol(Beta)
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

  return(diffs)
}

#' Merge X
#'
#' weighted mean
mergeX <- function(X, pair_to_merge, clusters){
  i        <- min(pair_to_merge)
  j        <- max(pair_to_merge)

  ni       <- sum(clusters == i)
  nj       <- sum(clusters == j)

  #X[,i] <- (X[,i] + X[,j])/2

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

