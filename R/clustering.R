#' Compute distance matrix between regression vectors
#'
#' @param beta matrix of regression vectors
#' @param distance euclidean or relative distance
dist_beta <- function(beta, distance = "euclidean") {
  k <- ncol(beta)

  if (k != 1) {
    diffs <- matrix(NA, nrow = k, ncol = k)
    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        diffs[i, j] <- norm(fun_lines(i, j, beta, `-`), type = "2")
      }
    }

    if (distance == "relative") {
      dsum <- matrix(NA, nrow = k, ncol = k)
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          dsum[i, j] <- norm(beta[i, ], type = "2") + norm(beta[j,
          ], type = "2")
        }
      }
      diffs <- diffs / dsum
    }
  } else {
    diffs <- matrix(0, nrow = 1, ncol = 1)
  }

  return(diffs)
}

#' compute clusters partition from pairs of variables to merge
#' @param pairs_to_merge table of the indices of variables to be merge
#' @param clusters numeric vector. By default 1:p where p is the number of variables
merge_clusters <- function(pairs_to_merge, clusters) {

  for (l in 1:nrow(pairs_to_merge)) {
    pair_to_merge <- pairs_to_merge[l, ]

    # for a upper-triangular matrix
    i <- min(pair_to_merge)
    j <- max(pair_to_merge)

    if (i != j) {
      # merge clusters
      clusters[clusters == j] <- i
      clusters[clusters > j] <- clusters[clusters > j] - 1

      # update the rest of the table with the new clusters
      pairs_to_merge[pairs_to_merge == j] <- i
      pairs_to_merge[pairs_to_merge > j] <- pairs_to_merge[pairs_to_merge >
                                                             j] - 1
    }
  }

  return(clusters)
}
