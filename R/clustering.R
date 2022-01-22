<<<<<<< HEAD
#' merge clusters from table
#'
#' @param to_merge
#' @param clusters
#' @param x
#' @param beta
#' @param level
#' @param gain_level
#' @param gains
#' @param labels
#' @param merge
merge_proc <- function(to_merge, clusters, x, beta, level, gain_level,
    gains, labels, merge) {
    for (l in seq_len(nrow(to_merge))) {
        pair_to_merge <- to_merge[l, ]

        i <- min(pair_to_merge)
        j <- max(pair_to_merge)

        if (i != j) {
            level <- level + 1

            # merge lines/cols in beta and x
            beta <- merge_beta(beta, pair_to_merge, clusters)
            x <- merge_x(x, pair_to_merge, clusters)

            # update dendrogram
            merge[level, ] <- c(labels[i], labels[j])
            labels <- merge_labels(pair_to_merge, labels, level)
            gains[level] <- ifelse(l > 1, 0, NA)

            # merge clusters
            clusters[clusters == j] <- i
            clusters[clusters > j] <- clusters[clusters > j] - 1

            # update the rest of the table with the new clusters
            to_merge[to_merge == j] <- i
            to_merge[to_merge > j] <- to_merge[to_merge > j] - 1
        }

    }

    out_mergeproc <- list(clusters = clusters, beta = beta, x = x, level = level,
        gains = gains, merge = merge, labels = labels)
    return(out_mergeproc)
}

#' distances beta
#'
#' @param beta
#' @param distance
dist_beta <- function(beta, distance = "euclidean") {
    k <- ncol(beta)

    if (distance == "wr") {
        diffs <- matrix(NA, nrow = k, ncol = k)
        for (i in 1:(k - 1)) {
            for (j in (i + 1):k) {
                diffs[i, j] <- norm(FUN_lines_wr(i, j, beta, `-`), type = "2")
            }
        }
    } else {
        if (k != 1) {
            diffs <- matrix(NA, nrow = k, ncol = k)
            for (i in 1:(k - 1)) {
                for (j in (i + 1):k) {
                  diffs[i, j] <- norm(FUN_lines(i, j, beta, `-`), type = "2")
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
                diffs <- diffs/dsum
            }
        } else {
            diffs <- matrix(0, nrow = 1, ncol = 1)
        }
    }

    return(diffs)
}

#' Merge x
#'
#' weighted mean
#'
#' @param x
#' @param pair_to_merge
#' @param clusters
merge_x <- function(x, pair_to_merge, clusters) {
    i <- min(pair_to_merge)
    j <- max(pair_to_merge)

    ni <- sum(clusters == i)
    nj <- sum(clusters == j)

    x[, i] <- (ni * x[, i] + nj * x[, j])/(ni + nj)
    x <- x[, -j]

    return(x)
}

#' Merge beta lines
#'
#' @param beta
#' @param pair_to_merge
#' @param clusters
merge_beta <- function(beta, pair_to_merge, clusters) {
    i <- min(pair_to_merge)
    j <- max(pair_to_merge)

    ni <- sum(clusters == i)
    nj <- sum(clusters == j)

    beta[i, ] <- fun_lines(i, j, beta, `+`, ni, nj)/(ni + nj)
    beta <- beta[-j, -j]

    return(beta)
}

#' Merge labels
#'
#' @param merged_pair
#' @param labels
#' @param level
merge_labels <- function(merged_pair, labels, level) {
    i <- min(merged_pair)
    j <- max(merged_pair)
    labels[i] <- level
    labels <- labels[-j]
    labels
}

#'
merge_clusters <- function(pairs_to_merge, clusters) {

    for (l in 1:nrow(pairs_to_merge)) {
        pair_to_merge <- pairs_to_merge[l, ]

        # can also take the 1st element cause it's always the min
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
=======
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
        diffs[i, j] <- norm(FUN_lines(i, j, beta, `-`), type = "2")
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
>>>>>>> initial
}
