#' Plot `mglasso` function output.
#'
#' Plot the object returned by the `mglasso` function.
#'
#' @param mglasso_ Object of class `mglasso`.
#' @param levels_ Character vector. Selected levels for which estimated matrices
#'   will be plot. If NULL plot all levels.
#'
#' @export
#' @return No return value.
plot_mglasso <- function(mglasso_, levels_ = NULL) {

  stopifnot(class(mglasso_) == "mglasso")

  len <- length(mglasso_$out)
  levels <- names(mglasso_$out)

  if (!is.null(levels_)) {
    levels <- levels_
    len <- length(levels_)
  }

  pl <- lapply(levels, function(level) {
    image_sparse(mglasso_$out[[level]]$beta, "", level)
  })

  l1 <- mglasso_$lambda1

  do.call(gridExtra::grid.arrange,
          c(pl, nrow = 2, top = paste0("lambda1 = ", l1)))

}

#' Plot MGLasso Clusterpath
#'
#' @param X numeric matrix
#' @param mglasso_res object of class `mglasso`
#' @param colnames_ columns labels
#'
#' @details This function plot the clustering path of mglasso method on the 2 principal components
#' axis of X. As the centroids matrices are not of the same dimension as X, we choose to plot the
#' predicted X matrix path.
#'
#' @importFrom ggplot2 ggplot aes geom_path geom_point xlab ylab theme_bw
#' @importFrom ggrepel geom_text_repel
#'
#' @return no return value.
#' @export
plot_clusterpath <- function(X, mglasso_res, colnames_ = NULL) {
  ## Check inputs
  stopifnot(class(mglasso_res) == "mglasso")

  ## Initialisations
  p <- ncol(X)
  df.paths <- data.frame(x=c(),y=c(), group=c())
  nlevel <- length(mglasso_res$out)

  ## Principal component analysis
  svdX <- svd(X)                ## singular value decomposition
  pc <- svdX$u[,1:2,drop=FALSE] ## singular vectors

  for (j in 1:nlevel) {
    Beta <- mglasso_res$out[[j]]$beta
    Xpred <- sapply(1:p, function(i){X %*% Beta[i,]})
    pcs <- t(pc)%*%Xpred
    x <- pcs[1,]
    y <- pcs[2,]
    df <- data.frame(x=pcs[1,], y=pcs[2,], group=1:p)
    df.paths <- rbind(df.paths,df)
  }

  X_data <- as.data.frame(t(X)%*%pc) ## PCA projections (scores)
  colnames(X_data) <- c("x","y")
  ifelse(is.null(colnames_),
         X_data$Name <- colnames(X),
         X_data$Name <- colnames_)
  data_plot <- ggplot(data=df.paths,aes(x=x,y=y))
  data_plot <- data_plot + geom_path(aes(group=group),colour='grey30',alpha=0.5)
  data_plot <- data_plot + geom_text_repel(data=X_data, aes(x=x,y=y,label=Name), max.overlaps = 20)
  data_plot <- data_plot + geom_point(data=X_data,aes(x=x,y=y),size=1.5)
  data_plot <- data_plot + xlab('Principal Component 1') + ylab('Principal Component 2')
  data_plot + theme_bw()
}
