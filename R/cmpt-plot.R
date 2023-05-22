# jouer sur le color rampalette
image_sparse <- function(matrix, main_ = "", sub_ = "", legend_ = TRUE, col_names = FALSE){
  main_ <- paste0(c(sub_, main_), collapse= " ")

  nn <- 100
  # n0 <- min(nn, max(0, round((0 - (-2))/(2-(-2)) * nn)))
  #
  # breaks <- seq(min, max, by = 0.01)
  #cols <- colorRampPalette(c("blue3", "yellow", "gray80", "gray75", "lightgreen", "red3"))(length(breaks) - 1)
  #cols <- colorRampPalette(c("blue4", "blue3","cornflowerblue", "gray80", "white", "gray75", "brown1", "red3","red4"))(length(breaks) - 1)

  #cols <- colorRampPalette(c(brewer.pal(3, "Blues"),
  # rev(brewer.pal(3, "Greys")), brewer.pal(3, "Greys"), brewer.pal(3, "PuRd")))(length(breaks) - 1)
  # cols <- heat.colors(length(breaks) - 1)
  # cols <- topo.colors(length(breaks) - 1)


  plt <- image(as(matrix, "sparseMatrix"), main = main_ ,
               sub = "", xlab = "", ylab = "",
               useAbs = FALSE
               #,
               #at = breaks
               #,
               #col.regions = cols
  )

  nn <- rownames(matrix)
  nn <- c(nn[length(nn)], nn)
  nn <- nn[1:(length(nn)-1)]
  labs <- c(nn, nn)
  if(col_names) {
    update(plt, scales = list(labels = labs))}

  plt

  #levelplot(matrix, at = breaks, col.regions = cols)
}

#' fonction qui affiche les matrices d'adjacence à chaque niveau de la hiérarchie
#' à automatiser
#' utiliser niveau de legende commune
plot_mglasso <- function(mglasso_, beta_true_, true_clusters_, levels_ = NULL, version = "no-merge", l1_temp){

  len <- length(mglasso_$out)
  levels <- names(mglasso_$out)

  if (version == "merge") {
    levels <- levels[!(levels == "level1")]
    len <- length(levels)
  }


  if(!is.null(levels_)){
    levels <- levels_
    len <- length(levels_)
  }

  # pl <- lapply(levels, function(level) {
  #   image_sparse(mglasso_$out[[level]]$Beta,
  #                as.character(mglasso_$out[[level]]$clusters),
  #                level)
  # })

  if (version == "merge") {
    pl <- lapply(levels, function(level) {
      image_sparse( expand_beta(mglasso_$out[[level]]$Beta, mglasso_$out[[level]]$clusters),
                    "",
                    level)
    })
  } else{
    pl <- lapply(levels, function(level) {
      image_sparse(mglasso_$out[[level]]$Beta,
                   "",
                   level)
    })
  }

  #pt <- image_sparse(beta_true_, true_clusters_, "true")
  pt <- image_sparse(beta_true_, "", "true")

  pl[[len+1]] <- pt
  l1 <- mglasso_$l1

  do.call(gridExtra::grid.arrange, c(pl, nrow=2, top = paste0("lambda1 = ", l1)))

}

#' pareil pour les clusters ACP dimensions ?
#'
#'
#'

# Graphs comparison -------------------------------------------------------

# donne la reparition du nombre de clusters en fonction du ratio n/p
repart <- function(cor_){
  table(subset(dt_rand, cor == cor_ & crit == "rand")$ncluster,
        subset(dt_rand, cor == cor_ & crit == "rand")$np)
}

select_cutlevels <- function(cor_, np_){
  temp <- repart(cor_ = cor_)
  v <- order(temp[, as.character(np_)], decreasing = TRUE) + 1
  v <- sort(v[1:10])
  v
}

#'
#'@export
plot_res <- function(dt, crit_, ncluster_, cor_, np_, perfs_threshold_ = 50, method_ = NULL, main = ""){
  if (is.null(method_))
    method_ <- unique(dt$method)

  nclusters.labs <- c("5 clusters", "10 clusters", "15 clusters", "20 clusters")
  names(nclusters.labs) <- c("5", "10", "15", "20")

  bp_error <- ggplot(
    subset(x      = dt,
           subset = (crit == crit_ &
                       ncluster %in% ncluster_ &
                       cor == cor_ &
                       np %in% np_ &
                       perfs < perfs_threshold_ &
                       method %in% method_
           )),

    aes(x     = factor(np),
        y     = perfs,
        fill = method
    )) +
    geom_boxplot() +
    facet_grid(. ~ ncluster, labeller = labeller(ncluster = nclusters.labs)) +
    ggtitle(main) +
    xlab("Ratio n/p") +
    scale_fill_manual(name = "Method", labels = c("HAC", "K-means", "MGLasso"),
                      values = rev(ghibli_palette("MarnieMedium1")))

  if(crit_ == "rand"){
    bp_error <- bp_error +
      ylab("Adjusted Rand Index")
  }

  bp_error
}

#' Title
#'
#' @param omega_hat_list
#' @param omega
#' @param type
#' @param main
#'
#' @return
#' @export
#'
#' @examples
ggplot_roc <- function(omega_hat_list, omega,
                       type = c("classical", "precision_recall"),
                       main = NULL) {

  type <- match.arg(type)
  pf_vec <- perf_vec(omega_hat_list, omega)

  switch (type,
          classical = {
            x   <- pf_vec$fpr
            y <- pf_vec$tpr
            xlabel <- "False Positive Rate"
            ylabel <- "True Positive Rate"
            slope = 1
            intercept = 0
            if(is.null(main)) main <- "ROC curve"
          },

          precision_recall = {
            x   <- pf_vec$tpr
            y <- pf_vec$prec
            xlabel <- "True Positive Rate"
            ylabel <- "Precision"
            slope = -1
            intercept = 1
            if(is.null(main)) main <- "Precision-Recall curve"
          }
  )

  pltdt <- data.frame(x = x, y = y)

  ggplot(pltdt, aes(x = x, y = y)) +
    geom_abline(intercept = intercept, slope = slope, linetype = "dashed", color = "grey") +
    geom_line() +
    #geom_line(aes(color = method)) # Use for the plot full roc +
    scale_x_continuous(xlabel, limits = c(0,1)) +
    scale_y_continuous(ylabel, limits = c(0,1)) +
    #geom_polygon(aes(x = X,y = Y), data = data.frame(X = c(0.75, 1, 1, 0.75), Y = c(0, 0, 0.2, 0.2)), fill = "white") +
    theme(legend.position = "none", plot.title = element_text(vjust=2, hjust = 0.5)) +
    labs(title = main) +
    theme_bw()
}

ggplot_full_roc <- function(mean_mats_roc){
  pltdt <- reformat_roc_res_for_ggplot(mean_mats_roc)

  ggplot(pltdt, aes(x     = fpr,
                    y     = tpr,
                    color = method )) +
    geom_line() +
    facet_grid(. ~ tv) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
    # scale_x_continuous("False Positive Rate", limits = c(0,1)) +
    # scale_y_continuous("True Positive Rate", limits = c(0,1)) +
    scale_colour_grey(start = 0.2, end = 0.6) +
    theme_bw()
}

plot_full_roc <- function(mean_mats_roc, l2) {
  nl2 <- length(mean_mats_roc$mean_pf_mglasso)

  par(mfrow = c(1, nl2))
  par(oma=c(2,2,2,2))

  tpr_mb <- mean_mats_roc$mean_pf_mb$tpr
  fpr_mb <- mean_mats_roc$mean_pf_mb$fpr

  for (i in 1:nl2) {
    tpr_mgl <- mean_mats_roc$mean_pf_mglasso[[i]]$tpr
    fpr_mgl <- mean_mats_roc$mean_pf_mglasso[[i]]$fpr

    plot(fpr_mgl, tpr_mgl, type="b", col = "green", xlab = "False Postive Rate",
         ylab = "True Postive Rate", main = paste0("tv = ", l2[i]), lty = 1, pch = 20)
    points(fpr_mb, tpr_mb, type="b", col = "red", lty = 2, pch = 20)
    points(0:1, 0:1, type="l", col = "gray")
  }

  legend("bottomright", legend=c("MGLasso", "Glasso"),
         col=c("green", "red"), lty=1:2, cex=0.8)

  title("ROC curves", outer =TRUE, cex.main = 1)
}

#'
#'@export
reformat_roc_res_for_ggplot <- function(dta) {
  l2 <- dta$l2
  nl2 <- length(l2)
  mean_pf_mb <- list()

  for (i in 1:nl2) {
    mean_pf_mb[[i]] <- dta$mean_pf_mb
    mean_pf_mb[[i]]$tv <- round(l2[i], 2)

    dta$mean_pf_mglasso[[i]]$tv <- round(l2[i], 2)
  }

  mean_pf_mglasso <- do.call(rbind, dta$mean_pf_mglasso)
  mean_pf_mb <- do.call(rbind, mean_pf_mb)

  mean_pf_mglasso$method <- "mglasso"
  mean_pf_mb$method <- "glasso"

  mean_pf <- rbind(mean_pf_mglasso, mean_pf_mb)

  return(mean_pf)
}
