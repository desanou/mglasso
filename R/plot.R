<<<<<<< HEAD
# jouer sur le color rampalette
image_sparse <- function(matrix, main_ = "", sub_ = "", legend_ = TRUE,
    col_names = FALSE) {
    main_ <- paste0(c(sub_, main_), collapse = " ")

    nn <- 100
    plt <- image(as(matrix, "sparseMatrix"), main = main_, sub = "",
        xlab = "", ylab = "", useAbs = FALSE)

    nn <- rownames(matrix)
    nn <- c(nn[length(nn)], nn)
    nn <- nn[1:(length(nn) - 1)]
    labs <- c(nn, nn)
    if (col_names) {
        update(plt, scales = list(labels = labs))
    }

    plt
}

#' fonction qui affiche les matrices d'adjacence à chaque niveau de la hiérarchie
#' à automatiser
#' utiliser niveau de legende commune
plot_mglasso <- function(mglasso_, beta_true_, true_clusters_, levels_ = NULL,
    version = "no-merge", l1_temp) {

    len <- length(mglasso_$out)
    levels <- names(mglasso_$out)

    if (version == "merge") {
        levels <- levels[!(levels == "level1")]
        len <- length(levels)
    }


    if (!is.null(levels_)) {
        levels <- levels_
        len <- length(levels_)
    }

    if (version == "merge") {
        pl <- lapply(levels, function(level) {
            image_sparse(expand_beta(mglasso_$out[[level]]$Beta, mglasso_$out[[level]]$clusters),
                "", level)
        })
    } else {
        pl <- lapply(levels, function(level) {
            image_sparse(mglasso_$out[[level]]$Beta, "", level)
        })
    }

    pt <- image_sparse(beta_true_, "", "true")

    pl[[len + 1]] <- pt
    l1 <- mglasso_$l1

    do.call(gridExtra::grid.arrange, c(pl, nrow = 2, top = paste0("lambda1 = ",
        l1)))
=======

#' Plot the mage of a matrix
image_sparse <- function(matrix, main_ = "", sub_ = "", legend_ = TRUE,
                         col_names = FALSE) {
  main_ <- paste0(c(sub_, main_), collapse = " ")

  nn <- 100
  plt <- image(as(matrix, "sparseMatrix"), main = main_, sub = "",
               xlab = "", ylab = "", useAbs = FALSE)

  nn <- rownames(matrix)
  nn <- c(nn[length(nn)], nn)
  nn <- nn[1:(length(nn) - 1)]
  labs <- c(nn, nn)
  if (col_names) {
    update(plt, scales = list(labels = labs))
  }

  plt
}

#'Plot mglasso output
plot_mglasso <- function(mglasso_, beta_true_, levels_ = NULL,
                         version = "no-merge") {

  len <- length(mglasso_$out)
  levels <- names(mglasso_$out)


  if (!is.null(levels_)) {
    levels <- levels_
    len <- length(levels_)
  }

  pl <- lapply(levels, function(level) {
    image_sparse(mglasso_$out[[level]]$Beta, "", level)
  })

  pt <- image_sparse(beta_true_, "", "true")

  pl[[len + 1]] <- pt
  l1 <- mglasso_$l1

  do.call(gridExtra::grid.arrange, c(pl, nrow = 2, top = paste0("lambda1 = ",
                                                                l1)))
>>>>>>> initial

}
