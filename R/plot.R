#' Plot the mage of a matrix
#'
#' @param matrix matrix of regression coefficients
#' @param main_
#' @param sub_
#' @param legend_
#' @param col_names
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
#'
#' @param mglasso_
#' @param beta_true_
#' @param levels_
#' @param version
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

}
