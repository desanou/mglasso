#' Plot \code{mglasso} function output.
#'
#' Plot the object returned by the \code{mglasso} function.
#'
#' @param mglasso_ Object of class \code{mglasso}.
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

  l1 <- mglasso_$l1

  do.call(gridExtra::grid.arrange,
          c(pl, nrow = 2, top = paste0("lambda1 = ", l1)))

}
