#' compute TPR, FPR, SHD given estimated and true precision matrices
#'
#' @details SHD: structural hamming distance
perf_one <- function (omega_hat, omega)
{
  upper <- upper.tri(omega_hat)

  true_nzero <- which(omega[upper] != 0)
  true_zero  <- which(omega[upper] == 0)

  nzero <- which(omega_hat[upper] != 0)
  zero  <- which(omega_hat[upper] == 0)

  TP <- sum(nzero %in% true_nzero)
  TN <- sum(zero %in%  true_zero)
  FP <- sum(nzero %in% true_zero)
  FN <- sum(zero %in%  true_nzero)

  tpr    <- TP/(TP + FN) ## also recall and sensitivity
  fpr   <- FP/(FP + TN) ## also 1 - specificit
  prec <- TP/(TP+FP)
  tpr[TP + FN == 0] <- NA
  fpr[TN + FP == 0] <- NA
  prec[TP + FP == 0] <- NA

  # Hamming distance
  omega[omega != 0] = 1
  omega_hat[omega_hat != 0] = 1

  shd <- sum(omega[upper] != omega_hat[upper]) / length(omega[upper])

  return(list(tpr = tpr, fpr = fpr, prec = prec, shd = shd))
}

#' get performances from list of estimations
perf_vec <- function(omega_hat_list, omega){
  pf <- tpr <- prec <- shd <- NULL

  for(i in 1:length(omega_hat_list)){
    pf <- perf_one(omega_hat_list[[i]], omega)
    tpr <- c(tpr, pf$tpr)
    fpr <- c(fpr, pf$fpr)
    prec <- c(prec, pf$prec)
    shd <- c(shd, pf$shd)
  }

  fpr <- c(0, fpr, 1)
  tpr <- c(0, tpr, 1)
  prec <- c(0, prec, 1)
  shd <- c(0, shd, 1)

  return(list(tpr = tpr, fpr = fpr, prec = prec, shd = shd))

}

#' Plot ROC curve and calculate AUC
#'
#' @param type Classical ROC curve tpr = f(FPR) or TPR = f(precision) adjusted version.
#' @param to_ maximum value for partial AUC. Default 1.
#'
#' @details When dealing with huge data sets with FPR and TPR do not vary at the same rate.
#' Low variation of number of edges has much more effect on TPR rather than FPR. It's better
#' to consider precision ie 1 - FDR (confidence) vs TPR (power) curve (type = "prec_rec").
#'
#' @import DescTools
plot_roc <- function(omega_hat_list, omega, type = "classical"){

  pf_vec <- perf_vec(omega_hat_list, omega)
  x   <- pf_vec$fpr

  if(type == "precision-recall"){
    x   <- pf_vec$prec
  }

  plot(x, tpr, xlim = 0:1, ylim = 0:1, main = paste0(type, " ROC curve"))
}

#' Compute AUC and partial  AUC
#'
#' @import DescTools
get_auc <- function(omega_hat_list, omega, to = to_){

  auc <- AUC(x, tpr, to = to_)

  return(auc)
}


