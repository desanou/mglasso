#' EBIC
select_ebic_weighted <- function(Thetas, ploglik, n_edges, n, p, gam = 0.5, pen_params){
  EBIC <- -ploglik[n_edges > 0] + n_edges[n_edges > 0]*log(n)/(2*n) + 4*gam*log(p)*n_edges[n_edges > 0]/(2*n)
  best_ind <- which.min(EBIC)
  beta_ebic <- Thetas[[best_ind]]

  params_opt = pen_params[[which(sapply(Thetas, function(e) identical(beta_ebic, e)))]]

  return(list(beta_ebic = beta_ebic, params_opt = params_opt))
}

select_capushe <- function(Thetas, ploglik, n_edges, n, pen_params){
  mat <- cbind(n_edges[n_edges > 0], n_edges[n_edges > 0], n_edges[n_edges > 0], -ploglik[n_edges > 0])

  res <- capushe(mat, n, psi.rlm = "lm", pct = 0.10)

  best_ind <- min(which(n_edges == as.numeric(res@DDSE@model)))
  beta_cap <- Thetas[[best_ind]]

  params_opt = pen_params[[which(sapply(Thetas, function(e) identical(beta_cap, e)))]]


  return(list(beta_cap = beta_cap, params_opt = params_opt))

}

ggmselect <- function(X, Thetas){
  n = nrow(X)
  p = ncol(X)
  Thetas_old = Thetas

  n_penalties = length(Thetas)

  for(i in 1:n_penalties){
    ## Taking adjacency martices
    Thetas[[i]] <- abs(sign(Thetas[[i]]))
    diag(Thetas[[i]]) <- 0
  }

  graph_degree <- function(g){
    ## degree of a graph
    nodes_degree <- max(apply(g, 2, sum))
    dmax <- max(nodes_degree)
    dmax
  }

  degrees <- sapply(Thetas, graph_degree)
  Thetas <- Thetas[which(degrees < n - 3)]

  GMF <- try(selectMyFam(X, Thetas, K = 1), silent = TRUE)

  if(class(GMF) == "try-error"){
    out = Thetas_old[[1]]
  }else{
    out = GMF$G
  }

  return(out)
}

select_stars <- function(huge_object, threshold){
  out_select <- huge.select(est       = huge_object,
                            criterion = "stars",
                            verbose   = FALSE,
                            stars.thresh = threshold)

  return(out_select$refit)
}

#'
#'@export
estimate_variability <- function(nrep, n, p, density, pi, alpha, rho, model) {
  variability = 0
  merge = matrix(0, p, p)

  for(i in 1:nrep){
    data <- sim_data(p, n/p, model, prob_mat = pi, alpha = alpha, rho = rho)

    graph <- data$graph
    graph <- abs(sign(graph))
    diag(graph) <- 0

    merge <- merge + graph
  }

  merge_prop <- merge / nrep

  variability <- 4 * sum(merge_prop * (1 - merge_prop) / (p * (p - 1)))

  variability
}

select_ric <- function(huge_object){
  out_select <- huge.select(est       = huge_object,
                            criterion = "ric",
                            verbose   = FALSE)

  return(out_select$refit)
}

#' Finds the optimal number of clusters using slope heuristic
#'
#' @importFrom capushe DDSE
#' @importFrom capushe Djump
#'
#' @return integer scalar. The indice of the selected model.
select_partition <- function(gains){
  k <- length(gains)
  dt_capushe <- data.frame(name = 1:k,
                           pen.shape = lchoose(k, 0:(k - 1)),
                           complexity = 1:k,
                           contrast = cumsum(gains))

  KC <- try(DDSE(dt_capushe), silent = TRUE)
  if (class(KC) == "try-error")
    KC <- Djump(dt_capushe)

  return(as.integer(KC@model))
}

#' Neighborhood selection estimate
graph_estimate <- function(rho, X){
  glasso(s      = var(X),
         rho    = rho,
         approx = TRUE,
         thr = 1e-7,
         maxit = 1e5)$wi
}



#' Mean error from classical regression
error <- function(Theta, X){
  n <- nrow(X)
  p <- ncol(X)

  error_prediction <- function(rvn){
    norm(X[,rvn] - X %*% Theta[,rvn], "2")^2/n
  }

  sum(sapply(1:p, error_prediction))/p
}

#' Formula from Huge paper
error_huge <- function(Theta, X){
  GSG <- t(Theta) %*% cov(X) %*% Theta
  GS <- t(Theta %*% cov(X))
  0.5 * sum(diag(GSG)) - sum(diag(GS))
}

#' K-fold cross validation neghborhood lasso selection
#' @param criterion c("ploglik", "rmse")
#'
#' @details
#' if criterion = "ploglik" use pseudo-log likelihood formula from Huge paper with matrix approach
#' if "rmse" use mean squarred prediction error
#'
select_kfold <- function(X, Thetas, lambdas=NULL, n_lambda, K_fold = 10, criterion = "ploglik", verbose = TRUE){
  n                = nrow(X)
  X                = scale(X)
  X_train          = X_test = X
  shuffled_indices = sample(n)

  if(is.null(lambdas)){
    nlambda = 10
    lambda_min_ratio = 0.1
    S <- cov(scale(X))
    lambda_max = max(max(S-diag(p)), -min(S-diag(p)))
    lambda_min = lambda_min_ratio*lambda_max
    lambdas = exp(seq(log(lambda_max), log(lambda_min), length = nlambda))
  }

  n_lambda <- length(lambdas)
  errors   <- matrix(NA, nrow = n_lambda, ncol = K_fold)

  for (k in 1:K_fold) {
    if(K_fold > 1){
      fold_size     = n/K_fold
      first_element = 1 + floor((k-1)*fold_size)
      last_element  = floor(k*fold_size)

      to_leave = shuffled_indices[first_element:last_element]
      X_train  = X[-to_leave, , drop = FALSE]
      X_test   = X[to_leave, , drop = FALSE]

      S_train = var(scale(X_train))
      S_test  = var(scale(X_test))
    }

    for(i in 1:n_lambda){
      lambda <- lambdas[i]
      Theta_train <- glasso(s      = S_train,
                            rho    = lambda,
                            approx = TRUE,
                            thr    = 1e-7,
                            maxit  = 1e5)$wi

      Theta_train <- symmetrize(Theta_train, "and")

      if(criterion == "ploglik"){
        error_prop <- function(Theta, X){
          GSG <- t(Theta) %*% cov(X) %*% Theta
          GS  <- t(Theta %*% cov(X))
          0.5 * sum(diag(GSG)) - sum(diag(GS))
        }

        errors[i,k] = error_prop(Theta_train, X_test)

      } else
        if(criterion == "rmse"){
          error_classic <- function(Theta, X){
            n <- nrow(X)
            p <- ncol(X)

            error_prediction <- function(rvn){
              norm(X[,rvn] - X %*% Theta[,rvn], "2")^2/n
            }
            return(sum(sapply(1:p, error_prediction))/p)
          }

          errors[i,k] = error_classic(Theta_train, X_test)
        }
    }

    if(verbose) message(paste0(k, "-th"), "fold done")
  }

  mean_errors <- apply(errors, 1, mean)
  best_lambda <- lambdas[which.min(mean_errors)]
  errors_min  <- min(mean_errors)

  return(Thetas[[best_lambda]])
}

#'
pseudo_likelihood <- function(X, n_lambda, lambdas = NULL){
  n <- nrow(X)
  p <- ncol(X)
  X <- scale(X)

  if(is.null(lambdas)){
    lambda_max <- max(abs(cov(X)))
    lambda_min <- 0.01
    lambdas    <- seq(lambda_min, lambda_max, length.out = n_lambda)
  }

  ploglik <- numeric(length(lambdas))
  n_edges <- numeric(length(lambdas))
  Thetas <- NULL

  for(i in 1:length(lambdas)){
    lambda <- lambdas[i]
    Theta <- glasso(s      = var(X),
                    rho    = lambda,
                    approx = TRUE,
                    thr    = 1e-7,
                    maxit  = 1e5)$wi

    Theta <- sign(Theta)*pmin(abs(Theta), t(abs(Theta))) ## AND rule

    Thetas[[i]] <- Theta
    n_edges[i] <- sum(Theta !=0 & col(Theta) < row(Theta))

    criterion <- function(Theta, X){
      GSG <- t(Theta) %*% cov(X) %*% Theta
      GS <- t(Theta %*% cov(X))
      return(- 0.5 * sum(diag(GSG)) + sum(diag(GS)))
    }
    ploglik[i] <- criterion(Theta, X)
  }

  return(list(ploglik = ploglik, n_edges = n_edges, Thetas = Thetas))
}
