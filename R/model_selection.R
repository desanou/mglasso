ebic <- function(out_simone, gamma = 0.5){
  n_penalties = length(out_simone$penalties)
  # df <- numeric(n_penalties)
  bic <- out_simone$BIC
  p <- ncol(out_simone$networks[[1]])
  
  # for(i in 1:n_penalties){
  #   Theta <- out_simone$networks[[i]]
  #   df[i] <- sum(abs(Theta)>0) - sum(abs(diag(Theta))>0)
  # }
  
  ebic_vec = bic - (4*gamma*log(p)*out_simone$n.edges)/2
  ebic_vec
  out_simone$networks[[which.max(ebic_vec)]]
}

ggmselect <- function(out_simone, X, ...){
  n = nrow(X)
  p = ncol(X)
  n_penalties = length(out_simone$penalties)
  MyFamily <- out_simone$networks
  
  if (length(MyFamily) == 1) {
    MyFamily = list(MyFamily)
  }
  
  for(i in 1:n_penalties){
    MyFamily[[i]] <- abs(sign(MyFamily[[i]]))
    diag(MyFamily[[i]]) <- 0
  }
  
  degrr <- function(g){
    deg <- max(apply(g,2,sum))
    dmax <- max(deg)
    dmax
  }
  
  all_deg <- sapply(MyFamily, degrr)
  MyFamily <- MyFamily[- which(all_deg >= n - 4)]
  
  if(max(all_deg != 0) && length(MyFamily) != 0){
    GMF <- selectMyFam(X, MyFamily, K = 1)
    out = GMF$G
  }else{
    out = matrix(0, p, p)
  }
  
  return(out)
  
  # for (g in 1:length(MyFamily)) {
  #   dmax = 0
  #   G <- MyFamily[[g]]
  #   
  #   # calculation of degree and maximum degree
  #   deg <- max(apply(G,2,sum))
  #   dmax <- max(dmax,deg)
  #   
  #   if(dmax>=3){
  #     MyFamily[[g]] <- NULL
  #   }
  # }
  
  
}
