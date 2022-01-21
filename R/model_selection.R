neighbor_select <- function(data = data$X, config, lambda_min_ratio = 0.01,
    nlambda = 10, nresamples = 20, lambdas = NULL, model = NULL, verbose = FALSE,
    estim_var = NULL) {

    p = ncol(data)
    if (is.null(lambdas)) {
        S <- cov(scale(data))
        lambda_max = max(max(S - diag(p)), -min(S - diag(p)))
        lambda_min = lambda_min_ratio * lambda_max
        lambdas = exp(seq(log(lambda_max), log(lambda_min), length = nlambda))
    }

    if (is.null(estim_var)) {
        if (!is.null(model)) {
            estim_var <- estimate_variability(nresamples, config$n, config$p,
                config$dnsty, config$pi, config$alpha, config$rho, model)
        }
    }

    mb_out <- huge.select(est = huge(x = scale(data), method = "mb",
        verbose = verbose, sym = "and", lambda = lambdas), criterion = "stars",
        verbose = verbose, stars.thresh = estim_var, rep.num = nresamples)

    lambda1 <- mb_out$opt.lambda

    beta_glasso <- mb_out$beta[[which(mb_out$lambda == lambda1)]]
    # beta_glasso <- symmetrize(beta_glasso, 'and')

    return(list(beta_glasso = beta_glasso, lambda_opt = lambda1, lambdas = lambdas))
}
