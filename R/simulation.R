
# Writen by Katstuhiro Komatsu --------------------------------------------



convert_parameters_from_structural_to_reduced <- function(lambda, 
                                                          matching_params) {
  # Structural params
  lambda_W <- lambda[1]
  lambda_F <- lambda[2]
  alpha_W <- matching_params[1]
  alpha_F <- matching_params[2]
  
  # Reduced form params
  beta <- rep(0, times = 4)
  
  # # beta_DW
  beta[1] <- (1 + (alpha_F + 2 * (alpha_W - 1)) / 2 / (2 - alpha_W - alpha_F)) * lambda_W
  # beta_SW
  beta[2] <- alpha_W / 2 / (2 - alpha_W - alpha_F) * lambda_W
  # beta_DF
  beta[3] <- alpha_F / 2 / (2 - alpha_W - alpha_F) * lambda_F
  # beta_SF
  beta[4] <- (1 + (alpha_W + 2 * (alpha_F - 1)) / 2 / (2 - alpha_W - alpha_F)) * lambda_F


  return(beta)
}

compute_inside_share <- 
  function(
    price,
    mu,
    a,
    H, 
    lambda, 
    matching_params, 
    spec
    ) {
    # price: (wage, fee)
    # mu: matching efficiency
    # a: (a_W, a_F)
    # H: (H_W, H_F)
    
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    mu_tilde <- compute_mu_tilde(mu, matching_params)
    
    if (spec == "linear") {
      s_W <- mu_tilde * exp(beta[1] * (a[1] + price[1]) + beta[3] * (a[2] - price[2])) / H[1] 
      s_F <- mu_tilde * exp(beta[2] * (a[1] + price[1]) + beta[4] * (a[2] - price[2])) / H[2]    
    } else if (spec == "log") {
      s_W <- mu_tilde * exp(beta[1] * (a[1] + log(price[1])) + beta[3] * (a[2] - log(price[2]))) / H[1]
      s_F <- mu_tilde * exp(beta[2] * (a[1] + log(price[1])) + beta[4] * (a[2] - log(price[2]))) / H[2]
    } else if (spec == "log-linear") {
      s_W <- mu_tilde * exp(beta[1] * (a[1] + log(price[1])) + beta[3] * (a[2] - price[2])) / H[1]
      s_F <- mu_tilde * exp(beta[2] * (a[1] + log(price[1])) + beta[4] * (a[2] - price[2])) / H[2]
    } else {
      stop("spec must be either linear, log, or log-linear")
    }
    
    return(cbind(s_W, s_F))
}


compute_inside_share_vec <- 
  function(
    price,
    mu,
    a,
    H,
    lambda,
    matching_params, 
    spec
    ) {
    # price: (wage, fee)
    # a: (a_W, a_F)
    # H: (H_W, H_F)
    
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    
    alpha_W <- matching_params[1]
    alpha_F <- matching_params[2]
    mu_tilde <- mu ^ (2 - (1 - alpha_W - alpha_F) / (2 - alpha_W - alpha_F))
    
    if (spec == "linear") {
      s_W <- mu_tilde * exp(beta[1] * (a[, 1] + price[, 1]) + beta[3] * (a[, 2] - price[, 2])) / H[1] 
      s_F <- mu_tilde * exp(beta[2] * (a[, 1] + price[, 1]) + beta[4] * (a[, 2] - price[, 2])) / H[2]    
    } else if (spec == "log") {
      s_W <- mu_tilde * exp(beta[1] * (a[, 1] + log(price[, 1])) + beta[3] * (a[, 2] - log(price[, 2]))) / H[1]
      s_F <- mu_tilde * exp(beta[2] * (a[, 1] + log(price[, 1])) + beta[4] * (a[, 2] - log(price[, 2]))) / H[2]
    } else if (spec == "log-linear") {
      s_W <- mu_tilde * exp(beta[1] * (a[, 1] + log(price[, 1])) + beta[3] * (a[, 2] - price[, 2])) / H[1]
      s_F <- mu_tilde * exp(beta[2] * (a[, 1] + log(price[, 1])) + beta[4] * (a[, 2] - price[, 2])) / H[2]
    } else {
      stop("spec must be either linear, log, or log-linear")
    }
  
    return(cbind(s_W, s_F))
}


compute_outside_share <- 
  function(
    w_0, 
    H,
    lambda,
    matching_params,
    spec
    ) {
    # These satisfy the normalization in the paper (beta_1 a_W + beta_2 a_F + (2 - (1-alpha - beta) / (2 - alpha - beta)) * log(mu) = 0)# These satisfy the normalization in the paper (beta_1 a_W + beta_2 a_F + (2 - (1-alpha - beta) / (2 - alpha - beta)) * log(mu) = 0)
    price <- matrix(c(w_0, w_0), nrow = 1)
    a <- matrix(c(0, 0), nrow = 1)
    mu <- 1
    s_0 <- compute_inside_share_vec(price, mu, a, H, lambda, matching_params, spec)
    
    return(s_0)
}

compute_mu_tilde <-
  function(
    mu,
    matching_params
  ) {
    alpha_W <- matching_params[1]
    alpha_F <- matching_params[2]
    mu_tilde <- mu ^ (2 - (1 - alpha_W - alpha_F) / (2 - alpha_W - alpha_F))
    return(mu_tilde)
  }

compute_aggregator <- 
  function(
    price, 
    mu,
    a, 
    w_0, 
    lambda,
    matching_params,
    spec
    ) {
    # Compute aggregator based on price profile
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    
    # compute the adjusted efficiency
    mu_tilde <- compute_mu_tilde(mu, matching_params)
    
    if (spec == "linear") {
      H_W <- (mu_tilde * exp(beta[1] * (a[, 1] + price[, 1]) + beta[3] * (a[, 2] - price[, 2]))) %>% sum()
      H_F <- (mu_tilde * exp(beta[2] * (a[, 1] + price[, 1]) + beta[4] * (a[, 2] - price[, 2]))) %>% sum()
      
      # Adding the contribution of outside option
      H_W <- H_W + exp(beta[1] * w_0 + beta[3] * (-w_0))
      H_F <- H_F + exp(beta[2] * w_0 + beta[4] * (-w_0))    
    } else if (spec == "log") {
      H_W <- (mu_tilde * exp(beta[1] * (a[, 1] + log(price[, 1])) + beta[3] * (a[, 2] - log(price[, 2])))) %>% sum()
      H_F <- (mu_tilde * exp(beta[2] * (a[, 1] + log(price[, 1])) + beta[4] * (a[, 2] - log(price[, 2])))) %>% sum()
      
      # Adding the contribution of outside option
      H_W <- H_W + exp(beta[1] * log(w_0) + beta[3] * (-log(w_0)))
      H_F <- H_F + exp(beta[2] * log(w_0) + beta[4] * (-log(w_0))) 
    } else if (spec == "log-linear") {
      H_W <- (mu_tilde * exp(beta[1] * (a[, 1] + log(price[, 1])) + beta[3] * (a[, 2] - price[, 2]))) %>% sum()
      H_F <- (mu_tilde * exp(beta[2] * (a[, 1] + log(price[, 1])) + beta[4] * (a[, 2] - price[, 2]))) %>% sum()
      
      # Adding the contribution of outside option
      H_W <- H_W + exp(beta[1] * log(w_0) + beta[3] * (-w_0))
      H_F <- H_F + exp(beta[2] * log(w_0) + beta[4] * (-w_0)) 
    }  else {
      stop("spec must be either linear, log, or log-linear")
    }
    
    H <- c(H_W, H_F) %>% unname()
    return(H)
  }

compute_match <-
  function(
    mu,
    s,
    S,
    matching_params
  ) {
    alpha_W <- matching_params[1]
    alpha_F <- matching_params[2]
    q <- mu * (s[, 1] * S[1]) ^ alpha_W * (s[, 2] * S[2]) ^ alpha_F
    return(q)
  }

compute_profit <- function(price, 
                           s, 
                           mu,
                           mc, 
                           S, 
                           matching_params, 
                           rp) {

  
  alpha_W <- matching_params[1]
  alpha_F <- matching_params[2]

  q <- mu * (s[1] * S[1]) ^ alpha_W * (s[2] * S[2]) ^ alpha_F
  profit <- q * (price[2] - price[1]) - (s[1] * S[1]) * mc[1] - (s[2] * S[2]) * mc[2] - rp * price[1] * mc[1] + rp * price[2] * mc[2]
  
  return(profit)
}

compute_profit_vec <- 
  function(
    price, 
    s, 
    mu,
    mc, 
    S, 
    matching_params, 
    rp
  ) {
    q <-
      compute_match(
        mu,
        s,
        S,
        matching_params
      ) 
    profit <- q * (price[, 2] - price[, 1]) - (s[, 1] * S[1]) * mc[, 1] - (s[, 2] * S[2]) * mc[, 2] - rp * price[, 1] * mc[, 1] + rp * price[, 2] * mc[, 2]
    
    return(profit)
  }



compute_worker_indirect_utility <- function(wage,
                                            a_W,
                                            n_W,
                                            n_F,
                                            shock,
                                            lambda,
                                            matching_params,
                                            spec) {
  
  if (spec == "log") {
    u <- (a_W + log(wage)) * lambda[1] - (1 - matching_params[1]) * log(n_W) + matching_params[2] * log(n_F) + shock    
  } else if (spec == "linear") {
    u <- (a_W + wage) * lambda[1] - (1 - matching_params[1]) * log(n_W) + matching_params[2] * log(n_F) + shock
  } else if (spec == "log-linear") {
    u <- (a_W + log(wage)) * lambda[1] - (1 - matching_params[1]) * log(n_W) + matching_params[2] * log(n_F) + shock
  } else {
    stop("spec must be either linear, log, or log-linear")
  }

  return(u)
}


compute_client_firm_indirect_utility <- function(fee,
                                            a_F,
                                            n_W,
                                            n_F,
                                            shock,
                                            lambda,
                                            matching_params,
                                            spec) {

  if (spec == "log") {
    v <- (a_F - log(fee)) * lambda[2] + matching_params[1] * log(n_W) - (1 - matching_params[2]) * log(n_F) + shock
  } else if (spec == "linear") {
    v <- (a_F - fee) * lambda[2] + matching_params[1] * log(n_W) - (1 - matching_params[2]) * log(n_F) + shock
  } else if (spec == "log-linear") {
    v <- (a_F - fee) * lambda[2] + matching_params[1] * log(n_W) - (1 - matching_params[2]) * log(n_F) + shock
  } else {
    stop("spec must be either linear, log, or log-linear")
  }

  return(v)
}

compute_derivatives <-
  function(
    q,
    price,
    s,
    S,
    lambda,
    matching_params,
    spec
  ) {
    N <- dim(price)[1]
    alpha_W <- matching_params[1]
    alpha_F <- matching_params[2]
    beta <- 
      convert_parameters_from_structural_to_reduced(
        lambda, 
        matching_params
        )
    beta_DW <- beta[1]
    beta_SW <- beta[2]
    beta_DF <- beta[3]
    beta_SF <- beta[4]

    
    dq_dw <- matrix(0, nrow = N, ncol = N)
    dq_df <- matrix(0, nrow = N, ncol = N)
    dDw_dw <- matrix(0, nrow = N, ncol = N)
    dDf_dw <- matrix(0, nrow = N, ncol = N)
    dDw_df <- matrix(0, nrow = N, ncol = N)
    dDf_df <- matrix(0, nrow = N, ncol = N)
    
    if (spec == "linear") {
      
      # Off diagonal (Diagonals are replaced later)
      
      dq_dw <- -q %*% (alpha_W * beta_DW * t(s[, 1]) + alpha_F * beta_SW * t(s[, 2]))
      dq_df <- q %*% (alpha_W * beta_DF * t(s[, 1]) + alpha_F * beta_SF * t(s[, 2]))
      dDw_dw <- -S[1] * beta_DW * s[, 1] %*% t(s[, 1])
      dDf_dw <- -S[2] * beta_SW * s[, 2] %*% t(s[, 2])
      dDw_df <- S[1] * beta_DF * s[, 1] %*% t(s[, 1])
      dDf_df <- S[2] * beta_SF * s[, 2] %*% t(s[, 2])    
      
      # Diagonal
      diag(dq_dw) <- q * (alpha_W * beta_DW * (1 - s[, 1]) + alpha_F * beta_SW * (1 - s[, 2]))
      diag(dq_df) <- -q * (alpha_W * beta_DF * (1 - s[, 1]) + alpha_F * beta_SF * (1 - s[, 2]))
      diag(dDw_dw) <- S[1] * beta_DW * (1 - s[, 1]) * s[, 1]
      diag(dDf_dw) <- S[2] * beta_SW * (1 - s[, 2]) * s[, 2]
      diag(dDw_df) <- -S[1] * beta_DF * (1 - s[, 1]) * s[, 1]
      diag(dDf_df) <- -S[2] * beta_SF * (1 - s[, 2]) * s[, 2]
      
    } else if (spec == "log") {
      
      # Off diagonal (Diagonals are replaced later)
      
      dq_dw <- -q %*% ( (alpha_W * beta_DW * t(s[, 1]) + alpha_F * beta_SW * t(s[, 2])) / t(price[, 1]) )
      dq_df <-  q %*% ( (alpha_W * beta_DF * t(s[, 1]) + alpha_F * beta_SF * t(s[, 2])) / t(price[, 2]) )
      dDw_dw <- -S[1] * beta_DW *  s[, 1] %*%  t(s[, 1] / price[, 1])
      dDf_dw <- -S[2] * beta_SW *  s[, 2] %*%  t(s[, 2] / price[, 2])
      dDw_df <-  S[1] * beta_DF *  s[, 1] %*%  t(s[, 1] / price[, 1])
      dDf_df <-  S[2] * beta_SF *  s[, 2] %*%  t(s[, 2] / price[, 2])
      
      # Diagonal
      diag(dq_dw) <- q * (alpha_W * beta_DW * (1 - s[, 1]) + alpha_F * beta_SW * (1 - s[, 2])) / price[, 1]
      diag(dq_df) <- -q * (alpha_W * beta_DF * (1 - s[, 1]) + alpha_F * beta_SF * (1 - s[, 2])) / price[, 2]
      diag(dDw_dw) <- S[1] * beta_DW * (1 - s[, 1]) * s[, 1] / price[, 1]
      diag(dDf_dw) <- S[2] * beta_SW * (1 - s[, 2]) * s[, 2] / price[, 1]
      diag(dDw_df) <- -S[1] * beta_DF * (1 - s[, 1]) * s[, 1] / price[, 2]
      diag(dDf_df) <- -S[2] * beta_SF * (1 - s[, 2]) * s[, 2] / price[, 2]
      
    } else if (spec == "log-linear") {
      
      # Off diagonal (Diagonals are replaced later)
      
      dq_dw <- -q %*% ( (alpha_W * beta_DW * t(s[, 1]) + alpha_F * beta_SW * t(s[, 2])) / t(price[, 1]) )
      dq_df <-  q %*% (alpha_W * beta_DF * t(s[, 1]) + alpha_F * beta_SF * t(s[, 2]))
      dDw_dw <- -S[1] * beta_DW *  s[, 1] %*%  t(s[, 1] / price[, 1])
      dDf_dw <- -S[2] * beta_SW *  s[, 2] %*%  t(s[, 2])
      dDw_df <-  S[1] * beta_DF *  s[, 1] %*%  t(s[, 1] / price[, 1])
      dDf_df <-  S[2] * beta_SF *  s[, 2] %*%  t(s[, 2])
      
      # Diagonal
      diag(dq_dw) <- q * (alpha_W * beta_DW * (1 - s[, 1]) + alpha_F * beta_SW * (1 - s[, 2])) / price[, 1]
      diag(dq_df) <- -q * (alpha_W * beta_DF * (1 - s[, 1]) + alpha_F * beta_SF * (1 - s[, 2]))
      diag(dDw_dw) <- S[1] * beta_DW * (1 - s[, 1]) * s[, 1] / price[, 1]
      diag(dDf_dw) <- S[2] * beta_SW * (1 - s[, 2]) * s[, 2] / price[, 1]
      diag(dDw_df) <- -S[1] * beta_DF * (1 - s[, 1]) * s[, 1]
      diag(dDf_df) <- -S[2] * beta_SF * (1 - s[, 2]) * s[, 2]
    } else {
      stop("spec must be either linear, log, or log-linear")
    }
    return(
      list(
        dq_dw = dq_dw,
        dq_df = dq_df,
        dDw_dw = dDw_dw,
        dDf_dw = dDf_dw,
        dDw_df = dDw_df,
        dDf_df = dDf_df
      )
      )
  }

compute_derivatives_numerical <-
  function(
    price,
    S,
    mu,
    a,
    w_0,
    lambda,
    matching_params,
    spec
  ) {
    
    f <-
      function(
    x,
    target,
    control
      ) {
        price_x <- price
        
        if (control == "w") {
          price_x[, 1] <- x
        } else if (control == "f") {
          price_x[, 2] <- x
        }
        
        H <-
          compute_aggregator(
            price = price_x,
            mu = mu,
            a = a,
            w_0 = w_0,
            lambda = lambda,
            matching_params = lambda,
            spec = spec
          )
        
        share <-
          compute_inside_share_vec(
            price = price_x,
            mu = mu,
            a = a,
            H = H,
            lambda = lambda,
            matching_params = matching_params,
            spec = spec
          )
        
        D <-
          cbind(
            share[, 1] * S[1],
            share[, 2] * S[2]
          )
        
        q <-
          compute_match(
            mu = mu,
            s = share,
            S = S,
            matching_params = matching_params
          ) 
        
        if (target == "Dw") {
          output <- D[, 1, drop = FALSE]
        } else if (target == "Df") {
          output <- D[, 2, drop = FALSE]
        } else if (target == "q") {
          output <- q
        }
        
        return(output)
      }
    
    dq_dw <-
      numDeriv::jacobian(
        x = price[, 1],
        func = f,
        target = "q",
        control = "w"
      )
    dq_df <-
      numDeriv::jacobian(
        x = price[, 2],
        func = f,
        target = "q",
        control = "f"
      )
    dDw_dw <-
      numDeriv::jacobian(
        x = price[, 1],
        func = f,
        target = "Dw",
        control = "w"
      )
    dDw_df <-
      numDeriv::jacobian(
        x = price[, 2],
        func = f,
        target = "Dw",
        control = "f"
      )
    dDf_dw <-
      numDeriv::jacobian(
        x = price[, 1],
        func = f,
        target = "Df",
        control = "w"
      )
    dDf_df <-
      numDeriv::jacobian(
        x = price[, 2],
        func = f,
        target = "Df",
        control = "f"
      )
    
    return(
      list(
        dq_dw = dq_dw,
        dq_df = dq_df,
        dDw_dw = dDw_dw,
        dDf_dw = dDf_dw,
        dDw_df = dDw_df,
        dDf_df = dDf_df
      )
    )
  }

compute_foc_error_with_ownership_matrix <- 
  function(
    x,
    ownership,
    w_0,
    mu,
    a,
    mc,
    S, 
    lambda,
    matching_params,
    spec,
    rp
  ) {
    price <- x %>% matrix(ncol = 2)
    H <- 
      compute_aggregator(
        price, 
        mu, 
        a, 
        w_0, 
        lambda, 
        matching_params, 
        spec
        )
    s <- 
      compute_inside_share_vec(
        price, 
        mu, 
        a, 
        H, 
        lambda, 
        matching_params, 
        spec
        )
    N <- dim(price)[1]
    q <-
      compute_match(
        mu,
        s,
        S,
        matching_params
      ) 
    derivatives <-
      compute_derivatives(
        q,
        price,
        s,
        S,
        lambda,
        matching_params,
        spec
      ) 
    
    foc_error_w <- 
      - q + 
      (ownership * derivatives$dq_dw) %*% (price[, 2] - price[, 1]) - 
      (ownership * derivatives$dDw_dw) %*% mc[, 1] - 
      (ownership * derivatives$dDf_dw) %*% mc[, 2] - 
      rp * mc[, 1]
    foc_error_f <- 
      q + 
      (ownership * derivatives$dq_df) %*% (price[, 2] - price[, 1]) - 
      (ownership * derivatives$dDw_df) %*% mc[, 1] - 
      (ownership * derivatives$dDf_df) %*% mc[, 2] + 
      rp * mc[, 2]
    
    # Rescale FOC (Skrainka 2012)
    foc_error_w <- foc_error_w / q
    foc_error_f <- foc_error_f / q
    
    
    foc_error <- 
      c(
        foc_error_w, 
        foc_error_f
        )
    
    return(foc_error)
  }


# 
# 
# compute_foc_error_jacobian_with_ownership_matrix <- function(x,
#                                                     ownership,
#                                                     w_0,
#                                                     mu,
#                                                     a,
#                                                     mc,
#                                                     S, 
#                                                     lambda,
#                                                     matching_params,
#                                                     spec,
#                                                     rp) {
#   
#   
#   alpha_W <- matching_params[1]
#   alpha_F <- matching_params[2]
#   
#   beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
#   
#   beta_DW <- beta[1]
#   beta_SW <- beta[2]
#   beta_DF <- beta[3]
#   beta_SF <- beta[4]
#   
#   price <- x %>% matrix(ncol = 2)
#   H <- compute_aggregator(price, a, w_0, lambda, matching_params, spec)
#   s <- compute_inside_share_vec(price, a, H, lambda, matching_params, spec)
#   N <- dim(price)[1]
#   
#   
#   # Construct matrix for derivatives  price[1] <- max(price[1], 1e-20)
#   
#   q <- mu * (s[, 1] * S[1]) ^ alpha_W * (s[, 2] * S[2]) ^ alpha_F
#   
#   dq_dw <- matrix(0, nrow = N, ncol = N)
#   dq_df <- matrix(0, nrow = N, ncol = N)
#   dDw_dw <- matrix(0, nrow = N, ncol = N)
#   dDf_dw <- matrix(0, nrow = N, ncol = N)
#   dDw_df <- matrix(0, nrow = N, ncol = N)
#   dDf_df <- matrix(0, nrow = N, ncol = N)
#   
#   if (spec == "linear") {
#     
#     # Off diagonal (Diagonals are replaced later)
#     
#     dq_dw <- -q %*% (alpha_W * beta_DW * t(s[, 1]) + alpha_F * beta_SW * t(s[, 2]))
#     dq_df <- q %*% (alpha_W * beta_DF * t(s[, 1]) + alpha_F * beta_SF * t(s[, 2]))
#     dDw_dw <- -S[1] * beta_DW * s[, 1] %*% t(s[, 1])
#     dDf_dw <- -S[2] * beta_SW * s[, 2] %*% t(s[, 2])
#     dDw_df <- S[1] * beta_DF * s[, 1] %*% t(s[, 1])
#     dDf_df <- S[2] * beta_SF * s[, 2] %*% t(s[, 2])    
#     
#     # Diagonal
#     diag(dq_dw) <- q * (alpha_W * beta_DW * (1 - s[, 1]) + alpha_F * beta_SW * (1 - s[, 2]))
#     diag(dq_df) <- -q * (alpha_W * beta_DF * (1 - s[, 1]) + alpha_F * beta_SF * (1 - s[, 2]))
#     diag(dDw_dw) <- S[1] * beta_DW * (1 - s[, 1]) * s[, 1]
#     diag(dDf_dw) <- S[2] * beta_SW * (1 - s[, 2]) * s[, 2]
#     diag(dDw_df) <- -S[1] * beta_DF * (1 - s[, 1]) * s[, 1]
#     diag(dDf_df) <- -S[2] * beta_SF * (1 - s[, 2]) * s[, 2]
#     
#   } else if (spec == "log") {
#     
#     # Off diagonal (Diagonals are replaced later)
#     
#     dq_dw <- -q %*% ( (alpha_W * beta_DW * t(s[, 1]) + alpha_F * beta_SW * t(s[, 2])) / t(price[, 1]) )
#     dq_df <-  q %*% ( (alpha_W * beta_DF * t(s[, 1]) + alpha_F * beta_SF * t(s[, 2])) / t(price[, 2]) )
#     dDw_dw <- -S[1] * beta_DW *  s[, 1] %*%  t(s[, 1] / price[, 1])
#     dDf_dw <- -S[2] * beta_SW *  s[, 2] %*%  t(s[, 2] / price[, 2])
#     dDw_df <-  S[1] * beta_DF *  s[, 1] %*%  t(s[, 1] / price[, 1])
#     dDf_df <-  S[2] * beta_SF *  s[, 2] %*%  t(s[, 2] / price[, 2])
#     
#     # Diagonal
#     diag(dq_dw) <- q * (alpha_W * beta_DW * (1 - s[, 1]) + alpha_F * beta_SW * (1 - s[, 2])) / price[, 1]
#     diag(dq_df) <- -q * (alpha_W * beta_DF * (1 - s[, 1]) + alpha_F * beta_SF * (1 - s[, 2])) / price[, 2]
#     diag(dDw_dw) <- S[1] * beta_DW * (1 - s[, 1]) * s[, 1] / price[, 1]
#     diag(dDf_dw) <- S[2] * beta_SW * (1 - s[, 2]) * s[, 2] / price[, 1]
#     diag(dDw_df) <- -S[1] * beta_DF * (1 - s[, 1]) * s[, 1] / price[, 2]
#     diag(dDf_df) <- -S[2] * beta_SF * (1 - s[, 2]) * s[, 2] / price[, 2]
#   } else {
#     stop("spec must be either linear or log")
#   }
#   
#   
#   foc_error_w <- - q + (ownership * dq_dw) %*% (price[, 2] - price[, 1]) - (ownership * dDw_dw) %*% mc[, 1] - (ownership * dDf_dw) %*% mc[, 2] - rp * mc[, 1]
#   foc_error_f <- q + (ownership * dq_df) %*% (price[, 2] - price[, 1]) - (ownership * dDw_df) %*% mc[, 1] - (ownership * dDf_df) %*% mc[, 2] + rp * mc[, 2]
#   foc_error <- c(foc_error_w, foc_error_f)
#   
#   return(foc_error)
# }





compute_inside_share_single_market <- function(H, 
                                              price, 
                                              mu,
                                              w_0, 
                                              a, 
                                              S, 
                                              lambda,
                                              matching_params,
                                              spec) {
  
  N <- dim(price)[1]
  out <- matrix(0, nrow = N, ncol = 2)
  for(i in 1:N) {
    price_i <- price[i, ]
    a_i <- a[i, ]
    mu_i <- mu[i]
    
    share_i <- compute_inside_share(price_i, mu_i, a_i, H, lambda, matching_params, spec)
    out[i, 1:2] <- share_i
  }
  
  return(out)
}

solve_equilibrium_single_market_with_ownership_matrix <- 
  function(
    price_init,
    ownership,
    w_0,
    mu,
    a, 
    mc,
    S, 
    lambda,
    matching_params,
    spec,
    rp,
    cpp = TRUE
  ) {
    
    x_init <- price_init %>% as.vector()
    
    if (cpp) {
      fn = compute_foc_error_with_ownership_matrix_cpp
    } else {
      fn = compute_foc_error_with_ownership_matrix
    }
    
    result <- 
      nleqslv::nleqslv(
        x = x_init,
        fn = fn,
        jac = NULL,
        ownership = ownership,
        w_0 = w_0,
        mu = mu,
        a = a,
        mc = mc,
        S = S,
        lambda = lambda,
        matching_params = matching_params,
        spec = spec,
        rp = rp,
        control = 
          list(
            maxit = 3000, 
            allowSingular = TRUE
            )
        )
    
    price <- result$x %>% matrix(ncol = 2)
    H <- compute_aggregator(price, mu, a, w_0, lambda, matching_params, spec)
    endogenous_vars <-
      cbind(
        price,
        compute_inside_share_vec_cpp(
          price, 
          mu, 
          a, 
          H, 
          lambda, 
          matching_params, 
          spec
        )
      )
    return(
      list(
        endogenous_vars = endogenous_vars, 
        convergence = result$termcd, 
        foc_error = max(abs(result$fvec))
      )
    )
  }

solve_equilibrium_single_market_with_ownership_matrix_using_minimization <- 
  function(
    price_init,
    ownership,
    w_0,
    mu,
    a,
    mc,
    S,
    lambda,
    matching_params,
    spec,
    rp
  ) {
    
    x_init <- price_init %>% as.vector()
    
    fn <- function(x) {
      out <- compute_foc_error_with_ownership_matrix_cpp(x, ownership, w_0, mu, a, mc, S, lambda, matching_params, spec, rp)
      return(sum(out ^ 2))
    }
    
    # result <- nloptr::neldermead(
    result <- 
      nloptr::lbfgs(
        x0 = x_init,
        fn = fn,
        lower = rep(0.01, times = length(x_init)),
        upper = rep(10, times = length(x_init))
      )
    
    price <- result$par %>% matrix(ncol = 2)
    H <- compute_aggregator(price, mu, a, w_0, lambda, matching_params, spec)

    endogenous_vars <-
      cbind(
        price,
        compute_inside_share_vec_cpp(
          price, 
          mu, 
          a, 
          H, 
          lambda, 
          matching_params, 
          spec
        )
      )
    
    return(
      list(
        endogenous_vars = endogenous_vars, 
        convergence = result$convergence
      )
    )
  }

solve_equilibrium_single_market_wrapper <- 
  function(
    data,
    ownership,
    parameters,
    spec,
    cpp = TRUE,
    init_value_multi = 1
    ) {
    # data: data for a single market
    
    lambda <- parameters$lambda
    matching_params <- parameters$matching_params
    rp <- parameters$rp
    
    # Extract matrices and scalars from list
    w_0 <- data$w_0
    mu <- data$mu
    a <- data$a
    mc <- data$mc
    S <- data$S
    
    if (!is.null(data$price)) {
      price_init <- data$price * init_value_multi
    } else {
      price_init <- matrix(0, nrow = dim(a)[1], ncol = 2)
      price_init[, 1] <- 0.9
      price_init[, 2] <- 1.2
    }
    
    out <- solve_equilibrium_single_market_with_ownership_matrix(price_init, ownership, w_0, mu, a, mc, S, lambda, matching_params, spec, rp, cpp)
    
    endogenous_vars <- out$endogenous_vars
    
    price <- endogenous_vars[, 1:2] %>% matrix(ncol = 2)
    s <- endogenous_vars[, 3:4] %>% matrix(ncol = 2)
    q <- compute_match(mu, s, S, matching_params)
    profit <- compute_profit_vec_cpp(price, s, mu, mc, S, matching_params, rp)
    
    endogenous_single_market <- 
      data.frame(
        id_unique = data$id_unique,
        wage = endogenous_vars[, 1],
        fee = endogenous_vars[, 2],
        s_W = endogenous_vars[, 3],
        s_F = endogenous_vars[, 4],
        Q = q %>% as.numeric(),
        profit = profit,
        convergence = out$convergence
      )
    
    return(endogenous_single_market)
}

solve_equilibrium_all <- 
  function(
    equilibrium,
    N_max = Inf,
    parallel = FALSE,
    cpp = TRUE,
    sim_id = 1
    ) {
    if (parallel == TRUE) {
      `%mydo%` <- `%dopar%`
    } else {
      `%mydo%` <- `%do%`
    }
  
    parameters <- equilibrium$parameters
    spec <- equilibrium$spec
    
    # Loop over markets
    endogenous <- 
      foreach(
        t = 1:length(equilibrium$exogenous), 
        .packages = c("Dispatching", "foreach", "magrittr")
        ) %mydo% {
          
          data_t <- equilibrium$extract_single_market(t, sim_id)
          ownership <- equilibrium$ownership[[t]]
          
          if (dim(data_t$price)[1] <= N_max) {
            out <- 
              solve_equilibrium_single_market_wrapper(
                data_t, 
                ownership, 
                parameters, 
                spec, 
                cpp
                )
            out$market <- t
            out <- as.matrix(out)
          } else {
            out <- 
              data.frame(
                id_unique = data_t$id_unique, 
                wage = NA, 
                fee = NA, 
                s_W = NA, 
                s_F = NA, 
                Q = NA, 
                profit = NA, 
                convergence = NA, 
                market = t
                )
            out <- as.matrix(out)
          }
          return(out)
        }  
    
    equilibrium_out <- equilibrium$copy()
    equilibrium_out$endogenous <- endogenous
    equilibrium_out$generate_dataframe()
    
    return(equilibrium_out) 
  
}


# 
# solve_equilibrium_all <- function(equilibrium,
#                                   N_max = Inf,
#                                   n_sim = 1,
#                                   actual = TRUE,
#                                   parallel = FALSE,
#                                   cpp = TRUE,
#                                   init_value_multi = 1) {
#   
# 
#   parameters <- equilibrium$parameters
#   spec <- equilibrium$spec
#   
#   # Restrict markets
#   mkt_vec <- c()
#   for (i in 1:length(equilibrium$exogenous)) {
#     N <- dim(equilibrium$exogenous[[i]])[1]
#     if (N <= N_max) {
#       mkt_vec <- c(mkt_vec, i)
#     }
#   }
#   
#   if (parallel == TRUE) {
#     `%mydo%` <- `%dopar%`
#   } else {
#     `%mydo%` <- `%do%`
#   }
#   
# 
#   simdata_list <- list()
#   
#   for(k in 1:n_sim) {
#     print(paste("Simulation: ", as.character(k), sep = ""))
#     
#     if (actual == T) {
#       sim_id <- 1
#     } else{
#       sim_id <- k + 1
#     }
#       
#     # Loop over markets
#     simdata_list[[k]] <- foreach(j = mkt_vec, .combine = "rbind", .inorder = FALSE, .packages = c("foreach", "magrittr", "Dispatching")) `%mydo%` {
#       data_j <- equilibrium$extract_single_market(j, sim_id)
#       ownership_j <- equilibrium$ownership[[j]]
#       out <- solve_equilibrium_single_market_wrapper(data_j, ownership_j, parameters, spec, cpp, init_value_multi)
#       return(out)
#     }  
#   }
# 
#   simdata <- do.call("rbind", simdata_list)  
#   rownames(simdata) <- NULL
#   return(simdata)
# 
# }
# 
# 



convert_dataframe_to_equilibrium_list <- function(data, var_names) {
  
  equilibrium <- list()
  equilibrium$exogenous <- list()
  equilibrium$endogenous <- list()
  equilibrium$instruments <- list()
  equilibrium$platform_heterogeneity <- list()
  equilibrium$shocks <- list()
  equilibrium$var_names <- var_names

  equilibrium$parameters <- list(
    lambda = c(0, 0), 
    linear_a_W = c(),
    linear_a_F = c(),
    linear_mc_W = c(),
    linear_mc_F = c(),
    matching_params = c(0, 0),
    rp = 0
    )
  
  
  year_cz_vec <- data$year_cz %>% unique() %>% sort()
  
  for (i in 1:length(year_cz_vec)) {
    
    data_i <- data %>% 
      dplyr::filter(year_cz == year_cz_vec[i])
    
    equilibrium$exogenous[[i]] <- data_i[c(var_names$x_names, "id_unique", "firm_id", "year", "cz", "year_cz", "ptwage", "S_W", "S_F")] %>% 
      as.matrix()
    equilibrium$endogenous[[i]] <- data_i[c("id_unique", "wage", "fee", "s_W", "s_F", "Q")] %>% 
      as.matrix()
    equilibrium$instruments[[i]] <- data_i[c("id_unique", var_names$iv_names)] %>% 
      as.matrix()
    
  }
  
  return(equilibrium)
}


extract_endogenous_variables <- function(data) {
  
  endogenous <- data[c("id_unique", "wage", "fee", "s_W", "s_F", "Q")]
  return(endogenous)
}



convert_equilibrium_list_to_dataframe <- function(equilibrium) {
  
  exogenous <- do.call("rbind", equilibrium$exogenous) %>% as.data.frame()
  endogenous <- do.call("rbind", equilibrium$endogenous) %>% as.data.frame()
  instruments <- do.call("rbind", equilibrium$instruments) %>% as.data.frame()
  data <- exogenous %>% 
    dplyr::left_join(endogenous, by = "id_unique") %>% 
    dplyr::left_join(instruments, by = "id_unique")
  
  return(data)
}




draw_residuals <- function(data, resid_type = "exact", supply_only = TRUE) {
  
  
  n <- length(data["a_W_resid"])
  
  if (resid_type == "resample") {
    
    if (supply_only == TRUE) {
      a_W_shock <- data["a_W_resid"]
      a_F_shock <- data["a_F_resid"]
      mc_W_shock <- sample(data["mc_W_resid"], replace = T)
      mc_F_shock <- sample(data["mc_F_resid"], replace = T)
    } else {
      a_W_shock <- sample(data["a_W_resid"], replace = T)
      a_F_shock <- sample(data["a_F_resid"], replace = T)
      mc_W_shock <- sample(data["mc_W_resid"], replace = T)
      mc_F_shock <- sample(data["mc_F_resid"], replace = T)
    }
    
  } else if (resid_type == "exact") {
    
    a_W_shock <- data["a_W_resid"]
    a_F_shock <- data["a_F_resid"]
    mc_W_shock <- data["mc_W_resid"]
    mc_F_shock <- data["mc_F_resid"]
    
  } else if (resid_type == "fill_zero") {
    
    if (supply_only == TRUE) {
      a_W_shock <- data["a_W_resid"]
      a_F_shock <- data["a_F_resid"]
      mc_W_shock <- rep(0, times = n)
      mc_F_shock <- rep(0, times = n)
    } else {
      a_W_shock <- rep(0, times = n)
      a_F_shock <- rep(0, times = n)
      mc_W_shock <- rep(0, times = n)
      mc_F_shock <- rep(0, times = n)
    }    
    
  } else {
    stop("resid_type must be resample, exact, or fill_zero.")
  }
  
  shocks <- cbind(a_W_shock, a_F_shock, mc_W_shock, mc_F_shock)
  colnames(shocks) <- c("a_W_shock", "a_F_shock", "mc_W_shock", "mc_F_shock")
  
  return(shocks)
}


# Added by Kohei Kawaguchi ------------------------------------------------

compute_profit_owner <-
  function(
    x,
    index,
    price,
    mu,
    a,
    w_0,
    lambda,
    matching_params,
    spec,
    S,
    mc,
    rp
  ) {
    # ensures fee >= wage
    x[(length(x)/2 + 1):length(x)] <- x[(length(x)/2 + 1):length(x)] + x[1:(length(x)/2)]
    # insert the relevant price
    price[index, ] <- x
    # calculate the profit
    profit <- 
      compute_payoff_vec(
      price, 
      mu, 
      a, 
      mc,
      w_0, 
      S,
      lambda, 
      matching_params, 
      rp,
      spec
      )
    profit <- profit[index] %>% sum()
    return(profit)
  }

compute_best_response_owner <-
  function(
    index,
    price,
    mu,
    a,
    w_0,
    lambda,
    matching_params,
    spec,
    S,
    mc,
    rp
  ) {
    # set initial value
    x <- price[index, ] %>% as.numeric() 
    # ensures fee >= wage
    x[(length(x)/2 + 1):length(x)] <- x[(length(x)/2 + 1):length(x)] - x[1:(length(x)/2)]
    # optimize
    result <-
      optim(
        par = x,
        fn = compute_profit_owner_cpp,
        method = "L-BFGS-B",
        lower = rep(0, length(x)),
        upper = rep(100, length(x)),
        control = list(
          fnscale = -1,
          factr = 1e-10
        ),
        index = index,
        price = price,
        mu = mu,
        a = a,
        w_0 = w_0,
        lambda = lambda,
        matching_params = matching_params,
        spec = spec,
        S = S,
        mc = mc,
        rp = rp
      )
    x <- result$par
    # ensures fee >= wage
    x[(length(x)/2 + 1):length(x)] <- x[(length(x)/2 + 1):length(x)] + x[1:(length(x)/2)]
    # update price vector
    price[index, ] <- x
    return(price)
  }

solve_equilibrium_single_market_iteration_with_ownership_matrix <- 
  function(
    price_init,
    ownership,
    w_0,
    mu,
    a, 
    mc,
    S, 
    lambda,
    matching_params,
    spec,
    rp,
    cpp = TRUE
  ) {
    
    owner_product <- mgcv::uniquecombs(ownership)
    price <- price_init
    
    distance <- 100
    
    while (distance > 1e-10) {
      price_old <- price
      for (i in 1:nrow(owner_product)) {
        index <- owner_product[i, ] %>% as.logical() %>% which()
        price <-
          compute_best_response_owner(
            index,
            price,
            mu,
            a,
            w_0,
            lambda,
            matching_params,
            spec,
            S,
            mc,
            rp
          ) 
      }
      distance <- max(abs(price - price_old))
      print(distance)
    }
    
    H <- compute_aggregator(price, mu, a, w_0, lambda, matching_params, spec)
    endogenous_vars <-
      cbind(
        price,      
        compute_inside_share_vec_cpp(
          price, 
          mu, 
          a , 
          H, 
          lambda, 
          matching_params, 
          spec
        )
      )
    
    return(list(endogenous_vars = endogenous_vars))
  }

compute_payoff_vec <-
  function(
    price, 
    mu, 
    a, 
    mc,
    w_0, 
    S,
    lambda, 
    matching_params, 
    rp,
    spec
  ) {
    # calculate the profit
    H <-
      compute_aggregator_cpp(
        price, 
        mu, 
        a, 
        w_0, 
        lambda, 
        matching_params, 
        spec
      )
    s <-
      compute_inside_share_single_market_cpp(
        H, 
        price, 
        mu, 
        w_0, 
        a, 
        S, 
        lambda, 
        matching_params, 
        spec
      )
    profit <-
      compute_profit_vec_cpp(
        price, 
        s, 
        mu, 
        mc, 
        S, 
        matching_params, 
        rp
      )
    return(profit)
  }


construct_hypothetical_market_duopoly <- 
  function(
    equilibrium, 
    type = list(a_W = TRUE, a_F = FALSE, mc_W = FALSE, mc_F = FALSE),
    mc_manual = NULL
  ) {
    
    heterogeneity <- 
      equilibrium$platform_heterogeneity %>% 
      do.call("rbind", .) %>% 
      as.data.frame()
    
    x <- 
      equilibrium$exogenous %>% 
      do.call("rbind", .) %>% 
      as.data.frame() %>% 
      dplyr::select(id_unique, mu)
    
    mu <- x$mu %>% mean()
    
    if (type$a_W == TRUE) {
      a_W <- heterogeneity$predicted_a_W
      a_W <- mean(a_W) + c(sd(a_W), -sd(a_W))
    } else {
      a_W <- heterogeneity$predicted_a_W %>% mean()
    }
    
    if (type$a_F == TRUE) {
      a_F <- heterogeneity$predicted_a_F
      a_F <- mean(a_F) + c(sd(a_F), -sd(a_F))
    } else {
      a_F <- heterogeneity$predicted_mc_W %>% mean()
    }
    
    if (type$mc_W == TRUE) {
      mc_W <- heterogeneity$predicted_mc_W
      mc_W <- mean(mc_W) + c(sd(mc_W), -sd(mc_W))
    } else {
      if (is.null(mc_manual)) {
        mc_W <- 0.1
      } else {
        mc_W <- mc_manual[1]
      }
    }
    
    if (type$mc_F == TRUE) {
      mc_F <- heterogeneity$predicted_mc_F
      mc_F <- mean(mc_F) + c(sd(mc_F), -sd(mc_F))
    } else {
      if (is.null(mc_manual)) {
        mc_F <- 0.1
      } else {
        mc_F <- mc_manual[2]
      }
    }
    
    data <- 
      expand.grid(
        a_W = a_W, 
        a_F = a_F, 
        mc_W = mc_W, 
        mc_F = mc_F, 
        mu = mu
      )
    
    data_list <- list()
    data_list$a <- 
      data %>%
      dplyr::select(dplyr::starts_with("a_")) %>%
      as.matrix()
    data_list$mc <-     
      data %>%
      dplyr::select(dplyr::starts_with("mc_")) %>%
      as.matrix()
    data_list$mu <- 
      data %>%
      dplyr::select(mu) %>%
      as.matrix()
    data_list$S <- c(1, 1)
    data_list$w_0 <- 0.8
    data_list$id_unique <- 1:dim(data)[1]
    
    return(data_list)
  }
