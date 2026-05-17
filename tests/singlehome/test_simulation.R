rm(list = ls())
gc()

devtools::load_all(".")
library(magrittr)
library(foreach)
library(codetools)
library(doParallel)

registerDoParallel(10)


# Load data ---------------------------------------------------------------

equilibrium <-
  readRDS("output/test_estimation/equilibrium_updated.rds")

# set constant ------------------------------------------------------------
hypothetical <- TRUE
cpp <- TRUE
mc_manual <- c(0.01, 0.01)
i <- 1

# # make hypothetical equilibrium to test -----------------------------------
# if (hypothetical) {
#   equilibrium <-
#     construct_hypothetical_equilibrium(
#       equilibrium,
#       cpp,
#       mc_manual
#     )
# }

# Test functions for computing equilibrium --------------------------------

# Pick a single market that is not too large
data_i <- equilibrium$extract_single_market(i, 1)

# Compute reduced form parameters from structural parameters
beta <-
  convert_parameters_from_structural_to_reduced(
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params
  )
beta_cpp <-
  convert_parameters_from_structural_to_reduced_cpp(
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params
  )
max(abs(beta - beta_cpp))

# compute mu_tilde
mu_tilde <-
  compute_mu_tilde(
    mu = data_i$mu,
    matching_params = equilibrium$parameters$matching_params
  )
mu_tilde_cpp <-
  compute_mu_tilde_cpp(
    mu = data_i$mu,
    matching_params = equilibrium$parameters$matching_params
  )
max(abs(mu_tilde - mu_tilde_cpp))

# Compute initial values for aggregator
H <-
  compute_aggregator(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = "linear"
  )
H_cpp <-
  compute_aggregator_cpp(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = "linear"
  )
max(abs(H - H_cpp))

H <-
  compute_aggregator(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = "log"
  )
H_cpp <-
  compute_aggregator_cpp(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = "log"
  )
max(abs(H - H_cpp))
H <-
  compute_aggregator(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = "log-linear"
  )
H_cpp <-
  compute_aggregator_cpp(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = "log-linear"
  )
max(abs(H - H_cpp))

H <-
  compute_aggregator(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$lambda,
    spec = equilibrium$spec
  )

## Test functions: solving FOC ---------------------------------------------

### Compute inside shares given aggregator ---------------------------------
share_vector <-
  compute_inside_share_vec(
    price = data_i$price[i, , drop = FALSE], 
    mu = data_i$mu[i], 
    a = data_i$a[i, , drop = FALSE], 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec
  )
share_vector_cpp <-
  compute_inside_share_vec_cpp(
    price = data_i$price[i, , drop = FALSE], 
    mu = data_i$mu[i], 
    a = data_i$a[i, , drop = FALSE], 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec
  )

share <-
  compute_inside_share_vec(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "linear"
  )
share_cpp <-
  compute_inside_share_vec_cpp(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "linear"
  )
max(abs(share - share_cpp))
share <-
  compute_inside_share_vec(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log"
  )
share_cpp <-
  compute_inside_share_vec_cpp(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log"
  )
max(abs(share - share_cpp))
share <-
  compute_inside_share_vec(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log-linear"
  )
share_cpp <-
  compute_inside_share_vec_cpp(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log-linear"
  )
max(abs(share - share_cpp))

share <-
  compute_inside_share_vec(
    price = data_i$price,
    mu = data_i$mu,
    a = data_i$a,
    H = H,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec
  )


### Compute outside shares ------------------------------------------------

share_out <-
  compute_outside_share(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "linear"
  )
share_out_cpp <-
  compute_outside_share_cpp(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "linear"
  )
max(abs(share_out - share_out_cpp))
share_out <-
  compute_outside_share(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log"
  )
share_out_cpp <-
  compute_outside_share_cpp(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log"
  )
max(abs(share_out - share_out_cpp))
share_out <-
  compute_outside_share(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log-linear"
  )
share_out_cpp <-
  compute_outside_share_cpp(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log-linear"
  )
max(abs(share_out - share_out_cpp))

share_out <-
  compute_outside_share(
    w_0 = data_i$w_0, 
    H = H, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec
  )

### Compute profit --------------------------------------------------------
q <-
  compute_match(
    mu = data_i$mu,
    s = share,
    S = data_i$S,
    matching_params = equilibrium$parameters$matching_params
  ) 
q_cpp <-
  compute_match_cpp(
    mu = data_i$mu,
    s = share,
    S = data_i$S,
    matching_params = equilibrium$parameters$matching_params
  ) 
max(abs(q - q_cpp))

profit <-
  compute_profit_vec(
    price = data_i$price, 
    s = share, 
    mu = data_i$mu, 
    mc = data_i$mc, 
    S = data_i$S, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp
  )
profit_cpp <-
  compute_profit_vec_cpp(
    price = data_i$price, 
    s = share, 
    mu = data_i$mu, 
    mc = data_i$mc, 
    S = data_i$S, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp
  )
max(abs(profit - profit_cpp))

## Test functions: equilibrium for a single market ------------------------

#### Compute FOC error with ownership matrix ------------------------------

derivatives <-
  compute_derivatives(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "linear"
  ) 
derivatives_cpp <-
  compute_derivatives_cpp(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "linear"
  ) 
max(abs(unlist(derivatives) - unlist(derivatives_cpp)))
derivatives <-
  compute_derivatives(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log"
  ) 
derivatives_cpp <-
  compute_derivatives_cpp(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log"
  ) 
max(abs(unlist(derivatives) - unlist(derivatives_cpp)))
derivatives <-
  compute_derivatives(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log-linear"
  ) 
derivatives_cpp <-
  compute_derivatives_cpp(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = "log-linear"
  ) 
max(abs(unlist(derivatives) - unlist(derivatives_cpp)))


# check with numerical derivatives

derivatives_numerical <-
  compute_derivatives_numerical(
    price = data_i$price,
    S = data_i$S,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec
    ) 

derivatives <-
  compute_derivatives(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec
  ) 

plot(
  derivatives$dq_dw,
  derivatives_numerical$dq_dw
)
plot(
  derivatives$dq_df,
  derivatives_numerical$dq_df
)
plot(
  derivatives$dDw_dw,
  derivatives_numerical$dDw_dw
)
plot(
  derivatives$dDw_df,
  derivatives_numerical$dDw_df
)
plot(
  derivatives$dDf_dw,
  derivatives_numerical$dDf_dw
)
plot(
  derivatives$dDf_df,
  derivatives_numerical$dDf_df
)

derivatives$dDw_df[abs(derivatives$dDw_df - derivatives_numerical$dDw_df) > 0.01]

index <- which(abs(derivatives$dDw_df - derivatives_numerical$dDw_df) > 0.01, arr.ind = TRUE)

derivatives <-
  compute_derivatives(
    q = q,
    price = data_i$price,
    s = share,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec
  ) 

out <-
  compute_foc_error_with_ownership_matrix(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "linear", 
    rp = equilibrium$parameters$rp
  )
out_cpp <-
  compute_foc_error_with_ownership_matrix_cpp(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "linear", 
    rp = equilibrium$parameters$rp
  )
max(abs(out - out_cpp))
out <-
  compute_foc_error_with_ownership_matrix(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log", 
    rp = equilibrium$parameters$rp
  )
out_cpp <-
  compute_foc_error_with_ownership_matrix_cpp(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log", 
    rp = equilibrium$parameters$rp
  )
max(abs(out - out_cpp))
out <-
  compute_foc_error_with_ownership_matrix(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log-linear", 
    rp = equilibrium$parameters$rp
  )
out_cpp <-
  compute_foc_error_with_ownership_matrix_cpp(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = "log-linear", 
    rp = equilibrium$parameters$rp
  )
max(abs(out - out_cpp))

out <-
  compute_foc_error_with_ownership_matrix(
    x = data_i$price %>% as.vector(), 
    ownership = equilibrium$ownership[[i]], 
    w_0 = data_i$w_0, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc, 
    S = data_i$S, 
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec, 
    rp = equilibrium$parameters$rp
  )

### Solve foc -------------------------------------------------------------

# with ownership matrix
out <-
  solve_equilibrium_single_market_with_ownership_matrix(
    price_init = data_i$price * 0.9,
    ownership = equilibrium$ownership[[i]],
    w_0 = data_i$w_0,
    mu = data_i$mu,
    a = data_i$a,
    mc = data_i$mc,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    rp = equilibrium$parameters$rp,
    cpp = FALSE
  )
out_cpp <-
  solve_equilibrium_single_market_with_ownership_matrix(
    price_init = data_i$price * 0.9,
    ownership = equilibrium$ownership[[i]],
    w_0 = data_i$w_0,
    mu = data_i$mu,
    a = data_i$a,
    mc = data_i$mc,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    rp = equilibrium$parameters$rp,
    cpp = TRUE
  )
max(abs(out$endogenous_vars - out_cpp$endogenous_vars))
out_foc <- out

out_2 <-
  solve_equilibrium_single_market_with_ownership_matrix_using_minimization(
    price_init = data_i$price * 0.9,
    ownership = equilibrium$ownership[[i]],
    w_0 = data_i$w_0,
    mu = data_i$mu,
    a = data_i$a,
    mc = data_i$mc,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    rp = equilibrium$parameters$rp
  )
max(abs(out$endogenous_vars - out_2$endogenous_vars))

out <-
  solve_equilibrium_single_market_wrapper(
    data = data_i, 
    ownership = equilibrium$ownership[[i]], 
    parameters = equilibrium$parameters, 
    spec = equilibrium$spec, 
    cpp = FALSE
  )
out_cpp <-
  solve_equilibrium_single_market_wrapper(
    data = data_i, 
    ownership = equilibrium$ownership[[i]], 
    parameters = equilibrium$parameters, 
    spec = equilibrium$spec,
    cpp = TRUE
  )
max(abs(out[, 2:5] - out_cpp[, 2:5]))

### Solve by best response iteration --------------------------------------

owner_product <- mgcv::uniquecombs(equilibrium$ownership[[i]])
index <- owner_product[1,] %>% as.logical() %>% which()

price <- data_i$price
x <- price[index,] %>% as.numeric()

check <- price
check[index,] <- x
price - check

x[(length(x) / 2 + 1):length(x)] <-
  x[(length(x) / 2 + 1):length(x)] - x[1:(length(x) / 2)]

check <-
  compute_profit_owner(
    x = x,
    index = index,
    price = data_i$price * 0.9,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    S = data_i$S,
    mc = data_i$mc,
    rp = equilibrium$parameters$rp
  )
check_cpp <-
  compute_profit_owner_cpp(
    x = x,
    index = index,
    price = data_i$price * 0.9,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    S = data_i$S,
    mc = data_i$mc,
    rp = equilibrium$parameters$rp
  )
max(abs(check - check_cpp))

profit <-
  compute_payoff_vec(
    price = data_i$price, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc,
    w_0 = data_i$w_0, 
    S = data_i$S,
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp,
    spec = equilibrium$spec
  )
profit_cpp <-
  compute_payoff_vec_cpp(
    price = data_i$price, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc,
    w_0 = data_i$w_0, 
    S = data_i$S,
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp,
    spec = equilibrium$spec
  )
max(abs(profit - profit_cpp))

compute_best_response_owner(
  index = index,
  price = data_i$price * 0.9,
  mu = data_i$mu,
  a = data_i$a,
  w_0 = data_i$w_0,
  lambda = equilibrium$parameters$lambda,
  matching_params = equilibrium$parameters$matching_params,
  spec = equilibrium$spec,
  S = data_i$S,
  mc = data_i$mc,
  rp = equilibrium$parameters$rp
)

solve_equilibrium_single_market_iteration_with_ownership_matrix(
  price_init = data_i$price * 0.9,
  ownership = equilibrium$ownership[[i]],
  w_0 = data_i$w_0,
  mu = data_i$mu,
  a = data_i$a,
  mc = data_i$mc,
  S = data_i$S,
  lambda = equilibrium$parameters$lambda,
  matching_params = equilibrium$parameters$matching_params,
  spec = equilibrium$spec,
  rp = equilibrium$parameters$rp,
  cpp = TRUE
)

# check whether the solution of solve_equilibrium_single_market_with_ownership_matrix is a best response
index <- owner_product[1,] %>% as.logical() %>% which()

# set price under foc
price_foc <- out_foc$endogenous_vars[, 1:2]
price <- price_foc
# calculate the profit
profit <-
  compute_payoff_vec(
    price = price, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc,
    w_0 = data_i$w_0, 
    S = data_i$S,
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp,
    spec = equilibrium$spec
  )
profit_foc <- profit
# get the best response price
price_best <-
  compute_best_response_owner(
    index = index,
    price = price,
    mu = data_i$mu,
    a = data_i$a,
    w_0 = data_i$w_0,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    S = data_i$S,
    mc = data_i$mc,
    rp = equilibrium$parameters$rp
  )
price <- price_best
# calculate the profit
profit <-
  compute_payoff_vec(
    price = price, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc,
    w_0 = data_i$w_0, 
    S = data_i$S,
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp,
    spec = equilibrium$spec
  )
profit_best <- profit

max(abs(price_best - price_foc))
max(abs(profit_best - profit_foc))

# get the equilibrium price
out <-
  solve_equilibrium_single_market_iteration_with_ownership_matrix(
    price_init = data_i$price * 0.9,
    ownership = equilibrium$ownership[[i]],
    w_0 = data_i$w_0,
    mu = data_i$mu,
    a = data_i$a,
    mc = data_i$mc,
    S = data_i$S,
    lambda = equilibrium$parameters$lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    rp = equilibrium$parameters$rp,
    cpp = TRUE
  )
price_equilibrium <- out$endogenous_vars[, 1:2]
price <- price_equilibrium
# calculate the profit
profit <-
  compute_payoff_vec(
    price = price, 
    mu = data_i$mu, 
    a = data_i$a, 
    mc = data_i$mc,
    w_0 = data_i$w_0, 
    S = data_i$S,
    lambda = equilibrium$parameters$lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    rp = equilibrium$parameters$rp,
    spec = equilibrium$spec
  )
profit_equilibrium <- profit

max(abs(price_equilibrium - price_foc))
max(abs(profit_equilibrium - profit_foc))

## Test functions: equilibrium for all markets ----------------------------
out <- 
  solve_equilibrium_all(
    equilibrium,
    N_max = Inf,
    parallel = FALSE,
    cpp = FALSE,
    sim_id = 1
  )
out_cpp <-
  solve_equilibrium_all(
    equilibrium,
    N_max = Inf,
    parallel = FALSE,
    cpp = TRUE,
    sim_id = 1
  )
max(abs(unlist(out$endogenous) - unlist(out_cpp$endogenous)))

