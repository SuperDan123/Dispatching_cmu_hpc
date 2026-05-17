rm(list = ls())
gc()

library(Dispatching)
library(magrittr)
library(foreach)
library(doParallel)
library(codetools)

registerDoParallel(2)

# Load data ---------------------------------------------------------------

equilibrium <- readRDS("output/estimate_structural_parameters/equilibrium_data_distance_IV.RDS")

parameters <- equilibrium$parameters
lambda <- parameters$lambda
matching_params <- parameters$matching_params
rp <- parameters$rp
spec <- equilibrium$spec


# Set constants -----------------------------------------------------------
hypothetical <- TRUE
N_max <- 10
N_copy <- 2
parallel <- TRUE
cpp <- TRUE
sim_id = 1
opts <- list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  maxeval = 1e+6,
  xtol_rel = 1e-8
) 
mc_manual <- c(0.01, 0.01)
margin_upper_bound <- 0.3

# make hypothetical equilibrium to test -----------------------------------
if (hypothetical) {
  equilibrium <-
    construct_hypothetical_equilibrium(
      equilibrium,
      cpp,
      mc_manual
    )
}


# Test functions for counterfactuals --------------------------------------

# Pick a single market that is not too large
i <- 1
data_i <- equilibrium$extract_single_market(i, 1)
ownership_i <- equilibrium$ownership[[i]]

H <- compute_aggregator(data_i$price, data_i$mu, data_i$a, data_i$w_0, lambda, matching_params, spec)

# Extract data for a single platform
j <- 1       
price_j <- data_i$price[j, ]
mu_j <- data_i$mu[j]
a_j <- data_i$a[j, ]
mc_j <- data_i$mc[j, ]
w_0 <- data_i$w_0
S <- data_i$S

s_j <- compute_inside_share(price_j, mu_j, a_j, H, lambda, matching_params, spec)



## Compute surplus ------------------------------------------------------

surplus <- 
  compute_surplus_single_market(
    data = data_i, 
    parameters = equilibrium$parameters, 
    spec = equilibrium$spec
    )
profit <- 
  compute_total_profit_single_market(
    data = data_i, 
    parameters = equilibrium$parameters, 
    spec = equilibrium$spec
  )
surplus <- compute_surplus(equilibrium)

## Construct hypothetical markets ---------------------------------------

data <- construct_hypothetical_market_nonrandom(equilibrium)
eq <- 
  construct_hypothetical_equilibrium(
    equilibrium,
    cpp
  ) 

# check that the result of extract_single_market coincides with construct_hypothetical_market_nonrandom
data_1 <- eq$extract_single_market(1)
data_2 <- construct_hypothetical_market_nonrandom(equilibrium)
max(abs(unlist(data_1$a) - unlist(data_2$a)))
max(abs(unlist(data_1$mc) - unlist(data_2$mc)))
max(abs(data_1$mu - data_2$mu))
max(abs(data_1$S - data_2$S))
max(abs(data_1$w_0 - data_2$w_0))
max(abs(data_1$id_unique - data_2$id_unique))


## Competition -----------------------------------------------------------


### make monopoly data ----------------------------------------------------
equilibrium_monopoly <-
  compute_monopoly_equilibrium(
    equilibrium = equilibrium,
    N_max = N_max,
    parallel = parallel,
    cpp = cpp,
    sim_id = sim_id
  ) 
check <-
  equilibrium_monopoly$endogenous %>%
  purrr::keep(~ dim(.)[1] <= N_max)
check[[1]]

# make n copy equilibrium -------------------------------------------------

equilibrium_n_copy <-
  compute_n_copy_equilibrium(
      equilibrium = equilibrium,
      N_copy = N_copy,
      N_max = N_max,
      parallel = parallel,
      cpp = cpp,
      sim_id = 1
    ) 
check <-
  equilibrium_n_copy$endogenous %>%
  purrr::keep(~ dim(.)[1] <= N_max)
check[[1]]

