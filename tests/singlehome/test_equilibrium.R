
# Initialize --------------------------------------------------------------

rm(list = ls())
gc()

library(magrittr)
library(Dispatching)
library(foreach)
library(doParallel)

registerDoParallel(10)


# Read data ---------------------------------------------------------------

share_data <- readRDS("output/share_data_small_markets.RDS")
matching_result <- readRDS("output/estimate_matching_function/matching_function_firmFE.RDS")
matching_params <- matching_result[[4]]$coefficients %>% as.vector()

result <- readRDS("output/estimate_structural_parameters/result_distance_IV.RDS")


# Prepare data ------------------------------------------------------------

share_data <- share_data %>% 
  dplyr::mutate(mu = exp(log(Q) - matching_params[1] * log(D_W) - matching_params[2] * log(D_F)))


x_names <- share_data %>% 
  dplyr::select(
    daily,
    shokai,
    oversea,
    cocurrent,
    dplyr::starts_with("type_")
  ) %>% 
  colnames()

iv_dist_each_names <- share_data %>%
  dplyr::select(
    dplyr::starts_with("d_mean_"),
    dplyr::starts_with("d_sd_")
  ) %>%
  colnames()


fe_names <- share_data %>% 
  dplyr::select(
    year,
    cz,
    firm_id
  ) %>% 
  colnames()

var_names <- list(
  x_names = x_names,
  iv_names = iv_dist_each_names,
  fe_names = fe_names
)

share_data <-
  share_data %>%
  dplyr::mutate(owner_id = firm_id)


# Test functions ----------------------------------------------------------

# Generate an instance
spec <- "linear"
equilibrium <- equilibriumClass$new(data = share_data, var_names = var_names, spec = spec)

data_list <-
  generate_data_list_market(
    dataframe = equilibrium$dataframe
  )


# Generate a list of matrix where each element of the list constitutes a single market
equilibrium$generate_data_list()


# Construct a list of ownership matrices using firm ID
equilibrium$construct_ownership_matrix()

# Generate a single dataframe from a list of market-level data matrix
data <- equilibrium$generate_dataframe()
head(data)

# Update parameters using estimation result
equilibrium$update_parameters_after_estimation(result)
print(equilibrium$parameters)

out <- compute_platform_heterogeneity(equilibrium)
equilibrium$shocks[[1]] <- out$residuals
equilibrium$parameters$fixed_effects <- out$fixed_effects
equilibrium$platform_heterogeneity <- out$predicted


# Generate shocks using empirical distribution of residuals
equilibrium$generate_shock_zeros_from_empirical_distribution(n_sim = 5, seed = 1)


# Extract only relevant information of a single market for equilibrium calculation
mkt_id <- 2
sim_id <- 1
equilibrium$extract_single_market(mkt_id, sim_id)




## Subset markets ---------------------------------------------------------
sim_id <- 1
cz_list <-
  equilibrium$dataframe$cz %>% 
  unique()
cz_list <-
  sample(
    cz_list,
    10
  )
market_list <-
  purrr::map(
    equilibrium$exogenous,
    ~ as.data.frame(.) %>%
      dplyr::filter(cz %in% cz_list) %>%
      nrow()
  ) %>%
  purrr::reduce(c)
market_list <- which(market_list > 0)
eq <- equilibrium$copy()
eq$endogenous <-
  eq$endogenous[market_list]
eq$exogenous <-
  eq$exogenous[market_list]
eq$shocks <-
  list(eq$shocks[[sim_id]][market_list])
eq$instruments <-
  eq$instruments[market_list]
eq$platform_heterogeneity <-
  eq$platform_heterogeneity[market_list]
eq$ownership <-
  eq$ownership[market_list]
eq$dataframe <-
  eq$generate_dataframe()

eq_2 <- 
  extract_cz(
    equilibrium,
    cz_list
    )

max(abs(eq$endogenous %>% unlist() - eq_2$endogenous %>% unlist()))
max(abs(eq$exogenous %>% unlist() - eq_2$exogenous %>% unlist()))
max(abs(eq$shocks %>% unlist() - eq_2$shocks %>% unlist()))
max(abs(eq$instruments %>% unlist() - eq_2$instruments %>% unlist()))
max(abs(eq$ownership %>% unlist() - eq_2$ownership %>% unlist()))
max(abs(eq$dataframe - eq_2$dataframe))


