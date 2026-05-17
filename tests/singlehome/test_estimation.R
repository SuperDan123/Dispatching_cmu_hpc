

# Initialize --------------------------------------------------------------

rm(list = ls())
gc()

devtools::load_all(".")
library(magrittr)
library(foreach)
library(doParallel)
library(codetools)
library(ggplot2)

registerDoParallel()
dir.create("output/test_estimation", showWarnings = FALSE)


# Read data ---------------------------------------------------------------

# data for estimation
data_establishment <- readRDS("output/data_establishments.RDS")
num_parttemp <- readRDS("output/num_parttemp_cz.RDS")
num_est <- readRDS("output/num_establishments_cz.RDS")
distance_iv <- readRDS("output/distance_iv.RDS")
distance_iv_each <- readRDS("output/distance_iv_each.RDS")
partwage_cz <- readRDS("output/partwage_cz.RDS")
mw <- readRDS("cleaned/data_pref_year_minimum_wage.RDS")
iv_selected <- readRDS("output/iv_selected.RDS")
hausman_iv <- readRDS("output/hausman_iv.RDS")
rivals_iv <- readRDS("output/num_rivals_iv.RDS")
matching_params <-
  readRDS("output/estimation_result/matching_function_selected_distance_iv.RDS")

# # data for test
# result <- readRDS(file = "output/test_estimation/result.rds")
# W_efficient <- readRDS(file = "output/test_estimation/W_efficient.rds")
# result_efficient <- readRDS(file = "output/test_estimation/result_efficient.rds")
# other_reduced <- readRDS(file = "output/test_estimation/other_reduced.rds")
# other <- readRDS(file = "output/test_estimation/other.rds")
# estimate <- readRDS(file = "output/test_estimation/estimate.rds")


# Set constants -----------------------------------------------------------

spec <- "linear"
lambda <- c(2, 2)
cpp <- TRUE
design_function <- make_design_matrix_foc
moment_function <- compute_gmm_moment_foc
i <- 1


# estimate by foc ---------------------------------------------------------

## prepare equilibrium for foc estimation ---------------------------------

data_estimation <-
  transform_data_estimation(
    data_establishment,
    num_parttemp,
    num_est,
    partwage_cz,
    mw,
    distance_iv,
    distance_iv_each,
    rivals_iv,
    hausman_iv,
    matching_params
  )

var_names <-
  make_names_estimation(
    data_estimation,
    iv_selected
  ) 

var_names$iv_names <- var_names$iv_dist_each_names

equilibrium <-
  make_equilibrium_estimation(
    data_estimation,
    var_names,
    matching_params,
    spec 
  )

## compute moments --------------------------------------------------------

partial_out <-
  partial_out_fe(
    vars = c("y_W", "y_F"),
    var_names = equilibrium$var_names,
    data_estimation = equilibrium$dataframe
  )
# partial_out_lm <-
#   partial_out_fe_lm(
#     vars = c("y_W", "y_F"),
#     var_names = equilibrium$var_names,
#     data_estimation = equilibrium$dataframe
#   )
# max(abs(partial_out - partial_out_lm))

# estimate_fixed(
#   vars = c("y_W"),
#   var_names = var_names,
#   data_estimation = equilibrium$dataframe
# ) 

exogenous_matrix <-
  make_exogenous_matrix(
    equilibrium = equilibrium
  ) 

W <- diag(4 * ncol(exogenous_matrix$z))

design_matrix <-
  make_design_matrix_foc(
    exogenous_matrix,
    W
  ) 

data_estimation <-
  make_y_demand_raw(
    lambda = lambda,
    equilibrium = equilibrium
  ) 

y_demand <-
  make_y_demand(
    lambda = lambda,
    equilibrium = equilibrium
  ) 

derivatives <-
  compute_derivatives(
    q = equilibrium$endogenous[[i]][, "Q", drop = FALSE],
    price = equilibrium$endogenous[[i]][, c("wage", "fee"), drop = FALSE],
    s = equilibrium$endogenous[[i]][, c("s_W", "s_F"), drop = FALSE], 
    S = equilibrium$exogenous[[i]][1, c("S_W", "S_F")], 
    lambda = lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec
  ) 
derivatives_cpp <-
  compute_derivatives_cpp(
    q = equilibrium$endogenous[[i]][, "Q", drop = FALSE],
    price = equilibrium$endogenous[[i]][, c("wage", "fee"), drop = FALSE],
    s = equilibrium$endogenous[[i]][, c("s_W", "s_F"), drop = FALSE], 
    S = equilibrium$exogenous[[i]][1, c("S_W", "S_F")], 
    lambda = lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec
  ) 
max(abs(unlist(derivatives) - unlist(derivatives_cpp)))

mc <-
  compute_marginal_cost_for_each_market(
    q = equilibrium$endogenous[[i]][, "Q", drop = FALSE], 
    ownership = equilibrium$ownership[[i]], 
    price = equilibrium$endogenous[[i]][, c("wage", "fee"), drop = FALSE], 
    s = equilibrium$endogenous[[i]][, c("s_W", "s_F"), drop = FALSE], 
    S = equilibrium$exogenous[[i]][1, c("S_W", "S_F")], 
    lambda = lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec, 
    rp = equilibrium$parameters$rp
  ) 
mc_cpp <-
  compute_marginal_cost_for_each_market_cpp(
    q = equilibrium$endogenous[[i]][, "Q", drop = FALSE], 
    ownership = equilibrium$ownership[[i]], 
    price = equilibrium$endogenous[[i]][, c("wage", "fee"), drop = FALSE], 
    s = equilibrium$endogenous[[i]][, c("s_W", "s_F"), drop = FALSE], 
    S = equilibrium$exogenous[[i]][1, c("S_W", "S_F")], 
    lambda = lambda, 
    matching_params = equilibrium$parameters$matching_params, 
    spec = equilibrium$spec, 
    rp = equilibrium$parameters$rp
  ) 
max(abs(mc - mc_cpp))

mc <-
  compute_marginal_cost_for_estimation(
    ownership = equilibrium$ownership,
    exogenous = equilibrium$exogenous,
    endogenous = equilibrium$endogenous,
    lambda = lambda,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec,
    rp = equilibrium$parameters$rp, 
    cpp = cpp
  ) 


data_estimation <-
  make_y_supply_raw(
    lambda = lambda,
    equilibrium = equilibrium
  ) 

y_supply <-
  make_y_supply(
    lambda = lambda,
    equilibrium = equilibrium
  ) 

residual <-
  compute_residual_foc(
    y_demand = y_demand,
    y_supply = y_supply,
    design_matrix = design_matrix
  ) 

moment <-
  compute_gmm_moment_foc(
    lambda = lambda,
    equilibrium = equilibrium,
    design_matrix = design_matrix
  ) 


## estimate the first stage -----------------------------------------------

objective <-
  compute_gmm_objective_generic(
    theta = lambda,
    equilibrium = equilibrium,
    design_matrix = design_matrix,
    moment_function = compute_gmm_moment_foc
  ) 

result <-
  estimate_gmm_nonlinear_generic(
    theta = lambda,
    equilibrium = equilibrium,
    W = W,
    design_function = make_design_matrix_foc,
    moment_function = compute_gmm_moment_foc
  ) 
saveRDS(result, file = "output/test_estimation/result.rds")

result


## estimate the second stage ----------------------------------------------

W_efficient <-
  compute_efficient_weight_foc(
    lambda = result$par,
    equilibrium = equilibrium,
    W = W
  )
saveRDS(W_efficient, file = "output/test_estimation/W_efficient.rds")

result_efficient <-
  estimate_gmm_nonlinear_generic(
    theta = result$par,
    equilibrium = equilibrium,
    W = W_efficient,
    design_function = make_design_matrix_foc,
    moment_function = compute_gmm_moment_foc
  ) 
saveRDS(result_efficient, file = "output/test_estimation/result_efficient.rds")

result_efficient$par
debug_result$params_nonlinear


## estimaqte other parameters ----------------------------------------------

fml <-
  make_formula_other_reduced(
    var_names = equilibrium$var_names,
    data_estimation = equilibrium$dataframe
  )

other_reduced <-
  estimate_other_reduced(
    vars = c("y_W"),
    var_names = equilibrium$var_names,
    data_estimation = equilibrium$dataframe
  ) 

other_reduced <-
  estiamte_gmm_other_reduced_foc(
    lambda = result_efficient$par,
    equilibrium = equilibrium
  ) 
saveRDS(other_reduced, file = "output/test_estimation/other_reduced.rds")

other <-
  other_reduced %>%
  purrr::map(
    .,
    ~ convert_reducecd_to_structural(
      reduced = .,
      lambda = result_efficient$par, 
      matching_params = equilibrium$parameters$matching_params
    ) 
  )
saveRDS(other, file = "output/test_estimation/other.rds")

check <-
  other %>%
  purrr::map(
    .,
    ~ convert_structural_to_reduced(
      structural = .,
      lambda = result_efficient$par, 
      matching_params = equilibrium$parameters$matching_params
    ) 
  )

# make matrix
exogenous_matrix <-
  make_exogenous_matrix(
    equilibrium = equilibrium
  ) 

design_matrix <-
  make_design_matrix_foc(
    exogenous_matrix = exogenous_matrix,
    W = W_efficient
  ) 

se_nonlinear <-
  estimate_gmm_se_nonlinear_efficient_foc(
    lambda = result_efficient$par,
    equilibrium = equilibrium,
    design_matrix = design_matrix
  ) 

# make output
estimate_nonlinear <-
  result_efficient$par %>%
  matrix(nrow = 1) %>%
  magrittr::set_colnames(c("lambda_W", "lambda_F"))

# return
estimate <-
  list(
    result = result,
    result_efficient = result_efficient,
    estimate_nonlinear = estimate_nonlinear,
    se_nonlinear = se_nonlinear,
    other = other,
    other_reduced = other_reduced
  )

estimate <-
  estimate_gmm_foc(
    lambda = lambda,
    equilibrium = equilibrium
  ) 
saveRDS(estimate, file = "output/test_estimation/estimate.rds")


## update equilibrium -----------------------------------------------------

# parameter
equilibrium$parameters$lambda <- estimate$estimate_nonlinear 
equilibrium$parameters$linear <- estimate$other$coefficient

# standard error
equilibrium$parameters_se$lambda <- estimate$se_nonlinear
equilibrium$parameters_se$linear <- estimate$other$se

# residual
equilibrium$dataframe <-
  equilibrium$dataframe %>%
  dplyr::select(-dplyr::contains(c("residual_a_", "residual_mc_"))) %>%
  dplyr::left_join(
    estimate$other$residual,
    by = "id_unique"
  )

# predicted
equilibrium$dataframe <-
  equilibrium$dataframe %>%
  dplyr::select(-dplyr::contains(c("predicted_a_", "predicted_mc_"))) %>%
  dplyr::left_join(
    estimate$other$predicted,
    by = "id_unique"
  )

equilibrium$generate_data_list()

equilibrium$shocks[[1]]
equilibrium$platform_heterogeneity

equilibrium_updated <-
  update_equilibrium_by_estimate(
    equilibrium,
    estimate
  )

## train model -------------------------------------------------------------

equilibrium_updated <-
  train_model_foc(
    lambda = lambda,
    data_estimation = equilibrium$dataframe,
    var_names = equilibrium$var_names,
    matching_params = equilibrium$parameters$matching_params,
    spec = equilibrium$spec
  ) 
saveRDS(equilibrium_updated, file = "output/test_estimation/equilibrium_updated.rds")




# what if single product? -------------------------------------------------

equilibrium_singleproduct <- equilibrium$copy()
equilibrium_singleproduct$dataframe <-
  equilibrium_singleproduct$dataframe %>%
  dplyr::mutate(
    owner_id = id_unique
  )

equilibrium_singleproduct <-
  train_model_foc(
    lambda = lambda,
    data_estimation = equilibrium_singleproduct$dataframe,
    var_names = equilibrium_singleproduct$var_names,
    matching_params = equilibrium_singleproduct$parameters$matching_params,
    spec = equilibrium_singleproduct$spec
  ) 

# add mc nonnegativity ----------------------------------------------------

## compute gradiet, constraint, and jacobian ------------------------------

gradient <-
  compute_gmm_objective_gradient_generic(
    theta = lambda,
    equilibrium = equilibrium,
    design_matrix = design_matrix,
    moment_function = compute_gmm_moment_foc
  ) 

constraint <-
  compute_gmm_constraint_mc_nonnegativity(
    theta = lambda,
    equilibrium = equilibrium,
    design_matrix = design_matrix,
    moment_function = compute_gmm_moment_foc
  ) 

jacobian <-
  compute_gmm_constraint_mc_nonnegativity_jacobian(
    theta = lambda,
    equilibrium,
    design_matrix = design_matrix,
    moment_function = compute_gmm_moment_foc
  ) 


## estiamte the first stage -----------------------------------------------

result_mc_nonnegativity <-
  estimate_gmm_nonlinear_mc_nonnegativity(
    theta = lambda,
    equilibrium = equilibrium,
    W = W,
    design_function = make_design_matrix_foc,
    moment_function = compute_gmm_moment_foc
  ) 


# check consistency -------------------------------------------------------

beta <- 
  convert_parameters_from_structural_to_reduced(
    lambda = equilibrium_updated$parameters$lambda, 
    matching_params = equilibrium_updated$parameters$matching_params
  )
beta_DW <- beta[1]
beta_SW <- beta[2]
beta_DF <- beta[3]
beta_SF <- beta[4]

B <- matrix(beta, nrow = 2)


## check partial out ------------------------------------------------------

check_1 <-
  partial_out_fe(
    vars = c("y_W", "y_F"),
    var_names = equilibrium_updated$var_names,
    data_estimation = equilibrium_updated$dataframe
  )
check_2 <-
  partial_out_fe_lm(
    vars = c("y_W", "y_F"),
    var_names = equilibrium_updated$var_names,
    data_estimation = equilibrium_updated$dataframe
  )
max(abs(check_1 - check_2))


## check consistency a_W and a_F ------------------------------------------

check <-
  equilibrium_updated$dataframe 
check <-  
  check %>%
  dplyr::mutate(
    a_W = predicted_a_W + residual_a_W,
    a_F = predicted_a_F + residual_a_F,
    y_W_fit = beta_DW * wage_net - beta_DF * fee_net + beta_DW * a_W + beta_DF * a_F,
    y_F_fit = - beta_SF * fee_net + beta_SW * wage_net + beta_SF * a_F + beta_SW * a_W
  )
summary(check$y_W - check$y_W_fit)
summary(check$y_F - check$y_F_fit)



# estimate only demand parameters -----------------------------------------

data_estimation_sub <-
  data_estimation %>%
  dplyr::group_by(
    cz,
    year 
  ) %>%
  dplyr::mutate(
    N = length(firm_id)
  ) %>%
  dplyr::ungroup() 


data_estimation_sub %>%
  ggplot(
    aes(
      x = D_W,
      y = s_W_adjusted
    )
  ) +
  geom_point()

data_estimation_sub %>%
  ggplot(
    aes(
      x = wage,
      y = D_W
    )
  ) +
  geom_point()

data_estimation_sub %>%
  ggplot(
    aes(
      x = wage,
      y = s_W_adjusted
    )
  ) +
  geom_point()

data_estimation_sub %>%
  ggplot(
    aes(
      x = wage,
      y = y_W
    )
  ) +
  geom_point()

fixest::feols(
  data = data_estimation_sub,
  fml = y_W ~ wage | cz + firm_id + year
  ) %>%
  summary()

fixest::feols(
  data = data_estimation_sub,
  fml = y_F ~ fee | cz + firm_id + year
  ) %>%
  summary()

var_names$iv_names <- var_names$iv_dist_each_names

equilibrium <-
  make_equilibrium_estimation(
    data_estimation = data_estimation_sub,
    var_names,
    matching_params,
    spec 
  )

exogenous_matrix <-
  make_exogenous_matrix(
    equilibrium = equilibrium
  ) 

W <- diag(2 * ncol(exogenous_matrix$z))

design_matrix <-
  make_design_matrix_foc(
    exogenous_matrix,
    W
  ) 

y_demand <-
  make_y_demand(
    lambda = lambda,
    equilibrium = equilibrium
  ) 

residual <-
  compute_residual_demand_foc(
    y_demand = y_demand,
    design_matrix = design_matrix
  ) 

moment <-
  compute_gmm_moment_demand_foc(
    lambda = lambda,
    equilibrium = equilibrium,
    design_matrix = design_matrix
  )

objective <-
  compute_gmm_objective_generic(
    theta = lambda,
    equilibrium = equilibrium,
    design_matrix = design_matrix,
    moment_function = compute_gmm_moment_demand_foc
  ) 

equilibrium$spec <- "linear"
result <-
  estimate_gmm_nonlinear_generic(
    theta = lambda,
    equilibrium = equilibrium,
    W = W,
    design_function = make_design_matrix_foc,
    moment_function = compute_gmm_moment_demand_foc
  ) 
result

equilibrium$spec <- "log"
result <-
  estimate_gmm_nonlinear_generic(
    theta = lambda,
    equilibrium = equilibrium,
    W = W,
    design_function = make_design_matrix_foc,
    moment_function = compute_gmm_moment_demand_foc
  ) 
result

equilibrium$spec <- "log-linear"
result <-
  estimate_gmm_nonlinear_generic(
    theta = lambda,
    equilibrium = equilibrium,
    W = W,
    design_function = make_design_matrix_foc,
    moment_function = compute_gmm_moment_demand_foc
  ) 
result

equilibrium$spec <- "linear"

# estimate cost parameters by smm -----------------------------------------



exogenous_matrix <-
  make_exogenous_matrix(
    equilibrium = equilibrium
  ) 




# a
data_estimation <-
  make_y_demand_raw(
    lambda = lambda,
    equilibrium = equilibrium
  ) 

a <-
  convert_reducecd_to_structural(
    reduced = equilibrium$dataframe %>%
      dplyr::select(
        "predicted_a_W" = "y_W",
        "predicted_a_F" = "y_F"
      ),
    lambda = lambda,
    matching_params = equilibrium$parameters$matching_params
  )

# mc




out_cpp <-
  solve_equilibrium_all(
    equilibrium = equilibrium,
    N_max = Inf,
    parallel = FALSE,
    cpp = TRUE,
    sim_id = 1
  )

