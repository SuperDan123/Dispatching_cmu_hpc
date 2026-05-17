# initialize ------------------------------------------------------------
rm(list = ls())
library(Dispatching)
library(foreach)
library(magrittr)
library(ggplot2)
library(doParallel)
registerDoParallel(
  detectCores() 
)
print(detectCores())

# set constant ----------------------------------------------------------
seed <- 1
set.seed(seed)
n <- 100
num_ths_max <- 15
prefix <- "output/multihome/estimate_data_initial"
dir.create(
  prefix,
  recursive = TRUE,
  showWarnings = FALSE
)

# load data -------------------------------------------------------------
equilibrium <- 
  readRDS(
    "output/multihome/make_equilibrium_zipcode_firm_from_data/equilibrium.rds"
  )

# sample data --------------------------------------------------------
equilibrium <- 
  sample_data_zip_code(
    n = n,
    seed = seed,
    num_ths_max = num_ths_max,
    equilibrium = equilibrium
  )

# check variation---------------------------------------------------------

check_data_variation(
  equilibrium = equilibrium,
  target = "x_a_w"
)

check_data_variation(
  equilibrium = equilibrium,
  target = "x_a_f"
)

check_data_variation(
  equilibrium = equilibrium,
  target = "x_c_w"
)

check_data_variation(
  equilibrium = equilibrium,
  target = "x_c_f"
)

# add intial parameter ---------------------------------------------------------
equilibrium$constant$use_exp <- "FALSE"


theta <- 
  transform_all_parameter_to_theta_optim(
    mu_p = equilibrium$parameter$mu_p,
    mu_ths = equilibrium$parameter$mu_ths,
    m_f = equilibrium$parameter$m_f,
    m_w = equilibrium$parameter$m_w,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f
  )

parameter <-
  transform_theta_to_all_parameter_demand(
    theta = theta,
    parameter = equilibrium$parameter
  )
# estimate parameters -------------------------------------------------

## make demand instruments -------------------------------------

instrument_demand <- 
  make_instrument_demand(
    equilibrium = equilibrium
  )

## make demand weighting matrix ---------------------------------

weighting_matrix_demand <- 
  compute_demand_weighting_matrix(
    instrument_demand = instrument_demand
  )

## estimate the model ------------------------------------------

print(
  system.time(
    compute_demand_objective_with_penalty(
      instrument_demand = instrument_demand,
      weighting_matrix_demand = weighting_matrix_demand,
      equilibrium = equilibrium
    )
  )
)

equilibrium_updated_with_penalty <- 
  estimate_parameter_with_penalty(
    equilibrium = equilibrium
  )


saveRDS(
  equilibrium_updated_with_penalty,
  paste0(
    prefix,
    "/equilibrium_updated_initial.rds"
  )
)
