rm(list = ls())
gc()

library(Dispatching)
library(magrittr)
library(foreach)
library(doParallel)
registerDoParallel()
dir.create("output/counterfactual_actual_market_margin_constraint", showWarnings = FALSE)
set.seed(1)

# Load data ---------------------------------------------------------------

equilibrium <- readRDS("output/estimate_structural_parameters/equilibrium_data_distance_IV.RDS")
parameters <- equilibrium$parameters
spec <- equilibrium$spec

# set constants -----------------------------------------------------------

N_max <- Inf
parallel <- TRUE
cpp <- TRUE
opts <- list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  maxeval = 1e+6,
  xtol_rel = 1e-8
) 
margin_upper_bound_vec <- c(0.7, 0.6, 0.5, 0.4, 0.3, 0.2)
sim_id <- 1
number_cz <- 1

# Subset markets ----------------------------------------------------------

cz_list <-
  equilibrium$dataframe$cz %>% 
  unique()
cz_list <-
  sample(
    cz_list,
    number_cz
  )
equilibrium_baseline <- 
  extract_cz(
    equilibrium,
    cz_list
  )

# Compute equilibrium -----------------------------------------------------

output <-
  compute_equilibrium_with_margin_constraint_multiple_settings(
    equilibrium_baseline = equilibrium_baseline,
    margin_upper_bound_vec,
    N_max,
    parallel,
    cpp,
    opts
  )

saveRDS(
  output, 
  file = "output/counterfactual_actual_market_margin_constraint/result_actual_market.RDS"
  )





