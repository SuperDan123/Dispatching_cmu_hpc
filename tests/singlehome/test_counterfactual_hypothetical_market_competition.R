rm(list = ls())
gc()

library(Dispatching)
library(magrittr)
library(foreach)
library(doParallel)
registerDoParallel()
dir.create("output/counterfactual_hypothetical_market_competition", showWarnings = FALSE)
dir.create("figuretable/counterfactual_hypothetical_market_competition", showWarnings = FALSE)

# Load data ---------------------------------------------------------------

equilibrium <- readRDS("output/estimate_structural_parameters/equilibrium_data_distance_IV.RDS")
parameters <- equilibrium$parameters
spec <- equilibrium$spec

# Set constants -----------------------------------------------------------

N_max <- Inf
N_copy_max <- 100
parallel <- TRUE
cpp <- TRUE
opts <- list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  maxeval = 1e+6,
  xtol_rel = 1e-8
) 
mc_manual <- c(1, 1)

# Make the original equilibrium -------------------------------------------

equilibrium_baseline <- 
  construct_hypothetical_equilibrium(
    equilibrium = equilibrium,
    cpp = cpp,
    mc_manual = mc_manual 
  ) 

# Compute equilibrium -----------------------------------------------------

## Make monopoly data -----------------------------------------------------
equilibrium_monopoly <-
  compute_monopoly_equilibrium(
    equilibrium = equilibrium_baseline,
    N_max,
    parallel,
    cpp
  ) 


## Make n copy data list --------------------------------------------------
equilibrium_copy_list <-
  foreach (n = seq(from = 1, to = N_copy_max, by = 10)) %do% {
    equilibrium_n_copy <-
      compute_n_copy_equilibrium(
        equilibrium = equilibrium_baseline,
        N_copy = n,
        N_max,
        parallel,
        cpp
      ) 
    return(equilibrium_n_copy)
  }


# Save results ------------------------------------------------------------
output <-
  list(
    equilibrium_baseline = equilibrium_baseline,
    equilibrium_monopoly = equilibrium_monopoly,
    equilibrium_copy_list = equilibrium_copy_list
  )
saveRDS(
  output,
  file = "output/counterfactual_hypothetical_market_competition/result_hypothetical_market.RDS"
)
