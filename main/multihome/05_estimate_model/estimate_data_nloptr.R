# initialize ------------------------------------------------------------
rm(list = ls())
devtools::load_all(".")
library(foreach)
library(magrittr)
library(ggplot2)
library(doParallel)
registerDoParallel()

# load data -------------------------------------------------------------
equilibrium <- 
  readRDS(
    "output/multihome/make_equilibrium_nationwide_top5_from_data/equilibrium_top5_fringe.rds"
  )
prefix <- "output/multihome/estimate_data"

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

df <-
  check_equilibrium(
    equilibrium = equilibrium
  )
summary(df)

# estimate demand parameters ------------------------------------


## make demand instruments --------------------------------------

instrument_demand <- 
  make_instrument_demand(
    equilibrium = equilibrium
  )

## make demand weighting matrix ---------------------------------

weighting_matrix_demand <- 
  compute_demand_weighting_matrix(
    instrument_demand = instrument_demand
  )

## estimate parameters 
solution_demand_constrained <-
  estimate_demand_parameter_constrained(
    weighting_matrix_demand = weighting_matrix_demand,
    equilibrium = equilibrium
  ) 

equilibrium_updated_constrained <-
  update_demand_nloptr(
    solution_demand = solution_demand_constrained,
    equilibrium = equilibrium
  ) 

## update supply parameter and shocks ------------------------------------
result_supply <- 
  solve_supply_shock(
    equilibrium = equilibrium_updated_constrained
  )

equilibrium_updated_constrained <- 
  update_supply_ec_gamma(
    result_supply = result_supply,
    equilibrium = equilibrium_updated_constrained
  ) 

df <-
  check_equilibrium(
    equilibrium = equilibrium_updated_constrained
  )
summary(df)

## save nloptr estimation result ----------------------------------------------
saveRDS(
  equilibrium_updated_constrained,
  paste0(
    prefix,
    "/equilibrium_updated_constrained.rds"
  )
)