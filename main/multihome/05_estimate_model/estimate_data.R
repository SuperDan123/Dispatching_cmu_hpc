# initialize ------------------------------------------------------------
rm(list = ls())
library(Dispatching)
library(foreach)
library(magrittr)
library(ggplot2)
library(doParallel)
registerDoParallel()
print(detectCores())

# set constant ----------------------------------------------------------
seed <- 1
set.seed(seed)
n_ths <- 2
multistart <- 20
solver <- "nleqslv"
use_parallel <- "TRUE"
n <- 100
num_ths_max <- 100
prefix <- "output/multihome/estimate_data"
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

# change method ---------------------------------------------------------

equilibrium$constant$method_s_w <- "approximate"

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
    compute_demand_objective(
      instrument_demand = instrument_demand,
      weighting_matrix_demand = weighting_matrix_demand,
      equilibrium = equilibrium
    )
  )
)


equilibrium_updated <- 
  estimate_parameter(
    equilibrium = equilibrium
  ) 

saveRDS(
  equilibrium_updated,
  paste0(
    prefix,
    "/equilibrium_updated.rds"
  )
)