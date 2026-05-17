
# Initialize --------------------------------------------------------------
rm(list = ls())
gc()

library(magrittr)
library(Dispatching)
library(ggplot2)
library(foreach)
library(doParallel)

registerDoParallel(10)

# Read data ---------------------------------------------------------------

equilibrium <- readRDS("output/test_estimation/equilibrium_iv_dist.rds")

# test functions ----------------------------------------------------------
exogenous <- equilibrium$exogenous
shocks <- equilibrium$shocks
n_sim <- 100
seed <- 1

# Randomly draw residuals
eq <- equilibrium$copy()
eq$generate_zero_shocks()
eq$shocks[[1]]

eq <- equilibrium$copy()
eq$generate_shock_zeros_from_empirical_distribution(
  n_sim = n_sim,
  seed = seed
)
eq$shocks[[2]]
