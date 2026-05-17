# initialize ------------------------------------------------------------
rm(list = ls())
library(Dispatching)
library(foreach)
library(magrittr)
library(ggplot2)
library(doParallel)
registerDoParallel()
print(detectCores())

# Get market and zone indices from command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Please provide market index (t) and zone index (j) as command line arguments")
}
t <- as.integer(args[1])
j <- as.integer(args[2])

cat("Processing market t =", t, ", zone j =", j, "\n")

# import data ----------------------------------------------------------
equilibrium <-
  readRDS(
    file = "output/multihome/estimate_data/equilibrium_updated_constrained.rds" %>% 
    here::here()
  )

# Verify market and zone indices are valid
n_markets <- length(equilibrium$exogenous)
if (t < 1 || t > n_markets) {
  stop("Invalid market index: ", t, ". Must be between 1 and ", n_markets)
}

n_zones <- length(equilibrium$exogenous[[t]])
if (j < 1 || j > n_zones) {
  stop("Invalid zone index: ", j, ". Must be between 1 and ", n_zones, " for market ", t)
}

equilibrium$constant$use_exp <- FALSE

# Evaluate counterfactual for this specific market-zone combination
counterfactual_tj <-
  evaluate_counterfactual_maximum_margin_tj(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

# Add market and zone identifiers
if (nrow(counterfactual_tj) > 0) {
  counterfactual_tj$t <- t
  counterfactual_tj$j <- j
}

# save counterfactual for this market-zone
dir.create(
  "output/multihome/counterfactual/maximum_margin",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  counterfactual_tj,
  file = paste0(
    "output/multihome/counterfactual/maximum_margin/counterfactual_maximum_margin_t",
    sprintf("%04d", t),
    "_j",
    sprintf("%04d", j),
    ".rds"
  ) %>%
    here::here()
)

cat("Saved results for market", t, ", zone", j, "to output/multihome/counterfactual/maximum_margin/\n")
cat("Number of rows:", nrow(counterfactual_tj), "\n")
