rm(list = ls())
gc()

library(Dispatching)
library(magrittr)
library(foreach)
library(doParallel)
registerDoParallel()
dir.create("output/counterfactual_hypothetical_market_margin_constraint", showWarnings = FALSE)
dir.create("figuretable/counterfactual_hypothetical_market_margin_constraint", showWarnings = FALSE)

# Load data ---------------------------------------------------------------

equilibrium <- readRDS("output/estimate_structural_parameters/equilibrium_data_distance_IV.RDS")
parameters <- equilibrium$parameters
spec <- equilibrium$spec

# Set constants -----------------------------------------------------------

N_max <- Inf
parallel <- TRUE
cpp <- TRUE
opts <- list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  maxeval = 1e+6,
  xtol_rel = 1e-8
) 
margin_upper_bound_vec <- c(0.7, 0.6, 0.5, 0.4, 0.3, 0.2)

# Make the original equilibrium -------------------------------------------

equilibrium_baseline <- 
  construct_hypothetical_equilibrium(
    equilibrium,
    cpp
  ) 

## Make parameter table ---------------------------------------------------
data <- equilibrium_baseline$extract_single_market(1)
a_W_high <- data$a[1, 1]
a_W_low <- data$a[2, 1]
a_F <- data$a[1, 2]
mc_W <- data$mc[1, 1]
mc_F <- data$mc[1, 2]
mu <- data$mu[1]
w_0 <- data$w_0
S_W <- data$S[1]
S_F <- data$S[2]

param_table <- data.frame(matrix(0, nrow = 9, ncol = 0))

param_table$description <- c("Worker side preference heterogeneity: high",
                             "Worker side preference heterogeneity: low", 
                             "Clinet firm side preference heterogeneity", 
                             "Worker side marginal cost",
                             "Client firm side marginal cost",
                             "Match efficiency", 
                             "Part-time wage",
                             "Worker side market size",
                             "Client firm side market size")

param_table$value <- c(a_W_high, a_W_low, a_F, mc_W, mc_F, mu, w_0, S_W, S_F)

rownames(param_table) <- c("$a^W_{high}$", "$a^W_{low}$", "$a^F$", "$c^W$", "$c^F$", "$\\mu$", "$w_0$", "$S^W$", "$S^F$")
colnames(param_table) <- c("Description", "Value")

param_table %>%
  kableExtra::kbl(
    format = "latex",
    digits = 2,
    booktabs = TRUE,
    escape = F,
    align = c("l", "r")
  ) %>%
  kableExtra::save_kable(
    file = here::here("figuretable/counterfactual_hypothetical_market_margin_constraint/parameters_hypothetical_market.tex")
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
  file = "output/counterfactual_hypothetical_market_margin_constraint/result_hypothetical_market.RDS"
  )
