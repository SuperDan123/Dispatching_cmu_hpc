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
instrument_demand

## make demand weighting matrix ---------------------------------

weighting_matrix_demand <- 
  compute_demand_weighting_matrix(
    instrument_demand = instrument_demand
  )
weighting_matrix_demand

## elicit demand residual and linear parameters -----------------

result<- 
  solve_demand_shock(
    equilibrium = equilibrium
  )
result

## compute demand moment condition ------------------------------

moment_demand <- 
  compute_moment_demand(
    instrument_demand = instrument_demand,
    ea_w = result$ea_w,
    ea_f = result$ea_f
  )
moment_demand

## evaluate demand objective function ----------------------------

compute_demand_objective(
  instrument_demand = instrument_demand,
  weighting_matrix_demand = weighting_matrix_demand,
  equilibrium = equilibrium
)

theta <-
  transform_parameter_to_theta_optim(
    mu_p = equilibrium$parameter$mu_p,
    mu_ths = equilibrium$parameter$mu_ths,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f
  )

parameter <-
  transform_theta_to_parameter_demand(
    theta = theta,
    parameter = equilibrium$parameter
  )

max(
  abs(
    parameter$mu_p - equilibrium$parameter$mu_p
  )
)

max(
  abs(
    parameter$mu_ths - equilibrium$parameter$mu_ths
  )
)

max(
  abs(
    parameter$m_w - equilibrium$parameter$m_w
  )
)

max(
  abs(
    parameter$m_f - equilibrium$parameter$m_f
  )
)

max(
  abs(
    parameter$lambda_w - equilibrium$parameter$lambda_w
  )
)

max(
  abs(
    parameter$lambda_f - equilibrium$parameter$lambda_f
  )
)

equilibrium <-
  update_mu(
    equilibrium = equilibrium,
    parameter = parameter
  ) 

compute_demand_objective_nonlinear(
  theta_demand_nonlinear = theta,
  instrument_demand = instrument_demand,
  weighting_matrix_demand = weighting_matrix_demand,
  equilibrium = equilibrium
) 

# minimize the demand objective function with nloptr --------------------------

solution_demand_constrained <-
  estimate_demand_parameter_constrained(
    weighting_matrix_demand = weighting_matrix_demand,
    equilibrium = equilibrium
  ) 

 ## update demand parameters and shocks -----------------------

equilibrium_updated_constrained <-
  update_demand_nloptr(
    solution_demand = solution_demand_constrained,
    equilibrium = equilibrium
  ) 

## check consistency between actual and predicted s_w---------------------------
equilibrium_updated_constrained <- 
  readRDS(
    "output/multihome/estimate_data/equilibrium_updated_constrained.rds"
  )

s_w_predicted <- 
  foreach(
    t = seq_along(equilibrium_updated_constrained$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium_updated_constrained$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      solution_tj <- equilibrium_updated_constrained$endogenous[[t]][[j]]$s_w
      return(solution_tj)
    }
  }

s_w_actual <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      s_w_tj <- equilibrium$endogenous[[t]][[j]]$s_w
#          equilibrium$endogenous[[t]][[j]]$s_w[3:nrow(equilibrium_updated_constrained$endogenous[[t]][[j]]$w)] %>% as.matrix()
      return(s_w_tj)
    }
  }

df <- 
  data.frame(
    s_w_predicted = s_w_predicted,
    s_w_actual = s_w_actual
  )

g <- 
  ggplot(
    df, 
    aes(
      x = s_w_predicted, 
      y = s_w_actual
    )
  ) +
  geom_point() +
  geom_abline(
    slope = 1, 
    intercept = 0, 
    color = "red", 
    linetype = "solid"
  ) +
  labs(
    x = "s_w_predicted",
    y = "s_w_actual",
    title = "Scatter Plot of share of workers"
  ) +
  theme_classic()
g

## update supply parameter and shocks ------------------------------------
result_supply <- 
  solve_supply_shock(
    equilibrium = equilibrium_updated_constrained
  )
result

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

# minimize the demand objective function optim without constraint-----------------------

## estimate parameters ---------------------------------------------------------
## initial parameter values
equilibrium$parameter$m_w <- 0.1
equilibrium$parameter$m_f <- 1 - equilibrium$parameter$m_w
equilibrium$parameter$lambda_w <- 1
equilibrium$parameter$lambda_f <- -1

equilibrium_updated <- 
  estimate_parameter(
    equilibrium = equilibrium
  ) 

## check consistency between actual s_w and predicted s_w ----------------------
s_w_predicted <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      a_w <-
        compute_a_w_tj(
          beta_w = equilibrium_updated$parameter$beta_w,
          x_a_w = equilibrium_updated$exogenous[[t]][[j]]$x_a_w,
          ea_w = equilibrium_updated$shock[[t]][[j]]$ea_w
        )
      
      solution_tj <- 
        solve_s_w_tj_from_a_w_exact_rcpp(
          a_w = a_w,
          m_w = equilibrium_updated$parameter$m_w,
          m_f = equilibrium_updated$parameter$m_f,
          lambda_w = equilibrium_updated$parameter$lambda_w,
          size_w = equilibrium_updated$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium_updated$exogenous[[t]][[j]]$size_f,
          mu = equilibrium_updated$shock[[t]][[j]]$mu,
          w = equilibrium_updated$endogenous[[t]][[j]]$w,
          s_f = equilibrium_updated$endogenous[[t]][[j]]$s_f
        ) %>% as.matrix()
      return(solution_tj)
    }
  }

s_w_actual <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      s_w_tj <- equilibrium$endogenous[[t]][[j]]$s_w
      return(s_w_tj)
    }
  }

df <- 
  data.frame(
    s_w_predicted = s_w_predicted,
    s_w_actual = s_w_actual
  )

g <- 
  ggplot(
    df, 
    aes(
      x = s_w_predicted, 
      y = s_w_actual
    )
  ) +
  geom_point() +
  geom_abline(
    slope = 1, 
    intercept = 0, 
    color = "red", 
    linetype = "solid"
  ) +
  labs(
    x = "s_w_predicted",
    y = "s_w_actual",
    title = "Scatter Plot of share of workers"
  ) +
  theme_classic()
g


# minimize the demand objective function with penalty --------------------------
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

max(
  abs(
    parameter$mu_p - equilibrium$parameter$mu_p
  )
)

max(
  abs(
    parameter$mu_ths - equilibrium$parameter$mu_ths
  )
)

max(
  abs(
    parameter$m_w - equilibrium$parameter$m_w
  )
)

max(
  abs(
    parameter$m_f - equilibrium$parameter$m_f
  )
)

max(
  abs(
    parameter$lambda_w - equilibrium$parameter$lambda_w
  )
)

max(
  abs(
    parameter$lambda_f - equilibrium$parameter$lambda_f
  )
)

solution_demand_with_penalty <-
  estimate_demand_parameter_with_penalty(
    equilibrium = equilibrium
  ) 

## update demand parameters ---------------------------------------------------

equilibrium_updated_with_penalty <- 
  update_demand_nonlinear_with_penalty(
    solution_demand = solution_demand_with_penalty,
    equilibrium = equilibrium
  ) 

result_demand <- 
  solve_demand_shock(
    equilibrium = equilibrium_updated_with_penalty
  )

equilibrium_updated_with_penalty <-
  update_demand_ea_beta(
    result_demand = result_demand,
    equilibrium = equilibrium_updated_with_penalty
  ) 

equilibrium_updated_with_penalty <- 
  update_demand_with_penalty(
    solution_demand = solution_demand_with_penalty,
    equilibrium = equilibrium
  )

result_supply <- 
  solve_supply_shock(
    equilibrium = equilibrium_updated_with_penalty
  )
result

equilibrium_updated_with_penalty <- 
  update_supply_ec_gamma(
    result_supply = result_supply,
    equilibrium = equilibrium
  ) 

## make demand efficient weighting matrix -------------------------
weighting_matrix_demand_efficient <- 
  compute_demand_efficient_weighting_matrix(
    instrument_demand = instrument_demand,
    ea_w = result_demand$ea_w,
    ea_f = result_demand$ea_f
  )


