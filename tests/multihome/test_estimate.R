# initialize --------------------------------------------------------
rm(list = ls())
devtools::load_all(".")
library(foreach)
library(magrittr)
library(ggplot2)
library(codetools)
library(doParallel)
registerDoParallel()

# set constant -----------------------------------------------------
seed <- 1
set.seed(seed)
t <- 1
j <- 1

# load equilibrium -----------------------------------------------

equilibrium <-
  readRDS(
    file = "output/multihome/simulate/equilibrium.rds"
  )

# check whether the foc is zero -----------------------------------

df <-
  check_equilibrium(
    equilibrium = equilibrium
  )

# delete non-zero foc market --------------------------------------
index <- 
  df %>%
  dplyr::filter(
    i != 0,
    i != 1,
    i != 2,
    abs(foc_f) > 1e-7 | 
      abs(foc_w) > 1e-7
  ) %>%
  dplyr::select(
    t,
    j
  ) %>%
  dplyr::distinct()
index <- 
  index[
    order(
      index$t,
      index$j,
      decreasing = TRUE
    ), 
  ]
index

if (length(index) > 2){
  for (
    i in 1:nrow(index)
  ) {
    equilibrium$shock[[index$t[i]]][[index$j[i]]] <- NULL
    equilibrium$endogenous[[index$t[i]]][[index$j[i]]] <- NULL
    equilibrium$exogenous[[index$t[i]]][[index$j[i]]] <- NULL
  }
}

equilibrium <- 
  purrr::map(
    equilibrium, 
    ~ purrr::discard(
      .x, 
      ~ length(.x) == 0
    )
  )

df <-
  check_equilibrium(
    equilibrium = equilibrium
  )

df %>%
  dplyr::filter(
    i != 0,
    i != 1,
    abs(foc_f) > 1e-7 | 
      abs(foc_w) > 1e-7
  ) %>%
  dplyr::select(
    t,
    j
  ) %>%
  dplyr::distinct()

# estimate demand parameters ------------------------------------

## make demand instruments --------------------------------------

d_w <- 
  compute_distance_d_w(
    equilibrium = equilibrium
  )
d_w

z_w_tj <- 
  compute_differential_iv_z_w_tj(
    d_w = d_w,
    t = t,
    j = j,
    equilibrium = equilibrium
  )
z_w_tj

z_aw <- 
  make_demand_differential_iv_z_w(
    equilibrium = equilibrium
  )
z_aw

d_f <- 
  compute_distance_d_f(
    equilibrium = equilibrium
  )
d_f

z_f_tj <- 
  compute_differential_iv_z_f_tj(
    d_f = d_f,
    t = t,
    j = j,
    equilibrium = equilibrium
  )
z_f_tj

z_af <- 
  make_demand_differential_iv_z_f(
    equilibrium = equilibrium
  )
z_af

x_c_tilde <- 
  make_demand_cost_iv(
    equilibrium = equilibrium
  )

instrument_demand <- 
  make_instrument_demand(
    equilibrium = equilibrium
  )
instrument_demand

## elicit demand shock ------------------------------------------

check_1 <- 
  compute_demand_shock_a_f_tj(
    m_f = equilibrium$parameter$m_f,
    m_w = equilibrium$parameter$m_w,
    lambda_f = equilibrium$parameter$lambda_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

check_rcpp <- 
  compute_demand_shock_a_f_tj_rcpp(
    m_f = equilibrium$parameter$m_f,
    m_w = equilibrium$parameter$m_w,
    lambda_f = equilibrium$parameter$lambda_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

max(
  abs(
    check_1 - check_rcpp
  )
)

check_2 <- 
  compute_a_f_tj(
    beta_f = equilibrium$parameter$beta_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f
  ) 

max(
  abs(
    check_1 - check_2
  )
)

cbind(
  check_1,
  check_2
)

check_1 <- 
  compute_demand_shock_nleqslv_a_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

check_1_rcpp <-
  compute_demand_shock_nleqslv_a_w_tj_rcpp(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

max(
  abs(
    check_1 - check_1_rcpp
  )
)

check_2 <- 
  compute_demand_shock_iteration_a_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

check_2_rcpp <- 
  compute_demand_shock_iteration_a_w_tj_rcpp(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

max(
  abs(
    check_2 - check_2_rcpp
  )
)

check_3 <- 
  compute_demand_shock_nloptr_a_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

check_4 <- 
  compute_a_w_tj(
    beta_w = equilibrium$parameter$beta_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w
  ) 

max(
  abs(
    check_1 - check_4
  )
)

max(
  abs(
    check_2 - check_4
  )
)

max(
  abs(
    check_3 - check_4
  )
)

## evaluate precision of eliciting a_w 

elicited_a_w <- 
  foreach(
    t = seq_along(equilibrium$endogenous),
    .combine = rbind,
    .packages = c(
      "foreach",
      "magrittr",
      "Dispatching"
    )
  ) %dopar% {
    a_w_t <- 
      foreach(
        j = seq_along(equilibrium$endogenous[[t]]),
        .combine = rbind
      ) %do% {
        a_w_tj <- 
          compute_demand_shock_nleqslv_a_w_tj(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            lambda_w = equilibrium$parameter$lambda_w,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            mu = equilibrium$shock[[t]][[j]]$mu,
            w = equilibrium$endogenous[[t]][[j]]$w,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            s_w = equilibrium$endogenous[[t]][[j]]$s_w,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol
          )
        return(a_w_tj)
      }
    return(a_w_t)
  }

real_a_w <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      a_w_tj <- 
        compute_a_w_tj(
          beta_w = equilibrium$parameter$beta_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w
        ) 
      return(a_w_tj)
    }
  }

df <- 
  data.frame(
    elicited_a_w = elicited_a_w,
    real_a_w = real_a_w
  )

g <- 
  ggplot(
    df, 
    aes(
      x = elicited_a_w, 
      y = real_a_w
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
    x = "a_w_predicted",
    y = "a_w_actual",
    title = "Scatter Plot of demand shocks a_w"
  ) +
  theme_classic()
g

elicited_a_w_rcpp <- 
  foreach(
    t = seq_along(equilibrium$endogenous),
    .combine = rbind,
    .packages = c(
      "foreach",
      "magrittr",
      "Dispatching"
    )
  ) %dopar% {
    a_w_t <- 
      foreach(
        j = seq_along(equilibrium$endogenous[[t]]),
        .combine = rbind
      ) %do% {
        a_w_tj <- 
          compute_demand_shock_nleqslv_a_w_tj_rcpp(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            lambda_w = equilibrium$parameter$lambda_w,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            mu = equilibrium$shock[[t]][[j]]$mu,
            w = equilibrium$endogenous[[t]][[j]]$w,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            s_w = equilibrium$endogenous[[t]][[j]]$s_w,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol
          )
        return(a_w_tj)
      }
    return(a_w_t)
  }

df$elicited_a_w_rcpp <-  elicited_a_w_rcpp

g <- 
  ggplot(
    df, 
    aes(
      x = elicited_a_w_rcpp, 
      y = real_a_w
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
    x = "a_w_predicted_rcpp",
    y = "a_w_actual",
    title = "Scatter Plot of demand shocks a_w"
  ) +
  theme_classic()
g
## elicit demand residual and linear parameters -----------------

system.time(
  result <- 
    solve_demand_shock(
      equilibrium = equilibrium
    )
)
result

cbind(
  result$beta_w,
  equilibrium$parameter$beta_w
)

cbind(
  result$beta_f,
  equilibrium$parameter$beta_f
)

max(
  abs(
    result$beta_w - equilibrium$parameter$beta_w
  )
)

max(
  abs(
    result$beta_f - equilibrium$parameter$beta_f
  )
)

ea_w_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      dis <- 
        equilibrium$shock[[t]][[j]]$ea_w
      
      return(dis)
    }
  }

ea_f_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      dis <- 
        equilibrium$shock[[t]][[j]]$ea_f
      return(dis)
    }
  }

df <- 
  data.frame(
    ea_f = result$ea_f,
    ea_f_true = ea_f_true,
    ea_w = result$ea_w,
    ea_w_true = ea_w_true
  )

g <- 
  ggplot(
    df, 
    aes(
      x = ea_f_true, 
      y = ea_f
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
    x = "true ea_f",
    y = "elicited ea_f",
    title = "Scatter Plot of demand shock ea_f"
  ) +
  theme_classic()
g

g <- 
  ggplot(
    df, 
    aes(
      x = ea_w_true, 
      y = ea_w
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
    x = "true ea_w",
    y = "elicited ea_w",
    title = "Scatter Plot of demand shock ea_w"
  ) +
  theme_classic()
g

## compute demand moment condition ------------------------------

moment_demand <- 
  compute_moment_demand(
    instrument_demand = instrument_demand,
    ea_w = result$ea_w,
    ea_f = result$ea_f
  )

## make demand weighting matrix ---------------------------------

weighting_matrix_demand <- 
  compute_demand_weighting_matrix(
    instrument_demand = instrument_demand
  )
weighting_matrix_demand

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

### check objective function shape ---------------------------------------------
target_list <- 
  list(
    mu_p = 
      seq(
        theta[1] - 0.1, 
        theta[1] + 0.1, 
        by = 0.01
      ),
    mu_ths = 
      seq(
        theta[2] - 0.1, 
        theta[2] + 0.1, 
        by = 0.01
      ),
    m_f = 
      seq(
        theta[3] - 0.1, 
        theta[3] + 0.1, 
        by = 0.01
      ),
    lambda_w = 
      seq(
        theta[4] - 0.1, 
        theta[4] + 0.1, 
        by = 0.01
      ),
    lambda_f = 
      seq(
        theta[5] - 0.1, 
        theta[5] + 0.1, 
        by = 0.01
      )
  )

objective_values <- 
  foreach(
    i = seq_along(target_list)
  ) %do% {
    foreach(
      param_value = target_list[[i]],
      .combine = "rbind"
    ) %do% {
      theta_temp <- theta
      theta_temp[i] <- param_value
      
      obj <- 
        compute_demand_objective_nonlinear(
          theta_demand_nonlinear = theta_temp,
          instrument_demand = instrument_demand,
          weighting_matrix_demand = weighting_matrix_demand,
          equilibrium = equilibrium
        )
      return(
        data.frame(
          parameter = names(target_list)[i],
          value = param_value,
          objective = obj
        )
      )
    }
  }

g_1 <-
  foreach (
    i = seq_along(target_list)
  ) %do% {
    ggplot(
      objective_values[[i]], 
      aes(
        x = value, 
        y = objective
      )
    ) +
      geom_point() +
      geom_line() +
      geom_vline(
        xintercept = theta[i], 
        linetype = "dashed", 
        color = "red"
      ) +
      theme_classic() +
      labs(
        x = "Parameter Value",
        y = "Objective Function Value",
        title = 
          paste(
            "Objective Function Values for", 
            names(target_list)[i]
          )
      )
  }
g_1[[1]]
g_1[[2]]
g_1[[3]]
g_1[[4]]
g_1[[5]]

theta <-
  transform_parameter_to_theta_optim_bounded(
    mu_p = equilibrium$parameter$mu_p,
    mu_ths = equilibrium$parameter$mu_ths,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f
  )

parameter <-
  transform_theta_to_parameter_demand_bounded(
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
  

## minimize demand objective by optim without constraint-----------------------

solution_demand <-
  estimate_demand_parameter(
    equilibrium = equilibrium
  ) 

plot(
  theta,
  solution_demand$par,
  xlab = "True Parameter",
  ylab = "Estimated Parameter",
  main = "Scatter Plot of Estimated vs True Parameters"
)
abline(
  a = 0,
  b = 1,
  col = "red",
  lty = "dashed"
)

### update demand parameters and shocks -----------------------

equilibrium_updated <- 
  update_demand_nonlinear(
    solution_demand = solution_demand,
    equilibrium = equilibrium
  ) 

result_demand <- 
  solve_demand_shock(
    equilibrium = equilibrium_updated
  )

equilibrium_updated <-
  update_demand_ea_beta(
    result_demand = result_demand,
    equilibrium = equilibrium_updated
  ) 

equilibrium_updated <-
  update_demand(
    solution_demand = solution_demand,
    equilibrium = equilibrium
  ) 

plot(
  c(
    equilibrium$parameter$m_w,
    equilibrium$parameter$m_f,
    equilibrium$parameter$lambda_w,
    equilibrium$parameter$lambda_f,
    equilibrium$parameter$beta_w,
    equilibrium$parameter$beta_f
  ),
  c(
    equilibrium_updated$parameter$m_w,
    equilibrium_updated$parameter$m_f,
    equilibrium_updated$parameter$lambda_w,
    equilibrium_updated$parameter$lambda_f,
    equilibrium_updated$parameter$beta_w,
    equilibrium_updated$parameter$beta_f
  ),
  xlab = "True Parameter",
  ylab = "Estimated Parameter",
  main = "Scatter Plot of Estimated vs True Parameters"
)
abline(
  a = 0,
  b = 1,
  col = "red",
  lty = "dashed"
)


## minimize demand parameters by nloptr with constraint  --------------------------

solution_demand_constrained <- 
  estimate_demand_parameter_constrained(
    weighting_matrix_demand = weighting_matrix_demand,
    equilibrium = equilibrium
  )

plot(
  theta,
  solution_demand_constrained[["solution"]],
  xlab = "True Parameter",
  ylab = "Estimated Parameter",
  main = "Scatter Plot of Estimated vs True Parameters"
)
abline(
  a = 0,
  b = 1,
  col = "red",
  lty = "dashed"
)

### update demand parameters and shocks -----------------------

equilibrium_updated_constrained <- 
  update_demand_nonlinear_nloptr(
    solution_demand = solution_demand_constrained,
    equilibrium = equilibrium
  ) 

result_demand <- 
  solve_demand_shock(
    equilibrium = equilibrium_updated_constrained
  )

equilibrium_updated_constrained <-
  update_demand_ea_beta(
    result_demand = result_demand,
    equilibrium = equilibrium_updated_constrained
  ) 

equilibrium_updated_constrained <-
  update_demand_nloptr(
    solution_demand = solution_demand_constrained,
    equilibrium = equilibrium
  ) 

plot(
  c(
    equilibrium$parameter$m_w,
    equilibrium$parameter$m_f,
    equilibrium$parameter$lambda_w,
    equilibrium$parameter$lambda_f,
    equilibrium$parameter$beta_w,
    equilibrium$parameter$beta_f
  ),
  c(
    equilibrium_updated_constrained$parameter$m_w,
    equilibrium_updated_constrained$parameter$m_f,
    equilibrium_updated_constrained$parameter$lambda_w,
    equilibrium_updated_constrained$parameter$lambda_f,
    equilibrium_updated_constrained$parameter$beta_w,
    equilibrium_updated_constrained$parameter$beta_f
  ),
  xlab = "True Parameter",
  ylab = "Estimated Parameter",
  main = "Scatter Plot of Estimated vs True Parameters"
)
abline(
  a = 0,
  b = 1,
  col = "red",
  lty = "dashed"
)


## minimize demand parameters with penalty -------------------------------------
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

compute_demand_objective_with_penalty(
  instrument_demand = instrument_demand,
  weighting_matrix_demand = weighting_matrix_demand,
  equilibrium = equilibrium
)

compute_demand_objective_with_penalty_nonlinear(
  theta_demand_nonlinear = theta,
  instrument_demand = instrument_demand,
  weighting_matrix_demand = weighting_matrix_demand,
  equilibrium = equilibrium
) 

### check objective function shape ---------------------------------------------
target_list <- 
  list(
    mu_p = 
      seq(
        theta[1] - 0.1, 
        theta[1] + 0.1, 
        by = 0.01
      ),
    mu_ths = 
      seq(
        theta[2] - 0.1, 
        theta[2] + 0.1, 
        by = 0.01
      ),
    m_f = 
      seq(
        theta[3] - 0.1, 
        theta[3] + 0.1, 
        by = 0.01
      ),
    m_w = 
      seq(
        theta[4] - 0.1, 
        theta[4] + 0.1, 
        by = 0.01
      ),
    lambda_w = 
      seq(
        theta[5] - 0.1, 
        theta[5] + 0.1, 
        by = 0.01
      ),
    lambda_f = 
      seq(
        theta[6] - 0.1, 
        theta[6] + 0.1, 
        by = 0.01
      )
  )

objective_values <- 
  foreach(
    i = seq_along(target_list)
  ) %do% {
    foreach(
      param_value = target_list[[i]],
      .combine = "rbind"
    ) %do% {
      theta_temp <- theta
      theta_temp[i] <- param_value
      
      obj <- 
        compute_demand_objective_with_penalty_nonlinear(
          theta_demand_nonlinear = theta_temp,
          instrument_demand = instrument_demand,
          weighting_matrix_demand = weighting_matrix_demand,
          equilibrium = equilibrium
        )
      return(
        data.frame(
          parameter = names(target_list)[i],
          value = param_value,
          objective = obj
        )
      )
    }
  }

g_1 <-
  foreach (
    i = seq_along(target_list)
  ) %do% {
    ggplot(
      objective_values[[i]], 
      aes(
        x = value, 
        y = objective
      )
    ) +
      geom_point() +
      geom_line() +
      geom_vline(
        xintercept = theta[i], 
        linetype = "dashed", 
        color = "red"
      ) +
      theme_classic() +
      labs(
        x = "Parameter Value",
        y = "Objective Function Value",
        title = 
          paste(
            "Objective Function Values for", 
            names(target_list)[i]
          )
      )
  }
g_1[[1]]
g_1[[2]]
g_1[[3]]
g_1[[4]]
g_1[[5]]
g_1[[6]]


solution_demand_with_penalty <-
  estimate_demand_parameter_with_penalty(
    equilibrium = equilibrium
  ) 
solution_demand_with_penalty

theta <-
  transform_parameter_to_theta_optim(
    mu_p = equilibrium$parameter$mu_p,
    mu_ths = equilibrium$parameter$mu_ths,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f
  )

plot(
  theta,
  solution_demand_with_penalty$par,
  xlab = "True Parameter",
  ylab = "Estimated Parameter",
  main = "Scatter Plot of Estimated vs True Parameters"
)
abline(
  a = 0,
  b = 1,
  col = "red",
  lty = "dashed"
)

### update demand parameters ----------------------------------------------------
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

## make demand efficient weighting matrix -------------------------
weighting_matrix_demand_efficient <- 
  compute_demand_efficient_weighting_matrix(
    instrument_demand = instrument_demand,
    ea_w = result_demand$ea_w,
    ea_f = result_demand$ea_f
  )

### check objective function shape with efficient weighting matrix --------------
target_list <- 
  list(
    mu_p = 
      seq(
        theta[1] - 0.1, 
        theta[1] + 0.1, 
        by = 0.01
      ),
    mu_ths = 
      seq(
        theta[2] - 0.1, 
        theta[2] + 0.1, 
        by = 0.01
      ),
    m_f = 
      seq(
        theta[3] - 0.1, 
        theta[3] + 0.1, 
        by = 0.01
      ),
    lambda_w = 
      seq(
        theta[4] - 0.1, 
        theta[4] + 0.1, 
        by = 0.01
      ),
    lambda_f = 
      seq(
        theta[5] - 0.1, 
        theta[5] + 0.1, 
        by = 0.01
      )
  )

objective_values <- 
  foreach(
    i = seq_along(target_list)
  ) %do% {
    foreach(
      param_value = target_list[[i]],
      .combine = "rbind"
    ) %do% {
      theta_temp <- theta
      theta_temp[i] <- param_value
      
      obj <- 
        compute_demand_objective_nonlinear(
          theta_demand_nonlinear = theta_temp,
          instrument_demand = instrument_demand,
          weighting_matrix_demand = weighting_matrix_demand_efficient,
          equilibrium = equilibrium
        )
      return(
        data.frame(
          parameter = names(target_list)[i],
          value = param_value,
          objective = obj
        )
      )
    }
  }

g_2 <-
  foreach (
    i = seq_along(target_list)
  ) %do% {
    ggplot(
      objective_values[[i]], 
      aes(
        x = value, 
        y = objective
      )
    ) +
      geom_point() +
      geom_line() +
      geom_vline(
        xintercept = theta[i], 
        linetype = "dashed", 
        color = "red"
      ) +
      theme_classic() +
      labs(
        x = "Parameter Value",
        y = "Objective Function Value",
        title = 
          paste(
            "Objective Function Values for", 
            names(target_list)[i]
          )
      )
  }
g_2[[1]]
g_2[[2]]
g_2[[3]]
g_2[[4]]
g_2[[5]]

# estimate supply parameters -----------------------------------

## elicit supply shocks ----------------------------------------
t <- 1
j <- 1

c <- 
  compute_c_w_tj(
    gamma_w = equilibrium$parameter$gamma_w,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    use_exp = FALSE
  ) 

check_1 <- 
  compute_supply_shock_c_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

check_1 <-
  rbind(
    c[1],
    c[2],
    check_1
  ) 

check_2 <- 
  compute_c_w_tj(
    gamma_w = equilibrium$parameter$gamma_w,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    use_exp = FALSE
  ) 

max(
  abs(
    check_1 - check_2
  )
)


c_w <- check_1
c <- 
  compute_c_f_tj(
    gamma_f = equilibrium$parameter$gamma_f,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    use_exp = FALSE
  )

check_1 <- 
  compute_supply_shock_c_f_tj(
    c_w = c_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    beta_f = equilibrium$parameter$beta_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  ) 

check_2 <- 
  compute_c_f_tj(
    gamma_f = equilibrium$parameter$gamma_f,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    use_exp = FALSE
  )

check_1 <-
  rbind(
    c[1],
    c[2],
    check_1
  ) 

max(
  abs(
    check_1 - check_2
  )
)


### check the regression and shocks are correctly specified --------------------------
#### check all elicited c_w and c_f are consistent with true ones --------------------
c_w_eclicited <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      c_w <- 
        compute_supply_shock_c_w_tj(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          f = equilibrium$endogenous[[t]][[j]]$f,
          w = equilibrium$endogenous[[t]][[j]]$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          s_w = equilibrium$endogenous[[t]][[j]]$s_w,
          owner = equilibrium$exogenous[[t]][[j]]$owner,
          method_s_w = equilibrium$constant$method_s_w,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
      
      return(c_w)
    }
  }

c_w_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      c_w <- 
        compute_c_w_tj(
          gamma_w = equilibrium$parameter$gamma_w,
          x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
          ec_w = equilibrium$shock[[t]][[j]]$ec_w,
          use_exp = FALSE
        ) 
      c_w <- 
        c_w[3]
      
      return(c_w)
    }
  }

df <- 
  data.frame(
    c_w_eclicited = c_w_eclicited,
    c_w_ture = c_w_true
  )

g <- 
  ggplot(
    df, 
    aes(
      x = c_w_true, 
      y = c_w_eclicited 
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
    x = "true c_w",
    y = "elicited c_w",
    title = "Scatter Plot of Costs c_w "
  ) +
  theme_classic()
g

c_f_eclicited <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      c_w <- 
        compute_supply_shock_c_w_tj(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          f = equilibrium$endogenous[[t]][[j]]$f,
          w = equilibrium$endogenous[[t]][[j]]$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          s_w = equilibrium$endogenous[[t]][[j]]$s_w,
          owner = equilibrium$exogenous[[t]][[j]]$owner,
          method_s_w = equilibrium$constant$method_s_w,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
      c_w <- 
        rbind(
          0,
          0,
          c_w
        ) 
      
      c_f <-   
        compute_supply_shock_c_f_tj(
          c_w = c_w,
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          beta_f = equilibrium$parameter$beta_f,
          lambda_w = equilibrium$parameter$lambda_w,
          lambda_f = equilibrium$parameter$lambda_f,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          ea_f = equilibrium$shock[[t]][[j]]$ea_f,
          owner = equilibrium$exogenous[[t]][[j]]$owner,
          w = equilibrium$endogenous[[t]][[j]]$w,
          f = equilibrium$endogenous[[t]][[j]]$f,
          s_w = equilibrium$endogenous[[t]][[j]]$s_w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          method_s_w = equilibrium$constant$method_s_w,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        ) 
      
      
      return(c_f)
    }
  }

c_f_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      c_f <- 
        compute_c_f_tj(
          gamma_f = equilibrium$parameter$gamma_f,
          x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
          ec_f = equilibrium$shock[[t]][[j]]$ec_f,
          use_exp = FALSE
        ) 
      c_f <- 
        c_f[3]
      return(c_f)
    }
  }

df <- 
  data.frame(
    c_f_eclicited = c_f_eclicited,
    c_f_ture = c_f_true
  )

g <- 
  ggplot(
    df, 
    aes(
      x = c_f_true, 
      y = c_f_eclicited 
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
    x = "true c_f",
    y = "elicited c_f",
    title = "Scatter Plot of Costs c_f"
  ) +
  theme_classic()
g

#### check regression accuracy with private market and fringe ths --------------
c <- 
  foreach(
    t = seq_along(equilibrium$endogenous),
    .combine = rbind,
    .packages = 
      c(
        "Dispatching",
        "foreach",
        "magrittr"
      )
  ) %dopar% {
    c_t <- 
      foreach(
        j = seq_along(equilibrium$endogenous[[t]]),
        .combine = rbind
      ) %do% { 
        c_w_initial <- 
          compute_c_w_tj(
            gamma_w = equilibrium$parameter$gamma_w,
            x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
            ec_w = equilibrium$shock[[t]][[j]]$ec_w,
            use_exp = FALSE
          ) 
        
        c_f_initial <- 
          compute_c_f_tj(
            gamma_f = equilibrium$parameter$gamma_f,
            x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
            ec_f = equilibrium$shock[[t]][[j]]$ec_f,
            use_exp = FALSE
          )
        c_w_tj <- 
          compute_supply_shock_c_w_tj(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            beta_w = equilibrium$parameter$beta_w,
            lambda_w = equilibrium$parameter$lambda_w,
            x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            mu = equilibrium$shock[[t]][[j]]$mu,
            ea_w = equilibrium$shock[[t]][[j]]$ea_w,
            f = equilibrium$endogenous[[t]][[j]]$f,
            w = equilibrium$endogenous[[t]][[j]]$w,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            s_w = equilibrium$endogenous[[t]][[j]]$s_w,
            owner = equilibrium$exogenous[[t]][[j]]$owner,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol
          )
        
        c_w_tj <- 
          rbind(
            c_w_initial[1],
            c_w_initial[2],
            c_w_tj
          ) 
        
        c_f_tj <- 
          compute_supply_shock_c_f_tj(
            c_w = c_w_tj,
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            beta_w = equilibrium$parameter$beta_w,
            beta_f = equilibrium$parameter$beta_f,
            lambda_w = equilibrium$parameter$lambda_w,
            lambda_f = equilibrium$parameter$lambda_f,
            x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
            x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            mu = equilibrium$shock[[t]][[j]]$mu,
            ea_w = equilibrium$shock[[t]][[j]]$ea_w,
            ea_f = equilibrium$shock[[t]][[j]]$ea_f,
            owner = equilibrium$exogenous[[t]][[j]]$owner,
            w = equilibrium$endogenous[[t]][[j]]$w,
            f = equilibrium$endogenous[[t]][[j]]$f,
            s_w = equilibrium$endogenous[[t]][[j]]$s_w,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol
          ) 
        c_f_tj <- 
          rbind(
            c_f_initial[1],
            c_f_initial[2],
            c_f_tj
          )

        c_tj <- 
          data.frame(
            c_w = c_w_tj,
            c_f = c_f_tj
          )
        return(c_tj)
      }
    return(c_t)
  }
colnames(c) <- c("c_w", "c_f")

x_c_w <- 
  foreach(
    t = seq_along(equilibrium$endogenous),
    .combine = rbind,
    .packages = 
      c(
        "Dispatching",
        "foreach",
        "magrittr"
      )
  ) %dopar% {
    x_c_w_t <- 
      foreach(
        j = seq_along(equilibrium$endogenous[[t]]),
        .combine = rbind
      ) %do% {
        x_c_w_t_j <- equilibrium$exogenous[[t]][[j]]$x_c_w
        return(
          x_c_w_t_j
        )
      }
    return(
      x_c_w_t
    )
  }

x_c_f <- 
  foreach(
    t = seq_along(equilibrium$endogenous),
    .combine = rbind,
    .packages = 
      c(
        "Dispatching",
        "foreach",
        "magrittr"
      )
  ) %do% {
    x_c_f_t <- 
      foreach(
        j = seq_along(equilibrium$endogenous[[t]]),
        .combine = rbind
      ) %do% {
        x_c_f_t_j <- equilibrium$exogenous[[t]][[j]]$x_c_f
        return(
          x_c_f_t_j
        )
      }
    return(x_c_f_t)
  }

result_w <- 
  lm(
    c$c_w ~ x_c_w + 0
  )

gamma_w <- 
  result_w$coefficients
gamma_w <- 
  ifelse(
    is.na(gamma_w),
    0,
    gamma_w
  )
ec_w <- 
  result_w$residuals %>%
  as.matrix()

result_f <- 
  lm(
    c$c_f ~ x_c_f + 0 
  )

gamma_f <- 
  result_f$coefficients
gamma_f <- 
  ifelse(
    is.na(gamma_f),
    0,
    gamma_f
  )
ec_f <- 
  result_f$residuals %>%
  as.matrix()

result <-
  list(
    gamma_w = gamma_w,
    gamma_f = gamma_f,
    ec_w = ec_w,
    ec_f = ec_f
  )

cbind(
  result$gamma_w,
  equilibrium$parameter$gamma_w
)

cbind(
  result$gamma_f,
  equilibrium$parameter$gamma_f
)

max(
  abs(
    result$gamma_w - equilibrium$parameter$gamma_w
  )
)

max(
  abs(
    result$gamma_f - equilibrium$parameter$gamma_f
  )
)

#### check supply shocks consistency ----------------------------------------------
ec_w_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      dis <- 
        equilibrium$shock[[t]][[j]]$ec_w
      
      return(dis)
    }
  }

ec_f_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      dis <- 
        equilibrium$shock[[t]][[j]]$ec_f
      return(dis)
    }
  }

df <- 
  data.frame(
    ec_f = result$ec_f,
    ec_f_true = ec_f_true,
    ec_w = result$ec_w,
    ec_w_true = ec_w_true
  )

g <- 
  ggplot(
    df, 
    aes(
      x = ec_f_true, 
      y = ec_f
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
    x = "true ec_f",
    y = "elicited ec_f",
    title = "Scatter Plot of supply shock ec_f"
  ) +
  theme_classic()
g

g <- 
  ggplot(
    df, 
    aes(
      x = ec_w_true, 
      y = ec_w
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
    x = "true ec_w",
    y = "elicited ec_w",
    title = "Scatter Plot of supply shock ec_w"
  ) +
  theme_classic()
g

## elicit supply residual and linear parameters -----------------

result <- 
  solve_supply_shock(
    equilibrium = equilibrium
  )
result

cbind(
  result$gamma_w,
  equilibrium$parameter$gamma_w
)

cbind(
  result$gamma_f,
  equilibrium$parameter$gamma_f
)

max(
  abs(
    result$gamma_w - equilibrium$parameter$gamma_w
  )
)

max(
  abs(
    result$gamma_f - equilibrium$parameter$gamma_f
  )
)

### update supply parameters and shocks -----------------------

equilibrium_updated <- 
  update_supply_ec_gamma(
    result_supply = result,
    equilibrium = equilibrium
  ) 

max(
  abs(
    equilibrium_updated$parameter$gamma_w - equilibrium$parameter$gamma_w
  )
)

max(
  abs(
    equilibrium_updated$parameter$gamma_f - equilibrium$parameter$gamma_f
  )
)

ec_w_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      dis <- 
        equilibrium$shock[[t]][[j]]$ec_w[
          3:nrow(
            equilibrium_updated$shock[[t]][[j]]$ec_w
          ),
          ,
          drop = FALSE
        ]
      
      return(dis)
    }
  }

ec_f_true <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = rbind
    ) %do% {
      dis <- 
        equilibrium$shock[[t]][[j]]$ec_f[
          3:nrow(
            equilibrium_updated$shock[[t]][[j]]$ec_w
          ),
          ,
          drop = FALSE
        ]
      return(dis)
    }
  }

ec_w_updated <- 
  foreach(
    t = seq_along(equilibrium$shock),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$shock[[t]]),
      .combine = rbind
    ) %do% {
      equilibrium_updated$shock[[t]][[j]]$ec_w[
        3:nrow(
          equilibrium_updated$shock[[t]][[j]]$ec_w
        ),
        ,
        drop = FALSE
      ]
    }
  }

ec_f_updated <- 
  foreach(
    t = seq_along(equilibrium$shock),
    .combine = rbind
  ) %do% {
    foreach(
      j = seq_along(equilibrium$shock[[t]]),
      .combine = rbind
    ) %do% {
      equilibrium_updated$shock[[t]][[j]]$ec_f[
        3:nrow(
          equilibrium_updated$shock[[t]][[j]]$ec_f
        ),
        ,
        drop = FALSE
      ]
    }
  }

df <- 
  data.frame(
    ec_w = ec_w_updated,
    ec_w_true = ec_w_true,
    ec_f = ec_f_updated,
    ec_f_true = ec_f_true
  )

g <- 
  ggplot(
    df, 
    aes(
      x = ec_f_true, 
      y = ec_f
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
    x = "true ec_f", 
    y = "elicited ec_f", 
    title = "Scatter Plot of supply shock ec_f"
  ) +
  theme_classic()
g

g <-
  ggplot(
    df, 
    aes(
      x = ec_w_true, 
      y = ec_w
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
    x = "true ec_w", 
    y = "elicited ec_w", 
    title = "Scatter Plot of supply shock ec_w"
  ) +
  theme_classic()
g

# estimate parameters ----------------------------------------------------------

equilibrium_updated <- 
  estimate_parameter(
    equilibrium = equilibrium
  ) 

equilibrium_updated_efficient <- 
  estimate_parameter_constrained(
    weighting_matrix_demand = weighting_matrix_demand,
    equilibrium = equilibrium
  )

equilibrium_updated_with_penalty <- 
  estimate_parameter_with_penalty(
    equilibrium = equilibrium
  )
