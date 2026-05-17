rm(list = ls())
devtools::load_all(".")
library(foreach)
library(magrittr)
library(ggplot2)
library(codetools)
library(doParallel)
registerDoParallel()

# set equilibrium object --------------------------------------------------------------
seed <- 1
set.seed(seed)
t <- 1
j <- 1

equilibrium <-
  generate_equilibrium(
    n_ths = 4,
    n_market = 2,
    n_zone = 2,
    seed = seed
  )

# base equilibrium ---------------------------------------------------------------
equilibrium_base <-
  solve_equilibrium(
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

# test competitive benchmark solver -----------------------------------------------
w_f <-
  solve_w_f_competitive_nleqslv_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    beta_f = equilibrium$parameter$beta_f,
    gamma_w = equilibrium$parameter$gamma_w,
    gamma_f = equilibrium$parameter$gamma_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 1,
    multistart = 10
  )

s_f <-
  solve_s_f_tj_rcpp(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = w_f$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 

s_w <-
  solve_s_w_tj_exact(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    w = w_f$w,
    s_f = s_f
  ) %>%
  as.matrix()

profit <-
  compute_profit_ths_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    gamma_w = equilibrium$parameter$gamma_w,
    gamma_f = equilibrium$parameter$gamma_f,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    w = w_f$w,
    f = w_f$f,
    s_w = s_w,
    s_f = s_f,
    use_exp = equilibrium$constant$use_exp
  ) 

profit[
  3:nrow(profit),
  , 
  drop = FALSE
]

# test competitive equilibrium updater -------------------------------------------
equilibrium_competitive_tj <-
  solve_equilibrium_competitive_tj(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 10
  )

equilibrium_competitive <- 
  solve_equilibrium_competitive(
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 10
  )

# test frictionless + free entry -----------------------------------------------
equilibrium_frictionless_free_entry <- 
  solve_equilibrium_frictionless_with_free_entry_tj(
    t = 1,
    j = 1,
    equilibrium = equilibrium
  )

equilibrium_frictionless_free_entry_all <-
  solve_equilibrium_frictionless_with_free_entry(
    equilibrium = equilibrium
  )

# test frictionless without free entry -----------------------------------------
compute_meeting_number_tj_frictionless(
  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
  s_w = equilibrium$endogenous[[t]][[j]]$s_w,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f
)

compute_meeting_probability_w_tj_frictionless(
  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
  s_w = equilibrium$endogenous[[t]][[j]]$s_w,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f
)

compute_meeting_probability_f_tj_frictionless(
  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
  s_w = equilibrium$endogenous[[t]][[j]]$s_w,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f
)

s_f_d_f <- 
  solve_s_f_d_f_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

s_w_d_f_exact <- 
  solve_s_w_d_f_tj(
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
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  ) 

c_w <- 
  compute_c_w_tj(
    gamma_w = equilibrium$parameter$gamma_w,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    use_exp = equilibrium$constant$use_exp
  ) 

c_f <-
  compute_c_f_tj(
    gamma_f = equilibrium$parameter$gamma_f,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    use_exp = equilibrium$constant$use_exp
  ) 

compute_foc_f_tj_frictionless(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    c_w = c_w,
    c_f = c_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_w = equilibrium$endogenous[[t]][[j]]$s_w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_w_d_f = s_w_d_f_exact,
    s_f_d_f = s_f_d_f
  )

compute_foc_tj_frictionless(
  m_w = equilibrium$parameter$m_w,
  m_f = equilibrium$parameter$m_f,
  beta_w = equilibrium$parameter$beta_w,
  beta_f = equilibrium$parameter$beta_f,
  gamma_w = equilibrium$parameter$gamma_w,
  gamma_f = equilibrium$parameter$gamma_f,
  lambda_w = equilibrium$parameter$lambda_w,
  lambda_f = equilibrium$parameter$lambda_f,
  x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
  x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
  x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
  x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
  owner = equilibrium$exogenous[[t]][[j]]$owner,
  mu = equilibrium$shock[[t]][[j]]$mu,
  ea_w = equilibrium$shock[[t]][[j]]$ea_w,
  ea_f = equilibrium$shock[[t]][[j]]$ea_f,
  ec_w = equilibrium$shock[[t]][[j]]$ec_w,
  ec_f = equilibrium$shock[[t]][[j]]$ec_f,
  w = equilibrium$endogenous[[t]][[j]]$w,
  f = equilibrium$endogenous[[t]][[j]]$f,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f,
  method_s_w = equilibrium$constant$method_s_w ,
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

solution_optim_tj_frictionless <-
  solve_w_f_optim_tj_frictionless(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    beta_f = equilibrium$parameter$beta_f,
    gamma_w = equilibrium$parameter$gamma_w,
    gamma_f = equilibrium$parameter$gamma_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = "approximate",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 10
  )
solution_optim_tj_frictionless$w
solution_optim_tj_frictionless $f

solution_nleqslv_tj_frictionless <-
  solve_w_f_nleqslv_tj_frictionless(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    beta_f = equilibrium$parameter$beta_f,
    gamma_w = equilibrium$parameter$gamma_w,
    gamma_f = equilibrium$parameter$gamma_f,
    lambda_w = equilibrium$parameter$lambda_w,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 10,
    multistart = 10
  )
solution_nleqslv_tj_frictionless$w
solution_nleqslv_tj_frictionless$f

max(
  abs(
    solution_nleqslv_tj_frictionless$w - solution_optim_tj_frictionless$w
  )
)

max(
  abs(
    solution_nleqslv_tj_frictionless$f - solution_optim_tj_frictionless$f
  )
)

endogenous_tj_frictionless_no_fe <-
  solve_endogenous_tj_frictionless(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    gamma_w = equilibrium$parameter$gamma_w,
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    gamma_f = equilibrium$parameter$gamma_f,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    solver = "nleqslv",
    multistart = 20
  ) 

equilibrium_optim_no_fe <-
  solve_equilibrium_tj_frictionless(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "optim",
    multistart = 20
  ) 

equilibrium_optim_no_fe  <-
  solve_equilibrium_tj_frictionless(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  ) 

equilibrium_frictionless_no_fe  <-
  solve_equilibrium_frictionless(
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )
