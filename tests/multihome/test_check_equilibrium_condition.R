
# initialize --------------------------------------------------------------
rm(list = ls())
library(Dispatching)
devtools::load_all(".")
library(foreach)
library(magrittr)
library(codetools)
library(ggplot2)
library(doParallel)
library(plotly)
library(htmlwidgets)
registerDoParallel()

# set constants -----------------------------------------------------------

seed <- 1
set.seed(seed)
n_ths <- 3
n_market <- 1
n_zone <- 1
t <- 1
j <- 1
multistart <- 10
solver <- "nleqslv"

# generate template -------------------------------------------------------

equilibrium <-
  generate_equilibrium(
    n_ths = n_ths,
    n_market = n_market,
    n_zone = n_zone,
    seed = seed
  )

# impose baseline setting -------------------------------------------------

# c_3^F = 0

equilibrium$parameter$gamma_f[3] <- -100
c_f <-
  compute_c_f_tj(
    gamma_f = equilibrium$parameter$gamma_f,
    x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
    ec_f = equilibrium$shock[[t]][[j]]$ec_f,
    use_exp = equilibrium$constant$use_exp
  ) 
c_f
max(
  abs(
    c_f[3]
  )
)

# no private market: mu_1 = 0
equilibrium$shock[[t]][[j]]$mu[1] <- 0
equilibrium$shock[[t]][[j]]$mu
max(
  abs(
    equilibrium$shock[[t]][[j]]$mu[1]
  )
)

# make ths popular

equilibrium$parameter$beta_f[3] <- 10
compute_a_f_tj(
  beta_f = equilibrium$parameter$beta_f,
  x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
  ea_f = equilibrium$shock[[t]][[j]]$ea_f
) 

# check conditions --------------------------------------------------------

check_c1_1(
  equilibrium = equilibrium
) 

compute_a_cap(
  f3 = 0,
  t = t,
  j = j,
  equilibrium = equilibrium
) 

f3_under <- 
  solve_f3_under(
    t = t,
    j = j,
    equilibrium = equilibrium
  )
f3_under

check_c1_3(
  t = t,
  j = j,
  equilibrium = equilibrium
) 

compute_condittion_f3_over(
  f3 = 0,
  t = t,
  j = j,
  equilibrium = equilibrium
) 

compute_b_cap(
  f3 = 1,
  t = t,
  j = j,
  equilibrium = equilibrium
) 

compute_c_cap(
  f3 = 1,
  t = t,
  j = j,
  equilibrium = equilibrium
)

check_ec_lhs(
  t = t,
  j = j,
  equilibrium = equilibrium
)

check_ec_rhs(
  t = t,
  j = j,
  equilibrium = equilibrium
)

check_sec_1(
  t = t,
  j = j,
  equilibrium = equilibrium
)

check_c1_2(
  equilibrium = equilibrium
)

check_sc1_3(
  t = t,
  j = j,
  f3 = 0,
  equilibrium = equilibrium
)

check_sec_2(
  t = t,
  j = j,
  f3 = 0,
  equilibrium = equilibrium
)

check_equilibrium_path_tj(
  t = t,
  j = j,
  equilibrium = equilibrium
)

