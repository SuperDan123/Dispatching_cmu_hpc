
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

# c_2^F = 0

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

# solve example -----------------------------------------------------------

# check equilibrium condition

check_c1_1(
  equilibrium = equilibrium
) 

check_sec_1(
  t = t,
  j = j,
  equilibrium = equilibrium
)

# plot foc ---------------------------------------------------------------

c_w <- 
  compute_c_w_tj(
    gamma_w = equilibrium$parameter$gamma_w,
    x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
    ec_w = equilibrium$shock[[t]][[j]]$ec_w,
    use_exp = equilibrium$constant$use_exp
  )

f3_range <-
  seq(
    from = equilibrium$exogenous[[t]][[j]]$w_0 + c_w[3],
    30,
    by = 0.1
  )

g <-
  f3_range %>%
  purrr::map(
    function(f3) {
      fz <- equilibrium$endogenous[[t]][[j]]$f
      fz[3] <- f3
      s_f_t <-
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
          f = fz,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f
        )
      return(
        tibble::tibble(
          f3 = f3,
          i = 1:nrow(s_f_t),
          s_f_1 = s_f_t %>% as.numeric(),
        )
      )
    }
  ) %>%
  dplyr::bind_rows()
g %>%
  ggplot(
    aes(
      x = f3,
      y = s_f_1,
      color = as.factor(i)
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()

g <-
  f3_range %>%
  purrr::map(
    function(f3) {
      cap_a <-
        compute_a_cap(
          f3 = f3,
          t = t,
          j = j,
          equilibrium = equilibrium
        )
      cap_b <-
        compute_b_cap(
          f3 = f3,
          t = t,
          j = j,
          equilibrium = equilibrium
        )
      return(
        tibble::tibble(
          f3 = f3,
          cap_a = cap_a,
          cap_b = cap_b
        )
      )
    }
  ) %>%
  dplyr::bind_rows()

g %>%
  ggplot(
    aes(
      x = f3,
      y = cap_a
    )
  ) +
  geom_line() +
  theme_classic()

g %>%
  ggplot(
    aes(
      x = f3,
      y = cap_b
    )
  ) +
  geom_line() +
  theme_classic()

g %>%
  tidyr::pivot_longer(
    cols = c(cap_a, cap_b),
    names_to = "cap",
    values_to = "value"
  ) %>%
  dplyr::filter(
    f3 > 15
  ) %>%
  ggplot(
    aes(
      x = f3,
      y = value,
      color = cap
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()

