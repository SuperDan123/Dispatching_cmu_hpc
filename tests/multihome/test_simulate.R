# initialize --------------------------------------------------------------
rm(list = ls())
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

prefix <- "test_simulate"
dir.create(
  paste(
    "figuretable",
    prefix,
    sep = "/"
  ),
  showWarnings = FALSE
)
seed <- 1
set.seed(1)
n_ths <- 3
n_market <- 1
n_zone <- 1

constant <- 
  set_constant(
    n_ths = n_ths,
    n_market = n_market,
    n_zone = n_zone
  )

t <- 1
j <- 1
u <- 0

# set parameters ----------------------------------------------------------

parameter <-
  set_parameter(
    constant = constant
  )

# set shocks --------------------------------------------------------------

shock <-
  generate_shock_zero(
    constant = constant,
    parameter = parameter
  ) 
shock

# set exogenous variables -------------------------------------------------

exogenous <-
  generate_exogenous(
    constant = constant
  ) 
exogenous


# compute cost ------------------------------------------------------------

c_w <- 
  compute_c_w_tj(
    gamma_w = parameter$gamma_w,
    x_c_w = exogenous[[t]][[j]]$x_c_w,
    ec_w = shock[[t]][[j]]$ec_w,
    use_exp = constant$use_exp
  ) 
c_w

c_f <-
  compute_c_f_tj(
    gamma_f = parameter$gamma_f,
    x_c_f = exogenous[[t]][[j]]$x_c_f,
    ec_f = shock[[t]][[j]]$ec_f,
    use_exp = constant$use_exp
  ) 
c_f

# set endogenous variables ------------------------------------------------

endogenous <-
  generate_endogenous(
    constant = constant,
    parameter = parameter,
    exogenous = exogenous,
    shock = shock
  ) 

# set equilibrium object --------------------------------------------------

equilibrium <-
  generate_equilibrium(
    n_ths = n_ths,
    n_market = n_market,
    n_zone = n_zone,
    seed = seed
  )

# solve endogenous --------------------------------------------------------

## solve firm decisions ----------------------------------------------------

### compute mean utility --------------------------------------------------

check_1 <-
  compute_a_f_tj(
    beta_f = equilibrium$parameter$beta_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f
  ) 
check_2 <-
  compute_a_f_tj_rcpp(
    beta_f = equilibrium$parameter$beta_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f
  ) 
max(
  abs(
    check_1 - check_2
  )
)

check_1 <-
  compute_h_f_tj(
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = equilibrium$endogenous[[t]][[j]]$f
  )
check_2 <-
  compute_h_f_tj_rcpp(
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = equilibrium$endogenous[[t]][[j]]$f
  )
max(
  abs(
    check_1 - check_2
  )
)

### evaluate s_f equilibrium condition ------------------------------------

check_1 <-
  compute_condition_s_f_numerator_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = equilibrium$endogenous[[t]][[j]]$f
  ) 
check_2 <-
  compute_condition_s_f_numerator_tj_rcpp(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = equilibrium$endogenous[[t]][[j]]$f
  ) 
max(
  abs(
    check_1 - check_2
  )
)

check_1 <-
  compute_condition_s_f_denominator_tj(
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
check_2 <-
  compute_condition_s_f_denominator_tj_rcpp(
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
max(
  abs(
    check_1 - check_2
  )
)

check_1 <-
  compute_condition_s_f_tj(
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
check_2 <-
  compute_condition_s_f_tj_rcpp(
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
max(
  abs(
    check_1 - check_2
  )
)


### solve for s_f ---------------------------------------------------------

f_z <- equilibrium$endogenous[[t]][[j]]$f
f_z[3] <- 1

s_f_tj <-
  solve_s_f_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_f = equilibrium$parameter$beta_f,
    lambda_f = equilibrium$parameter$lambda_f,
    x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_f = equilibrium$shock[[t]][[j]]$ea_f,
    f = f_z,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

s_f_tj_rcpp <-
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
    f = f_z,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

max(
  abs(
    s_f_tj - s_f_tj_rcpp
  )
)

solve_s_f(
  equilibrium = equilibrium
) 

df <-
  seq(
    0,
    2,
    by = 0.1
  ) %>%
  purrr::map(
    function(z) {
      f_z <- equilibrium$endogenous[[t]][[j]]$f
      f_z[3] <- z
      solution <-
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
          f = f_z,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f
        )
      df_t <-
        data.frame(
          f_3 = f_z[3],
          ths = 1:length(f_z) %>% as.factor(),
          share = solution
        )
      return(df_t)
    }
  )  %>%
  dplyr::bind_rows()

df %>%
  ggplot(
    aes(
      x = f_3,
      y = share,
      color = ths
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()


## solve worker decisions --------------------------------------------------

met_list <-
  rje::powerSet(
    x = 1:nrow(equilibrium$endogenous[[t]][[j]]$s_f),
    m = nrow(equilibrium$endogenous[[t]][[j]]$s_f) 
  )

met_first <- met_list[[1]]
met_last <- met_list[[length(met_list)]]

### compute the components ------------------------------------------------

a_w <-
  compute_a_w_tj(
    beta_w = equilibrium$parameter$beta_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w
  )

a_w_rcpp <-
  compute_a_w_tj_rcpp(
    beta_w = equilibrium$parameter$beta_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w
  ) 

max(
  abs(
    a_w - a_w_rcpp
  )
)

# translate
h_w <-
  compute_h_w_tj(
    a_w = a_w,
    lambda_w = equilibrium$parameter$lambda_w,
    w = equilibrium$endogenous[[t]][[j]]$w
  )
h_w

h_w_rcpp <-
  compute_h_w_tj_rcpp(
    a_w = a_w,
    lambda_w = equilibrium$parameter$lambda_w,
    w = equilibrium$endogenous[[t]][[j]]$w
  )

max(
  abs(
    h_w - h_w_rcpp
  )
)

meeting_number <- 
  compute_meeting_number_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 
meeting_number

meeting_number_rcpp <-
  compute_meeting_number_tj_rcpp(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

max(
  abs(
    meeting_number - meeting_number_rcpp
  )
) 

# translate
meeting_probability_w <-
  compute_meeting_probability_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 
meeting_probability_w

meeting_probability_w_rcpp <-
  compute_meeting_probability_w_tj_rcpp(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

max(
  abs(
    meeting_probability_w - meeting_probability_w_rcpp
  )
)

omega <-
  compute_omega_tj(
    meeting_probability_w = meeting_probability_w,
    met = met_last
  ) 

omega_rcpp <-
  compute_omega_tj_rcpp(
    meeting_probability_w = meeting_probability_w,
    met = met_last
  )

max(
  abs(
    omega - omega_rcpp
  )
)

rho <-
  compute_rho_tj(
    h_w = h_w,
    meeting_probability_w = meeting_probability_w,
    met = met_last
  )

rho_rcpp <-
  compute_rho_tj_rcpp(
    h_w = h_w,
    meeting_probability_w = meeting_probability_w,
    met = met_last
  )

max(
  abs(
    rho - rho_rcpp
  )
)

# translate
s_w_met <-
  compute_s_w_met(
    meeting_probability_w = meeting_probability_w,
    h_w = h_w,
    met = met_last
  ) 

s_w_met_rcpp <-
  compute_s_w_met_rcpp(
    meeting_probability_w = meeting_probability_w,
    h_w = h_w,
    met = met_last
  )

max(
  abs(
    s_w_met - s_w_met_rcpp
  )
)

compute_omega_tj(
  meeting_probability_w = meeting_probability_w,
  met = met_first
) 

compute_rho_tj(
  h_w = h_w,
  meeting_probability_w = meeting_probability_w,
  met = met_first
)

compute_s_w_met(
  meeting_probability_w = meeting_probability_w,
  h_w = h_w,
  met = met_first
) 

df <-
  seq(
    0,
    2,
    by = 0.1
  ) %>%
  purrr::map(
    function(z) {
      w_z <- equilibrium$endogenous[[t]][[j]]$w
      w_z[3] <- z
      h_w_z <-
        compute_h_w_tj(
          a_w = a_w,
          lambda_w = equilibrium$parameter$lambda_w,
          w = w_z
        )
      solution <-
        compute_s_w_met(
          meeting_probability_w = meeting_probability_w,
          h_w = h_w_z,
          met = met_last
        ) 
      df_t <-
        data.frame(
          w_3 = w_z[3],
          ths = 1:length(w_z) %>% as.factor(),
          share = solution
        )
      return(df_t)
    }
  )  %>%
  dplyr::bind_rows()

df %>%
  ggplot(
    aes(
      x = w_3,
      y = share,
      color = ths
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()

### solve s_w exactly -----------------------------------------------------

# translate
check_1 <-
  solve_s_w_tj_from_a_w_exact(
    a_w = a_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

check_1_rcpp <-
  solve_s_w_tj_from_a_w_exact_rcpp(
    a_w = a_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

max(
  abs(
    check_1 - check_1_rcpp
  )
)

check_2 <-
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
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 

max(
  abs(
    check_1 - check_2
  )
)

check_time <- 
  4:6 %>%
  purrr::map(
    function (size) {
      x_a_w_size <- 
        rep(
          list(equilibrium$exogenous[[t]][[j]]$x_a_w),
          size
        ) %>%
        purrr::reduce(rbind)
      mu_size <- 
        rep(
          list(equilibrium$shock[[t]][[j]]$mu),
          size
        ) %>%
        purrr::reduce(rbind)
      ea_w_size <- 
        rep(
          list(equilibrium$shock[[t]][[j]]$ea_w),
          size
        ) %>%
        purrr::reduce(rbind)
      w_size <- 
        rep(
          list(equilibrium$endogenous[[t]][[j]]$w),
          size
        ) %>%
        purrr::reduce(rbind)
      s_f_size <- 
        rep(
          list(equilibrium$endogenous[[t]][[j]]$s_f),
          size
        ) %>%
        purrr::reduce(rbind)
      s_f_size <-
        s_f_size / sum(s_f_size)
      time <-
        system.time(
          s_w_size <-
            solve_s_w_tj_exact(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              lambda_w = equilibrium$parameter$lambda_w,
              x_a_w = x_a_w_size,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              mu = mu_size,
              ea_w = ea_w_size,
              w = w_size,
              s_f = s_f_size
            ) 
        )
      result <-
        data.frame(
          size = nrow(s_f_size),
          time = time["elapsed"] %>% as.numeric()
        )
      return(result)
    }
  ) %>%
  dplyr::bind_rows() 

check_time %>%
  ggplot(
    aes(
      x = size,
      y = log(time)
    )
  ) +
  geom_point() +
  geom_line() +
  theme_classic()


### solve s_w approximately -----------------------------------------------

# make full

full <-
  compute_h_w_meetng_probability_full(
    a_w = a_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 
full

full_rcpp <-
  compute_h_w_meetng_probability_full_rcpp(
    a_w = a_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  )

max(
  abs(
    full$h_w_full - full_rcpp$h_w_full
  )
)

max(
  abs(
    full$meeting_probability_w_full - full_rcpp$meeting_probability_w_full
  )
)

# compute distribution of utility
d <-
  compute_utility_distribution(
    u = u, 
    utility = log(full$h_w_full)
  ) 

d_rcpp <-
  compute_utility_distribution_rcpp(
    u = u, 
    utility = log(full$h_w_full)
  ) 

max(
  abs(
    d - d_rcpp
  )
)

# compute density of utility
d <-
  compute_utility_density(
    u = u, 
    utility = log(full$h_w_full)
  ) 

d_rcpp <-
  compute_utility_density_rcpp(
    u = u, 
    utility = log(full$h_w_full)
  ) 

max(
  abs(
    d - d_rcpp
  )
)

# compute integrand
integrand <-
  compute_integrand(
    u = u, 
    utility = log(full$h_w_full), 
    consideration = full$meeting_probability_w_full
  ) 

integrand_rcpp <-
  compute_integrand_rcpp(
    u = u, 
    utility = log(full$h_w_full), 
    consideration = full$meeting_probability_w_full
  )

max(
  abs(
    integrand - integrand_rcpp
  )
)

# auxiliary functions to find upper and lower margin of utility
f_upper <-
  compute_f_upper(
    u = u, 
    utility = log(full$h_w_full), 
    margin = equilibrium$constant$margin
  )

f_upper_rcpp <-
  compute_f_upper_rcpp(
    u = u, 
    utility = log(full$h_w_full), 
    margin = equilibrium$constant$margin
  )

max(
  abs(
    f_upper - f_upper_rcpp
  )
)

f_lower <-
  compute_f_lower(
    u = u, 
    utility = log(full$h_w_full), 
    margin = equilibrium$constant$margin
  )

f_lower_rcpp <-
  compute_f_lower_rcpp(
    u = u, 
    utility = log(full$h_w_full), 
    margin = equilibrium$constant$margin
  )

max(
  abs(
    f_lower - f_lower_rcpp
  )
)

a <- min(log(full$h_w_full)) - 30
b <- max(log(full$h_w_full)) + 30

compute_f_upper(
  u = a,
  utility = log(full$h_w_full),
  margin = equilibrium$constant$margin
)

compute_f_upper(
  u = b,
  utility = log(full$h_w_full),
  margin = equilibrium$constant$margin
)

compute_f_lower(
  u = a,
  utility = log(full$h_w_full),
  margin = equilibrium$constant$margin
)

compute_f_lower(
  u = b,
  utility = log(full$h_w_full),
  margin = equilibrium$constant$margin
)

# compute choice probability
choice_probability <-
  compute_choice_probability_with_consideration(
    utility = log(full$h_w_full), 
    consideration = full$meeting_probability_w_full, 
    margin = equilibrium$constant$margin, 
    quadrature_size = equilibrium$constant$quadrature_size, 
    tol = equilibrium$constant$tol
  ) 

choice_probability_rcpp <-
  compute_choice_probability_with_consideration_rcpp(
    utility = log(full$h_w_full), 
    consideration = full$meeting_probability_w_full, 
    margin = equilibrium$constant$margin, 
    quadrature_size = equilibrium$constant$quadrature_size, 
    tol = equilibrium$constant$tol
  )

max(
  abs(
    choice_probability - choice_probability_rcpp
  )
)

# compute s_w approximately
# translate
check_1 <- 
  solve_s_w_tj_from_a_w_approximate(
    a_w = a_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

check_1_rcpp <-
  solve_s_w_tj_from_a_w_approximate_rcpp(
    a_w = a_w,
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    lambda_w = equilibrium$parameter$lambda_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
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
  solve_s_w_tj_approximate(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

max(
  abs(
    check_1 - check_2
  )
)

# compare exact and approximate

s_w_exact <-
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
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 

s_w_approximate <-
  solve_s_w_tj_approximate(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  )

cbind(
  s_w_exact,
  s_w_approximate
)

check_accuracy <-
  seq(
    from = 3,
    to = 50,
    by = 1
  ) %>%
  purrr::map(
    function (n) {
      s_w_approximate <-
        solve_s_w_tj_approximate(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = equilibrium$endogenous[[t]][[j]]$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          margin = equilibrium$constant$margin,
          quadrature_size = n,
          tol = equilibrium$constant$tol
        )
      out <-
        cbind(
          n, 
          s_w_approximate %>% t()
        ) %>%
        magrittr::set_colnames(
          c(
            "n",
            paste0(
              "s_w_",
              1:nrow(s_w_approximate)
            )
          )
        ) %>%
        as.data.frame()
      return(out)
    }
  ) %>%
  dplyr::bind_rows()

check_accuracy %>%
  ggplot(
    aes(
      x = n,
      y = s_w_1
    )
  ) +
  geom_line(
    color = "red"
  ) +
  geom_hline(
    yintercept = s_w_exact[1],
    color = "blue",
    linetype = "dashed"
  ) +
  theme_classic()


check_accuracy %>%
  ggplot(
    aes(
      x = n,
      y = s_w_2
    )
  ) +
  geom_line(
    color = "red"
  ) +
  geom_hline(
    yintercept = s_w_exact[2],
    color = "blue",
    linetype = "dashed"
  ) +
  theme_classic()

# check time
check_time <-
  4:9 %>%
  purrr::map(
    function (size) {
      x_a_w_size <- 
        rep(
          list(equilibrium$exogenous[[t]][[j]]$x_a_w),
          size
        ) %>%
        purrr::reduce(rbind)
      mu_size <- 
        rep(
          list(equilibrium$shock[[t]][[j]]$mu),
          size
        ) %>%
        purrr::reduce(rbind)
      ea_w_size <- 
        rep(
          list(equilibrium$shock[[t]][[j]]$ea_w),
          size
        ) %>%
        purrr::reduce(rbind)
      w_size <- 
        rep(
          list(equilibrium$endogenous[[t]][[j]]$w),
          size
        ) %>%
        purrr::reduce(rbind)
      s_f_size <- 
        rep(
          list(equilibrium$endogenous[[t]][[j]]$s_f),
          size
        ) %>%
        purrr::reduce(rbind)
      s_f_size <-
        s_f_size / sum(s_f_size)
      time <-
        system.time(
          s_w_size <-
            solve_s_w_tj_approximate(
              m_w = equilibrium$parameter$  m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              lambda_w = equilibrium$parameter$lambda_w,
              x_a_w = x_a_w_size,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              mu = mu_size,
              ea_w = ea_w_size,
              w = w_size,
              s_f = s_f_size,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol
            ) 
        )
      result <-
        data.frame(
          size = nrow(s_f_size),
          time = time["elapsed"] %>% as.numeric()
        )
      return(result)
    }
  ) %>%
  dplyr::bind_rows() 

check_time %>%
  ggplot(
    aes(
      x = size,
      y = time
    )
  ) +
  geom_point() +
  geom_line() +
  theme_classic()

solve_s_w_exact(
  equilibrium = equilibrium
) 

solve_s_w_approximate(
  equilibrium = equilibrium
)

df <-
  seq(
    0,
    2,
    by = 0.1
  ) %>%
  purrr::map(
    function(z) {
      w_z <- equilibrium$endogenous[[t]][[j]]$w
      w_z[3] <- z
      s_w_exact <-
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
          w = w_z,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f
        )
      df_t <-
        data.frame(
          w_3 = w_z[3],
          ths = 1:length(w_z) %>% as.factor(),
          share = s_w_exact
        )
      return(df_t)
    }
  )  %>%
  dplyr::bind_rows()

df %>%
  ggplot(
    aes(
      x = w_3,
      y = share,
      color = ths
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()


df <-
  seq(
    0,
    2,
    by = 0.1
  ) %>%
  purrr::map(
    function(z) {
      w_z <- equilibrium$endogenous[[t]][[j]]$w
      w_z[3] <- z
      s_w_approximate <-
        solve_s_w_tj_approximate(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_z,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
      df_t <-
        data.frame(
          w_3 = w_z[3],
          ths = 1:length(w_z) %>% as.factor(),
          share = s_w_approximate
        )
      return(df_t)
    }
  )  %>%
  dplyr::bind_rows()

df %>%
  ggplot(
    aes(
      x = w_3,
      y = share,
      color = ths
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()


## solve ths decisions ----------------------------------------------------

### compute derivatives ---------------------------------------------------

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
s_f_d_f

s_w_d_w_exact <-
  solve_s_w_d_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  ) 
s_w_d_w_exact

s_w_d_w_approximate <-
  solve_s_w_d_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    beta_w = equilibrium$parameter$beta_w,
    lambda_w = equilibrium$parameter$lambda_w,
    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
    w = equilibrium$endogenous[[t]][[j]]$w,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    method_s_w = equilibrium$constant$method_s_w,
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  ) 
s_w_d_w_approximate

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
s_w_d_f_exact

s_w_d_f_approximate <-
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
    method_s_w = "approximate",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol
  ) 
s_w_d_f_approximate

meeting_number_d_f <- 
  compute_meeting_number_d_f_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    s_f_d_f = s_f_d_f
  ) 
meeting_number_d_f

### compute first order conditions ----------------------------------------

compute_foc_w_tj(
  owner = equilibrium$exogenous[[t]][[j]]$owner,
  c_w = c_w,
  w = equilibrium$endogenous[[t]][[j]]$w,
  f = equilibrium$endogenous[[t]][[j]]$f,
  s_w = s_w_exact,
  s_w_d_w = s_w_d_w_exact
) 

compute_foc_w_tj(
  owner = equilibrium$exogenous[[t]][[j]]$owner,
  c_w = c_w,
  w = equilibrium$endogenous[[t]][[j]]$w,
  f = equilibrium$endogenous[[t]][[j]]$f,
  s_w = s_w_approximate,
  s_w_d_w = s_w_d_w_approximate
) 

compute_foc_f_tj(
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
  s_w = s_w_exact,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f,
  s_w_d_f = s_w_d_f_exact,
  s_f_d_f = s_f_d_f
) 

compute_foc_f_tj(
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
  s_w = s_w_approximate,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f,
  s_w_d_f = s_w_d_f_approximate,
  s_f_d_f = s_f_d_f
) 

compute_foc_tj(
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
  method_s_w = "exact",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

compute_foc_tj(
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
  method_s_w = "approximate",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

## solve monopoly w and f -------------------------------------------------

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
  w = equilibrium$endogenous[[t]][[j]]$w,
  f = equilibrium$endogenous[[t]][[j]]$f,
  s_w = equilibrium$endogenous[[t]][[j]]$s_w,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f,
  use_exp = equilibrium$constant$use_exp
) 

solve_profit_ths_tj(
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
  method_s_w = "exact",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 


solve_profit_ths_tj(
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
  method_s_w = "approximate",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

df <-
  seq(
    1,
    - 10 / equilibrium$parameter$lambda_f,
    by = 1
  ) %>%
  purrr::map(
    function(z) {
      w_z <- 
        rep(
          z,
          nrow(equilibrium$endogenous[[t]][[j]]$w)
        )
      f_z <-
        w_z + 1
      solution <-
        solve_profit_ths_tj(
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
          w = w_z,
          f = f_z,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          method_s_w = "exact",
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol,
          use_exp = equilibrium$constant$use_exp
        ) 
      df_t <-
        data.frame(
          z = z,
          ths = 1:length(equilibrium$endogenous[[t]][[j]]$w) %>% as.factor(),
          y = solution
        )
      return(df_t)
    }
  )  %>%
  dplyr::bind_rows()

df %>%
  ggplot(
    aes(
      x = z,
      y = y,
      color = ths
    )
  ) +
  geom_line() +
  scale_color_viridis_d() +
  theme_classic()

solve_w_f_monopoly_itj(
  i = 3,
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
  use_exp = equilibrium$constant$use_exp
) 

solve_w_f_monopoly_itj(
  i = 3,
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
  use_exp = equilibrium$constant$use_exp
) 

solve_w_f_monopoly_tj(
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
  use_exp = equilibrium$constant$use_exp
)

solve_w_f_monopoly_tj(
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
  use_exp = equilibrium$constant$use_exp
)

## solve oligopoly w and f -------------------------------------------------

bound <- 
  compute_monopoly_bound_tj(
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
    use_exp = equilibrium$constant$use_exp
  )


bound <-
  relax_bound_tj(
    bound = bound
  ) 

w_f <- 
  transform_x_to_w_f_optim(
    x = bound$lower,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f
  )
w_f
    
solution_optim_tj <-
  solve_w_f_optim_tj(
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

solve_w_f_optim_tj(
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
  seed = 10
)

initial_w_f <-
  check_initial_value(
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    lower = bound$lower,
    upper = bound$upper
  ) 

x <-
  transform_w_f_to_x_nleqslv(
    w = initial_w_f$w,
    f = initial_w_f$f,
    lower = bound$lower,
    upper = bound$upper
  )

w_f <-
  transform_x_to_w_f_nleqslv(
    x = x,
    w = initial_w_f$w,
    f = initial_w_f$f,
    lower = bound$lower,
    upper = bound$upper
  ) 

max(
  abs(
    w_f$w - initial_w_f$w
  )
)

max(
  abs(
    w_f$f - initial_w_f$f
  )
)

solve_w_f_nleqslv_tj(
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
  seed = 10
)

solution_nleqslv_tj <-
  solve_w_f_nleqslv_tj(
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
    seed = 10
  )

max(
  abs(
    solution_nleqslv_tj$w - solution_optim_tj$w
  )
)

max(
  abs(
    solution_nleqslv_tj$f - solution_optim_tj$f
  )
)

x <-
  transform_w_f_to_x_bestresponse_itj(
    i = 3,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    w = solution_nleqslv_tj$w,
    f = solution_nleqslv_tj$f,
    lower = bound$lower,
    upper = bound$upper
  )

w_f <-
  transform_x_to_w_f_bestresponset_itj(
    x = x,
    i = 3,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    w = solution_nleqslv_tj$w,
    f = solution_nleqslv_tj$f,
    lower = bound$lower,
    upper = bound$upper
 ) 

max(
  abs(
    w_f$w - solution_nleqslv_tj$w
  )
)

max(
  abs(
    w_f$f - solution_nleqslv_tj$f
  )
)

solution_bestresponse_itj <-
  solve_w_f_bestresponse_itj(
    i = 3,
    lower = bound$lower,
    upper = bound$upper,
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
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_bestresponse_itj$w - solution_nleqslv_tj$w
  )
)

max(
  abs(
    solution_bestresponse_itj$f - solution_nleqslv_tj$f
  )
)

solution_bestresponse_tj <-
  solve_w_f_bestrsponse_tj(
    lower = bound$lower,
    upper = bound$upper,
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
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_bestresponse_tj$w - solution_nleqslv_tj$w
  )
)

max(
  abs(
    solution_bestresponse_tj$f - solution_nleqslv_tj$f
  )
)

solution_iteration_tj <-
  solve_w_f_iteration_tj(
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
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_iteration_tj$w - solution_nleqslv_tj$w
  )
)

max(
  abs(
    solution_iteration_tj$f - solution_nleqslv_tj$f
  )
)


# solve equilibrium -------------------------------------------------------

endogenous_tj <-
  solve_endogenous_tj(
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
    solver = "optim",
    multistart = 20
  ) 

equilibrium <-
  solve_equilibrium_tj(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "optim",
    multistart = 20
  ) 

equilibrium <-
  solve_equilibrium_tj(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  ) 

equilibrium <-
  solve_equilibrium(
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

compute_f_upper_bound(
  beta_f = equilibrium$parameter$beta_f,
  lambda_f = equilibrium$parameter$lambda_f,
  x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
  ea_f = equilibrium$shock[[t]][[j]]$ea_f,
  initial_f = 0, 
  target_h_f = 1.5e-15, 
  step_size = 0.1, 
  max_iter = 1.0e4
)

# compute surplus ---------------------------------------------------------
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
  w = equilibrium$endogenous[[t]][[j]]$w,
  f = equilibrium$endogenous[[t]][[j]]$f,
  s_w = equilibrium$endogenous[[t]][[j]]$s_w,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f,
  use_exp = equilibrium$constant$use_exp
) 

compute_meeting_probability_f_tj(
  m_w = equilibrium$parameter$m_w,
  m_f = equilibrium$parameter$m_f,
  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
  mu = equilibrium$shock[[t]][[j]]$mu,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f
)

compute_surplus_f_tj(
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

h_w <-
  compute_h_w_tj(
    a_w = a_w,
    lambda_w = equilibrium$parameter$lambda_w,
    w = equilibrium$endogenous[[t]][[j]]$w
  )

meeting_probability_w <-
  compute_meeting_probability_w_tj(
    m_w = equilibrium$parameter$m_w,
    m_f = equilibrium$parameter$m_f,
    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
    mu = equilibrium$shock[[t]][[j]]$mu,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f
  ) 

compute_inclusive_value_w_tj(
  lambda_w = equilibrium$parameter$lambda_w,
  h_w = h_w,
  meeting_probability_w = meeting_probability_w,
  met = 1
) 

compute_surplus_w_tj_met(
  lambda_w = equilibrium$parameter$lambda_w,
  meeting_probability_w = meeting_probability_w,
  h_w = h_w,
  met = 1
)

compute_surplus_w_tj_exact(
  m_w = equilibrium$parameter$m_w,
  m_f = equilibrium$parameter$m_f,
  beta_w = equilibrium$parameter$beta_w,
  lambda_w = equilibrium$parameter$lambda_w,
  x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
  mu = equilibrium$shock[[t]][[j]]$mu,
  ea_w = equilibrium$shock[[t]][[j]]$ea_w,
  w = equilibrium$endogenous[[t]][[j]]$w,
  s_f = equilibrium$endogenous[[t]][[j]]$s_f
) 

# run comparative statics -------------------------------------------------

check_equilibrium_tj(
  t = t,
  j = j,
  equilibrium = equilibrium
) 

evaluate_equilibrium_tj(
  x = 0.1,
  target = "m_w",
  t = t,
  j = j,
  equilibrium = equilibrium,
  solver = "optim",
  multistart = 20
) 

evaluate_equilibrium_tj(
  x = 0.1,
  target = "m_w",
  t = t,
  j = j,
  equilibrium = equilibrium,
  solver = "nleqslv",
  multistart = 20
) 

comparative <- 
  evaluate_comparative_tj(
    target = "m_w",
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

# run counterfactual ------------------------------------------------------

## impose minimum wage ----------------------------------------------------
compute_foc_minimum_wage_tj(
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
  eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
  method_s_w = "exact",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

compute_foc_minimum_wage_tj(
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
  eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
  method_s_w = "approximate",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
)

bound <- 
  compute_monopoly_bound_tj(
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
    use_exp = equilibrium$constant$use_exp
  )

bound <-
  relax_bound_minimum_wage_tj(
    bound = bound,
    w_0 = equilibrium$exogenous[[t]][[j]]$w_0
  ) 

x <-
  transform_eta_w_f_to_x_minimum_wage_nleqslv(
    eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    lower = bound$lower,
    upper = bound$upper
  ) 

eta_w_f <-
  transform_x_to_eta_w_f_minimum_wage_nleqslv(
    x = x,
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    lower = bound$lower,
    upper = bound$upper
  ) 

max(
  abs(
    eta_w_f$eta_w - equilibrium$endogenous[[t]][[j]]$eta_w
  )
)

max(
  abs(
    eta_w_f$w - equilibrium$endogenous[[t]][[j]]$w
  )
)

max(
  abs(
    eta_w_f$f - equilibrium$endogenous[[t]][[j]]$f
  )
)


# non-binding
solution_minimum_wage_nleqslv_tj <-
  solve_w_f_minimum_wage_nleqslv_tj(
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
    eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 10
  )

max(
  abs(
    solution_minimum_wage_nleqslv_tj$w - equilibrium$endogenous[[t]][[j]]$w
  )
)

max(
  abs(
    solution_minimum_wage_nleqslv_tj$f - equilibrium$endogenous[[t]][[j]]$f
  )
)

# binding
w_0_binding <-
  equilibrium$endogenous[[t]][[j]]$w[3:length(equilibrium$endogenous[[t]][[j]]$w)] %>%
  max() +
  0.1

solution_minimum_wage_nleqslv_tj <-
  solve_w_f_minimum_wage_nleqslv_tj(
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
    w_0 = w_0_binding,
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
    eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 20
  )

max(
  abs(
    solution_minimum_wage_nleqslv_tj$w[3] - w_0_binding
  )
)

max(
  abs(
    solution_minimum_wage_nleqslv_tj$f - equilibrium$endogenous[[t]][[j]]$f
  )
)

solution_minimum_wage_iteration_tj <-
  solve_w_f_minimum_wage_iteration_tj(
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
    w_0 = w_0_binding,
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
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_minimum_wage_iteration_tj$w - solution_minimum_wage_nleqslv_tj$w
  )
)

max(
  abs(
    solution_minimum_wage_iteration_tj$f - solution_minimum_wage_nleqslv_tj$f
  )
)

equilibrium_minimum_wage_nleqslv <-
  solve_equilibrium_minimum_wage_tj(
    t = t,
    j = j,
    eta_w = 1e-16,
    equilibrium = equilibrium, 
    solver = "nleqslv",
    multistart = 20
  )

equilibrium_minimum_wage_iteration <-
  solve_equilibrium_minimum_wage_tj(
    t = t,
    j = j,
    eta_w = 1e-16,
    equilibrium = equilibrium, 
    solver = "iteration",
    multistart = 20
  )

check_foc_w_shape_minimum_wage_tj(
  equilibrium = equilibrium_minimum_wage_nleqslv,
  t = t,
  j = j,
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  i = 3
) 

check_foc_f_shape_minimum_wage_tj(
  equilibrium = equilibrium_minimum_wage_nleqslv,
  t = t,
  j = j,
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  i = 3
)

check_profit_w_shape_minimum_wage_tj(
  equilibrium = equilibrium_minimum_wage_nleqslv,
  t = t,
  j = j,
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  i = 3
) 

check_profit_f_shape_minimum_wage_tj(
  equilibrium = equilibrium_minimum_wage_nleqslv,
  t = t,
  j = j,
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  i = 3
) 

check_profit_w_shape_minimum_wage_tj(
  equilibrium = equilibrium_minimum_wage_iteration,
  t = t,
  j = j,
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  i = 3
) 

check_profit_f_shape_minimum_wage_tj(
  equilibrium = equilibrium_minimum_wage_iteration,
  t = t,
  j = j,
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  i = 3
) 

df <- 
  check_equilibrium_minimum_wage_tj(
    equilibrium = equilibrium_minimum_wage_nleqslv,
    t = t,
    j = j
  ) 
df

evaluate_equilibrium_minimum_wage_tj(
  w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
  t = t,
  j = j,
  equilibrium = equilibrium,
  solver = "nleqslv",
  multistart = 20
)

counterfactual <- 
  evaluate_counterfactual_minimum_wage_tj(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

g <- 
  plot_counterfactual_minimum_wage(
    counterfactual = counterfactual
  )

## impose maximum margin--------------------------------------------------

m_bar <- 0.99

initial_w_f <-
  check_initial_value_maximum_margin(
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
  )

x <-
  transform_eta_w_f_to_x_maximum_margin(
    eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
    w = initial_w_f$w,
    f = initial_w_f$f
  ) 

bound <-
  relax_bound_maximum_margin_tj(
    bound = bound
  ) 

eta_w_f <-
  transform_x_to_eta_w_f_maximum_margin(
    x = x,
    w = initial_w_f$w,
    f = initial_w_f$f
  ) 

max(
  abs(
    eta_w_f$eta_f - equilibrium$endogenous[[t]][[j]]$eta_f
  )
)

max(
  abs(
    eta_w_f$w - initial_w_f$w
  )
)

max(
  abs(
    eta_w_f$f - initial_w_f$f
  )
)

compute_foc_maximum_margin_tj(
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
  eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
  m_bar = m_bar,
  method_s_w = "exact",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

compute_foc_maximum_margin_tj(
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
  eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
  m_bar = m_bar,
  method_s_w = "approximate",
  margin = equilibrium$constant$margin,
  quadrature_size = equilibrium$constant$quadrature_size,
  tol = equilibrium$constant$tol,
  use_exp = equilibrium$constant$use_exp
) 

# non-binding
m_bar <- 0.99

initial_w_f <-
  check_initial_value_maximum_margin(
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
  )

solution_maximum_margin_nloptr_tj <-
  solve_w_f_maximum_margin_nloptr_tj(
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
    eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
    m_bar = m_bar,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 1
  ) 

max(
  abs(
    solution_maximum_margin_nloptr_tj$w - initial_w_f$w
  )
)

max(
  abs(
    solution_maximum_margin_nloptr_tj$f - initial_w_f$f
  )
)

# binding
m_bar <- 0.3

initial_w_f <-
  check_initial_value_maximum_margin(
    w = equilibrium$endogenous[[t]][[j]]$w,
    f = equilibrium$endogenous[[t]][[j]]$f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
  )

solution_maximum_margin_nloptr_tj <-
  solve_w_f_maximum_margin_nloptr_tj(
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
    eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
    m_bar = m_bar,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 20
  ) 

max(
  abs(
    solution_maximum_margin_nloptr_tj$w - initial_w_f$w
  )
)

max(
  abs(
    solution_maximum_margin_nloptr_tj$f - initial_w_f$f
  )
)

m <-
  (
    solution_maximum_margin_nloptr_tj$f -
    solution_maximum_margin_nloptr_tj$w
  ) / solution_maximum_margin_nloptr_tj$f
m <-
  m[3:length(m)] 

max(
  m - m_bar
)

x <-
  transform_eta_w_f_to_x_maximum_margin_nleqslv(
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    eta_f = solution_maximum_margin_nloptr_tj$eta_f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
  ) 

eta_w_f <-
  transform_x_to_eta_w_f_maximum_margin_nleqslv(
    x = x,
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
  ) 

max(
  abs(
    eta_w_f$eta_f - solution_maximum_margin_nloptr_tj$eta_f
  )
)

max(
  abs(
    eta_w_f$w - solution_maximum_margin_nloptr_tj$w
  )
)

max(
  abs(
    eta_w_f$f - solution_maximum_margin_nloptr_tj$f
  )
)

solution_maximum_margin_nleqslv_tj <-
  solve_w_f_maximum_margin_nleqslv_tj(
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
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    eta_f = solution_maximum_margin_nloptr_tj$eta_f,
    m_bar = m_bar,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp,
    seed = 10
  )

max(
  abs(
    solution_maximum_margin_nleqslv_tj$eta_f - solution_maximum_margin_nloptr_tj$eta_f
  )
)

max(
  abs(
    solution_maximum_margin_nleqslv_tj$w - solution_maximum_margin_nloptr_tj$w
  )
)

max(
  abs(
    solution_maximum_margin_nleqslv_tj$f - solution_maximum_margin_nloptr_tj$f
  )
)

m <-
  (
    solution_maximum_margin_nleqslv_tj$f -
      solution_maximum_margin_nleqslv_tj$w
  ) / solution_maximum_margin_nleqslv_tj$f
m <-
  m[3:length(m)] 

max(
  m - m_bar
)

x <-
  transform_w_f_to_x_maximum_margin_bestresponse_itj(
    i = 3,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
  )

w_f <-
  transform_x_to_w_f_maximum_margin_bestresponse_itj(
    x = x,
    i = 3,
    owner = equilibrium$exogenous[[t]][[j]]$owner,
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    eta_f = solution_maximum_margin_nloptr_tj$eta_f,
    lower = bound$lower,
    upper = bound$upper,
    m_bar = m_bar
 ) 

max(
  abs(
    w_f$w - solution_maximum_margin_nloptr_tj$w
  )
)

max(
  abs(
    w_f$f - solution_maximum_margin_nloptr_tj$f
  )
)

solution_maximum_margin_bestresponse_itj <-
  solve_w_f_maximum_margin_bestresponse_itj(
    i = 3,
    lower = bound$lower,
    upper = bound$upper,
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
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    eta_f = solution_maximum_margin_nloptr_tj$eta_f,
    m_bar = m_bar,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_maximum_margin_bestresponse_itj$w - solution_maximum_margin_nloptr_tj$w
  )
)

max(
  abs(
    solution_maximum_margin_bestresponse_itj$f - solution_maximum_margin_nloptr_tj$f
  )
)

solution_maximum_margin_bestresponse_tj <-
  solve_w_f_maximum_margin_bestresponse_tj(
    lower = bound$lower,
    upper = bound$upper,
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
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    eta_f = solution_maximum_margin_nloptr_tj$eta_f,
    m_bar = m_bar,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_maximum_margin_bestresponse_tj$w - solution_maximum_margin_nloptr_tj$w
  )
)

max(
  abs(
    solution_maximum_margin_bestresponse_tj$f - solution_maximum_margin_nloptr_tj$f
  )
)

solution_maximum_margin_iteration_tj <-
  solve_w_f_maximum_margin_iteration_tj(
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
    w = solution_maximum_margin_nloptr_tj$w,
    f = solution_maximum_margin_nloptr_tj$f,
    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
    eta_f = solution_maximum_margin_nloptr_tj$eta_f,
    m_bar = m_bar,
    method_s_w = "exact",
    margin = equilibrium$constant$margin,
    quadrature_size = equilibrium$constant$quadrature_size,
    tol = equilibrium$constant$tol,
    use_exp = equilibrium$constant$use_exp
  ) 

max(
  abs(
    solution_maximum_margin_iteration_tj$w - solution_maximum_margin_nloptr_tj$w
  )
)

max(
  abs(
    solution_maximum_margin_iteration_tj$f - solution_maximum_margin_nloptr_tj$f
  )
)

equilibrium_maximum_margin_nloptr <-
  solve_equilibrium_maximum_margin_tj(
    t = t,
    j = j,
    eta_f = 1e-16,
    equilibrium = equilibrium,
    solver = "nloptr",
    m_bar = m_bar,
    multistart = 5
  )

equilibrium_maximum_margin_nleqslv <-
  solve_equilibrium_maximum_margin_tj(
    t = t,
    j = j,
    eta_f = 1e-16,
    equilibrium = equilibrium,
    solver = "nleqslv",
    m_bar = m_bar,
    multistart = 20
  )

max(
  abs(
    equilibrium_maximum_margin_nloptr$endogenous[[t]][[j]]$eta_f - 
    equilibrium_maximum_margin_nleqslv$endogenous[[t]][[j]]$eta_f
  )
)

max(
  abs(
    equilibrium_maximum_margin_nloptr$endogenous[[t]][[j]]$w - 
    equilibrium_maximum_margin_nleqslv$endogenous[[t]][[j]]$w
  )
)

max(
  abs(
    equilibrium_maximum_margin_nloptr$endogenous[[t]][[j]]$f - 
    equilibrium_maximum_margin_nleqslv$endogenous[[t]][[j]]$f
  )
)

equilibrium_maximum_margin_iteration <-
  solve_equilibrium_maximum_margin_tj(
    t = t,
    j = j,
    eta_f = 1e-16,
    equilibrium = equilibrium,
    solver = "iteration",
    m_bar = m_bar,
    multistart = 20
  )

max(
  abs(
    equilibrium_maximum_margin_iteration$endogenous[[t]][[j]]$w - 
    equilibrium_maximum_margin_nloptr$endogenous[[t]][[j]]$w
  )
)

max(
  abs(
    equilibrium_maximum_margin_iteration$endogenous[[t]][[j]]$f - 
    equilibrium_maximum_margin_nloptr$endogenous[[t]][[j]]$f
  )
)


check_foc_w_shape_maximum_margin_tj(
  equilibrium = equilibrium_maximum_margin_nloptr,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
) 

check_foc_f_shape_maximum_margin_tj(
  equilibrium = equilibrium_maximum_margin_nloptr,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
)

check_foc_w_shape_maximum_margin_tj(
  equilibrium = equilibrium_maximum_margin_nleqslv,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
) 

check_foc_f_shape_maximum_margin_tj(
  equilibrium = equilibrium_maximum_margin_nleqslv,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
)

check_profit_w_shape_maximum_margin(
  equilibrium = equilibrium_maximum_margin_nloptr,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
) 

check_profit_f_shape_maximum_margin(
  equilibrium = equilibrium_maximum_margin_nloptr,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
)

check_profit_w_shape_maximum_margin(
  equilibrium = equilibrium_maximum_margin_nleqslv,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
) 

check_profit_f_shape_maximum_margin(
  equilibrium = equilibrium_maximum_margin_nleqslv,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
)

check_profit_w_shape_maximum_margin(
  equilibrium = equilibrium_maximum_margin_iteration,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
) 

check_profit_f_shape_maximum_margin(
  equilibrium = equilibrium_maximum_margin_iteration,
  t = t,
  j = j,
  m_bar = m_bar,
  i = 3
)

check_equilibrium_maximum_margin_tj(
  equilibrium = equilibrium_maximum_margin_nloptr,
  t = t,
  j = j,
  m_bar = m_bar
) 

evaluate_equilibrium_maximum_margin_tj(
  m_bar = m_bar,
  t = t,
  j = j,
  equilibrium = equilibrium,
  solver = "nleqslv",
  multistart = 20
) 

counterfactual <- 
  evaluate_counterfactual_maximum_margin_tj(
    t = t,
    j = j,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

g <- 
  plot_counterfactual_maximum_margin(
    counterfactual = counterfactual
  )

