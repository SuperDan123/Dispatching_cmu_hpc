
# initialize --------------------------------------------------------------
rm(list = ls())
devtools::load_all(".")
library(foreach)
library(magrittr)
library(doParallel)
registerDoParallel()

# set constants -----------------------------------------------------------

prefix <- "output/multihome/simulate/"
dir.create(
  prefix,
  showWarnings = FALSE
)
seed <- 1
set.seed(1)
n_ths <- 3
n_market <- 100
n_zone <- 2
multistart <- 20
solver <- "nleqslv"
use_rcpp <- TRUE

# solve and simulate equilibrium ------------------------------------------

## set equilibrium object -------------------------------------------------

equilibrium <-
  generate_equilibrium_stochastic(
    n_ths = n_ths,
    n_market = n_market,
    n_zone = n_zone,
    seed = seed
  )

## solve equilibrium ------------------------------------------------------

equilibrium <-
  solve_equilibrium(
    equilibrium = equilibrium,
    solver = solver,
    multistart = multistart
  )

## check the foc ----------------------------------------------------------

df <-
  check_equilibrium(
    equilibrium = equilibrium
  )

df %>%
  dplyr::filter(
    i != 0,
    i != 1,
    i != 2,
  ) %>%
  dplyr::select(
    foc_w,
    foc_f
  ) %>%
  summary()

df %>%
  dplyr::filter(
    i != 0
  ) %>%
  dplyr::select(
    -foc_w,
    -foc_f
  ) %>%
  summary()



# save --------------------------------------------------------------------

saveRDS(
  equilibrium,
  file = 
    paste0(
      prefix,
      "equilibrium.rds"
    )
)
