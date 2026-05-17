
# Initialize --------------------------------------------------------------

rm(list = ls())
gc()

devtools::load_all(".")
library(magrittr)
library(foreach)
library(codetools)


# Read data ---------------------------------------------------------------

data_establishment <- readRDS("output/data_establishments.RDS")
num_parttemp <- readRDS("output/num_parttemp_cz.RDS")
num_est <- readRDS("output/num_establishments_cz.RDS")
distance_iv <- readRDS("output/distance_iv.RDS")
distance_iv_each <- readRDS("output/distance_iv_each.RDS")
hausman_iv <- readRDS("output/hausman_iv.RDS")
rivals_iv <- readRDS("output/num_rivals_iv.RDS")
iv_selected <- readRDS("output/iv_selected.RDS")

partwage_cz <- readRDS("output/partwage_cz.RDS")
mw <- readRDS("cleaned/data_pref_year_minimum_wage.RDS")

# Set constant ------------------------------------------------------------

matching_params <- c(0.5, 0.5)
baseline_firm <- "log(Q) ~ log(D_W) + log(D_F) | year + firm_id + cz | 0 | establishment_id"
baseline_establishment <- "log(Q) ~ log(D_W) + log(D_F) | year + establishment_id + cz | 0 | establishment_id"

# Construct analysis sample -----------------------------------------------

data_estimation <-
  transform_data_estimation(
    data_establishment,
    num_parttemp,
    num_est,
    partwage_cz,
    mw,
    distance_iv,
    distance_iv_each,
    rivals_iv,
    hausman_iv,
    matching_params
  )

var_names <-
  make_names_estimation(
    data_estimation,
    iv_selected
  ) 

# Estimate matching function ----------------------------------------------

## Firm FE ----------------------------------------------------------------

result <-
  estimate_matching_params(
    data_estimation = data_estimation,
    var_names = var_names,
    baseline = baseline_firm
  )


wald_test <- conduct_wald_test_matching_params(result) 


result <-
  estimate_matching_params_wrapper(
    data_estimation = data_estimation,
    var_names = var_names,
    baseline = baseline_firm
  ) 

result %>%
  purrr::map(summary)

## Establishment FE -------------------------------------------------------

result <-
  estimate_matching_params(
    data_estimation = data_estimation,
    var_names = var_names,
    baseline = baseline_establishment
  )

wald_test <- conduct_wald_test_matching_params(result) 

result <-
  estimate_matching_params_wrapper(
    data_estimation = data_estimation,
    var_names = var_names,
    baseline = baseline_establishment
  ) 

result %>%
  purrr::map(summary)



