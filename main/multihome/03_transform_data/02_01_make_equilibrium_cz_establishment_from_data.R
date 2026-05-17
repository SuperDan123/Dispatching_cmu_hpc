# initialize ------------------------------------------------------------
rm(list = ls())
library(Dispatching)
library(foreach)
library(magrittr)
library(codetools)
library(ggplot2)

# set constant ----------------------------------------------------------

method_s_w <- "exact"
margin <- 1e-10
quadrature_size <- 21
tol <- 1e-16
minimum_wage <- FALSE
geography <- "cz"
prefix <- "output/make_equilibrium_from_data/"
dir.create(
  prefix,
  recursive = TRUE,
  showWarnings = FALSE
)

# load data --------------------------------------------------------------

data_establishment <- 
  readRDS(
    "cleaned/data_establishments.RDS"
  )
data_zipcode <- 
  readRDS(
    "cleaned/data_zipcode.RDS"
  )
data_area <- 
  readRDS(
    "cleaned/data_area.RDS"
  )
data_pref_year_minimum_wage <- 
  readRDS(
    "cleaned/data_pref_year_minimum_wage.RDS"
  )
data_year <- 
  readRDS(
    "cleaned/data_year.RDS"
  )
data_area_year_num_parttemp <-
  readRDS(
    "cleaned/data_area_year_num_parttemp.RDS"
  )
data_area_year_num_firm <-
  readRDS(
    "cleaned/data_area_year_num_firms.RDS"
  )
data_area_year_num_labor <-
  readRDS(
    "cleaned/data_area_year_num_labor.RDS"
  )
data_area_year_partwage <-
  readRDS(
    "cleaned/data_area_year_partwage.RDS"
  )
 
# make equilibrium -------------------------------------------------------

## all markets

equilibrium <-
  make_equilibrium_from_data(
    data_establishment = data_establishment,
    data_zipcode = data_zipcode,
    data_area = data_area,
    data_pref_year_minimum_wage = data_pref_year_minimum_wage,
    data_year = data_year,
    data_area_year_num_parttemp = data_area_year_num_parttemp,
    data_area_year_num_firm = data_area_year_num_firm,
    data_area_year_num_labor = data_area_year_num_labor,
    data_area_year_partwage = data_area_year_partwage,
    method_s_w = method_s_w,
    margin = margin,
    quadrature_size = quadrature_size,
    tol = tol,
    minimum_wage = minimum_wage,
    geography = geography
  ) 

saveRDS(
  equilibrium,
    paste0(
      prefix,
      "equilibrium.rds"
    )
)

## markets with ths no greater than 10

data_establishment_10 <-
  data_establishment %>%
  dplyr::left_join(
    data_zipcode,
    by = "zipcode"
  ) %>%
  dplyr::left_join(
    data_area,
    by = "area_code"
  ) %>%
  dplyr::group_by(
    year,
    cz
  ) %>%
  dplyr::mutate(
    num = length(id_unique)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    num <= 10
  ) %>%
  dplyr::select(
    -area_code,
    -cz,
    -num
  )

equilibrium_10 <-
  make_equilibrium_from_data(
    data_establishment = data_establishment_10,
    data_zipcode = data_zipcode,
    data_area = data_area,
    data_pref_year_minimum_wage = data_pref_year_minimum_wage,
    data_year = data_year,
    data_area_year_num_parttemp = data_area_year_num_parttemp,
    data_area_year_num_firm = data_area_year_num_firm,
    data_area_year_num_labor = data_area_year_num_labor,
    data_area_year_partwage = data_area_year_partwage,
    method_s_w = method_s_w,
    margin = margin,
    quadrature_size = quadrature_size,
    tol = tol,
    minimum_wage = minimum_wage,
    geography = geography
  ) 

saveRDS(
  equilibrium_10,
    paste0(
      prefix,
      "equilibrium_10.rds"
    )
)
