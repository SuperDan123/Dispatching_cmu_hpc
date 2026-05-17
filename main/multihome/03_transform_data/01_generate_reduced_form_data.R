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

# merge data -----------------------------------------------------
data_area_year_num_firm_expand <-
  interpolate_year(
    data_area_year = data_area_year_num_firm
  ) 

data_area_year_num_labor_expand <-
  interpolate_year(
    data_area_year = data_area_year_num_labor
  ) 

data_area_year_parttemp_expand <-
  interpolate_year(
    data_area_year = data_area_year_num_parttemp
  ) 

data_cz_year <-
  make_data_cz_year( 
    data_area_year_num_parttemp = data_area_year_num_parttemp,
    data_area_year_num_firm = data_area_year_num_firm, 
    data_area_year_num_labor = data_area_year_num_labor,
    data_area_year_partwage = data_area_year_partwage
  )

data_cz_year  

data_cz_year %>%
  dplyr::filter(
    is.finite(
      num_parttemp
    )
  )

df <-
  data_establishment %>%
  dplyr::left_join(
    data_zipcode,
    by = "zipcode"
  ) %>%
  dplyr::left_join(
    data_area,
    by = "area_code"
  ) %>%
  dplyr::left_join(
    data_pref_year_minimum_wage,
    by = c(
      "pref",
      "year"
    )
  ) %>%
  dplyr::left_join(
    data_year,
    by = "year"
  ) %>%
  dplyr::left_join(
    data_cz_year,
    by = c(
      "cz",
      "year"
    )
  ) 

df <-
  df %>%
  dplyr::filter(
    tokutei == 0
  ) %>%
  dplyr::filter(
    wage > 0, 
    fee > 0,
    sales > 0,
    client > 0,
    register > 0,
    (tempfixed + tempperm) >= 5,
    fee > wage
  ) %>%
  dplyr::filter(
    wage <= 
      quantile(
        wage, 
        prob = 0.99, 
        na.rm = TRUE
      ),
    wage >= 
      quantile(
        wage, 
        prob = 0.01, 
        na.rm = TRUE
      ),
    fee <= 
      quantile(
        fee, 
        prob = 0.99, 
        na.rm = TRUE
      ),
    fee >= 
      quantile(
        fee, 
        prob = 0.01, 
        na.rm = TRUE
      )
  ) %>%
  dplyr::filter(
    firm_id != "",
    zipcode != "",
    zipcode != "000"
  ) %>%
  dplyr::filter(
    is.finite(num_parttemp)
  )

