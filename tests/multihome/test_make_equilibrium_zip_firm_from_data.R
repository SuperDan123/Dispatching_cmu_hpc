# initialize ------------------------------------------------------------
rm(list = ls())
devtools::load_all(".")
library(foreach)
library(magrittr)
library(codetools)
library(ggplot2)

reference <-
  generate_equilibrium(
    n_ths = 2,
    n_market = 1,
    n_zone = 1,
    seed = 1
  )

# set constant ----------------------------------------------------------

prefix <- "output/multihome/test_make_equilibrium_zipcode_firm_from_data/"
method_s_w <- "exact"
margin <- 1e-10
quadrature_size <- 21
tol <- 1e-16
minimum_wage <- FALSE
geography <- "zipcode"

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

# check data -------------------------------------------------------------

data_establishment %>%
  dim()

data_establishment %>% summary()

data_establishment %>%
  dplyr::select(
    tokutei
  ) %>% 
  summary()

data_establishment %>%
  dplyr::select(
    wage,
    fee 
  ) %>%
  summary()

data_establishment %>%
  dplyr::filter(
    tokutei == 0
  ) %>%
  dplyr::select(
    wage,
    fee 
  ) %>%
  summary()

 
# make equilibrium -------------------------------------------------------

## make base data --------------------------------------------------------

data_area_year_num_firm_expand <-
  interpolate_year(
    data_area_year = data_area_year_num_firm
  ) 

data_cz_year <-
  make_data_cz_year( 
    data_area_year_num_parttemp = data_area_year_num_parttemp,
    data_area_year_num_firm = data_area_year_num_firm, 
    data_area_year_num_labor = data_area_year_num_labor,
    data_area_year_partwage = data_area_year_partwage
  )

data_cz_year  

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
  select_data(
    df = df
  ) 

data_base <-
  make_data_base(
    data_establishment = data_establishment,
    data_zipcode = data_zipcode,
    data_area = data_area,
    data_pref_year_minimum_wage = data_pref_year_minimum_wage,
    data_year = data_year,
    data_area_year_num_parttemp = data_area_year_num_parttemp,
    data_area_year_num_firm = data_area_year_num_firm,
    data_area_year_num_labor = data_area_year_num_labor,
    data_area_year_partwage = data_area_year_partwage
  ) 

data_base %>%
  summary()

data_base %>%
  purrr::map(
    ~ is.na(.) %>%
      any()
  ) %>%
  purrr::keep(
    ~ . == TRUE
  )

data_base %>%
  dplyr::group_by(
    cz,
    year
  ) %>%
  dplyr::summarise(
    parttime = 
      parttime %>%
      is.na() %>%
      any(),
    temp = 
      temp %>%
      is.na() %>%
      any(),
    ptwage =
      ptwage %>%
      is.na() %>%
      any()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(
    dplyr::across(
      .cols = c(
        parttime,
        temp,
        ptwage
      ),
      .fns = ~ sum(.) / length(.)
    )
  ) 

data_base %>%
  dplyr::group_by(
    year,
    !!rlang::sym(geography)
  ) %>%
  dplyr::summarise(
    size_w = sd(size_w),
    size_f = sd(size_f)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    is.finite(size_w),
    size_w > 0
  )

data_base <-
  make_data_base_zipcode_firm(
    data_base = data_base
  )

data_base %>%
  summary() 

data_base %>%
  dplyr::group_by(
    year,
    !!rlang::sym(geography)
  ) %>%
  dplyr::summarise(
    size_w = sd(size_w),
    size_f = sd(size_f)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    is.finite(size_w),
    size_w > 0
  )

data_base %>%
  purrr::map(
    ~ is.na(.) %>%
      any()
  ) %>%
  purrr::keep(
    ~ . == TRUE
  )

# correct

g <-
  binsreg::binsreg(
    y = log(data_base$s_w),
    x = data_base$w
  ) 

g$bins_plot +
  theme_classic()

g <-
  binsreg::binsreg(
    y = log(data_base$s_f),
    x = data_base$f
  )

g$bins_plot +
  theme_classic()

# how about this?

g <-
  binsreg::binsreg(
    y = log(data_base$q / data_base$size_w),
    x = data_base$w
  ) 

g$bins_plot +
  theme_classic()


# wrong

g <-
  binsreg::binsreg(
    y = log(data_base$register / data_base$size_w),
    x = data_base$w
  ) 

g$bins_plot +
  theme_classic()


# check if higher than 1
k <- 5
data_base %>%
  dplyr::mutate(
    check = q / (k * register)
  ) %>%
  dplyr::summarise(
    check = sum(check >= 1) / length(check)
  )


## make endogenous -------------------------------------------------------

reference$endogenous

w_list <-
  make_object_list(
    data_base = data_base,
    object = "w",
    geography = geography
  ) 

f_list <-
  make_object_list(
    data_base = data_base,
    object = "f",
    geography = geography
  ) 

s_w_list <-
  make_object_list(
    data_base = data_base,
    object = "s_w",
    geography = geography
  ) 

s_f_list <-
  make_object_list(
    data_base = data_base,
    object = "s_f",
    geography = geography
  ) 

endogenous <-
  make_endogenous_from_data(
    data_base = data_base,
    geography = geography
  )

check <-
  add_parttime_to_w(
    data_base = data_base,
    endogenous = endogenous,
    geography = geography
  ) 

check <-
  add_parttime_to_f(
    data_base = data_base,
    endogenous = endogenous,
    geography = geography
  )

check <-
  add_parttime_to_s_w(
    data_base = data_base,
    endogenous = endogenous,
    geography = geography
  )

check <-
  add_parttime_to_s_f(
    data_base = data_base,
    endogenous = endogenous,
    geography = geography
  )

endogenous <-
  add_parttime_to_endogenous(
    data_base = data_base,
    endogenous = endogenous,
    geography = geography
  )

## make exogenous --------------------------------------------------------

reference$exogenous

df <-
  make_type_dummy(
    data_base = data_base
  ) 

x_a_w_list <-
  make_x_a_w(
    data_base = data_base,
    geography = geography
  ) 

x_a_f_list <-
  make_x_a_f(
    data_base = data_base,
    geography = geography
  )

x_c_w_list <-
  make_x_c_w(
    data_base = data_base,
    geography = geography
  ) 

x_c_f_list <-
  make_x_c_f(
    data_base = data_base,
    geography = geography
  )

size_w_list <-
  make_object_list(
    data_base = data_base,
    object = "size_w",
    geography = geography
  ) 

size_w <-
  size_w_list$value %>%
  purrr::map_depth(
    2,
    ~ unique(.)
  )

size_f_list <-
  make_object_list(
    data_base = data_base,
    object = "size_f",
    geography = geography
  ) 

size_f <-
  size_f_list$value %>%
  purrr::map_depth(
    2,
    ~ unique(.)
  )

w_0_list <-
  make_object_list(
    data_base = data_base,
    object = "w_0",
    geography = geography
  ) 

w_0 <-
  w_0_list$value %>%
  purrr::map_depth(
    2,
    ~ unique(.)
  )

f_0_list <-
  make_object_list(
    data_base = data_base,
    object = "f_0",
    geography = geography
  ) 

f_0 <-
  f_0_list$value %>%
  purrr::map_depth(
    2,
    ~ unique(.)
  ) 

owner <-
  make_owner(
    data_base = data_base,
    geography = geography
  ) 

exogenous <-
  make_exogenous_from_data(
    data_base = data_base,
    geography = geography
  ) 

check <-
  add_zero_to_top(
    object = exogenous[[1]][[1]]$x_a_w
  )

exogenous <-
  add_parttime_to_exogenous(
    exogenous = exogenous,
    geography = geography
  )

## make shock ------------------------------------------------------------

reference$shock 

shock_zero <-
  make_shock_zero(
    data_base = data_base,
    geography = geography
  ) 

shock <-
  make_shock_from_data(
    data_base = data_base,
    geography = geography
  ) 

shock <-
  add_parttime_to_shock(
    shock = shock,
    geography = geography
  )
   
## make parameter -------------------------------------------------------

reference$parameter

coef <-
  make_coef(
    x = exogenous[[1]][[1]]$x_a_w,
    geography = geography
  )

parameter <-
  make_parameter_from_data(
    exogenous = exogenous,
    geography = geography
  ) 

## make equilibrium -------------------------------------------------------

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

equilibrium <-
  readRDS(
    paste0(
      prefix,
      "equilibrium.rds"
    )
  )

# check na

equilibrium %>% 
  purrr::map(
    ~ unlist(.) %>% 
      is.na(.) %>% 
      any()
  )

equilibrium$exogenous %>% 
  purrr::map(
    ~ unlist(.) %>% 
      is.na(.) %>% 
      any()
  )

equilibrium$exogenous[[1]][[1]] %>% 
  purrr::map(
    ~ unlist(.) %>% 
      is.na(.) %>% 
      any()
  )

equilibrium$exogenous[[1]][[1]]$x_c_f

equilibrium$endogenous %>% 
  purrr::map(
    ~ unlist(.) %>% 
      is.na(.) %>% 
      any()
  )

equilibrium$endogenous[[1]] %>% 
  purrr::map(
    ~ unlist(.) %>% 
      is.na(.) %>% 
      any()
  ) %>%
  unlist() %>%
  which()

equilibrium$endogenous[[1]][[3595]]