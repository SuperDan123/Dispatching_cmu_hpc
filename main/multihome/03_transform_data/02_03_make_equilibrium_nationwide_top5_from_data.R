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
geography <- "nationwide"
prefix <- "output/multihome/make_equilibrium_nationwide_top5_from_data/"
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

# debug ------------------------------------------------------------------------
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

summary(data_base)

data_base_top5 <- 
  make_data_base_nationwide_top5(
    data_base = data_base
  ) 

summary(data_base_top5)


data_base_fringe <-
  make_data_base_fringe_ths(
    data_base = data_base,
    data_base_top5 = data_base_top5
  ) 
summary(data_base_fringe)

# make equilibrium -------------------------------------------------------------

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

# check construction of top5 and fringe ths length <=7
df <- 
  foreach(
    t = seq_along(equilibrium$exogenous),
    .combine = "rbind"
  ) %do%{
    foreach(
      j = seq_along(equilibrium$exogenous[[t]]),
      .combine = "rbind"
    ) %do% { 
      num_vec <- 
        length(equilibrium$endogenous[[t]][[j]]$w)
      df_tj <- 
        data.frame(
          t = t,
          j = j,
          num = num_vec
        )
      return(df_tj)
    }
  }

summary(df)

# check share distribution
equilibrium$constant$use_exp <- FALSE
df <-
  check_equilibrium(
    equilibrium = equilibrium
  )
summary(df)

saveRDS(
  equilibrium,
  paste0(
    prefix,
    "equilibrium_top5_fringe.rds"
  )
)

# update owner matrix --------------------------------------------------------
owner <-
  update_owner(
    data_base = data_base_top5,
    geography = "cz"
  ) 

owner_list <-
  foreach (
    t = seq_along(owner)
  ) %do% {
    owner_list_t <-
      foreach (
        i = seq_along(owner[[t]])
      ) %do% {
        owner_list_t_i <-
          list(
            owner = owner[[t]][[i]]
          )
      }
  }
# add fringe ownership
for (
  t in seq_along(owner_list)
) {
  for (
    i in seq_along(owner_list[[t]])
  ) {
    owner_list[[t]][[i]]$owner <-
      Matrix::bdiag(
        list(
          1,
          owner_list[[t]][[i]]$owner
        )
      ) %>%
      as.matrix()
  }
}
# add private ownership
for (
  t in seq_along(owner_list)
) {
  for (
    i in seq_along(owner_list[[t]])
  ) {
    owner_list[[t]][[i]]$owner <-
      Matrix::bdiag(
        list(
          1,
          owner_list[[t]][[i]]$owner
        )
      ) %>%
      as.matrix()
  }
}


saveRDS(
  owner_list,
  paste0(
    prefix,
    "updated_ownership_matrix.rds"
  )
)
