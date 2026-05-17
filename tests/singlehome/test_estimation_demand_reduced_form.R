
# Initialize --------------------------------------------------------------

rm(list = ls())
gc()

devtools::load_all(".")
library(magrittr)
library(foreach)
library(codetools)
library(ggplot2)


# read data ---------------------------------------------------------------

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

matching_function_estimate <- readRDS(file = "output/estimate_matching_function/matching_function_firmFE.RDS")

# set constant ------------------------------------------------------------

matching_param_spec <- "iv_dist_each_no_sd_names"
matching_params <- 
  matching_function_estimate[[matching_param_spec]]$coefficients %>% 
  as.numeric()


# transform data ----------------------------------------------------------


## construct analysis sample ----------------------------------------------

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

var_names$iv_names <- var_names[[matching_param_spec]]


## add variables ----------------------------------------------------------

### add match per registration --------------------------------------------

data_estimation <-
  data_estimation %>%
  dplyr::mutate(
    Q_per_D_W = Q/D_W
  )
summary(data_estimation$Q_per_D_W)

### make residual ---------------------------------------------------------

fml <- 
  paste(
    var_names$x_names,
    collapse = " + "
  ) %>%
  paste(
    .,
    "| cz + year + firm_id"
  )

data_estimation <-
  data_estimation %>%
  dplyr::mutate(
    y_W_residual =
      fixest::feols(
        data = .,
        fml = 
          paste(
            "y_W ~",
            fml
          ) %>%
          as.formula()
      )$residuals,
    y_F_residual =
      fixest::feols(
        data = .,
        fml = 
          paste(
            "y_F ~",
            fml
          ) %>%
          as.formula()
      )$residuals,
    wage_residual =
      fixest::feols(
        data = .,
        fml = 
          paste(
            "wage ~",
            fml
          ) %>%
          as.formula()
      )$residuals,
    fee_residual =
      fixest::feols(
        data = .,
        fml = 
          paste(
            "fee ~",
            fml
          ) %>%
          as.formula()
      )$residuals
  )



## make subsample ---------------------------------------------------------

data_type_0 <-
  data_estimation %>%
  dplyr::filter(
    type_0 == 1
  )


# estimate demand function ------------------------------------------------

data <- data_type_0

## plot --------------------------------------------------------------------

data %>%
  ggplot(
    aes(
      x = wage,
      y = log(D_W)
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
    ) +
  theme_classic()

data %>%
  ggplot(
    aes(
      x = fee,
      y = log(D_F)
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
  ) +
  theme_classic()

data %>%
  ggplot(
    aes(
      x = wage,
      y = log(Q_per_D_W)
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
  ) +
  theme_classic()


data %>%
  ggplot(
    aes(
      x = wage,
      y = y_W
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
  ) +
  theme_classic()

data %>%
  ggplot(
    aes(
      x = fee,
      y = y_F
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
  ) +
  theme_classic()

data %>%
  ggplot(
    aes(
      x = wage_residual,
      y = y_W_residual
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
  ) +
  theme_classic()

data %>%
  ggplot(
    aes(
      x = fee_residual,
      y = y_F_residual
    )
  ) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    method = "lm", 
    se = FALSE
  ) +
  theme_classic()


## bin plot ---------------------------------------------------------------

binsreg::binsreg(
  y = log(data$D_W),
  x = data$wage
)

binsreg::binsreg(
  y = log(data$D_F),
  x = data$fee
)

binsreg::binsreg(
  y = log(data$Q_per_D_W),
  x = data$wage
)

binsreg::binsreg(
  y = data$y_W,
  x = data$wage
)

binsreg::binsreg(
  y = data$y_F,
  x = data$fee
)

binsreg::binsreg(
  y = data$y_W_residual,
  x = data$wage_residual
)

binsreg::binsreg(
  y = data$y_F_residual,
  x = data$fee_residual
)


## ols ---------------------------------------------------------------------

fml <- 
  paste(
    paste(
      var_names$x_names,
      collapse = " + "
    ),
    "| cz + year + firm_id"
  )

data %>%
  fixest::feols(
    data = .,
    fml = 
      paste(
        "y_W ~ wage + fee +",
        fml
      ) %>%
      as.formula()
  )

data %>%
  fixest::feols(
    data = .,
    fml = 
      paste(
        "y_F ~ wage + fee +",
        fml
      ) %>%
      as.formula()
  )


## iv ---------------------------------------------------------------------

fml <- 
  paste(
    paste(
      var_names$x_names,
      collapse = " + "
    ),
    "| cz + year + firm_id |",
    paste(
      "wage + fee ~",
      paste(
        var_names$iv_dist_selected_names,
        collapse = " + "
      )
    )
  )

fml <- 
  paste(
    paste(
      var_names$x_names,
      collapse = " + "
    ),
    "| cz + year + firm_id |",
    paste(
      "wage + fee ~",
      paste(
        var_names$iv_dist_selected_names,
        collapse = " + "
      )
    )
  )

data %>%
  fixest::feols(
    data = .,
    fml = 
      paste(
        "y_W ~ 1 +",
        fml
      ) %>%
      as.formula()
  )

data %>%
  fixest::feols(
    data = .,
    fml = 
      paste(
        "y_F ~ 1 +",
        fml
      ) %>%
      as.formula()
  )

