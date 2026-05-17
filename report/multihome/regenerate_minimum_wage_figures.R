# initialize --------------------------------------------------------------
rm(list = ls())
options(error = stop)

library(ggplot2)
library(foreach)
library(magrittr)

# load functions ----------------------------------------------------------
source(here::here("R", "functions_multihome.R"))

# read data ---------------------------------------------------------------
equilibrium <-
  readRDS(
    here::here(
      "output",
      "multihome",
      "estimate_data",
      "equilibrium_updated_constrained.rds"
    )
  )

updated_ownership <-
  readRDS(
    here::here(
      "output",
      "multihome",
      "make_equilibrium_nationwide_top5_from_data",
      "updated_ownership_matrix.rds"
    )
  )

equilibrium$constant$use_exp <- FALSE

# update ownership --------------------------------------------------------
for (t in seq_along(equilibrium$exogenous)) {
  for (j in seq_along(equilibrium$exogenous[[t]])) {
    equilibrium$exogenous[[t]][[j]]$owner <- updated_ownership[[t]][[j]]$owner
  }
}

# read counterfactual -----------------------------------------------------
counterfactual <-
  readRDS(
    here::here(
      "output",
      "multihome",
      "counterfactual",
      "counterfactual_minimum_wage_combined.rds"
    )
  )

counterfactual_with_real_index <-
  counterfactual %>%
  dplyr::mutate(
    real_index = NA_integer_
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    real_index = {
      i_int <- as.integer(as.character(i))
      if (i_int == 0) {
        0L
      } else if (i_int <= 2) {
        i_int
      } else {
        owner_tj <- equilibrium$exogenous[[t]][[j]]$owner
        firm_col <- i_int
        establishment_rows <- which(owner_tj[, firm_col] == 1)
        establishment_rows[1]
      }
    }
  ) %>%
  dplyr::ungroup()

counterfactual_aggregated <-
  counterfactual_with_real_index %>%
  dplyr::group_by(
    real_index,
    minimum_wage
  ) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        c(
          "w",
          "f",
          "m",
          "s_w",
          "s_f",
          "profit_ths",
          "surplus_w",
          "surplus_f",
          "meeting_probability_w",
          "meeting_probability_f",
          "c_w",
          "c_f"
        )
      ),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    n_markets = dplyr::n(),
    i = dplyr::first(i),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    real_index,
    minimum_wage
  )

# plot and save -----------------------------------------------------------
plot_counterfactual_minimum_wage_outside(
  counterfactual = counterfactual_aggregated
)

plot_counterfactual_minimum_wage_inside(
  counterfactual = counterfactual_aggregated
)

