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
      "counterfactual_maximum_margin_combined.rds"
    )
  )

# add real_index ----------------------------------------------------------
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
  dplyr::ungroup() %>%
  dplyr::filter(
    !is.na(real_index)
  )

# aggregate across markets ------------------------------------------------
avg_cols <-
  c(
    "w",
    "f",
    "m"
  )
share_cols <-
  c(
    "s_w",
    "s_f"
  )
sum_cols <-
  c(
    "profit_ths",
    "surplus_w",
    "surplus_f"
  )
mean_cols <-
  c(
    "augmented_foc_w",
    "augmented_foc_f",
    "foc_w",
    "foc_f",
    "complementarity",
    "foc_s_f",
    "eta_w",
    "eta_f",
    "meeting_probability_w",
    "meeting_probability_f",
    "c_w",
    "c_f"
  )

counterfactual_aggregated <-
  counterfactual_with_real_index %>%
  dplyr::group_by(
    real_index,
    maximum_markup
  ) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::any_of(avg_cols),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    dplyr::across(
      dplyr::any_of(share_cols),
      ~ sum(.x, na.rm = TRUE) / dplyr::n(),
      .names = "{.col}"
    ),
    dplyr::across(
      dplyr::any_of(sum_cols),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    dplyr::across(
      dplyr::any_of(mean_cols),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    n_markets = dplyr::n(),
    i = dplyr::first(i),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    i,
    maximum_markup
  )

# select common grid of maximum_markup ------------------------------------
counterfactual_aggregated_plot <-
  counterfactual_aggregated %>%
  dplyr::group_by(real_index) %>%
  dplyr::arrange(
    maximum_markup,
    .by_group = TRUE
  ) %>%
  dplyr::mutate(
    rank = dplyr::row_number(),
    n_total = dplyr::n()
  ) %>%
  dplyr::group_modify(
    ~ {
      target_ranks <-
        round(
          seq(
            1,
            .x$n_total[1],
            length.out = 10
          )
        )
      .x %>%
        dplyr::filter(
          rank %in% target_ranks
        )
    }
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -rank,
    -n_total
  ) %>%
  dplyr::arrange(
    i,
    maximum_markup
  )

make_cap_median_iqr <-
  function(
    x,
    k = 2
  ) {
    q25 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q75 <- stats::quantile(x, 0.75, na.rm = TRUE)
    median_val <- stats::median(x, na.rm = TRUE)
    iqr <- q75 - q25
    upper_bound <- median_val + k * iqr
    lower_bound <- median_val - k * iqr
    x_capped <-
      pmax(
        pmin(x, upper_bound),
        lower_bound
      )
    return(x_capped)
  }

counterfactual_aggregated_plot_inside <-
  counterfactual_aggregated_plot %>%
  dplyr::mutate(
    profit_ths = make_cap_median_iqr(profit_ths),
    surplus_w  = make_cap_median_iqr(surplus_w),
    surplus_f  = make_cap_median_iqr(surplus_f)
  )

# plot and save -----------------------------------------------------------
plot_counterfactual_maximum_margin_outside(
  counterfactual = counterfactual_aggregated_plot
)

plot_counterfactual_maximum_margin_inside(
  counterfactual = counterfactual_aggregated_plot_inside
)

