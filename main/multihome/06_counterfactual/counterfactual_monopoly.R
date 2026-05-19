# initialize ------------------------------------------------------------
rm(list = ls())
library(Dispatching)
library(foreach)
library(magrittr)
library(codetools)
library(ggplot2)
library(kableExtra)
library(doParallel)
registerDoParallel()
print(detectCores())

# import solved equilibrium --------------------------------------------
equilibrium <-
  readRDS(
    file = "output/multihome/estimate_data/equilibrium_updated_constrained.rds" %>%
      here::here()
  )

equilibrium$constant$use_exp <- FALSE

# monopoly counterfactual setup ----------------------------------------
#
# Structural monopoly (instead of zeroing rival mu): rows 1–2 are part-time
# private and fringe; THS platforms start at row `ths_first_row` (3). The
# survivor is the THS row with the largest baseline `s_f`; rows 1–2 are never
# candidates (only `s_f[ths_first_row:nr, ]` is compared). Then drop other THSs
# and align exogenous, shock, and endogenous objects. Owner columns with no
# remaining platform are dropped.
ths_first_row <- 3L

for (
  t in seq_along(equilibrium$exogenous)
) {
  for (
    j in seq_along(equilibrium$exogenous[[t]])
  ) {
    ex <-
      equilibrium$exogenous[[t]][[j]]
    nr <-
      nrow(ex$x_a_w)
    if (
      nr <= ths_first_row
    ) {
      next
    }
    ths <-
      ths_first_row:nr
    if (
      length(ths) <= 1L
    ) {
      next
    }
    own <-
      ex$owner
    end <-
      equilibrium$endogenous[[t]][[j]]
    # ths == ths_first_row:nr only; exclude part-time (row 1) and fringe (row 2).
    # `1L` is the column index of the single-column matrix s_f.
    s_sub <-
      end$s_f[ths, 1L, drop = TRUE]
    survivor <-
      ths[which.max(s_sub)]
    keep_rows <-
      c(
        1L,
        2L,
        survivor
      )
    ex$x_a_w <-
      ex$x_a_w[keep_rows, , drop = FALSE]
    ex$x_a_f <-
      ex$x_a_f[keep_rows, , drop = FALSE]
    ex$x_c_w <-
      ex$x_c_w[keep_rows, , drop = FALSE]
    ex$x_c_f <-
      ex$x_c_f[keep_rows, , drop = FALSE]
    own_k <-
      own[keep_rows, , drop = FALSE]
    keep_cols <-
      which(
        colSums(abs(own_k)) > 1e-10
      )
    if (
      length(keep_cols) == 0L
    ) {
      stop(
        "owner has no active columns after restricting to dominant THS"
      )
    }
    ex$owner <-
      own_k[, keep_cols, drop = FALSE]
    if (
      length(ex$size_w) == nr
    ) {
      ex$size_w <-
        ex$size_w[keep_rows]
    }
    if (
      length(ex$size_f) == nr
    ) {
      ex$size_f <-
        ex$size_f[keep_rows]
    }
    sh <-
      equilibrium$shock[[t]][[j]]
    for (
      nm in c("mu", "ea_w", "ea_f", "ec_w", "ec_f")
    ) {
      sh[[nm]] <-
        sh[[nm]][keep_rows, , drop = FALSE]
    }
    en <-
      equilibrium$endogenous[[t]][[j]]
    for (
      nm in c("w", "f", "s_w", "s_f")
    ) {
      en[[nm]] <-
        en[[nm]][keep_rows, , drop = FALSE]
    }
    equilibrium$exogenous[[t]][[j]] <-
      ex
    equilibrium$shock[[t]][[j]] <-
      sh
    equilibrium$endogenous[[t]][[j]] <-
      en
  }
}

# correct bugs ---------------------------------------------------------
for (
  t in seq_along(equilibrium$exogenous)
) {
  for (
    j in seq_along(equilibrium$exogenous[[t]])
  ) {
    
  equilibrium[["exogenous"]][[t]][[j]]$w_0 <- equilibrium[["exogenous"]][[t]][[j]]$w_0[1]
  }
}

# time a single market-zone solve --------------------------------------
# `solve_equilibrium_tj` returns the modified equilibrium; R does not update the
# caller's `equilibrium` object in place. Assign the return value back, or you
# will still see the pre-solve endogenous values on `equilibrium`.

equilibrium_1 <-
  solve_equilibrium_tj(
    t = 1,
    j = 29,
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )


# run counterfactual for all markets -----------------------------------
# Same rule as `solve_equilibrium_tj`: use the returned object
# (`equilibrium_monopoly`); the argument `equilibrium` in the calling
# environment is not updated in place.
equilibrium_monopoly <-
  solve_equilibrium(
    equilibrium = equilibrium,
    solver = "nleqslv",
    multistart = 20
  )

# save equilibrium_monopoly --------------------------------------------
dir.create(
  "output/multihome/counterfactual/monopoly",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  equilibrium_monopoly,
  file = "output/multihome/counterfactual/monopoly/equilibrium_benchmark_monopoly.rds" %>%
    here::here()
)

cat(
  "Saved equilibrium_monopoly to output/multihome/counterfactual/monopoly/equilibrium_benchmark_monopoly.rds\n"
)
