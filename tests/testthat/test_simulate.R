library(testthat)
library(foreach)
library(magrittr)
library(doParallel)

test_equilibrium <- NULL  # Initialize at file scope

test_that("simulate functions work correctly", {
  # Set up parallel processing
  registerDoParallel()
  
  # Set constants
  prefix <- "test_simulate"
  dir.create(
    paste(
      "figuretable",
      prefix,
      sep = "/"
    ),
    showWarnings = FALSE
  )
  seed <- 1
  set.seed(1)
  n_ths <- 2
  n_market <- 1
  n_zone <- 1
  
  constant <- 
    set_constant(
      n_ths = n_ths,
      n_market = n_market,
      n_zone = n_zone
    )
  
  t <- 1
  j <- 1
  
  # Set parameters
  parameter <-
    set_parameter(
      constant = constant
    )
  
  # Set shocks
  shock <-
    generate_shock_zero(
      constant = constant
    )
  expect_type(shock, "list")
  
  # Set exogenous variables
  exogenous <-
    generate_exogenous(
      constant = constant
    )
  expect_type(exogenous, "list")
  
  # Compute cost
  c_w <- 
    compute_c_w_tj(
      gamma_w = parameter$gamma_w,
      x_c_w = exogenous[[t]][[j]]$x_c_w,
      ec_w = shock[[t]][[j]]$ec_w
    )
  expect_type(c_w, "double")
  
  c_f <-
    compute_c_f_tj(
      gamma_f = parameter$gamma_f,
      x_c_f = exogenous[[t]][[j]]$x_c_f,
      ec_f = shock[[t]][[j]]$ec_f
    )
  expect_type(c_f, "double")
  
  # Set endogenous variables
  endogenous <-
    generate_endogenous(
      constant = constant,
      parameter = parameter,
      exogenous = exogenous,
      shock = shock
    )
  expect_type(endogenous, "list")
  
  # Set equilibrium object
  equilibrium <-
    generate_equilibrium(
      n_ths = n_ths,
      n_market = n_market,
      n_zone = n_zone,
      seed = seed
    )

  test_equilibrium <<- equilibrium  # Use <<- to assign to parent scope
  expect_type(equilibrium, "list")
})

test_that("firm decision functions work correctly", {
  equilibrium <- test_equilibrium
  t <- 1
  j <- 1

  # Test mean utility computations
  check_1 <- 
    compute_a_f_tj(
      beta_f = equilibrium$parameter$beta_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f
    )
    
  check_2 <- 
    compute_a_f_tj_rcpp(
      beta_f = equilibrium$parameter$beta_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f
    )
  
  expect_equal(check_1, check_2)

  check_1 <- 
    compute_h_f_tj(
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f
  )
  
  check_2 <- 
    compute_h_f_tj_rcpp(
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f
  )
  
  expect_equal(check_1, check_2)

  # Test s_f equilibrium condition computations
  check_1 <- 
    compute_condition_s_f_numerator_tj(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f
    )
  
  check_2 <- 
    compute_condition_s_f_numerator_tj_rcpp(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f
    )
  
  expect_equal(check_1, check_2)

  check_1 <- 
    compute_condition_s_f_denominator_tj(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      size_f = equilibrium$exogenous[[t]][[j]]$size_f,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f,
      s_f = equilibrium$endogenous[[t]][[j]]$s_f
    )
  
  check_2 <- 
    compute_condition_s_f_denominator_tj_rcpp(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      size_f = equilibrium$exogenous[[t]][[j]]$size_f,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f,
      s_f = equilibrium$endogenous[[t]][[j]]$s_f
    )
  
  expect_equal(check_1, check_2)

  check_1 <- 
    compute_condition_s_f_tj(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      size_f = equilibrium$exogenous[[t]][[j]]$size_f,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f,
      s_f = equilibrium$endogenous[[t]][[j]]$s_f
    )
    
  check_2 <- 
    compute_condition_s_f_tj_rcpp(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      size_f = equilibrium$exogenous[[t]][[j]]$size_f,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f,
      s_f = equilibrium$endogenous[[t]][[j]]$s_f
    )
  
  expect_equal(check_1, check_2)


  f_z <- equilibrium$endogenous[[t]][[j]]$f
  f_z[2] <- 1

  s_f_tj <-
    solve_s_f_tj(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      size_f = equilibrium$exogenous[[t]][[j]]$size_f,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f,
      s_f = equilibrium$endogenous[[t]][[j]]$s_f
    )

  s_f_tj_rcpp <-
    solve_s_f_tj_rcpp(
      m_w = equilibrium$parameter$m_w,
      m_f = equilibrium$parameter$m_f,
      beta_f = equilibrium$parameter$beta_f,
      lambda_f = equilibrium$parameter$lambda_f,
      x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
      size_w = equilibrium$exogenous[[t]][[j]]$size_w,
      size_f = equilibrium$exogenous[[t]][[j]]$size_f,
      mu = equilibrium$shock[[t]][[j]]$mu,
      ea_f = equilibrium$shock[[t]][[j]]$ea_f,
      f = equilibrium$endogenous[[t]][[j]]$f,
      s_f = equilibrium$endogenous[[t]][[j]]$s_f
    )

  expect_equal(s_f_tj, s_f_tj_rcpp)

  expect_no_error(
    solve_s_f(
      equilibrium = equilibrium
    )
  )

  df <-
    seq(
      0,
      2,
      by = 0.1
    ) %>%
    purrr::map(
      function(z) {
        f_z <- equilibrium$endogenous[[t]][[j]]$f
        f_z[2] <- z
        solution <-
          solve_s_f_tj_rcpp(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            beta_f = equilibrium$parameter$beta_f,
            lambda_f = equilibrium$parameter$lambda_f,
            x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            mu = equilibrium$shock[[t]][[j]]$mu,
            ea_f = equilibrium$shock[[t]][[j]]$ea_f,
            f = f_z,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f
          )
        df_t <-
          data.frame(
            f_2 = f_z[2],
            ths = 1:length(f_z) %>% as.factor(),
            share = solution
          )
        return(df_t)
      }
    )  %>%
    dplyr::bind_rows()

  # Check share of ths=1 is increasing in f_2
  df_1 <- df %>% dplyr::filter(ths == 1)
  expect_true(
    all(diff(df_1$share) >= 0)
  )

  # Check share of ths=2 is decreasing in f_2 
  df_2 <- df %>% dplyr::filter(ths == 2)
  expect_true(
    all(diff(df_2$share) <= 0)
  )
  
  # Test compute_a_w_tj function
  a_w_test <-
    compute_a_w_tj(
      beta_w = equilibrium$parameter$beta_w,
      x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
      ea_w = equilibrium$shock[[t]][[j]]$ea_w
    )
  expect_type(a_w_test, "double")
  expect_length(a_w_test, 1)

  # Test compute_h_w_tj function
  h_w_test <-
    compute_h_w_tj(
      a_w = a_w_test,
      lambda_w = equilibrium$parameter$lambda_w,
      w = equilibrium$endogenous[[t]][[j]]$w
    )
  expect_type(h_w_test, "double")
  expect_length(h_w_test, length(equilibrium$endogenous[[t]][[j]]$w))

  # Test that h_w is decreasing in w
  w_test <- equilibrium$endogenous[[t]][[j]]$w * 1.1
  h_w_higher <-
    compute_h_w_tj(
      a_w = a_w_test,
      lambda_w = equilibrium$parameter$lambda_w,
      w = w_test
    )
  expect_true(all(h_w_higher < h_w_test))

})

