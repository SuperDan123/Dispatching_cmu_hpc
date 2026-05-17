set_constant_monopoly <-
  function() {
    ## set dunebsuibs ---------------------------------------------------------
    
    n_market <- 1
    n_ths <- 
      rep(
        list(2),
        n_market
      )
    n_x_a <- 2
    n_x_c <- 2 
    
    ## set tuning parameters --------------------------------------------------
    
    method_s_w <- "exact"
    margin <- 1e-10
    quadrature_size <- 21
    tol <- 1e-16
    minimum_wage <- FALSE
    
    constant <-
      list(
        n_ths = n_ths,
        n_market = n_market,
        n_x_a = n_x_a,
        n_x_c = n_x_c,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size <- quadrature_size,
        tol = tol,
        minimum_wage = minimum_wage
      )
    
    return(constant)
  }


set_constant <-
  function(
    n_ths,
    n_market,
    n_zone
  ) {
    ## set dunebsuibs ---------------------------------------------------------
    
    n_ths <- 
      rep(
        list(n_ths),
        n_zone
      )
    n_ths <- 
      rep(
        list(n_ths),
        n_market
      )
    
    n_x_a <- 3
    n_x_c <- 3 
    
    ## set tuning parameters --------------------------------------------------
    
    method_s_w <- "exact"
    margin <- 1e-10
    quadrature_size <- 21
    tol <- 1e-16
    minimum_wage <- FALSE
    use_exp <- FALSE
    
    constant <-
      list(
        n_ths = n_ths,
        n_market = n_market,
        n_zone = n_zone, 
        n_x_a = n_x_a,
        n_x_c = n_x_c,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        minimum_wage = minimum_wage,
        use_exp = use_exp
      )
    
    return(constant)
  }

set_parameter <-
  function(
    constant
  ) {
    mu_p <- 0.9
    mu_ths <- 0.9
    m_w <- 0.5
    m_f <- 0.5
    lambda_w <- 0.5
    lambda_f <- - 0.5
    beta_w <- 
      rep(
        -1,
        constant$n_x_a
      ) 
    beta_w[1:2] <- 3
    beta_f <- 
      rep(
        1,
        constant$n_x_a
      ) 
    beta_f[1:2] <- 3
    gamma_w <-
      rep(
        0.3,
        constant$n_x_c
      )
    gamma_f <-
      rep(
        0.3,
        constant$n_x_c
      )
    
    parameter <-
      list(
        mu_p = mu_p,
        mu_ths = mu_ths,
        m_w = m_w,
        m_f = m_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f
      )
    
    return(parameter)
  }

set_parameter_stochastic <-
  function(
    constant
  ) {
    mu_p <- 0.9
    mu_ths <- 0.9
    m_w <- 0.5
    m_f <- 0.5
    lambda_w <- 0.5
    lambda_f <- - 0.5
    beta_w <- 
      rep(
        -1,
        constant$n_x_a + 1
      ) 
    beta_w[1:2] <- 3
    beta_f <- 
      rep(
        1,
        constant$n_x_a + 1
      ) 
    beta_f[1:2] <- 3
    gamma_w <-
      rep(
        0.3,
        constant$n_x_c + 1
      )
    gamma_f <-
      rep(
        0.3,
        constant$n_x_c + 1
      )
    
    parameter <-
      list(
        mu_p = mu_p,
        mu_ths = mu_ths,
        m_w = m_w,
        m_f = m_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f
      )
    
    return(parameter)
  }

generate_shock_zero <-
  function(
    constant,
    parameter
  ) {
    n_market <- constant$n_market
    n_zone <- constant$n_zone
    n_ths <- constant$n_ths
    
    shock <-
      1:n_market %>%
      purrr::map(
        function (t) {
          1:n_zone %>%
            purrr::map(
              function (j) {
                mu <-
                  rep(
                    parameter$mu_ths,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                mu[1:2] <- parameter$mu_p
                ea_w <-
                  rep(
                    0,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                ea_f <-
                  rep(
                    0,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                ec_w <-
                  rep(
                    0,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                ec_f <-
                  rep(
                    0,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                
                shock_tj <-
                  list(
                    mu = mu,
                    ea_w = ea_w,
                    ea_f = ea_f,
                    ec_w = ec_w,
                    ec_f = ec_f
                  )
                
                return(shock_tj)
              }
            )
        }
      )
    
    return(shock)
  }

generate_shock_stochastic <-
  function(
    constant,
    parameter
  ) {
    n_market <- constant$n_market
    n_zone <- constant$n_zone
    n_ths <- constant$n_ths
    
    shock <-
      1:n_market %>%
      purrr::map(
        function (t) {
          1:n_zone %>%
            purrr::map(
              function (j) {
                mu <-
                  rep(
                    parameter$mu_ths,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                mu[1:2] <- parameter$mu_p
                ea_w <-
                  rnorm(
                    n_ths[[t]][[j]],
                    sd = 0.01
                  ) %>%
                  as.matrix()
                ea_f <-
                  rnorm(  
                    n_ths[[t]][[j]],
                    sd = 0.01
                  ) %>%
                  as.matrix()
                ec_w <-
                  rnorm(
                    n_ths[[t]][[j]],
                    sd = 0.01
                  ) %>%
                  as.matrix()
                ec_f <-
                  rnorm(
                    n_ths[[t]][[j]],
                    sd = 0.01
                  ) %>%
                  as.matrix()
                
                shock_tj <-
                  list(
                    mu = mu,
                    ea_w = ea_w,
                    ea_f = ea_f,
                    ec_w = ec_w,
                    ec_f = ec_f
                  )
                
                return(shock_tj)
              }
            )
        }
      )
    
    return(shock)
  }  

generate_exogenous <-
  function(
    constant
  ) {
    n_market <- constant$n_market
    n_zone <- constant$n_zone
    n_ths <- constant$n_ths
    n_x_a <- constant$n_x_a
    n_x_c <- constant$n_x_c
    
    exogenous <-
      1:n_market %>%
      purrr::map(
        function(t) {
          1:n_zone %>%
            purrr::map(
              function (j) {
                x_a_w <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_a
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  ) 
                # intercept
                x_a_w[, 1] <- 1
                # inside dummy
                diag(x_a_w) <- 1
                x_a_w[, 2] <- 0
                
                x_a_f <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_a
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  ) 
                
                x_a_f[, 1] <- 1
                
                diag(x_a_f) <- 1
                x_a_f[, 2] <- 0
                x_c_w <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_c
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  )
                
                x_c_w[, 1] <- 1
                
                diag(x_c_w) <- 1
                x_c_w[, 2] <- 0
                x_c_f <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_c
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  )
                
                x_c_f[, 1] <- 1
                
                diag(x_c_f) <- 1
                x_c_f[, 2] <- 0
                w_0 <- - 10
                
                f_0 <- 0
                
                size_w <- 1
                
                size_f <- 1
                
                owner <-
                  rep(
                    1,
                    n_ths[[t]][[j]]
                  ) %>%
                  diag()
                
                exogenous_tj <-
                  list(
                    x_a_w = x_a_w,
                    x_a_f = x_a_f,
                    x_c_w = x_c_w,
                    x_c_f = x_c_f,
                    w_0 = w_0,
                    f_0 = f_0,
                    size_w = size_w,
                    size_f = size_f,
                    owner = owner
                  )
                
                return(exogenous_tj)
              }
            )
        }
      )
    return(exogenous)
  }

generate_exogenous_stochastic <- 
  function(
    constant
  ) {
    n_market <- constant$n_market
    n_zone <- constant$n_zone
    n_ths <- constant$n_ths
    n_x_a <- constant$n_x_a
    n_x_c <- constant$n_x_c
    
    exogenous <-
      1:n_market %>%
      purrr::map(
        function(t) {
          1:n_zone %>%
            purrr::map(
              function (j) {
                x_a_w <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_a
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  ) 
                # intercept
                x_a_w[, 1] <- 1
                
                # inside dummy
                diag(x_a_w) <- 1

                
                # demand shifter
                y_a <- 
                  rnorm(
                    n_ths[[t]][[j]],
                    sd = 0.1
                  ) %>% 
                  as.matrix()
                
                x_a_w <- 
                  cbind(
                    x_a_w,
                    y_a
                  )
                
                x_a_f <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_a
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  ) 
                
                x_a_f[, 1] <- 1
                
                diag(x_a_f) <- 1

                
                x_a_f <- 
                  cbind(
                    x_a_f,
                    y_a
                  )
                
                x_c_w <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_c
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  )
                
                # supply shifter
                y_c <- 
                  rnorm(
                    n_ths[[t]][[j]],
                    sd = 0.1
                  ) %>% 
                  as.matrix()
                
                x_c_w[, 1] <- 1
                
                diag(x_c_w) <- 1

                
                x_c_w <- 
                  cbind(
                    x_c_w,
                    y_c
                  )
                
                x_c_f <-
                  rep(
                    0,
                    n_ths[[t]][[j]] * n_x_c
                  ) %>%
                  matrix(
                    nrow = n_ths[[t]][[j]]
                  )
                
                x_c_f[, 1] <- 1
                
                diag(x_c_f) <- 1

                
                x_c_f <- 
                  cbind(
                    x_c_f,
                    y_c
                  )
                
                w_0 <- - 10
                
                f_0 <- 0
                
                size_w <- 1
                
                size_f <- 1
                
                owner <-
                  rep(
                    1,
                    n_ths[[t]][[j]]
                  ) %>%
                  diag()
                
                exogenous_tj <-
                  list(
                    x_a_w = x_a_w,
                    x_a_f = x_a_f,
                    x_c_w = x_c_w,
                    x_c_f = x_c_f,
                    w_0 = w_0,
                    f_0 = f_0,
                    size_w = size_w,
                    size_f = size_f,
                    owner = owner
                  )
                
                return(exogenous_tj)
              }
            )
        }
      )
    return(exogenous)
  }

generate_endogenous <-
  function(
    constant,
    parameter,
    exogenous,
    shock
  ) {
    n_market <- constant$n_market
    n_ths <- constant$n_ths
    n_zone <- constant$n_zone
    
    endogenous <-
      1:n_market %>%
      purrr::map(
        function (t) {
          endogenous_t <- 
            1:n_zone %>%
            purrr::map(
              function (j) {
                w <- 
                  rep(
                    0.1,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                c_w <- 
                  compute_c_w_tj(
                    gamma_w = parameter$gamma_w,
                    x_c_w = exogenous[[t]][[j]]$x_c_w,
                    ec_w = shock[[t]][[j]]$ec_w,
                    use_exp = constant$use_exp
                  ) 
                # private market
                w[1:2] <- c_w[1:2]
                f <-
                  w + 
                  rep(
                    5,
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                c_f <-
                  compute_c_f_tj(
                    gamma_f = parameter$gamma_f,
                    x_c_f = exogenous[[t]][[j]]$x_c_f,
                    ec_f = shock[[t]][[j]]$ec_f,
                    use_exp = constant$use_exp
                  ) 
                # private market
                f[1:2] <- w[1:2] + c_f[1:2]
                s_w <-
                  rep(
                    1 / n_ths[[t]][[j]],
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                s_f <-
                  rep(
                    1 / n_ths[[t]][[j]],
                    n_ths[[t]][[j]]
                  ) %>%
                  as.matrix()
                eta_w <-
                  rep(
                    1e-2,
                    n_ths[[t]][[j]] - 2
                  ) %>%
                  as.matrix()
                eta_f <-
                  rep(
                    1e-2,
                    n_ths[[t]][[j]] - 2
                  ) %>%
                  as.matrix()
                endogenous_tj <-
                  list(
                    w = w,
                    f = f,
                    s_w = s_w,
                    s_f = s_f,
                    eta_w = eta_w,
                    eta_f = eta_f
                  )
                return(endogenous_tj)
              }
            )
        }
      )
    return(endogenous)
  }

generate_equilibrium <-
  function(
    n_ths,
    n_market,
    n_zone,
    seed
  ) {
    set.seed(seed)
    
    # set constants -----------------------------------------------------------
    
    constant <- 
      set_constant(
        n_ths = n_ths,
        n_market = n_market,
        n_zone = n_zone
      )
    
    # set parameters ----------------------------------------------------------
    
    parameter <-
      set_parameter(
        constant = constant
      )
    
    # set shocks --------------------------------------------------------------
    
    shock <-
      generate_shock_zero(
        constant = constant,
        parameter = parameter
      ) 
    
    # set exogenous variables -------------------------------------------------
    
    exogenous <-
      generate_exogenous(
        constant = constant
      ) 
    
    # set endogenous variables ------------------------------------------------
    
    endogenous <-
      generate_endogenous(
        constant = constant,
        parameter = parameter,
        exogenous = exogenous,
        shock = shock
      ) 
    
    
    # set equilibrium object --------------------------------------------------
    
    equilibrium <-
      list(
        constant = constant,
        parameter = parameter,
        shock = shock,
        exogenous = exogenous,
        endogenous = endogenous
      )
    
    return(equilibrium)
    
  }

generate_equilibrium_stochastic <-
  function(
    n_ths,
    n_market,
    n_zone,
    seed
  ) {
    set.seed(seed)
    
    # set constants -----------------------------------------------------------
    
    constant <- 
      set_constant(
        n_ths = n_ths,
        n_market = n_market,
        n_zone = n_zone
      )
    
    # set parameters ----------------------------------------------------------
    
    parameter <-
      set_parameter_stochastic(
        constant = constant
      )
    
    # set shocks --------------------------------------------------------------
    
    shock <-
      generate_shock_stochastic(
        constant = constant,
        parameter = parameter
      ) 
    
    # set exogenous variables -------------------------------------------------
    
    exogenous <-
      generate_exogenous_stochastic(
        constant = constant
      ) 
    
    # set endogenous variables ------------------------------------------------
    
    endogenous <-
      generate_endogenous(
        constant = constant,
        parameter = parameter,
        exogenous = exogenous,
        shock = shock
      ) 
    
    
    # set equilibrium object --------------------------------------------------
    
    equilibrium <-
      list(
        constant = constant,
        parameter = parameter,
        shock = shock,
        exogenous = exogenous,
        endogenous = endogenous
      )
    
    return(equilibrium)
    
  }

compute_a_w_tj <-
  function(
    beta_w,
    x_a_w,
    ea_w
  ) {
    a_w <- x_a_w %*% beta_w + ea_w
    return(a_w)
  }

compute_a_f_tj <-
  function(
    beta_f,
    x_a_f,
    ea_f
  ) {
    a_f <- x_a_f %*% beta_f + ea_f
    return(a_f)
  }

compute_h_w_tj <-
  function(
    a_w,
    lambda_w,
    w
  ) {
    h_w <- 
      exp(
        a_w  + w * lambda_w
      )
    return(h_w)
  }

compute_h_f_tj <-
  function(
    beta_f,
    lambda_f,
    x_a_f,
    ea_f,
    f
  ) {
    a_f <-
      compute_a_f_tj (
        beta_f = beta_f,
        x_a_f = x_a_f,
        ea_f = ea_f
      ) 
    h_f <-
      exp(
        a_f + f * lambda_f
      )
    return(h_f)
  }

compute_condition_s_f_numerator_tj <-
  function(
    m_w,
    m_f,
    beta_f,
    lambda_f,
    x_a_f,
    size_w,
    mu,
    ea_f,
    f
  ) {
    h_f <-
      compute_h_f_tj (
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        ea_f = ea_f,
        f = f
      )
    numerator <-
      mu * (size_w)^m_w * h_f
    numerator <-
      numerator^(1 / (2 - m_f))
    return(numerator)
  }

compute_condition_s_f_denominator_tj <-
  function(
    m_w,
    m_f,
    beta_f,
    lambda_f,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_f,
    f,
    s_f
  ) {
    h_f <-
      compute_h_f_tj (
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        ea_f = ea_f,
        f = f
      )
    denominator <-
      s_f^(1 - m_f) * 
      size_f^(1 - m_f) * 
      (mu * (size_w)^m_w * h_f)^(- (1 - m_f) / (2 - m_f)) 
    denominator <-
      denominator +
      sum(
        (mu * (size_w)^m_w * h_f)^(1 / (2 - m_f))
      )
    return(denominator)
  }

compute_condition_s_f_tj <-
  function(
    m_w,
    m_f,
    beta_f,
    lambda_f,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_f,
    f,
    s_f
  ) {
    numerator <-
      compute_condition_s_f_numerator_tj(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        mu = mu,
        ea_f = ea_f,
        f = f
      ) 
    denominator <-
      compute_condition_s_f_denominator_tj(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    # condition <-
    #   1 - numerator / (denominator * s_f)
    condition <-
      s_f - numerator / denominator
    return(condition)
  }

solve_s_f_tj <-
  function(
    m_w,
    m_f,
    beta_f,
    lambda_f,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_f,
    f,
    s_f
  ) {
    x <-
      rep(
        0,
        nrow(s_f)
      )
    
    fn <-
      function(x) {
        s_f_x <-
          exp(x) / (1 + sum(exp(x)))
        condition_t <-
          compute_condition_s_f_tj(
            m_w = m_w,
            m_f = m_f,
            beta_f = beta_f,
            lambda_f = lambda_f,
            x_a_f = x_a_f,
            size_w = size_w,
            size_f = size_f,
            mu = mu,
            ea_f = ea_f,
            f = f,
            s_f = s_f_x
          )
        return(condition_t)
      }
    
    solution <-
      nleqslv::nleqslv(
        x = x,
        fn = fn,
        control = 
          list(
            allowSingular = TRUE
          )
      )
    
    solution$x <-
      exp(solution$x) / (1 + sum(exp(solution$x)))
    
    return(solution$x %>% as.matrix())
  }

solve_s_f <-
  function(
    equilibrium
  ) {
    equilibrium$endogenous <-
      1:length(equilibrium$exogenous) %>%
      purrr::map(
        function(t) {
          1:length(equilibrium$exogenous[[t]]) %>%
            purrr::map(
              function(j) {
                solution_tj <- 
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
                endogenous_tj <-
                  equilibrium$endogenous[[t]][[j]]
                endogenous_tj$s_f <- 
                  solution_tj
                return(endogenous_tj)
              }
            )
        }
      ) 
    return(equilibrium)
  }

compute_meeting_number_tj <-
  function(
    m_w,
    m_f,
    size_w,
    size_f,
    mu,
    s_f
  ) {
    q <- mu * size_w^m_w * size_f^m_f * s_f^m_f
    return(q)
  }

compute_meeting_probability_w_tj <-
  function(
    m_w,
    m_f,
    size_w,
    size_f,
    mu,
    s_f
  ) {
    q <- 
      compute_meeting_number_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    p <- q / size_w
    return(p)
  }

compute_meeting_probability_f_tj <-
  function(
    m_w,
    m_f,
    size_w,
    size_f,
    mu,
    s_f
  ) {
    q <- 
      compute_meeting_number_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    p <- q / (size_f * s_f)
    return(p)
  }

compute_omega_tj <-
  function(
    meeting_probability_w,
    met
  ) {
    p_met <- meeting_probability_w[met]
    unmet <- 
      setdiff(
        1:nrow(meeting_probability_w),
        met)
    p_unmet <- meeting_probability_w[unmet]
    omega_f <-
      prod(p_met) * prod(1 - p_unmet)
    meeting_probability_w <-
      ifelse(
        meeting_probability_w < 1e-16,
        1e-16,
        meeting_probability_w
      )
    omega_f <-
      omega_f / meeting_probability_w
    omega_f[unmet] <- 0
    return(omega_f)
  }

compute_rho_tj <-
  function(
    h_w,
    meeting_probability_w,
    met
  ) {
    unmet <- 
      setdiff(
        1:nrow(meeting_probability_w),
        met)
    h_w_met <- h_w
    h_w_met[unmet] <- 0
    rho_f <-
      h_w_met / (1 + sum(h_w_met))
    return(rho_f)
  }


compute_s_w_met <-
  function (
    meeting_probability_w,
    h_w,
    met
  ) {
    
    omega <-
      compute_omega_tj(
        meeting_probability_w = meeting_probability_w,
        met = met
      ) 
    
    rho <-
      compute_rho_tj(
        h_w = h_w,
        meeting_probability_w = meeting_probability_w,
        met = met
      )
    
    s_w_met <- omega * rho
    
    return(s_w_met)
  }

solve_s_w_tj_from_a_w_exact <-
  function(
    a_w,
    m_w,
    m_f,
    lambda_w,
    size_w,
    size_f,
    mu,
    w,
    s_f
  ) {
    h_w <-
      compute_h_w_tj(
        a_w = a_w,
        lambda_w = lambda_w,
        w = w
      )
    
    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    
    met_list <-
      rje::powerSet(
        x = 1:nrow(s_f),
        m = nrow(s_f) 
      )
    
    s_w <-
      met_list %>%
      purrr::map(
        ~ compute_s_w_met(
          meeting_probability_w = meeting_probability_w,
          h_w = h_w,
          met = .
        ) 
      ) 
    
    s_w <-
      s_w %>%
      purrr::reduce(`+`)
    
    return(s_w)
  }

solve_s_w_tj_exact <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    x_a_w,
    size_w,
    size_f,
    mu,
    ea_w,
    w,
    s_f
  ) {
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    
    s_w <-
      solve_s_w_tj_from_a_w_exact_rcpp(
        a_w = a_w,
        m_w = m_w,
        m_f = m_f,
        lambda_w = lambda_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        w = w,
        s_f = s_f
      )
    
    return(s_w)
    
  }

solve_s_w_tj_exact_check <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    x_a_w,
    size_w,
    size_f,
    mu,
    ea_w,
    w,
    s_f
  ) {
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      )
    
    h_w <-
      compute_h_w_tj(
        a_w = a_w,
        lambda_w = lambda_w,
        w = w
      )
    
    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    
    met_list <-
      rje::powerSet(
        x = 1:nrow(s_f),
        m = nrow(s_f) 
      )
    
    s_w <-
      met_list %>%
      purrr::map(
        ~ compute_s_w_met(
          meeting_probability_w = meeting_probability_w,
          h_w = h_w,
          met = .
        ) 
      ) 
    
    s_w <-
      s_w %>%
      purrr::reduce(`+`)
    
    return(s_w)
    
  }

compute_h_w_meetng_probability_full <-
  function(
    a_w,
    m_w,
    m_f,
    lambda_w,
    size_w,
    size_f,
    mu,
    w,
    s_f
  ) {
    
    h_w <-
      compute_h_w_tj(
        a_w = a_w,
        lambda_w = lambda_w,
        w = w
      )
    
    h_w <- 
      ifelse(
        h_w < 1e-20,
        1e-20,
        h_w
      )
    
    h_w <- 
      ifelse(
        h_w > 1e20,
        1e20,
        h_w
      )
    
    h_w_full <-
      rbind(
        1,
        h_w
      )

    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    meeting_probability_w_full <-
      rbind(
        1,
        meeting_probability_w
      )
    return(
      list(
        h_w_full = h_w_full,
        meeting_probability_w_full = meeting_probability_w_full
      )
    )
  }


compute_utility_distribution <-
  function(
    u, 
    utility
  ) {
    distribution <- 
      evd::pgev(
        q = u, 
        loc = utility, 
        scale = 1, 
        shape = 0, 
        lower.tail = TRUE
      )
    return(distribution)
  }

compute_utility_density <-
  function(
    u, 
    utility
  ) {
    density <- 
      evd::dgev(
        x = u, 
        loc = utility, 
        scale = 1,
        shape = 0
      )
    return(density)
  }

compute_integrand <-
  function(
    u, 
    utility, 
    consideration
  ) {
    # compute density of utility
    density <- 
      compute_utility_density(
        u = u, 
        utility = utility
      )
    # compute distribution of utility
    distribution <- 
      compute_utility_distribution(
        u = u,
        utility = utility
      )
    # compute product
    components <- 
      1 - 
      consideration + 
      consideration * distribution
    components <- 
      ifelse(
        components < 1e-16, 
        1e-16, 
        components
      )
    product <- prod(components)
    product <- 
      matrix(
        rep(
          product, 
          dim(components)[1]
        )
      )
    product <- product / components
    # compute integrand
    integrand <- product * density
    # return
    return(integrand)
  }

compute_f_upper <-
  function(
    u, 
    utility, 
    margin
  ) {
    utility_max <- max(utility)
    distribution <- 
      compute_utility_distribution(
        u = u, 
        utility = utility_max
      )
    y <- distribution - (1 - margin/2)
    return(y)
  }

compute_f_lower <-
  function(
    u, 
    utility, 
    margin
  ) {
    utility_min <- min(utility)
    distribution <- 
      compute_utility_distribution(
        u = u,
        utility = utility_min
      )
    y <- distribution - margin/2
    return(y)
  }

compute_choice_probability_with_consideration <-
  function(
    utility, 
    consideration, 
    margin, 
    quadrature_size, 
    tol
  ) {
    a <- min(utility) - 30
    b <- max(utility) + 30
    # compute upper and lower u
    u_upper <- 
      uniroot(
        f = compute_f_upper, 
        interval = c(a, b),
        utility = utility, 
        margin = margin, 
        tol = tol
      )
    u_upper <- u_upper$root
    u_lower <- 
      uniroot(
        f = compute_f_lower, 
        interval = c(a, b),
        utility = utility, 
        margin = margin, 
        tol = tol
      )
    u_lower <- u_lower$root
    # set the sequence of quadrature points
    u_sequence <- 
      seq(
        from = u_lower, 
        to = u_upper, 
        length.out = quadrature_size
      )
    # compute G at each quadrature point
    G_sequence <-
      foreach (
        n = 1:length(u_sequence)
      ) %do% {
        u_n <- u_sequence[n]
        G_n <- 
          compute_integrand(
            u = u_n, 
            utility = utility, 
            consideration = consideration
          )
        return(G_n)
      }
    # compute integrand at each quadrature point
    integrand_sequence <- 
      foreach (
        n = 1:(length(u_sequence) - 1)
      ) %do% {
        u_n <- u_sequence[n]
        u_n_1 <- u_sequence[n + 1]
        G_n <- G_sequence[[n]]
        G_n_1 <- G_sequence[[n + 1]]
        G_n_bar <- (G_n + G_n_1)/2
        component <- G_n_bar * (u_n_1 - u_n)
        return(component)
      }
    # sum up
    integrand_sequence <-
      purrr::reduce(
        integrand_sequence, 
        `+`
      )
    # return
    return(integrand_sequence)
  }

solve_s_w_tj_from_a_w_approximate <-
  function(
    a_w,
    m_w,
    m_f,
    lambda_w,
    size_w,
    size_f,
    mu,
    w,
    s_f,
    margin,
    quadrature_size,
    tol
  ) {
    
    full <-
      compute_h_w_meetng_probability_full(
        a_w = a_w,
        m_w = m_w,
        m_f = m_f,
        lambda_w = lambda_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        w = w,
        s_f = s_f
      ) 
    
    # compute choice probability
    s_w_full <-
      compute_choice_probability_with_consideration(
        utility = log(full$h_w_full), 
        consideration = full$meeting_probability_w_full, 
        margin = margin, 
        quadrature_size = quadrature_size, 
        tol = tol
      ) 
    
    # drop outside option
    s_w <-
      s_w_full[
        2:nrow(s_w_full),
        ,
        drop = FALSE
      ]
    return(s_w)
  }

solve_s_w_tj_approximate <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    x_a_w,
    size_w,
    size_f,
    mu,
    ea_w,
    w,
    s_f,
    margin,
    quadrature_size,
    tol
  ) {
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    s_w <-
      solve_s_w_tj_from_a_w_approximate(
        a_w = a_w,
        m_w = m_w,
        m_f = m_f,
        lambda_w = lambda_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        w = w,
        s_f = s_f,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      )
    return(s_w)
  }

solve_s_w_exact <-
  function(
    equilibrium
  ) {
    equilibrium$endogenous <-
      1:length(equilibrium$endogenous) %>%
      purrr::map(
        function(t) {
          1:length(equilibrium$endogenous[[t]]) %>%
            purrr::map(
              function(j) {
                solution_tj <- 
                  solve_s_w_tj_exact(
                    m_w = equilibrium$parameter$m_w,
                    m_f = equilibrium$parameter$m_f,
                    beta_w = equilibrium$parameter$beta_w,
                    lambda_w = equilibrium$parameter$lambda_w,
                    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                    mu = equilibrium$shock[[t]][[j]]$mu,
                    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                    w = equilibrium$endogenous[[t]][[j]]$w,
                    s_f = equilibrium$endogenous[[t]][[j]]$s_f
                  )
                endogenous_tj <-
                  equilibrium$endogenous[[t]][[j]]
                endogenous_tj$s_w <- solution_tj
                return(endogenous_tj)
              }
            )
        }
      ) 
    return(equilibrium)
  }

solve_s_w_approximate <-
  function(
    equilibrium
  ) {
    equilibrium$endogenous <-
      1:length(equilibrium$endogenous) %>%
      purrr::map(
        function(t) {
          1:length(equilibrium$endogenous[[t]]) %>%
            purrr::map(
              function(j) {
                solution_tj <- 
                  solve_s_w_tj_approximate(
                    m_w = equilibrium$parameter$m_w,
                    m_f = equilibrium$parameter$m_f,
                    beta_w = equilibrium$parameter$beta_w,
                    lambda_w = equilibrium$parameter$lambda_w,
                    x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                    size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                    size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                    mu = equilibrium$shock[[t]][[j]]$mu,
                    ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                    w = equilibrium$endogenous[[t]][[j]]$w,
                    s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                    margin = equilibrium$constant$margin,
                    quadrature_size = equilibrium$constant$quadrature_size,
                    tol = equilibrium$constant$tol
                  )
                endogenous_tj <-
                  equilibrium$endogenous[[t]][[j]]
                endogenous_tj$s_w <- solution_tj
                return(endogenous_tj)
              }
            )
        }
      ) 
    return(equilibrium)
  }

solve_s_f_d_f_tj <-
  function(
    m_w,
    m_f,
    beta_f,
    lambda_f,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_f,
    f,
    s_f
  ) {
    solve_s_f_tj_x <-
      function(x) {
        solution <- 
          solve_s_f_tj_rcpp(
            m_w = m_w,
            m_f = m_f,
            beta_f = beta_f,
            lambda_f = lambda_f,
            x_a_f = x_a_f,
            size_w = size_w,
            size_f = size_f,
            mu = mu,
            ea_f = ea_f,
            f = x,
            s_f = s_f
          )
        return(solution)
      } 
    s_f_d_f_t <- 
      numDeriv::jacobian(
        func = solve_s_f_tj_x,
        x = f
      )
    return(s_f_d_f_t)
  }

solve_s_w_d_w_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    x_a_w,
    size_w,
    size_f,
    mu,
    ea_w,
    w,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {
    solve_s_w_t_x <-
      function(x) {
        if (method_s_w == "approximate") {
          s_w_t <-
            solve_s_w_tj_approximate(
              m_w = m_w,
              m_f = m_f,
              beta_w = beta_w,
              lambda_w = lambda_w,
              x_a_w = x_a_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              ea_w = ea_w,
              w = x,
              s_f = s_f,
              margin = margin,
              quadrature_size = quadrature_size,
              tol = tol
            )
        } else if (method_s_w== "exact") {
          s_w_t <-
            solve_s_w_tj_exact(
              m_w = m_w,
              m_f = m_f,
              beta_w = beta_w,
              lambda_w = lambda_w,
              x_a_w = x_a_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              ea_w = ea_w,
              w = x,
              s_f = s_f
            )
        }
        
        return(s_w_t)
      }
    s_w_d_w_t <-
      numDeriv::jacobian(
        func = solve_s_w_t_x,
        x = w
      )
    return(s_w_d_w_t)
  }

solve_s_w_d_f_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_w,
    ea_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {
    solve_s_w_t_x <-
      function(x) {
        s_f_t <- 
          solve_s_f_tj_rcpp(
            m_w = m_w,
            m_f = m_f,
            beta_f = beta_f,
            lambda_f = lambda_f,
            x_a_f = x_a_f,
            size_w = size_w,
            size_f = size_f,
            mu = mu,
            ea_f = ea_f,
            f = x,
            s_f = s_f
          )
        if (method_s_w == "approximate") {
          s_w_t <-
            solve_s_w_tj_approximate(
              m_w = m_w,
              m_f = m_f,
              beta_w = beta_w,
              lambda_w = lambda_w,
              x_a_w = x_a_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              ea_w = ea_w,
              w = w,
              s_f = s_f_t,
              margin = margin,
              quadrature_size = quadrature_size,
              tol = tol
            )
        } else if (method_s_w == "exact") {
          s_w_t <-
            solve_s_w_tj_exact(
              m_w = m_w,
              m_f = m_f,
              beta_w = beta_w,
              lambda_w = lambda_w,
              x_a_w = x_a_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              ea_w = ea_w,
              w = w,
              s_f = s_f_t
            )
        }
        
        return(s_w_t)
      }
    s_w_d_f_t <-
      numDeriv::jacobian(
        func = solve_s_w_t_x,
        x = f
      )
    return(s_w_d_f_t)
  }

compute_meeting_number_d_f_tj <-
  function(
    m_w,
    m_f,
    size_w,
    size_f,
    mu,
    s_f,
    s_f_d_f
  ) {
    meeting_number_d_f <- mu * size_w^m_w * size_f^m_f * m_f * s_f^(m_f - 1) 
    meeting_number_d_f <- meeting_number_d_f %*% t(rep(1, ncol(s_f_d_f)))
    meeting_number_d_f <- meeting_number_d_f * s_f_d_f
    return(meeting_number_d_f)
  }

compute_c_w_tj <-
  function(
    gamma_w,
    x_c_w,
    ec_w,
    use_exp
  ) {
    if (use_exp == TRUE){
      c_w <- exp(x_c_w %*% gamma_w + ec_w)
    }else{
      c_w <- x_c_w %*% gamma_w + ec_w
    }
    
    return(c_w)
  }

compute_c_f_tj <-
  function(
    gamma_f,
    x_c_f,
    ec_f,
    use_exp
  ) {
    if (use_exp == TRUE){
      c_f <- exp(x_c_f %*% gamma_f + ec_f)
    }else{
      c_f <- x_c_f %*% gamma_f + ec_f
    }
    
    return(c_f)
  }

compute_profit_ths_tj <-
  function(
    m_w,
    m_f,
    gamma_w,
    gamma_f,
    x_c_w,
    x_c_f,
    size_w,
    size_f,
    owner,
    mu,
    ec_w,
    ec_f,
    w,
    f,
    s_w,
    s_f,
    use_exp
  ) {
    meeting_number <- 
      compute_meeting_number_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    
    c_f <-
      compute_c_f_tj(
        gamma_f = gamma_f,
        x_c_f = x_c_f,
        ec_f = ec_f,
        use_exp = use_exp
      ) 
    
    o <- t(owner) %*% owner
    
    profit_ths <-
      meeting_number * s_w * (f - w - c_w) - c_f * s_f * size_f
    profit_ths <-
      o %*% profit_ths
    
    return(profit_ths)
  }

solve_profit_ths_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else if (method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f
        )
    }
    
    profit_ths <-
      compute_profit_ths_tj(
        m_w = m_w,
        m_f = m_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_w = s_w,
        s_f = s_f,
        use_exp = use_exp
      ) 
    
    return(profit_ths)
  }

solve_w_f_monopoly_itj <-
  function(
    i,
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    
    index <- owner[i, ] %>% as.logical()
    num <- sum(index)
    
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    
    a_f <-
      compute_a_f_tj (
        beta_f = beta_f,
        x_a_f = x_a_f,
        ea_f = ea_f
      ) 
    
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    
    lower <- 
      c(
        w_0,
        w_0 + c_w[index]
      )
    
    upper <-
      c(
        abs (
          (- a_f[index]) / lambda_f - c_w[index]
        ),
        abs(
          (- a_f[index]) / lambda_f
        )
      )
    upper <-
      5 * upper
    upper <-
      ifelse(
        upper > 700,
        700,
        upper
      )
    
    x <- lower
    
    
    fn <-
      function(x) {
        w_i <- 
          x[1:num] %>% 
          as.matrix()
        f_i <- 
          x[(num + 1):length(x)] %>%
          as.matrix()
        profit_ths <-
          solve_profit_ths_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w[index, , drop = FALSE],
            x_a_f = x_a_f[index, , drop = FALSE],
            x_c_w = x_c_w[index, , drop = FALSE],
            x_c_f = x_c_f[index, , drop = FALSE],
            size_w = size_w,
            size_f = size_f,
            owner = owner[i, index, drop = FALSE],
            mu = mu[index, , drop = FALSE],
            ea_w = ea_w[index, , drop = FALSE],
            ea_f = ea_f[index, , drop = FALSE],
            ec_w = ec_w[index, , drop = FALSE],
            ec_f = ec_f[index, , drop = FALSE],
            w = w_i,
            f = f_i,
            s_f = s_f[index, , drop = FALSE],
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(-profit_ths)
      }
    
    eval_grad_f <-
      function(x) {
        grad <-
          numDeriv::grad(
            func = fn,
            x = x
          )
        return(grad)
      }
    
    eval_g_ineq <-
      function(x) {
        w_i <- 
          x[1:num] %>% 
          as.matrix()
        f_i <- 
          x[(num + 1):length(x)] %>%
          as.matrix()
        return(w_i - f_i)
      }
    
    eval_jac_g_ineq <-
      function(x) {
        jac <-
          numDeriv::jacobian(
            func = eval_g_ineq,
            x = x
          )
        return(jac)
      }
    
    solution <-
      nloptr::nloptr(
        x0 = x,
        eval_f = fn,
        eval_grad_f = eval_grad_f,
        eval_g_ineq = eval_g_ineq,
        eval_jac_g_ineq = eval_jac_g_ineq,
        lb = lower,
        ub = upper,
        opts = 
          list(
            algorithm = "NLOPT_LD_SLSQP",
            xtol_rel = 1e-4
          )
      )
    x <- solution$solution
    w_i <- 
      x[1:num] %>% 
      as.matrix()
    f_i <- 
      x[(num + 1):length(x)]
    return(
      list(
        w_i = w_i,
        f_i = f_i
      )
    )
  }

solve_w_f_monopoly_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    solution <-
      1:nrow(owner) %>%
      purrr::map(
        ~ solve_w_f_monopoly_itj(
            i = .,
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w,
            f = f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
      )
    for (i in 1:nrow(owner)) {
      index <- owner[i, ] %>% as.logical()
      w[index] <- solution[[i]]$w_i
      f[index] <- solution[[i]]$f_i
    }
    return(
      list(
        w = w,
        f = f
      )
    )
  }



compute_foc_w_tj <-
  function(
    owner,
    c_w,
    w,
    f,
    s_w,
    s_w_d_w
  ) {
    
    o <- t(owner) %*% owner
    
    foc_w <- - s_w + (s_w_d_w * o) %*% (f - w - c_w)
    foc_w <- foc_w
    
    
    return(foc_w)
  }

compute_foc_f_tj <-
  function(
    m_w,
    m_f,
    owner,
    c_w,
    c_f,
    size_w,
    size_f,
    mu,
    w,
    f,
    s_w,
    s_f,
    s_w_d_f,
    s_f_d_f
  ) {
    
    meeting_number <- 
      compute_meeting_number_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    
    meeting_number_d_f <- 
      compute_meeting_number_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f,
        s_f_d_f = s_f_d_f
      ) 
    
    o <- t(owner) %*% owner
    
    foc_f <-
      meeting_number * s_w +
      (o * meeting_number_d_f) %*% (s_w * (f - w - c_w)) +
      (o * s_w_d_f) %*% (meeting_number * (f - w - c_w)) -
      (o * s_f_d_f) %*% c_f * size_f
    
    foc_f <- foc_f
    
    return(foc_f)
  }

compute_foc_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    
    c_f <-
      compute_c_f_tj(
        gamma_f = gamma_f,
        x_c_f = x_c_f,
        ec_f = ec_f,
        use_exp = use_exp
      ) 
    
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else if (method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f
        )
    }
    
    s_f_d_f <- 
      solve_s_f_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    s_w_d_w <-
      solve_s_w_d_w_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        lambda_w = lambda_w,
        x_a_w = x_a_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        w = w,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      ) 
    
    s_w_d_f <-
      solve_s_w_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      ) 
    
    foc_w <-
      compute_foc_w_tj(
        owner = owner,
        c_w = c_w,
        w = w,
        f = f,
        s_w = s_w,
        s_w_d_w = s_w_d_w
      ) 
    
    foc_f <-
      compute_foc_f_tj(
        m_w = m_w,
        m_f = m_f,
        owner = owner,
        c_w = c_w,
        c_f = c_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        w = w,
        f = f,
        s_w = s_w,
        s_f = s_f,
        s_w_d_f = s_w_d_f,
        s_f_d_f = s_f_d_f
      ) 
    
    foc <-
      rbind(
        foc_w[3:length(foc_w), 1] %>% as.matrix(),
        foc_f[3:length(foc_f), 1] %>% as.matrix()
      )
    
    return(foc)
    
  }

compute_monopoly_bound_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    monopoly <-
      solve_w_f_monopoly_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    
    lower <-
      c(
        monopoly$w[3:length(monopoly$w)],
        c_w[3:length(c_w)]
      )
    
    upper <-
      c(
        rep(
          max(monopoly$f - c_w),
          length(monopoly$f) - 2
        ),
        rep(
          max(monopoly$f - monopoly$w),
          length(monopoly$f) - 2
        )
      )
    
    return(
      list(
        lower = lower,
        upper = upper
      )
    )
  }

relax_bound_tj <-
  function(
    bound
  ) {
    # adjust lower 
    lower_w <-
      bound$lower[1:(length(bound$lower)/2)]
    lower_w <-
      ifelse(
        lower_w > 0,
        0.1 * lower_w,
        2 * lower_w
      )
    bound$lower[1:(length(bound$lower)/2)] <-
      lower_w
    
    # adjust upper
    bound$upper <-
      ifelse(
        bound$upper > 0,
        2 * bound$upper,
        0.1 * bound$upper
      )
    
    return(bound)
  }

transform_x_to_w_f_optim <-
  function(
    x,
    w,
    f
  ) {
    w_x <- x[1:(length(w) - 2)]
    diff_x <- x[(length(w) - 1):length(x)]
    w_x <- 
      c(
        w[1], 
        w[2],
        w_x
        )
    diff_x <- 
      c(
        f[1] - w[1],
        f[2] - w[2],
        diff_x
        )
    f_x <- w_x + diff_x
    return(
      list(
        w = w_x,
        f = f_x
      )
    )
  }

solve_w_f_optim_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_tj(
        bound = bound
      ) 
    
    set.seed(seed)
    e <- rnorm(length(bound$lower))
    x <- 
      bound$lower +
      (bound$upper - bound$lower) * exp(e) / (1 + exp(e))
    
    fn <-
      function(x) {
        w_f <-
          transform_x_to_w_f_optim(
            x = x,
            w = w,
            f = f
          )
        foc <-
          compute_foc_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_f$w,
            f = w_f$f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        objective <- max(abs(foc))
        return(objective)
      }
    
    solution <-
      optim(
        par = x,
        fn = fn,
        method = "L-BFGS-B",
        lower = bound$lower,
        upper = bound$upper,
        control = list(trace = 1)
      )
    
    x <- solution$par
    
    w_f <-
      transform_x_to_w_f_optim(
        x = x,
        w = w,
        f = f
      )
    
    return(
      list(
        w = w_f$w %>% as.matrix(),
        f = w_f$f %>% as.matrix(),
        objective = solution$value
      )
    )
  }

transform_w_f_to_x_nleqslv <-
  function(
    w,
    f,
    lower,
    upper
  ) {
    w_x <- 
      w[
        3:length(w)
      ]
    
    diff_x <- 
      f[
        3:length(w)
      ] - 
      w_x
    
    y <- 
      (
        diff_x  -
          lower[
            (length(w) - 1):(2*(length(w) - 2))
          ]
      ) / 
      (upper - lower)[
        (length(w) - 1):(2*(length(w) - 2))
      ]
    
    x_f <- 
      log( 
        y / (1-y) 
      )
    
    z <- 
      ( 
        w_x - 
          lower[
            1:(length(w) - 2)
          ] 
      ) / 
      (upper - lower)[
        1:(length(w) - 2)
      ]
    
    x_w <- 
      log( 
        z / (1 - z) 
      )
    
    x <- 
      c(
        x_w,
        x_f
      )
    return(x)
  }

transform_x_to_w_f_nleqslv <-
  function(
    x,
    w,
    f,
    lower,
    upper
  ) {
    w_x <- x[1:(length(w) - 2)]
    diff_x <- x[(length(w) - 1):length(x)]
    w_x <- 
      lower[1:(length(w) - 2)] + 
      (upper - lower)[1:(length(w) - 2)] * 
      exp(w_x) / (1 + exp(w_x))
    diff_x <-
      lower[(length(w) - 1):(2*(length(w) - 2))] +
      (upper - lower)[(length(w) - 1):length(x)] *
      exp(diff_x) / (1 + exp(diff_x))
    w_x <- 
      c(
        w[1],
        w[2],
        w_x
        )
    diff_x <- 
      c(
        f[1] - w[1],
        f[2] - w[2],
        diff_x
        )
    f_x <- w_x + diff_x
    return(
      list(
        w = w_x %>% as.matrix(),
        f = f_x %>% as.matrix()
      )
    )
  }

check_initial_value <-
  function(
    w,
    f,
    lower,
    upper
  ) {
    w_x <- 
      w[
        3:nrow(w)
      ]
    diff_x <- 
      f[
        3:nrow(w)
      ] - 
      w_x
    w_x <-
      ifelse(
        w_x < lower[1:(nrow(w) - 2)],
        lower[1:(nrow(w) - 2)] + 1e-10,
        ifelse(
          w_x > upper[1:(nrow(w) - 2)],
          upper[1:(nrow(w) - 2)] - 1e-10,
          w_x
        )
      )
    diff_x <-
      ifelse(
        diff_x < lower[(nrow(w) - 1):(2*(nrow(w) - 2))],
        lower[(nrow(w) - 1):(2*(nrow(w) - 2))] + 1e-10,
        ifelse(
          diff_x > upper[(nrow(w) -1 ):(2*(nrow(w) - 2))],
          upper[(nrow(w) - 1):(2*(nrow(w) - 2))] - 1e-10,
          diff_x
        )
      )
    w[
      3:nrow(w)
    ] <- w_x
    f[
      3:nrow(w)
    ] <- w_x + diff_x
    return(
      list(
        w = w,
        f = f
      )
    )
  }

solve_w_f_nleqslv_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_tj(
        bound = bound
      )
    
    initial_w_f <-
      check_initial_value(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper
      ) 
    
    set.seed(seed)
    e <- rnorm(length(bound$lower))
    
    x <- 
      transform_w_f_to_x_nleqslv(
        w = initial_w_f$w,
        f = initial_w_f$f,
        lower = bound$lower,
        upper = bound$upper
      ) +
      e
    
    fn <-
      function(x) {
        w_f <-
          transform_x_to_w_f_nleqslv(
            x = x,
            w = w,
            f = f,
            lower = bound$lower,
            upper = bound$upper
          ) 
        foc <-
          compute_foc_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_f$w,
            f = w_f$f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(foc)
      }
    
    solution <-
      nleqslv::nleqslv(
        x = x,
        fn = fn,
        control =
          list(
            allowSingular = TRUE
          )
      )
    
    x <- solution$x
    
    w_f <-
      transform_x_to_w_f_nleqslv(
        x = x,
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper
      ) 
    
    return(
      list(
        w = w_f$w,
        f = w_f$f,
        objective = max(abs(solution$fvec))
      )
    )
  }

transform_w_f_to_x_bestresponse_itj <-
  function(
    i,
    owner,
    w,
    f,
    lower,
    upper
  ) {
    index <- owner[i, ] %>% as.logical()
    index <- index[3:length(index)]
    index <-
      c(
        index,
        index
      )
    x <-
      transform_w_f_to_x_nleqslv(
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    x <- x[index]
    return(x)
  }

transform_x_to_w_f_bestresponset_itj <-
  function(
    x,
    i,
    owner,
    w,
    f,
    lower,
    upper
  ) {
    index <- owner[i, ] %>% as.logical()
    index <- index[3:length(index)]
    index <-
      c(
        index,
        index
      )
    xx <-
      transform_w_f_to_x_nleqslv(
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    xx[index] <- x
    w_f <-
      transform_x_to_w_f_nleqslv(
        x = xx,
        w = w,
        f = f,
        lower = lower,
        upper = upper
      ) 
    return(w_f)
  }

solve_w_f_bestresponse_itj <-
  function(
    i,
    lower,
    upper,
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    x <-
      transform_w_f_to_x_bestresponse_itj(
        i = i,
        owner = owner,
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    fn <-
      function(x) {
        w_f <-
          transform_x_to_w_f_bestresponset_itj(
            x = x,
            i = i,
            owner = owner,
            w = w,
            f = f,
            lower = lower,
            upper = upper
          ) 
        profit_ths <-
          solve_profit_ths_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_f$w,
            f = w_f$f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(profit_ths[i])
      }
    solution_it <-
      optim(
        par = x,
        fn = fn,
        method = "L-BFGS-B",
        control = list(
          fnscale = -1
        )
      )
    w_f <-
      transform_x_to_w_f_bestresponset_itj(
        x = solution_it$par,
        i = i,
        owner = owner,
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    return(
      list(
        w = w_f$w,
        f = w_f$f,
        objective = -solution_it$value %>% as.numeric()
      )
    )
  }

solve_w_f_bestrsponse_tj <-
  function(
    lower,
    upper,
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    old_w <- w
    old_f <- f
    distance <- 100
    while(distance > 1e-10) {
      for (
        i in 3:nrow(owner)
      ) {
        solution_bestresponse_it <-
          solve_w_f_bestresponse_itj(
            i = i,
            lower = lower,
            upper = upper,
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = old_w,
            f = old_f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        distance_w <-
          max(
            abs(
              solution_bestresponse_it$w - old_w
            )
          )
        distance_f <-
          max(
            abs(
              solution_bestresponse_it$f - old_f
            )
          )
        distance <-
          max(
            distance_w,
            distance_f
          )

        old_w <- solution_bestresponse_it$w
        old_f <- solution_bestresponse_it$f
      }
    }
    return(
      list(
        w = solution_bestresponse_it$w,
        f = solution_bestresponse_it$f
      )
    )
  }

solve_w_f_iteration_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_tj(
        bound = bound
      )
    
    initial_w_f <-
      check_initial_value(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper
      )
    
    solution_bestresponse_t <-
      solve_w_f_bestrsponse_tj(
        lower = bound$lower,
        upper = bound$upper,
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = initial_w_f$w,
        f = initial_w_f$f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size,
        tol = tol,
        use_exp = use_exp
      ) 
    
    return(
      list(
        w = solution_bestresponse_t$w,
        f = solution_bestresponse_t$f
      )
    )
  }

solve_endogenous_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    gamma_w,
    beta_f,
    lambda_f,
    gamma_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ec_w,
    ea_f,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    solver,
    multistart
  ) {
    # solve ths problem for wages and fees
    if (solver == "optim") {
      w_f <-
        foreach(
          n = 1:multistart,
          .packages = 
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %do% {
          solve_w_f_optim_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w,
            f = f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp,
            seed = n
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
    } else if (solver == "nleqslv") {
      w_f <-
        foreach(
          n = 1:multistart,
          .packages = 
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %do% {
          solve_w_f_nleqslv_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w,
            f = f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp,
            seed = n
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
    } else {
      stop("no solver")
    }
    
    # solve client firm shares
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = w_f$f,
        s_f = s_f
      )
    
    # solve worker shares
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w_f$w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else if (method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w_f$w,
          s_f = s_f
        )
    }
    return(
      list(
        w = w_f$w,
        f = w_f$f,
        s_f = s_f,
        s_w = s_w
      )
    )
  }

solve_equilibrium_tj <-
  function(
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    endogenous_tj <-
      solve_endogenous_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        lambda_w = equilibrium$parameter$lambda_w,
        gamma_w = equilibrium$parameter$gamma_w,
        beta_f = equilibrium$parameter$beta_f,
        lambda_f = equilibrium$parameter$lambda_f,
        gamma_f = equilibrium$parameter$gamma_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp,
        solver = solver,
        multistart = multistart
      ) 
    
    
    # update endogenous variables
    equilibrium$endogenous[[t]][[j]]$w <- endogenous_tj$w
    equilibrium$endogenous[[t]][[j]]$f <- endogenous_tj$f
    equilibrium$endogenous[[t]][[j]]$s_f <- endogenous_tj$s_f
    equilibrium$endogenous[[t]][[j]]$s_w <- endogenous_tj$s_w
    
    # return equilibrium
    return(equilibrium)
  }

solve_equilibrium <-
  function(
    equilibrium,
    solver,
    multistart
  ) {
    endogenous <- 
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages = 
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {  
        endogenous_t <-
          foreach (
            j = seq_along(equilibrium$exogenous[[t]])
          ) %do% {
            endogenous_tj <-
              solve_endogenous_tj(
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                lambda_w = equilibrium$parameter$lambda_w,
                gamma_w = equilibrium$parameter$gamma_w,
                beta_f = equilibrium$parameter$beta_f,
                lambda_f = equilibrium$parameter$lambda_f,
                gamma_f = equilibrium$parameter$gamma_f,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
                x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
                x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
                w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                ec_w = equilibrium$shock[[t]][[j]]$ec_w,
                ea_f = equilibrium$shock[[t]][[j]]$ea_f,
                ec_f = equilibrium$shock[[t]][[j]]$ec_f,
                w = equilibrium$endogenous[[t]][[j]]$w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol,
                use_exp = equilibrium$constant$use_exp,
                solver = solver,
                multistart = multistart
              )
            return(endogenous_tj)
          }
        return(endogenous_t)
      }
    for (
      t in seq_along(equilibrium$exogenous)
    ) {
      for (
        j in seq_along(equilibrium$exogenous[[t]])
      ) {
        equilibrium$endogenous[[t]][[j]]$w <- endogenous[[t]][[j]]$w
        equilibrium$endogenous[[t]][[j]]$f <- endogenous[[t]][[j]]$f
        equilibrium$endogenous[[t]][[j]]$s_f <- endogenous[[t]][[j]]$s_f
        equilibrium$endogenous[[t]][[j]]$s_w <- endogenous[[t]][[j]]$s_w
      }
    }
    return(equilibrium)
  }

compute_social_planner_welfare_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f
        )
    }

    profit_ths <-
      compute_profit_ths_tj(
        m_w = m_w,
        m_f = m_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_w = s_w,
        s_f = s_f,
        use_exp = use_exp
      )
    
    surplus_w <-
      compute_surplus_w_tj_exact(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        lambda_w = lambda_w,
        x_a_w = x_a_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        w = w,
        s_f = s_f
      )
    
    surplus_f <-
      compute_surplus_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        ea_f = ea_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        f = f,
        s_f = s_f
      )
    
    welfare <-
      sum(profit_ths, na.rm = TRUE) +
      sum(surplus_w, na.rm = TRUE) +
      sum(surplus_f, na.rm = TRUE)
    
    return(
      list(
        welfare = welfare,
        w = as.matrix(w),
        f = as.matrix(f),
        s_w = as.matrix(s_w),
        s_f = as.matrix(s_f),
        profit_ths = profit_ths,
        surplus_w = surplus_w,
        surplus_f = surplus_f
      )
    )
  }


solve_w_f_social_planner <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    set.seed(seed)
    x <- 
      c(
        as.numeric(w[3:nrow(w), 1]),
        as.numeric(f[3:nrow(f), 1] - w[3:nrow(w), 1])
      )
    e <- rnorm(nrow(w) - 2 + nrow(f) - 2)
    lower <- rep(1e-8, length(x))
    upper <- rep(Inf, length(x))
    
    objective <- 
      function(x) {
        w_f <- 
          transform_x_to_w_f_optim(
            x = x,
            w = w,
            f = f
          )

      obj <-
        compute_social_planner_welfare_tj(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          beta_f = beta_f,
          gamma_w = gamma_w,
          gamma_f = gamma_f,
          lambda_w = lambda_w,
          lambda_f = lambda_f,
          x_a_w = x_a_w,
          x_a_f = x_a_f,
          x_c_w = x_c_w,
          x_c_f = x_c_f,
          size_w = size_w,
          size_f = size_f,
          owner = owner,
          mu = mu,
          ea_w = ea_w,
          ea_f = ea_f,
          ec_w = ec_w,
          ec_f = ec_f,
          w = as.matrix(w_f$w),
          f = as.matrix(w_f$f),
          s_f = s_f,
          method_s_w = method_s_w,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol,
          use_exp = use_exp
        )
      return(-obj$welfare)
    }
    
    solution <-
      optim(
        par = x,
        fn = objective,
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = list(maxit = 1e6)
      )
    
    w_f <- 
      transform_x_to_w_f_optim(
        x = solution$par,
        w = w,
        f = f
      )
    
    w_opt <- as.matrix(w_f$w)
    f_opt <- as.matrix(w_f$f)
    
    outcome <-
      compute_social_planner_welfare_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w_opt,
        f = f_opt,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    return(outcome)
  }

solve_equilibrium_social_planner_tj <-
  function(
    t,
    j,
    equilibrium,
    maxit = 300
  ) {
    solution <-
      solve_w_f_social_planner(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        beta_f = equilibrium$parameter$beta_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp,
        seed = 1
      )
    
    equilibrium$endogenous[[t]][[j]]$w <- solution$w
    equilibrium$endogenous[[t]][[j]]$f <- solution$f
    equilibrium$endogenous[[t]][[j]]$s_w <- solution$s_w
    equilibrium$endogenous[[t]][[j]]$s_f <- solution$s_f
    
    return(equilibrium)
  }

compute_surplus_f_tj <-
  function(
    m_w,
    m_f,
    beta_f,
    lambda_f,
    x_a_f,
    ea_f,
    size_w,
    size_f,
    mu,
    f,
    s_f
  ) {
    meeting_probability_f <-
      compute_meeting_probability_f_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      )
    
    a_f <-  
      compute_a_f_tj_rcpp(
        beta_f = beta_f,
        x_a_f = x_a_f,
        ea_f = ea_f
      ) 
    
    meeting_probability_f_full <-
      rbind(
        1,
        meeting_probability_f
      )
    
    a_f_full <-
      rbind(
        0,
        a_f
      )
    
    s_f_full <-
      rbind(
        1 - sum(s_f),
        s_f
      )
    
    f_full <-
      rbind(
        0,
        f
      )
    
    surplus_f_full <-
      s_f_full *
      meeting_probability_f_full *
      (
        a_f_full + lambda_f * f_full - digamma(1) - log(s_f_full)
      ) /
      (- lambda_f)
    
    surplus_f_full <-
      surplus_f_full *
      size_f
    
    return(surplus_f_full)
    
  }

compute_inclusive_value_w_tj <-
  function(
    lambda_w,
    h_w,
    meeting_probability_w,
    met
  ) {
    unmet <- 
      setdiff(
        1:nrow(meeting_probability_w),
        met)
    h_w_met <- h_w
    h_w_met[unmet] <- 0
    h_w_met <-
      rbind(
        1,
        h_w_met
      )
    s_w_met <-
      h_w_met / sum(h_w_met)
    
    inclusive_value_w_met <-
      s_w_met *
      (
        log(h_w_met) -
          log(s_w_met) -
          digamma(1)
      )
    inclusive_value_w_met[
      h_w_met == 0
    ] <- 0
    inclusive_value_w_met <-
      inclusive_value_w_met /
      lambda_w
    
    return(inclusive_value_w_met)
  }

compute_surplus_w_tj_met <-
  function (
    lambda_w,
    meeting_probability_w,
    h_w,
    met
  ) {
    
    omega <-
      compute_omega_tj(
        meeting_probability_w = meeting_probability_w,
        met = met
      ) 
    
    omega <-
      rbind(
        prod(1 - meeting_probability_w),
        omega
      )
    
    inclusive_value_w <-
      compute_inclusive_value_w_tj(
        lambda_w = lambda_w,
        h_w = h_w,
        meeting_probability_w = meeting_probability_w,
        met = met
      )
    
    surplus_w_met <- omega * inclusive_value_w
    
    return(surplus_w_met)
  }

compute_surplus_w_tj_exact <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    x_a_w,
    size_w,
    size_f,
    mu,
    ea_w,
    w,
    s_f
  ) {
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    
    h_w <-
      compute_h_w_tj(
        a_w = a_w,
        lambda_w = lambda_w,
        w = w
      )
    
    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    
    met_list <-
      rje::powerSet(
        x = 1:nrow(s_f),
        m = nrow(s_f) 
      )
    
    surplus_w <-
      met_list %>%
      purrr::map(
        ~ compute_surplus_w_tj_met(
          lambda_w = lambda_w,
          meeting_probability_w = meeting_probability_w,
          h_w = h_w,
          met = .
        ) 
      ) 
    
    surplus_w <-
      surplus_w %>%
      purrr::reduce(`+`)
    
    surplus_w <-
      surplus_w *
      size_w
    
    return(surplus_w)
    
  }

check_equilibrium_tj <-
  function(
    t,
    j,
    equilibrium
  ) {
    foc_s_f <-
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
    
    foc_w_f <-
      compute_foc_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        beta_f = equilibrium$parameter$beta_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        method_s_w = equilibrium$constant$method_s_w,     
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    foc_w <-
      foc_w_f[
        1:(length(foc_w_f) / 2)
      ]
    foc_f <-
      foc_w_f[
        (length(foc_w_f)/2 + 1):length(foc_w_f)
      ]
    
    # compute ths profit and surplus
    profit_ths_tj <-
      compute_profit_ths_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_w = equilibrium$endogenous[[t]][[j]]$s_w,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    surplus_w_tj <- 
      compute_surplus_w_tj_exact(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        lambda_w = equilibrium$parameter$lambda_w,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        w = equilibrium$endogenous[[t]][[j]]$w,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    surplus_f_tj <-
      compute_surplus_f_tj(
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
    
    # compute meeting probability
    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      ) 
    
    meeting_probability_f <-
      compute_meeting_probability_f_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    # compute cost 
    c_w <-
      compute_c_w_tj(
        gamma_w = equilibrium$parameter$gamma_w,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        use_exp = equilibrium$constant$use_exp
      )
    c_f <-
      compute_c_f_tj(
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        use_exp = equilibrium$constant$use_exp
      )
    
    # summarize the results
    df <-
      data.frame(
        i = factor(0:length(equilibrium$endogenous[[t]][[j]]$w)),
        foc_w = c(NA, NA, NA, foc_w),
        foc_f = c(NA, NA, NA, foc_f),
        foc_s_f = c(NA, foc_s_f),
        w = c(NA, equilibrium$endogenous[[t]][[j]]$w),
        f = c(NA, equilibrium$endogenous[[t]][[j]]$f),
        m = 
          c(NA, equilibrium$endogenous[[t]][[j]]$f) - 
          c(NA, equilibrium$endogenous[[t]][[j]]$w),
        s_w = 
          c(
            1 - sum(equilibrium$endogenous[[t]][[j]]$s_w * meeting_probability_w), 
            equilibrium$endogenous[[t]][[j]]$s_w
          ),
        s_f = 
          c(
            1 - sum(equilibrium$endogenous[[t]][[j]]$s_f), 
            equilibrium$endogenous[[t]][[j]]$s_f
          ),  
        meeting_probability_w = c(NA, meeting_probability_w),
        meeting_probability_f = c(NA, meeting_probability_f),
        c_w = c(NA, c_w),
        c_f = c(NA, c_f),
        profit_ths = c(NA, profit_ths_tj),
        surplus_w = surplus_w_tj,
        surplus_f = surplus_f_tj
      )
    
    return(df)
  }

evaluate_equilibrium_tj <-
  function (
    x,
    target,
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    if (target == "mu") {
      equilibrium$shock[[t]][[j]]$mu[
        length(equilibrium$shock[[t]][[j]]$mu)
      ] <- x
    } else if(target == "size_w" | target == "size_f") {
      equilibrium$exogenous[[t]][[j]][[target]][
        length(equilibrium$exogenous[[t]][[j]][[target]])
      ] <- x
    }else {
      equilibrium$parameter[[target]][
        length(equilibrium$parameter[[target]])
      ] <- x
    }
    equilibrium$parameter$m_f <- 
      1 - 
      equilibrium$parameter$m_w
    
    df <-
      tryCatch(
        {
          equilibrium <-
            solve_equilibrium_tj(
              t = t,
              j = j,
              equilibrium = equilibrium,
              solver = solver,
              multistart = multistart
            ) 
          
          df <- 
            check_equilibrium_tj(
              t = t,
              j = j,
              equilibrium = equilibrium
            ) 
          
          df <-
            df %>%
            dplyr::mutate(
              target = rep(x, nrow(df))
            ) %>%
            dplyr::select(
              target,
              dplyr::everything()
            )
        }, error = function(e) {
          df <- NULL
        }
      )
    return(df)
  }

plot_comparative <-
  function(
    comparative
  ) {
    comparative$segment <- 
      dplyr::if_else(
        abs(comparative$foc_w) < 1e-03 & abs(comparative$foc_f) < 1e-03, 
        1, 
        0
      )
    variables <-
      comparative %>%
      dplyr::select(
        -target,
        -i
      ) %>%
      colnames() 
    
    base <-
      comparative %>%
      dplyr::pull(target)
    
    base <- base[length(base)/2]
    
    p <-
      foreach (
        i = seq_along(variables)
      ) %do% {
        p <-
          ggplot(
            comparative,
            aes(
              x = target,
              y = !!sym(variables[i]),
              color = i
            )
          ) +
          geom_line(
            data = subset(
              comparative, 
              segment == 1
            ),
            linetype = "solid"
          ) + 
          geom_line(
            data = subset(
              comparative, 
              segment != 1
            ),
            linetype = "dashed"
          ) +
          geom_vline(
            xintercept = base,
            linetype = "dotted"
          ) +
          geom_hline(
            yintercept = 0,
            linetype = "dotted"
          ) +
          theme_classic() 
        p <- p +
          geom_line(
            data = subset(
              comparative, 
              i == 0),
            aes(
              x = target,
              y = !!sym(variables[i])
            )
          ) +
          geom_line(
            data = subset(
              comparative, 
              i == 1),
            aes(
              x = target,
              y = !!sym(variables[i])
            )
          )
        if (grepl("foc", variables[i])) {
          p <-
            p +
            ylim(
              c(
                -0.1, 
                0.1
              )
            )
        }
        return(p)
      }
    p <-
      p %>%
      magrittr::set_names(variables)
    p <- p[names(p) %in% "segment" == FALSE] 
    for (i in seq_along(p)) {
      cat(
        "#### ",
        names(p)[i],
        "\n\n"
      )
      print(p[[i]])
      cat("\n\n")
    }
    
    return(p)
  }

evaluate_comparative_tj <-
  function(
    target,
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    if (target == "mu") {
      base <- 
        equilibrium$shock[[t]][[j]]$mu[
          length(equilibrium$shock[[t]][[j]]$mu)
        ]
    } else if (target == "size_w" | target == "size_f"  ) {
      base <- equilibrium$exogenous[[t]][[j]][[target]][
        length(equilibrium$exogenous[[t]][[j]][[target]])
      ]
    } else {
      base <- 
        equilibrium$parameter[[target]][
          length(equilibrium$parameter[[target]])
        ]
    }
    
    start <- base * 0.5
    end <- base * 1.5
    increment <- (end - start) / 10
    comparative <-
      seq(
        start,
        end,
        by = increment
      ) %>%
      purrr::map(
        ~ evaluate_equilibrium_tj(
          .,
          target = target,
          t = t,
          j = j,
          equilibrium = equilibrium,
          solver = solver,
          multistart = multistart
        ) 
      ) %>%
      dplyr::bind_rows()
    return(comparative)
  }


compute_elasticity_comparative <- 
  function(
    comparative
  ) {
    df <- comparative %>%
      dplyr::arrange(i) %>%
      dplyr::group_by(i) %>%
      dplyr::mutate(
        percentage_change_w = (w - dplyr::lag(w)) / dplyr::lag(w) * 100,
        percentage_change_f = (f - dplyr::lag(f)) / dplyr::lag(f) * 100,
        percentage_change_s_w = (s_w - dplyr::lag(s_w)) / dplyr::lag(s_w) * 100,
        percentage_change_s_f = (s_f - dplyr::lag(s_f)) / dplyr::lag(s_f) * 100,
        percentage_change_target = (target - dplyr::lag(target)) / dplyr::lag(target) * 100,
        unit_change_target = target - dplyr::lag(target),
        elasticity_w = percentage_change_w / percentage_change_target,
        elasticity_f = percentage_change_f / percentage_change_target,    
        elasticity_s_w = percentage_change_s_w / percentage_change_target,    
        elasticity_s_f = percentage_change_s_f / percentage_change_target,
        semi_elasticity_w = percentage_change_w / unit_change_target,
        semi_elasticity_f = percentage_change_w / unit_change_target, 
        semi_elasticity_s_w = percentage_change_s_w / unit_change_target,    
        semi_elasticity_s_f = percentage_change_s_f / unit_change_target
      ) %>%
      dplyr::ungroup() 
    
    df$target <- target
    
    result_df <- 
      df %>%
      dplyr::select(
        target,
        i,
        elasticity_w,
        elasticity_f,
        elasticity_s_w,
        elasticity_s_f,
        semi_elasticity_w,
        semi_elasticity_f,
        semi_elasticity_s_w,
        semi_elasticity_s_f
      ) 
    
    return(result_df)
  }

compute_elasticity_index <- 
  function(
    comparative
  ) {
    base <-
      comparative %>%
      dplyr::pull(target)
    
    base <- base[length(base)/2]
    
    index <- 
      which(
        comparative$target == base
      ) + 1
    
    return(index)
  }

check_c1_1 <-
  function(
    equilibrium
  ) {
    m_f <- equilibrium$parameter$m_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    condition <-
      ((2 - m_f) / (m_f * -lambda_f)) - (1 / lambda_w)
    return(condition >= 0)
  }

compute_a_cap <-
  function(
    f3,
    t,
    j,
    equilibrium
  ) {
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    gamma_w <- equilibrium$parameter$gamma_w
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    use_exp <- equilibrium$constant$use_exp
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f_z,
        s_f = s_f
      )
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    a_cap <-
      f_z[3] - c_w[3] -
      (
        (m_f - 1) * s_f[3] + 2 - m_f
      ) /
      (
        - m_f * lambda_f * (1 - s_f[3])
      )
    return(a_cap)
  }

solve_f3_under <-
  function(
    t,
    j,
    equilibrium
  ) {
    fun <-
      function(
    x
      ) {
        a_cap <- 
          compute_a_cap(
            f3 = x,
            t = t,
            j = j,
            equilibrium = equilibrium
          ) 
        return(a_cap)
      }
    solution <-
      uniroot(
        f = fun,
        lower = 0,
        upper = 10000
      )      
    f3_under <- solution$root
    return(f3_under)
  }

check_c1_3 <-
  function(
    t,
    j,
    equilibrium
  ) {
    f3_under <- 
      solve_f3_under(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3_under
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f_z,
        s_f = s_f
      )
    condition <-
      (
        (m_f - 1) * s_f[3] + 2 - m_f
      ) /
      (
        - m_f * lambda_f * (1 - s_f[3])
      ) -
      1 / lambda_w
    return(condition > 0)
  }

compute_condittion_f3_over <-
  function(
    f3,
    t,
    j,
    equilibrium
  ) {
    size_f <- equilibrium$endogenous$size_f
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_f <- equilibrium$parameter$beta_f
    lambda_w <- equilibrium$parameter$lambda_w
    lambda_f <- equilibrium$parameter$lambda_f
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f_z,
        s_f = s_f
      )
    check <-
      (
        lambda_w *
          (
            (m_f - 1) * s_f[3] + 2 - m_f
          )
      ) /
      (
        m_f * (-lambda_f) *
          (1 - s_f[3])
      ) - 1
    return(check)
  }

compute_b_cap <-
  function(
    f3,
    t,
    j,
    equilibrium
  ) {
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_w <- equilibrium$parameter$beta_w
    beta_f <- equilibrium$parameter$beta_f
    lambda_w <- equilibrium$parameter$lambda_w
    lambda_f <- equilibrium$parameter$lambda_f
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_w <- equilibrium$shock[[t]][[j]]$ea_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f_z,
        s_f = s_f
      )
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    check <-
      (
        lambda_w *
          (
            (m_f - 1) * s_f[3] + 2 - m_f
          )
      ) /
      (
        m_f * (-lambda_f) *
          (1 - s_f[3])
      ) - 1
    if (check < 0) return(NA)
    
    b_cap <-
      (
        log(check) - a_w[3]
      ) /
      lambda_w
    return(b_cap)
  }

compute_c_cap <-
  function(
    f3,
    t,
    j,
    equilibrium
  ) {
    a_cap <-
      compute_a_cap(
        f3 = f3,
        t = t,
        j = j,
        equilibrium = equilibrium
      ) 
    b_cap <-
      compute_b_cap(
        f3 = f3,
        t = t,
        j = j,
        equilibrium = equilibrium
      ) 
    c_cap <- a_cap - b_cap
    return(c_cap)
  }

check_ec_lhs <- 
  function(
    t,
    j,
    equilibrium
  ) {
    f3_under <- 
      solve_f3_under(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    c_cap <- 
      compute_c_cap(
        f3 = f3_under,
        t = t,
        j = j,
        equilibrium = equilibrium
      ) 
    condition <- c_cap
    return(condition < 0)
  }

check_ec_rhs <- 
  function(
    t,
    j,
    equilibrium
  ) {
    f3_under <- 
      solve_f3_under(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    beta_w <- equilibrium$parameter$beta_w
    lambda_w <- equilibrium$parameter$lambda_w
    gamma_w <- equilibrium$parameter$gamma_w
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    ea_w <- equilibrium$shock[[t]][[j]]$ea_w  
    use_exp <- equilibrium$constant$use_exp
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    check <- lambda_w * (f3_under - c_w[3]) - 1
    if (check < 0) return(NA)
    
    ec_rhs <- log(check) - a_w[3]
    return(ec_rhs > 0)
  }

check_foc <-
  function(
    t,
    equilibrium
  ) {
    fun <-
      function(
    x
      ) {
        c_cap <- 
          compute_c_cap(
            f3 = x,
            t = t,
            equilibrium = equilibrium
          ) 
        return(c_cap)
      }
    solution <-
      uniroot(
        f = fun,
        lower = 0,
        upper = 1e17
      )      
    f3_star <- solution$root
    return(f3_star)
  }

check_sec_1 <- 
  function(
    t,
    j,
    equilibrium
  ) {
    m_f <- equilibrium$parameter$m_f
    beta_w <- equilibrium$parameter$beta_w
    lambda_w <- equilibrium$parameter$lambda_w
    lambda_f <- equilibrium$parameter$lambda_f
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    ea_w <- equilibrium$shock[[t]][[j]]$ea_w
    
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      ) 
    
    check <-  
      lambda_w * 
      (2 - m_f) / 
      (m_f * (- lambda_f)) - 1
    if (check < 0) return(NA)
    sec_1 <- 
      log(check) - a_w[3]
    return(sec_1 >= 0)
  }

check_c1_2 <- 
  function(
    equilibrium
  ) {
    m_f <- equilibrium$parameter$m_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    condition <-
      ((2 - m_f)/(m_f * -lambda_f)) - (1/lambda_w)
    return(condition < 0)
  }

check_sc1_3 <- 
  function(
    f3,
    t,
    j,
    equilibrium
  ) {
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    gamma_w <- equilibrium$parameter$gamma_w
    x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    use_exp <- equilibrium$constant$use_exp
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    h_f <-
      compute_h_f_tj(
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        ea_f = ea_f,
        f = f_z
      )
    mu_f <- mu[3] * (size_w^m_w)
    
    condition_1 <- 
      c_w[3] +
      (3 - m_f) / 
      (m_f * (-lambda_f)
      )
    
    condition_2 <- 
      (
        (mu_f * h_f[3]) ^ (1 + m_f) *
          ((size_f/2) ^ (m_f^2 - 1)) +
          2 - m_f
      )/
      (m_f * (-lambda_f)) 
    
    f_hat <- max(condition_1,condition_2)
    f_z[3] <- f_hat
    
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f_z,
        s_f = s_f
      )
    condition <-
      (
        (m_f - 1) * s_f[3] + 2 - m_f
      ) /
      (
        - m_f * lambda_f * (1 - s_f[3])
      ) -
      1 / lambda_w
    
    return(condition > 0)
  }

check_sec_2 <- 
  function(
    f3,
    t,
    j,
    equilibrium
  ) {
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    gamma_w <- equilibrium$parameter$gamma_w
    x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    use_exp <- equilibrium$constant$use_exp
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    h_f <-
      compute_h_f_tj(
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        ea_f = ea_f,
        f = f_z
      )
    mu_f <- mu[3] * (size_w^m_w)
    
    condition_1 <- 
      c_w[3] +
      (3 - m_f) / 
      (m_f * (-lambda_f)
      )
    
    condition_2 <- 
      (
        (mu_f * h_f[3]) ^ (1 + m_f) *
          ((size_f/2) ^ (m_f^2 - 1)) +
          2 - m_f
      )/
      (m_f * (-lambda_f)) 
    
    f_hat <- max(condition_1,condition_2)
    f_z[3] <- f_hat
    
    condition <-
      compute_c_cap(
        f3 = f_z[3],
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    if (is.na(condition)) {
      return(FALSE)
    }else if( condition > 0) {
      return(FALSE)
    }else{
      return(TRUE)
    }
  }

solve_f3_ab_tj <-
  function(
    t,
    j,
    equilibrium
  ) {
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    
    fun <-
      function(
    x
      ) {
        base_fun <- 
          compute_a_cap(
            f3 = x,
            t = t,
            j = j,
            equilibrium = equilibrium
          ) -
          compute_b_cap(
            f3 = x,
            t = t,
            j = j,
            equilibrium = equilibrium
          ) 
        return(base_fun)
      }
    
    check_c12 <- 
      check_c1_2(
        equilibrium = equilibrium
      )
    
    if ( check_c12 == TRUE) {
      upper_bound <- 
        solve_f3_upper_bar(
          m_f = m_f,
          m_w = m_w,
          f = f,
          lambda_f = lambda_f ,
          lambda_w = lambda_w,
          beta_f = beta_f,
          x_a_f = x_a_f,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_f = ea_f,
          s_f = s_f 
        )
      upper <- upper_bound - 1.0e-4
      solution <-
        uniroot(
          f = fun,
          lower = 0,
          upper = upper
        )   
      return(solution$root)
    }else{
      solution <-
        uniroot(
          f = fun,
          lower = 0,
          upper = 1000
        )      
      f3 <- solution$root
      return(f3)
    }
  }

solve_w3_ab_tj <- 
  function(
    t,
    j,
    equilibrium
  ) {
    f3 <- 
      solve_f3_ab_tj(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    f_z <- equilibrium$endogenous[[t]][[j]]$f
    f_z[3] <- f3
    lambda_w <- equilibrium$parameter$lambda_w
    m_f <- equilibrium$parameter$m_f
    m_w <- equilibrium$parameter$m_w
    beta_f <- equilibrium$parameter$beta_f
    beta_w <- equilibrium$parameter$beta_w
    lambda_f <- equilibrium$parameter$lambda_f
    gamma_w <- equilibrium$parameter$gamma_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    ea_w <- equilibrium$shock[[t]][[j]]$ec_w
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    mu <- equilibrium$shock[[t]][[j]]$mu 
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    a_w <-
      compute_a_w_tj(
        beta_w = beta_w,
        x_a_w = x_a_w,
        ea_w = ea_w
      )
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f_z,
        s_f = s_f
      )
    w_2 <- lambda_w/ 
      (m_f * -lambda_f) *
      ((m_f -1)*s_f[3] + 2 - m_f) / 
      (1 - s_f[3]) - 1
    
    if (w_2 <= 0) {
      return(NA)
    } else if (w_2 > 0) {
      w_2 <- (log(w_2) - a_w[3])/lambda_w
      return(w_2)
    }
  }

solve_equilibrium_ab_tj <- 
  function(
    t,
    j,
    equilibrium
  ) {
    f_3 <- 
      solve_f3_ab_tj(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    w_3 <- 
      solve_w3_ab_tj(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_w <- equilibrium$parameter$beta_w
    lambda_w <- equilibrium$parameter$lambda_w
    gamma_w <- equilibrium$parameter$gamma_w
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    gamma_f <- equilibrium$parameter$gamma_f
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    w_0 <- equilibrium$exogenous[[t]][[j]]$w_0
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    owner <- equilibrium$exogenous[[t]][[j]]$owner
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_w <- equilibrium$shock[[t]][[j]]$ea_w
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    ec_f <- equilibrium$shock[[t]][[j]]$ec_f
    w <- equilibrium$endogenous[[t]][[j]]$w
    w[3] <- w_3
    f <- equilibrium$endogenous[[t]][[j]]$f
    f[3] <- f_3
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    method_s_w <- equilibrium$constant$method_s_w
    margin <- equilibrium$constant$margin
    quadrature_size <- equilibrium$constant$quadrature_size
    tol <- equilibrium$constant$tol
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else if (method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f
        )
    }
    
    equilibrium$endogenous[[t]][[j]]$w <- w
    equilibrium$endogenous[[t]][[j]]$f <- f
    equilibrium$endogenous[[t]][[j]]$s_f <- s_f
    equilibrium$endogenous[[t]][[j]]$s_w <- s_w
    return(equilibrium)
  }

compute_foc_ab_tj <- 
  function(
    t,
    j,
    equilibrium
  ) {
    equilibrium <- 
      solve_equilibrium_ab_tj(
        t = t,
        j = j,
        equilibrium = equilibrium
      )
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_w <- equilibrium$parameter$beta_w
    beta_f <- equilibrium$parameter$beta_f
    gamma_w <- equilibrium$parameter$gamma_w
    gamma_f <- equilibrium$parameter$gamma_f
    lambda_f <- equilibrium$parameter$lambda_f
    lambda_w <- equilibrium$parameter$lambda_w
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    x_c_f <- equilibrium$exogenous[[t]][[j]]$x_c_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    owner <- equilibrium$exogenous[[t]][[j]]$owner
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_w <- equilibrium$shock[[t]][[j]]$ea_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    ec_f <- equilibrium$shock[[t]][[j]]$ec_f
    w <- equilibrium$endogenous[[t]][[j]]$w
    f <- equilibrium$endogenous[[t]][[j]]$f
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    s_w <- equilibrium$endogenous[[t]][[j]]$s_w
    margin <- equilibrium$constant$margin
    quadrature_size <- equilibrium$constant$quadrature_size
    tol <- equilibrium$constant$tol
    use_rcpp <- equilibrium$constant$use_rcpp
    foc <- 
      compute_foc_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = "exact",
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      ) 
    return(foc)
  }


compute_f_upper_bound <-
  function(
    beta_f, 
    lambda_f, 
    x_a_f, 
    ea_f, 
    initial_f = 0, 
    target_h_f = 1.5e-15, 
    step_size = 0.1, 
    max_iter = 1.0e4
  ) {
    f <- initial_f
    
    h_f <- 
      compute_h_f_tj (
        beta_f = beta_f, 
        lambda_f = lambda_f, 
        x_a_f = x_a_f, 
        ea_f = ea_f, 
        f = f
      )
    
    iter <- 0
    
    while( max(h_f) >= target_h_f 
           && iter < max_iter
    ) {
      f <- f + step_size
      h_f <- 
        compute_h_f_tj (
          beta_f = beta_f, 
          lambda_f = lambda_f, 
          x_a_f = x_a_f, 
          ea_f = ea_f, 
          f = f
        )
      iter <- iter + 1
    }
    
    
    if (max(h_f) < target_h_f) {
      f <- f - step_size
    }
    
    return(f)
  }

solve_f2_upper_bar <- 
  function(
    m_f,
    m_w,
    f,
    lambda_f,
    lambda_w,
    beta_f,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_f,
    s_f
  ) {
    fun <- 
      function(x) {
        s_f <-
          solve_s_f_tj_rcpp(
            m_w = m_w,
            m_f = m_f,
            beta_f = beta_f,
            lambda_f = lambda_f,
            x_a_f = x_a_f,
            size_w = size_w,
            size_f = size_f,
            mu = mu,
            ea_f = ea_f,
            f = x,
            s_f = s_f
          )
        check <- (
          (m_f -1) * s_f[2] + 2 - m_f
        )/(
          m_f * -lambda_f *(1 - s_f[2])
        ) - (1/lambda_w)
        
        return(check)
      }
    
    solution <-
      uniroot(
        f = fun,
        lower = 0,
        upper = 10000
      ) 
    
    return(solution$root)
  }

check_equilibrium_path_tj <- 
  function(
    t,
    j,
    equilibrium
  ) {
    c11 <- 
      check_c1_1(
        equilibrium = equilibrium
      ) 
    
    if (c11 == TRUE) {
      
      sec1 <- 
        check_sec_1(
          t = t,
          j = j,
          equilibrium = equilibrium
        )
      
      sec2 <- 
        check_sec_2(
          t = t,
          j = j,
          f3 = 0,
          equilibrium = equilibrium
        )
      
      if ( sec1 | sec2 == TRUE) {
        return( "Path2: Equilirbium exists")
      }else{
        ec <- 
          check_ec_lhs(
            t = t,
            j = j,
            equilibrium = equilibrium
          )
        if (ec == TRUE) {
          return( "Path1: Equilirbium exists")
        }else{
          return("No equilibrium")
        }
      }
      
    }else{
      sc13 <- 
        check_sc1_3(
          t = t,
          j = j,
          f3 = 0,
          equilibrium = equilibrium
        )
      
      if (sc13 == TRUE) {
        sec2 <- 
          check_sec_2(
            t = t,
            j = j,
            f3 = 0,
            equilibrium = equilibrium
          )
        if (sec2 == TRUE) {
          return("Path3: Equilirbium exists")
        }else{
          ec <- 
            check_ec_lhs(
              t = t,
              j = j,
              equilibrium = equilibrium
            )
          if (ec == TRUE) {
            return("Path4: Equilirbium exists")
          }else{
            return("No equilibrium")
          }
        }
      }else{
        
        c13 <- 
          check_c1_3(
            t = t,
            j = j,
            equilibrium = equilibrium
          ) 
        
        if (c13 == TRUE) {
          sec2 <- 
            check_sec_2(
              t = t,
              j = j,
              f3 = 0,
              equilibrium = equilibrium
            )
          if (sec2 == TRUE) {
            
            return("Path5: Equilirbium exists")
          }else{
            
            ec <- 
              check_ec_lhs(
                t = t,
                j = j,
                equilibrium = equilibrium
              )
            if (ec == TRUE) {
              return("Path6: Equilirbium exists")
            }else{
              return("No equilibrium")
            }
          }
        }else{
          
          return("No equilibrium")
        }
      }
      
    }
    
  }

compute_foc_minimum_wage_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_w,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    # compute unconstrained foc
    foc_tj <- 
      compute_foc_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      ) 
    
    # add lagrange multipliers to foc_w_t 
    foc_tj[
      1:nrow(eta_w)
    ] <- 
      foc_tj[
        1:nrow(eta_w)
      ] +
      eta_w
    
    # w_0 type change
    w_0 <- w_0 %>% as.numeric()
    
    # consider unconstrained case 
    diff <- 
      w[
        3:nrow(w),
        ,
        drop = FALSE
      ] - 
      w_0
    
    diff <- 
      ifelse(
        diff >= 0,
        0,
        diff
      )
    # add complementarity slackness condition
    complementarity <-
      eta_w * diff
    
    foc_tj <-
      rbind(
        foc_tj,
        complementarity
      )
    return(foc_tj)
  }

relax_bound_minimum_wage_tj <-
  function(
    bound,
    w_0
  ) {
    # adjust lower 
    lower_w <-
      rep(
        w_0,
        length(bound$lower)/2
      ) %>%
      as.matrix()
    
    bound$lower[1:(length(bound$lower)/2)] <-
      lower_w
    
    # adjust upper
    bound$upper <-
      ifelse(
        bound$upper > 0,
        2 * bound$upper,
        0.1 * bound$upper
      )
    
    return(bound)
  }

transform_eta_w_f_to_x_minimum_wage_nleqslv <-
  function(
    eta_w,
    w,
    f,
    lower,
    upper
  ) {
    x_eta_w <- log(eta_w)
    
    x_w_f <-
      transform_w_f_to_x_nleqslv(
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    
    x <- 
      c(
        x_eta_w,
        x_w_f
      )
    return(x)
  }

transform_x_to_eta_w_f_minimum_wage_nleqslv <-
  function(
    x,
    w,
    f,
    lower,
    upper
  ) {
    x_eta <-
      x[
        1:(length(w) - 2)
      ]
    x_w_f <-
      x[
        (length(w) - 2 + 1):length(x)
      ]
    eta_w <-
      exp(x_eta) %>%
      as.matrix()
    
    w_f <-
      transform_x_to_w_f_nleqslv(
        x = x_w_f,
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    
    return(
      list(
        eta_w = eta_w,
        w = w_f$w,
        f = w_f$f
      )
    )
  }

solve_w_f_minimum_wage_nleqslv_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_w,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_minimum_wage_tj(
        bound = bound,
        w_0 = w_0
      ) 
    
    initial_w_f <-
      check_initial_value(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper
      ) 
    
    set.seed(seed)
    e <- rnorm(length(bound$lower) + length(eta_w))
    
    x <- 
      transform_eta_w_f_to_x_minimum_wage_nleqslv(
        eta_w = eta_w,
        w = initial_w_f$w,
        f = initial_w_f$f,
        lower = bound$lower,
        upper = bound$upper
      ) + e
    
    fn <-
      function(x) {
        eta_w_f <-
          transform_x_to_eta_w_f_minimum_wage_nleqslv(
            x = x,
            w = initial_w_f$w,
            f = initial_w_f$f,
            lower = bound$lower,
            upper = bound$upper
          ) 
        
        foc <-
          compute_foc_minimum_wage_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = eta_w_f$w,
            f = eta_w_f$f,
            s_f = s_f,
            eta_w = eta_w_f$eta_w,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(foc)
      }
    
    solution <-
      nleqslv::nleqslv(
        x = x,
        fn = fn,
        control =
          list(
            allowSingular = TRUE
          )
      )
    
    eta_w_f <-
      transform_x_to_eta_w_f_minimum_wage_nleqslv(
        x = solution$x,
        w = initial_w_f$w,
        f = initial_w_f$f,
        lower = bound$lower,
        upper = bound$upper
      )  
    
    return(
      list(
        eta_w = eta_w_f$eta_w,
        w = eta_w_f$w,
        f = eta_w_f$f,
        objective = max(abs(solution$fvec))
      )
    )
  }

solve_w_f_minimum_wage_iteration_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_minimum_wage_tj(
        bound = bound,
        w_0 = w_0
      ) 
    
    initial_w_f <-
      check_initial_value(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper
      ) 
    
    solution_bestresponse_tj <-
      solve_w_f_bestrsponse_tj(
        lower = bound$lower,
        upper = bound$upper,
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = initial_w_f$w,
        f = initial_w_f$f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size,
        tol = tol,
        use_exp = use_exp
      ) 
    
    return(
      list(
        w = solution_bestresponse_tj$w,
        f = solution_bestresponse_tj$f
      )
    )
  }

relax_bound_maximum_margin_tj <- 
  function(
    bound
  ){
    # adjust lower
    bound$lower <- 
      ifelse(
        bound$lower > 0,
        - 10 * bound$lower,
        3 * bound$lower
      )
    bound$lower <- 
      ifelse(
        bound$lower == 0,
        -1e2,
        bound$lower
      )
    # adjust upper
    bound$upper <-
      ifelse(
        bound$upper > 0,
        2 * bound$upper,
        0.1 * bound$upper
      )
    return(bound)
  }

transform_eta_w_f_to_x_maximum_margin_nleqslv <-
  function(
    w,
    f,
    eta_f,
    lower,
    upper,
    m_bar
  ) {
    x_eta_f <- 
      log(
        eta_f
      )
    x_w <- 
      rep(
        0,
        length(w) - 2
      ) %>%
      as.matrix()
    
    lower_w <-
      lower[1:(length(w) - 2)]
    upper_w <-
      upper[1:(length(w) - 2)]
    
    x_w <-
      log(
        (w[3:length(w)] - lower_w) /
          (upper_w - w[3:length(w)])
      )
    
    m <-
      (f - w) / f
    
    m <-
      m[3:length(m)]
    
    m <-
      m / m_bar
    
    x_m <-
      log(
        m / (1 - m)
      )
    
    x <-
      c(
        x_eta_f,
        x_w,
        x_m
      )
    
    return(x)
  }

transform_x_to_eta_w_f_maximum_margin_nleqslv <-
  function(
    x,
    w,
    f,
    lower,
    upper,
    m_bar
  ) {
    # slice
    x_eta <-
      x[
        1:(length(f) - 2)
      ]
    x_w_m <-
      x[
        (length(f) - 2 + 1):length(x)
      ]
    x_w <-
      x_w_m[1:(length(w) - 2)]
    x_m <-
      x_w_m[(length(w) - 1):length(x_w_m)]
    
    # transform
    eta_f <-
      exp(x_eta) %>%
      as.matrix()
    
    lower_w <-
      lower[1:(length(w) - 2)]
    upper_w <-
      upper[1:(length(w) - 2)]
    
    w_x <- 
      lower_w + 
      (upper_w - lower_w) * 
      exp(x_w) / (1 + exp(x_w))
    
    m_x <-
      m_bar * exp(x_m) / (1 + exp(x_m))
    
    f_x <-
      w_x / (1 - m_x)
    
    return(
      list(
        eta_f = eta_f %>% as.matrix(),
        w = 
          c(
            w[1],
            w[2],
            w_x
          ) %>% 
          as.matrix(),
        f = 
          c(
            f[1],
            f[2],
            f_x
          )%>% 
          as.matrix()
      )
    )
  }

solve_w_f_maximum_margin_nloptr_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_maximum_margin_tj(
        bound = bound
      )
    
    initial_w_f <-
      check_initial_value_maximum_margin(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper,
        m_bar = m_bar
      )
    set.seed(seed)
    e <- rnorm(length(bound$lower) + length(eta_f))
    
    x <-
      transform_eta_w_f_to_x_maximum_margin(
        eta_f = eta_f,
        w = initial_w_f$w,
        f = initial_w_f$f
      ) + e
    
    eval_f <-
      function(
    x
      ) {
        eta_w_f <-
          transform_x_to_eta_w_f_maximum_margin(
            x = x,
            w = initial_w_f$w,
            f = initial_w_f$f    
          )
        foc <-
          compute_foc_maximum_margin_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = eta_w_f$w,
            f = eta_w_f$f,
            s_f = s_f,
            eta_f = eta_w_f$eta_f,
            m_bar = m_bar,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(
          max(
            abs(
              foc
            )
          )
        )
      }
    
    eval_grad_f <-
      function(
    x
      ) {
        grad_f <-
          numDeriv::grad(
            f = eval_f,
            x = x
          )
        return(grad_f)
      }
    
    eval_g_ineq <-
      function(
    x
      ) {
        eta_w_f <-
          transform_x_to_eta_w_f_maximum_margin(
            x = x,
            w = initial_w_f$w,
            f = initial_w_f$f    
          )
        m <-
          (eta_w_f$f - eta_w_f$w) / eta_w_f$f
        m <-
          m[2:length(m)]
        return(
          m - m_bar
        )
      }
    
    eval_jac_g_ineq <-
      function(x) {
        jac_g_ineq <-
          numDeriv::jacobian(
            f = eval_g_ineq,
            x = x
          )
        return(jac_g_ineq)
      }
    
    lower <-
      c(
        rep(
          -100,
          length(eta_f)
        ),
        0.1 * bound$lower
      )
    upper <-
      c(
        rep(
          Inf,
          length(eta_f)
        ),
        bound$upper
      )
    
    solution <-
      nloptr::nloptr(
        x0 = x,
        eval_f = eval_f,
        eval_grad_f = eval_grad_f,
        lb = lower,
        ub = upper,
        eval_g_ineq = eval_g_ineq,
        eval_jac_g_ineq = eval_jac_g_ineq,
        opts = list(
          algorithm = "NLOPT_LD_SLSQP",
          xtol_rel = 1e-4
        )
      )
    
    eta_w_f <-
      transform_x_to_eta_w_f_maximum_margin(
        x = solution$solution,
        w = initial_w_f$w,
        f = initial_w_f$f
      ) 
    
    return(
      list(
        eta_f = eta_w_f$eta_f,
        w = eta_w_f$w,
        f = eta_w_f$f,
        objective = solution$objective
      )
    )
  }


solve_equilibrium_multiple_initials_maximum_margin_nleqslv_tj <- 
  function(
    t,
    j,
    equilibrium,
    solver,
    m_bar
  ) {
    m_w <- equilibrium$parameter$m_w
    m_f <- equilibrium$parameter$m_f
    beta_w <- equilibrium$parameter$beta_w
    lambda_w <- equilibrium$parameter$lambda_w
    gamma_w <- equilibrium$parameter$gamma_w
    beta_f <- equilibrium$parameter$beta_f
    lambda_f <- equilibrium$parameter$lambda_f
    gamma_f <- equilibrium$parameter$gamma_f
    x_a_w <- equilibrium$exogenous[[t]][[j]]$x_a_w
    x_a_f <- equilibrium$exogenous[[t]][[j]]$x_a_f
    x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
    x_c_f <- equilibrium$exogenous[[t]][[j]]$x_c_f
    size_w <- equilibrium$exogenous[[t]][[j]]$size_w
    size_f <- equilibrium$exogenous[[t]][[j]]$size_f
    owner <- equilibrium$exogenous[[t]][[j]]$owner
    w_0 <- equilibrium$exogenous[[t]][[j]]$w_0
    mu <- equilibrium$shock[[t]][[j]]$mu
    ea_w <- equilibrium$shock[[t]][[j]]$ea_w
    ec_w <- equilibrium$shock[[t]][[j]]$ec_w
    ea_f <- equilibrium$shock[[t]][[j]]$ea_f
    ec_f <- equilibrium$shock[[t]][[j]]$ec_f
    w <- equilibrium$endogenous[[t]][[j]]$w
    f <- equilibrium$endogenous[[t]][[j]]$f
    s_f <- equilibrium$endogenous[[t]][[j]]$s_f
    method_s_w <- equilibrium$constant$method_s_w
    margin <- equilibrium$constant$margin
    quadrature_size <- equilibrium$constant$quadrature_size
    tol <- equilibrium$constant$tol
    use_rcpp <- equilibrium$constant$use_rcpp
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_tj(
        bound = bound
      ) 
    
    m_0 <- (f - w)/f 
    w_f_list <- 
      compute_initial_points_maximum_margin(
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        lower = bound$lower,
        upper = bound$upper,
        m_bar = m_bar,
        multistart = multistart
      )
    
    aug_foc_list <- 
      list(
        lower = 0
      )
    equilibrium_list <- 
      list(
        lower = 0
      )
    
    
    for (
      i in 1:length(w_f_list) 
    ) {
      equilibrium$endogenous[[t]][[j]]$w <- w_f_list[[i]]$w
      equilibrium$endogenous[[t]][[j]]$f <- w_f_list[[i]]$f
      
      equilibrium_list[[i]] <- 
        solve_equilibrium_maximum_margin_tj(
          t = t,
          j = j,
          equilibrium = equilibrium,
          solver = solver,
          m_bar = m_bar
        )
      
      w <- equilibrium_list[[i]]$endogenous[[t]][[j]]$w
      f <- equilibrium_list[[i]]$endogenous[[t]][[j]]$f
      eta_f <- equilibrium_list[[i]]$endogenous[[t]][[j]]$eta_f
      aug_foc <- 
        compute_foc_maximum_margin_tj(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          beta_f = beta_f,
          gamma_w = gamma_w,
          gamma_f = gamma_f,
          lambda_w = lambda_w,
          lambda_f = lambda_f,
          x_a_w = x_a_w,
          x_a_f = x_a_f,
          x_c_w = x_c_w,
          x_c_f = x_c_f,
          size_w = size_w,
          size_f = size_f,
          owner = owner,
          mu = mu,
          ea_w = ea_w,
          ea_f = ea_f,
          ec_w = ec_w,
          ec_f = ec_f,
          w = w,
          f = f,
          s_f = s_f,
          eta_f = eta_f,
          m_bar = m_bar,
          method_s_w = "exact",
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol,
          use_exp = use_exp
        )  
      aug_foc_list[[i]] <- aug_foc
    }
    
    sum <- 
      sapply(
        aug_foc_list, 
        function(v) 
          sum(abs(v))
      )
    index <- which.min(sum)
    equilibrium <- 
      equilibrium_list[[index]]
    
    return(equilibrium)
  }

check_initial_value_maximum_margin <-
  function(
    w,
    f,
    lower,
    upper,
    m_bar
  ) {
    w_f <-
      check_initial_value(
        w = w,
        f = f,
        lower = lower,
        upper = upper
      )
    w_f$f <-
      ifelse(
        w_f$f > w_f$w / (1 - m_bar + 1e-10),
        w_f$w / (1 - m_bar + 1e-10) - 1e-10,
        w_f$f
      )
    w_f$f[1:2] <- f[1:2]
    return(w_f)
  }

transform_eta_w_f_to_x_maximum_margin <-
  function(
    eta_f,
    w,
    f
  ) {
    x <-
      c(
        eta_f,
        w[3:length(w)],
        (f - w)[3:length(f)]
      )
    return(x)
  }

transform_x_to_eta_w_f_maximum_margin <-
  function(
    x,
    w,
    f
  ) {
    eta_f <-
      x[1:(length(f) - 2)]
    w_x <-
      x[(length(f) - 1):(length(f) - 2 + length(w) - 2)]
    diff_x <-
      x[(length(f) - 1 + length(w) - 2):length(x)]
    f_x <-
      w_x + diff_x
    w_x <-
      c(
        w[1],
        w[2],
        w_x 
      )
    f_x <-
      c(
        f[1],
        f[2],
        f_x
      )
    return(
      list(
        eta_f = eta_f %>% as.matrix(),
        w = w_x %>% as.matrix(),
        f = f_x %>% as.matrix()
      )
    )
  }

compute_foc_maximum_margin_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    # compute unconstrained foc
    foc_tj <- 
      compute_foc_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      ) 
    
    # set constraint
    m <- m_bar * f - f + w
    
    # add lagrange multipliers to foc_w_t and foc_f_t
    foc_tj[
      1:nrow(eta_f)
    ] <- 
      foc_tj[
        1:nrow(eta_f)
      ] +
      eta_f
    
    foc_tj[
      (nrow(eta_f) + 1):length(foc_tj)
    ] <- 
      foc_tj[
        (nrow(eta_f) + 1):length(foc_tj)
      ] +
      eta_f * (m_bar - 1)
    
    # add complementarity slackness condition
    complementarity <-
      eta_f * 
      (
        m[
          3:nrow(m),
          ,
          drop = FALSE
        ]
      )
    
    foc_tj <-
      rbind(
        foc_tj,
        complementarity
      )
    return(foc_tj)
  }

solve_w_f_maximum_margin_nleqslv_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_tj(
        bound = bound
      )
    
    initial_w_f <-
      check_initial_value_maximum_margin(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper,
        m_bar = m_bar
      )
    
    set.seed(seed)
    e <- rnorm(length(bound$lower) + length(eta_f))

    x <-
      transform_eta_w_f_to_x_maximum_margin_nleqslv(
        w = initial_w_f$w,
        f = initial_w_f$f,
        eta_f = eta_f,
        lower = bound$lower,
        upper = bound$upper,
        m_bar = m_bar
      ) + e
    
    fn <-
      function(x) {
        eta_w_f <-
          transform_x_to_eta_w_f_maximum_margin_nleqslv(
            x = x,
            w = initial_w_f$w,
            f = initial_w_f$f,
            lower = bound$lower,
            upper = bound$upper,
            m_bar = m_bar
          ) 
        foc <-
          compute_foc_maximum_margin_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = eta_w_f$w,
            f = eta_w_f$f,
            s_f = s_f,
            eta_f = eta_w_f$eta_f,
            m_bar = m_bar,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(foc)
      }
    
    solution <-
      nleqslv::nleqslv(
        x = x,
        fn = fn,
        control =
          list(
            allowSingular = TRUE
          )
      )
    
    x <- solution$x
    
    eta_w_f <-
      transform_x_to_eta_w_f_maximum_margin_nleqslv(
        x = x,
        w = initial_w_f$w,
        f = initial_w_f$f,
        lower = bound$lower,
        upper = bound$upper,
        m_bar = m_bar
      ) 
    
    return(
      list(
        eta_f = eta_w_f$eta_f,
        w = eta_w_f$w,
        f = eta_w_f$f,
        objective = max(abs(solution$fvec))
      )
    )
  }

transform_w_f_to_x_maximum_margin_bestresponse_itj <-
  function(
    i,
    owner,
    w,
    f,
    eta_f,
    lower,
    upper,
    m_bar
  ) {
    index <- owner[i, ] %>% as.logical()
    index <- index[3:length(index)]
    index <-
      c(
        rep(
          FALSE,
          length(index)
        ),
        index,
        index
      )
    x <-
      transform_eta_w_f_to_x_maximum_margin_nleqslv(
        w = w,
        f = f,
        eta_f = eta_f,
        lower = lower,
        upper = upper,
        m_bar = m_bar
      )
    x <- x[index]
    return(x)
  }

transform_x_to_w_f_maximum_margin_bestresponse_itj <-
  function(
    x,
    i,
    owner,
    w,
    f,
    eta_f,
    lower,
    upper,
    m_bar
  ) {
    index <- owner[i, ] %>% as.logical()
    index <- index[3:length(index)]
    index <-
      c(
        rep(
          FALSE,
          length(index)
        ),
        index,
        index
      )
    xx <-
      transform_eta_w_f_to_x_maximum_margin_nleqslv(
        w = w,
        f = f,
        eta_f = eta_f,
        lower = lower,
        upper = upper,
        m_bar = m_bar
      )
    xx[index] <- x
    eta_w_f <-
      transform_x_to_eta_w_f_maximum_margin_nleqslv(
        x = xx,
        w = w,
        f = f,
        lower = lower,
        upper = upper,
        m_bar = m_bar
      ) 
    return(
      list(
        w = eta_w_f$w,
        f = eta_w_f$f
      )
    )
  }

solve_w_f_maximum_margin_bestresponse_itj <-
  function(
    i,
    lower,
    upper,
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    x <-
      transform_w_f_to_x_maximum_margin_bestresponse_itj(
        i = i,
        owner = owner,
        w = w,
        f = f,
        eta_f = eta_f,
        lower = lower,
        upper = upper,
        m_bar = m_bar
      )
    fn <-
      function(x) {
        w_f <-
          transform_x_to_w_f_maximum_margin_bestresponse_itj(
            x = x,
            i = i,
            owner = owner,
            w = w,
            f = f,
            eta_f = eta_f,
            lower = lower,
            upper = upper,
            m_bar = m_bar
          ) 
        profit_ths <-
          solve_profit_ths_tj(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_f$w,
            f = w_f$f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(profit_ths[i])
      }
    solution_it <-
      optim(
        par = x,
        fn = fn,
        method = "L-BFGS-B",
        control = list(
          fnscale = -1
        )
      )
    w_f <-
      transform_x_to_w_f_maximum_margin_bestresponse_itj(
        x = solution_it$par,
        i = i,
        owner = owner,
        w = w,
        f = f,
        eta_f = eta_f,
        lower = lower,
        upper = upper,
        m_bar = m_bar
      )
    return(
      list(
        w = w_f$w,
        f = w_f$f,
        objective = -solution_it$value %>% as.numeric()
      )
    )
  }

solve_w_f_maximum_margin_bestresponse_tj <-
  function(
    lower,
    upper,
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    old_w <- w
    old_f <- f
    distance <- 100
    while(distance > 1e-10) {
      for (
        i in 2:nrow(owner)
      ) {
        solution_bestresponse_itj <-
          solve_w_f_maximum_margin_bestresponse_itj(
            i = i,
            lower = lower,
            upper = upper,
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = old_w,
            f = old_f,
            s_f = s_f,
            eta_f = eta_f,
            m_bar = m_bar,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        distance_w <-
          max(
            abs(
              solution_bestresponse_itj$w - old_w
            )
          )
        distance_f <-
          max(
            abs(
              solution_bestresponse_itj$f - old_f
            )
          )
        distance <-
          max(
            distance_w,
            distance_f
          )
        print(distance)
        old_w <- solution_bestresponse_itj$w
        old_f <- solution_bestresponse_itj$f
      }
    }
    return(
      list(
        w = solution_bestresponse_itj$w,
        f = solution_bestresponse_itj$f
      )
    )
  }

solve_w_f_maximum_margin_iteration_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    eta_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    bound <-
      compute_monopoly_bound_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        use_exp = use_exp
      )
    
    bound <-
      relax_bound_tj(
        bound = bound
      )
    
    initial_w_f <-
      check_initial_value_maximum_margin(
        w = w,
        f = f,
        lower = bound$lower,
        upper = bound$upper,
        m_bar = m_bar
      )
    
    solution_bestresponse_t <-
      solve_w_f_maximum_margin_bestresponse_tj(
        lower = bound$lower,
        upper = bound$upper,
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        x_c_w = x_c_w,
        x_c_f = x_c_f,
        w_0 = w_0,
        size_w = size_w,
        size_f = size_f,
        owner = owner,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        ec_w = ec_w,
        ec_f = ec_f,
        w = initial_w_f$w,
        f = initial_w_f$f,
        s_f = s_f,
        eta_f = eta_f,
        m_bar = m_bar,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size,
        tol = tol,
        use_exp = use_exp
      ) 
    
    return(
      list(
        w = solution_bestresponse_t$w,
        f = solution_bestresponse_t$f
      )
    )
  }


solve_equilibrium_minimum_wage_tj <-
  function(
    t,
    j,
    eta_w,
    equilibrium,
    solver,
    multistart
  ) {

    eta_w <- 
      rep(
        eta_w,
        length(equilibrium$endogenous[[t]][[j]]$w) - 2
      ) %>% as.matrix()
    if (
      solver == "nleqslv"
    ) {
      w_f <- 
        foreach(
          n = 1:multistart,
          .packages =
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %do% {
          solve_w_f_minimum_wage_nleqslv_tj(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            beta_w = equilibrium$parameter$beta_w,
            beta_f = equilibrium$parameter$beta_f,
            gamma_w = equilibrium$parameter$gamma_w,
            gamma_f = equilibrium$parameter$gamma_f,
            lambda_w = equilibrium$parameter$lambda_w,
            lambda_f = equilibrium$parameter$lambda_f,
            x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
            x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
            x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
            x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
            w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            owner = equilibrium$exogenous[[t]][[j]]$owner,
            mu = equilibrium$shock[[t]][[j]]$mu,
            ea_w = equilibrium$shock[[t]][[j]]$ea_w,
            ea_f = equilibrium$shock[[t]][[j]]$ea_f,
            ec_w = equilibrium$shock[[t]][[j]]$ec_w,
            ec_f = equilibrium$shock[[t]][[j]]$ec_f,
            w = equilibrium$endogenous[[t]][[j]]$w,
            f = equilibrium$endogenous[[t]][[j]]$f,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            eta_w = eta_w,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol,
            use_exp = equilibrium$constant$use_exp,
            seed = n
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
    } else if (
      solver == "iteration"
    ) {
      w_f <-
        solve_w_f_minimum_wage_iteration_tj(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          beta_f = equilibrium$parameter$beta_f,
          gamma_w = equilibrium$parameter$gamma_w,
          gamma_f = equilibrium$parameter$gamma_f,
          lambda_w = equilibrium$parameter$lambda_w,
          lambda_f = equilibrium$parameter$lambda_f,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
          x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
          x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
          w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          owner = equilibrium$exogenous[[t]][[j]]$owner,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          ea_f = equilibrium$shock[[t]][[j]]$ea_f,
          ec_w = equilibrium$shock[[t]][[j]]$ec_w,
          ec_f = equilibrium$shock[[t]][[j]]$ec_f,
          w = equilibrium$endogenous[[t]][[j]]$w,
          f = equilibrium$endogenous[[t]][[j]]$f,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          method_s_w = equilibrium$constant$method_s_w,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol,
          use_exp = equilibrium$constant$use_exp
        )
      
    } else {
      stop("no solver")
    }
    
    # solve client firm shares
    s_f <-
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
        f = w_f$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    # solve worker shares
    if (equilibrium$constant$method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_f$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
    } else if (equilibrium$constant$method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_f$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f
        )
    }
    
    # update endogenous variables
    equilibrium$endogenous[[t]][[j]]$eta_w <- w_f$eta_w
    equilibrium$endogenous[[t]][[j]]$w <- w_f$w
    equilibrium$endogenous[[t]][[j]]$f <- w_f$f
    equilibrium$endogenous[[t]][[j]]$s_f <- s_f
    equilibrium$endogenous[[t]][[j]]$s_w <- s_w
    
    # return equilibrium
    return(equilibrium)
  }

solve_equilibrium_maximum_margin_tj <-
  function(
    t,
    j,
    eta_f,
    equilibrium,
    solver,
    m_bar,
    multistart
  ) {
    eta_f <- 
      rep(
        eta_f,
        length(equilibrium$endogenous[[t]][[j]]$f) - 2
      ) %>% as.matrix()
    if (
      solver == "nleqslv"
    ) {
      w_f <- 
        foreach(
          n = 1:multistart,
          .packages =
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %do% {
          solve_w_f_maximum_margin_nleqslv_tj(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            beta_w = equilibrium$parameter$beta_w,
            beta_f = equilibrium$parameter$beta_f,
            gamma_w = equilibrium$parameter$gamma_w,
            gamma_f = equilibrium$parameter$gamma_f,
            lambda_w = equilibrium$parameter$lambda_w,
            lambda_f = equilibrium$parameter$lambda_f,
            x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
            x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
            x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
            x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
            w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            owner = equilibrium$exogenous[[t]][[j]]$owner,
            mu = equilibrium$shock[[t]][[j]]$mu,
            ea_w = equilibrium$shock[[t]][[j]]$ea_w,
            ea_f = equilibrium$shock[[t]][[j]]$ea_f,
            ec_w = equilibrium$shock[[t]][[j]]$ec_w,
            ec_f = equilibrium$shock[[t]][[j]]$ec_f,
            w = equilibrium$endogenous[[t]][[j]]$w,
            f = equilibrium$endogenous[[t]][[j]]$f,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            eta_f = eta_f,
            m_bar = m_bar,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol,
            use_exp = equilibrium$constant$use_exp,
            seed = n
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
      
    } else if (
      solver == "nloptr"
    ) {
      w_f <- 
        w_f <- 
        foreach(
          n = 1:multistart,
          .packages =
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %do% {
          solve_w_f_maximum_margin_nloptr_tj(
            m_w = equilibrium$parameter$m_w,
            m_f = equilibrium$parameter$m_f,
            beta_w = equilibrium$parameter$beta_w,
            beta_f = equilibrium$parameter$beta_f,
            gamma_w = equilibrium$parameter$gamma_w,
            gamma_f = equilibrium$parameter$gamma_f,
            lambda_w = equilibrium$parameter$lambda_w,
            lambda_f = equilibrium$parameter$lambda_f,
            x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
            x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
            x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
            x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
            w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
            size_w = equilibrium$exogenous[[t]][[j]]$size_w,
            size_f = equilibrium$exogenous[[t]][[j]]$size_f,
            owner = equilibrium$exogenous[[t]][[j]]$owner,
            mu = equilibrium$shock[[t]][[j]]$mu,
            ea_w = equilibrium$shock[[t]][[j]]$ea_w,
            ea_f = equilibrium$shock[[t]][[j]]$ea_f,
            ec_w = equilibrium$shock[[t]][[j]]$ec_w,
            ec_f = equilibrium$shock[[t]][[j]]$ec_f,
            w = equilibrium$endogenous[[t]][[j]]$w,
            f = equilibrium$endogenous[[t]][[j]]$f,
            s_f = equilibrium$endogenous[[t]][[j]]$s_f,
            eta_f = eta_f,
            m_bar = m_bar,
            method_s_w = equilibrium$constant$method_s_w,
            margin = equilibrium$constant$margin,
            quadrature_size = equilibrium$constant$quadrature_size,
            tol = equilibrium$constant$tol,
            use_exp = equilibrium$constant$use_exp,
            seed = n
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
    } else if (
      solver == "iteration"
    ) {
      w_f <- 
        solve_w_f_maximum_margin_iteration_tj(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          beta_f = equilibrium$parameter$beta_f,
          gamma_w = equilibrium$parameter$gamma_w,
          gamma_f = equilibrium$parameter$gamma_f,
          lambda_w = equilibrium$parameter$lambda_w,
          lambda_f = equilibrium$parameter$lambda_f,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
          x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
          x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
          w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          owner = equilibrium$exogenous[[t]][[j]]$owner,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          ea_f = equilibrium$shock[[t]][[j]]$ea_f,
          ec_w = equilibrium$shock[[t]][[j]]$ec_w,
          ec_f = equilibrium$shock[[t]][[j]]$ec_f,
          w = equilibrium$endogenous[[t]][[j]]$w,
          f = equilibrium$endogenous[[t]][[j]]$f,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          eta_f = eta_f,
          m_bar = m_bar,
          method_s_w = equilibrium$constant$method_s_w,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol,
          use_exp = equilibrium$constant$use_exp
        ) 
    }
    
    # solve client firm shares
    s_f <-
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
        f = w_f$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    # solve worker shares
    if (equilibrium$constant$method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_f$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
    } else if (equilibrium$constant$method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_f$w,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f
        )
    }
    
    # update endogenous variables
    equilibrium$endogenous[[t]][[j]]$eta_f <- w_f$eta_f
    equilibrium$endogenous[[t]][[j]]$w <- w_f$w
    equilibrium$endogenous[[t]][[j]]$f <- w_f$f
    equilibrium$endogenous[[t]][[j]]$s_f <- s_f
    equilibrium$endogenous[[t]][[j]]$s_w <- s_w
    
    # return equilibrium
    return(equilibrium)
  }


check_equilibrium_minimum_wage_tj <-
  function(
    equilibrium,
    t,
    j
  ) {
    
    foc_s_f <-
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
    
    aug_foc_w_f <-
      compute_foc_minimum_wage_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        beta_f = equilibrium$parameter$beta_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    foc_w_f <- 
      compute_foc_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        beta_f = equilibrium$parameter$beta_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    aug_foc_w <-
      aug_foc_w_f[
        1:( (length(aug_foc_w_f) - nrow(equilibrium$endogenous[[t]][[j]]$eta_w)) / 2)
      ]
    aug_foc_f <-
      aug_foc_w_f[
        ((length(aug_foc_w_f) - nrow(equilibrium$endogenous[[t]][[j]]$eta_w))/2 + 1):(length(aug_foc_w_f) - nrow(equilibrium$endogenous[[t]][[j]]$eta_w))
      ]

    complementarity <- 
      equilibrium$endogenous[[t]][[j]]$eta_w *(
        equilibrium$endogenous[[t]][[j]]$w[
          3:nrow(equilibrium$endogenous[[t]][[j]]$w)
        ] - 
          equilibrium$exogenous[[t]][[j]]$w_0
      )
    
    foc_w <-
      foc_w_f[
        1:(length(equilibrium$endogenous[[t]][[j]]$w) - 2)
      ]
    foc_f <-
      foc_w_f[(length(equilibrium$endogenous[[t]][[j]]$w) - 1):length(foc_w_f)]
    
    # compute ths profit and surplus
    profit_ths_tj <-
      compute_profit_ths_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_w = equilibrium$endogenous[[t]][[j]]$s_w,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    surplus_w_tj <- 
      compute_surplus_w_tj_exact(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        lambda_w = equilibrium$parameter$lambda_w,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        w = equilibrium$endogenous[[t]][[j]]$w,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    surplus_f_tj <-
      compute_surplus_f_tj(
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
    
    # compute meeting probability
    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      ) 
    meeting_probability_f <-
      compute_meeting_probability_f_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    # compute cost 
    c_w <- 
      compute_c_w_tj(
        gamma_w = equilibrium$parameter$gamma_w,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    c_f <-
      compute_c_f_tj(
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    # summarize the results
    df <-
      data.frame(
        i = factor(0:length(equilibrium$endogenous[[t]][[j]]$w)),
        augmented_foc_w = c(NA, NA, NA, aug_foc_w),
        augmented_foc_f = c(NA, NA, NA, aug_foc_f),
        foc_w = c(NA, NA, NA, foc_w),
        foc_f = c(NA, NA, NA, foc_f),
        complementarity = c(NA, NA, NA, complementarity),
        foc_s_f = c(NA, foc_s_f),
        w = c(NA, equilibrium$endogenous[[t]][[j]]$w),
        f = c(NA, equilibrium$endogenous[[t]][[j]]$f),
        m = 
          (
          c(NA, equilibrium$endogenous[[t]][[j]]$f) - 
          c(NA, equilibrium$endogenous[[t]][[j]]$w)
          )/c(NA, equilibrium$endogenous[[t]][[j]]$f),
        eta_w =
          c(
            NA,
            NA,
            NA,
            equilibrium$endogenous[[t]][[j]]$eta_w
          ),
        s_w = 
          c(
            1 - sum(equilibrium$endogenous[[t]][[j]]$s_w * meeting_probability_w), 
            equilibrium$endogenous[[t]][[j]]$s_w
          ),
        s_f = 
          c(
            1 - sum(equilibrium$endogenous[[t]][[j]]$s_f), 
            equilibrium$endogenous[[t]][[j]]$s_f
          ),  
        meeting_probability_w = c(NA, meeting_probability_w),
        meeting_probability_f = c(NA, meeting_probability_f),
        c_w = c(NA, c_w),
        c_f = c(NA, c_f),
        profit_ths = c(NA, profit_ths_tj),
        surplus_w = surplus_w_tj,
        surplus_f = surplus_f_tj
      )
    
    return(df)
  }

check_equilibrium_maximum_margin_tj <-
  function(
    equilibrium,
    t,
    j,
    m_bar
  ) {
    
    foc_s_f <-
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
    
    aug_foc_w_f <-
      compute_foc_maximum_margin_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        beta_f = equilibrium$parameter$beta_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
        m_bar = m_bar,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp
      )
    foc_w_f <- 
      compute_foc_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        beta_f = equilibrium$parameter$beta_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    aug_foc_w <-
      aug_foc_w_f[
        1:( (length(aug_foc_w_f) - nrow(equilibrium$endogenous[[t]][[j]]$eta_f)) / 2)
      ]
    aug_foc_f <-
      aug_foc_w_f[
        ((length(aug_foc_w_f) - nrow(equilibrium$endogenous[[t]][[j]]$eta_f))/2 + 1):(length(aug_foc_w_f) - nrow(equilibrium$endogenous[[t]][[j]]$eta_f))
      ]
    
    m <- 
      m_bar * equilibrium$endogenous[[t]][[j]]$f - 
      equilibrium$endogenous[[t]][[j]]$f + 
      equilibrium$endogenous[[t]][[j]]$w
    
    complementarity <-
      equilibrium$endogenous[[t]][[j]]$eta_f*
      (
        m[
          3:nrow(m),
          ,
          drop = FALSE
        ]
      )
    
    foc_w <-
      foc_w_f[
        1:(length(equilibrium$endogenous[[t]][[j]]$w) - 2)
      ]
    foc_f <-
      foc_w_f[(length(equilibrium$endogenous[[t]][[j]]$w) - 1) :length(foc_w_f)]
    
    # compute ths profit and surplus
    profit_ths_tj <-
      compute_profit_ths_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        gamma_w = equilibrium$parameter$gamma_w,
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_w = equilibrium$endogenous[[t]][[j]]$s_w,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    surplus_w_tj <- 
      compute_surplus_w_tj_exact(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        lambda_w = equilibrium$parameter$lambda_w,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        w = equilibrium$endogenous[[t]][[j]]$w,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    surplus_f_tj <-
      compute_surplus_f_tj(
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
    
    # compute meeting probability
    meeting_probability_w <-
      compute_meeting_probability_w_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      ) 
    meeting_probability_f <-
      compute_meeting_probability_f_tj(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        mu = equilibrium$shock[[t]][[j]]$mu,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )
    
    # compute cost 
    c_w <- 
      compute_c_w_tj(
        gamma_w = equilibrium$parameter$gamma_w,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    c_f <-
      compute_c_f_tj(
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        use_exp = equilibrium$constant$use_exp
      ) 
    
    
    # check maximum margin condition
    up_m_bar <- 
      (
        equilibrium$endogenous[[t]][[j]]$f - equilibrium$endogenous[[t]][[j]]$w
      )/equilibrium$endogenous[[t]][[j]]$f - m_bar 
    
    # summarize the results
    df <-
      data.frame(
        i = factor(0:length(equilibrium$endogenous[[t]][[j]]$w)),
        augmented_foc_w = c(NA, NA, NA, aug_foc_w),
        augmented_foc_f = c(NA, NA, NA, aug_foc_f),
        foc_w = c(NA, NA, NA, foc_w),
        foc_f = c(NA, NA, NA, foc_f),
        complementarity = c(NA, NA, NA, complementarity),
        up_m_bar = c(NA, up_m_bar),
        foc_s_f = c(NA, foc_s_f),
        w = c(NA, equilibrium$endogenous[[t]][[j]]$w),
        f = c(NA, equilibrium$endogenous[[t]][[j]]$f),
        m = 
          c(
            NA,
            (
              equilibrium$endogenous[[t]][[j]]$f - 
                equilibrium$endogenous[[t]][[j]]$w
            ) / equilibrium$endogenous[[t]][[j]]$f
          ),
        eta_f =
          c(
            NA,
            NA,
            NA,
            equilibrium$endogenous[[t]][[j]]$eta_f
          ),
        s_w = 
          c(
            1 - sum(equilibrium$endogenous[[t]][[j]]$s_w * meeting_probability_w), 
            equilibrium$endogenous[[t]][[j]]$s_w
          ),
        s_f = 
          c(
            1 - sum(equilibrium$endogenous[[t]][[j]]$s_f), 
            equilibrium$endogenous[[t]][[j]]$s_f
          ),  
        meeting_probability_w = c(NA, meeting_probability_w),
        meeting_probability_f = c(NA, meeting_probability_f),
        c_w = c(NA, c_w),
        c_f = c(NA, c_f),
        profit_ths = c(NA, profit_ths_tj),
        surplus_w = surplus_w_tj,
        surplus_f = surplus_f_tj
      )
    
    
    return(df)
  }

check_foc_w_shape_minimum_wage_tj <-
  function(
    equilibrium,
    t,
    j,
    w_0,
    i
  ) {
    g <-
      seq(
        0,
        40,
        by = 1
      ) %>%
      purrr::map(
        function(x) {
          w_x <- equilibrium$endogenous[[t]][[j]]$w
          w_x[i] <- x
          foc <-
            compute_foc_minimum_wage_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              beta_f = equilibrium$parameter$beta_f,  
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              lambda_w = equilibrium$parameter$lambda_w,
              lambda_f = equilibrium$parameter$lambda_f,
              x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
              x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              w_0 = w_0,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ea_w = equilibrium$shock[[t]][[j]]$ea_w,
              ea_f = equilibrium$shock[[t]][[j]]$ea_f,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = w_x,
              f = equilibrium$endogenous[[t]][[j]]$f,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
              method_s_w = equilibrium$constant$method_s_w,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol,
              use_exp = equilibrium$constant$use_exp
            ) 
          return(
            tibble::tibble(
              w = x,
              aug_foc_w = foc[i - 1]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    
    g <-
      g %>%
      ggplot(
        aes(
          x = w,
          y = aug_foc_w
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$w[i],
        linetype = "dashed"
      ) +
      theme_classic()
    
    return(g)
  }

check_foc_f_shape_minimum_wage_tj <-
  function(
    equilibrium,
    t,
    j,
    w_0,
    i
  ) {
    g <-
      seq(
        -10,
        max(equilibrium$endogenous[[t]][[j]]$f) + 40,
        by = 1
      ) %>%
      purrr::map(
        function(x) {
          f_x <- equilibrium$endogenous[[t]][[j]]$f
          f_x[i] <- x
          foc <-
            compute_foc_minimum_wage_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              beta_f = equilibrium$parameter$beta_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              lambda_w = equilibrium$parameter$lambda_w,
              lambda_f = equilibrium$parameter$lambda_f,
              x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
              x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              w_0 = w_0,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ea_w = equilibrium$shock[[t]][[j]]$ea_w,
              ea_f = equilibrium$shock[[t]][[j]]$ea_f,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = equilibrium$endogenous[[t]][[j]]$w,
              f = f_x,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              eta_w = equilibrium$endogenous[[t]][[j]]$eta_w,
              method_s_w = equilibrium$constant$method_s_w,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol,
              use_exp = equilibrium$constant$use_exp
            ) 
          return(
            tibble::tibble(
              f = x,
              aug_foc_f = 
                foc[
                  (length(equilibrium$endogenous[[t]][[j]]$f) - 2) +
                    i - 2
                ]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    
    g <-
      g %>%
      ggplot(
        aes(
          x = f,
          y = aug_foc_f
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$f[i],
        linetype = "dashed"
      ) +
      theme_classic()
    
    return(g)
  }

check_profit_w_shape_minimum_wage_tj <-
  function(
    equilibrium,
    t,
    j,
    w_0,
    i
  ) {
    g <-
      seq(
        0,
        20,
        by = 0.1
      ) %>%
      purrr::map(
        function(x) {
          w_x <- equilibrium$endogenous[[t]][[j]]$w
          w_x[i] <- x
          profit_ths_t <-
            solve_profit_ths_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              beta_f = equilibrium$parameter$beta_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              lambda_w = equilibrium$parameter$lambda_w,
              lambda_f = equilibrium$parameter$lambda_f,
              x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
              x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ea_w = equilibrium$shock[[t]][[j]]$ea_w,
              ea_f = equilibrium$shock[[t]][[j]]$ea_f,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = w_x,
              f = equilibrium$endogenous[[t]][[j]]$f,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              method_s_w = equilibrium$constant$method_s_w,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol,
              use_exp = equilibrium$constant$use_exp
            )
          return(
            tibble::tibble(
              x = x,
              profit = profit_ths_t[i]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    
    g <-
      g %>%
      ggplot(
        aes(
          x = x,
          y = profit
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$w[i],
        linetype = "dashed"
      ) +
      theme_classic()
    
    return(g)
  }

check_profit_f_shape_minimum_wage_tj <-
  function(
    equilibrium,
    t,
    j,
    w_0,
    i
  ) {
    g <-
      seq(
        0,
        40,
        by = 0.1
      ) %>%
      purrr::map(
        function(x) {
          f_x <- equilibrium$endogenous[[t]][[j]]$f
          f_x[i] <- x
          profit_ths_t <-
            solve_profit_ths_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              beta_f = equilibrium$parameter$beta_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              lambda_w = equilibrium$parameter$lambda_w,
              lambda_f = equilibrium$parameter$lambda_f,
              x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
              x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ea_w = equilibrium$shock[[t]][[j]]$ea_w,
              ea_f = equilibrium$shock[[t]][[j]]$ea_f,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = equilibrium$endogenous[[t]][[j]]$w,
              f = f_x,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              method_s_w = equilibrium$constant$method_s_w,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol,
              use_exp= equilibrium$constant$use_exp
            )
          return(
            tibble::tibble(
              x = x,
              profit = profit_ths_t[i]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    
    g <-
      g %>%
      ggplot(
        aes(
          x = x,
          y = profit
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$f[i],
        linetype = "dashed"
      ) +
      theme_classic()
    
    return(g)
  }

check_foc_w_shape_maximum_margin_tj <-
  function(
    equilibrium,
    t,
    j,
    m_bar,
    i
  ) {
    g <-
      seq(
        -10,
        20,
        by = 0.1
      ) %>%
      purrr::map(
        function(x) {
          w_x <- equilibrium$endogenous[[t]][[j]]$w
          w_x[i] <- x
          foc <-
            compute_foc_maximum_margin_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              beta_f = equilibrium$parameter$beta_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              lambda_w = equilibrium$parameter$lambda_w,
              lambda_f = equilibrium$parameter$lambda_f,
              x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
              x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ea_w = equilibrium$shock[[t]][[j]]$ea_w,
              ea_f = equilibrium$shock[[t]][[j]]$ea_f,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = w_x,
              f = equilibrium$endogenous[[t]][[j]]$f,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
              m_bar = m_bar,
              method_s_w = equilibrium$constant$method_s_w,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol,
              use_exp = equilibrium$constant$use_exp
            ) 
          return(
            tibble::tibble(
              w = x,
              aug_foc_w = foc[i - 2]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    
    g <-
      g %>%
      ggplot(
        aes(
          x = w,
          y = aug_foc_w
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$w[i],
        linetype = "dashed"
      ) +
      theme_classic()
    return(g)
  }

check_foc_f_shape_maximum_margin_tj <-
  function(
    equilibrium,
    t,
    j,
    m_bar,
    i
  ) {
    g <-
      seq(
        -10,
        15,
        by = 0.1
      ) %>%
      purrr::map(
        function(x) {
          f_x <- equilibrium$endogenous[[t]][[j]]$f
          f_x[i] <- x
          foc <-
            compute_foc_maximum_margin_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              beta_w = equilibrium$parameter$beta_w,
              beta_f = equilibrium$parameter$beta_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              lambda_w = equilibrium$parameter$lambda_w,
              lambda_f = equilibrium$parameter$lambda_f,
              x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
              x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ea_w = equilibrium$shock[[t]][[j]]$ea_w,
              ea_f = equilibrium$shock[[t]][[j]]$ea_f,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = equilibrium$endogenous[[t]][[j]]$w,
              f = f_x,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              eta_f = equilibrium$endogenous[[t]][[j]]$eta_f,
              m_bar = m_bar,
              method_s_w = equilibrium$constant$method_s_w,
              margin = equilibrium$constant$margin,
              quadrature_size = equilibrium$constant$quadrature_size,
              tol = equilibrium$constant$tol,
              use_exp = equilibrium$constant$use_exp
            ) 
          return(
            tibble::tibble(
              f = x,
              aug_foc_f = foc[
                length(equilibrium$endogenous[[t]][[j]]$w) - 2 +
                  i - 2
              ]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    g <-
      g %>%
      ggplot(
        aes(
          x = f,
          y = aug_foc_f
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$f[i],
        linetype = "dashed"
      ) +
      theme_classic()
    return(g)
  }

check_profit_w_shape_maximum_margin <-
  function(
    equilibrium,
    t,
    j,
    m_bar,
    i
  ) {
    g <-
      seq(
        -10,
        20,
        by = 0.1
      ) %>%
      purrr::map(
        function(x) {
          w_x <- equilibrium$endogenous[[t]][[j]]$w
          w_x[i] <- x
          profit_ths <-
            compute_profit_ths_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = w_x,
              f = equilibrium$endogenous[[t]][[j]]$f,
              s_w = equilibrium$endogenous[[t]][[j]]$s_w,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              use_exp = equilibrium$constant$use_exp
            )
          return(
            tibble::tibble(
              x = x,
              profit = profit_ths[i]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    
    g <-
      g %>%
      ggplot(
        aes(
          x = x,
          y = profit
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$w[i],
        linetype = "dashed"
      ) +
      theme_classic()
    return(g)
  }

check_profit_f_shape_maximum_margin <-
  function(
    equilibrium,
    t,
    j,
    m_bar,
    i
  ) {
    g <-
      seq(
        -10,
        15,
        by = 0.1
      ) %>%
      purrr::map(
        function(x) {
          f_x <- equilibrium$endogenous[[t]][[j]]$f
          f_x[i] <- x
          profit_ths <-
            compute_profit_ths_tj(
              m_w = equilibrium$parameter$m_w,
              m_f = equilibrium$parameter$m_f,
              gamma_w = equilibrium$parameter$gamma_w,
              gamma_f = equilibrium$parameter$gamma_f,
              x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
              x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
              size_w = equilibrium$exogenous[[t]][[j]]$size_w,
              size_f = equilibrium$exogenous[[t]][[j]]$size_f,
              owner = equilibrium$exogenous[[t]][[j]]$owner,
              mu = equilibrium$shock[[t]][[j]]$mu,
              ec_w = equilibrium$shock[[t]][[j]]$ec_w,
              ec_f = equilibrium$shock[[t]][[j]]$ec_f,
              w = equilibrium$endogenous[[t]][[j]]$w,
              f = f_x,
              s_w = equilibrium$endogenous[[t]][[j]]$s_w,
              s_f = equilibrium$endogenous[[t]][[j]]$s_f,
              use_exp = equilibrium$constant$use_exp
            )
          return(
            tibble::tibble(
              x = x,
              profit = profit_ths[i]
            )
          )
        }
      ) %>%
      dplyr::bind_rows()
    g <-
      g %>%
      ggplot(
        aes(
          x = x,
          y = profit
        )
      ) +
      geom_line() +
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = equilibrium$endogenous[[t]][[j]]$f[i],
        linetype = "dashed"
      ) +
      theme_classic()
    return(g)
  }


evaluate_equilibrium_minimum_wage_tj <-
  function (
    w_0,
    t,
    j,
    solver,
    multistart,
    equilibrium
  ) {
    equilibrium$exogenous[[t]][[j]]$w_0 <- w_0
    # update w_0 into fringe and private market

    if (equilibrium$endogenous[[t]][[j]]$w[1] < w_0){
      equilibrium$endogenous[[t]][[j]]$w[1] <- w_0
      equilibrium$endogenous[[t]][[j]]$f[1] <- w_0
    }
    # compute margin of fringe market
    m <- (equilibrium$endogenous[[t]][[j]]$f[2] - equilibrium$endogenous[[t]][[j]]$w[2])/equilibrium$endogenous[[t]][[j]]$f[2]
    
    if (equilibrium$endogenous[[t]][[j]]$w[2] < w_0){
      equilibrium$endogenous[[t]][[j]]$w[2] <- w_0
      equilibrium$endogenous[[t]][[j]]$f[2] <- w_0 / (1 - m)
    }
    
    equilibrium <-
      solve_equilibrium_minimum_wage_tj(
        t = t,
        j = j,
        eta_w = 1e-16,
        equilibrium = equilibrium,
        solver = solver,
        multistart = multistart
      )
    
    df <- 
      check_equilibrium_minimum_wage_tj(
        equilibrium = equilibrium,
        t = t,
        j = j
      ) 
    
    df <-
      df %>%
      dplyr::mutate(
        minimum_wage = rep(w_0, nrow(df))
      ) %>%
      dplyr::select(
        minimum_wage,
        dplyr::everything()
      )
    
    return(df)
  }

evaluate_equilibrium_maximum_margin_tj <-
  function (
    m_bar,
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    # private margin is always 0, no update
    # update fringe ths 
    # compute margin of fringe market
    m <- (equilibrium$endogenous[[t]][[j]]$f[2] - equilibrium$endogenous[[t]][[j]]$w[2])/equilibrium$endogenous[[t]][[j]]$f[2]
    
    if (m > m_bar){
      equilibrium$endogenous[[t]][[j]]$f[2] <-  equilibrium$endogenous[[t]][[j]]$w[2]/ (1 - m_bar)
    }
    equilibrium <-
      solve_equilibrium_maximum_margin_tj(
        t = t,
        j = j,
        eta_f = 1e-16,
        equilibrium = equilibrium, 
        solver = solver,
        m_bar = m_bar,
        multistart = multistart
      )
    
    df <- 
      check_equilibrium_maximum_margin_tj(
        equilibrium = equilibrium,
        t = t,
        j = j,
        m_bar = m_bar
      ) 
    
    df <-
      df %>%
      dplyr::mutate(
        maximum_markup = rep(m_bar, nrow(df))
      ) %>%
      dplyr::select(
        maximum_markup,
        dplyr::everything()
      )
    
    return(df)
  }

evaluate_counterfactual_minimum_wage_tj <-
  function(
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    upper <- 
      max(
        unlist(
          lapply(
            equilibrium$endogenous,
            lapply, 
            "[[", "w")
          )
        )
    
    start <- 0
    end <- 
      min(
        1.1 * upper, 
        na.rm = TRUE
      )
    increment <- (end - start) / 15
    comparative <-
      round(
        seq(
          start,
          end,
          by = increment
        ),
        3
      ) %>%
      purrr::map(
        ~ evaluate_equilibrium_minimum_wage_tj(
          .,
          t = t,
          j = j,
          equilibrium = equilibrium,
          multistart = multistart,
          solver = solver
        ) 
      ) %>%
      dplyr::bind_rows()
    return(comparative)
  }

evaluate_counterfactual_minimum_wage <-
  function(
    equilibrium,
    solver,
    multistart
  ) {
    # Get the maximum wage for all markets
    upper <- 
      max(
        unlist(
          lapply(
            equilibrium$endogenous,
            lapply, 
            "[[", "w")
        )
      )
    
    start <- 0
    end <- 
      min(
        1.1 * upper, 
        na.rm = TRUE
      )
    increment <- (end - start) / 15
    
    # Iterate over all markets (t) and zones (j)
    comparative_all <-
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages = 
          c(
            "Dispatching",
             "foreach", 
             "magrittr", 
             "dplyr", 
             "purrr"
             )
      ) %do% {
        comparative_t <-
          foreach(
            j = seq_along(equilibrium$exogenous[[t]]),
            .combine = "rbind"
          ) %do% {
            # Evaluate counterfactual for this specific market-zone
            comparative_tj <-
              round(
                seq(
                  start,
                  end,
                  by = increment
                ),
                3
              ) %>%
              purrr::map(
                ~ evaluate_equilibrium_minimum_wage_tj(
                  .,
                  t = t,
                  j = j,
                  equilibrium = equilibrium,
                  multistart = multistart,
                  solver = solver
                ) 
              ) %>%
              dplyr::bind_rows()
            
            # Add market and zone identifiers
            if (nrow(comparative_tj) > 0) {
              comparative_tj$t <- t
              comparative_tj$j <- j
            }
            
            return(comparative_tj)
          }
        return(comparative_t)
      } %>%
      dplyr::bind_rows()
    
    return(comparative_all)
  }

evaluate_counterfactual_maximum_margin_tj <-
  function(
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    start <- 0.9
    end <- 0.05
    decrease <- -(start - end) / 15
    comparative <-
      round(
        seq(
          start,
          end,
          by = decrease
        ),
        3
      ) %>%
      purrr::map(
        ~ evaluate_equilibrium_maximum_margin_tj(
          m_bar = .,
          t = t,
          j = j,
          equilibrium = equilibrium,
          solver = solver,
          multistart = multistart
        ) 
      ) %>%
      dplyr::bind_rows()
    return(comparative)
  }

evaluate_counterfactual_maximum_margin <-
  function(
    equilibrium,
    solver,
    multistart
  ) {
    # Set the range for maximum margin values
    start <- 0.9
    end <- 0.05
    decrease <- -(start - end) / 15
    
    # Iterate over all markets (t) and zones (j)
    comparative_all <-
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages = c(
          "Dispatching",
          "foreach", 
          "magrittr", 
          "dplyr", 
          "purrr"
        )
      ) %do% {
        comparative_t <-
          foreach(
            j = seq_along(equilibrium$exogenous[[t]]),
            .combine = "rbind"
          ) %do% {
            # Evaluate counterfactual for this specific market-zone
            comparative <-
              round(
                seq(
                  start,
                  end,
                  by = decrease
                ),
                3
              ) %>%
              purrr::map(
                ~ evaluate_equilibrium_maximum_margin_tj(
                  m_bar = .,
                  t = t,
                  j = j,
                  equilibrium = equilibrium,
                  solver = solver,
                  multistart = multistart
                ) 
              ) %>%
              dplyr::bind_rows()
            
            # Add market and zone identifiers
            if (nrow(comparative_tj) > 0) {
              comparative_tj$t <- t
              comparative_tj$j <- j
            }
            
            return(comparative_tj)
          }
        return(comparative_t)
      } %>%
      dplyr::bind_rows()
    
    return(comparative_all)
  }

make_minimum_wage_shade_df <-
  function(
    counterfactual,
    tol_bind = 1e-3,
    shade_alpha = c(0.06, 0.10, 0.14)
  ) {
    if (!identical(length(shade_alpha), 3L)) {
      stop("shade_alpha must be a numeric vector of length 3")
    }

    minwage_breaks <-
      counterfactual %>%
      dplyr::mutate(
        bind = abs(w - minimum_wage) < tol_bind
      ) %>%
      dplyr::summarise(
        minw_pt     = min(minimum_wage[real_index == 1 & bind], na.rm = TRUE),
        minw_fringe = min(minimum_wage[real_index == 2 & bind], na.rm = TRUE),
        minw_top5   = min(minimum_wage[real_index >= 3 & bind], na.rm = TRUE),
        minw_max    = max(minimum_wage, na.rm = TRUE)
      )
    
    if (!is.finite(minwage_breaks$minw_pt)) {
      stop("Part-time market binding threshold not found (real_index == 1)")
    }
    if (!is.finite(minwage_breaks$minw_fringe)) {
      stop("Fringe THS binding threshold not found (real_index == 2)")
    }
    if (!is.finite(minwage_breaks$minw_top5)) {
      stop("Top-5 THS binding threshold not found (real_index >= 3)")
    }
    if (minwage_breaks$minw_pt > minwage_breaks$minw_fringe) {
      stop("Binding order violated: part-time binds after fringe THS")
    }
    if (minwage_breaks$minw_fringe > minwage_breaks$minw_top5) {
      stop("Binding order violated: fringe THS binds after top-5 THS")
    }
    
    shade_df <-
      data.frame(
        xmin  = c(minwage_breaks$minw_pt, minwage_breaks$minw_fringe, minwage_breaks$minw_top5),
        xmax  = c(minwage_breaks$minw_fringe, minwage_breaks$minw_top5, minwage_breaks$minw_max),
        alpha = shade_alpha
      )
    
    return(shade_df)
  }

make_maximum_margin_shade_df <-
  function(
    counterfactual,
    tol_bind = 0.02,
    shade_alpha = 0.10
  ) {
    if (!identical(length(shade_alpha), 1L)) {
      stop("shade_alpha must be a numeric scalar")
    }
    
    bind_start <-
      counterfactual %>%
      dplyr::filter(
        as.numeric(real_index) >= 3
      ) %>%
      dplyr::mutate(
        slack = maximum_markup - m,
        bind = slack <= tol_bind
      ) %>%
      dplyr::summarise(
        maximum_markup_bind_start = max(maximum_markup[bind], na.rm = TRUE),
        maximum_markup_min = min(maximum_markup, na.rm = TRUE)
      )
    
    if (!is.finite(bind_start$maximum_markup_bind_start)) {
      stop("Maximum margin binding threshold not found (real_index >= 3)")
    }
    if (!is.finite(bind_start$maximum_markup_min)) {
      stop("maximum_markup has no finite values")
    }
    
    shade_df <-
      data.frame(
        xmin = bind_start$maximum_markup_min,
        xmax = bind_start$maximum_markup_bind_start,
        alpha = shade_alpha
      )
    
    return(shade_df)
  }

save_legend_pdf <-
  function(
    plot,
    file_name,
    width = 6.5,
    height = 0.7
  ) {
    plot_grob <-
      ggplot2::ggplotGrob(plot)
    legend_grob <-
      gtable::gtable_filter(
        plot_grob,
        "guide-box"
      )
    
    grDevices::pdf(
      file = file_name,
      width = width,
      height = height,
      onefile = FALSE
    )
    grid::grid.newpage()
    grid::grid.draw(legend_grob)
    grDevices::dev.off()
    
    return(invisible(NULL))
  }

plot_counterfactual_minimum_wage_outside <-
  function(
    counterfactual,
    tol_bind = 1e-3,
    shade_alpha = c(0.06, 0.10, 0.14),
    shade_fill = "grey30"
  ) {
    shade_df <-
      make_minimum_wage_shade_df(
        counterfactual = counterfactual,
        tol_bind = tol_bind,
        shade_alpha = shade_alpha
      )

    counterfactual <- 
      counterfactual %>%
      dplyr::filter(
        as.numeric(real_index) < 3
      ) %>%
      dplyr::mutate(
        group_label = dplyr::case_when(
          as.numeric(real_index) == 0 ~ "Non-labor",
          as.numeric(real_index) == 1 ~ "Part-time job market",
          as.numeric(real_index) == 2 ~ "Fringe THS"
        ),
        group_label = factor(
          group_label,
          levels = c(
            "Fringe THS",
            "Part-time job market",
            "Non-labor"
          )
        )
      )
    
    variables <-
      counterfactual %>%
      dplyr::select(
        -minimum_wage,
        -i
      ) %>%
      colnames() 

    # Set output directory for paper figures (outside market, minimum wage)
    output_dir <-
      here::here(
        "draft",
        "figuretable",
        "counterfactual",
        "multihome",
        "minimum_wage"
      )
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    legend_source_plot <- NULL
    
    p <-
      foreach (
        i = seq_along(variables)
      ) %do% {
        var_name <- variables[i]
        plot_df <- counterfactual
        y_label <- "Unit: level"
        if (var_name %in% c("s_w", "s_f", "m")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]] * 100
            )
          y_label <- "%"
        } else if (var_name %in% c("w", "f", "surplus_w", "surplus_f", "profit_ths")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
          y_label <- "10k yen/8h"
        } else {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
        }
        p <-
          ggplot(
            plot_df,
            aes(
              x = minimum_wage,
              y        = y_value,
              color    = group_label,
              linetype = group_label,
              shape    = group_label
            )
          ) +
          geom_rect(
            data = shade_df,
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = -Inf,
              ymax = Inf,
              alpha = alpha
            ),
            inherit.aes = FALSE,
            fill = shade_fill
          ) +
          scale_alpha_identity(
            guide = "none"
          ) +
          geom_line() +
          geom_hline(
            yintercept = 0,
            linetype = "dotted"
          ) +
          labs(
            x = NULL,
            y = y_label
          ) +
          theme_classic(
            base_size = 12
          ) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position  = "none",
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            axis.text        = element_text(size = 11),
            plot.margin      = margin(t = 12, r = 6, b = 6, l = 6)
          ) +
          coord_cartesian(clip = "off") +
          annotate(
            "text",
            x = -Inf,
            y = Inf,
            label = y_label,
            hjust = 0,
            vjust = -0.25,
            size = 3
          ) +
          scale_color_viridis_d(
            name = "Market / THS",
            option = "viridis",
            end = 0.85
          ) +
          scale_linetype_manual(
            name   = "Market / THS",
            values = c(
              "solid",
              "dashed",
              "dotted",
              "dotdash",
              "longdash",
              "twodash"
            )
          ) +
          scale_shape_manual(
            name   = "Market / THS",
            values = c(
              16, # solid circle
              17, # solid triangle
              15, # solid square
              1,  # open circle
              2,  # open triangle
              0   # open square
            )
          )
        
        if (identical(variables[i], "profit_ths")) {
          legend_source_plot <-
            p +
            theme(
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title = element_text(size = 11),
              legend.text  = element_text(size = 11),
              legend.key.height = grid::unit(0.30, "cm"),
              legend.key.width  = grid::unit(0.60, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank()
            )
        } else {
          p <- p
        }
        
        for(
          j in unique(plot_df$group_label)
        ){
          p <- 
            p + 
            geom_line(
              data = subset(plot_df, group_label == j),
              aes(
                x = minimum_wage,
                  y        = y_value,
                  color    = group_label,
                  linetype = group_label,
                  shape    = group_label
              )
            ) +
            geom_point(
              size  = 2,
              alpha = 0.8
            )
        } 

        # Save BW friendly PDF for LaTeX
        file_name <-
          file.path(
            output_dir,
            paste0(
              "minimum_wage_outside_",
              variables[i],
              ".pdf"
            )
          )
        ggplot2::ggsave(
          filename = file_name,
          plot     = p,
          width    = 3.6,
          height   = 2.6,
          device   = "pdf"
        )

        return(p)
      }
    p <-
      p %>%
      magrittr::set_names(variables)
    
    p <- p[names(p) %in% "segment" == FALSE] 
    
    if (!is.null(legend_source_plot)) {
      file_name_legend <-
        file.path(
          output_dir,
          "minimum_wage_outside_legend.pdf"
        )
      save_legend_pdf(
        plot = legend_source_plot,
        file_name = file_name_legend,
        width = 6.5,
        height = 0.8
      )
    }
    
    for (i in seq_along(p)) {
      cat(
        "#### ",
        names(p)[i],
        "\n\n"
      )
      print(p[[i]])
      cat("\n\n")
    }
    
    return(p)
  }

plot_counterfactual_minimum_wage_inside <-
  function(
    counterfactual,
    tol_bind = 1e-3,
    shade_alpha = c(0.06, 0.10, 0.14),
    shade_fill = "grey30"
  ) {
    shade_df <-
      make_minimum_wage_shade_df(
        counterfactual = counterfactual,
        tol_bind = tol_bind,
        shade_alpha = shade_alpha
      )

    counterfactual <- 
      counterfactual %>%
      dplyr::filter(as.numeric(real_index) >= 3) %>%
      dplyr::mutate(
        i_plot = as.integer(real_index) - 2L
      )
    
    variables <-
      counterfactual %>%
      dplyr::select(
        -minimum_wage,
        -i
      ) %>%
      colnames() 

    # Set output directory for paper figures (inside market, minimum wage)
    output_dir <-
      here::here(
        "draft",
        "figuretable",
        "counterfactual",
        "multihome",
        "minimum_wage"
      )
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    legend_source_plot <- NULL
    
    p <-
      foreach (
        i = seq_along(variables)
      ) %do% {
        var_name <- variables[i]
        plot_df <- counterfactual
        y_label <- "Unit: level"
        if (var_name %in% c("s_w", "s_f", "m")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]] * 100
            )
          y_label <- "%"
        } else if (var_name %in% c("w", "f", "surplus_w", "surplus_f", "profit_ths")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
          y_label <- "10k yen/8h"
        } else {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
        }
        p <-
          ggplot(
            plot_df,
            aes(
              x = minimum_wage,
              y = y_value,
              color    = factor(i_plot),
              linetype = factor(i_plot),
              shape    = factor(i_plot)
            )
          ) +
          geom_rect(
            data = shade_df,
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = -Inf,
              ymax = Inf,
              alpha = alpha
            ),
            inherit.aes = FALSE,
            fill = shade_fill
          ) +
          scale_alpha_identity(
            guide = "none"
          ) +
          geom_line() +
          geom_hline(
            yintercept = 0,
            linetype = "dotted"
          ) +
          labs(
            x = NULL,
            y = y_label
          ) +
          theme_classic(
            base_size = 12
          ) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position  = "none",
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            axis.text        = element_text(size = 11),
            plot.margin      = margin(t = 12, r = 6, b = 6, l = 6)
          ) +
          coord_cartesian(clip = "off") +
          annotate(
            "text",
            x = -Inf,
            y = Inf,
            label = y_label,
            hjust = 0,
            vjust = -0.25,
            size = 3
          ) +
          scale_color_viridis_d(
            name = "THS",
            option = "viridis",
            end = 0.85
          ) +
          scale_linetype_manual(
            name   = "THS",
            values = c(
              "solid",
              "dashed",
              "dotted",
              "dotdash",
              "longdash",
              "twodash"
            )
          ) +
          scale_shape_manual(
            name   = "THS",
            values = c(
              16,
              17,
              15,
              1,
              2,
              0
            )
          )
        
        if (identical(variables[i], "profit_ths")) {
          legend_source_plot <-
            p +
            theme(
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title = element_text(size = 11),
              legend.text  = element_text(size = 11),
              legend.key.height = grid::unit(0.30, "cm"),
              legend.key.width  = grid::unit(0.60, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank()
            )
        } else {
          p <- p
        }
        
        for(
          j in unique(plot_df$i_plot)
        ){
          p <- 
            p + 
            geom_line(
              data = subset(plot_df, i_plot == j),
              aes(
                x = minimum_wage,
                y = y_value,
                color    = factor(i_plot),
                linetype = factor(i_plot),
                shape    = factor(i_plot)
              )
            ) +
            geom_point(
              size  = 2,
              alpha = 0.8
            )
        } 

        # Save BW friendly PDF for LaTeX
        file_name <-
          file.path(
            output_dir,
            paste0(
              "minimum_wage_inside_",
              variables[i],
              ".pdf"
            )
          )
        ggplot2::ggsave(
          filename = file_name,
          plot     = p,
          width    = 3.6,
          height   = 2.6,
          device   = "pdf"
        )

        return(p)
      }
    p <-
      p %>%
      magrittr::set_names(variables)
    p <- p[names(p) %in% "segment" == FALSE] 
    
    if (!is.null(legend_source_plot)) {
      file_name_legend <-
        file.path(
          output_dir,
          "minimum_wage_inside_legend.pdf"
        )
      save_legend_pdf(
        plot = legend_source_plot,
        file_name = file_name_legend
      )
    }
    for (i in seq_along(p)) {
      cat(
        "#### ",
        names(p)[i],
        "\n\n"
      )
      print(p[[i]])
      cat("\n\n")
    }
    
    return(p)
  }

plot_counterfactual_maximum_margin_outside <-
  function(
    counterfactual
  ) {
    shade_df <- make_maximum_margin_shade_df(counterfactual = counterfactual)

    counterfactual <- 
      counterfactual %>%
      dplyr::filter(
        as.numeric(real_index) < 3
      ) %>%
      dplyr::mutate(
        group_label = dplyr::case_when(
          as.numeric(real_index) == 0 ~ "Non-labor",
          as.numeric(real_index) == 1 ~ "Part-time job market",
          as.numeric(real_index) == 2 ~ "Fringe THS"
        ),
        group_label = factor(
          group_label,
          levels = c(
            "Fringe THS",
            "Part-time job market",
            "Non-labor"
          )
        )
      )
    
    variables <-
      counterfactual %>%
      dplyr::select(
        -maximum_markup,
        -i
      ) %>%
      colnames() 

    # Set output directory for paper figures (outside market, maximum margin)
    output_dir <-
      here::here(
        "draft",
        "figuretable",
        "counterfactual",
        "multihome",
        "maximum_margin"
      )
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    legend_source_plot <- NULL
    
    p <-
      foreach (
        i = seq_along(variables)
      ) %do% {
        var_name <- variables[i]
        plot_df <- counterfactual
        y_label <- "Unit: level"
        if (var_name %in% c("s_w", "s_f", "m")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]] * 100
            )
          y_label <- "%"
        } else if (var_name %in% c("w", "f", "surplus_w", "surplus_f", "profit_ths")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
          y_label <- "10k yen/8h"
        } else {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
        }
        p <-
          ggplot(
            plot_df,
            aes(
              x = maximum_markup,
              y        = y_value,
              color    = group_label,
              linetype = group_label,
              shape    = group_label
            )
          ) +
          geom_rect(
            data = shade_df,
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = -Inf,
              ymax = Inf,
              alpha = alpha
            ),
            inherit.aes = FALSE,
            fill = "grey30"
          ) +
          scale_alpha_identity(
            guide = "none"
          ) +
          geom_line() + 
          geom_hline(
            yintercept = 0,
            linetype = "dotted"
          ) +
          labs(
            x = NULL,
            y = y_label
          ) +
          theme_classic(
            base_size = 12
          ) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position  = "none",
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            axis.text        = element_text(size = 11),
            plot.margin      = margin(t = 12, r = 6, b = 6, l = 6)
          ) +
          coord_cartesian(clip = "off") +
          annotate(
            "text",
            x = -Inf,
            y = Inf,
            label = y_label,
            hjust = 0,
            vjust = -0.25,
            size = 3
          ) +
          scale_color_viridis_d(
            name = "Market / THS",
            option = "viridis",
            end = 0.85
          ) +
          scale_linetype_manual(
            name   = "Market / THS",
            values = c(
              "solid",
              "dashed",
              "dotted",
              "dotdash",
              "longdash",
              "twodash"
            )
          ) +
          scale_shape_manual(
            name   = "Market / THS",
            values = c(
              16,
              17,
              15,
              1,
              2,
              0
            )
          )
        
        if (identical(variables[i], "profit_ths")) {
          legend_source_plot <-
            p +
            theme(
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title = element_text(size = 11),
              legend.text  = element_text(size = 11),
              legend.key.height = grid::unit(0.30, "cm"),
              legend.key.width  = grid::unit(0.60, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank()
            )
        } else {
          p <- p
        }
        
        # Add 45-degree line for variable "m"
        if (variables[i] == "m") {
          p <- p + 
            geom_abline(
              slope = 1,
              intercept = 0,
              linetype = "dashed",
              color = "black",
              alpha = 0.7
            )
        }
        
        for(
          j in unique(plot_df$group_label)
        ){
          p <- 
            p + 
            geom_line(
              data = subset(plot_df, group_label == j),
              aes(
                x = maximum_markup,
                  y        = y_value,
                  color    = group_label,
                  linetype = group_label,
                  shape    = group_label
              )
            ) +
            geom_point(
              size  = 2,
              alpha = 0.8
            )
        } 

        # Save BW friendly PDF for LaTeX
        file_name <-
          file.path(
            output_dir,
            paste0(
              "maximum_margin_outside_",
              variables[i],
              ".pdf"
            )
          )
        ggplot2::ggsave(
          filename = file_name,
          plot     = p,
          width    = 3.6,
          height   = 2.6,
          device   = "pdf"
        )

        return(p)
      }
    
    p <-
      p %>%
      magrittr::set_names(variables)
    p <- p[names(p) %in% "segment" == FALSE] 
    
    if (!is.null(legend_source_plot)) {
      file_name_legend <-
        file.path(
          output_dir,
          "maximum_margin_outside_legend.pdf"
        )
      save_legend_pdf(
        plot = legend_source_plot,
        file_name = file_name_legend
      )
    }
    for (i in seq_along(p)) {
      cat(
        "#### ",
        names(p)[i],
        "\n\n"
      )
      print(p[[i]])
      cat("\n\n")
    }
    
    return(p)
  }

plot_counterfactual_maximum_margin_inside <-
  function(
    counterfactual
  ) {
    shade_df <- make_maximum_margin_shade_df(counterfactual = counterfactual)

    counterfactual <- 
      counterfactual %>%
      dplyr::filter(as.numeric(real_index) >= 3) %>%
      dplyr::mutate(
        i_plot = as.integer(real_index) - 2L
      )
    
    variables <-
      counterfactual %>%
      dplyr::select(
        -maximum_markup,
        -i
      ) %>%
      colnames() 

    # Set output directory for paper figures (inside market, maximum margin)
    output_dir <-
      here::here(
        "draft",
        "figuretable",
        "counterfactual",
        "multihome",
        "maximum_margin"
      )
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    legend_source_plot <- NULL
    
    p <-
      foreach (
        i = seq_along(variables)
      ) %do% {
        var_name <- variables[i]
        plot_df <- counterfactual
        y_label <- "Unit: level"
        if (var_name %in% c("s_w", "s_f", "m")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]] * 100
            )
          y_label <- "%"
        } else if (var_name %in% c("w", "f", "surplus_w", "surplus_f", "profit_ths")) {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
          y_label <- "10k yen/8h"
        } else {
          plot_df <-
            plot_df %>%
            dplyr::mutate(
              y_value = .data[[var_name]]
            )
        }
        p <-
          ggplot(
            plot_df,
            aes(
              x = maximum_markup,
              y = y_value,
              color    = factor(i_plot),
              linetype = factor(i_plot),
              shape    = factor(i_plot)
            )
          ) +
          geom_rect(
            data = shade_df,
            aes(
              xmin = xmin,
              xmax = xmax,
              ymin = -Inf,
              ymax = Inf,
              alpha = alpha
            ),
            inherit.aes = FALSE,
            fill = "grey30"
          ) +
          scale_alpha_identity(
            guide = "none"
          ) +
          geom_line() + 
          geom_hline(
            yintercept = 0,
            linetype = "dotted"
          ) +
          labs(
            x = NULL,
            y = y_label
          ) +
          theme_classic(
            base_size = 12
          ) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position  = "none",
            axis.title.x     = element_blank(),
            axis.title.y     = element_blank(),
            axis.text        = element_text(size = 11),
            plot.margin      = margin(t = 12, r = 6, b = 6, l = 6)
          ) +
          coord_cartesian(clip = "off") +
          annotate(
            "text",
            x = -Inf,
            y = Inf,
            label = y_label,
            hjust = 0,
            vjust = -0.25,
            size = 3
          ) +
          scale_color_viridis_d(
            name = "THS",
            option = "viridis",
            end = 0.85
          ) +
          scale_linetype_manual(
            name   = "THS",
            values = c(
              "solid",
              "dashed",
              "dotted",
              "dotdash",
              "longdash",
              "twodash"
            )
          ) +
          scale_shape_manual(
            name   = "THS",
            values = c(
              16,
              17,
              15,
              1,
              2,
              0
            )
          )
        
        if (identical(variables[i], "profit_ths")) {
          legend_source_plot <-
            p +
            theme(
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title = element_text(size = 11),
              legend.text  = element_text(size = 11),
              legend.key.height = grid::unit(0.30, "cm"),
              legend.key.width  = grid::unit(0.60, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank()
            )
        } else {
          p <- p
        }

        # Add 45-degree line for variable "m"
        if (variables[i] == "m") {
          p <- p + 
            geom_abline(
              slope = 1,
              intercept = 0,
              linetype = "dashed",
              color = "black",
              alpha = 0.7
            )
        }
        for(
          j in unique(plot_df$i_plot)
        ){
          p <- 
            p + 
            geom_line(
              data = subset(plot_df, i_plot == j),
              aes(
                x = maximum_markup,
                y = y_value,
                color    = factor(i_plot),
                linetype = factor(i_plot),
                shape    = factor(i_plot)
              )
            ) +
            geom_point(
              size  = 2,
              alpha = 0.8
            )
        } 

        # Save BW friendly PDF for LaTeX
        file_name <-
          file.path(
            output_dir,
            paste0(
              "maximum_margin_inside_",
              variables[i],
              ".pdf"
            )
          )
        ggplot2::ggsave(
          filename = file_name,
          plot     = p,
          width    = 3.6,
          height   = 2.6,
          device   = "pdf"
        )

        return(p)
      }
    
    p <-
      p %>%
      magrittr::set_names(variables)
    p <- p[names(p) %in% "segment" == FALSE] 
    
    if (!is.null(legend_source_plot)) {
      file_name_legend <-
        file.path(
          output_dir,
          "maximum_margin_inside_legend.pdf"
        )
      save_legend_pdf(
        plot = legend_source_plot,
        file_name = file_name_legend
      )
    }
    for (i in seq_along(p)) {
      cat(
        "#### ",
        names(p)[i],
        "\n\n"
      )
      print(p[[i]])
      cat("\n\n")
    }
    
    return(p)
  }

solve_w_f_maximum_margin_nleqslv_unbounded_t <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a,
    x_c,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    m_bar,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    seed
  ) {
    
    c_w <- 
      compute_c_w_t(
        gamma_w = gamma_w,
        x_c = x_c,
        ec_w = ec_w
      ) 
    
    eta_f <-
      rep(
        1e-3,
        length(f) - 1
      ) %>%
      as.matrix()
    
    x <- 
      c(
        log(
          eta_f
        ),
        log(
          w[
            2:length(w)
          ]
        ),
        log(
          w[
            2:length(w)
          ] +
            c_w[
              2:length(w)
            ]
          
        )
      )
    eta_w_f <-
      list(
        eta_f = 
          exp(
            x[
              1:(length(w) - 1)
            ]
          ) %>% 
          as.matrix(),
        w = 
          c(
            w[1],
            exp(
              x[
                (length(w) - 1 + 1):( 2*(length(w)-1) )
              ]
            )
          ) %>% 
          as.matrix(),
        f = 
          c(
            f[1],
            exp(
              x[
                (length(w) - 1 + 1):(2*(length(w) - 1) )
              ] + 
                c_w[
                  2:length(w)
                ]
            )/(1 - m_bar)
          ) %>% 
          as.matrix()
      )   
    fn <-
      function(x) {
        eta_w_f <-
          list(
            eta_f = 
              exp(
                x[
                  1:(length(w) - 1)
                ]
              ) %>% 
              as.matrix(),
            w = 
              c(
                w[1],
                exp(
                  x[
                    (length(w) - 1 + 1):( 2*(length(w)-1) )
                  ]
                )
              ) %>% 
              as.matrix(),
            f = 
              c(
                f[1],
                exp(
                  x[
                    (length(w) - 1 + 1):(2*(length(w) - 1) )
                  ] + 
                    c_w[
                      2:length(w)
                    ]
                )/(1 - m_bar)
              ) %>% 
              as.matrix()
          )  
        
        foc <-
          compute_foc_maximum_margin_t(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a = x_a,
            x_c = x_c,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = eta_w_f$w,
            f = eta_w_f$f,
            s_f = s_f,
            eta_f = eta_w_f$eta_f,
            m_bar = m_bar,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol
          ) 
        return(foc)
      }
    
    solution <-
      nleqslv::nleqslv(
        x = x,
        fn = fn,
        control =
          list(
            allowSingular = TRUE
          )
      )
    
    x <- solution$x
    
    eta_w_f <-
      list(
        eta_f = 
          exp(
            x[
              1:(length(w) - 1)
            ]
          ) %>% 
          as.matrix(),
        w = 
          c(
            w[1],
            exp(
              x[
                (length(w) - 1 + 1):( 2*(length(w)-1) )
              ]
            )
          ) %>% 
          as.matrix(),
        f = 
          c(
            f[1],
            exp(
              x[
                (length(w) - 1 + 1):(2*(length(w) - 1) )
              ] + 
                c_w[
                  2:length(w)
                ]
            )/(1 - m_bar)
          ) %>% 
          as.matrix()
      )   
    return(
      list(
        eta_f = eta_w_f$eta_f,
        w = eta_w_f$w,
        f = eta_w_f$f,
        objective = max(abs(solution$fvec))
      )
    )
  }

compute_supply_shock_c_w_tj <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    x_a_w,
    size_w,
    size_f,
    mu,
    ea_w,
    f,
    w,
    s_f,
    s_w,
    owner,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {
    s_w_d_w <-
      solve_s_w_d_w_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        lambda_w = lambda_w,
        x_a_w = x_a_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        w = w,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      ) 
    
    o <- t(owner) %*% owner
    
    b <- (s_w_d_w * o) %*% (f - w) - s_w
    
    A <- s_w_d_w * o
    
    # mask private market 
    A <- 
      A[
        -1,
        -1,
        drop = FALSE
      ]
    # mask fringe ths
    A <- 
      A[
        -1,
        -1,
        drop = FALSE
      ]
    
    b <- 
      b[
        -1,
        drop = FALSE
      ]
    
    b <- 
      b[
        -1,
        drop = FALSE
      ]
    
    #ridge regularization
    cond_A <- kappa(A)
    
    # Threshold for acceptable conditioning (commonly 1e12)
    if (cond_A < 1e12) {
      # Well-conditioned: solve directly
      c_w <- Matrix::solve(A, b) %>% as.matrix()
    } else {
      # Ill-conditioned: use adaptive ridge regularization
      epsilon <- 1e-10
      A_reg <- A + epsilon * diag(ncol(A))
      c_w <- Matrix::solve(A_reg, b) %>% as.matrix()
    }

    return(c_w)
  }


compute_supply_shock_c_f_tj <-
  function(
    c_w,
    m_w,
    m_f,
    beta_w,
    beta_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    size_w,
    size_f,
    mu,
    ea_w,
    ea_f,
    owner,
    w,
    f,
    s_w,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {

    s_f_d_f <- 
      solve_s_f_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    s_w_d_f <- 
      solve_s_w_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      ) 
    
    meeting_number <- 
      compute_meeting_number_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f
      ) 
    
    meeting_number_d_f <- 
      compute_meeting_number_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        s_f = s_f,
        s_f_d_f = s_f_d_f
      ) 
    
    o <- t(owner) %*% owner
    
    b <-  meeting_number * s_w +
      (o * meeting_number_d_f) %*% (s_w * (f - w - c_w)) +
      (o * s_w_d_f) %*% (meeting_number * (f - w - c_w)) 
    
    A <-  (o * s_f_d_f) * size_f
    
    # mask private market 
    A <- 
      A[
        -1,
        -1,
        drop = FALSE
      ]
    # mask fringe ths
    A <- 
      A[
        -1,
        -1,
        drop = FALSE
      ]
    
    b <- 
      b[
        -1,
        drop = FALSE
      ]
    
    b <- 
      b[
        -1,
        drop = FALSE
      ]
    #ridge regularization
    cond_A <- kappa(A)
    
    # Threshold for acceptable conditioning (commonly 1e12)
    if (cond_A < 1e12) {
      # Well-conditioned: solve directly
      c_f <- Matrix::solve(A, b) %>% as.matrix()
    } else {
      # Ill-conditioned: use adaptive ridge regularization
      epsilon <- 1e-10
      A_reg <- A + epsilon * diag(ncol(A))
      c_f <- Matrix::solve(A_reg, b) %>% as.matrix()
    }
    
    return(c_f)
  }

solve_supply_shock <- 
  function(
    equilibrium
  ) {
    c <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = 
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {
        c_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {            
            c_w_tj <- 
              compute_supply_shock_c_w_tj(
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                lambda_w = equilibrium$parameter$lambda_w,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                w = equilibrium$endogenous[[t]][[j]]$w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              )
            
            c_w_tj <- 
              rbind(
                0,
                0,
                c_w_tj
              ) 
            
            c_f_tj <- 
              compute_supply_shock_c_f_tj(
                c_w = c_w_tj,
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                beta_f = equilibrium$parameter$beta_f,
                lambda_w = equilibrium$parameter$lambda_w,
                lambda_f = equilibrium$parameter$lambda_f,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                ea_f = equilibrium$shock[[t]][[j]]$ea_f,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                w = equilibrium$endogenous[[t]][[j]]$w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              ) 
            
            c_tj <- 
              data.frame(
                c_w = 
                  c_w_tj[
                    3:nrow(c_w_tj),
                    ,
                    drop = FALSE
                  ],
                c_f = c_f_tj
              )
            return(c_tj)
          }
        return(c_t)
      }
    colnames(c) <- c("c_w", "c_f")
    x_c_w <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = 
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {
        x_c_w_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_c_w_t_j <- equilibrium$exogenous[[t]][[j]]$x_c_w
            return(
              x_c_w_t_j[
                3:nrow(x_c_w_t_j),
                ,
                drop = FALSE
              ]
            )
          }
        return(
          x_c_w_t
        )
      }
    
    x_c_f <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = 
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %do% {
        x_c_f_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_c_f_t_j <- equilibrium$exogenous[[t]][[j]]$x_c_f
            return(
              x_c_f_t_j[
                3:nrow(x_c_f_t_j),
                ,
                drop = FALSE
              ]
            )
          }
        return(x_c_f_t)
      }
    
    result_w <- 
      lm(
        c$c_w ~ x_c_w + 0
      )
    
    gamma_w <- 
      result_w$coefficients
    gamma_w <- 
      ifelse(
        is.na(gamma_w),
        0,
        gamma_w
      )
    ec_w <- 
      result_w$residuals %>%
      as.matrix()
    
    result_f <- 
      lm(
        c$c_f ~ x_c_f + 0 
      )
    
    gamma_f <- 
      result_f$coefficients
    gamma_f <- 
      ifelse(
        is.na(gamma_f),
        0,
        gamma_f
      )
    ec_f <- 
      result_f$residuals %>%
      as.matrix()
    
    result <-
      list(
        gamma_w = gamma_w,
        gamma_f = gamma_f,
        ec_w = ec_w,
        ec_f = ec_f
      )
    return(result)
  }

update_supply_ec_gamma <-
  function(
    result_supply,
    equilibrium
  ) {
    end <- 0
    for (
      t in seq_along(equilibrium$shock)
    ) {
      for (
        j in seq_along(equilibrium$shock[[t]])
      ) {
        start <- end + 1
        end <- end + nrow(equilibrium$shock[[t]][[j]]$ec_w) - 2
        equilibrium$shock[[t]][[j]]$ec_w[
          3:nrow(equilibrium$shock[[t]][[j]]$ec_w)
        ] <- 
          result_supply$ec_w[start:end]
        equilibrium$shock[[t]][[j]]$ec_f[
          3:nrow(equilibrium$shock[[t]][[j]]$ec_f),
        ] <- 
          result_supply$ec_f[start:end]
      }
    }
    equilibrium$parameter$gamma_w <- result_supply$gamma_w
    equilibrium$parameter$gamma_f <- result_supply$gamma_f
    return(equilibrium)
  }

estimate_parameter <-
  function(
    equilibrium
  ) {
    solution_demand <-
      estimate_demand_parameter(
        equilibrium = equilibrium
      ) 
    
    equilibrium_updated <-
      update_demand(
        solution_demand = solution_demand,
        equilibrium = equilibrium
      )
    
    result_supply <- 
      solve_supply_shock(
        equilibrium = equilibrium_updated
      )
    
    equilibrium_updated <-
      update_supply_ec_gamma(
        result_supply = result_supply,
        equilibrium = equilibrium_updated
      )
    
    return(
      equilibrium_updated
    )
  }

estimate_parameter_with_penalty <-
  function(
    equilibrium
  ) {
    solution_demand_with_penalty <-
      estimate_demand_parameter_with_penalty(
        equilibrium = equilibrium
      ) 
    
    equilibrium_updated <-
      update_demand_with_penalty(
        solution_demand = solution_demand_with_penalty,
        equilibrium = equilibrium
      )
    
    result_supply <- 
      solve_supply_shock(
        equilibrium = equilibrium_updated
      )
    
    equilibrium_updated <-
      update_supply_ec_gamma(
        result_supply = result_supply,
        equilibrium = equilibrium_updated
      )
    
    return(
      equilibrium_updated
    )
  }

estimate_parameter_constrained <-
  function(
    weighting_matrix_demand,
    equilibrium
  ) {
    solution_demand <-
      estimate_demand_parameter_constrained(
        weighting_matrix_demand = weighting_matrix_demand,
        equilibrium = equilibrium
      ) 
    
    equilibrium_updated <-
      update_demand_nloptr(
        solution_demand = solution_demand,
        equilibrium = equilibrium
      )
    
    result_supply <- 
      solve_supply_shock(
        equilibrium = equilibrium_updated
      )
    
    equilibrium_updated <-
      update_supply_ec_gamma(
        result_supply = result_supply,
        equilibrium = equilibrium_updated
      )
    
    return(
      equilibrium_updated
    )
  }

compute_demand_shock_a_f_tj <- 
  function(
    m_f,
    m_w,
    lambda_f,
    mu,
    size_f,
    size_w,
    f,
    s_f
  ) {
    s_f_0 <- 1 - sum(s_f)
    a_f <- 
      s_f / (
        s_f_0 * (mu * size_w^m_w) * (size_f * s_f)^(m_f - 1)
      ) 
    a_f <- log(a_f) - lambda_f * f
    return(a_f)
  }

compute_demand_shock_nleqslv_a_w_tj <- 
  function(
    m_w,
    m_f,
    lambda_w,
    size_w,
    size_f,
    mu,
    w,
    s_f,
    s_w,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {
    s_w_real <- s_w
    
    fn <- 
      function(x) {
        if (
          method_s_w == "exact"
        ) {
          s_w <-
            solve_s_w_tj_from_a_w_exact_rcpp(
              a_w = x,
              m_w = m_w,
              m_f = m_f,
              lambda_w = lambda_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              w = w,
              s_f = s_f
            )
        } else {
          s_w <- 
            solve_s_w_tj_from_a_w_approximate_rcpp(
              a_w = x,
              m_w = m_w,
              m_f = m_f,
              lambda_w = lambda_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              w = w,
              s_f = s_f,
              margin = margin,
              quadrature_size = quadrature_size,
              tol = tol
            )
        }
        
        buffer <- 1e-16
        # Check for zeros or non-positive values
        if (
          any(s_w <= 0, na.rm = TRUE)
          ) {
          cat(s_w)
          
          s_w <- pmax(s_w, buffer)
        }
        
        objective <- log(s_w_real) - log(s_w)
        
        return(objective)
      }

    x <- 
      rep(
        0,
        length(w)
      ) %>% as.matrix()
    
    solution <-
      nleqslv::nleqslv(
        x = x,
        fn = fn,
        method = "Broyden",
        control = 
          list(
            allowSingular = TRUE
          )
      )

    a_w <- 
      solution$x %>% as.matrix()
    
    return(a_w)
    
  }

compute_demand_shock_iteration_a_w_tj <- 
  function(
    m_w,
    m_f,
    lambda_w,
    size_w,
    size_f,
    mu,
    w,
    s_f,
    s_w,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {
    s_w_real <- s_w
    fn <- 
      function(x) {
        if (
          method_s_w == "exact"
        ) {
          s_w <-
            solve_s_w_tj_from_a_w_exact(
              a_w = x,
              m_w = m_w,
              m_f = m_f,
              lambda_w = lambda_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              w = w,
              s_f = s_f
            )
        } else {
          s_w <- 
            solve_s_w_tj_from_a_w_approximate(
              a_w = x,
              m_w = m_w,
              m_f = m_f,
              lambda_w = lambda_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              w = w,
              s_f = s_f,
              margin = margin,
              quadrature_size = quadrature_size,
              tol = tol
            )
        }
        
        objective <- log(s_w_real) - log(s_w)
        
        return(objective)
      }
    
    old_a_w <- 
      rep(
        0.1,
        length(w)
      ) %>% 
      as.matrix()
    
    distance <- 100
    
    while (distance > 1e-10) {
      
      objective <- fn(old_a_w)
      
      a_w <- 
        old_a_w + 
        objective
      
      distance <- 
        abs(
          max(old_a_w - a_w)
        )
      old_a_w <- a_w
    }

    return(a_w)
  }

compute_demand_shock_nloptr_a_w_tj <- 
  function(
    m_w,
    m_f,
    lambda_w,
    size_w,
    size_f,
    mu,
    w,
    s_f,
    s_w,
    method_s_w,
    margin,
    quadrature_size,
    tol
  ) {
    s_w_real <- s_w

    fn <- 
      function(x) {
        if (
          method_s_w == "exact"
        ) {
          s_w <-
            solve_s_w_tj_from_a_w_exact(
              a_w = x,
              m_w = m_w,
              m_f = m_f,
              lambda_w = lambda_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              w = w,
              s_f = s_f
            )
        } else {
          s_w <- 
            solve_s_w_tj_from_a_w_approximate(
              a_w = x,
              m_w = m_w,
              m_f = m_f,
              lambda_w = lambda_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              w = w,
              s_f = s_f,
              margin = margin,
              quadrature_size = quadrature_size,
              tol = tol
            )
        }
        
        diff <- log(s_w_real) - log(s_w)
        
        objective <- sum(diff^2)
        
        return(objective)
      }

    x0 <- 
      rep(
        0,
        length(w)
      ) %>% as.matrix()

    solution <- 
      nloptr::nloptr(
        x0 = x0,
        eval_f = fn,
        opts = list(
          algorithm = "NLOPT_LN_BOBYQA", 
          xtol_rel = tol        
        )
      )

    a_w <- 
      solution$solution %>% as.matrix()
    return(a_w)
  }
  
solve_demand_shock <- 
  function(
    equilibrium
  ) {    
    a_f <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = c(
          "foreach",
          "magrittr",
          "Dispatching"
        )
      ) %dopar% {
        a_f_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            a_f_tj <- 
              compute_demand_shock_a_f_tj_rcpp(
                m_f = equilibrium$parameter$m_f,
                m_w = equilibrium$parameter$m_w,
                lambda_f = equilibrium$parameter$lambda_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f
              )
            return(a_f_tj)
          }
        return(a_f_t)
      }
    
    a_w <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = c(
          "foreach",
          "magrittr",
          "Dispatching"
        )
      ) %dopar% {
        a_w_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            a_w_tj <- 
              compute_demand_shock_nleqslv_a_w_tj(
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                lambda_w = equilibrium$parameter$lambda_w,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                w = equilibrium$endogenous[[t]][[j]]$w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              )
            return(a_w_tj)
          }
        return(a_w_t)
      }
    
    x_a_w <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = c(
          "foreach",
          "magrittr",
          "Dispatching"
        )
      ) %dopar% {
        x_a_w_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_a_w_t_j <- equilibrium$exogenous[[t]][[j]]$x_a_w
            return(x_a_w_t_j)
          }
        return(x_a_w_t)
      }
    
    x_a_f <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = c(
          "foreach",
          "magrittr",
          "Dispatching"
        )
      ) %dopar% {
        x_a_f_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_a_f_t_j <- equilibrium$exogenous[[t]][[j]]$x_a_f
            return(x_a_f_t_j)
          }
        return(x_a_f_t)
      }
    
    result_w <- 
      lm(
        a_w ~ x_a_w + 0
      )
    
    beta_w <- 
      result_w$coefficients %>% as.numeric()
    beta_w <-
      ifelse(
        is.na(beta_w),
        0,
        beta_w
      )
    
    ea_w <-
      a_w - x_a_w %*% beta_w
    ea_w <-
      ea_w %>% as.matrix()
    
    result_f <- 
      lm(
        a_f ~ x_a_f + 0 
      )
    
    beta_f <- 
      result_f$coefficients %>% as.numeric()
    beta_f <-
      ifelse(
        is.na(beta_f),
        0,
        beta_f
      )
    
    ea_f <-
      a_f - x_a_f %*% beta_f
    ea_f <-
      ea_f %>% as.matrix()
    
    result <-
      list(
        beta_w = beta_w,
        beta_f = beta_f,
        ea_w = ea_w,
        ea_f = ea_f
      )
    
    return(result)
  }

make_demand_cost_iv <- 
  function(
    equilibrium
  ) {
    x_c_tilde <- 
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages = c(
          "foreach",
          "magrittr",
          "Dispatching"
        )
      ) %dopar% {
        foreach(
          j = seq_along(equilibrium$exogenous[[t]])
        ) %do% {
          x_c_w <- equilibrium$exogenous[[t]][[j]]$x_c_w
          x_c_f <- equilibrium$exogenous[[t]][[j]]$x_c_f          
          x_c_tilde <- 
            cbind(
              x_c_w,
              x_c_f
            )
          return(x_c_tilde)
        }
      }
  }

make_instrument_demand <- 
  function(
    equilibrium
  ) {
    z_w <- 
      make_demand_differential_iv_z_w(
        equilibrium = equilibrium
      )
    
    z_f <- 
      make_demand_differential_iv_z_f(
        equilibrium = equilibrium
      )
    
    x_c_tilde <- 
      make_demand_cost_iv(
        equilibrium = equilibrium
      )
    
    iv <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .packages = c(
          "foreach",
          "magrittr",
          "Dispatching"
        )
      ) %dopar% {
        iv_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]])
          ) %do% {
            x_c_tilde_tj <- x_c_tilde[[t]][[j]]
            z_w_ij <- z_w[[t]][[j]]
            z_f_ij <- z_f[[t]][[j]]
            iv_tj <- 
              cbind(
                x_c_tilde_tj,
                z_w_ij,
                z_f_ij
              ) 
            return(iv_tj)
          }
        return(iv_t)
      }
    return(iv)          
  }

compute_moment_demand <- 
  function(
    instrument_demand,
    ea_w,
    ea_f
  ) {
    instrument <-
      instrument_demand %>%
      purrr::map(
        purrr::reduce,
        rbind
      ) %>%
      purrr::reduce(
        rbind
      )
    
    combo_info <- caret::findLinearCombos(instrument)
    if (!is.null(combo_info$remove)) {
      instrument <- instrument[, -combo_info$remove]
    }
    
    moment_w <-
      ea_w %*% t(rep(1, ncol(instrument)))
    moment_w <-
      moment_w * 
      instrument
    moment_w <-
      moment_w %>%
      colMeans() %>%
      as.matrix()
    
    moment_f <-
      ea_f %*% t(rep(1, ncol(instrument)))
    moment_f <-
      moment_f * 
      instrument
    moment_f <-
      moment_f %>%
      colMeans() %>%
      as.matrix()
    
    moment_demand <-
      rbind(
        moment_w,
        moment_f
      )
    
    return(moment_demand)
  }

compute_demand_weighting_matrix <- 
  function(
    instrument_demand
  ) {
    instrument <-
      instrument_demand %>%
      purrr::map(
        purrr::reduce,
        rbind
      ) %>%
      purrr::reduce(
        rbind
      )
    combo_info <- 
      caret::findLinearCombos(instrument)
    if (!is.null(combo_info$remove)) {
      instrument <- 
        instrument[
          , 
          -combo_info$remove
          ]
    }
    
    w <- 
      diag(
        2 * ncol(instrument)
      )
    return(w)
  }

compute_demand_efficient_weighting_matrix <- 
  function(
    instrument_demand,
    ea_w,
    ea_f
  ){
    instrument <-
      instrument_demand %>%
      purrr::map(
        purrr::reduce,
        rbind
      ) %>%
      purrr::reduce(
        rbind
      )
    
    combo_info <- caret::findLinearCombos(instrument)
    if (!is.null(combo_info$remove)) {
      instrument <- instrument[, -combo_info$remove]
    }
    
    n <- nrow(ea_w) 
    moment_w <-
      ea_w %*% t(rep(1, ncol(instrument)))
    moment_w <-
      moment_w * 
      instrument
    
    moment_f <-
      ea_f %*% t(rep(1, ncol(instrument)))
    moment_f <-
      moment_f * 
      instrument
    
    moment <-
      cbind(
        moment_w,
        moment_f
      )
    moment <-
      crossprod(moment)/n
    
    w <- solve(moment)
      
    return(w)
  }

compute_demand_objective <- 
  function(
    instrument_demand,
    weighting_matrix_demand,
    equilibrium
  ) {
    
    result_demand <- 
      solve_demand_shock(
        equilibrium = equilibrium
      )
    
    moment_demand <- 
      compute_moment_demand(
        instrument_demand = instrument_demand,
        ea_w = result_demand$ea_w,
        ea_f = result_demand$ea_f
      )
    
    objective <- 
      t(moment_demand) %*% weighting_matrix_demand %*% moment_demand
    
    return(objective)
  }

compute_demand_objective_with_penalty <- 
  function(
    instrument_demand,
    weighting_matrix_demand,
    equilibrium
  ) {
    
    result_demand <- 
      solve_demand_shock(
        equilibrium = equilibrium
      )
    
    equilibrium <-
      update_demand_ea_beta(
        result_demand = result_demand,
        equilibrium = equilibrium
      ) 
    
    moment_demand <- 
      compute_moment_demand(
        instrument_demand = instrument_demand,
        ea_w = result_demand$ea_w,
        ea_f = result_demand$ea_f
      )
    
    objective <- 
      t(moment_demand) %*% weighting_matrix_demand %*% moment_demand
    
    c <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind,
        .packages = 
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {
        c_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {            
            c_w_tj <- 
              compute_supply_shock_c_w_tj(
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                lambda_w = equilibrium$parameter$lambda_w,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                w = equilibrium$endogenous[[t]][[j]]$w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              )
            
            c_w_tj <- 
              rbind(
                0,
                0,
                c_w_tj
              ) 
            
            c_f_tj <- 
              compute_supply_shock_c_f_tj(
                c_w = c_w_tj,
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                beta_f = equilibrium$parameter$beta_f,
                lambda_w = equilibrium$parameter$lambda_w,
                lambda_f = equilibrium$parameter$lambda_f,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                ea_f = equilibrium$shock[[t]][[j]]$ea_f,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                w = equilibrium$endogenous[[t]][[j]]$w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              ) 
            
            c_tj <- 
              rbind(
                c_w_tj[
                  3:nrow(c_w_tj),
                  ,
                  drop = FALSE
                ],
                c_f_tj
              )
            return(c_tj)
          }
        return(c_t)
      }
    c <- 
      ifelse(
        c >= 0,
        0,
        -c
      )
    objective_with_penalty <- 
      objective + sqrt(sum(c))
    return(objective_with_penalty)
  }

compute_demand_objective_nonlinear <-
  function(
    theta_demand_nonlinear,
    instrument_demand,
    weighting_matrix_demand,
    equilibrium
  ) {
    parameter <-
      transform_theta_to_parameter_demand(
        theta = theta_demand_nonlinear,
        parameter = equilibrium$parameter
      )

    equilibrium$parameter <- parameter
    equilibrium <-
      update_mu(
        equilibrium = equilibrium,
        parameter = parameter
      ) 
    
    objective <- 
      compute_demand_objective(
        instrument_demand = instrument_demand,
        weighting_matrix_demand = weighting_matrix_demand,
        equilibrium = equilibrium
      )
    
    return(objective)
  }

compute_demand_objective_with_penalty_nonlinear <-
  function(
    theta_demand_nonlinear,
    instrument_demand,
    weighting_matrix_demand,
    equilibrium
  ) {
    parameter <-
      transform_theta_to_all_parameter_demand(
        theta = theta,
        parameter = equilibrium$parameter
      )
    
    equilibrium$parameter <- parameter
    equilibrium <-
      update_mu(
        equilibrium = equilibrium,
        parameter = parameter
      ) 
    
    objective <- 
      compute_demand_objective_with_penalty(
        instrument_demand = instrument_demand,
        weighting_matrix_demand = weighting_matrix_demand,
        equilibrium = equilibrium
      )
    
    return(objective)
  }

plot_objective_supply <- 
  function(
    target,
    theta,
    equilibrium,
    use_parallel
  ) {
    instrument_supply <- 
      make_supply_iv(
        equilibrium = equilibrium
      )
    
    weighting_matrix_supply <- 
      diag(
        2*(
          ncol(
            equilibrium$exogenous[[1]][[1]]$x_c_w
          ) + 
            ncol(
              equilibrium$exogenous[[1]][[1]]$x_c_f
            ) 
        )
      )
    
    target_true <- theta[[target]] 
    target_list <- 
      theta[[target]] * 
      seq(
        1 - 1e-1, 
        1 + 1e-1,
        by = 1e-2
      )
    objective <-
      foreach (
        i = 1:length(target_list),
        .combine = "rbind"
      ) %do% {
        
        theta[[target]] <- target_list[i]
        
        if (target == "m_f"){
          equilibrium$parameter$m_f <- theta[[target]]
          equilibrium$parameter$m_w <- 1 - equilibrium$parameter$m_f
        }else{
          equilibrium$parameter[[target]] <- theta[[target]]
        }
        
        objective_i <- 
          compute_supply_objective_nonlinear(
            m_f = equilibrium$parameter$m_f,
            m_w = equilibrium$parameter$m_w,
            lambda_w = equilibrium$parameter$lambda_w,
            lambda_f = equilibrium$parameter$lambda_f,
            instrument_supply = instrument_supply,
            weighting_matrix_supply = weighting_matrix_supply,
            equilibrium = equilibrium
          ) 
        return(objective_i)
      }
    
    df_graph <- 
      data.frame(
        x = target_list, 
        y = objective
      )
    min_y_value <- min(objective) - 1e-10
    max_y_value <- max(objective) + 1e-10
    g <- 
      ggplot(
        data = df_graph, 
        aes(
          x = x,
          y = y
        )
      ) +
      geom_point() +
      geom_vline(
        xintercept = target_true,
        linetype = "dotted"
      ) +
      ylab("objective supply function") + 
      xlab(target) + 
      theme_classic() +
      ylim(min_y_value, max_y_value)
    
    return(g)
  }

plot_objective_demand <- 
  function(
    target,
    theta,
    equilibrium
  ) {
    instrument_demand <- 
      make_demand_iv(
        equilibrium = equilibrium
      )
    
    weighting_matrix_demand <- 
      diag(
        2*(
          ncol(
            equilibrium$exogenous[[1]][[1]]$x_a_w
          ) + 
            ncol(
              equilibrium$exogenous[[1]][[1]]$x_a_f
            ) + 4
        )
      )
    
    target_true <- theta[[target]] 
    target_list <- 
      theta[[target]] * 
      seq(
        1 - 1e-1, 
        1 + 1e-1,
        by = 1e-2
      )
    objective <-
      foreach (
        i = 1:length(target_list),
        .combine = "rbind"
      ) %do% {
        
        theta[[target]] <- target_list[i]
        
        if (target == "m_f"){
          equilibrium$parameter$m_f <- theta[[target]]
          equilibrium$parameter$m_w <- 1 - equilibrium$parameter$m_f
        }else{
          equilibrium$parameter[[target]] <- theta[[target]]
        }
        
        objective_i <- 
          compute_demand_objective_nonlinear(
            theta_demand_nonlinear = theta,
            instrument_demand = instrument_demand,
            weighting_matrix_demand = weighting_matrix_demand,
            equilibrium = equilibrium
          ) 
        return(objective_i)
      }
    
    min_y_value <- min(objective) - 1e-10
    max_y_value <- max(objective) + 1e-10
    
    df_graph <- 
      data.frame(
        x = target_list, 
        y = objective
      )
    
    g <- 
      ggplot(
        data = df_graph, 
        aes(
          x = x,
          y = y
        )
      ) +
      geom_point() +
      geom_vline(
        xintercept = target_true,
        linetype = "dotted"
      ) +
      ylab("objective demand function") + 
      xlab(target) + 
      theme_classic() +
      ylim(min_y_value, max_y_value)
    return(g)
  }

interpolate_year <-
  function(
    data_area_year
  ) {
    area_year <-
      data_area_year %>%
      dplyr::distinct(
        area_code,
        year
      ) %>%
      tidyr::complete(
        area_code = unique(area_code),
        year = 2009:2015
      )
    
    data_area_year_expand <-
      area_year %>%
      dplyr::left_join(
        data_area_year,
        by = c(
          "area_code",
          "year"
        )
      ) %>%
      dplyr::arrange(
        area_code,
        year
      ) %>%
      dplyr::group_by(
        area_code
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ zoo::na.approx(
            . %>% as.numeric(),
            rule = 2,
            na.rm = FALSE
          ) %>% round()
        )
      ) %>%
      dplyr::ungroup()
    
    return(data_area_year_expand)
  }

make_data_cz_year <-
  function(
    data_area_year_num_parttemp,
    data_area_year_num_firm,
    data_area_year_num_labor,
    data_area_year_partwage
  ) {
    data_area_year_num_firm_expand <-
      interpolate_year(
        data_area_year = data_area_year_num_firm
      ) 
    
    data_area_year_num_labor_expand <-
      interpolate_year(
        data_area_year = data_area_year_num_labor
      ) 
    
    data_area_year_num_parttemp_expand <-
      interpolate_year(
        data_area_year = data_area_year_num_parttemp
      ) 
    
    data_cz_year <-
      data_area_year_num_parttemp_expand %>%
      dplyr::left_join(
        data_area_year_num_firm_expand,
        by = c(
          "area_code",
          "year"
        )
      ) %>%
      dplyr::left_join(
        data_area_year_num_labor_expand,
        by = c(
          "area_code",
          "year"
        )
      ) %>%
      dplyr::left_join(
        data_area_year_partwage %>%
          dplyr::select(
            -n
          ),
        by = c(
          "area_code",
          "year"
        )
      ) %>%
      dplyr::left_join(
        data_area,
        by = "area_code"
      ) %>%
      dplyr::mutate(
        num_parttemp = parttime + temp
      ) %>%
      dplyr::filter(
        is.finite(cz)
      ) %>%
      dplyr::select(
        -firms_all,
        -labor_all
      ) %>%
      dplyr::group_by(
        cz,
        year
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            num_parttemp,
            dplyr::starts_with("firms_"),
            dplyr::starts_with("labor_")
          ),
          ~ sum(
            .,
            na.rm = TRUE
          )
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          ptwage,
          ~ mean(
            .,
            na.rm = TRUE
          )
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(
        cz,
        year,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("firms_") | dplyr::starts_with("labor_"),
          ~ . / num_parttemp
        )
      ) %>%
      dplyr::select(
        -area_code
      )
    
    return(data_cz_year)
  }

select_data <-
  function(
    df
  ) {
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
    
    return(df)
  }

make_data_base <-
  function(
    data_establishment,
    data_zipcode,
    data_area,
    data_pref_year_minimum_wage,
    data_year,
    data_area_year_num_parttemp,
    data_area_year_num_firm,
    data_area_year_num_labor,
    data_area_year_partwage
  ) {
    
    ## make cz year level data -----------------------------------------------
    
    data_cz_year <-
      make_data_cz_year( 
        data_area_year_num_parttemp = data_area_year_num_parttemp,
        data_area_year_num_firm = data_area_year_num_firm, 
        data_area_year_num_labor = data_area_year_num_labor,
        data_area_year_partwage = data_area_year_partwage
      )
    
    ## join variables --------------------------------------------------------
    
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
    
    ## select data -----------------------------------------------------------
    
    df <-
      select_data(
        df = df
      ) 
    
    ## rename variables -----------------------------------------------------
    
    df <-
      df %>%
      dplyr::rename(
        w = wage,
        f = fee,
        w_0 = mw
      )
    
    ## add variables --------------------------------------------------------
    
    df <-
      df %>%
      dplyr::mutate(
        f_0 = 0
      )
    
    ## deflate wage and fee --------------------------------------------------
    
    df <-
      df %>%
      dplyr::mutate(
        w = w / (cpi / 100),
        f = f / (cpi / 100)
      ) 
    
    ## make share ----------------------------------------------------------
    
    df <-
      df %>%
      dplyr::mutate(
        q = (tempfixed + tempperm),
        s_w = q / register,
        s_f = client / num_parttemp,
      )
    
    ## make market size -- --------------------------------------------------
    
    df <-
      df %>%
      dplyr::mutate(
        size_w = num_parttemp,
        size_f = num_parttemp
      )
    
    ## drop unnecesary variables --------------------------------------------
    
    df <-
      df %>%
      dplyr::select(
        -feedaily,
        -dplyr::matches("fee[0-9].*"),
        -wagedaily,
        -dplyr::matches("wage[0-9].*")
      )
    
    ## drop markets with na in main variables -------------------------------
    
    df <-
      df %>%
      dplyr::group_by(
        cz,
        year
      ) %>%
      dplyr::mutate(
        check_parttime = 
          parttime %>%
          is.na() %>%
          any(),
        check_temp =
          temp %>%
          is.na() %>%
          any(),
        check_ptwage =  
          ptwage %>%
          is.na() %>%
          any()
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
        check_parttime == FALSE,
        check_temp == FALSE,
        check_ptwage == FALSE
      ) %>%
      dplyr::select(
        -check_parttime,
        -check_temp,
        -check_ptwage
      )
    
    
    ## deal with na-dropping na ----------------------------------------------
    
    df <-
      df %>%
      dplyr::mutate_all(
        ~ ifelse(
          is.na(.),
          0,
          .
        )
      )
    
    ## show standard deviation for all variable --------------------------------
    
    df_dummy <- 
      df %>%
      dplyr::select(
        tidyselect::where(
          ~ is.numeric(.x) && all(unique(.x) %in% c(0, 1))
        )
      ) %>%
      dplyr::select(
        -f_0
      ) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          ~ sd(.x)
        ) 
      )%>%
      t() %>%                                 
      as.data.frame() %>%                    
      dplyr::rename(sd = V1) %>% 
      dplyr::arrange(sd) %>%
      dplyr::filter(
        sd < 0.1
      ) %>%
      rownames()
    
    df <-
      df %>%
      dplyr::select(
        - dplyr::any_of(
          df_dummy
        )
      )
    
    ## deal with variables with no variation ----------------------------------
    df <-
      df %>%
      dplyr::select(
        tidyselect::where (~ !is.numeric(.x) || (is.numeric(.x) && sd(.x, na.rm = TRUE) != 0))
      )
    
    return(df)
  }

make_data_base_zipcode_firm <-
  function(
    data_base
  ) {
    df <-
      data_base %>%
      dplyr::group_by(
        firm_id,
        zipcode,
        year
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            perm,
            fixed,
            tempdaily_fte,
            tempperm_fte,
            tempfixed_fte,
            register,
            tempdaily,
            tempperm,
            tempfixed,
            client,
            sales,
            overseanum,
            shokaiapp,
            shokaiactual,
            shokaioffer,
            shokaihire,
            dplyr::starts_with("length"),
            dplyr::starts_with("training")
          ),
          sum
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            f,
            dplyr::starts_with("fee"),
            w,
            dplyr::starts_with("wage")
          ),
          mean
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(
        firm_id,
        zipcode,
        year,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        q = (tempfixed + tempperm),
        s_w = q / (5 * register),
        s_f = client / num_parttemp,
        size_w = num_parttemp,
        size_f = num_parttemp
      ) %>%
      dplyr::mutate(
        s_w = 
          ifelse(
            s_w > 0.99,
            0.99,
            s_w
          )
      )
    return(
      df
    )
  }

make_object_list <-
  function(
    data_base,
    object,
    geography
  ) {
    df <-
      data_base %>%
      dplyr::arrange(
        year,
        !!rlang::sym(geography),
        firm_id
      ) %>%
      dplyr::group_split(
        year
      ) %>%
      purrr::map(
        ~ dplyr::group_split(
          .,
          !!rlang::sym(geography)
        ) 
      )
    header <-
      df %>%
      purrr::map_depth(
        .depth = 2,
        ~ dplyr::select(
          .,
          year,
          !!rlang::sym(geography),
          firm_id
        )
      )
    
    value <-
      df %>%
      purrr::map_depth(
        .depth = 2,
        ~ dplyr::select(
          .,
          dplyr::any_of(object)
        ) %>%
          as.matrix()
      ) 
    
    return(
      list(
        header = header,
        value = value
      )
    )
  }

make_object_list_fringe <-
  function(
    data_base,
    object,
    geography
  ) {
    df <-
      data_base %>%
      dplyr::arrange(
        year,
        !!rlang::sym(geography),
      ) %>%
      dplyr::group_split(
        year
      ) %>%
      purrr::map(
        ~ dplyr::group_split(
          .,
          !!rlang::sym(geography)
        ) 
      )
    header <-
      df %>%
      purrr::map_depth(
        .depth = 2,
        ~ dplyr::select(
          .,
          year,
          !!rlang::sym(geography)
        )
      )
    
    value <-
      df %>%
      purrr::map_depth(
        .depth = 2,
        ~ dplyr::select(
          .,
          dplyr::any_of(object)
        ) %>%
          as.matrix()
      ) 
    
    return(
      list(
        header = header,
        value = value
      )
    )
  }

make_endogenous_from_data <-
  function(
    data_base,
    geography
  ) {
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
      foreach (
        t = seq_along(w_list$value)
      ) %do% {
        endogenous_t <-
          foreach (
            i = seq_along(w_list$value[[t]])
          ) %do% {
            endogenous_t_i <-
              list(
                w = w_list$value[[t]][[i]],
                f = f_list$value[[t]][[i]],
                s_w = s_w_list$value[[t]][[i]],
                s_f = s_f_list$value[[t]][[i]]
              )
          }
      }
    
    return(
      endogenous = endogenous
    )
  }

add_fringe_to_w <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    
    w_fringe <-
      data_base %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        w = w
      ) %>%
      make_object_list_fringe(
        data_base = .,
        object = "w",
        geography = geography
      )
    
    w_fringe <-
      w_fringe$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      )
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$w <-
          rbind(
            w_fringe[[t]][[i]],
            endogenous[[t]][[i]]$w
          )
      }
    }
    return(endogenous)
  }

add_fringe_to_f <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    
    f_fringe <-
      data_base %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        f = f
      ) %>%
      make_object_list_fringe(
        data_base = .,
        object = "f",
        geography = geography
      )
    
    f_fringe <-
      f_fringe$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      )
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$f <-
          rbind(
            f_fringe[[t]][[i]],
            endogenous[[t]][[i]]$f
          )
      }
    }
    return(endogenous)
  }

add_fringe_to_s_w <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    s_w_fringe <-
      data_base %>%
      dplyr::mutate(
        s_w = s_w
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        s_w
      ) %>%
      make_object_list_fringe(
        data_base = .,
        object = "s_w",
        geography = geography
      )
    
    s_w_fringe <-
      s_w_fringe$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      )
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$s_w <-
          rbind(
            s_w_fringe[[t]][[i]],
            endogenous[[t]][[i]]$s_w
          )
      }
    }
    return(endogenous)
  }

add_fringe_to_s_f <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    s_f_fringe <-
      data_base %>%
      dplyr::mutate(
        s_f = s_f
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        s_f
      ) %>%
      make_object_list_fringe(
        data_base = .,
        object = "s_f",
        geography = geography
      )
    
    s_f_fringe <-
      s_f_fringe$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      ) 
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$s_f <-
          rbind(
            s_f_fringe[[t]][[i]],
            endogenous[[t]][[i]]$s_f
          )
      }
    }
    return(endogenous)  
  } 


add_parttime_to_w <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    w_parttime <-
      data_base %>%
      dplyr::select(
        -w
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        w = ptwage
      ) %>%
      make_object_list(
        data_base = .,
        object = "w",
        geography = geography
      )

    w_parttime <-
      w_parttime$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      )
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$w <-
          rbind(
            w_parttime[[t]][[i]],
            endogenous[[t]][[i]]$w
          )
      }
    }
    return(endogenous)
  }

add_parttime_to_f <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    f_parttime <-
      data_base %>%
      dplyr::select(
        -f
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        f = ptwage
      ) %>%
      make_object_list(
        data_base = .,
        object = "f",
        geography = geography
      )

    f_parttime <-
      f_parttime$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      )
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$f <-
          rbind(
            f_parttime[[t]][[i]],
            endogenous[[t]][[i]]$f
          )
      }
    }
    return(endogenous)
  }

add_parttime_to_s_w <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    s_w_parttime <-
      data_base %>%
      dplyr::select(
        - s_w
      ) %>%
      dplyr::mutate(
        s_w = parttime / num_parttemp
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        s_w
      ) %>%
      make_object_list(
        data_base = .,
        object = "s_w",
        geography = geography
      )

    s_w_parttime <-
      s_w_parttime$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      )
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$s_w <-
          rbind(
            s_w_parttime[[t]][[i]],
            endogenous[[t]][[i]]$s_w
          )
      }
    }
    return(endogenous)
  }

add_parttime_to_s_f <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    s_f_parttime <-
      data_base %>%
      dplyr::select(
        - s_f
      ) %>%
      dplyr::mutate(
        s_f = parttime / num_parttemp
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        s_f
      ) %>%
      make_object_list(
        data_base = .,
        object = "s_f",
        geography = geography
      )

    s_f_parttime <-
      s_f_parttime$value %>%
      purrr::map_depth(
        2,
        ~ unique(.)
      ) 
    for (
      t in seq_along(endogenous)
    ) {
      for (
        i in seq_along(endogenous[[t]])
      ) {
        endogenous[[t]][[i]]$s_f <-
          rbind(
            s_f_parttime[[t]][[i]],
            endogenous[[t]][[i]]$s_f
          )
      }
    }
    return(endogenous)  
  } 

add_parttime_to_endogenous <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    endogenous <-
      add_parttime_to_w(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    endogenous <-
      add_parttime_to_f(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    endogenous <-
      add_parttime_to_s_w(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    endogenous <-
      add_parttime_to_s_f(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    return(endogenous)
  }

add_fringe_to_endogenous <-
  function(
    data_base,
    endogenous,
    geography
  ) {
    endogenous <-
      add_fringe_to_w(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    endogenous <-
      add_fringe_to_f(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    endogenous <-
      add_fringe_to_s_w(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    endogenous <-
      add_fringe_to_s_f(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    return(endogenous)
  }

make_type_dummy <-
  function(
    data_base
  ) {
    df <-
      data_base %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("^wage\\d+$"),
          ~ (!is.na(.) & (. > 0)) %>% as.integer()
        )
      ) %>%
      fastDummies::dummy_cols(
        select_columns = "trainingtype"
      ) 
    
    
    df_dummy <- 
      df %>%
      dplyr::select(
        dplyr::starts_with("trainingtype_")
      ) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          ~ sd(.x)
        ) 
      )%>%
      t() %>%                                 
      as.data.frame() %>%                    
      dplyr::rename(sd = V1) %>% 
      dplyr::arrange(sd) %>%
      dplyr::filter(
        sd < 0.1
      ) %>%
      rownames()
    
    df <-
      df %>%
      dplyr::select(
        - dplyr::any_of(
          df_dummy
        )
      )
    
    return(
      df
    )
  }


make_x_a_w <-
  function(
    data_base,
    geography
  ) {
    x_a_w <-
      make_type_dummy(
        data_base = data_base
      ) %>% 
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        dplyr::matches("^wage\\d+$"),
        dplyr::starts_with("trainingtype"),
        dplyr::any_of(
          c(
            "oversea",
            "shokai",
            "cocurrent"
          )
        ),
        dplyr::starts_with("labor_")
      ) 
    x_a_w <-
      make_object_list(
        data_base = x_a_w,
        object = x_a_w %>%
          dplyr::select(
            -year,
            -!!rlang::sym(geography),
            -firm_id
          ) %>%
          colnames(),
        geography = geography
      )

    return(
      x_a_w
    )
  }

make_x_a_f <-
  function(
    data_base,
    geography
  ) {
    x_a_f <-
      make_type_dummy(
        data_base = data_base
      ) %>% 
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        dplyr::matches("^wage\\d+$"),
        dplyr::starts_with("trainingtype"),
        dplyr::any_of(
          c(
            "oversea",
            "shokai",
            "cocurrent"
          )
        ),
        dplyr::starts_with("firms_")
      ) 
    x_a_f <-
      make_object_list(
        data_base = x_a_f,
        object = x_a_f %>%
          dplyr::select(
            -year,
            -!!rlang::sym(geography),
            -firm_id
          ) %>%
          colnames(),
        geography = geography
      )

    return(
      x_a_f
    )
  }

make_x_c_w <-
  function(
    data_base,
    geography
  ) {
    x_c_w <-
      make_type_dummy(
        data_base = data_base
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        dplyr::matches("^wage\\d+$"),
        dplyr::starts_with("trainingtype"),
        dplyr::any_of(
          c(
            "oversea",
            "shokai",
            "cocurrent"
          )
        ),
        ptwage
      ) 
    x_c_w <-
      make_object_list(
        data_base = x_c_w,
        object = x_c_w %>%
          dplyr::select(
            -year,
            -!!rlang::sym(geography),
            -firm_id
          ) %>%
          colnames(),
        geography = geography
      )
    
    return(
      x_c_w
    )
  }

make_x_c_f <-
  function(
    data_base,
    geography
  ) {
    x_c_f <-
      make_type_dummy(
        data_base = data_base
      ) %>%
      dplyr::select(
        !!rlang::sym(geography),
        year,
        firm_id,
        dplyr::matches("^fee\\d+$"),
        dplyr::starts_with("trainingtype"),
        dplyr::any_of(
          c(
            "oversea",
            "shokai",
            "cocurrent"
          )
        ),
        ptwage
      )
    x_c_f <-
      make_object_list(
        data_base = x_c_f,
        object = x_c_f %>%
          dplyr::select(
            -year,
            -!!rlang::sym(geography),
            -firm_id
          ) %>%
          colnames(),
        geography = geography
      )

    return(
      x_c_f
    )
  }

make_owner <-
  function(
    data_base,
    geography
  ) {
    owner_list <-
      make_object_list(
        data_base = data_base,
        object = "firm_id",
        geography = geography
      )
    
    owner <-
      owner_list$header %>%
      purrr::map_depth(
        2,
        ~ {
          owner_ti <-
            fastDummies::dummy_cols(
              .data = .,
              select_columns = "firm_id",
              remove_selected_columns = TRUE
            ) %>%
            dplyr::select(
              dplyr::starts_with("firm_id_")
            ) %>%
            as.matrix()
          owner_ti <- t(owner_ti)
          return(owner_ti)
        }
      )
    return(owner)
  }

update_owner <-
  function(
    data_base,
    geography
  ) {
    owner_list <-
      make_object_list(
        data_base = data_base,
        object = "firm_id",
        geography = geography
      )

    # Build a consistent set of firm_id levels from data_base
    firm_levels <-
      data_base %>%
      dplyr::distinct(firm_id) %>%
      dplyr::pull(firm_id)


    owner <-
      owner_list$header %>%
      purrr::map_depth(
        2,
        ~ {
          # Make dummies for present firms in this (t, j)
          owner_ti <-
            fastDummies::dummy_cols(
              .data = .,
              select_columns = "firm_id",
              remove_selected_columns = TRUE
            ) %>%
            dplyr::select(
              dplyr::starts_with("firm_id_")
            ) %>%
            as.matrix()

          # Pad missing global firm levels with zero columns
          present_cols <- colnames(owner_ti)
          desired_cols <- paste0("firm_id_", firm_levels)
          missing_cols <- 
            setdiff(
              desired_cols, 
              present_cols
              )
          if (length(missing_cols) > 0) {
            zeros <- 
              matrix(
                0, 
                nrow = nrow(owner_ti), 
                ncol = length(missing_cols)
                )
            
            colnames(zeros) <- missing_cols
            owner_ti <- 
              cbind(
                owner_ti, 
                zeros
                )
          }
          # Reorder to desired (global) column order
          owner_ti <- owner_ti[, desired_cols, drop = FALSE]
          
          owner_ti <- t(owner_ti)
          return(owner_ti)
        }
      )
    return(owner)
  }

make_exogenous_from_data <-
  function(
    data_base,
    geography
  ) {
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
    
    size_w_global_mean <-
      size_w %>%
      purrr::flatten() %>%
      purrr::flatten() %>%
      unlist() %>%
      mean(na.rm = TRUE)
    
    # Normalize all elements by the global mean
    size_w <-
      size_w %>%
      purrr::map_depth(
        2,
        ~ . / size_w_global_mean
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
    
    size_f_global_mean <-
      size_f %>%
      purrr::flatten() %>%
      purrr::flatten() %>%
      unlist() %>%
      mean(na.rm = TRUE)
    
    # Normalize all elements by the global mean
    size_f <-
      size_f %>%
      purrr::map_depth(
        2,
        ~ . / size_f_global_mean
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
      foreach (
        t = seq_along(x_a_w_list$value)
      ) %do% {
        endogenous_t <-
          foreach (
            i = seq_along(x_a_w_list$value[[t]])
          ) %do% {
            exogenous_t_i <-
              list(
                x_a_w = x_a_w_list$value[[t]][[i]],
                x_a_f = x_a_f_list$value[[t]][[i]],
                x_c_w = x_c_w_list$value[[t]][[i]],
                x_c_f = x_c_f_list$value[[t]][[i]],
                w_0 = w_0[[t]][[i]],
                f_0 = f_0[[t]][[i]],
                size_w = size_w[[t]][[i]] %>% as.numeric(),
                size_f = size_f[[t]][[i]] %>% as.numeric(),
                owner = owner[[t]][[i]]
              )
          }
      }
    
    return(
      exogenous
    )
  }

add_zero_to_top <-
  function(
    object
  ) {
    object <-
      rbind(
        rep(
          0,
          ncol(object)
        ) %>%
          as.matrix() %>%
          t(),
        object
      )
    return(object)
  }

add_one_to_top <-
  function(
    object
  ) {
    object <-
      rbind(
        rep(
          1,
          ncol(object)
        ) %>%
          as.matrix() %>%
          t(),
        object
      )
    return(object)
  }

add_parttime_to_exogenous <-
  function(
    exogenous,
    geography
  ) {
    for (
      t in seq_along(exogenous)
    ) {
      for (
        i in seq_along(exogenous[[t]])
      ) {
        exogenous[[t]][[i]]$x_a_w <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_a_w
          )
        exogenous[[t]][[i]]$x_a_f <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_a_f
          )
        exogenous[[t]][[i]]$x_c_w <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_c_w
          )
        exogenous[[t]][[i]]$x_c_f <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_c_f
          )
        exogenous[[t]][[i]]$owner <-
          Matrix::bdiag(
            list(
              1,
              exogenous[[t]][[i]]$owner
            )
          ) %>%
          as.matrix()
      }
    }
    return(exogenous)
  }

add_fringe_to_exogenous <-
  function(
    exogenous,
    geography
  ) {
    for (
      t in seq_along(exogenous)
    ) {
      for (
        i in seq_along(exogenous[[t]])
      ) {
        exogenous[[t]][[i]]$x_a_w <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_a_w
          )
        exogenous[[t]][[i]]$x_a_f <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_a_f
          )
        exogenous[[t]][[i]]$x_c_w <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_c_w
          )
        exogenous[[t]][[i]]$x_c_f <-
          add_zero_to_top(
            object = exogenous[[t]][[i]]$x_c_f
          )
        exogenous[[t]][[i]]$owner <-
          Matrix::bdiag(
            list(
              1,
              exogenous[[t]][[i]]$owner
            )
          ) %>%
          as.matrix()
      }
    }
    return(exogenous)
  }

make_shock_zero <-
  function(
    data_base,
    geography 
  ) {
    shock_list <-
      make_object_list(
        data_base = data_base,
        object = "w",
        geography = geography
      )
    shock <-
      shock_list$value %>%
      purrr::map_depth(
        2,
        ~ {
          x <-
            magrittr::set_colnames(
              .,
              ""
            )
          return(0 * x)
        }
      )
    return(shock)
  }

make_shock_from_data <-
  function(
    data_base,
    geography
  ) {
    shock_zero <-
      make_shock_zero(
        data_base = data_base,
        geography = geography
      )
    shock <-
      foreach(
        t = seq_along(shock_zero)
      ) %do% {
        shock_t <-
          foreach(
            i = seq_along(shock_zero[[t]])
          ) %do% {
            shock_t_i <-
              list(
                mu = shock_zero[[t]][[i]] + 1,
                ea_w = shock_zero[[t]][[i]],
                ea_f = shock_zero[[t]][[i]],
                ec_w = shock_zero[[t]][[i]],
                ec_f = shock_zero[[t]][[i]]
              )
          }
      }
    return(shock)
  }

add_parttime_to_shock <-
  function(
    shock
  ) {
    for (
      t in seq_along(shock)
    ) {
      for (
        i in seq_along(shock[[t]])
      ) {
        shock[[t]][[i]]$mu <-
          add_one_to_top(
            object = shock[[t]][[i]]$mu
          )
        shock[[t]][[i]]$ea_w <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ea_w
          )
        shock[[t]][[i]]$ea_f <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ea_f
          )
        shock[[t]][[i]]$ec_w <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ec_w
          )
        shock[[t]][[i]]$ec_f <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ec_f
          )
      }
    }
    return(shock)
  }

add_fringe_to_shock <-
  function(
    shock
  ) {
    for (
      t in seq_along(shock)
    ) {
      for (
        i in seq_along(shock[[t]])
      ) {
        shock[[t]][[i]]$mu <-
          add_one_to_top(
            object = shock[[t]][[i]]$mu
          )
        shock[[t]][[i]]$ea_w <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ea_w
          )
        shock[[t]][[i]]$ea_f <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ea_f
          )
        shock[[t]][[i]]$ec_w <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ec_w
          )
        shock[[t]][[i]]$ec_f <-
          add_zero_to_top(
            object = shock[[t]][[i]]$ec_f
          )
      }
    }
    return(shock)
  }

make_coef <-
  function(
    x 
  ) {
    coef <-
      rep(
        0,
        x %>% ncol()
      ) %>%
      as.matrix()
    return(coef)
  }

make_parameter_from_data <-
  function(
    exogenous
  ) {
    mu_p <- 1
    mu_ths <- 1
    m_w <- 0.5
    m_f <- 0.5
    lambda_w <- 0.5
    lambda_f <- -0.5
    beta_w <-
      make_coef(
        x = exogenous[[1]][[1]]$x_a_w
      )
    beta_f <-
      make_coef(
        x = exogenous[[1]][[1]]$x_a_f
      )
    gamma_w <-
      make_coef(
        x = exogenous[[1]][[1]]$x_c_w
      )
    gamma_f <-
      make_coef(
        x = exogenous[[1]][[1]]$x_c_f
      )
    return(
      list(
        mu_p = mu_p,
        mu_ths = mu_ths,
        m_w = m_w,
        m_f = m_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        beta_w = beta_w,
        beta_f = beta_f,
        gamma_w = gamma_w,
        gamma_f = gamma_f
      )
    )
  }

make_equilibrium_from_data <-
  function(
    data_establishment,
    data_zipcode,
    data_area,
    data_pref_year_minimum_wage,
    data_year,
    data_area_year_num_parttemp,
    data_area_year_num_firm,
    data_area_year_num_labor,
    data_area_year_partwage,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    minimum_wage,
    geography
  ) {
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
    
    if (geography == "nationwide"){
      
      data_base_top5 <- 
        make_data_base_nationwide_top5(
          data_base = data_base
        )

      data_base_fringe <-
        make_data_base_fringe_ths(
          data_base = data_base,
          data_base_top5 = data_base_top5
        ) 
      
      # reset geography
      geography <- "cz"
      
      endogenous <-
        make_endogenous_from_data(
          data_base = data_base_top5,
          geography = geography
        )
      exogenous <-
        make_exogenous_from_data(
          data_base = data_base_top5,
          geography = geography
        )
      shock <-
        make_shock_from_data(
          data_base = data_base_top5,
          geography = geography
        )
      parameter <-
        make_parameter_from_data(
          exogenous = exogenous
        )
      constant <-
        list(
          method_s_w = method_s_w,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol,
          minimum_wage = minimum_wage,
          geography = geography
        )
      
      # add fringe ths
      endogenous <- 
        add_fringe_to_endogenous(
          data_base = data_base_fringe,
          endogenous = endogenous,
          geography = geography
        )
      
      exogenous <-
        add_fringe_to_exogenous(
          exogenous = exogenous,
          geography = geography
        )
      
      shock <-
        add_fringe_to_shock(
          shock = shock
        )
      
      # add private market
      endogenous <-
        add_parttime_to_endogenous(
          data_base = data_base,
          endogenous = endogenous,
          geography = geography
        )
      
      exogenous <-
        add_parttime_to_exogenous(
          exogenous = exogenous,
          geography = geography
        )
      
      shock <-
        add_parttime_to_shock(
          shock = shock
        )
      
      return(
        list(
          constant = constant,
          parameter = parameter,
          shock = shock,
          exogenous = exogenous,
          endogenous = endogenous
        )
      )
      
    }
    
    if (
      geography == "zipcode"
    ) {
      data_base <-
        make_data_base_zipcode_firm(
          data_base = data_base
        )
    }
    
    endogenous <-
      make_endogenous_from_data(
        data_base = data_base,
        geography = geography
      )
    exogenous <-
      make_exogenous_from_data(
        data_base = data_base,
        geography = geography
      )
    shock <-
      make_shock_from_data(
        data_base = data_base,
        geography = geography
      )
    parameter <-
      make_parameter_from_data(
        exogenous = exogenous
      )
    constant <-
      list(
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol,
        minimum_wage = minimum_wage,
        geography = geography
      )
    
    endogenous <-
      add_parttime_to_endogenous(
        data_base = data_base,
        endogenous = endogenous,
        geography = geography
      )
    
    exogenous <-
      add_parttime_to_exogenous(
        exogenous = exogenous,
        geography = geography
      )
    
    shock <-
      add_parttime_to_shock(
        shock = shock
      )
    
    return(
      list(
        constant = constant,
        parameter = parameter,
        shock = shock,
        exogenous = exogenous,
        endogenous = endogenous
      )
    )
  }

make_data_base_nationwide_top5 <- 
  function(
    data_base
  ){
    # select top5 ths in every year 
    # rank by their q, s_w and s_f
    year_firm_id_top5 <- 
      data_base %>%
      dplyr::group_by(
        firm_id
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            perm,
            fixed,
            tempdaily_fte,
            tempperm_fte,
            tempfixed_fte,
            register,
            tempdaily,
            tempperm,
            tempfixed,
            client,
            sales,
            overseanum,
            shokaiapp,
            shokaiactual,
            shokaioffer,
            shokaihire,
            dplyr::starts_with("length"),
            dplyr::starts_with("training")
          ),
          sum
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            f,
            dplyr::starts_with("fee"),
            w,
            dplyr::starts_with("wage")
          ),
          mean
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(
        firm_id,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        q = (tempfixed + tempperm),
        s_w = q / (1e3 * register),
        s_f = client / num_parttemp,
        size_w = num_parttemp,
        size_f = num_parttemp
      ) %>%
      dplyr::arrange(
        dplyr::desc(q), 
        dplyr::desc(s_f),
        dplyr::desc(s_w)
      ) %>%
      dplyr::slice_head(n = 5) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        firm_id
      )
    
    # obtain all the data for top5
    data_base <- 
      data_base %>%
      dplyr::inner_join(
        year_firm_id_top5, 
        by = 
          c(
            "firm_id"
          )
      ) %>% 
      dplyr::group_by(
        firm_id,
        cz,
        year
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            perm,
            fixed,
            tempdaily_fte,
            tempperm_fte,
            tempfixed_fte,
            register,
            tempdaily,
            tempperm,
            tempfixed,
            client,
            sales,
            overseanum,
            shokaiapp,
            shokaiactual,
            shokaioffer,
            shokaihire,
            dplyr::starts_with("length"),
            dplyr::starts_with("training")
          ),
          sum
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            f,
            dplyr::starts_with("fee"),
            w,
            dplyr::starts_with("wage")
          ),
          mean
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(
        firm_id,
        cz,
        year,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        q = (tempfixed + tempperm),
        s_w = q / (1e2 * register),
        s_f = client / num_parttemp,
        size_w = num_parttemp,
        size_f = num_parttemp
      )

    return(data_base)
    
  }

make_data_base_fringe_ths <- 
  function(
    data_base,
    data_base_top5
  ){
    data_base_top5 <- 
      data_base_top5 %>%
      dplyr::distinct(
        firm_id,
        .keep_all = TRUE
      )
    year_firm_id_fringe_ths <- 
      data_base %>%
      dplyr::anti_join(
        data_base_top5, 
        by = 
          c(
            "firm_id"
          )
      )
    
    # aggregate all fringe to cz-year level as one
    data_base <- 
      year_firm_id_fringe_ths %>%
      dplyr::group_by(
        cz,
        year
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            perm,
            fixed,
            tempdaily_fte,
            tempperm_fte,
            tempfixed_fte,
            register,
            tempdaily,
            tempperm,
            tempfixed,
            client,
            sales,
            overseanum,
            shokaiapp,
            shokaiactual,
            shokaioffer,
            shokaihire,
            dplyr::starts_with("length"),
            dplyr::starts_with("training")
          ),
          sum
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(
            f,
            dplyr::starts_with("fee"),
            w,
            dplyr::starts_with("wage")
          ),
          mean
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(
        cz,
        year,
        .keep_all = TRUE
      ) %>%
      dplyr::mutate(
        q = (tempfixed + tempperm),
        s_w = q / (1e3 * register),
        s_f = client / num_parttemp,
        size_w = num_parttemp,
        size_f = num_parttemp
      ) 
    return(data_base)
  }

compute_distance_d_w <- 
  function(
    equilibrium
  ) {
    d_w <- 
      foreach(
        t = seq_along(equilibrium$exogenous)
      ) %do%{
        foreach(
          j = seq_along(equilibrium$exogenous[[t]])
        ) %do% { 
          x_a_w <- 
            equilibrium$exogenous[[t]][[j]]$x_a_w
          d_ij <-
            x_a_w %>%
            dist(
              upper = TRUE
            ) %>%
            as.matrix()
          return(d_ij)
        }
      }
    
    d_w_lower <-
      d_w %>%
      purrr::map_depth(
        .,
        .depth = 2,
        function(x) {
          x[upper.tri(x)] <- NA
          x <- x[!is.na(x)]
          return(x)
        }
      ) %>%
      unlist()
    
    d_w_1 <-  
      quantile(
        d_w_lower, 
        0.25
      ) %>%
      as.numeric()
    
    d_w_2 <- 
      median(
        d_w_lower
      ) %>%
      as.numeric()
    
    threshold_w <-
      c(
        d_w_1,
        d_w_2
      )
    
    return(
      list(
        d_w = d_w,
        threshold_w = threshold_w
      )
    )
  }

compute_differential_iv_z_w_tj <- 
  function(
    d_w,
    t,
    j,
    equilibrium
  ) {
    owner <- equilibrium$exogenous[[t]][[j]]$owner
    o <-  t(owner) %*% (owner)
    if (
      nrow(o) == 3
    ) {
      # add monopoly index
      z_w <-
        cbind(
          rep(
            1,
            nrow(d_w$d_w[[t]][[j]])
          ),
          rep(
            0,
            nrow(d_w$d_w[[t]][[j]])
          ) %>%
            matrix(
              nrow = nrow(d_w$d_w[[t]][[j]])
            )
        )
    } else {
      d_w_tj <- d_w$d_w[[t]][[j]]
      # mask private market and fringe ths
      d_w_tj[o == 1] <- NA
      d_w_tj[o == 2] <- NA
      z_w <-
        rowMeans(
          d_w_tj,
          na.rm = TRUE
          ) %>% as.matrix()
      
      # add non-monopoly index
      z_w <-
        cbind(
          rep(
            0,
            nrow(z_w)
          ),
          z_w
        )
    }
    return(z_w)
  }

make_demand_differential_iv_z_w <- 
  function(
    equilibrium
  ) {
    d_w <- 
      compute_distance_d_w(
        equilibrium = equilibrium
      )
    z_w <- 
      foreach(
        t = seq_along(equilibrium$endogenous)
      ) %do% {
        z_w_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]])
          ) %do% {
            z_w_tj <- 
              compute_differential_iv_z_w_tj(
                d_w = d_w,
                t = t,
                j = j,
                equilibrium = equilibrium
              )
            return(z_w_tj)
          }
        return(z_w_t)
      }
    return(z_w)
  }

compute_distance_d_f <- 
  function(
    equilibrium
  ) {
    d_f <- 
      foreach(
        t = seq_along(equilibrium$exogenous)
      ) %do% {
        foreach(
          j = seq_along(equilibrium$exogenous[[t]])
        ) %do% {
          x_a_f <- 
            equilibrium$exogenous[[t]][[j]]$x_a_f
          d_ij <-
            x_a_f %>%
            dist(
              upper = TRUE
            ) %>%
            as.matrix()
          return(d_ij)         
        }
      }
    
    d_f_lower <-
      d_f %>%
      purrr::map_depth(
        .,
        .depth = 2,
        function(x) {
          x[upper.tri(x)] <- NA
          x <- x[!is.na(x)]
          return(x)
        }
      ) %>%
      unlist()
    
    d_f_1 <- 
      quantile(
        d_f_lower, 
        0.25
      ) %>%
      as.numeric()
    
    d_f_2 <- 
      median(
        d_f_lower
      ) %>%
      as.numeric()
    
    threshold_f <-
      c(
        d_f_1,
        d_f_2
      )
    
    return(
      list(
        d_f = d_f,
        threshold_f = threshold_f
      )
    )
  }

compute_differential_iv_z_f_tj <- 
  function(
    d_f,
    t,
    j,
    equilibrium
  ) {
    owner <- equilibrium$exogenous[[t]][[j]]$owner
    o <-  t(owner) %*% (owner)
    d_f_tj <- d_f$d_f[[t]][[j]]
    
    d_f_tj[o == 1] <- NA
    d_f_tj[o == 2] <- NA
    z_f <-
      rowMeans(
        d_f_tj,
        na.rm = TRUE
      ) %>% as.matrix()
    return(z_f)
  }


make_demand_differential_iv_z_f <- 
  function(
    equilibrium
  ) {
    d_f <- 
      compute_distance_d_f(
        equilibrium = equilibrium
      )
    z_f <- 
      foreach(
        t = seq_along(equilibrium$endogenous)
      ) %do% {
        z_f_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]])
          ) %do% {
            z_f_tj <- 
              compute_differential_iv_z_f_tj(
                d_f = d_f,
                t = t,
                j = j,
                equilibrium = equilibrium
              )
            return(z_f_tj)
          }
        return(z_f_t)
      }
    return(z_f)
  }

check_equilibrium <- 
  function(
    equilibrium
  ) {
    df <- 
      foreach(
        t = seq_along(equilibrium$exogenous),
        .combine = "rbind"
      ) %do% {
        df_t <-
          foreach(
            j = seq_along(equilibrium$exogenous[[t]]),
            .combine = "rbind"
          ) %do% {
            df_tj <- 
              check_equilibrium_tj(
                t = t,
                j = j, 
                equilibrium = equilibrium
              )
            df_tj$t <- t
            df_tj$j <- j
            return(df_tj)
          }
        return(df_t)
      }
    return(df)
  }

check_a_w_time <-
  function(
    t_seq,
    j_seq,
    equilibrium
  ) {
    time <- 
      foreach(
        t = t_seq,
        .combine = rbind
      ) %do% {
        a_w_t <- 
          foreach(
            j = j_seq,
            .combine = rbind
          ) %do% {
            time <- 
              system.time(
                compute_demand_shock_nleqslv_a_w_tj(
                  m_w = equilibrium$parameter$m_w,
                  m_f = equilibrium$parameter$m_f,
                  lambda_w = equilibrium$parameter$lambda_w,
                  size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                  size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                  mu = equilibrium$shock[[t]][[j]]$mu,
                  w = equilibrium$endogenous[[t]][[j]]$w,
                  s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                  s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                  method_s_w = equilibrium$constant$method_s_w,
                  margin = equilibrium$constant$margin,
                  quadrature_size = equilibrium$constant$quadrature_size,
                  tol = equilibrium$constant$tol
                )
              )
            result <-
              data.frame(
                elapsed = time[3] %>% as.numeric()
              )
            return(result)
          }
      }
    return(time)
  }

sample_data_zip_code <- 
  function(
    n,
    seed,
    num_ths_max,
    equilibrium
  ) {
    set.seed(seed)
    
    num_ths_list <- 
      foreach(
        t = seq_along(equilibrium$endogenous)
      ) %do% {
        foreach(
          j = seq_along(equilibrium$endogenous[[t]])
        ) %do% {
          num <- length(equilibrium$endogenous[[t]][[j]]$w)
          return(num)
        }
      }
    
    # find geographical minimum index
    min_t_zones <- 
      min(
        sapply(
          equilibrium$shock, 
          length
        )
      )
    
    # pick the smallest sample year
    min_index <- 
      which.min(
        sapply(
          equilibrium$shock, 
          length
        )
      )
    
    # restrict number of ths to 20
    index_list <- 
      foreach(
        t = seq_along(equilibrium$exogenous)
      )%do%{
        index <- 
          which(
            num_ths_list[[t]] <= num_ths_max
          )
        return(index)
      }
    
    intersec <- 
      Reduce(
        intersect, 
        index_list[1:length(index_list)]
      )
    
    population_index <- 
      intersect(
        intersec,
        seq(
          1,
          min_t_zones
        )
      )
    
    # sample geographical area
    sample_unit <- 
      sample(
        population_index, 
        size = n
      )
    
    # balanced panel data
    eq <- equilibrium
    foreach (
      t = seq_along(equilibrium$endogenous)
    )%do%{
      
      endogenous <- 
        equilibrium$endogenous[[t]][sample_unit]
      
      
      exogenous <- 
        equilibrium$exogenous[[t]][sample_unit]
      
      shock <- 
        equilibrium$shock[[t]][sample_unit]
      
      eq$shock[[t]] <- shock
      eq$exogenous[[t]] <- exogenous
      eq$endogenous[[t]] <- endogenous
    }
    return(eq)
  }

check_data_variation <- 
  function(
    equilibrium,
    target
  ){
    x_a_w <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind
      ) %do% {
        x_a_w_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_a_w_t_j <- equilibrium$exogenous[[t]][[j]]$x_a_w
            return(x_a_w_t_j)
          }
        return(x_a_w_t)
      } %>% as.data.frame()
    
    x_a_f <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind
      ) %do% {
        x_a_f_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_a_f_t_j <- equilibrium$exogenous[[t]][[j]]$x_a_f
            return(x_a_f_t_j)
          }
        return(x_a_f_t)
      } %>% as.data.frame()
    
    x_c_w <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind
      ) %do% {
        x_c_w_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_c_w_t_j <- equilibrium$exogenous[[t]][[j]]$x_c_w
            return(x_c_w_t_j)
          }
        return(x_c_w_t)
      } %>% as.data.frame()
    
    x_c_f <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind
      ) %do% {
        x_c_f_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            x_c_f_t_j <- equilibrium$exogenous[[t]][[j]]$x_c_f
            return(x_c_f_t_j)
          }
        return(x_c_f_t)
      } %>% as.data.frame()
    
    if (target == "x_a_w"){
      df <- 
        x_a_w %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            ~ sd(.x)
          ) 
        )%>%
        t() %>%                                 
        as.data.frame() %>%                    
        dplyr::rename(sd = V1) %>% 
        dplyr::arrange(sd) %>%
        dplyr::filter(
          sd < 0.1
        )
    }else if (target == "x_a_f"){
      df <- 
        x_a_f %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            ~ sd(.x)
          ) 
        )%>%
        t() %>%                                 
        as.data.frame() %>%                    
        dplyr::rename(sd = V1) %>% 
        dplyr::arrange(sd) %>%
        dplyr::filter(
          sd < 0.1
        )
    }else if (target == "x_c_f"){
      df <- 
        x_c_f %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            ~ sd(.x)
          ) 
        )%>%
        t() %>%                                 
        as.data.frame() %>%                    
        dplyr::rename(sd = V1) %>% 
        dplyr::arrange(sd) %>%
        dplyr::filter(
          sd < 0.1
        )
    }else if (target == "x_c_w"){
      df <- 
        x_c_w %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            ~ sd(.x)
          ) 
        )%>%
        t() %>%                                 
        as.data.frame() %>%                    
        dplyr::rename(sd = V1) %>% 
        dplyr::arrange(sd) %>%
        dplyr::filter(
          sd < 0.1
        )
    }
    return(df)
    
  }

transform_parameter_to_theta_optim <- 
  function(
    mu_p,
    mu_ths,
    m_f,
    lambda_w,
    lambda_f
  ) {
    x_mu_p <- 
      log(mu_p)
    
    x_mu_ths <- 
      log(mu_ths)
    
    x_lambda_w <- 
      log(lambda_w) 
    
    x_m_f <- 
      log(
        m_f / (1 - m_f)
      )
    
    x_lambda_f <- 
      log(-lambda_f) 
    
    
    theta <- 
      rbind(
        x_mu_p,
        x_mu_ths,
        x_m_f,
        x_lambda_w,
        x_lambda_f
      )
    return(theta)
  }

transform_parameter_to_theta_optim_bounded <-
  function(
    mu_p,
    mu_ths,
    m_f,
    lambda_w,
    lambda_f
  ) {
    x_mu_p <-
      log(mu_p / (30 - mu_p))     # mu_p in [0, 30]
    
    x_mu_ths <-
      log(mu_ths / (30 - mu_ths)) # mu_ths in [0, 30]
    
    x_m_f <-
      log(m_f / (1 - m_f))        # m_f in (0,1)
    
    x_lambda_w <-
      log(lambda_w / (5 - lambda_w))  # lambda_w in [0,5]
    
    x_lambda_f <-
      log((-lambda_f) / (5 + lambda_f))  # lambda_f in [-5,0]
    
    theta <-
      rbind(
        x_mu_p,
        x_mu_ths,
        x_m_f,
        x_lambda_w,
        x_lambda_f
      )
    
    return(theta)
  }


transform_theta_to_parameter_demand <- 
  function(
    theta,
    parameter
  ) {
    x_mu_p <- theta[1]
    x_mu_ths <- theta[2]
    x_m_f <- theta[3]
    x_lambda_w <- theta[4]
    x_lambda_f <- theta[5]
    
    mu_p <- exp(x_mu_p)
    mu_ths <- exp(x_mu_ths)
    m_f <- exp(x_m_f) / (1 + exp(x_m_f)) 
    m_w <- 1 - m_f
    lambda_w <- exp(x_lambda_w)  
    lambda_f <- -exp(x_lambda_f)   
    
    parameter$mu_p <- mu_p
    parameter$mu_ths <- mu_ths
    parameter$m_f <- m_f
    parameter$m_w <- m_w
    parameter$lambda_w <- lambda_w
    parameter$lambda_f <- lambda_f
    
    return(parameter)
  }

transform_theta_to_parameter_demand_bounded <-
  function(
    theta,
    parameter
  ) {
    x_mu_p <- theta[1]
    x_mu_ths <- theta[2]
    x_m_f <- theta[3]
    x_lambda_w  <- theta[4]
    x_lambda_f  <- theta[5]
    
    mu_p <-
      30 * exp(x_mu_p) / (1 + exp(x_mu_p))
    
    mu_ths <-
      30 * exp(x_mu_ths) / (1 + exp(x_mu_ths))
    
    m_f <-
      exp(x_m_f) / (1 + exp(x_m_f))
    
    m_w <-
      1 - m_f
    
    lambda_w <-
      5 * exp(x_lambda_w) / (1 + exp(x_lambda_w))
    
    lambda_f <-
      -5 * exp(x_lambda_f) / (1 + exp(x_lambda_f))
    
    parameter$mu_p     <- mu_p
    parameter$mu_ths   <- mu_ths
    parameter$m_f      <- m_f
    parameter$m_w      <- m_w
    parameter$lambda_w <- lambda_w
    parameter$lambda_f <- lambda_f
    
    return(parameter)
  }

transform_all_parameter_to_theta_optim <- 
  function(
    mu_p,
    mu_ths,
    m_f,
    m_w,
    lambda_w,
    lambda_f
  ) {
    x_mu_p <- 
      log(mu_p)
    
    x_mu_ths <- 
      log(mu_ths)
    
    x_lambda_w <- 
      log(lambda_w / (5 - lambda_w))
    
    x_m_f <- 
      log(
        m_f / (1 - m_f)
      )
    x_m_w <- 
      log(
        (1 - m_f) / m_f
      )
    
    x_lambda_f <- 
      log((lambda_f + 5) / -lambda_f)
    
    
    theta <- 
      rbind(
        x_mu_p,
        x_mu_ths,
        x_m_f,
        x_m_w,
        x_lambda_w,
        x_lambda_f
      )
    return(theta)
  }

transform_theta_to_all_parameter_demand <- 
  function(
    theta,
    parameter
  ) {
    x_mu_p <- theta[1]
    x_mu_ths <- theta[2]
    x_m_f <- theta[3]
    x_m_w <- theta[4]
    x_lambda_w <- theta[5]
    x_lambda_f <- theta[6]
    
    mu_p <- exp(x_mu_p)
    mu_ths <- exp(x_mu_ths)
    m_f <- exp(x_m_f) / (1 + exp(x_m_f)) 
    m_w <- exp(x_m_w) / (1 + exp(x_m_w)) 
    lambda_w <- 5 / (1 + exp(-x_lambda_w))
    lambda_f <- -5 + 5 / (1 + exp(-x_lambda_f))
    
    parameter$mu_p <- mu_p
    parameter$mu_ths <- mu_ths
    parameter$m_f <- m_f
    parameter$m_w <- m_w
    parameter$lambda_w <- lambda_w
    parameter$lambda_f <- lambda_f
    
    return(parameter)
  }

update_mu <-
  function(
    equilibrium = equilibrium,
    parameter = parameter
  ) {
    for (
      t in seq_along(equilibrium$shock)
    ) {
      for (
        j in seq_along(equilibrium$shock[[t]])
      ) {
        equilibrium$shock[[t]][[j]]$mu <-
          rep(
            parameter$mu_ths,
            length(equilibrium$shock[[t]][[j]]$mu)
          ) %>% as.matrix()
        equilibrium$shock[[t]][[j]]$mu[1:2] <- parameter$mu_p
      }
    }
    return(equilibrium)
  }

estimate_demand_parameter <- 
  function(
    equilibrium
  ) {
    instrument_demand <- 
      make_instrument_demand(
        equilibrium = equilibrium
      )
    
    weighting_matrix_demand <- 
      compute_demand_weighting_matrix(
        instrument_demand = instrument_demand
      )
    
    theta <-
      transform_parameter_to_theta_optim(
        mu_p = equilibrium$parameter$mu_p,
        mu_ths = equilibrium$parameter$mu_ths,
        m_f = equilibrium$parameter$m_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f
      )
    
    solution <-
      optim(
        par = theta,
        fn = compute_demand_objective_nonlinear,
        method = "Nelder-Mead",
        control = list(trace = 3),
        instrument_demand = instrument_demand,
        weighting_matrix_demand = weighting_matrix_demand,
        equilibrium = equilibrium
      )
    
    # Check convergence status
    if (solution$convergence == 0) {
      message("Optimization converged successfully!")
    } else if (solution$convergence == 1) {
      warning("Optimization reached max iterations (did not converge).")
    } else {
      warning("Optimization failed with code: ", solution$convergence)
    }
    
    return(
      solution
    )
  }

estimate_demand_parameter_with_penalty <- 
  function(
    equilibrium
  ) {
    instrument_demand <- 
      make_instrument_demand(
        equilibrium = equilibrium
      )
    
    weighting_matrix_demand <- 
      compute_demand_weighting_matrix(
        instrument_demand = instrument_demand
      )
    
    theta <- 
      transform_all_parameter_to_theta_optim(
        mu_p = equilibrium$parameter$mu_p,
        mu_ths = equilibrium$parameter$mu_ths,
        m_f = equilibrium$parameter$m_f,
        m_w = equilibrium$parameter$m_w,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f
      )
    
    solution <-
      optim(
        par = theta,
        fn = compute_demand_objective_with_penalty_nonlinear,
        method = "Nelder-Mead",
        control = 
          list(
            trace = 3,
            maxit = 800
          ),
        instrument_demand = instrument_demand,
        weighting_matrix_demand = weighting_matrix_demand,
        equilibrium = equilibrium
      )
    
    # Check convergence status
    if (solution$convergence == 0) {
      message("Optimization converged successfully!")
    } else if (solution$convergence == 1) {
      warning("Optimization reached max iterations (did not converge).")
    } else {
      warning("Optimization failed with code: ", solution$convergence)
    }
    
    return(
      solution
    )
  }

compute_cost <- 
  function(
    theta,
    equilibrium
  ){
    parameter <-
      transform_theta_to_parameter_demand(
        theta = theta,
        parameter = equilibrium$parameter
      )
    equilibrium$parameter <- parameter
    equilibrium <-
      update_mu(
        equilibrium = equilibrium,
        parameter = parameter
      ) 

    result_demand <- 
      solve_demand_shock(
        equilibrium = equilibrium
      )
    
    equilibrium <-
      update_demand_ea_beta(
        result_demand = result_demand,
        equilibrium = equilibrium
      ) 
    
    c <- 
      foreach(
        t = seq_along(equilibrium$endogenous),
        .combine = rbind
      ) %do% {
        c_t <- 
          foreach(
            j = seq_along(equilibrium$endogenous[[t]]),
            .combine = rbind
          ) %do% {
            c_w_tj <- 
              compute_supply_shock_c_w_tj(
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                lambda_w = equilibrium$parameter$lambda_w,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                w = equilibrium$endogenous[[t]][[j]]$w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              )
            c_w_tj <- 
              rbind(
                0,
                0,
                c_w_tj
              )
            
            c_f_tj <- 
              compute_supply_shock_c_f_tj(
                c_w = c_w_tj,
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                beta_f = equilibrium$parameter$beta_f,
                lambda_w = equilibrium$parameter$lambda_w,
                lambda_f = equilibrium$parameter$lambda_f,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                ea_f = equilibrium$shock[[t]][[j]]$ea_f,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                w = equilibrium$endogenous[[t]][[j]]$w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                s_w = equilibrium$endogenous[[t]][[j]]$s_w,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol
              ) 
            
          c_w_tj <- 
            c_w_tj[
              3:nrow(c_w_tj),
              ,
              drop = FALSE
            ]
            
          c_f_tj <- c_f_tj
          
          c_tj <- 
            rbind(
              c_w_tj,
              c_f_tj
            )
          }
        return(c_t)
      }
    
    return(c)
  }

estimate_demand_parameter_constrained <- 
  function(
    weighting_matrix_demand,
    equilibrium
  ) {
    instrument_demand <- 
      make_instrument_demand(
        equilibrium = equilibrium
      )
    
    theta <-
      transform_parameter_to_theta_optim(
        mu_p = equilibrium$parameter$mu_p,
        mu_ths = equilibrium$parameter$mu_ths,
        m_f = equilibrium$parameter$m_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f
      )
    
    f <- 
      function(
        theta,
        instrument_demand,
        weighting_matrix_demand,
        equilibrium
        ) {
         objective <-
           compute_demand_objective_nonlinear(
             theta_demand_nonlinear = theta,
             instrument_demand = instrument_demand,
             weighting_matrix_demand = weighting_matrix_demand,
             equilibrium = equilibrium
           )
         
      return(objective)
      }
    
    g_ineq <- 
      function(
        theta,
        instrument_demand,
        weighting_matrix_demand,
        equilibrium
    ) {
      c <- 
        compute_cost(
          theta = theta,
          equilibrium = equilibrium
        )
      # Add a small buffer to ensure strict positivity
      return(-c + 1e-6)
    }

    grad_f <- 
      function(
        theta,
        instrument_demand,
        weighting_matrix_demand,
        equilibrium
    ) {
        gradient <- 
          numDeriv::grad(
            func = f, 
            x = theta, 
            instrument_demand = instrument_demand,
            weighting_matrix_demand = weighting_matrix_demand,
            equilibrium = equilibrium
          )
        gradient <- as.numeric(gradient)
        return(gradient)
    }

    jacobian_g_ineq <- 
      function(
        theta,
        instrument_demand,
        weighting_matrix_demand,
        equilibrium
      ) {
        jacobian <- 
          numDeriv::jacobian(
            func = g_ineq, 
            x = theta,
            instrument_demand = instrument_demand,
            weighting_matrix_demand = weighting_matrix_demand,
            equilibrium = equilibrium
          )
        return(jacobian)
      }

    lower_bound <- 
      c(
        -15,
        -15,
        -15,
        -6,
        -6
      )
    upper_bound <- 
      c(
        15,
        15,
        15,
        6,
        6
      )
      
    solution <-
      nloptr::nloptr(
        x0 = theta,
        eval_f = f,
        eval_grad_f = grad_f,
        eval_g_ineq = g_ineq,
        eval_jac_g_ineq = jacobian_g_ineq,
        lb = lower_bound,
        ub = upper_bound,
        opts = list(
          "algorithm" = "NLOPT_LD_MMA",
          "xtol_rel" = 1e-8,               
          "print_level" = 3
        ),
        instrument_demand = instrument_demand,
        weighting_matrix_demand = weighting_matrix_demand,
        equilibrium = equilibrium
      )
    print(solution)
    return(
      solution
    )
  }

update_demand_nonlinear <-
  function(
    solution_demand,
    equilibrium
  ) {
    parameter <-
      transform_theta_to_parameter_demand(
        theta = solution_demand$par,
        parameter = equilibrium$parameter
      )
    
    equilibrium$parameter <- parameter
    equilibrium <- 
      update_mu(
        equilibrium = equilibrium,
        parameter = parameter
      )
    
    return(equilibrium)
  }

update_demand_nonlinear_nloptr <-
  function(
    solution_demand,
    equilibrium
  ) {
    parameter <-
      transform_theta_to_parameter_demand(
        theta = solution_demand$solution,
        parameter = equilibrium$parameter
      )
    
    equilibrium$parameter <- parameter
    equilibrium <- 
      update_mu(
        equilibrium = equilibrium,
        parameter = parameter
      )
    
    return(equilibrium)
  }

update_demand_nonlinear_with_penalty <-
  function(
    solution_demand,
    equilibrium
  ) {
    parameter <-
      transform_theta_to_parameter_demand(
        theta = solution_demand$par,
        parameter = equilibrium$parameter
      )
    
    equilibrium$parameter <- parameter
    equilibrium <- 
      update_mu(
        equilibrium = equilibrium,
        parameter = parameter
      )
    
    return(equilibrium)
  }


update_demand_ea_beta <-
  function(
    result_demand,
    equilibrium
  ) {
    end <- 0
    for (
      t in seq_along(equilibrium$shock)
    ) {
      for (
        j in seq_along(equilibrium$shock[[t]])
      ) {
        start <- end + 1
        end <- end + nrow(equilibrium$shock[[t]][[j]]$ea_w)
        equilibrium$shock[[t]][[j]]$ea_w <- 
          result_demand$ea_w[start:end] %>%
          as.matrix()
        equilibrium$shock[[t]][[j]]$ea_f <- 
          result_demand$ea_f[start:end] %>%
          as.matrix()
      }
    }
    equilibrium$parameter$beta_w <- result_demand$beta_w
    equilibrium$parameter$beta_f <- result_demand$beta_f
    return(equilibrium)
  }

update_demand <-
  function(
    solution_demand,
    equilibrium
  ) {
    equilibrium <- 
      update_demand_nonlinear(
        solution_demand,
        equilibrium
      )
    
    result_demand <- 
      solve_demand_shock(
        equilibrium = equilibrium
      )
    
    equilibrium <- 
      update_demand_ea_beta(
        result_demand,
        equilibrium
      )
    
    return(equilibrium)
  }

update_demand_with_penalty <-
  function(
    solution_demand,
    equilibrium
  ) {
    equilibrium <- 
      update_demand_nonlinear_with_penalty(
        solution_demand = solution_demand,
        equilibrium = equilibrium
      ) 
    
    result_demand <- 
      solve_demand_shock(
        equilibrium = equilibrium
      )
    
    equilibrium <- 
      update_demand_ea_beta(
        result_demand = result_demand,
        equilibrium = equilibrium
      ) 
        
    return(equilibrium)
  }

update_demand_nloptr <-
  function(
    solution_demand,
    equilibrium
  ) {
    equilibrium <- 
      update_demand_nonlinear_nloptr(
        solution_demand = solution_demand,
        equilibrium = equilibrium
      )
    
    result_demand <- 
      solve_demand_shock(
        equilibrium = equilibrium
      )
    
    equilibrium <- 
      update_demand_ea_beta(
        result_demand = result_demand,
        equilibrium = equilibrium
      )
    
    return(equilibrium)
  }

solve_theta_supply_obejctive_optim <- 
  function(
    equilibrium,
    seed
  ){
    set.seed(seed)
    e <- rnorm(1)
    theta <- 
      transform_parameter_to_theta_optim(
        m_f = equilibrium$parameter$m_f,
        lambda_w = equilibrium$parameter$lambda_w,
        lambda_f = equilibrium$parameter$lambda_f
      ) + e
    
    instrument_supply <- 
      make_supply_iv(
        equilibrium = equilibrium
      )
    
    weighting_matrix_supply <- 
      diag(
        2*(
          ncol(
            equilibrium$exogenous[[1]][[1]]$x_c_w
          ) + 
            ncol(
              equilibrium$exogenous[[1]][[1]]$x_c_f
            ) 
        )
      )
    
    fn <- 
      function(theta) {
        parameter <- 
          transform_theta_to_parameter_demand(
            theta = theta
          )
        
        objective <- 
          compute_supply_objective_nonlinear(
            m_f = parameter$m_f,
            m_w = parameter$m_w,
            lambda_w = parameter$lambda_w,
            lambda_f = parameter$lambda_f,
            instrument_supply = instrument_supply,
            weighting_matrix_supply = weighting_matrix_supply,
            equilibrium = equilibrium
          ) 
        return(abs(objective))
      }
    
    solution <-
      optim(
        par = theta,
        fn = fn,
        method = "L-BFGS-B",
        control = list(trace = 1)
      )
    
    theta <- solution$par
    
    parameter <- 
      transform_theta_to_parameter_demand(
        theta = theta
      )
    return(parameter)
  }

solve_w_f_competitive_nleqslv_tj <- 
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ){
    # save for negative c_f
    c_f <-
      compute_c_f_tj(
        gamma_f = gamma_f,
        x_c_f = x_c_f,
        ec_f = ec_f,
        use_exp = use_exp
      ) 
    w_og <- w
    f_og <- f
    # Competitive regime: pin active wages to part-time wage
    w_parttime <- w[2]
    w[3:length(w)] <- w_parttime 
    
    # Left-side root selection: parameterize active fees as f = -exp(x) <= 0.
    x_base <- rep(log(200), nrow(f) - 2)
    
    fn <-
      function(x) {
        f_active <- -exp(as.numeric(x))
        w_x <- w
        f_x <- f
        f_x[3:nrow(f_x), 1] <- f_active
        
        s_f_x <-
          solve_s_f_tj_rcpp(
            m_w = m_w,
            m_f = m_f,
            beta_f = beta_f,
            lambda_f = lambda_f,
            x_a_f = x_a_f,
            size_w = size_w,
            size_f = size_f,
            mu = mu,
            ea_f = ea_f,
            f = f_x,
            s_f = s_f
          ) %>%
          as.matrix()
        
        s_w_x <-
          if (method_s_w == "approximate"){
            solve_s_w_tj_approximate(
              m_w = m_w,
              m_f = m_f,
              beta_w = beta_w,
              lambda_w = lambda_w,
              x_a_w = x_a_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              ea_w = ea_w,
              w = w_x,
              s_f = s_f_x,
              margin = margin,
              quadrature_size = quadrature_size,
              tol = tol
            )
          }else{
            solve_s_w_tj_exact(
              m_w = m_w,
              m_f = m_f,
              beta_w = beta_w,
              lambda_w = lambda_w,
              x_a_w = x_a_w,
              size_w = size_w,
              size_f = size_f,
              mu = mu,
              ea_w = ea_w,
              w = w_x,
              s_f = s_f_x
            )
          }
        s_w_x <- as.matrix(s_w_x)
        
        profit_x <-
          compute_profit_ths_tj(
            m_w = m_w,
            m_f = m_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_x,
            f = f_x,
            s_w = s_w_x,
            s_f = s_f_x,
            use_exp = use_exp
          ) %>%
          as.matrix()
        
        # Free-entry equations set signed profits to zero.
        eq_entry <-
          profit_x[
            3:nrow(profit_x), 
            , 
            drop = FALSE
          ]
        return(eq_entry)
      }
    
    solution <-
      nleqslv::nleqslv(
        x = x_base,
        fn = fn,
        control =
          list(
            allowSingular = TRUE
          )
      )
    objective <- max(abs(solution$fvec))
    
    f[3:nrow(f), 1] <- -exp(as.numeric(solution$x))
    
    return(
      list(
        w = w,
        f = f,
        objective = objective
      )
    )
  }

solve_equilibrium_competitive_tj <-
  function(
    t,
    j,
    equilibrium,
    solver
  ) {
    if (solver == "nleqslv"){
      w_f <-
        solve_w_f_competitive_nleqslv_tj(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          beta_f = equilibrium$parameter$beta_f,
          gamma_w = equilibrium$parameter$gamma_w,
          gamma_f = equilibrium$parameter$gamma_f,
          lambda_w = equilibrium$parameter$lambda_w,
          lambda_f = equilibrium$parameter$lambda_f,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
          x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
          x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
          w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          owner = equilibrium$exogenous[[t]][[j]]$owner,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          ea_f = equilibrium$shock[[t]][[j]]$ea_f,
          ec_w = equilibrium$shock[[t]][[j]]$ec_w,
          ec_f = equilibrium$shock[[t]][[j]]$ec_f,
          w = equilibrium$endogenous[[t]][[j]]$w,
          f = equilibrium$endogenous[[t]][[j]]$f,
          s_f = equilibrium$endogenous[[t]][[j]]$s_f,
          method_s_w = equilibrium$constant$method_s_w,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol,
          use_exp = equilibrium$constant$use_exp
        )
    }
    
    s_f <-
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
        f = w_f$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      ) 
    
    if (equilibrium$constant$method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_f$w,
          s_f = s_f
        ) %>%
        as.matrix()
    } else if (equilibrium$constant$method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w_f$w,
          s_f = s_f,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
    }
    
    equilibrium$endogenous[[t]][[j]]$w <- w_f$w
    equilibrium$endogenous[[t]][[j]]$f <- w_f$f
    equilibrium$endogenous[[t]][[j]]$s_w <- s_w
    equilibrium$endogenous[[t]][[j]]$s_f <- s_f
    
    return(equilibrium)
  }

solve_equilibrium_competitive <-
  function(
    equilibrium,
    solver,
    seed
  ) {
    endogenous <-
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages =
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {
        endogenous_t <-
          foreach(
            j = seq_along(equilibrium$exogenous[[t]])
          ) %do% {
            equilibrium_tj <-
              solve_equilibrium_competitive_tj(
                t = t,
                j = j,
                equilibrium = equilibrium,
                solver = solver
              )
            endogenous_tj <- equilibrium_tj$endogenous[[t]][[j]]
            return(endogenous_tj)
          }
        return(endogenous_t)
      }
    
    for (
      t in seq_along(equilibrium$exogenous)
    ) {
      for (
        j in seq_along(equilibrium$exogenous[[t]])
      ) {
        equilibrium$endogenous[[t]][[j]]$w <- endogenous[[t]][[j]]$w
        equilibrium$endogenous[[t]][[j]]$f <- endogenous[[t]][[j]]$f
        equilibrium$endogenous[[t]][[j]]$s_f <- endogenous[[t]][[j]]$s_f
        equilibrium$endogenous[[t]][[j]]$s_w <- endogenous[[t]][[j]]$s_w
      }
    }
    
    return(equilibrium)
  }

compute_meeting_number_tj_frictionless <-
  function(
    size_w,
    size_f,
    s_w,
    s_f
  ) {
    n_f <- size_f * s_f
    n_w <- size_w * s_w
    m <- pmin(n_f, n_w)
    return(m)
  }

compute_meeting_probability_w_tj_frictionless <-
  function(
    size_w,
    size_f,
    s_w,
    s_f
  ) {
    m <-
      compute_meeting_number_tj_frictionless(
        size_w = size_w,
        size_f = size_f,
        s_w = s_w,
        s_f = s_f
      )
    p_w <- m / pmax(size_w * s_w, 1e-12)
    return(p_w)
  }

compute_meeting_probability_f_tj_frictionless <-
  function(
    size_w,
    size_f,
    s_w,
    s_f
  ) {
    m <-
      compute_meeting_number_tj_frictionless(
        size_w = size_w,
        size_f = size_f,
        s_w = s_w,
        s_f = s_f
      )
    p_f <- m / pmax(size_f * s_f, 1e-12)
    return(p_f)
  }

solve_equilibrium_frictionless_with_free_entry_tj <-
  function(
    t,
    j,
    equilibrium
  ){
    w <- equilibrium$endogenous[[t]][[j]]$w
    w[3:length(w)] <- w[2]
    c_w <- 
      compute_c_w_tj(
        gamma_w = equilibrium$parameter$gamma_w,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        use_exp = equilibrium$constant$use_exp
      ) 

    c_f <-
      compute_c_f_tj(
        gamma_f = equilibrium$parameter$gamma_f,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        use_exp = equilibrium$constant$use_exp
      ) 
    f <- w + c_w + c_f

    s_f <-
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
        f = f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f
      )

    if (equilibrium$constant$method_s_w == "approximate"){
      s_w <-
        solve_s_w_tj_approximate(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w,
          s_f = s_f,
          margin = equilibrium$constant$margin,
          quadrature_size = equilibrium$constant$quadrature_size,
          tol = equilibrium$constant$tol
        )
    }else{
      s_w <-
        solve_s_w_tj_exact(
          m_w = equilibrium$parameter$m_w,
          m_f = equilibrium$parameter$m_f,
          beta_w = equilibrium$parameter$beta_w,
          lambda_w = equilibrium$parameter$lambda_w,
          x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
          size_w = equilibrium$exogenous[[t]][[j]]$size_w,
          size_f = equilibrium$exogenous[[t]][[j]]$size_f,
          mu = equilibrium$shock[[t]][[j]]$mu,
          ea_w = equilibrium$shock[[t]][[j]]$ea_w,
          w = w,
          s_f = s_f
        )
    }

    equilibrium$endogenous[[t]][[j]]$w <- w
    equilibrium$endogenous[[t]][[j]]$f <- f
    equilibrium$endogenous[[t]][[j]]$s_w <- s_w
    equilibrium$endogenous[[t]][[j]]$s_f <- s_f
    return(equilibrium)
  }

solve_equilibrium_frictionless_with_free_entry <-
  function(
    equilibrium
  ) {
    endogenous <-
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages =
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {
        endogenous_t <-
          foreach(
            j = seq_along(equilibrium$exogenous[[t]])
          ) %do% {
            equilibrium_tj <-
              solve_equilibrium_frictionless_with_free_entry_tj(
                t = t,
                j = j,
                equilibrium = equilibrium
              )
            endogenous_tj <- equilibrium_tj$endogenous[[t]][[j]]
            return(endogenous_tj)
          }
        return(endogenous_t)
      }
    
    for (
      t in seq_along(equilibrium$exogenous)
    ) {
      for (
        j in seq_along(equilibrium$exogenous[[t]])
      ) {
        equilibrium$endogenous[[t]][[j]]$w <- endogenous[[t]][[j]]$w
        equilibrium$endogenous[[t]][[j]]$f <- endogenous[[t]][[j]]$f
        equilibrium$endogenous[[t]][[j]]$s_f <- endogenous[[t]][[j]]$s_f
        equilibrium$endogenous[[t]][[j]]$s_w <- endogenous[[t]][[j]]$s_w
      }
    }
    
    return(equilibrium)
  }

compute_foc_f_tj_frictionless <-
  function(
    m_w,
    m_f,
    owner,
    c_w,
    c_f,
    size_w,
    size_f,
    mu,
    w,
    f,
    s_w,
    s_f,
    s_w_d_f,
    s_f_d_f
  ) {
    # under frictionless condition max instead of cobb-douglas
    meeting_number <- 
      compute_meeting_number_tj_frictionless(
        size_w = size_w,
        size_f = size_f,
        s_w = s_w,
        s_f = s_f
      )
    
    o <- t(owner) %*% owner
    
    # meeting_number_d_f is zero for long-sided worker
    foc_f <-
      meeting_number * s_w +
      (o * s_w_d_f) %*% (meeting_number * (f - w - c_w)) -
      (o * s_f_d_f) %*% c_f * size_f
    
    return(foc_f)
  }

compute_foc_tj_frictionless <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp
  ) {
    
    c_w <- 
      compute_c_w_tj(
        gamma_w = gamma_w,
        x_c_w = x_c_w,
        ec_w = ec_w,
        use_exp = use_exp
      ) 
    
    c_f <-
      compute_c_f_tj(
        gamma_f = gamma_f,
        x_c_f = x_c_f,
        ec_f = ec_f,
        use_exp = use_exp
      ) 
    
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else if (method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w,
          s_f = s_f
        )
    }
    
    s_f_d_f <- 
      solve_s_f_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = f,
        s_f = s_f
      )
    
    s_w_d_w <-
      solve_s_w_d_w_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        lambda_w = lambda_w,
        x_a_w = x_a_w,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        w = w,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      ) 
    
    s_w_d_f <-
      solve_s_w_d_f_tj(
        m_w = m_w,
        m_f = m_f,
        beta_w = beta_w,
        beta_f = beta_f,
        lambda_w = lambda_w,
        lambda_f = lambda_f,
        x_a_w = x_a_w,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_w = ea_w,
        ea_f = ea_f,
        w = w,
        f = f,
        s_f = s_f,
        method_s_w = method_s_w,
        margin = margin,
        quadrature_size = quadrature_size,
        tol = tol
      ) 
    
    foc_w <-
      compute_foc_w_tj(
        owner = owner,
        c_w = c_w,
        w = w,
        f = f,
        s_w = s_w,
        s_w_d_w = s_w_d_w
      ) 
    
    foc_f <-
      compute_foc_f_tj_frictionless(
        m_w = m_w,
        m_f = m_f,
        owner = owner,
        c_w = c_w,
        c_f = c_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        w = w,
        f = f,
        s_w = s_w,
        s_f = s_f,
        s_w_d_f = s_w_d_f,
        s_f_d_f = s_f_d_f
      ) 
    
    foc <-
      rbind(
        foc_w[3:length(foc_w), 1] %>% as.matrix(),
        foc_f[3:length(foc_f), 1] %>% as.matrix()
      )
    
    return(foc)
    
  }

solve_w_f_optim_tj_frictionless <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed
  ) {
    n_active_w <- nrow(w) - 2
    n_active_f <- nrow(f) - 2
    x_lower <- 0
    x_upper <- 50
    
    bound <- 
      list(
        lower = 
          c(
            rep(
              x_lower,
              n_active_w
            ),
            rep(
              x_lower,
              n_active_f
            )
          ),
        upper =
          c(
            rep(
              x_upper,
              n_active_w
            ),
            rep(
              x_upper,
              n_active_f
            )
          )
      )
    
    set.seed(seed)
    e <- rnorm(length(bound$lower))
    x <- 
      bound$lower +
      (bound$upper - bound$lower) * exp(e) / (1 + exp(e))
    
    fn <-
      function(x) {
        w_f <-
          transform_x_to_w_f_optim(
            x = x,
            w = w,
            f = f
          )
        foc <-
          compute_foc_tj_frictionless(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_f$w,
            f = w_f$f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        objective <- max(abs(foc))
        return(objective)
      }
    
    solution <-
      optim(
        par = x,
        fn = fn,
        method = "L-BFGS-B",
        lower = bound$lower,
        upper = bound$upper,
        control = list(trace = 1)
      )
    
    x <- solution$par
    
    w_f <-
      transform_x_to_w_f_optim(
        x = x,
        w = w,
        f = f
      )
    
    return(
      list(
        w = w_f$w %>% as.matrix(),
        f = w_f$f %>% as.matrix(),
        objective = solution$value
      )
    )
  }

solve_w_f_nleqslv_tj_frictionless <-
  function(
    m_w,
    m_f,
    beta_w,
    beta_f,
    gamma_w,
    gamma_f,
    lambda_w,
    lambda_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ea_f,
    ec_w,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    seed,
    multistart
  ) {
    n_active_w <- nrow(w) - 2
    n_active_f <- nrow(f) - 2
    x_base <-
      c(
        as.numeric(w[3:nrow(w), 1]),
        as.numeric(f[3:nrow(f), 1])
      )
    
    fn <-
      function(x) {
        x <- as.numeric(x)
        w_x <- w
        f_x <- f
        w_idx <- seq_len(n_active_w)
        f_idx <- n_active_w + seq_len(n_active_f)
        w_x[3:nrow(w_x), 1] <- x[w_idx]
        f_x[3:nrow(f_x), 1] <- x[f_idx]
        foc <-
          compute_foc_tj_frictionless(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w_x,
            f = f_x,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp
          ) 
        return(foc)
      }
    
    solution_list <-
      vector(
        mode = "list",
        length = multistart
      )
    
    objective_list <-
      rep(
        Inf,
        multistart
      )
    
    for (n in 1:multistart) {
      set.seed(seed + n - 1)
      x0 <- x_base + rnorm(length(x_base))
      
      solution_n <-
        tryCatch(
          {
            nleqslv::nleqslv(
              x = x0,
              fn = fn,
              control =
                list(
                  allowSingular = TRUE
                )
            )
          },
          error = function(e) {
            NULL
          }
        )
      
      if (!is.null(solution_n)) {
        objective_n <- max(abs(solution_n$fvec))
        solution_list[[n]] <- solution_n
        objective_list[[n]] <- objective_n
      }
    }
    
    if (all(!is.finite(objective_list))) {
      solution <-
        nleqslv::nleqslv(
          x = x_base,
          fn = fn,
          control =
            list(
              allowSingular = TRUE
            )
        )
      objective <- max(abs(solution$fvec))
    } else {
      best <- which.min(objective_list)
      solution <- solution_list[[best]]
      objective <- objective_list[[best]]
    }
    
    x <- as.numeric(solution$x)
    w_idx <- seq_len(n_active_w)
    f_idx <- n_active_w + seq_len(n_active_f)
    w[3:nrow(w), 1] <- x[w_idx]
    f[3:nrow(f), 1] <- x[f_idx]
    
    return(
      list(
        w = w,
        f = f,
        objective = objective
      )
    )
  }

solve_endogenous_tj_frictionless <-
  function(
    m_w,
    m_f,
    beta_w,
    lambda_w,
    gamma_w,
    beta_f,
    lambda_f,
    gamma_f,
    x_a_w,
    x_a_f,
    x_c_w,
    x_c_f,
    w_0,
    size_w,
    size_f,
    owner,
    mu,
    ea_w,
    ec_w,
    ea_f,
    ec_f,
    w,
    f,
    s_f,
    method_s_w,
    margin,
    quadrature_size,
    tol,
    use_exp,
    solver,
    multistart
  ) {
    # solve ths problem for wages and fees
    if (solver == "optim") {
      w_f <-
        foreach(
          n = 1:multistart,
          .packages = 
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %dopar% {
          solve_w_f_optim_tj_frictionless(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w,
            f = f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp,
            seed = 1
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
    } else if (solver == "nleqslv") {
      w_f <-
        foreach(
          n = 1:multistart,
          .packages = 
            c(
              "Dispatching",
              "foreach",
              "magrittr"
            )
        ) %dopar% {
          solve_w_f_nleqslv_tj_frictionless(
            m_w = m_w,
            m_f = m_f,
            beta_w = beta_w,
            beta_f = beta_f,
            gamma_w = gamma_w,
            gamma_f = gamma_f,
            lambda_w = lambda_w,
            lambda_f = lambda_f,
            x_a_w = x_a_w,
            x_a_f = x_a_f,
            x_c_w = x_c_w,
            x_c_f = x_c_f,
            w_0 = w_0,
            size_w = size_w,
            size_f = size_f,
            owner = owner,
            mu = mu,
            ea_w = ea_w,
            ea_f = ea_f,
            ec_w = ec_w,
            ec_f = ec_f,
            w = w,
            f = f,
            s_f = s_f,
            method_s_w = method_s_w,
            margin = margin,
            quadrature_size = quadrature_size,
            tol = tol,
            use_exp = use_exp,
            seed = 1,
            multistart = multistart
          )
        }
      best <-
        w_f %>%
        purrr::map_dbl(
          ~ .x$objective
        ) %>%
        which.min()
      w_f <- w_f[[best]]
    } else {
      stop("no solver")
    }
    
    # solve client firm shares
    s_f <-
      solve_s_f_tj_rcpp(
        m_w = m_w,
        m_f = m_f,
        beta_f = beta_f,
        lambda_f = lambda_f,
        x_a_f = x_a_f,
        size_w = size_w,
        size_f = size_f,
        mu = mu,
        ea_f = ea_f,
        f = w_f$f,
        s_f = s_f
      )
    
    # solve worker shares
    if (method_s_w == "approximate") {
      s_w <-
        solve_s_w_tj_approximate(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w_f$w,
          s_f = s_f,
          margin = margin,
          quadrature_size = quadrature_size,
          tol = tol
        )
    } else if (method_s_w == "exact") {
      s_w <-
        solve_s_w_tj_exact(
          m_w = m_w,
          m_f = m_f,
          beta_w = beta_w,
          lambda_w = lambda_w,
          x_a_w = x_a_w,
          size_w = size_w,
          size_f = size_f,
          mu = mu,
          ea_w = ea_w,
          w = w_f$w,
          s_f = s_f
        )
    }
    return(
      list(
        w = w_f$w,
        f = w_f$f,
        s_f = s_f,
        s_w = s_w
      )
    )
  }

solve_equilibrium_tj_frictionless <-
  function(
    t,
    j,
    equilibrium,
    solver,
    multistart
  ) {
    endogenous_tj <-
      solve_endogenous_tj_frictionless(
        m_w = equilibrium$parameter$m_w,
        m_f = equilibrium$parameter$m_f,
        beta_w = equilibrium$parameter$beta_w,
        lambda_w = equilibrium$parameter$lambda_w,
        gamma_w = equilibrium$parameter$gamma_w,
        beta_f = equilibrium$parameter$beta_f,
        lambda_f = equilibrium$parameter$lambda_f,
        gamma_f = equilibrium$parameter$gamma_f,
        x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
        x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
        x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
        x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
        w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
        size_w = equilibrium$exogenous[[t]][[j]]$size_w,
        size_f = equilibrium$exogenous[[t]][[j]]$size_f,
        owner = equilibrium$exogenous[[t]][[j]]$owner,
        mu = equilibrium$shock[[t]][[j]]$mu,
        ea_w = equilibrium$shock[[t]][[j]]$ea_w,
        ec_w = equilibrium$shock[[t]][[j]]$ec_w,
        ea_f = equilibrium$shock[[t]][[j]]$ea_f,
        ec_f = equilibrium$shock[[t]][[j]]$ec_f,
        w = equilibrium$endogenous[[t]][[j]]$w,
        f = equilibrium$endogenous[[t]][[j]]$f,
        s_f = equilibrium$endogenous[[t]][[j]]$s_f,
        method_s_w = equilibrium$constant$method_s_w,
        margin = equilibrium$constant$margin,
        quadrature_size = equilibrium$constant$quadrature_size,
        tol = equilibrium$constant$tol,
        use_exp = equilibrium$constant$use_exp,
        solver = solver,
        multistart = multistart
      ) 
    
    
    # update endogenous variables
    equilibrium$endogenous[[t]][[j]]$w <- endogenous_tj$w
    equilibrium$endogenous[[t]][[j]]$f <- endogenous_tj$f
    equilibrium$endogenous[[t]][[j]]$s_f <- endogenous_tj$s_f
    equilibrium$endogenous[[t]][[j]]$s_w <- endogenous_tj$s_w
    
    # return equilibrium
    return(equilibrium)
  }

solve_equilibrium_frictionless <-
  function(
    equilibrium,
    solver,
    multistart
  ) {
    endogenous <- 
      foreach(
        t = seq_along(equilibrium$exogenous),
        .packages = 
          c(
            "Dispatching",
            "foreach",
            "magrittr"
          )
      ) %dopar% {  
        endogenous_t <-
          foreach (
            j = seq_along(equilibrium$exogenous[[t]])
          ) %do% {
            endogenous_tj <-
              solve_endogenous_tj_frictionless(
                m_w = equilibrium$parameter$m_w,
                m_f = equilibrium$parameter$m_f,
                beta_w = equilibrium$parameter$beta_w,
                lambda_w = equilibrium$parameter$lambda_w,
                gamma_w = equilibrium$parameter$gamma_w,
                beta_f = equilibrium$parameter$beta_f,
                lambda_f = equilibrium$parameter$lambda_f,
                gamma_f = equilibrium$parameter$gamma_f,
                x_a_w = equilibrium$exogenous[[t]][[j]]$x_a_w,
                x_a_f = equilibrium$exogenous[[t]][[j]]$x_a_f,
                x_c_w = equilibrium$exogenous[[t]][[j]]$x_c_w,
                x_c_f = equilibrium$exogenous[[t]][[j]]$x_c_f,
                w_0 = equilibrium$exogenous[[t]][[j]]$w_0,
                size_w = equilibrium$exogenous[[t]][[j]]$size_w,
                size_f = equilibrium$exogenous[[t]][[j]]$size_f,
                owner = equilibrium$exogenous[[t]][[j]]$owner,
                mu = equilibrium$shock[[t]][[j]]$mu,
                ea_w = equilibrium$shock[[t]][[j]]$ea_w,
                ec_w = equilibrium$shock[[t]][[j]]$ec_w,
                ea_f = equilibrium$shock[[t]][[j]]$ea_f,
                ec_f = equilibrium$shock[[t]][[j]]$ec_f,
                w = equilibrium$endogenous[[t]][[j]]$w,
                f = equilibrium$endogenous[[t]][[j]]$f,
                s_f = equilibrium$endogenous[[t]][[j]]$s_f,
                method_s_w = equilibrium$constant$method_s_w,
                margin = equilibrium$constant$margin,
                quadrature_size = equilibrium$constant$quadrature_size,
                tol = equilibrium$constant$tol,
                use_exp = equilibrium$constant$use_exp,
                solver = solver,
                multistart = multistart
              )
            return(endogenous_tj)
          }
        return(endogenous_t)
      }
    for (
      t in seq_along(equilibrium$exogenous)
    ) {
      for (
        j in seq_along(equilibrium$exogenous[[t]])
      ) {
        equilibrium$endogenous[[t]][[j]]$w <- endogenous[[t]][[j]]$w
        equilibrium$endogenous[[t]][[j]]$f <- endogenous[[t]][[j]]$f
        equilibrium$endogenous[[t]][[j]]$s_f <- endogenous[[t]][[j]]$s_f
        equilibrium$endogenous[[t]][[j]]$s_w <- endogenous[[t]][[j]]$s_w
      }
    }
    return(equilibrium)
  }