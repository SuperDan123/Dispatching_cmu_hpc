transform_data_estimation <-
  function(
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
  ) {
    # Join data
    data_estimation <- 
      data_establishment %>%
      dplyr::left_join(num_parttemp, by = c("year", "cz")) %>%
      dplyr::left_join(num_est, by = c("year", "cz")) %>%
      dplyr::left_join(distance_iv, by = c("id_unique")) %>%
      dplyr::left_join(distance_iv_each, by = c("id_unique")) %>%
      dplyr::left_join(rivals_iv, by = c("id_unique")) %>%
      dplyr::left_join(hausman_iv, by = c("id_unique")) %>%
      dplyr::left_join(partwage_cz, by = c("year", "cz")) %>%
      dplyr::left_join(mw, by = c("year", "pref")) 
    
    # Add variables
    data_estimation <-
      data_estimation %>%
      dplyr::arrange(
        year_cz, 
        firm_id, 
        id_unique
      ) %>%
      dplyr::mutate(
        owner_id = firm_id,
        Q = (tempfixed + tempperm + tempdaily),
        D_W = register,
        D_F = client,
        S_W = num_parttemp,
        S_F = num_parttemp,
        s_W = D_W / S_W,
        s_F = D_F / S_F,
        mu = exp(log(Q) - matching_params[1] * log(D_W) - matching_params[2] * log(D_F))
      ) %>%
      dplyr::mutate(
        wage_net = wage - ptwage,
        fee_net = fee - ptwage,
        log_wage_net = log(wage / ptwage),
        log_fee_net = log(fee / ptwage),
        s_W_adjusted = s_W / (mu ^ (2 - (1 - matching_params[1] - matching_params[2]) / (2 - matching_params[1] - matching_params[2]))),
        s_F_adjusted = s_F / (mu ^ (2 - (1 - matching_params[1] - matching_params[2]) / (2 - matching_params[1] - matching_params[2])))
      ) %>% 
      dplyr::group_by(cz, year) %>% 
      dplyr::mutate(
        y_W = log(s_W_adjusted / (1 - sum(s_W))),
        y_F = log(s_F_adjusted / (1 - sum(s_F))),
      ) %>% 
      dplyr::ungroup()
    
    # # Exclude the largest 5 commuting zones so that recovering MC is feasible
    # large_markets <- 
    #   data_estimation %>%
    #   dplyr::group_by(cz) %>%
    #   dplyr::summarise(N = dplyr::n()) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::arrange(-N)
    # 
    # large_markets <- large_markets$cz[1:5]
    # 
    # data_estimation <- 
    #   data_estimation %>%
    #   dplyr::filter(!(cz %in% large_markets))
    
    
    # Select variables
    data_estimation <- 
      data_estimation %>%
      dplyr::select(
        id_unique,
        establishment_id,
        firm_id,
        owner_id,
        pref,
        cz,
        year,
        year_cz,
        dplyr::starts_with("d_mean_"),
        dplyr::starts_with("d_sd_"),
        dplyr::starts_with("dist_"),
        dplyr::starts_with("num_rivals"),
        wage_iv,
        fee_iv,
        Q,
        s_W,
        s_F,
        D_W,
        D_F,
        S_W,
        S_F,
        mw,
        wage,
        fee,
        daily,
        dplyr::starts_with("type_"),
        shokai,
        oversea,
        cocurrent,
        ptwage,
        mu,
        wage_net,
        fee_net,
        log_wage_net,
        log_fee_net,
        s_W_adjusted,
        s_F_adjusted,
        y_W,
        y_F
      ) %>%
      dplyr::filter(Q > 0, D_W > 0, D_F > 0) %>%
      na.omit() %>%
      as.data.frame()
    return(data_estimation)
  }

make_names_estimation <-
  function(
    data_estimation,
    iv_selected
  ) {
    # Make names
    x_names <- 
      data_estimation %>%
      dplyr::select(
        daily,
        shokai,
        oversea,
        cocurrent,
        dplyr::starts_with("type_")
      ) %>%
      colnames()
    
    fe_names <- 
      data_estimation %>%
      dplyr::select(
        year,
        cz,
        firm_id
      ) %>%
      colnames()
    
    iv_dist_names <- 
      data_estimation %>% 
      dplyr::select(
        dist_mean,
        dist_sd
      ) %>% 
      colnames()
    
    iv_dist_each_names <- 
      data_estimation %>%
      dplyr::select(
        dplyr::starts_with("d_mean_"),
        dplyr::starts_with("d_sd_")
      ) %>%
      colnames()
    
    iv_dist_each_no_sd_names <- 
      data_estimation %>%
      dplyr::select(
        dplyr::starts_with("d_mean_")
      ) %>%
      colnames()
    
    iv_dist_selected_names <- iv_selected$vars
    
    iv_rivals_names <- 
      data_estimation %>%
      dplyr::select(
        dplyr::starts_with("num_rivals")
      ) %>%
      colnames()
    
    iv_hausman_names <- 
      data_estimation %>%
      dplyr::select(
        wage_iv,
        fee_iv
      ) %>%
      colnames()
    
    return(
      list(
        x_names = x_names,
        fe_names = fe_names,
        iv_dist_names = iv_dist_names,
        iv_dist_each_names = iv_dist_each_names,
        iv_dist_each_no_sd_names = iv_dist_each_no_sd_names,
        iv_dist_selected_names = iv_dist_selected_names,
        iv_rivals_names = iv_rivals_names,
        iv_hausman_names = iv_hausman_names
      )
    )
  }


estimate_matching_params <-
  function(
    data_estimation,
    var_names,
    baseline
  ) {
    # make the baseline for iv estimation
    baseline_iv <- 
      baseline %>%
      gsub(
        "log\\(D_W\\) \\+ log\\(D_F\\)",
        "1",
        .
      )
    
    # make formulas
    fml <-
      var_names[grep("iv_", names(var_names))] %>%
      purrr::map(
        ~ paste("(log(D_W) | log(D_F) ~ ", paste(., collapse = " + "), ")", sep = "")
      ) %>%
      purrr::map(
        ~ gsub(
          "0",
          .,
          baseline_iv
        )
      )
    
    fml <-
      c(
        baseline,
        fml
      )
    
    # estimate
    result <-
      fml %>%
      purrr::map(
        .,
        ~ lfe::felm(
          formula = as.formula(.),
          data = data_estimation
        ) 
      )
    
    return(result)
  }


conduct_wald_test_matching_params <-
  function(
    result
  ) {
    # Wald test
    L1 <- matrix(c(1, 1), ncol = 2, nrow = 1)    # CRS
    L2 <- matrix(c(1, -1), ncol = 2, nrow = 1)   # alpha = beta
    H0_1 <- c(1)
    H0_2 <- c(0)
    
    test_crs <-
      result %>%
      purrr::map(
        ~ aod::wald.test(
          Sigma = .$clustervcv, 
          b = .$coefficients, 
          L = L1, 
          H0 = H0_1)$result$chi2["P"]
      )
    
    test_equality <-
      result %>%
      purrr::map(
        ~ aod::wald.test(
          Sigma = .$clustervcv, 
          b = .$coefficients, 
          L = L2, 
          H0 = H0_2)$result$chi2["P"]
      )
    
    return(
      list(
        test_crs = test_crs,
        test_equality = test_equality
      )
    )
  }

estimate_matching_params_wrapper <-
  function(
    data_estimation,
    var_names,
    baseline
  ) {
    result <-
      estimate_matching_params(
        data_estimation = data_estimation,
        var_names = var_names,
        baseline = baseline
      )
    
    wald_test <- conduct_wald_test_matching_params(result) 
    
    # add coef names
    coef_name <- c("Log Workers", "Log Client Firms")
    for (i in 1:length(result)) {
      rownames(result[[i]]$coefficients) <- coef_name
      rownames(result[[i]]$beta) <- coef_name
      names(result[[i]]$cse) <- coef_name
      names(result[[i]]$ctval) <- coef_name
      names(result[[i]]$cpval) <- coef_name
      result[[i]]$endovars <- coef_name
      result[[i]]$pval_crs <- wald_test$test_crs[i]
      result[[i]]$pval_equality <- wald_test$test_equality[i]
    }
    
    return(result)
  }












make_equilibrium_estimation <- 
  function(
    data_estimation,
    var_names,
    matching_params,
    spec 
  ) {
    equilibrium <- equilibriumClass$new(data = data_estimation, var_names = var_names, spec = spec)
    equilibrium$generate_data_list()
    equilibrium$construct_ownership_matrix()
    equilibrium$parameters$matching_params <- matching_params
    equilibrium$parameters$rp <- 0
    return(equilibrium)
  }

partial_out_fe <-
  function(
    vars,
    var_names,
    data_estimation
  ) {
    fe_vars <- 
      var_names$fe_names %>% 
      paste(sep = "", collapse = " + ")
    
    partial_out <- 
      foreach (
        i = 1:length(vars),
        .combine = "cbind"
      ) %do% {
        v <- vars[i]
        fml <- paste(v, " ~ 1 | ", fe_vars)
        data_v <- 
          fixest::feols(fml = as.formula(fml), data = data_estimation)$residuals %>%
          as.data.frame() %>%
          magrittr::set_colnames(v)
        return(data_v)
      }
    
    return(partial_out)
  }

partial_out_fe_lm <-
  function(
    vars,
    var_names,
    data_estimation
  ) {
    fe_vars <- 
      var_names$fe_names %>% 
      paste0("factor(", ., ")") %>%
      paste(sep = "", collapse = " + ")
    
    partial_out <- 
      foreach (
        i = 1:length(vars),
        .combine = "cbind"
      ) %do% {
        v <- vars[i]
        fml <- paste(v, " ~ -1 + ", fe_vars)
        data_v <- 
          lm(formula = as.formula(fml), data = data_estimation)$residuals %>%
          as.data.frame() %>%
          magrittr::set_colnames(v) 
        return(data_v)
      } 

    return(partial_out)
  }

estimate_fixed <-
  function(
    vars,
    var_names,
    data_estimation
  ) {
    fe_vars <- 
      var_names$fe_names %>% 
      paste0("factor(", ., ")") %>%
      paste(sep = "", collapse = " + ")
    
    fixed <- 
      foreach (
        i = 1:length(vars)
      ) %do% {
        v <- vars[i]
        fml <- paste(v, " ~ -1 + ", fe_vars)
        data_v <- 
          lm(formula = as.formula(fml), data = data_estimation)
        return(data_v)
      }    
    
    coefficient <-
      fixed %>%
      purrr::map(
        .,
        ~ .$coefficients %>%
          as.data.frame()
      ) %>%
      purrr::reduce(cbind) %>%
      magrittr::set_colnames(vars)
    
    predicted <-
      fixed %>%
      purrr::map(
        .,
        ~ .$fitted.values %>%
          as.data.frame()
      ) %>%
      purrr::reduce(cbind) %>%
      magrittr::set_colnames(vars)
    
    return(
      list(
        coefficient = coefficient,
        predicted = predicted
      )
    )
  }

make_exogenous_matrix <-
  function(
    equilibrium
  ) {
    var_names <- equilibrium$var_names
    vars <- 
      c(
        var_names$x_names, 
        var_names$iv_names
      )
    
    fe_vars <- 
      var_names$fe_names %>% 
      paste(sep = "", collapse = " + ")
    
    xz_partial_out <-
      partial_out_fe(
        vars,
        var_names,
        data_estimation = equilibrium$dataframe
      ) 
    
    x <- xz_partial_out[, var_names$x_names] %>% as.matrix()
    z <- xz_partial_out %>% as.matrix()
    
    return(
      list(
        x = x,
        z = z
      )
    )
  }

make_design_matrix_foc <-
  function(
    exogenous_matrix,
    W
  ) {
    num <- ncol(W) / ncol(exogenous_matrix$z)
    
    X <- 
      exogenous_matrix$x %>%
      replicate(num, ., simplify = FALSE) %>%
      Matrix::bdiag() %>%
      as.matrix()
    Z <- 
      exogenous_matrix$z %>%
      replicate(num, ., simplify = FALSE) %>%
      Matrix::bdiag() %>%
      as.matrix()
    A <- crossprod(X, Z) %*% solve(W, crossprod(Z, X))
    B <- crossprod(X, Z) %*% solve(W, t(Z))
    P <- solve(A, B)
    return(
      list(
        X = X,
        Z = Z,
        W = W,
        P = P
      )
    )
  }

make_y_demand_raw <-
  function(
    lambda,
    equilibrium
  ) {
    data_estimation <- equilibrium$dataframe
    beta <- convert_parameters_from_structural_to_reduced(lambda, equilibrium$parameters$matching_params)
    beta_DW <- beta[1]
    beta_SW <- beta[2]
    beta_DF <- beta[3]
    beta_SF <- beta[4]
    
    if (equilibrium$spec == "linear") {
      data_estimation$y_W <- data_estimation$y_W - beta_DW * data_estimation$wage_net + beta_DF * data_estimation$fee_net
      data_estimation$y_F <- data_estimation$y_F - beta_SW * data_estimation$wage_net + beta_SF * data_estimation$fee_net    
    } else if (equilibrium$spec == "log") {
      data_estimation$y_W <- data_estimation$y_W - beta_DW * data_estimation$log_wage_net + beta_DF * data_estimation$log_fee_net
      data_estimation$y_F <- data_estimation$y_F - beta_SW * data_estimation$log_wage_net + beta_SF * data_estimation$log_fee_net 
    } else if (equilibrium$spec == "log-linear") {
      data_estimation$y_W <- data_estimation$y_W - beta_DW * data_estimation$log_wage_net + beta_DF * data_estimation$fee_net
      data_estimation$y_F <- data_estimation$y_F - beta_SW * data_estimation$log_wage_net + beta_SF * data_estimation$fee_net    
    } else {
      stop("spec must be either linear, log, or log-linear")
    }
    return(data_estimation)
  }


make_y_demand <-
  function(
    lambda,
    equilibrium
  ) {
    
    vars <- c("y_W", "y_F")
    
    data_estimation <-
      make_y_demand_raw(
        lambda,
        equilibrium
      )
    
    y_partial_out <-
      partial_out_fe(
        vars,
        var_names = equilibrium$var_names,
        data_estimation = data_estimation
      ) 
    
    return(
      list(
        y_W = y_partial_out[, "y_W", drop = FALSE] %>% as.matrix(),
        y_F = y_partial_out[, "y_F", drop = FALSE] %>% as.matrix(),
        y_W_raw = data_estimation[, "y_W", drop = FALSE] %>% as.matrix(),
        y_F_raw = data_estimation[, "y_F", drop = FALSE] %>% as.matrix()
      )
    )
  }

make_y_supply_raw <-
  function(
    lambda,
    equilibrium
  ) {
    mc <- 
      compute_marginal_cost_for_estimation(
        ownership = equilibrium$ownership, 
        exogenous = equilibrium$exogenous, 
        endogenous = equilibrium$endogenous, 
        lambda, 
        matching_params = equilibrium$parameters$matching_params, 
        spec = equilibrium$spec, 
        rp = equilibrium$parameters$rp, 
        cpp = TRUE
      )
    mc <- dplyr::bind_rows(mc) 
    data_estimation <-
      mc %>%
      dplyr::left_join(
        equilibrium$dataframe,
        by = "id_unique"
      )
    return(data_estimation)
  }

make_y_supply <-
  function(
    lambda,
    equilibrium
  ) {
    vars <- c("mc_W", "mc_F")
    
    data_estimation <-
      make_y_supply_raw(
        lambda,
        equilibrium
      )
    
    mc_partial_out <-
      partial_out_fe(
        vars,
        var_names = equilibrium$var_names,
        data_estimation = data_estimation
      ) 
    
    return(
      list(
        mc_W = mc_partial_out[, "mc_W", drop = FALSE] %>% as.matrix(),
        mc_F = mc_partial_out[, "mc_F", drop = FALSE] %>% as.matrix(),
        mc_W_raw = data_estimation[, "mc_W"] %>% as.matrix(),
        mc_F_raw = data_estimation[, "mc_F"] %>% as.matrix()
      )
    )
  }

compute_residual_foc <-
  function(
    y_demand,
    y_supply,
    design_matrix
  ) {
    y <-
      rbind(
        y_demand[c("y_W", "y_F")] %>%
          purrr::reduce(rbind),
        y_supply[c("mc_W", "mc_F")] %>%
          purrr::reduce(rbind)
      )
    gamma_reduced <- design_matrix$P %*% y
    predicted <-  design_matrix$X %*% gamma_reduced
    residual <- y - predicted
    return(
      list(
        gamma_reduced = gamma_reduced,
        predicted = predicted,
        residual = residual
      )
    )
  }


compute_marginal_cost_for_each_market <- 
  function(
    q, 
    ownership, 
    price, 
    s, 
    S, 
    lambda, 
    matching_params, 
    spec, 
    rp
  ) {
    N <- price %>% nrow()
    derivatives <-
      compute_derivatives(
        q,
        price,
        s,
        S,
        lambda,
        matching_params,
        spec
      ) 
    
    A <- matrix(0, nrow = N * 2, ncol = N * 2)
    b <- matrix(0, nrow = N * 2, ncol = 1)
    
    A[1:N, 1:N] <- ownership * derivatives$dDw_dw + rp * diag(N) # add a ridge param
    A[1:N, (N + 1):(2 * N)] <- ownership * derivatives$dDf_dw
    A[(N + 1):(2 * N), 1:N] <- ownership * derivatives$dDw_df
    A[(N + 1):(2 * N), (N + 1):(2 * N)] <- ownership * derivatives$dDf_df - rp * diag(N) # add a ridge param (note that dDf_df < 0)
    
    b[1:N, 1] <- - q + (ownership * derivatives$dq_dw) %*% (price[, 2] - price[, 1])
    b[(N + 1):(2 * N), 1] <- q + (ownership * derivatives$dq_df) %*% (price[, 2] - price[, 1])
    
    mc <- solve(A, b)
    
    mc_W <- mc[1:N, 1]
    mc_F <- mc[(N + 1):(2 * N), 1]
    
    mc <- data.frame(mc_W = mc_W, mc_F = mc_F)
    
    return(mc)
  }

compute_marginal_cost_for_estimation <- 
  function(
    ownership,
    exogenous,
    endogenous,
    lambda,
    matching_params,
    spec,
    rp, 
    cpp = TRUE
  ) {
    
    if (cpp == TRUE) {
      compute_marginal_cost <- compute_marginal_cost_for_each_market_cpp
    } else {
      compute_marginal_cost <- compute_marginal_cost_for_each_market
    }
    
    mc <- 
      foreach(
        i = 1:length(endogenous), 
        .inorder = FALSE
      ) %do% {
        mc <- 
          compute_marginal_cost(
            q = endogenous[[i]][, "Q", drop = FALSE], 
            ownership = ownership[[i]], 
            price = endogenous[[i]][, c("wage", "fee"), drop = FALSE], 
            s = endogenous[[i]][, c("s_W", "s_F"), drop = FALSE], 
            S = exogenous[[i]][1, c("S_W", "S_F")], 
            lambda = lambda, 
            matching_params = matching_params, 
            spec = spec, 
            rp = rp
          )
        
        mc_W <- mc[, 1]
        mc_F <- mc[, 2]
        
        mc <- data.frame(id_unique = exogenous[[i]][, "id_unique"], mc_W = mc_W, mc_F = mc_F)
        rownames(mc) <- NULL
        return(mc)
      }
    
    return(mc)
  }



compute_gmm_moment_foc <-
  function(
    lambda,
    equilibrium,
    design_matrix
  ) {
    y_demand <-
      make_y_demand(
        lambda = lambda,
        equilibrium = equilibrium
      ) 
    y_supply <-
      make_y_supply(
        lambda = lambda,
        equilibrium = equilibrium
      ) 
    residual <-
      compute_residual_foc(
        y_demand,
        y_supply,
        design_matrix
      ) 
    moment <- 
      residual$residual %*% matrix(rep(1, ncol(design_matrix$Z)), nrow = 1) * 
      design_matrix$Z
    return(moment)
  }

compute_gmm_objective_generic <-
  function(
    theta,
    equilibrium,
    design_matrix,
    moment_function
  ) {
    lambda <- theta
    moment <-
      moment_function(
        lambda = lambda,
        equilibrium = equilibrium,
        design_matrix = design_matrix
      ) 
    n <- nrow(moment) / 4
    moment <- 
      apply(moment, 2, sum) %>%
      matrix(nrow = 1)
    moment <- moment / n
    objective <- n * moment %*% solve(design_matrix$W, t(moment))
    return(objective)
  }

estimate_gmm_nonlinear_generic <-
  function(
    theta,
    equilibrium,
    W,
    design_function,
    moment_function
  ) {
    exogenous_matrix <-
      make_exogenous_matrix(
        equilibrium = equilibrium
      ) 
    design_matrix <-
      design_function(
        exogenous_matrix = exogenous_matrix,
        W = W
      ) 
    result <-
      optim(
        par = theta,
        fn = compute_gmm_objective_generic,
        control = list(
          trace = 3
        ),
        method = "Nelder-Mead",
        equilibrium = equilibrium,
        design_matrix = design_matrix,
        moment_function = moment_function
      )
    return(result)
  }

compute_efficient_weight_foc <-
  function(
    lambda,
    equilibrium,
    W
  ) {
    exogenous_matrix <-
      make_exogenous_matrix(
        equilibrium = equilibrium
      ) 
    design_matrix <-
      make_design_matrix_foc(
        exogenous_matrix = exogenous_matrix,
        W = W
      ) 
    moment <-
      compute_gmm_moment_foc(
        lambda = lambda,
        equilibrium = equilibrium,
        design_matrix = design_matrix
      )
    n <- nrow(moment) / 4
    moment_mean <- 
      apply(moment, 2, sum) %>%
      matrix(nrow = 1)
    moment_mean <- moment_mean / n
    omega <- crossprod(moment, moment) / n - crossprod(moment_mean, moment_mean)
    
    return(omega)
  }

convert_reducecd_to_structural <-
  function(
    reduced,
    lambda, 
    matching_params
  ) {
    header <-
      reduced %>%
      dplyr::select(-dplyr::contains(c("a_W", "a_F", "mc_W", "mc_F")))
    structural_s <- 
      reduced %>%
      dplyr::select(dplyr::contains(c("mc_W", "mc_F")))
    reduced_d <- 
      reduced %>%
      dplyr::select(dplyr::contains(c("a_W", "a_F"))) %>%
      as.matrix()
    
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    B <- matrix(beta, nrow = 2)
    
    if (max(grepl("se", colnames(reduced))) == 1) {
      structural_d <-
        foreach (
          i = 1:nrow(reduced_d),
          .combine = "rbind"
        ) %do% {
          row_i <- reduced_d[i, ]
          var_i <- diag(row_i^2)
          var_i <- solve(B, var_i) %*% t(solve(B))
          se_i <- 
            diag(var_i) %>% 
            sqrt()
          return(se_i)
        } %>%
        magrittr::set_colnames(colnames(reduced_d)) %>%
        tibble::as_tibble()
    } else {
      
      structural_d <- 
        solve(B, t(reduced_d)) %>% 
        t() %>%
        magrittr::set_colnames(colnames(reduced_d)) %>%
        tibble::as_tibble()
    }
    structural <-
      tibble::tibble(
        header,
        structural_d,
        structural_s
      )
    return(structural)
  }

convert_structural_to_reduced <-
  function(
    structural,
    lambda, 
    matching_params
  ) {
    header <-
      structural %>%
      dplyr::select(-dplyr::contains(c("a_W", "a_F", "mc_W", "mc_F")))
    reduced_s <- 
      structural %>%
      dplyr::select(dplyr::contains(c("mc_W", "mc_F")))
    structural_d <- 
      structural %>%
      dplyr::select(dplyr::contains(c("a_W", "a_F"))) %>%
      as.matrix()
    
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    B <- matrix(beta, nrow = 2)
    
    if (max(grepl("se", colnames(structural))) == 1) {
      reduced_d <-
        foreach (
          i = 1:nrow(structural_d),
          .combine = "rbind"
        ) %do% {
          row_i <- structural_d[i, ]
          var_i <- diag(row_i^2)
          var_i <- B %*% var_i %*% t(B)
          se_i <- 
            diag(var_i) %>% 
            sqrt()
          return(se_i)
        } %>%
        magrittr::set_colnames(colnames(structural_d)) %>%
        tibble::as_tibble()
    } else {
      reduced_d <- 
        B %*% t(structural_d) %>% 
        t() %>%
        magrittr::set_colnames(colnames(structural_d)) %>%
        tibble::as_tibble()
    }
    reduced <-
      tibble::tibble(
        header,
        reduced_d,
        reduced_s
      )
    return(reduced)
  }


estimate_gmm_se_nonlinear_efficient_foc <-
  function(
    lambda,
    equilibrium,
    design_matrix
  ) {
    n <- nrow(design_matrix$Z) / 4
    g <-
      function(
    lambda
      ) {
        moment <-
          compute_gmm_moment_foc(
            lambda = lambda,
            equilibrium = equilibrium,
            design_matrix = design_matrix
          ) 
        moment_mean <- 
          apply(moment, 2, sum) %>%
          matrix(nrow = 1)
        moment_mean <- moment_mean / n
        return(moment_mean)
      }
    moment_mean_jacobian <-
      numDeriv::jacobian(
        func = g,
        x = lambda
      )
    variance <- 
      crossprod(
        moment_mean_jacobian,
        solve(
          design_matrix$W,
          moment_mean_jacobian
        )
      ) %>%
      solve()
    se <-
      variance %>%
      diag() %>%
      sqrt()
    se <- se / sqrt(n)
    se <-
      matrix(se, nrow = 1) %>%
      magrittr::set_colnames(c("lambda_W", "lambda_F"))
    return(se)
  }

make_formula_other_reduced <-
  function(
    var_names,
    data_estimation
  ) {
    x_vars <-
      var_names$x_names %>%
      paste(collapse = " + ")
    fe_vars <- 
      var_names$fe_names %>% 
      paste0("factor(", ., ")") %>%
      paste(collapse = " + ")
    fml <- paste(" ~ -1", x_vars, fe_vars, sep = " + ")
    return(fml)
  }

estimate_other_reduced <-
  function(
    vars,
    var_names,
    data_estimation
  ) {
    fml <-
      make_formula_other_reduced(
        var_names,
        data_estimation
      ) 
    
    other_reduced <- 
      foreach (
        i = 1:length(vars)
      ) %do% {
        v <- vars[i]
        data_v <- 
          lm(
            formula = 
              paste(
                v,
                fml,
                sep = ""
              ) %>%
              as.formula(),
            data = data_estimation
          )
        return(data_v)
      }    
    
    coefficient <-
      other_reduced %>%
      purrr::map(
        .,
        ~ .$coefficients %>%
          as.data.frame()
      ) %>%
      purrr::reduce(cbind) %>%
      magrittr::set_colnames(vars)
    
    se <-
      other_reduced %>%
      purrr::map(
        .,
        ~ summary(.) %>%
          coef() %>%
          as.data.frame() %>%
          dplyr::select(`Std. Error`)
      ) %>%
      purrr::reduce(cbind) %>%
      magrittr::set_colnames(vars)
    
    predicted <-
      other_reduced %>%
      purrr::map(
        .,
        ~ .$fitted.values %>%
          as.data.frame()
      ) %>%
      purrr::reduce(cbind) %>%
      magrittr::set_colnames(vars)
    
    residual <-
      other_reduced %>%
      purrr::map(
        .,
        ~ .$residuals %>%
          as.data.frame()
      ) %>%
      purrr::reduce(cbind) %>%
      magrittr::set_colnames(vars)
    
    return(
      list(
        coefficient = coefficient,
        se = se,
        predicted = predicted,
        residual = residual
      )
    )
  }

estiamte_gmm_other_reduced_foc <-
  function(
    lambda,
    equilibrium
  ) {
    # reduced linear 
    y_demand_raw <-
      make_y_demand_raw(
        lambda = lambda,
        equilibrium = equilibrium
      ) 
    y_supply_raw <-
      make_y_supply_raw(
        lambda = lambda,
        equilibrium = equilibrium
      ) 
    other_reduced_demand <-
      estimate_other_reduced(
        vars = c("y_W", "y_F"),
        var_names = equilibrium$var_names,
        data_estimation = y_demand_raw
      )
    other_reduced_supply <-
      estimate_other_reduced(
        vars = c("mc_W", "mc_F"),
        var_names = equilibrium$var_names,
        data_estimation = y_supply_raw
      )
    coefficient <-
      tibble::tibble(
        term = rownames(other_reduced_demand$coefficient),
        other_reduced_demand$coefficient,
        other_reduced_supply$coefficient
      ) %>%
      dplyr::rename(
        coefficient_a_W = y_W,
        coefficient_a_F = y_F,
        coefficient_mc_W = mc_W,
        coefficient_mc_F = mc_F
      )
    se <-
      tibble::tibble(
        term = rownames(other_reduced_demand$coefficient),
        other_reduced_demand$se,
        other_reduced_supply$se
      ) %>%
      dplyr::rename(
        se_a_W = y_W,
        se_a_F = y_F,
        se_mc_W = mc_W,
        se_mc_F = mc_F
      )
    predicted <-
      tibble::tibble(
        id_unique = equilibrium$dataframe$id_unique,
        other_reduced_demand$predicted,
        other_reduced_supply$predicted
      ) %>%
      dplyr::rename(
        predicted_a_W = y_W,
        predicted_a_F = y_F,
        predicted_mc_W = mc_W,
        predicted_mc_F = mc_F
      )
    residual <-
      tibble::tibble(
        id_unique = equilibrium$dataframe$id_unique,
        other_reduced_demand$residual,
        other_reduced_supply$residual
      ) %>%
      dplyr::rename(
        residual_a_W = y_W,
        residual_a_F = y_F,
        residual_mc_W = mc_W,
        residual_mc_F = mc_F
      )
    other_reduced <-
      list(
        coefficient = coefficient,
        se = se,
        predicted = predicted,
        residual = residual
      )
    return(other_reduced)
  }



estimate_gmm_foc <-
  function(
    lambda,
    equilibrium
  ) {
    # set theta
    theta <- lambda
    
    # number of instruments
    K <- 
      length(equilibrium$var_names$x_names) +
      length(equilibrium$var_names$iv_names)
    
    # make initial weighting matrix
    W <-
      diag(
        rep(
          1,
          4 * K
        )
      )
    
    # estimate the first stage gmm estimator
    result <-
      estimate_gmm_nonlinear_generic(
        theta = theta,
        equilibrium = equilibrium,
        W = W,
        design_function = make_design_matrix_foc,
        moment_function = compute_gmm_moment_foc
      ) 
    
    # make efficient weighting matrix
    W_efficient <-
      compute_efficient_weight_foc(
        lambda = result$par,
        equilibrium = equilibrium,
        W = W
      )
    
    # estimate the efficient gmm estimator
    result_efficient <-
      estimate_gmm_nonlinear_generic(
        theta = result$par,
        equilibrium = equilibrium,
        W = W_efficient,
        design_function = make_design_matrix_foc,
        moment_function = compute_gmm_moment_foc
      ) 
    
    # estimate other parameters
    fml <-
      make_formula_other_reduced(
        var_names = equilibrium$var_names,
        data_estimation = equilibrium$dataframe
      )
    
    other_reduced <-
      estiamte_gmm_other_reduced_foc(
        lambda = result_efficient$par,
        equilibrium = equilibrium
      ) 
    
    other <-
      other_reduced %>%
      purrr::map(
        .,
        ~ convert_reducecd_to_structural(
          reduced = .,
          lambda = result_efficient$par, 
          matching_params = equilibrium$parameters$matching_params
        ) 
      )
    
    # make matrix
    exogenous_matrix <-
      make_exogenous_matrix(
        equilibrium = equilibrium
      ) 
    
    design_matrix <-
      make_design_matrix_foc(
        exogenous_matrix = exogenous_matrix,
        W = W_efficient
      ) 
    
    se_nonlinear <-
      estimate_gmm_se_nonlinear_efficient_foc(
        lambda = result_efficient$par,
        equilibrium = equilibrium,
        design_matrix = design_matrix
      ) 
    
    # make output
    estimate_nonlinear <-
      result_efficient$par %>%
      matrix(nrow = 1) %>%
      magrittr::set_colnames(c("lambda_W", "lambda_F"))
    
    # return
    estimate <-
      list(
        result = result,
        result_efficient = result_efficient,
        estimate_nonlinear = estimate_nonlinear,
        se_nonlinear = se_nonlinear,
        other = other,
        other_reduced = other_reduced
      )
    
    return(estimate)
    
  }

update_equilibrium_by_estimate <-
  function(
    equilibrium,
    estimate
  ) {
    # parameter
    equilibrium$parameters$lambda <- estimate$estimate_nonlinear 
    equilibrium$parameters$linear <- estimate$other$coefficient
    
    # standard error
    equilibrium$parameters_se$lambda <- estimate$se_nonlinear
    equilibrium$parameters_se$linear <- estimate$other$se
    
    # residual
    equilibrium$dataframe <-
      equilibrium$dataframe %>%
      dplyr::select(-dplyr::contains(c("residual_a_", "residual_mc_"))) %>%
      dplyr::left_join(
        estimate$other$residual,
        by = "id_unique"
      )
    
    # predicted
    equilibrium$dataframe <-
      equilibrium$dataframe %>%
      dplyr::select(-dplyr::contains(c("predicted_a_", "predicted_mc_"))) %>%
      dplyr::left_join(
        estimate$other$predicted,
        by = "id_unique"
      )
    
    equilibrium$generate_data_list()
    
    
    return(equilibrium)
  }

train_model_foc <- 
  function(
    lambda,
    data_estimation,
    var_names,
    matching_params,
    spec
  ) {
    
    # create equilibrium object
    equilibrium <-
      make_equilibrium_estimation(
        data_estimation,
        var_names,
        matching_params,
        spec 
      )
    
    # estimate parameters
    estimate <-
      estimate_gmm_foc(
        lambda,
        equilibrium
      ) 
    
    # update parameters by estimate
    equilibrium <-
      update_equilibrium_by_estimate(
        equilibrium,
        estimate
      )
    
    return(equilibrium)
    
  }


compute_gmm_objective_gradient_generic <-
  function(
    theta,
    equilibrium,
    design_matrix,
    moment_function
  ) {
    f <- 
      function(theta) {
        objective <- 
          compute_gmm_objective_generic(
            theta,
            equilibrium,
            design_matrix,
            moment_function
          )
        return(objective)
      }
    gradient <-
      numDeriv::grad(
        func = f,
        x = theta
      )
    return(gradient)
  }

compute_gmm_constraint_mc_nonnegativity <-
  function(
    theta,
    equilibrium,
    design_matrix,
    moment_function
  ) {
    lambda <- theta
    mc <- 
      compute_marginal_cost_for_estimation(
        ownership = equilibrium$ownership, 
        exogenous = equilibrium$exogenous, 
        endogenous = equilibrium$endogenous, 
        lambda, 
        matching_params = equilibrium$parameters$matching_params, 
        spec = equilibrium$spec, 
        rp = equilibrium$parameters$rp, 
        cpp = TRUE
      )
    mc <- mc %>%
      purrr::reduce(rbind)
    constraint <- min(min(mc$mc_W), min(mc$mc_F))
    constraint <- - constraint
    return(constraint)
  }

compute_gmm_constraint_mc_nonnegativity_jacobian <-
  function(
    theta,
    equilibrium,
    design_matrix,
    moment_function
  ) {
    g <-
      function(theta) {
        constraint <-
          compute_gmm_constraint_mc_nonnegativity(
            theta,
            equilibrium,
            design_matrix,
            moment_function
          ) 
        return(constraint)
      }
    jacobian <-
      numDeriv::jacobian(
        func = g,
        x = theta
      )
    return(jacobian)
  }

estimate_gmm_nonlinear_mc_nonnegativity <-
  function(
    theta,
    equilibrium,
    W,
    design_function,
    moment_function
  ) {
    exogenous_matrix <-
      make_exogenous_matrix(
        equilibrium = equilibrium
      ) 
    design_matrix <-
      design_function(
        exogenous_matrix = exogenous_matrix,
        W = W
      ) 
    result <-
      nloptr::nloptr(
        x0 = theta,
        eval_f = compute_gmm_objective_generic,
        eval_grad_f = compute_gmm_objective_gradient_generic,
        eval_g_ineq = compute_gmm_constraint_mc_nonnegativity,
        eval_jac_g_ineq = compute_gmm_constraint_mc_nonnegativity_jacobian,
        lb = rep(0.1, length(theta)),
        ub = rep(75, length(theta)),
        opts = list(
          "algorithm" = "NLOPT_LD_SLSQP",
          "print_level" = 3,
          "xtol_rel" = 1e-4
        ),
        equilibrium = equilibrium,
        design_matrix = design_matrix,
        moment_function = moment_function
      )
    return(result)
  }


compute_residual_demand_foc <-
  function(
    y_demand,
    design_matrix
  ) {
    y <- 
      y_demand[c("y_W", "y_F")] %>%
      purrr::reduce(rbind)
    gamma_reduced <- design_matrix$P %*% y
    predicted <-  design_matrix$X %*% gamma_reduced
    residual <- y - predicted
    return(
      list(
        gamma_reduced = gamma_reduced,
        predicted = predicted,
        residual = residual
      )
    )
  }

compute_gmm_moment_demand_foc <-
  function(
    lambda,
    equilibrium,
    design_matrix
  ) {
    y_demand <-
      make_y_demand(
        lambda = lambda,
        equilibrium = equilibrium
      ) 
    residual <-
      compute_residual_demand_foc(
        y_demand,
        design_matrix
      ) 
    moment <- 
      residual$residual %*% matrix(rep(1, ncol(design_matrix$Z)), nrow = 1) * 
      design_matrix$Z
    return(moment)
  }
