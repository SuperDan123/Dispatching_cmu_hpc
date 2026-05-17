
# written by Katsuhiro Komatsu --------------------------------------------
compute_surplus_single_market <- 
  function(
    data, 
    parameters, 
    spec
  ) {
    
    H <- compute_aggregator(data$price, data$mu, data$a, data$w_0, parameters$lambda, parameters$matching_params, spec)
    
    WS <- data$S[1] * ((2 - parameters$matching_params[1]) * H[1] - parameters$matching_params[2] * H[2] + (-digamma(1))) / parameters$lambda[1]
    FS <- data$S[2] * ((2 - parameters$matching_params[2]) * H[2] - parameters$matching_params[1] * H[1] + (-digamma(1))) / parameters$lambda[2]
    
    return(
      c(
        WS, 
        FS
      )
    )
  }

compute_total_profit_single_market <- 
  function(
    data, 
    parameters,
    spec
    ) {
    profit <-
      compute_payoff_vec(
        data$price, 
        data$mu, 
        data$a, 
        data$mc,
        data$w_0, 
        data$S,
        parameters$lambda, 
        parameters$matching_params, 
        parameters$rp,
        spec
      )
    profit <- sum(profit)
    return(profit)
}


compute_surplus <- 
  function(
    equilibrium,
    cpp = FALSE
    ) {
    parameters <- equilibrium$parameters
    spec <- equilibrium$spec
    
    sim_id <- 1 # Actual residuals
    
    surplus_list <- 
      foreach(
        t = 1:length(equilibrium$exogenous)
        ) %do% {
          data_t <- equilibrium$extract_single_market(t, sim_id)
          simulated <- !is.na(data_t$price[1, 1])
          
          if (simulated == TRUE) {
            surplus <- compute_surplus_single_market(data_t, parameters, spec)
            profit <- compute_total_profit_single_market(data_t, parameters, spec)
            year <- equilibrium$exogenous[[t]][, "year"][1]
            cz <- equilibrium$exogenous[[t]][, "cz"][1]
            out <- 
              data.frame(
                "cz" = cz, 
                "year" = year, 
                "surplus_worker" = surplus[1],
                "surplus_firm" = surplus[2],
                "profit" = profit
                )
          } else {
            out <- NULL
          }
          
          return(out)
    }  
    return(surplus_list)
}



remove_markets_with_extreme_values <- function(simdata, truncation) {
  # simdata must be a list of endogenous variables for each market
  
  n_market <- length(simdata)
  simdata <- do.call("rbind", simdata) %>% 
    as.data.frame()
  
  wage_bound <- quantile(simdata$wage, prob = truncation, na.rm = T)
  fee_bound <- quantile(simdata$fee, prob = truncation, na.rm = T)
  
  simdata <- simdata %>% 
    dplyr::mutate(
      to_remove = dplyr::if_else(wage > wage_bound[2] | fee > wage_bound[2] | wage < wage_bound[1] | fee < fee_bound[1], 1, 0)
    ) %>% 
    dplyr::group_by(market) %>% 
    dplyr::mutate(to_remove = max(to_remove)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      wage = dplyr::if_else(to_remove == 1, NA_real_, wage),
      fee = dplyr::if_else(to_remove == 1, NA_real_, fee),
      s_W = dplyr::if_else(to_remove == 1, NA_real_, s_W),
      s_F = dplyr::if_else(to_remove == 1, NA_real_, s_F),
      Q = dplyr::if_else(to_remove == 1, NA_real_, Q),
      convergence = dplyr::if_else(to_remove == 1, NA_real_, convergence)
    ) %>% 
    dplyr::select(-to_remove)
  
  simdata_list <- list()
  
  for (i in 1:n_market) {
    
    simdata_list[[i]] <- simdata %>% 
      dplyr::filter(market == i) %>% 
      as.matrix()
    
  } 
  
  return(simdata_list)
  
}





construct_hypothetical_market_size_N <- function(equilibrium, N, seed = 1) {
  
  set.seed(seed)
  
  heterogeneity <- equilibrium$platform_heterogeneity %>% 
    do.call("rbind", .) %>% 
    as.data.frame() %>% 
    dplyr::sample_n(size = N)
  
  X <- equilibrium$exogenous %>% 
    do.call("rbind", .) %>% 
    as.data.frame() %>% 
    dplyr::select(id_unique, mu)
  
  # Extract prices just for setting initial values when solving for equilibrium
  p <- equilibrium$endogenous %>% 
    do.call("rbind", .) %>% 
    as.data.frame() %>% 
    dplyr::select(id_unique, wage, fee)
  
  data <- heterogeneity %>% 
    dplyr::left_join(X, by = "id_unique") %>% 
    dplyr::left_join(p, by = "id_unique")
  
  data_list <- list()
  data_list$a <- heterogeneity[, 2:3] %>% as.matrix()
  data_list$mc <- heterogeneity[, 4:5] %>% as.matrix()
  data_list$mu <- data[, 6] %>% as.matrix()
  data_list$S <- c(1, 1)
  data_list$w_0 <- 0.8
  data_list$id_unique <- 1:N
  data_list$price <- data[, 7:8] %>% as.matrix()
  
  return(data_list)
}



construct_hypothetical_market_nonrandom <- 
  function(
    equilibrium, 
    type = list(a_W = TRUE, a_F = FALSE, mc_W = FALSE, mc_F = FALSE),
    mc_manual = NULL
    ) {
  
  heterogeneity <- equilibrium$platform_heterogeneity %>% 
    do.call("rbind", .) %>% 
    as.data.frame()
  
  x <- equilibrium$exogenous %>% 
    do.call("rbind", .) %>% 
    as.data.frame() %>% 
    dplyr::select(id_unique, mu)
  
  mu <- x$mu %>% mean()
  
  if (type$a_W == TRUE) {
    a_W <- heterogeneity$a_W_pred
    a_W <- mean(a_W) + c(sd(a_W), -sd(a_W))
  } else {
    a_W <- heterogeneity$a_W_pred %>% mean()
  }
  
  if (type$a_F == TRUE) {
    a_F <- heterogeneity$a_F_pred
    a_F <- mean(a_F) + c(sd(a_F), -sd(a_F))
  } else {
    a_F <- heterogeneity$a_F_pred %>% mean()
  }
  
  if (type$mc_W == TRUE) {
    mc_W <- heterogeneity$mc_W_pred
    mc_W <- mean(mc_W) + c(sd(mc_W), -sd(mc_W))
  } else {
    if (is.null(mc_manual)) {
      mc_W <- 0.1
    } else {
      mc_W <- mc_manual[1]
    }
  }
  
  if (type$mc_F == TRUE) {
    mc_F <- heterogeneity$c_F_pred
    mc_F <- mean(mc_F) + c(sd(mc_F), -sd(mc_F))
  } else {
    if (is.null(mc_manual)) {
      mc_F <- 0.1
    } else {
      mc_F <- mc_manual[2]
    }
  }
  
  data <- expand.grid(a_W, a_F, mc_W, mc_F, mu)
  
  data_list <- list()
  data_list$a <- data[, 1:2] %>% as.matrix()
  data_list$mc <- data[, 3:4] %>% as.matrix()
  data_list$mu <- data[, 5] %>% as.matrix()
  data_list$S <- c(1, 1)
  data_list$w_0 <- 0.8
  data_list$id_unique <- 1:dim(data)[1]
  
  return(data_list)
}





construct_hypothetical_market_homogeneous <- function(equilibrium, N) {
  
  heterogeneity <- equilibrium$platform_heterogeneity %>% 
    do.call("rbind", .) %>% 
    as.data.frame()
  
  x <- equilibrium$exogenous %>% 
    do.call("rbind", .) %>% 
    as.data.frame() %>% 
    dplyr::select(id_unique, mu)
  
  mu <- x$mu %>% mean()
  a_W <- mean(heterogeneity$a_W_pred)
  a_F <- mean(heterogeneity$a_F_pred)
  mc_W <- mean(heterogeneity$mc_W_pred)
  mc_F <- mean(heterogeneity$mc_F_pred)
  

  
  data <- expand.grid(a_W, a_F, mc_W, mc_F, mu)
  
  data_list <- list()
  data_list$a <- pracma::repmat(c(a_W, a_F), N, 1)
  data_list$mc <- pracma::repmat(c(mc_W, mc_F), N, 1)
  data_list$mu <- rep(mu, N) %>% as.matrix()
  data_list$S <- c(1, 1)
  data_list$w_0 <- 0.8
  data_list$id_unique <- 1:N
  
  return(data_list)
}



copy_establishments <- function(equilibrium, N_copy) {
  
  
  N_markets <- length(equilibrium$exogenous)
  eq_out <- equilibrium$copy()
  
  for (i in 1:N_markets) {
    
    endogenous_i <- equilibrium$endogenous[[i]]
    exogenous_i <- equilibrium$exogenous[[i]]
    heterogeneity_i <- equilibrium$platform_heterogeneity[[i]]
    ownership_i <- equilibrium$ownership[[i]]
    shocks_i <- equilibrium$shocks[[1]][[i]]
    
    eq_out$endogenous[[i]] <- pracma::repmat(endogenous_i, N_copy, 1)
    eq_out$exogenous[[i]] <- pracma::repmat(exogenous_i, N_copy, 1)
    eq_out$platform_heterogeneity[[i]] <- pracma::repmat(heterogeneity_i, N_copy, 1)
    eq_out$shocks[[1]][[i]] <- pracma::repmat(shocks_i, N_copy, 1)
    
    colnames(eq_out$endogenous[[i]]) <- colnames(endogenous_i)
    colnames(eq_out$exogenous[[i]]) <- colnames(exogenous_i)
    colnames(eq_out$platform_heterogeneity[[i]]) <- colnames(heterogeneity_i)
    
    ownership <- matrix(0, nrow = N_copy * dim(ownership_i)[1], ncol = N_copy * dim(ownership_i)[2])
    start <- 1
    for (j in 1:N_copy) {
      end <- start + dim(ownership_i)[1] - 1
      ownership[start:end, start:end] <- ownership_i  
      start <- end + 1
    }
    eq_out$ownership[[i]] <- ownership
    
  }
  
  return(eq_out)
}



# added by Kohei Kawaguchi ------------------------------------------------


construct_hypothetical_equilibrium <-
  function(
    equilibrium,
    cpp,
    mc_manual = NULL
  ) {
    
    # make the hypothetical firms
    data <- 
      construct_hypothetical_market_duopoly(
        equilibrium = equilibrium, 
        mc_manual = mc_manual
      )
    ownership <- diag(length(data$id_unique))
    var_names <- list(
      x_names = NULL,
      iv_names = NULL,
      fe_names = NULL
    )
    
    # solve the endogenous variables
    out <- solve_equilibrium_single_market_wrapper(data, ownership, equilibrium$parameters, equilibrium$spec, cpp)
    out$year <- 1
    out$cz <- 1
    out$year_cz <- 1
    out$firm_id <- data$id_unique
    out$establishment_id <- data$id_unique
    out$ptwage <- data$w_0
    out$S_W <- data$S[1]
    out$S_F <- data$S[2]
    out$mu <- data$mu
    out$owner_id <- out$firm_id
    
    
    # define equilibrium
    eq <- equilibriumClass$new(data = out, var_names = var_names, spec = equilibrium$spec)
    eq$generate_data_list()
    eq$construct_ownership_matrix()
    
    # heterogeneity
    eq$platform_heterogeneity[[1]] <-
      cbind(
        out$id_unique,
        data$a,
        data$mc
      ) %>%
      magrittr::set_colnames(
        equilibrium$platform_heterogeneity[[1]] %>% colnames()
      )
    # shock
    eq$shocks[[1]] <-
      list(
        cbind(
          out$id_unique,
          matrix(rep(0, 8), nrow = 2)
        ) %>%
          magrittr::set_colnames(
            equilibrium$shocks[[1]][[1]] %>% colnames()
          )
      )
    # parameters
    eq$parameters$lambda <- equilibrium$parameters$lambda
    eq$parameters$matching_params <- equilibrium$parameters$matching_params
    eq$parameters$rp <- equilibrium$parameters$rp
    
    
    return(eq)
    
  }

compute_monopoly_equilibrium <-
  function(
    equilibrium,
    N_max,
    parallel,
    cpp,
    sim_id
  ) {
    
    # make monopoly data
    eq <- equilibrium$copy()
    eq$dataframe <-
      eq$dataframe %>%
      dplyr::mutate(
        owner_id = 1
      )
    eq$construct_ownership_matrix()
    eq$generate_data_list()
    
    # solve endogenous variables
    eq <- 
      solve_equilibrium_all(
        eq, 
        N_max,
        parallel, 
        cpp,
        sim_id
      )
    
    return(eq)
  }

compute_n_copy_equilibrium <-
  function(
    equilibrium,
    N_copy,
    N_max,
    parallel,
    cpp,
    sim_id
  ) {
    
    # make n opy data
    eq <- equilibrium$copy()
    eq$dataframe <-
      1:N_copy %>%
      purrr::map(
        .,
        ~ dplyr::mutate(
          eq$dataframe,
          copy_id = .
        ) 
      ) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::group_by(copy_id, owner_id) %>%
      dplyr::mutate(owner_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        id_unique_original = id_unique,
        id_unique = 1:length(id_unique)
      )
    # update ownership matrix
    eq$construct_ownership_matrix()
    # update endogenous, exogenous, and instruments list
    eq$generate_data_list()
    
    # update shocks and platform_heterogeneity
    id_unique_original_to_new <-
      eq$dataframe %>%
      dplyr::select(
        id_unique,
        id_unique_original
      )
    shocks <-
      foreach (
        i = 1:length(eq$shocks)
      ) %do% {
        shocks_i <-
          foreach (j = 1:length(eq$shocks[[i]])) %do% {
            shocks_ij <- 
              eq$shocks[[i]][[j]] %>%
              as.data.frame() %>%
              dplyr::rename(id_unique_original = id_unique)
            index_j <- 
              eq$endogenous[[j]] %>%
              as.data.frame() %>%
              dplyr::select(id_unique) %>%
              dplyr::left_join(
                eq$dataframe %>% dplyr::select(id_unique, id_unique_original),
                by = "id_unique"
              )
            shocks_ij <-
              index_j %>%
              dplyr::left_join(
                shocks_ij,
                by = "id_unique_original"
              ) %>%
              dplyr::select(-id_unique_original) %>%
              as.matrix()
            return(shocks_ij)
          }
        return(shocks_i)
      }
    platform_heterogeneity <-
      foreach (j = 1:length(eq$shocks[[i]])) %do% {
        platform_heterogeneity_j <- 
          eq$platform_heterogeneity[[j]] %>%
          as.data.frame() %>%
          dplyr::rename(id_unique_original = id_unique)
        index_j <- 
          eq$endogenous[[j]] %>%
          as.data.frame() %>%
          dplyr::select(id_unique) %>%
          dplyr::left_join(
            eq$dataframe %>% dplyr::select(id_unique, id_unique_original),
            by = "id_unique"
          )
        platform_heterogeneity_j <-
          index_j %>%
          dplyr::left_join(
            platform_heterogeneity_j,
            by = "id_unique_original"
          ) %>%
          dplyr::select(-id_unique_original) %>%
          as.matrix()
        return(platform_heterogeneity_j)
      }
    eq$shocks <- shocks
    eq$platform_heterogeneity <- platform_heterogeneity
    
    # solve endogenous variables
    eq <- 
      solve_equilibrium_all(
        eq, 
        N_max,
        parallel, 
        cpp,
        sim_id
      )
    
    return(eq)
  }
