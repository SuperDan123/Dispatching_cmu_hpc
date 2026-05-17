

make_coefficient_table_demand_side <- 
  function(
    equilibrium
  ) {
    
    matching_params <- equilibrium$parameters$matching_params
    lambda <- equilibrium$parameters$lambda
    lambda_se <- equilibrium$parameters_se$lambda
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    
    key_linear <- c("daily", "shokai", "oversea", "cocurrent")
    coefficient_a_W <- 
      equilibrium$parameters$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(coefficient_a_W)
    se_a_W <- 
      equilibrium$parameters_se$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(se_a_W)
    coefficient_a_F <- 
      equilibrium$parameters$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(coefficient_a_F)
    se_a_F <- 
      equilibrium$parameters_se$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(se_a_F)
    
    coef_W <- c(lambda[, "lambda_W"], NA, coefficient_a_W)
    se_W <- c(lambda_se[, "lambda_W"], NA, se_a_W)
    coef_F <- c(NA, lambda[, "lambda_F"], coefficient_a_F)
    se_F <- c(NA, lambda_se[, "lambda_F"], se_a_F) 
    
    
    result_table <- 
      data.frame(
        coef_W = coef_W,
        se_W = se_W,
        coef_F = coef_F,
        se_F = se_F
      )
    
    result_table <- 
      round(result_table, 2) %>% 
      format(nsmall = 2) %>% 
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~gsub("NA", "", .)
        )
      )
    
    rownames(result_table) <- c("Wage", "Fee", "Daily", "Temp to Perm", "Oversea", "Employment Placement")
    colnames(result_table) <- c("Coef.", "S.E.", "Coef.", "S.E.")
    
    return(result_table)
    
  }

make_coefficient_table_demand_side_converted_to_reduced_form <- 
  function(
    equilibrium
    ) {
  
    matching_params <- equilibrium$parameters$matching_params
    lambda <- equilibrium$parameters$lambda
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    
    result_table <- 
      data.frame(
        coef_W = c(beta[1], beta[3]),
        coef_F = c(beta[2], beta[4])
      )
    
    colnames(result_table) <- c("Worker", "Client firm")
    rownames(result_table) <- c("Wage", "Fee")
    result_table <- round(result_table, 2)
    
    return(result_table)
  
}

make_coefficient_table_supply_side <- 
  function(
    equilibrium
    ) {
  
    key_linear <- c("daily", "shokai", "oversea", "cocurrent")
    coef_W <- 
      equilibrium$parameters$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(coefficient_mc_W)
    se_W <- 
      equilibrium$parameters_se$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(se_mc_W)
    coef_F <- 
      equilibrium$parameters$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(coefficient_mc_F)
    se_F <- 
      equilibrium$parameters_se$linear %>%
      dplyr::filter(term %in% key_linear) %>%
      dplyr::pull(se_mc_F)
    
    result_table <- 
      data.frame(
        coef_W = coef_W,
        se_W = se_W,
        coef_F = coef_F,
        se_F = se_F
      )
    
    rownames(result_table) <- c("Daily", "Temp to Perm", "Oversea", "Employment Placement")
    colnames(result_table) <- c("Coef.", "S.E.", "Coef.", "S.E.")
    result_table <- round(result_table, 2) %>% format(nsmall = 2)
    
    return(result_table)
  
}

make_elasticity_table <- function(result) {
  

  lambda_W <- result$params_nonlinear[1]
  lambda_F <- result$params_nonlinear[2]
  alpha_W <- matching_params[1]
  alpha_F <- matching_params[2]
  
  lambda <- c(lambda_W, lambda_F)
  beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
  
  beta_DW <- beta[1]
  beta_SW <- beta[2] 
  beta_DF <- beta[3]
  beta_SF <- beta[4]
  
  direct_W <- lambda_W
  direct_F <- lambda_F
  
  network_total_W <- beta_DW - direct_W
  network_total_F <- beta_SF - direct_F
  
  network_direct_W <- -(1 - alpha_W) / (2 - alpha_W) * lambda_W
  network_direct_F <- -(1 - alpha_F) / (2 - alpha_F) * lambda_F
  
  network_indirect_W <- alpha_F / (2 - alpha_F) * lambda_W
  network_indirect_F <- alpha_W / (2 - alpha_W) * lambda_F
  
  network_interaction_W <- network_total_W - network_direct_W - network_indirect_W
  network_interaction_F <- network_total_F - network_direct_F - network_indirect_F

  
  result_table <- data.frame(
    wage = c(beta_DW, direct_W, network_direct_W, network_indirect_W, network_interaction_W),
    fee  = c(beta_SF, direct_F, network_direct_F, network_indirect_F, network_interaction_F)
  )
  
  rownames(result_table) <- c("Total", "Price effect", "Network effect: Direct", "Network effect: Indirect", "Network effect: Interaction")
  colnames(result_table) <- c("Worker share and wage", "Client firm share and fee")
  
  
  result_table <- round(result_table, 2) %>% format(nsmall = 2)
  
  return(tesult_table)
  
}

make_elasticity_table_detailed <- 
  function(
    equilibrium
    ) {
  
    matching_params <- equilibrium$parameters$matching_params
    lambda_W <- equilibrium$parameters$lambda[, "lambda_W"]
    lambda_F <- equilibrium$parameters$lambda[, "lambda_F"]
    alpha_W <- matching_params[1]
    alpha_F <- matching_params[2]
    
    lambda <- c(lambda_W, lambda_F)
    beta <- convert_parameters_from_structural_to_reduced(lambda, matching_params)
    
    beta_DW <- beta[1]
    beta_SW <- beta[2]  
    beta_DF <- beta[3]
    beta_SF <- beta[4]
    
    direct_W <- lambda_W
    direct_F <- lambda_F
    
    network_total_W <- beta_DW - direct_W
    network_total_F <- beta_SF - direct_F
    
    network_direct_W <- -(1 - alpha_W) / (2 - alpha_W) * lambda_W
    network_direct_F <- -(1 - alpha_F) / (2 - alpha_F) * lambda_F
    
    network_indirect_W <- alpha_F / (2 - alpha_F) * lambda_W
    network_indirect_F <- alpha_W / (2 - alpha_W) * lambda_F
    
    network_interaction_W <- network_total_W - network_direct_W - network_indirect_W
    network_interaction_F <- network_total_F - network_direct_F - network_indirect_F
    
    result_table <- 
      data.frame(
        worker_wage = c(beta_DW, direct_W, network_direct_W, network_indirect_W, network_interaction_W),
        worker_fee = c(beta_DF, NA, NA, beta_DF, NA),
        firm_wage = c(beta_SW, NA, NA, beta_SW, NA),
        firm_fee  = c(beta_SF, direct_F, network_direct_F, network_indirect_F, network_interaction_F)
      )
    
    data <- equilibrium$generate_dataframe()
    
    if (equilibrium$spec == "linear") {
      result_table$worker_wage <- result_table$worker_wage * mean((1 - data$s_W) * data$wage)
      result_table$worker_fee <- result_table$worker_fee * mean((1 - data$s_W) * data$fee)
      result_table$firm_wage <- result_table$firm_wage * mean((1 - data$s_F) * data$wage)
      result_table$firm_fee <- result_table$firm_fee * mean((1 - data$s_F) * data$fee)
    } else {
      result_table$worker_wage <- result_table$worker_wage * mean((1 - data$s_W))
      result_table$worker_fee <- result_table$worker_fee * mean((1 - data$s_W))
      result_table$firm_wage <- result_table$firm_wage * mean((1 - data$s_F))
      result_table$firm_fee <- result_table$firm_fee * mean((1 - data$s_F))
    }
    
    result_table <- 
      round(result_table, 2) %>% 
      format(nsmall = 2) %>% 
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~gsub("NA", "", .)
        )
      )
    
    rownames(result_table) <- c("Total", "Price effect", "Network effect: Direct", "Network effect: Indirect", "Network effect: Interaction")
    colnames(result_table) <- c("wage", "fee", "wage", "fee")
    
    
    return(result_table)
  
}


compute_indirect_utility <- function(equilibrium) {
  
  indirect_utility <- list()
  lambda <- equilibrium$parameters$lambda
  matching_params <- equilibrium$parameters$matching_params
  N <- length(equilibrium$endogenous)
  
  out <- list()
  for (i in 1:N) {
    
    heterogeneity_i <- equilibrium$platform_heterogeneity[[i]]
    endogenous_i <- equilibrium$endogenous[[i]]
    exogenous_i <- equilibrium$exogenous[[i]]
    
    a_W <- heterogeneity_i[, 2]
    a_F <- heterogeneity_i[, 3]
    wage <- endogenous_i[, 2]
    fee <- endogenous_i[, 3]
    w_0 <- exogenous_i[1, "ptwage"]
    S_W <- exogenous_i[1, "S_W"]
    S_F <- exogenous_i[1, "S_F"]
    
    h_W <- exp((a_W + log(wage)) / lambda[1])
    h_F <- exp((a_F - log(fee)) / lambda[2])
    h_W_0 <- exp(log(w_0) / lambda[1])
    h_F_0 <- exp(-log(w_0) / lambda[2])
    
    D_W <- endogenous_i[, 4] * S_W
    D_F <- endogenous_i[, 5] * S_F
    
    D_W_0 <- S_W - sum(D_W)
    D_F_0 <- S_F - sum(D_F)
    
    util_W <- log(h_W) - (1 - matching_params[1]) * log(D_W) + matching_params[2] * log(D_F)
    util_F <- log(h_F) + matching_params[1] * log(D_W) - (1 - matching_params[2]) * log(D_F)
    util_W_0 <- log(h_W_0) - (1 - matching_params[1]) * log(D_W_0) + matching_params[2] * log(D_F_0)
    util_F_0 <- log(h_F_0) + matching_params[1] * log(D_W_0) - (1 - matching_params[2]) * log(D_F_0)
    
    out[[i]] <- data.frame(
      id_unique = exogenous_i[, "id_unique"],
      year = exogenous_i[, "year"],
      cz = exogenous_i[, "cz"],
      util_W = util_W,
      util_F = util_F,
      util_W_0 = util_W_0,
      util_F_0 = util_F_0
    )
    
  }
  
  out <- do.call("rbind", out)
  
  return(out)
}



compute_indirect_utility_relative_to_outside_option <- function(equilibrium) {
  
  out <- compute_indirect_utility(equilibrium)
  out <- out %>% 
    dplyr::group_by(year, cz) %>% 
    dplyr::summarise(
      u_W = log(1 + sum(exp(util_W - util_W_0))),
      u_F = log(1 + sum(exp(util_F - util_F_0)))
    ) %>% 
    dplyr::ungroup()
  
  return(out)
}



