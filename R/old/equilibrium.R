

equilibriumClass <- setRefClass(
  Class = "equilibriumClass",
  
  fields = list(
    dataframe = "data.frame",
    ownership = "list",
    exogenous = "list",
    endogenous = "list",
    instruments = "list",
    platform_heterogeneity = "list",
    shocks = "list",
    var_names = "list",
    parameters = "list",
    parameters_se = "list",
    spec = "character"
  ),
  
  method = list(
    
    # Initialization
    initialize = function(data = data.frame(),
                          platform_heterogeneity = list(),
                          shocks = list(),
                          var_names = list(),
                          spec = "")
    {

      dataframe <<- data
      platform_heterogeneity <<- platform_heterogeneity
      shocks <<- shocks
      var_names <<- var_names
      parameters <<- 
        list(
          lambda = c(0, 0), 
          matching_params = c(0, 0),
          linear = c(),
          rp = 0
        )
      parameters_se <<- 
        list(
          lambda = c(), 
          matching_params = c(),
          linear = c(),
          rp = 0
        )
      spec <<- spec
    },
    
    # Construct list of data matrix where each list element is for each market
    generate_data_list = function() {
      
      data_list <- generate_data_list_market(dataframe)
      exogenous <<-
        data_list %>%
        purrr::map(
          ~ .[c(var_names$x_names, "id_unique", "firm_id", "establishment_id", "year", "cz", "year_cz", "ptwage", "S_W", "S_F", "mu")] %>% 
            as.matrix()
          )
      endogenous <<-
        data_list %>%
        purrr::map(
          ~ .[c("id_unique", "wage", "fee", "s_W", "s_F", "Q")] %>% 
            as.matrix()
          )
      instruments <<-
        data_list %>%
        purrr::map(
          ~ .[c("id_unique", var_names$iv_names)] %>%
            as.matrix()
        )
      shocks[[1]] <<-
        data_list %>%
        purrr::map(
          ~ dplyr::select(
            .,
            dplyr::matches(c("id_unique", "residual_a_W", "residual_a_F", "residual_mc_W", "residual_mc_F"))
            ) %>%
            as.matrix()
        )
      platform_heterogeneity <<-
        data_list %>%
        purrr::map(
          ~ dplyr::select(
            .,
            dplyr::matches(c("id_unique", "predicted_a_W", "predicted_a_F", "predicted_mc_W", "predicted_mc_F"))
          ) %>%
            as.matrix()
        )
    },
    # Construct ownership matrix 
    construct_ownership_matrix = function() {
      data_list <- generate_data_list_market(dataframe)
      ownership <<-
        data_list %>%
        purrr::map(
          ~ model.matrix(
            data = .,
            object = "~ -1 + factor(owner_id)" %>% as.formula()
          ) %>%
            as.matrix() %>%
            tcrossprod(., .) 
        )
    },
    
    # Make a data frame using list of market-level data matrices
    generate_dataframe = function() {
      data_exogenous <- do.call("rbind", exogenous) %>% as.data.frame()
      data_endogenous <- do.call("rbind", endogenous) %>% as.data.frame()
      data_instruments <- do.call("rbind", instruments) %>% as.data.frame()
      data_shocks <- do.call("rbind", shocks[[1]]) %>% as.data.frame()
      data_heterogeneity <- do.call("rbind", platform_heterogeneity) %>% as.data.frame()
      dataframe <<- data_exogenous %>% 
        dplyr::left_join(data_endogenous, by = "id_unique") %>% 
        dplyr::left_join(data_instruments, by = "id_unique") %>%
        dplyr::left_join(data_shocks, by = "id_unique") %>%
        dplyr::left_join(data_heterogeneity, by = "id_unique")
    },
    
    # Extract only relevant information of a single market for equilibrium calculation
    extract_single_market = function(mkt_id, sim_id = 1) {
      
      # Market i & simulation k
      exogenous_i <- exogenous[[mkt_id]]
      endogenous_i <- endogenous[[mkt_id]]
      heterogeneity_i <- platform_heterogeneity[[mkt_id]]
      shocks_k <- shocks[[sim_id]]
      shocks_ki <- shocks_k[[mkt_id]]
      
      a <- 
        heterogeneity_i %>%
        as.data.frame() %>%
        dplyr::select(dplyr::starts_with("predicted_a")) %>%
        as.matrix() +
        shocks_ki %>%
        as.data.frame() %>%
        dplyr::select(dplyr::starts_with("residual_a")) %>%
        as.matrix() 
        
      mc <- 
        heterogeneity_i %>%
        as.data.frame() %>%
        dplyr::select(dplyr::starts_with("predicted_mc")) %>%
        as.matrix() +
        shocks_ki %>%
        as.data.frame() %>%
        dplyr::select(dplyr::starts_with("residual_mc")) %>%
        as.matrix() 
      
      mu <- exogenous_i[, "mu"]
      w_0 <- exogenous_i[, "ptwage"][1]
      S_W <- exogenous_i[, "S_W"][1]
      S_F <- exogenous_i[, "S_F"][1]
      S <- c(S_W, S_F)
      price <- endogenous_i[, c("wage", "fee"), drop = F] %>% unname()
      id_unique <- exogenous_i[, "id_unique"]
      
      data <- 
        list(
          w_0 = w_0,
          mu = mu,
          a = a,
          mc = mc,
          S = S,
          price = price,
          id_unique = id_unique
          )
      
      return(data)
    },
    generate_zero_shocks = function() {
      shocks[[1]] <<-
        shocks[[1]] %>%
        purrr::map(
          .,
          function(x) {
            x[, colnames(x) != "id_unique"] <- 0
            return(x)
          }
        )
    },
    # Randomly draw residuals
    generate_shock_zeros_from_empirical_distribution = function(n_sim, seed) {
      
      set.seed(seed)
      shocks <<- shocks[1]
      shocks_actual <- shocks[[1]]
      shocks_resampled <-
        foreach (i = 1:n_sim) %do% {
          shocks_resampled_i <-
            foreach (t = 1:length(shocks_actual)) %do% {
              s <- shocks_actual[[t]]
              shocks_resampled_it <-
                cbind(
                  s[, "id_unique", drop = FALSE],
                  s[
                    sample(
                      x = 1:nrow(s),
                      size = nrow(s),
                      replace = TRUE
                    ),
                    colnames(s) != "id_unique"
                  ]
                )
              return(shocks_resampled_it)
            }
          return(shocks_resampled_i)
        }
      shocks <<-
        append(
          shocks,
          shocks_resampled
        )
    }
  )
)

# Extract markets
extract_cz <-
  function(
    equilibrium,
    cz_list, 
    sim_id = 1
    ) {
  market_list <-
    purrr::map(
      equilibrium$exogenous,
      ~ as.data.frame(.) %>%
        dplyr::filter(cz %in% cz_list) %>%
        nrow()
    ) %>%
    purrr::reduce(c)
  market_list <- which(market_list > 0)
  eq <- equilibrium$copy()
  eq$endogenous <-
    eq$endogenous[market_list]
  eq$exogenous <-
    eq$exogenous[market_list]
  eq$shocks <-
    list(eq$shocks[[sim_id]][market_list])
  eq$instruments <-
    eq$instruments[market_list]
  eq$platform_heterogeneity <-
    eq$platform_heterogeneity[market_list]
  eq$ownership <-
    eq$ownership[market_list]
  eq$dataframe <-
    eq$generate_dataframe()
  return(eq)
}

generate_data_list_market <-
  function(
    dataframe
  ) {
    data_list <-
      dataframe %>%
      dplyr::arrange(year_cz) %>%
      dplyr::group_split(year_cz)
    return(data_list)
  }