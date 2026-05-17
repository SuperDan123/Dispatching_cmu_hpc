 # Intialize--------------------------------------------------------------------
 rm(list = ls())


 library("magrittr")
 library("kableExtra")
 library("ggplot2")

 # Read data --------------------------------------------------------------------

 data_zip_establishments <- readRDS(file = "output/data_zip_establishments.rds")
 data_zip_establishments_all <- readRDS(file = "output/data_zip_establishments_all.rds")
 data_zip_establishments_all_ippann <- readRDS(file = "output/data_zip_establishments_all_ippann.rds")
 data_zip_establishments_all_ippann_unique <- readRDS(file = "output/data_zip_establishments_all_ippann_unique.rds")
 data_zip_establishments_all_ippann_unique_weights <- readRDS(file = "output/data_zip_establishments_all_ippann_unique_weights.rds")
 
 
 # Describe N_{zip_t}-----------------------------------------------------------
 
 N_zip_t_unique <- 
   data_zip_establishments %>% 
   dplyr::select(N_zip_t) %>% 
   unique()
 
 quantile(
   N_zip_t_unique$N_zip_t, 
   c(0.05, 0.1, 0.25, 0.5, 0.75, 1)
 ) %>% 
   kbl(col.names = "N_zip_t") %>% 
   kable_styling(full_width = FALSE)
 
 ## show histogram
 histogram_N_zip_t_unique <- 
   ggplot() + 
   geom_histogram(
     data = N_zip_t_unique, 
     mapping = aes(x = N_zip_t),
     bins = 100
   )
 
 print(histogram_N_zip_t_unique)
 
 data_zip_establishments_N_zip_t <- 
   data_zip_establishments %>% 
   dplyr::select(N_zip_t)
 
 histogram_N_zip_t <- 
   ggplot() + 
   geom_histogram(
     data = data_zip_establishments_N_zip_t, 
     mapping = aes(x = N_zip_t),
     bins = 100
   )
 
 print(histogram_N_zip_t)
 
 
 # Describe wage (wage_{it})----------------------------------------------------
 
 data_zip_establishments_wage_fee <- 
   data_zip_establishments %>% 
   dplyr::group_by(
     year, 
     zip_three
   ) %>% 
   dplyr::mutate(
     wage_jt = mean(wage),
     fee_jt = mean(fee)
   ) 
 
 wage_jt_unique <- 
   data_zip_establishments_wage_fee %>% 
   dplyr::select(wage_jt) %>% 
   unique()
 
 summary(wage_jt_unique$wage_jt) 
 
 
 # Describe fee (fee_{it})------------------------------------------------------ 
 
 fee_jt_unique <- 
   data_zip_establishments_wage_fee %>% 
   dplyr::select(fee_jt) %>% 
   unique() 
 
 summary(fee_jt_unique$fee_jt)
 
 
 # Analyze N_{zip_t}----------------------------------------------------------------
 
 results11 <- list()
 
 results11[[1]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t | zip_three + year ,
     data = data_zip_establishments
   )
 
 results11[[2]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t | year + firm_id ,
     data = data_zip_establishments
   )
 
 results11[[3]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t | zip_three + year ,
     data = data_zip_establishments
   )
 
 results11[[4]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t | year + firm_id ,
     data = data_zip_establishments
   )
 
 rows <- 
   tibble::tribble(
     ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
     'j_FE', 'Y', 'N', 'Y', 'N',
     't_FE', 'Y', 'Y', 'Y', 'Y',
     'i_FE', 'N', 'Y', 'N', 'Y' 
   )
 
 attr(rows, 'position') <- c(6, 7, 8, 9)
 
 result_11 <- 
   list(
     "log(fee)" = results11[[1]],
     "log(fee)" = results11[[2]],
     "log(wage)" = results11[[3]],
     "log(wage)" = results11[[4]]
   )
 
 modelsummary::modelsummary(
   result_11,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 
 # Analyze D_ for zip_three------------------------------------------------------
 
 ## Include D_1 to D_5------------------------------------------------------------
 
 results22 <- list()
 
 results22[[1]] <- 
   lfe::felm(
     formula = log_fee ~ D_2 + D_3 + D_4 + D_5  | zip_three + year ,
     data = data_zip_establishments
   )
 
 results22[[2]] <- 
   lfe::felm(
     formula = log_fee ~ D_2 + D_3 + D_4 + D_5 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results22[[3]] <- 
   lfe::felm(
     formula = log_wage ~ D_2 + D_3 + D_4 + D_5 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results22[[4]] <- 
   lfe::felm(
     formula = log_wage ~ D_2 + D_3 + D_4 + D_5 | year + firm_id ,
     data = data_zip_establishments
   )
 
 rows_D <- 
   tibble::tribble(
     ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
     'j_FE', 'Y', 'N', 'Y', 'N',
     't_FE', 'Y', 'Y', 'Y', 'Y',
     'i_FE', 'N', 'Y', 'N', 'Y' 
   )
 
 attr(rows_D, 'position') <- c(12, 13, 14, 15)
 
 result_22 <- 
   list(
     "log(fee)" = results22[[1]],
     "log(fee)" = results22[[2]],
     "log(wage)" = results22[[3]],
     "log(wage)" = results22[[4]]
   )
 
 modelsummary::modelsummary(
   result_22,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows_D
 ) 
 
 
 ## Include D_1 to D_10------------------------------------------------------------
 
 results23 <- list()
 
 results23[[1]] <- 
   lfe::felm(
     formula = log_fee ~ D_2 + D_3 + D_4 + D_5 + D_6 + D_7 + D_8 + D_9 + D_10 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results23[[2]] <- 
   lfe::felm(
     formula = log_fee ~ D_2 + D_3 + D_4 + D_5 + D_6 + D_7 + D_8 + D_9 + D_10 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results23[[3]] <- 
   lfe::felm(
     formula = log_wage ~ D_2 + D_3 + D_4 + D_5 + D_6 + D_7 + D_8 + D_9 + D_10 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results23[[4]] <- 
   lfe::felm(
     formula = log_wage ~ D_2 + D_3 + D_4 + D_5 + D_6 + D_7 + D_8 + D_9 + D_10 | year + firm_id ,
     data = data_zip_establishments
   )
 
 rows_DD <- 
   tibble::tribble(
     ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
     'j_FE', 'Y', 'N', 'Y', 'N',
     't_FE', 'Y', 'Y', 'Y', 'Y',
     'i_FE', 'N', 'Y', 'N', 'Y' 
   )
 
 attr(rows_DD, 'position') <- c(20, 21, 22, 23)
 
 result_23 <- 
   list(
     "log(fee)" = results23[[1]],
     "log(fee)" = results23[[2]],
     "log(wage)" = results23[[3]],
     "log(wage)" = results23[[4]]
   )
 
 modelsummary::modelsummary(
   result_23,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows_DD
 ) 
 
 
 # Analyze different N_{jt}-----------------------------------------------------
 
 ## Analyze N_{jt}_10-----------------------------------------------------------
 
 results33 <- list()
 
 results33[[1]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_10 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results33[[2]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_10 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results33[[3]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_10 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results33[[4]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_10 | year + firm_id ,
     data = data_zip_establishments
   )
 
 attr(rows, 'position') <- c(6, 7, 8, 9)
 
 result_33 <- 
   list(
     "log(fee)" = results33[[1]],
     "log(fee)" = results33[[2]],
     "log(wage)" = results33[[3]],
     "log(wage)" = results33[[4]]
   )
 
 modelsummary::modelsummary(
   result_33,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 
 ## Analyze N_{jt}_1000----------------------------------------------------------
 
 results44 <- list()
 
 results44[[1]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_1000 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results44[[2]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_1000 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results44[[3]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_1000 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results44[[4]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_1000 | year + firm_id ,
     data = data_zip_establishments
   )
 
 attr(rows, 'position') <- c(6, 7, 8, 9)
 
 result_44 <- 
   list(
     "log(fee)" = results44[[1]],
     "log(fee)" = results44[[2]],
     "log(wage)" = results44[[3]],
     "log(wage)" = results44[[4]]
   )
 
 modelsummary::modelsummary(
   result_44,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 
 ## Analyze N_{jt}_100-------------------------------------------------------------
 
 results55 <- list()
 
 results55[[1]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_100 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results55[[2]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_100 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results55[[3]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_100 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results55[[4]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_100 | year + firm_id ,
     data = data_zip_establishments
   )
 
 attr(rows, 'position') <- c(6, 7, 8, 9)
 
 result_55 <- 
   list(
     "log(fee)" = results55[[1]],
     "log(fee)" = results55[[2]],
     "log(wage)" = results55[[3]],
     "log(wage)" = results55[[4]]
   )
 
 modelsummary::modelsummary(
   result_55,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 
 ## Analyze N_{jt}_10000---------------------------------------------------------
 
 results66 <- list()
 
 results66[[1]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_10000 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results66[[2]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_10000 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results66[[3]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_10000 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results66[[4]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_10000 | year + firm_id ,
     data = data_zip_establishments
   )
 
 attr(rows, 'position') <- c(6, 7, 8, 9)
 
 result_66 <- 
   list(
     "log(fee)" = results66[[1]],
     "log(fee)" = results66[[2]],
     "log(wage)" = results66[[3]],
     "log(wage)" = results66[[4]]
   )
 
 modelsummary::modelsummary(
   result_66,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 
 ## Analyze N_{jt}_100000-------------------------------------------------------
 
 results77 <- list()
 
 results77[[1]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_100000 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results77[[2]] <- 
   lfe::felm(
     formula = log_fee ~  N_zip_t_100000 | year + firm_id ,
     data = data_zip_establishments
   )
 
 results77[[3]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_100000 | zip_three + year ,
     data = data_zip_establishments
   )
 
 results77[[4]] <- 
   lfe::felm(
     formula = log_wage ~  N_zip_t_100000 | year + firm_id ,
     data = data_zip_establishments
   )
 
 attr(rows, 'position') <- c(6, 7, 8, 9)
 
 result_77 <- 
   list(
     "log(fee)" = results77[[1]],
     "log(fee)" = results77[[2]],
     "log(wage)" = results77[[3]],
     "log(wage)" = results77[[4]]
   )
 
 modelsummary::modelsummary(
   result_77,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 # Analyze Bartick IV (bartik_zip_t) -------------------------------------------
 
 results_b11 <- list()
 
 results_b11[[1]] <- 
   lfe::felm(
     formula = log_fee ~ 1 | zip_three + year | (N_zip_t_1000 ~ bartik_zip_t) | zip_three,
     data = data_zip_establishments_all_ippann
   )
 
 results_b11[[2]] <- 
   lfe::felm(
     formula = log_wage ~ 1 | zip_three + year | (N_zip_t_1000 ~ bartik_zip_t) | zip_three,
     data = data_zip_establishments_all_ippann
   )
 
 rows <- 
   tibble::tribble(
     ~term, ~`(1)`, ~`(2)`,
     'j_FE', 'Y', 'Y',
     't_FE', 'Y', 'Y',
     'i_FE', 'N', 'N', 
   )
 
 attr(rows, 'position') <- c(3, 4, 5, 6)
 
 resultsb11 <- 
   list(
     "log(fee)" = results_b11[[1]],
     "log(wage)" = results_b11[[2]]
   )
 
 modelsummary::modelsummary(
   resultsb11,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 ## 1st stage 
 
 rows <- 
   tibble::tribble(
     ~term, ~`(1)`
   )
 
 attr(rows, 'position') <- c(3, 4, 5, 6)
 
 resultsb33 <- 
   list(
     "N_zip_t_1000(1st stage zip)" = results_b11[[1]]$stage1
   )
 
 modelsummary::modelsummary(
   resultsb33,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95
 )
 
 ## reduced form
 
 results_b22 <- list()
 
 results_b22[[1]] <- 
   lfe::felm(
     formula = log_fee ~ bartik_zip_t | zip_three + year ,
     data = data_zip_establishments_all_ippann
   )
 
 results_b22[[2]] <- 
   lfe::felm(
     formula = log_wage ~ bartik_zip_t | zip_three + year,
     data = data_zip_establishments_all_ippann
   )
 
 rows <- 
   tibble::tribble(
     ~term, ~`(1)`, ~`(2)`,
     'j_FE', 'Y', 'Y',
     't_FE', 'Y', 'Y',
     'i_FE', 'N', 'N', 
   )
 
 attr(rows, 'position') <- c(3, 4, 5, 6)
 
 resultsb22 <- 
   list(
     "log(fee)" = results_b22[[1]],
     "log(wage)" = results_b22[[2]]
   )
 
 modelsummary::modelsummary(
   resultsb22,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95,
   gof_omit = "AIC|BIC",
   add_rows = rows
 ) 
 
 # Analyze residuals for zip data-----------------------------------------------
 
 ## Regress N_zip_t on zip_three / year fixed effect 
 
 result_u_zip <- 
   lfe::felm(
     formula = N_zip_t ~ 1 + zip_three + year ,
     data = data_zip_establishments_all_ippann
   )
 
 id_zip <- c(1:55843)
 
 ## Calculate residuals 
 
 residual_zip <-
   as.data.frame(result_u_zip$residuals) %>% 
   dplyr::rename(u_hat_zip = N_zip_t) %>% 
   cbind(id_zip)
 
 residual_zip_unique <- 
   residual_zip %>% 
   dplyr::select(
     u_hat_zip
   ) %>% 
   unique() %>% 
   as.data.frame() 
 
 summary(residual_zip_unique$u_hat_zip)
 
 
 ## show histogram (unique)
 
 histogram_residuals_zip_unique <- 
   ggplot() + 
   geom_histogram(
     data = residual_zip_unique, 
     mapping = aes(x = u_hat_zip),
     bins = 100
   )
 
 print(histogram_residuals_zip_unique)
 
 
 
 ## show histogram (all)
 
 residual_zip_all <- 
   residual_zip 
 
 histogram_residuals_zip_all <- 
   ggplot() + 
   geom_histogram(
     data = residual_zip_all, 
     mapping = aes(x = u_hat_zip),
     bins = 100
   )
 
 print(histogram_residuals_zip_all)
 
 summary(residual_zip_all$u_hat_zip)
 
 #16,325 
 data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2010
   ) 

 
 # Analyze first order difference for zip data ----------------------------------------------
 
 ## first stage (zip)
 
 results_diff_zip <- list()
 
 results_diff_zip[[1]] <- 
   lfe::felm(
     formula = log_diff_10_14 ~  share_zip_2010  ,
     data = data_zip_establishments_all_ippann_unique
   )
 
 result_diffzip <- 
   list(
     "log_diff_10_14" = results_diff_zip[[1]]
   )
 
 modelsummary::modelsummary(
   result_diffzip,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC"
 ) 
 
 
 ## reduced form (zip)
 
 ### wage
 
 results_growth_zip <- list()
 
 results_growth_zip[[1]] <- 
   lfe::felm(
     formula = log_diff_wage ~  share_zip_2010 ,
     data = data_zip_establishments_all_ippann
   )
 
 result_growthzip <- 
   list(
     "log_diff_wage" = results_growth_zip[[1]]
   )
 
 modelsummary::modelsummary(
   result_growthzip,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC"
 ) 
 
 ### fee
 
 results_feegrowth_zip <- list()
 
 results_feegrowth_zip[[1]] <- 
   lfe::felm(
     formula = log_diff_fee ~  share_zip_2010  ,
     data = data_zip_establishments_all_ippann
   )
 
 
 
 result_feegrowthzip <- 
   list(
     "log_diff_fee" = results_feegrowth_zip[[1]]
   )
 
 modelsummary::modelsummary(
   result_feegrowthzip,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC"
 ) 
 
 
 ## structural form (zip)
 
 ### wage 
 
 #### 2nd stage
 
 results_s_wage_zip <- list()
 
 results_s_wage_zip[[1]] <- 
   lfe::felm(
     formula = log_diff_wage ~  1 | 0 | (log_diff_10_14 ~ share_zip_2010) | zip_three ,
     data = data_zip_establishments_all_ippann
   )
 
 results_s_wage_zip[[2]] <- 
   lfe::felm(
     formula = log_diff_wage ~  1 | 0 | (log_diff_10_14 ~ share_zip_2010) | 0,
     data = data_zip_establishments_all_ippann
   )
 
 result_swagegrowthzip <- 
   list(
     "log_diff_wage" = results_s_wage_zip[[1]],
     "log_diff_wage" = results_s_wage_zip[[2]]
   )
 
 modelsummary::modelsummary(
   result_swagegrowthzip,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC"
 ) 
 
 #### 1st stage 
 
 resultsb5 <- 
   list(
     "log_diff_10_14(1st stage)" = results_s_wage_zip[[1]]$stage1,
     "log_diff_10_14(1st stage)" = results_s_wage_zip[[2]]$stage1
   )
 
 modelsummary::modelsummary(
   resultsb5,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95
 )
 
 ### fee  
 
 #### 2nd stage
 
 results_s_fee_zip <- list()
 
 results_s_fee_zip[[1]] <- 
   lfe::felm(
     formula = log_diff_fee ~ 1 | 0 | (log_diff_10_14 ~ share_zip_2010) | zip_three ,
     data = data_zip_establishments_all_ippann
   )
 
 results_s_fee_zip[[2]] <- 
   lfe::felm(
     formula = log_diff_fee ~ 1 | 0 | (log_diff_10_14 ~ share_zip_2010) | 0,
     data = data_zip_establishments_all_ippann
   )
 
 result_sfeegrowthzip <- 
   list(
     "log_diff_fee" = results_s_fee_zip[[1]],
     "log_diff_fee" = results_s_fee_zip[[2]]
   )
 
 modelsummary::modelsummary(
   result_sfeegrowthzip,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC"
 ) 
 
 #### 1st stage 
 
 resultsb6 <- 
   list(
     "log_diff_10_14(1st stage)" = results_s_fee_zip[[1]]$stage1,
     "log_diff_10_14(1st stage)" = results_s_fee_zip[[2]]$stage1
   )
 
 modelsummary::modelsummary(
   resultsb6,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95
 )
 
 # Check for zip data each year-------------------------------------------------
 
 # year == 2010, 16,324 entries
 data_zip_establishments_all_ippann_2010 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(year == 2010)
 
 data_zip_establishments_all_ippann_2010 <-
   data_zip_establishments_all_ippann_2010 %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     nzipt = dplyr::n()
   ) %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_2010 %>% 
   dplyr::select(
     zip_three,
     nzipt
   ) %>% 
   dplyr::filter(
     zip_three == 113
   ) %>% 
   View()
 
 # year == 2011, 14,855 entries
 data_zip_establishments_all_ippann_2011 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(year == 2011)
 
 
 data_zip_establishments_all_ippann_2011 <-
   data_zip_establishments_all_ippann_2011 %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     nzipt = dplyr::n()
   ) %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_2011 %>% 
   dplyr::select(
     zip_three,
     nzipt
   ) %>% 
   dplyr::filter(
     zip_three == 100
   ) %>% 
   View()
 
 # year == 2012, 14,361  entries
 data_zip_establishments_all_ippann_2012 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(year == 2012) 
 
 data_zip_establishments_all_ippann_2012 <-
   data_zip_establishments_all_ippann_2012 %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     nzipt = dplyr::n()
   ) %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_2012 %>% 
   dplyr::select(
     zip_three,
     nzipt
   ) %>% 
   dplyr::filter(
     zip_three == 100
   ) %>% 
   View()
 
 # year == 2013, 13,160 entries
 data_zip_establishments_all_ippann_2013 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(year == 2013) 
 
 data_zip_establishments_all_ippann_2013 <-
   data_zip_establishments_all_ippann_2013 %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     nzipt = dplyr::n()
   ) %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_2013 %>% 
   dplyr::select(
     zip_three,
     nzipt
   ) %>% 
   dplyr::filter(
     zip_three == 100
   ) %>% 
   View()
 
 # year == 2014, 13,659 entries
 data_zip_establishments_all_ippann_2014 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(year == 2014) 
 
 data_zip_establishments_all_ippann_2014 <-
   data_zip_establishments_all_ippann_2014 %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     nzipt = dplyr::n()
   ) %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_2014 %>% 
   dplyr::select(
     zip_three,
     nzipt
   ) %>% 
   dplyr::filter(
     zip_three == 100
   ) %>% 
   View()
 
 ## Combine all years' data to one data-----------------------------------------
 #72,954 entries
 data_zip_establishments_general <-
   rbind(data_zip_establishments_all_ippann_2010, 
         data_zip_establishments_all_ippann_2011) %>% 
   rbind(data_zip_establishments_all_ippann_2012) %>% 
   rbind(data_zip_establishments_all_ippann_2013) %>% 
   rbind(data_zip_establishments_all_ippann_2014) %>% 
   tidyr::drop_na(
     wage,
     fee,
     zip_three,
     nzipt
   )　
 
 #there are 71,486 establishments in data from 2010 to 2014 (NOT like PANEL structure)
 data_zip_establishments_general %>% 
   dplyr::select(
     id_unique,
     year,
     zip_three,
     nzipt
   ) %>% 
   dplyr::select(
     id_unique
   ) 
 
 
 # 855  (all variation)
 data_zip_establishments_general %>% 
   dplyr::select(zip_three) %>% 
   unique() %>% 
   View()
 
 # Check for zip data
 
 ## Check for 2010 zip data------------------------------------------------------
 
 # 830 variation in zip in 2010
 data_zip_establishments_all_ippann_2010 %>% 
   dplyr::select(zip_three) %>% 
   unique() %>% 
   dplyr::arrange(zip_three) %>% 
   View()
 
 data_zip_establishments_all_ippann_2014 %>% 
   dplyr::select(
     year,
     zip_three,
     firm_id,
     nzipt
   ) %>% 
   dplyr::select(
     nzipt
   ) %>% 
   unique() %>% 
   View()
 
 quantile(
   data_zip_establishments_all_ippann_2010$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2010 all") %>% 
   kable_styling(full_width = FALSE)
 
 ## quantile for 2010 data
 nzipt_unique_2010 <- 
   data_zip_establishments_all_ippann_2010 %>% 
   dplyr::select(nzipt) %>% 
   unique()
 
 quantile(
   nzipt_unique_2010$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2010 unique") %>% 
   kable_styling(full_width = FALSE)
 
 ## Check for 2011 zip data------------------------------------------------------
 
 # 818 variation in zip in 2011
 data_zip_establishments_all_ippann_2011 %>% 
   dplyr::select(zip_three) %>% 
   unique() %>% 
   dplyr::arrange(zip_three) %>% 
   View()
 
 quantile(
   data_zip_establishments_all_ippann_2011$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2011 all") %>% 
   kable_styling(full_width = FALSE)
 
 ## quantile for 2011 data
 nzipt_unique_2011 <- 
   data_zip_establishments_all_ippann_2011 %>% 
   dplyr::select(nzipt) %>% 
   unique() 
 
 quantile(
   nzipt_unique_2011$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2011 unique") %>% 
   kable_styling(full_width = FALSE)
 
 ## Check for 2012 zip data------------------------------------------------------
 
 # 799 variation in zip in 2012
 data_zip_establishments_all_ippann_2012 %>% 
   dplyr::select(zip_three) %>% 
   unique() %>% 
   dplyr::arrange(zip_three) %>% 
   View()
 
 quantile(
   data_zip_establishments_all_ippann_2012$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2012 all") %>% 
   kable_styling(full_width = FALSE)
 
 ## quantile for 2012 data
 nzipt_unique_2012 <- 
   data_zip_establishments_all_ippann_2012 %>% 
   dplyr::select(nzipt) %>% 
   unique()
 
 quantile(
   nzipt_unique_2012$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2012 unique") %>% 
   kable_styling(full_width = FALSE)
 
 ## Check for 2013 zip data------------------------------------------------------
 
 # 800 variation in zip in 2013
 data_zip_establishments_all_ippann_2013 %>% 
   dplyr::select(zip_three) %>% 
   unique() %>% 
   dplyr::arrange(zip_three) %>% 
   View()
 
 quantile(
   data_zip_establishments_all_ippann_2013$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2013 all") %>% 
   kable_styling(full_width = FALSE)
 
 ## quantile for 2013 data
 nzipt_unique_2013 <- 
   data_zip_establishments_all_ippann_2013 %>% 
   dplyr::select(nzipt) %>% 
   unique()
 
 quantile(
   nzipt_unique_2013$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2013 unique") %>% 
   kable_styling(full_width = FALSE)
 
 
 ## Check for 2014 zip data------------------------------------------------------
 
 # 793 variation in zip in 2014
 data_zip_establishments_all_ippann_2014 %>% 
   dplyr::select(zip_three) %>% 
   unique() %>% 
   dplyr::arrange(zip_three) %>% 
   View()
 
 quantile(
   data_zip_establishments_all_ippann_2014$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2014 all") %>% 
   kable_styling(full_width = FALSE)
 
 ## quantile for 2014 data
 nzipt_unique_2014 <- 
   data_zip_establishments_all_ippann_2014 %>% 
   dplyr::select(nzipt) %>% 
   unique()
 
 quantile(
   nzipt_unique_2014$nzipt, 
   c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
 ) %>% 
   kbl(col.names = "N_zip_t 2014 unique") %>% 
   kable_styling(full_width = FALSE)
 
 
 # Check for distribution of tokutei establishments / all establishments in 2010----------------
 
 ## zip
 data_zip_establishments_all_ippann
 
 
 ### total plot
 
 data_zip_establishments_all_ippann_share <-
   data_zip_establishments_all_ippann %>% 
   dplyr::select(
     year,
     zip_three,
     share_zip_2010,
     share_zip_t
   ) %>% 
   unique() 
 
 data_zip_establishments_all_ippann_share <-
   data_zip_establishments_all_ippann_share %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     d_2014 = dplyr::if_else(year == 2014, 1, 0),
     share_zip_2014 = max(d_2014*(share_zip_t))
   ) %>% 
   dplyr::select(
     - d_2014
   ) %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_share <- 
   data_zip_establishments_all_ippann_share %>% 
   dplyr::mutate(
     share_growth = (share_zip_2014 - share_zip_2010)/share_zip_2010
   )
 
 g14 <- ggplot() +
   geom_point(
     data = data_zip_establishments_all_ippann_share,
     mapping = aes(x = share_zip_2010, y = share_growth)
   )
 
 print(g14)
 
 ### total regress
 ## 2010
 
 data_zip_establishments_all_ippann_share_2010 <-
   data_zip_establishments_all_ippann_share %>% 
   dplyr::filter(year == 2010) %>% 
   tidyr::drop_na(
     share_growth,
     share_zip_2010
   ) %>% 
   dplyr::filter(
     ! share_zip_2010 == 0
   ) 
 
 
 # Analyze weighted version of first stage --------------------------------------
 
 results_diff_zip_w <- list()
 
 
 results_diff_zip_w[[1]] <- 
   lm(
     formula = log_diff_10_14 ~  share_zip_2010 | 0,
     data = data_zip_establishments_all_ippann_unique_weights,
     weights = weights
   )
 
 result_diffzipw <- 
   list(
     "log_diff_10_14" = results_diff_zip_w[[1]]
   )
 
 modelsummary::modelsummary(
   result_diffzipw,
   stars = TRUE,
   statistic = 'conf.int',
   conf_level = .95 ,
   gof_omit = "AIC|BIC"
 ) 
 
 
 