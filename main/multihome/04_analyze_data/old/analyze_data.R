# Intialize--------------------------------------------------------------------
rm(list = ls())


library("magrittr")
library("kableExtra")
library("ggplot2")

# Read data --------------------------------------------------------------------

data_cz_establishments <- readRDS(file = "output/data_cz_establishments.rds")
data_zip_establishments <- readRDS(file = "output/data_zip_establishments.rds")
data_cz_establishments_all <- readRDS(file = "output/data_cz_establishments_all.rds")
data_zip_establishments_all <- readRDS(file = "output/data_zip_establishments_all.rds")
data_cz_establishments_all_ippann <- readRDS(file = "output/data_cz_establishments_all_ippann.rds")
data_zip_establishments_all_ippann <- readRDS(file = "output/data_zip_establishments_all_ippann.rds")
data_cz_establishments_all_ippann_unique <- readRDS(file = "output/data_cz_establishments_all_ippann_unique.rds")
data_zip_establishments_all_ippann_unique <- readRDS(file = "output/data_zip_establishments_all_ippann_unique.rds")

# Analyze N_{jt}----------------------------------------------------------------

results1 <- list()

results1[[1]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt | cz + year ,
    data = data_cz_establishments
  )

results1[[2]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt | year + firm_id ,
    data = data_cz_establishments
  )

results1[[3]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt | cz + year ,
    data = data_cz_establishments
  )

results1[[4]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt | year + firm_id ,
    data = data_cz_establishments
  )

rows <- 
  tibble::tribble(
    ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
    'j_FE', 'Y', 'N', 'Y', 'N',
    't_FE', 'Y', 'Y', 'Y', 'Y',
    'i_FE', 'N', 'Y', 'N', 'Y' 
  )

attr(rows, 'position') <- c(6, 7, 8, 9)

result_1 <- 
  list(
    "log(fee)" = results1[[1]],
    "log(fee)" = results1[[2]],
    "log(wage)" = results1[[3]],
    "log(wage)" = results1[[4]]
  )

modelsummary::modelsummary(
  result_1,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 


# Analyze D_---------------------------------------------------------------------

results2 <- list()

results2[[1]] <- 
  lfe::felm(
    formula = log_fee ~ D_2 + D_3 + D_4 + D_5 | cz + year ,
    data = data_cz_establishments
  )

results2[[2]] <- 
  lfe::felm(
    formula = log_fee ~ D_2 + D_3 + D_4 + D_5 | year + firm_id ,
    data = data_cz_establishments
  )

results2[[3]] <- 
  lfe::felm(
    formula = log_wage ~ D_2 + D_3 + D_4 + D_5 | cz + year ,
    data = data_cz_establishments
  )

results2[[4]] <- 
  lfe::felm(
    formula = log_wage ~ D_2 + D_3 + D_4 + D_5 | year + firm_id ,
    data = data_cz_establishments
  )

rows_D <- 
  tibble::tribble(
    ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
    'j_FE', 'Y', 'N', 'Y', 'N',
    't_FE', 'Y', 'Y', 'Y', 'Y',
    'i_FE', 'N', 'Y', 'N', 'Y' 
  )

attr(rows_D, 'position') <- c(12, 13, 14, 15)

result_2 <- 
  list(
    "log(fee)" = results2[[1]],
    "log(fee)" = results2[[2]],
    "log(wage)" = results2[[3]],
    "log(wage)" = results2[[4]]
  )

modelsummary::modelsummary(
  result_2,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows_D
) 


# Analyze different N_{jt}-----------------------------------------------------

## Analyze N_{jt}_10-----------------------------------------------------------

results3 <- list()

results3[[1]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_10 | cz + year ,
    data = data_cz_establishments
  )

results3[[2]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_10 | year + firm_id ,
    data = data_cz_establishments
  )

results3[[3]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_10 | cz + year ,
    data = data_cz_establishments
  )

results3[[4]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_10 | year + firm_id ,
    data = data_cz_establishments
  )

attr(rows, 'position') <- c(6, 7, 8, 9)

result_3 <- 
  list(
    "log(fee)" = results3[[1]],
    "log(fee)" = results3[[2]],
    "log(wage)" = results3[[3]],
    "log(wage)" = results3[[4]]
  )

modelsummary::modelsummary(
  result_3,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 


## Analyze N_{jt}_1000----------------------------------------------------------

results4 <- list()

results4[[1]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_1000 | cz + year ,
    data = data_cz_establishments
  )

results4[[2]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_1000 | year + firm_id ,
    data = data_cz_establishments
  )

results4[[3]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_1000 | cz + year ,
    data = data_cz_establishments
  )

results4[[4]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_1000 | year + firm_id ,
    data = data_cz_establishments
  )

attr(rows, 'position') <- c(6, 7, 8, 9)

result_4 <- 
  list(
    "log(fee)" = results4[[1]],
    "log(fee)" = results4[[2]],
    "log(wage)" = results4[[3]],
    "log(wage)" = results4[[4]]
  )

modelsummary::modelsummary(
  result_4,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 


## Analyze N_{jt}_100-------------------------------------------------------------

results5 <- list()

results5[[1]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_100 | cz + year ,
    data = data_cz_establishments
  )

results5[[2]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_100 | year + firm_id ,
    data = data_cz_establishments
  )

results5[[3]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_100 | cz + year ,
    data = data_cz_establishments
  )

results5[[4]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_100 | year + firm_id ,
    data = data_cz_establishments
  )

attr(rows, 'position') <- c(6, 7, 8, 9)

result_5 <- 
  list(
    "log(fee)" = results5[[1]],
    "log(fee)" = results5[[2]],
    "log(wage)" = results5[[3]],
    "log(wage)" = results5[[4]]
  )

modelsummary::modelsummary(
  result_5,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 


## Analyze N_{jt}_10000---------------------------------------------------------

results6 <- list()

results6[[1]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_10000 | cz + year ,
    data = data_cz_establishments
  )

results6[[2]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_10000 | year + firm_id ,
    data = data_cz_establishments
  )

results6[[3]] <- 
  lfe::felm(
    formula = log_fee ~ N_jt_10000 | year + firm_id + cz,
    data = data_cz_establishments
  )

results6[[4]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_10000 | cz + year ,
    data = data_cz_establishments
  )

results6[[5]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_10000 | year + firm_id ,
    data = data_cz_establishments
  )

results6[[6]] <- 
  lfe::felm(
    formula = log_wage ~ N_jt_10000 | year + firm_id + cz, 
    data = data_cz_establishments
  )


attr(rows, 'position') <- c(6, 7, 8, 9)

result_6 <- 
  list(
    "log(fee)" = results6[[1]],
    "log(fee)" = results6[[2]],
    "log(fee)" = results6[[3]],
    "log(wage)" = results6[[4]],
    "log(wage)" = results6[[5]],
    "log(wage)" = results6[[6]]
  )

modelsummary::modelsummary(
  result_6,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 


## Analyze N_{jt}_100000-------------------------------------------------------

results7 <- list()

results7[[1]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_100000 | cz + year ,
    data = data_cz_establishments
  )

results7[[2]] <- 
  lfe::felm(
    formula = log_fee ~  N_jt_100000 | year + firm_id ,
    data = data_cz_establishments
  )

results7[[3]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_100000 | cz + year ,
    data = data_cz_establishments
  )

results7[[4]] <- 
  lfe::felm(
    formula = log_wage ~  N_jt_100000 | year + firm_id ,
    data = data_cz_establishments
  )

attr(rows, 'position') <- c(6, 7, 8, 9)

result_7 <- 
  list(
    "log(fee)" = results7[[1]],
    "log(fee)" = results7[[2]],
    "log(wage)" = results7[[3]],
    "log(wage)" = results7[[4]]
  )

modelsummary::modelsummary(
  result_7,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 

# Describe N_{jt}--------------------------------------------------------------

N_jt_unique <- 
  data_cz_establishments %>% 
  dplyr::select(N_jt) %>% 
  unique()

quantile(
  N_jt_unique$N_jt, 
  c(0.1, 0.25, 0.5, 0.75, 1)
) %>% 
  kbl(col.names = "N_jt") %>% 
  kable_styling(full_width = FALSE)

## show histogram
histogram_N_jt_unique <- 
  ggplot() + 
  geom_histogram(
    data = N_jt_unique, 
    mapping = aes(x = N_jt),
    bins = 100
  )

print(histogram_N_jt_unique)

data_cz_establishments_N_jt <- 
  data_cz_establishments %>% 
  dplyr::select(N_jt)

histogram_N_jt <- 
  ggplot() + 
  geom_histogram(
    data = data_cz_establishments_N_jt, 
    mapping = aes(x = N_jt),
    bins = 100
  )

print(histogram_N_jt)


# Describe wage (wage_{it})----------------------------------------------------

data_cz_establishments_wage_fee <- 
  data_cz_establishments %>% 
  dplyr::group_by(
    year, 
    cz
  ) %>% 
  dplyr::mutate(
    wage_jt = mean(wage),
    fee_jt = mean(fee)
  ) 

wage_jt_unique <- 
  data_cz_establishments_wage_fee %>% 
  dplyr::select(wage_jt) %>% 
  unique()

summary(wage_jt_unique$wage_jt) 


# Describe fee (fee_{it})------------------------------------------------------ 

fee_jt_unique <- 
  data_cz_establishments_wage_fee %>% 
  dplyr::select(fee_jt) %>% 
  unique() 

summary(fee_jt_unique$fee_jt)

# Analyze using Bartik IV (bartik_cz_t) ----------------------------------------

## 2nd stage

results_b1 <- list()

results_b1[[1]] <- 
  lfe::felm(
    formula = log_fee ~ 1 | cz + year | (N_jt_1000 ~ bartik_cz_t) | cz,
    data = data_cz_establishments_all_ippann
  )

results_b1[[2]] <- 
  lfe::felm(
    formula = log_wage ~ 1 | cz + year | (N_jt_1000 ~ bartik_cz_t) | cz,
    data = data_cz_establishments_all_ippann
  )

rows <- 
  tibble::tribble(
    ~term, ~`(1)`, ~`(2)`,
    'j_FE', 'Y', 'Y',
    't_FE', 'Y', 'Y',
    'i_FE', 'N', 'N', 
  )

attr(rows, 'position') <- c(3, 4, 5, 6)

resultsb1 <- 
  list(
    "log(fee)" = results_b1[[1]],
    "log(wage)" = results_b1[[2]]
  )

modelsummary::modelsummary(
  resultsb1,
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

resultsb3 <- 
  list(
    "N_jt_1000(1st stage cz)" = results_b1[[1]]$stage1
  )

modelsummary::modelsummary(
  resultsb3,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95
)

## reduced form

results_b2 <- list()

results_b2[[1]] <- 
  lfe::felm(
    formula = log_fee ~ bartik_cz_t | cz + year ,
    data = data_cz_establishments_all_ippann
  )

results_b2[[2]] <- 
  lfe::felm(
    formula = log_wage ~ bartik_cz_t | cz + year,
    data = data_cz_establishments_all_ippann
  )

rows <- 
  tibble::tribble(
    ~term, ~`(1)`, ~`(2)`,
    'j_FE', 'Y', 'Y',
    't_FE', 'Y', 'Y',
    'i_FE', 'N', 'N', 
  )

attr(rows, 'position') <- c(3, 4, 5, 6)

resultsb1 <- 
  list(
    "log(fee)" = results_b2[[1]],
    "log(wage)" = results_b2[[2]]
  )

modelsummary::modelsummary(
  resultsb1,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95,
  gof_omit = "AIC|BIC",
  add_rows = rows
) 

# Analyze residuals for cz data-----------------------------------------------

## Regress N_jt on commuting zone (cz) / year fixed effect 

result_u_cz <- 
  lfe::felm(
    formula = N_jt_1000 ~ 1 + cz + year ,
    data = data_cz_establishments_all_ippann
  )

id <- c(1:71486)

## Calculate residuals 

residual_cz <-
  as.data.frame(result_u_cz$residuals) %>% 
  dplyr::rename(u_hat_cz = N_jt_1000) %>% 
  cbind(id)

residual_cz_unique <- 
  residual_cz %>% 
  dplyr::select(
    u_hat_cz
  ) %>% 
  unique() %>% 
  as.data.frame() 

summary(residual_cz_unique)

## show histogram (unique)

histogram_residuals_cz_unique <- 
  ggplot() + 
  geom_histogram(
    data = residual_cz_unique, 
    mapping = aes(x = u_hat_cz),
    bins = 100
  )

print(histogram_residuals_cz_unique)

## show histogram (all)

residual_cz_all <- 
  residual_cz 

histogram_residuals_cz_all <- 
  ggplot() + 
  geom_histogram(
    data = residual_cz_all, 
    mapping = aes(x = u_hat_cz),
    bins = 100
  )

print(histogram_residuals_cz_all)


# Analyze first order difference for cz data ----------------------------------------------

## first stage (cz)

results_diff_cz <- list()

results_diff_cz[[1]] <- 
  lfe::felm(
    formula = log_diff_10_14 ~  share_cz_2010 | 0,
    data = data_cz_establishments_all_ippann_unique
  )

result_diffcz <- 
  list(
    "log_diff_10_14" = results_diff_cz[[1]]
  )

modelsummary::modelsummary(
  result_diffcz,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 

## reduced form (cz)

### wage

results_growth_cz <- list()

results_growth_cz[[1]] <- 
  lfe::felm(
    formula = log_diff_wage ~  share_cz_2010 | 0 ,
    data = data_cz_establishments_all_ippann
  )

result_growthcz <- 
  list(
    "log_diff_wage" = results_growth_cz[[1]]
  )

modelsummary::modelsummary(
  result_growthcz,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 


### fee

results_feegrowth_cz <- list()

results_feegrowth_cz[[1]] <- 
  lfe::felm(
    formula = log_diff_fee ~  share_cz_2010 | 0 ,
    data = data_cz_establishments_all_ippann
  )


result_feegrowthcz <- 
  list(
    "log_diff_fee" = results_feegrowth_cz[[1]]
  )

modelsummary::modelsummary(
  result_feegrowthcz,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 

## structural form (cz)

### wage 

#### 2nd stage

results_s_wage_cz <- list()

results_s_wage_cz[[1]] <- 
  lfe::felm(
    formula = log_diff_wage ~  1 | 0 | (log_diff_10_14 ~ share_cz_2010) | cz ,
    data = data_cz_establishments_all_ippann
  )

results_s_wage_cz[[2]] <- 
  lfe::felm(
    formula = log_diff_wage ~  1 | 0 | (log_diff_10_14 ~ share_cz_2010) | 0 ,
    data = data_cz_establishments_all_ippann
  )

result_swagegrowthcz <- 
  list(
    "log_diff_wage" = results_s_wage_cz[[1]],
    "log_diff_wage" = results_s_wage_cz[[2]]
  )

modelsummary::modelsummary(
  result_swagegrowthcz,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 

#### 1st stage 

resultsb3 <- 
  list(
    "log_diff_10_14(1st stage)" = results_s_wage_cz[[1]]$stage1,
    "log_diff_10_14(1st stage)" = results_s_wage_cz[[2]]$stage1
  )

modelsummary::modelsummary(
  resultsb3,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95
)

### fee  

#### 2nd stage

results_s_fee_cz <- list()

results_s_fee_cz[[1]] <- 
  lfe::felm(
    formula = log_diff_fee ~ 1 | 0 | (log_diff_10_14 ~ share_cz_2010) | cz ,
    data = data_cz_establishments_all_ippann
  )

results_s_fee_cz[[2]] <- 
  lfe::felm(
    formula = log_diff_fee ~ 1 | 0 | (log_diff_10_14 ~ share_cz_2010) | 0,
    data = data_cz_establishments_all_ippann
  )

result_sfeegrowthcz <- 
  list(
    "log_diff_fee" = results_s_fee_cz[[1]],
    "log_diff_fee" = results_s_fee_cz[[2]]
  )

modelsummary::modelsummary(
  result_sfeegrowthcz,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 

#### 1st stage 

resultsb4 <- 
  list(
    "log_diff_fee(1st stage)" = results_s_fee_cz[[1]]$stage1,
    "log_diff_fee(1st stage)" = results_s_fee_cz[[2]]$stage1
  )

modelsummary::modelsummary(
  resultsb4,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95
)


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

id_zip <- c(1:72954)

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
  ) %>% 
  s
View()

#16,061 
data_cz_establishments_all_ippann %>% 
  dplyr::filter(
    year == 2010
  ) %>% 
  View()

# Analyze difference value for zip data ----------------------------------------------

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


# Check for cz data each year-------------------------------------------------

# year == 2010, 16,061 entries
data_cz_establishments_all_ippann_2010 <- 
  data_cz_establishments_all_ippann %>% 
  dplyr::filter(year == 2010) 

data_cz_establishments_all_ippann_2010 <-
  data_cz_establishments_all_ippann_2010 %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    njt = dplyr::n()
  ) %>% 
  dplyr::ungroup()

data_cz_establishments_all_ippann_2010 %>% 
  dplyr::select(
    cz,
    njt
  ) %>% 
  dplyr::filter(
    cz == 100
  ) %>% 
  View()

# year == 2011, 14,632 entries
data_cz_establishments_all_ippann_2011 <- 
  data_cz_establishments_all_ippann %>% 
  dplyr::filter(year == 2011) 

data_cz_establishments_all_ippann_2011 <-
  data_cz_establishments_all_ippann_2011 %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    njt = dplyr::n()
  ) %>% 
  dplyr::ungroup()

data_cz_establishments_all_ippann_2011 %>% 
  dplyr::select(
    cz,
    njt
  ) %>% 
  dplyr::filter(
    cz == 100
  ) %>% 
  View()

# year == 2012, 14,174  entries
data_cz_establishments_all_ippann_2012 <- 
  data_cz_establishments_all_ippann %>% 
  dplyr::filter(year == 2012) 

data_cz_establishments_all_ippann_2012 <-
  data_cz_establishments_all_ippann_2012 %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    njt = dplyr::n()
  ) %>% 
  dplyr::ungroup()

data_cz_establishments_all_ippann_2012 %>% 
  dplyr::select(
    cz,
    njt
  ) %>% 
  dplyr::filter(
    cz == 100
  ) %>% 
  View()

# year == 2013, 13,056 entries
data_cz_establishments_all_ippann_2013 <- 
  data_cz_establishments_all_ippann %>% 
  dplyr::filter(year == 2013) 

data_cz_establishments_all_ippann_2013 <-
  data_cz_establishments_all_ippann_2013 %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    njt = dplyr::n()
  ) %>% 
  dplyr::ungroup()

data_cz_establishments_all_ippann_2013 %>% 
  dplyr::select(
    cz,
    njt
  ) %>% 
  dplyr::filter(
    cz == 10
  ) %>% 
  View()

# year == 2014, 13,563 entries
data_cz_establishments_all_ippann_2014 <- 
  data_cz_establishments_all_ippann %>% 
  dplyr::filter(year == 2014) 

data_cz_establishments_all_ippann_2014 <-
  data_cz_establishments_all_ippann_2014 %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    njt = dplyr::n()
  ) %>% 
  dplyr::ungroup()

data_cz_establishments_all_ippann_2014 %>% 
  dplyr::select(
    cz,
    njt
  ) %>% 
  dplyr::filter(
    cz == 1
  ) %>% 
  View()

## Combine all years' data to one data-----------------------------------------
#71,486 entries
data_cz_establishments_general <-
  rbind(data_cz_establishments_all_ippann_2010, 
        data_cz_establishments_all_ippann_2011) %>% 
  rbind(data_cz_establishments_all_ippann_2012) %>% 
  rbind(data_cz_establishments_all_ippann_2013) %>% 
  rbind(data_cz_establishments_all_ippann_2014) %>% 
  tidyr::drop_na(
    wage,
    fee,
    cz,
    njt
  )

#there are 71,486 establishments in data from 2010 to 2014 (NOT like PANEL structure)
data_cz_establishments_general %>% 
  dplyr::select(
    id_unique,
    year,
    cz,
    njt
  ) %>% 
  dplyr::select(
    id_unique
  ) 


# 220 cz (all variation)
data_cz_establishments_general %>% 
  dplyr::select(cz) %>% 
  table() %>% 
  View()

## Check for 2010 cz data------------------------------------------------------

# 210 variation in cz in 2010
data_cz_establishments_all_ippann_2010 %>% 
  dplyr::select(cz) %>% 
  unique() %>% 
  dplyr::arrange(cz) %>% 
  View()

data_cz_establishments_all_ippann_2014 %>% 
  dplyr::select(
    year,
    cz,
    firm_id,
    njt
  ) %>% 
  dplyr::select(
    njt
  ) %>% 
  unique() %>% 
  View()

quantile(
  data_cz_establishments_all_ippann_2010$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2010 all") %>% 
  kable_styling(full_width = FALSE)

## quantile for 2010 data
njt_unique_2010 <- 
  data_cz_establishments_all_ippann_2010 %>% 
  dplyr::select(njt) %>% 
  unique()

quantile(
  njt_unique_2010$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2010 unique") %>% 
  kable_styling(full_width = FALSE)

## Check for 2011 cz data------------------------------------------------------

# 209 variation in cz in 2011
data_cz_establishments_all_ippann_2011 %>% 
  dplyr::select(cz) %>% 
  unique() %>% 
  dplyr::arrange(cz) %>% 
  View()

quantile(
  data_cz_establishments_all_ippann_2011$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2011 all") %>% 
  kable_styling(full_width = FALSE)

## quantile for 2011 data
njt_unique_2011 <- 
  data_cz_establishments_all_ippann_2011 %>% 
  dplyr::select(njt) %>% 
  unique()

quantile(
  njt_unique_2011$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2011 unique") %>% 
  kable_styling(full_width = FALSE)

## Check for 2012 cz data------------------------------------------------------

# 204 variation in cz in 2012
data_cz_establishments_all_ippann_2012 %>% 
  dplyr::select(cz) %>% 
  unique() %>% 
  dplyr::arrange(cz) %>% 
  View()

quantile(
  data_cz_establishments_all_ippann_2012$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2012 all") %>% 
  kable_styling(full_width = FALSE)

## quantile for 2012 data
njt_unique_2012 <- 
  data_cz_establishments_all_ippann_2012 %>% 
  dplyr::select(njt) %>% 
  unique()

quantile(
  njt_unique_2012$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2012 unique") %>% 
  kable_styling(full_width = FALSE)

## Check for 2013 cz data------------------------------------------------------

# 204 variation in cz in 2013
data_cz_establishments_all_ippann_2013 %>% 
  dplyr::select(cz) %>% 
  unique() %>% 
  dplyr::arrange(cz) %>% 
  View()

quantile(
  data_cz_establishments_all_ippann_2013$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2013 all") %>% 
  kable_styling(full_width = FALSE)

## quantile for 2013 data
njt_unique_2013 <- 
  data_cz_establishments_all_ippann_2013 %>% 
  dplyr::select(njt) %>% 
  unique()

quantile(
  njt_unique_2013$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2013 unique") %>% 
  kable_styling(full_width = FALSE)


## Check for 2014 cz data------------------------------------------------------

# 206 variation in cz in 2014
data_cz_establishments_all_ippann_2014 %>% 
  dplyr::select(cz) %>% 
  unique() %>% 
  dplyr::arrange(cz) %>% 
  View()

quantile(
  data_cz_establishments_all_ippann_2014$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2014 all") %>% 
  kable_styling(full_width = FALSE)

## quantile for 2014 data
njt_unique_2014 <- 
  data_cz_establishments_all_ippann_2014 %>% 
  dplyr::select(njt) %>% 
  unique()

quantile(
  njt_unique_2014$njt, 
  c(0.1, 0.25, 0.5, 0.75, 0.9, 1)
) %>% 
  kbl(col.names = "N_jt 2014 unique") %>% 
  kable_styling(full_width = FALSE)



### check for zip data 
data_zip_establishments_all_ippann <-
  data_zip_establishments_all_ippann %>% 
  dplyr::filter(
    ! zip_three == ""
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

## cz----------------------------------------------------------------------------
data_cz_establishments_all_ippann

g1 <- ggplot() +
  geom_histogram(
    data = data_cz_establishments_all_ippann,
    mapping = aes(x = share_cz_2010),
    bins = 100
  )
print(g1)

g2 <- ggplot() +
  geom_density(
    data = data_cz_establishments_all_ippann,
    mapping = aes(x = share_cz_2010)
  )

print(g2)

g3 <- ggplot() +
  geom_density(
    data = data_cz_establishments_all_ippann_2010,
    mapping = aes(x = share_cz_2010)
  )

print(g3)

g4 <- ggplot() +
  geom_density(
    data = data_cz_establishments_all_ippann_2011,
    mapping = aes(x = share_cz_2010)
  )

print(g4)


g5 <- ggplot() +
  geom_density(
    data = data_cz_establishments_all_ippann_2012,
    mapping = aes(x = share_cz_2010)
  )

print(g5)

g6 <- ggplot() +
  geom_density(
    data = data_cz_establishments_all_ippann_2013,
    mapping = aes(x = share_cz_2010)
  )

print(g6)

g7 <- ggplot() +
  geom_density(
    data = data_cz_establishments_all_ippann_2014,
    mapping = aes(x = share_cz_2010)
  )

print(g7)

## plot data for share_cz_2010 vs share_cz_t-----------------------------------

### 2010

g8 <- ggplot() +
  geom_point(
    data = data_cz_establishments_all_ippann_2010,
    mapping = aes(x = share_cz_2010, y = share_cz_t)
  )

print(g8)

### 2011

g9 <- ggplot() +
  geom_point(
    data = data_cz_establishments_all_ippann_2011,
    mapping = aes(x = share_cz_2010, y = share_cz_t)
  )

print(g9)

### 2012

g10 <- ggplot() +
  geom_point(
    data = data_cz_establishments_all_ippann_2012,
    mapping = aes(x = share_cz_2010, y = share_cz_t)
  )

print(g10)

### 2013

g11 <- ggplot() +
  geom_point(
    data = data_cz_establishments_all_ippann_2013,
    mapping = aes(x = share_cz_2010, y = share_cz_t)
  )

print(g11)


### 2014

g12 <- ggplot() +
  geom_point(
    data = data_cz_establishments_all_ippann_2014,
    mapping = aes(x = share_cz_2010, y = share_cz_t)
  )

print(g12)

### total plot

data_cz_establishments_all_ippann_share <-
  data_cz_establishments_all_ippann %>% 
  dplyr::select(
    year,
    cz,
    share_cz_2010,
    share_cz_t
  ) %>% 
  unique() 

data_cz_establishments_all_ippann_share <-
  data_cz_establishments_all_ippann_share %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    d_2014 = dplyr::if_else(year == 2014, 1, 0),
    share_cz_2014 = max(d_2014*(share_cz_t))
  ) %>% 
  dplyr::select(
    - d_2014
  ) %>% 
  dplyr::ungroup()

data_cz_establishments_all_ippann_share <- 
  data_cz_establishments_all_ippann_share %>% 
  dplyr::mutate(
    share_growth = (share_cz_2014 - share_cz_2010)/share_cz_2010
  )

g13 <- ggplot() +
  geom_point(
    data = data_cz_establishments_all_ippann_share,
    mapping = aes(x = share_cz_2010, y = share_growth)
  )

print(g13)

### total regress
## 2010

data_cz_establishments_all_ippann_share_2010 <-
  data_cz_establishments_all_ippann_share %>% 
  dplyr::filter(year == 2010) %>% 
  tidyr::drop_na(
    share_growth,
    share_cz_2010
  ) %>% 
  dplyr::filter(
    ! share_cz_2010 == 0
  ) 


resultsa <- list()

resultsa[['share_growth']] <- 
  lm(
    formula =  share_growth ~  share_cz_2010,
    data = data_cz_establishments_all_ippann_share_2010
  )

modelsummary::modelsummary(
  resultsa,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 


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


resultsaa <- list()

resultsaa[['share_growth']] <- 
  lm(
    formula =  share_growth ~  share_zip_2010,
    data = data_zip_establishments_all_ippann_share_2010
  )

modelsummary::modelsummary(
  resultsaa,
  stars = TRUE,
  statistic = 'conf.int',
  conf_level = .95 ,
  gof_omit = "AIC|BIC"
) 





