 # Intialize--------------------------------------------------------------------
 rm(list = ls())
 gc()

 library("magrittr")


 # Read data ---------------------------------------------------------------------

 data_establishments <- readRDS(file = "cleaned/data_establishments.rds")
 data_zipcode <- readRDS(file = "cleaned/data_zipcode.rds")
 data_area <- readRDS(file = "cleaned/data_area.rds")
 data_pref_year_minimum_wage <- readRDS(file = "cleaned/data_pref_year_minimum_wage.rds")
 

 # Transform data ---------------------------------------------------------------
 
 # Define commuting zone with first three digits of zip code--------------------
 
 ## Extract first three digits of zip code--------------------------------------
 
 # Drop na in zip code / fee / wage 
 data_zip_establishments <- 
   data_establishments %>% 
   tidyr::drop_na(
     wage,
     fee,
     zipcode
   )   
 
 zip_first_three <- 
   data_zip_establishments %>% 
   dplyr::select(
     zipcode
   ) %>% 
   as.vector()
 
 
 # Create variable for first three digits of zip code
 zip_three <- 
   stringr::str_sub(
     zip_first_three$zipcode,    
     start = 1,
     end = 3
   ) %>% 
   as.data.frame() %>% 
   dplyr::rename(
     zip_three = "."
   )
 
 data_zip_establishments <- 
   data_zip_establishments %>%  
   cbind(zip_three)
 
 data_zip_establishments <- 
   data_zip_establishments %>% 
   dplyr::left_join(
     data_pref_year_minimum_wage,
     by = c("year", "pref")
   )
 
 ## Construct N_{zip_t} (the number of establishments in the commute zone (zip_three) j at year t)----
 data_zip_establishments <- 
   data_zip_establishments %>% 
   dplyr::filter(tokutei == 0) %>% 
   dplyr::group_by(
     year,
     zip_three
   ) %>% 
   dplyr::mutate(
     N_zip_t = dplyr::n() 
   ) %>% 
   dplyr::ungroup() 
 
 data_zip_establishments <-
   data_zip_establishments %>% 
   dplyr::filter(!zip_three == "000")
 
 
 ## Create log transformed wage / fee / additional N_{zip_t}------------------------
 data_zip_establishments <- 
   data_zip_establishments %>% 
   dplyr::mutate(
     log_wage = log(wage),
     log_fee = log(fee),
     N_zip_t_10 = N_zip_t / 10,
     N_zip_t_100 = N_zip_t / 100,
     N_zip_t_1000 = N_zip_t / 1000,
     N_zip_t_10000 = N_zip_t / 10000,
     N_zip_t_100000 = N_zip_t / 100000
   ) 
 
 ## Create dummy variables------------------------------------------------------
 data_zip_establishments <- 
   data_zip_establishments %>% 
   dplyr::mutate( 
     D_2 = dplyr::if_else(N_zip_t >= 2, 1, 0),
     D_3 = dplyr::if_else(N_zip_t >= 3, 1, 0), 
     D_4 = dplyr::if_else(N_zip_t >= 4, 1, 0),
     D_5 = dplyr::if_else(N_zip_t >= 5, 1, 0),
     D_6 = dplyr::if_else(N_zip_t >= 6, 1, 0),
     D_7 = dplyr::if_else(N_zip_t >= 7, 1, 0),
     D_8 = dplyr::if_else(N_zip_t >= 8, 1, 0),
     D_9 = dplyr::if_else(N_zip_t >= 9, 1, 0),
     D_10 = dplyr::if_else(N_zip_t >= 10, 1, 0),
     D_11 = dplyr::if_else(N_zip_t >= 11, 1, 0),
     D_12 = dplyr::if_else(N_zip_t >= 12, 1, 0),
     D_13 = dplyr::if_else(N_zip_t >= 13, 1, 0)
   )
 
 ## Add for each 20 quantile (zip)
 
 data_zip_establishments <- 
   data_zip_establishments %>% 
   dplyr::mutate(
     D_20 = dplyr::if_else(N_zip_t < 38.4, 1, 0),
     D_40 = dplyr::if_else(N_zip_t >= 38.4 & N_zip_t < 118.4, 1, 0),
     D_60 = dplyr::if_else(N_zip_t >= 118.4 & N_zip_t < 195.6, 1, 0),
     D_80 = dplyr::if_else(N_zip_t >= 195.6 & N_zip_t < 491, 1, 0),
     D_100 = dplyr::if_else(N_zip_t >= 491, 1, 0)
   )
 
 ## Add for each 10 quantile (zip)
 
 data_zip_establishments <- 
   data_zip_establishments %>% 
   dplyr::mutate(
     D10 = dplyr::if_else(N_zip_t < 19.7, 1, 0),
     D20 = dplyr::if_else(N_zip_t >= 19.7 & N_zip_t < 38.4, 1, 0),
     D30 = dplyr::if_else(N_zip_t >= 38.4 & N_zip_t < 57.1, 1, 0),
     D40 = dplyr::if_else(N_zip_t >= 57.1 & N_zip_t < 75.8, 1, 0),
     D50 = dplyr::if_else(N_zip_t >= 75.8 & N_zip_t < 95, 1, 0),
     D60 = dplyr::if_else(N_zip_t >= 95 & N_zip_t < 118.4, 1, 0),
     D70 = dplyr::if_else(N_zip_t >= 118.4 & N_zip_t < 152.9, 1, 0),
     D80 = dplyr::if_else(N_zip_t >= 152.9 & N_zip_t < 195.6, 1, 0),
     D90 = dplyr::if_else(N_zip_t >= 195.6 & N_zip_t < 245.6, 1, 0),
     D100 = dplyr::if_else(N_zip_t >= 245.6, 1, 0)
   )
 
 saveRDS(data_zip_establishments, file = "output/data_zip_establishments.rds")

 
 # Create Bartik IV (bartik_zip_t)---------------------------------------------------------
 
 ## Compute the share  of Tokutei jigyousha 
 
 ### Create data with both Ippan and Tokutei jigyousha
 
 data_zip_establishments_all <- 
   data_establishments %>% 
   tidyr::drop_na(
     wage,
     fee,
     zipcode
   )   
 
 zip_first_three <- 
   data_zip_establishments_all %>% 
   dplyr::select(
     zipcode
   ) %>% 
   as.vector()
 
 zip_three <- 
   stringr::str_sub(
     zip_first_three$zipcode,    
     start = 1,
     end = 3
   ) %>% 
   as.data.frame() %>% 
   dplyr::rename(
     zip_three = "."
   )
 
 data_zip_establishments_all <- 
   data_zip_establishments_all %>%  
   cbind(zip_three) 
 
 data_zip_establishments_all <- 
   data_zip_establishments_all %>% 
   dplyr::group_by(
     year,
     zip_three
   ) %>% 
   dplyr::mutate(
     n_zip_t = dplyr::n(),
     n_tokutei_zip_t = sum(tokutei),
     share_zip_t = n_tokutei_zip_t / n_zip_t,
     n_ippann_zip_t = dplyr::n() - sum(tokutei),
     share_zip_t_ippann = n_ippann_zip_t / n_zip_t
   ) %>% 
   dplyr::ungroup() 
 
 ### Create bartik_zip_t
 
 data_zip_establishments_all <- 
   data_zip_establishments_all %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     d_2010 = dplyr::if_else(year == 2010, 1, 0),
     share_zip_2010 = max(d_2010*(share_zip_t)),
     share_zip_2010_ippann = max(d_2010*(share_zip_t_ippann)),
     bartik_zip_t = share_zip_2010*(share_zip_t)
   ) %>% 
   dplyr::select(
     - d_2010
   ) %>% 
   dplyr::ungroup()
   
 saveRDS(data_zip_establishments_all, file = "output/data_zip_establishments_all.rds")
   
 ### Select only ippann jigyosho
 
 data_zip_establishments_all_ippann <- 
   data_zip_establishments_all %>% 
   dplyr::filter(
     tokutei == 0
   ) %>% 
   dplyr::group_by(
     year,
     zip_three
   ) %>% 
   dplyr::mutate(
     N_zip_t = dplyr::n()
   ) %>% 
   dplyr::ungroup() 
 
 data_zip_establishments_all_ippann <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::mutate(
     log_fee = log(fee),
     log_wage = log(wage),
     N_zip_t_10 = N_zip_t / 10,
     N_zip_t_100 = N_zip_t / 100,
     N_zip_t_1000 = N_zip_t / 1000,
     N_zip_t_10000 = N_zip_t / 10000,
     N_zip_t_100000 = N_zip_t / 100000
   ) %>% 
   dplyr::filter(!zip_three == "000") %>% 
   dplyr::filter(!zip_three == "")
 
 saveRDS(data_zip_establishments_all_ippann, file = "output/data_zip_establishments_all_ippann.rds")

 
 ## create the number of the general establishments in 2010 variable for weight--
 
 weights_general_2010 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::select(
     year,
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::filter(
     year == 2010
   ) %>% 
   dplyr::distinct() %>% 
   dplyr::select(
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::rename(
     weights = N_zip_t
   ) 
 
 ## match the he weights data by cz
 
 data_zip_establishments_all_ippann <- 
   dplyr::full_join(
     data_zip_establishments_all_ippann,
     weights_general_2010,
     by = "zip_three"
   )
 
 saveRDS(data_zip_establishments_all_ippann, file = "output/data_zip_establishments_all_ippann.rds")
 
 # Create differentiated variables ---------------------------------------------
 
 ## the number of general establishments-----------------------------------------
 
 data_zip_establishments_all_ippann_2010 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2010
   ) %>% 
   dplyr::select(
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::rename(
     N_zip2010 = N_zip_t
   ) %>% 
   dplyr::mutate(
     log_N_zip2010 = log(N_zip2010)
   ) %>% 
   dplyr::distinct() 
 
 data_zip_establishments_all_ippann_2011 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2011
   ) %>% 
   dplyr::select(
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::rename(
     N_zip2011 = N_zip_t
   ) %>% 
   dplyr::mutate(
     log_N_zip2011 = log(N_zip2011)
   ) %>%
   dplyr::distinct() 
 
 data_zip_establishments_all_ippann_2012 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2012
   ) %>% 
   dplyr::select(
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::rename(
     N_zip2012 = N_zip_t
   ) %>% 
   dplyr::mutate(
     log_N_zip2012 = log(N_zip2012)
   ) %>%
   dplyr::distinct() 
 
 data_zip_establishments_all_ippann_2013 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2013
   ) %>% 
   dplyr::select(
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::rename(
     N_zip2013 = N_zip_t
   ) %>% 
   dplyr::mutate(
     log_N_zip2013 = log(N_zip2013)
   ) %>%
   dplyr::distinct() 
 
 data_zip_establishments_all_ippann_2014 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2014
   ) %>% 
   dplyr::select(
     zip_three,
     N_zip_t
   ) %>% 
   dplyr::rename(
     N_zip2014 = N_zip_t
   ) %>% 
   dplyr::mutate(
     log_N_zip2014 = log(N_zip2014)
   ) %>%
   dplyr::distinct() 
 
 ## merge all years' data
 
 data_zip_diff <- 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2010,
     data_zip_establishments_all_ippann_2011,
     by = "zip_three"
   ) %>% 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2012,
     by = "zip_three"
   ) %>% 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2013,
     by = "zip_three"
   ) %>% 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2014,
     by = "zip_three"
   ) %>% 
   dplyr::mutate(
     diff_2011 = N_zip2011 - N_zip2010,
     diff_2012 = N_zip2012 - N_zip2011,
     diff_2013 = N_zip2013 - N_zip2012,
     diff_2014 = N_zip2014 - N_zip2013,
     log_diff_2011 = log_N_zip2011 - log_N_zip2010,
     log_diff_2012 = log_N_zip2012 - log_N_zip2011,
     log_diff_2013 = log_N_zip2013 - log_N_zip2012,
     log_diff_2014 = log_N_zip2014 - log_N_zip2013
   ) %>% 
   dplyr::mutate(
     log_diff_10_14 = log_N_zip2014 - log_N_zip2010,
     diff_14_10 = N_zip2014 - N_zip2010
   ) %>% 
   dplyr::mutate(
     diff_14_10_1000 = diff_14_10 / 1000,
     N_growth = (N_zip2014 - N_zip2010) / N_zip2010
   )
 
 ## match data_zip_diff and data_zip_establishments_all_ippann by zip_three
 
 data_zip_establishments_all_ippann <- 
   dplyr::inner_join(
     data_zip_establishments_all_ippann, 
     data_zip_diff,
     by = "zip_three"
   )
 
 data_zip_establishments_all_ippann <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::mutate(
     diff = dplyr::if_else(
       year == 2011, diff_2011, NA_real_
     ),
     diff = dplyr::if_else(
       year == 2012, diff_2012, diff
     ),
     diff = dplyr::if_else(
       year == 2013, diff_2013, diff
     ),
     diff = dplyr::if_else(
      year == 2014, diff_2014, diff
     )
   ) %>% 
   dplyr::mutate(
     log_diff = dplyr::if_else(
       year == 2011, log_diff_2011, NA_real_
     ),
     log_diff = dplyr::if_else(
       year == 2012, log_diff_2012, diff
     ),
     log_diff = dplyr::if_else(
       year == 2013, log_diff_2013, diff
     ),
     log_diff = dplyr::if_else(
       year == 2014, log_diff_2014, diff
     )
   ) %>%
   dplyr::mutate(
     diff_100 = diff / 100,
     diff_1000 = diff / 1000,
     diff_10000 = diff / 10000
   )

 
 saveRDS(data_zip_establishments_all_ippann, file = "output/data_zip_establishments_all_ippann.rds")
 
 
 ## wage / fee -------------------------------------------------------------------
 
 data_zip_establishments_all_ippann_2010 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2010
   ) %>% 
   dplyr::select(
     zip_three,
     wage,
     fee
   ) %>% 
   dplyr::rename(
     wage2010 = wage,
     fee2010 = fee
   ) %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     wage2010_avr = mean(wage2010),
     fee2010_avr = mean(fee2010)
   ) %>% 
   dplyr::mutate(
     log_wage2010_avr = log(wage2010_avr),
     log_fee2010_avr = log(fee2010_avr)
   ) %>% 
   dplyr::select(
     zip_three,
     wage2010_avr,
     fee2010_avr,
     log_wage2010_avr,
     log_fee2010_avr
   ) %>% 
   dplyr::distinct() %>% 
   dplyr::ungroup() 
 
 data_zip_establishments_all_ippann_2011 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2011
   ) %>% 
   dplyr::select(
     zip_three,
     wage,
     fee
   ) %>% 
   dplyr::rename(
     wage2011 = wage,
     fee2011 = fee
   ) %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     wage2011_avr = mean(wage2011),
     fee2011_avr = mean(fee2011)
   ) %>% 
   dplyr::mutate(
     log_wage2011_avr = log(wage2011_avr),
     log_fee2011_avr = log(fee2011_avr)
   ) %>% 
   dplyr::select(
     zip_three,
     wage2011_avr,
     fee2011_avr,
     log_wage2011_avr,
     log_fee2011_avr
   ) %>% 
   dplyr::distinct() %>% 
   dplyr::ungroup() 
 
 data_zip_establishments_all_ippann_2012 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2012
   ) %>% 
   dplyr::select(
     zip_three,
     wage,
     fee
   ) %>% 
   dplyr::rename(
     wage2012 = wage,
     fee2012 = fee
   ) %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     wage2012_avr = mean(wage2012),
     fee2012_avr = mean(fee2012)
   ) %>% 
   dplyr::mutate(
     log_wage2012_avr = log(wage2012_avr),
     log_fee2012_avr = log(fee2012_avr)
   ) %>% 
   dplyr::select(
     zip_three,
     wage2012_avr,
     fee2012_avr,
     log_wage2012_avr,
     log_fee2012_avr
   ) %>% 
   dplyr::distinct() %>% 
   dplyr::ungroup() 
 
 data_zip_establishments_all_ippann_2013 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2013
   ) %>% 
   dplyr::select(
     zip_three,
     wage,
     fee
   ) %>% 
   dplyr::rename(
     wage2013 = wage,
     fee2013 = fee
   ) %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     wage2013_avr = mean(wage2013),
     fee2013_avr = mean(fee2013)
   ) %>% 
   dplyr::mutate(
     log_wage2013_avr = log(wage2013_avr),
     log_fee2013_avr = log(fee2013_avr)
   ) %>% 
   dplyr::select(
     zip_three,
     wage2013_avr,
     fee2013_avr,
     log_wage2013_avr,
     log_fee2013_avr
   ) %>% 
   dplyr::distinct() %>% 
   dplyr::ungroup()
 
 data_zip_establishments_all_ippann_2014 <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::filter(
     year == 2014
   ) %>% 
   dplyr::select(
     zip_three,
     wage,
     fee
   ) %>% 
   dplyr::rename(
     wage2014 = wage,
     fee2014 = fee
   ) %>% 
   dplyr::group_by(
     zip_three
   ) %>% 
   dplyr::mutate(
     wage2014_avr = mean(wage2014),
     fee2014_avr = mean(fee2014)
   ) %>% 
   dplyr::mutate(
     log_wage2014_avr = log(wage2014_avr),
     log_fee2014_avr = log(fee2014_avr)
   ) %>% 
   dplyr::select(
     zip_three,
     wage2014_avr,
     fee2014_avr,
     log_wage2014_avr,
     log_fee2014_avr
   ) %>% 
   dplyr::distinct() %>% 
   dplyr::ungroup()
 
 ## merge all years' data
 
 data_zip_diff_wage_fee <- 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2010,
     data_zip_establishments_all_ippann_2011,
     by = "zip_three"
   ) %>% 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2012,
     by = "zip_three"
   ) %>% 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2013,
     by = "zip_three"
   ) %>% 
   dplyr::full_join(
     data_zip_establishments_all_ippann_2014,
     by = "zip_three"
   ) %>% 
   dplyr::mutate(
     diffwage_2011 = wage2011_avr - wage2010_avr,
     diffwage_2012 = wage2012_avr - wage2011_avr,
     diffwage_2013 = wage2013_avr - wage2012_avr,
     diffwage_2014 = wage2014_avr - wage2013_avr,
     diffwage_14_10 = wage2014_avr - wage2010_avr,
     difffee_2011 = fee2011_avr - fee2010_avr,
     difffee_2012 = fee2012_avr - fee2011_avr,
     difffee_2013 = fee2013_avr - fee2012_avr,
     difffee_2014 = fee2014_avr - fee2013_avr,
     difffee_14_10 = fee2014_avr - fee2010_avr
   ) %>% 
   dplyr::mutate(
     log_diffwage_2011 = log_wage2011_avr - log_wage2010_avr,
     log_diffwage_2012 = log_wage2012_avr - log_wage2011_avr,
     log_diffwage_2013 = log_wage2013_avr - log_wage2012_avr,
     log_diffwage_2014 = log_wage2014_avr - log_wage2013_avr,
     log_diffwage_14_10 = log_wage2014_avr - log_wage2010_avr,
     log_difffee_2011 = log_fee2011_avr - log_fee2010_avr,
     log_difffee_2012 = log_fee2012_avr - log_fee2011_avr,
     log_difffee_2013 = log_fee2013_avr - log_fee2012_avr,
     log_difffee_2014 = log_fee2014_avr - log_fee2013_avr,
     log_difffee_14_10 = log_fee2014_avr - log_fee2010_avr
   ) %>% 
   dplyr::mutate(
     wage_growth_2011 = diffwage_2011 / wage2010_avr,
     wage_growth_2012 = diffwage_2012 / wage2011_avr,
     wage_growth_2013 = diffwage_2013 / wage2012_avr,
     wage_growth_2014 = diffwage_2014 / wage2013_avr,
     wage_growth_14_10 = diffwage_14_10 / wage2010_avr,
     fee_growth_2011 = difffee_2011 / fee2010_avr,
     fee_growth_2012 = difffee_2012 / fee2011_avr,
     fee_growth_2013 = difffee_2013 / fee2012_avr,
     fee_growth_2014 = difffee_2014 / fee2013_avr,
     fee_growth_14_10 = difffee_14_10 / fee2010_avr
   )
 
 
 data_zip_establishments_all_ippann <- 
   dplyr::inner_join(
     data_zip_establishments_all_ippann, 
     data_zip_diff_wage_fee,
     by = "zip_three"
   )
 
 data_zip_establishments_all_ippann <- 
   data_zip_establishments_all_ippann %>% 
   dplyr::mutate(
     diff_wage = dplyr::if_else(
       year == 2011, diffwage_2011, NA_real_
     ),
     diff_wage = dplyr::if_else(
       year == 2012, diffwage_2012, diff_wage
     ),
     diff_wage = dplyr::if_else(
       year == 2013, diffwage_2013, diff_wage
     ),
     diff_wage = dplyr::if_else(
       year == 2014, diffwage_2014, diff_wage
     ) 
   ) %>% 
   dplyr::mutate(
     diff_fee = dplyr::if_else(
       year == 2011, difffee_2011, NA_real_
     ),
     diff_fee = dplyr::if_else(
       year == 2012, difffee_2012, diff_fee
     ),
     diff_fee = dplyr::if_else(
       year == 2013, difffee_2013, diff_fee
     ),
     diff_fee = dplyr::if_else(
       year == 2014, difffee_2014, diff_fee
     ) 
   ) %>% 
   dplyr::mutate(
     log_diff_wage = dplyr::if_else(
       year == 2011, log_diffwage_2011, NA_real_
     ),
     log_diff_wage = dplyr::if_else(
       year == 2012, log_diffwage_2012, log_diff_wage
     ),
     log_diff_wage = dplyr::if_else(
       year == 2013, log_diffwage_2013, log_diff_wage
     ),
     log_diff_wage = dplyr::if_else(
       year == 2014, log_diffwage_2014, log_diff_wage
     ) 
   ) %>% 
   dplyr::mutate(
     log_diff_fee = dplyr::if_else(
       year == 2011, log_difffee_2011, NA_real_
     ),
     log_diff_fee = dplyr::if_else(
       year == 2012, log_difffee_2012, log_diff_fee
     ),
     log_diff_fee = dplyr::if_else(
       year == 2013, log_difffee_2013, log_diff_fee
     ),
     log_diff_fee = dplyr::if_else(
       year == 2014, log_difffee_2014, log_diff_fee
     ) 
   ) %>% 
   dplyr::mutate(
     wage_growth = dplyr::if_else(
       year == 2011, wage_growth_2011, NA_real_
     ),
     wage_growth = dplyr::if_else(
       year == 2012, wage_growth_2012, wage_growth
     ),
     wage_growth = dplyr::if_else(
       year == 2013, wage_growth_2013, wage_growth
     ),
     wage_growth = dplyr::if_else(
       year == 2014, wage_growth_2014, wage_growth
     )
   ) %>% 
   dplyr::mutate(
     fee_growth = dplyr::if_else(
       year == 2011, fee_growth_2011, NA_real_
     ),
     fee_growth = dplyr::if_else(
       year == 2012, fee_growth_2012, fee_growth
     ),
     fee_growth = dplyr::if_else(
       year == 2013, fee_growth_2013, fee_growth
     ),
     fee_growth = dplyr::if_else(
       year == 2014, fee_growth_2014, fee_growth
     )
   )
 
 saveRDS(data_zip_establishments_all_ippann, file = "output/data_zip_establishments_all_ippann.rds")
 
 ## data for first stage regression
 
 data_zip_establishments_all_ippann_unique <-
   data_zip_establishments_all_ippann %>% 
   dplyr::select(
     zip_three,
     log_diff_10_14,
     share_zip_2010,
     diff_14_10,
     diff_14_10_1000,
     N_growth,
     weights
   ) %>% 
   tidyr::drop_na(
     log_diff_10_14,
     N_growth,
     diff_14_10,
     diff_14_10_1000
   ) %>% 
   dplyr::distinct()
 
 stats::quantile(
   data_zip_establishments_all_ippann_unique$N_growth, 
   c(0.01, 0.9, 0.95, 0.99)
 ) 
 
 data_zip_establishments_all_ippann_unique <- 
   data_zip_establishments_all_ippann_unique %>% 
   dplyr::mutate(
     N_growth_w1 = N_growth,
     N_growth_w5 = N_growth,
     N_growth_w10 = N_growth
   ) %>% 
   dplyr::mutate(
     N_growth_w1 = ifelse(N_growth > 1.0766667, 1.0766667, N_growth),
     N_growth_w5 = ifelse(N_growth > 0.3890323, 0.3890323, N_growth),
     N_growth_w10 = ifelse(N_growth > 0.1550000, 0.1550000, N_growth)
   )
 
 saveRDS(data_zip_establishments_all_ippann_unique, file = "output/data_zip_establishments_all_ippann_unique.rds")
 
 ## data for reduced form analysis
 
 data_zip_establishments_all_ippann_unique_wage <-
   data_zip_establishments_all_ippann %>% 
   dplyr::select(
     zip_three,
     log_diff_10_14,
     log_diffwage_14_10,
     diffwage_14_10,
     wage_growth_14_10,
     diff_14_10,
     share_zip_2010,
     diff_14_10_1000,
     N_growth,
     weights
   ) %>% 
   tidyr::drop_na(
     log_diffwage_14_10,
     log_diff_10_14,
     diffwage_14_10,
     diff_14_10,
     wage_growth_14_10,
     diff_14_10_1000,
     N_growth,
     weights
   ) %>% 
   dplyr::distinct()
 
 stats::quantile(
   data_zip_establishments_all_ippann_unique_wage$wage_growth_14_10, 
   c(0.01, 0.9, 0.95, 0.99)
 ) 
 
 data_zip_establishments_all_ippann_unique_wage <- 
   data_zip_establishments_all_ippann_unique_wage %>% 
   dplyr::mutate(
     N_growth_w1 = N_growth,
     N_growth_w5 = N_growth,
     N_growth_w10 = N_growth,
     wage_growth_14_10_1 = wage_growth_14_10
   ) %>% 
   dplyr::mutate(
     N_growth_w1 = ifelse(N_growth > 1.0766667, 1.0766667, N_growth),
     N_growth_w5 = ifelse(N_growth > 0.3890323, 0.3890323, N_growth),
     N_growth_w10 = ifelse(N_growth > 0.1550000, 0.1550000, N_growth),
     wage_growth_14_10_1 = ifelse(wage_growth_14_10 > 0.9139159, 0.9139159, wage_growth_14_10)
   )
 
 saveRDS(data_zip_establishments_all_ippann_unique_wage, file = "output/data_zip_establishments_all_ippann_unique_wage.rds")
 
 
 data_zip_establishments_all_ippann_unique_fee <-
   data_zip_establishments_all_ippann %>% 
   dplyr::select(
     zip_three,
     log_diff_10_14,
     log_difffee_14_10,
     difffee_14_10,
     diff_14_10,
     fee_growth_14_10,
     share_zip_2010,
     diff_14_10_1000,
     N_growth,
     weights
   ) %>% 
   tidyr::drop_na(
     log_difffee_14_10,
     log_diff_10_14,
     difffee_14_10,
     diff_14_10,
     fee_growth_14_10,
     diff_14_10_1000,
     N_growth,
     weights
   ) %>% 
   dplyr::distinct()
 
 stats::quantile(
   data_zip_establishments_all_ippann_unique_fee$fee_growth_14_10, 
   c(0.01, 0.9, 0.95, 0.99)
 ) 
 
 data_zip_establishments_all_ippann_unique_fee <- 
   data_zip_establishments_all_ippann_unique_fee %>% 
   dplyr::mutate(
     N_growth_w1 = N_growth,
     N_growth_w5 = N_growth,
     N_growth_w10 = N_growth,
     fee_growth_14_10_1 = fee_growth_14_10
   ) %>% 
   dplyr::mutate(
     N_growth_w1 = ifelse(N_growth > 1.0766667, 1.0766667, N_growth),
     N_growth_w5 = ifelse(N_growth > 0.3890323, 0.3890323, N_growth),
     N_growth_w10 = ifelse(N_growth > 0.1550000, 0.1550000, N_growth),
     fee_growth_14_10_1 = ifelse(fee_growth_14_10 > 0.9093623, 0.9093623, fee_growth_14_10)
   )
 
 saveRDS(data_zip_establishments_all_ippann_unique_fee, file = "output/data_zip_establishments_all_ippann_unique_fee.rds")
 
 ## data for weighted least square
 data_zip_establishments_all_ippann_unique_weights <- 
   data_zip_establishments_all_ippann_unique %>% 
   tidyr::drop_na(
     weights
   )
 
 saveRDS(data_zip_establishments_all_ippann_unique_weights, file = "output/data_zip_establishments_all_ippann_unique_weights.rds")
 
 
 