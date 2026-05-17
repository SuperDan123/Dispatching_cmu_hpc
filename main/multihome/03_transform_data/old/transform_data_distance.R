 # Intialize--------------------------------------------------------------------
 rm(list = ls())

 #rm(list = setdiff(ls(),c("data_cz_establishments_distance_2010" )))
 
 library("magrittr")

 data_cz_establishments <- 
   here::here(file = "output/data_cz_establishments.rds") %>% 
   readRDS()

 data_area_area_distance <- 
   here::here(file = "cleaned/data_area_area_distance.rds") %>% 
   readRDS()
 
 data_cz_area_distance <- 
   data_cz_establishments %>% 
   tidyr::drop_na(
     firm_id
   )
 
 data_area_area_distance_2nd_min <- 
   data_area_area_distance %>% 
   dplyr::arrange(
     area_code_1
   ) %>% 
   dplyr::group_by(
     area_code_1
   ) %>% 
   dplyr::filter(
    ! distance == 0
   ) %>% 
   dplyr::mutate(
     distance_r = min(distance)
   ) %>% 
   dplyr::filter(
     distance == distance_r
   ) %>% 
   dplyr::ungroup() %>% 
   dplyr::select(
     area_code_1,
     distance_r,
   ) %>% 
   dplyr::distinct()
 
 # 2010 data---------------------------------------------------------------------
 
 data_cz_area_distance_2010 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2010
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count > 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2010_2 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2010
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count == 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2010_2 <- 
   data_cz_area_distance_2010_2 %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::mutate(
     n_firm = dplyr::n()
   )
 
 data_cz_area_distance_2010_2_1 <-
   data_cz_area_distance_2010_2 %>% 
   dplyr::filter(
     n_firm > 1
   ) %>% 
   dplyr::mutate(
     distance_min_r = 0
   ) %>% 
   dplyr::select(
     - n_firm
   )
 
 data_cz_area_distance_2010_2_0 <-
   data_cz_area_distance_2010_2 %>% 
   dplyr::filter(
     n_firm == 1
   ) %>% 
   dplyr::mutate(
     area_code_1 = area_code
   )
 
 data_cz_area_distance_2010_2_0 <- 
   dplyr::left_join(
     data_cz_area_distance_2010_2_0,
     data_area_area_distance_2nd_min,
     by = "area_code_1"
   ) %>% 
   dplyr::mutate(
     distance_min_r = distance_r
   ) %>% 
   dplyr::select(
     - n_firm,
     - area_code_1,
     - distance_r
   )
 
 data_cz_area_distance_2010_2 <- 
   rbind(data_cz_area_distance_2010_2_1,
         data_cz_area_distance_2010_2_0)
 
 t = subset(data_cz_area_distance_2010_2, is.na(distance_min_r))
 View(t)
   
 data_cz_establishments_distance_2010 <- data.frame()
 
 for (i in unique(data_cz_area_distance_2010$firm_id)) {
   data_cz_area_distance_2010_i <- 
     data_cz_area_distance_2010 %>% 
     dplyr::filter(
       firm_id == i
     ) 
   
   combn_i <- 
     gtools::permutations(
       n = length(unique(data_cz_area_distance_2010_i$area_code)),
       r = 2,
       v = unique(data_cz_area_distance_2010_i$area_code),
       repeats.allowed = F
     ) %>% 
     as.data.frame() %>% 
     dplyr::rename(
       area_code_1 = V1,
       area_code_2 = V2
     ) 
   
   combn_i <-
     dplyr::left_join(
       combn_i,
       data_area_area_distance,
       by = c("area_code_1", "area_code_2")
     ) %>% 
     dplyr::group_by(
       area_code_1
     ) %>% 
     dplyr::mutate(
       distance_min = min(distance)
     ) 
   
   combn_i_unique <- 
     combn_i %>% 
     dplyr::filter(
       distance == distance_min
     ) %>% 
     dplyr::distinct()
   
   data_cz_area_distance_2010_i <- 
     data_cz_area_distance_2010_i %>% 
     dplyr::mutate(
       area_code_1 = area_code
     ) 
   
   data_cz_area_distance_2010_i <- 
     dplyr::left_join(
       data_cz_area_distance_2010_i,
       combn_i_unique,
       by = "area_code_1"
     ) %>% 
     dplyr::select(
       - distance
     )
   
   data_cz_area_distance_2010_i <- 
     data_cz_area_distance_2010_i %>% 
     dplyr::group_by(
       area_code
     ) %>% 
     dplyr::mutate(
       area_code_count = dplyr::n()
     ) %>% 
     dplyr::mutate(
       distance_min_r = ifelse(area_code_count > 1, 0, distance_min)
     ) %>% 
     dplyr::select(
       - area_code_1,
       - area_code_2,
       - distance_min,
       - area_code_count
     )
   
   data_cz_establishments_distance_2010 <- 
     data_cz_establishments_distance_2010 %>% 
     dplyr::bind_rows(data_cz_area_distance_2010_i)
   
 }
 
 data_cz_establishments_distance_2010 <- 
   rbind(data_cz_establishments_distance_2010,
         data_cz_area_distance_2010_2)
 
 s <- data_cz_establishments_distance_2010 %>% 
   dplyr::select(
     firm_id, 
     area_code,
     id_unique,
     distance_min_r
   )
 
 data_area_area_distance %>% 
   dplyr::filter(
     area_code_1 == 14109
   ) %>% 
   View()
 
 t = subset(s, is.na(distance_min_r))
 View(t)
 
 # 2011 data---------------------------------------------------------------------
 
 data_cz_area_distance_2011 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2011
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count > 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2011_2 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2011
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count == 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2011_2 <- 
   data_cz_area_distance_2011_2 %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::mutate(
     n_firm = dplyr::n()
   )
 
 data_cz_area_distance_2011_2_1 <-
   data_cz_area_distance_2011_2 %>% 
   dplyr::filter(
     n_firm > 1
   ) %>% 
   dplyr::mutate(
     distance_min_r = 0
   ) %>% 
   dplyr::select(
     - n_firm
   )
 
 data_cz_area_distance_2011_2_0 <-
   data_cz_area_distance_2011_2 %>% 
   dplyr::filter(
     n_firm == 1
   ) %>% 
   dplyr::mutate(
     area_code_1 = area_code
   )
 
 data_cz_area_distance_2011_2_0 <- 
   dplyr::left_join(
     data_cz_area_distance_2011_2_0,
     data_area_area_distance_2nd_min,
     by = "area_code_1"
   ) %>% 
   dplyr::mutate(
     distance_min_r = distance_r
   ) %>% 
   dplyr::select(
     - n_firm,
     - area_code_1,
     - distance_r
   )
 
 data_cz_area_distance_2011_2 <- 
   rbind(data_cz_area_distance_2011_2_1,
         data_cz_area_distance_2011_2_0)
 
 data_cz_establishments_distance_2011 <- data.frame()
 
 for (i in unique(data_cz_area_distance_2011$firm_id)) {
   data_cz_area_distance_2011_i <- 
     data_cz_area_distance_2011 %>% 
     dplyr::filter(
       firm_id == i
     ) 
   
   combn_i <- 
     gtools::permutations(
       n = length(unique(data_cz_area_distance_2011_i$area_code)),
       r = 2,
       v = unique(data_cz_area_distance_2011_i$area_code),
       repeats.allowed = F
     ) %>% 
     as.data.frame() %>% 
     dplyr::rename(
       area_code_1 = V1,
       area_code_2 = V2
     ) 
   
   combn_i <-
     dplyr::left_join(
       combn_i,
       data_area_area_distance,
       by = c("area_code_1", "area_code_2")
     ) %>% 
     dplyr::group_by(
       area_code_1
     ) %>% 
     dplyr::mutate(
       distance_min = min(distance)
     ) 
   
   combn_i_unique <- 
     combn_i %>% 
     dplyr::filter(
       distance == distance_min
     ) %>% 
     dplyr::distinct()
   
   data_cz_area_distance_2011_i <- 
     data_cz_area_distance_2011_i %>% 
     dplyr::mutate(
       area_code_1 = area_code
     ) 
   
   data_cz_area_distance_2011_i <- 
     dplyr::left_join(
       data_cz_area_distance_2011_i,
       combn_i_unique,
       by = "area_code_1"
     ) %>% 
     dplyr::select(
       - distance
     )
   
   data_cz_area_distance_2011_i <- 
     data_cz_area_distance_2011_i %>% 
     dplyr::group_by(
       area_code
     ) %>% 
     dplyr::mutate(
       area_code_count = dplyr::n()
     ) %>% 
     dplyr::mutate(
       distance_min_r = ifelse(area_code_count > 1, 0, distance_min)
     ) %>% 
     dplyr::select(
       - area_code_1,
       - area_code_2,
       - distance_min,
       - area_code_count
     )
   
   data_cz_establishments_distance_2011 <- 
     data_cz_establishments_distance_2011 %>% 
     dplyr::bind_rows(data_cz_area_distance_2011_i)
   
 }
 
 data_cz_establishments_distance_2011 <- 
   rbind(data_cz_establishments_distance_2011,
         data_cz_area_distance_2011_2)
 
 # 2012 data---------------------------------------------------------------------
 
 data_cz_area_distance_2012 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2012
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count > 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2012_2 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2012
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count == 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2012_2 <- 
   data_cz_area_distance_2012_2 %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::mutate(
     n_firm = dplyr::n()
   )
 
 data_cz_area_distance_2012_2_1 <-
   data_cz_area_distance_2012_2 %>% 
   dplyr::filter(
     n_firm > 1
   ) %>% 
   dplyr::mutate(
     distance_min_r = 0
   ) %>% 
   dplyr::select(
     - n_firm
   )
 
 data_cz_area_distance_2012_2_0 <-
   data_cz_area_distance_2012_2 %>% 
   dplyr::filter(
     n_firm == 1
   ) %>% 
   dplyr::mutate(
     area_code_1 = area_code
   )
 
 data_cz_area_distance_2012_2_0 <- 
   dplyr::left_join(
     data_cz_area_distance_2012_2_0,
     data_area_area_distance_2nd_min,
     by = "area_code_1"
   ) %>% 
   dplyr::mutate(
     distance_min_r = distance_r
   ) %>% 
   dplyr::select(
     - n_firm,
     - area_code_1,
     - distance_r
   )
 
 data_cz_area_distance_2012_2 <- 
   rbind(data_cz_area_distance_2012_2_1,
         data_cz_area_distance_2012_2_0)
 
 data_cz_establishments_distance_2012 <- data.frame()
 
 for (i in unique(data_cz_area_distance_2012$firm_id)) {
   data_cz_area_distance_2012_i <- 
     data_cz_area_distance_2012 %>% 
     dplyr::filter(
       firm_id == i
     ) 
   
   combn_i <- 
     gtools::permutations(
       n = length(unique(data_cz_area_distance_2012_i$area_code)),
       r = 2,
       v = unique(data_cz_area_distance_2012_i$area_code),
       repeats.allowed = F
     ) %>% 
     as.data.frame() %>% 
     dplyr::rename(
       area_code_1 = V1,
       area_code_2 = V2
     ) 
   
   combn_i <-
     dplyr::left_join(
       combn_i,
       data_area_area_distance,
       by = c("area_code_1", "area_code_2")
     ) %>% 
     dplyr::group_by(
       area_code_1
     ) %>% 
     dplyr::mutate(
       distance_min = min(distance)
     ) 
   
   combn_i_unique <- 
     combn_i %>% 
     dplyr::filter(
       distance == distance_min
     ) %>% 
     dplyr::distinct()
   
   data_cz_area_distance_2012_i <- 
     data_cz_area_distance_2012_i %>% 
     dplyr::mutate(
       area_code_1 = area_code
     ) 
   
   data_cz_area_distance_2012_i <- 
     dplyr::left_join(
       data_cz_area_distance_2012_i,
       combn_i_unique,
       by = "area_code_1"
     ) %>% 
     dplyr::select(
       - distance
     )
   
   data_cz_area_distance_2012_i <- 
     data_cz_area_distance_2012_i %>% 
     dplyr::group_by(
       area_code
     ) %>% 
     dplyr::mutate(
       area_code_count = dplyr::n()
     ) %>% 
     dplyr::mutate(
       distance_min_r = ifelse(area_code_count > 1, 0, distance_min)
     ) %>% 
     dplyr::select(
       - area_code_1,
       - area_code_2,
       - distance_min,
       - area_code_count
     )
   
   data_cz_establishments_distance_2012 <- 
     data_cz_establishments_distance_2012 %>% 
     dplyr::bind_rows(data_cz_area_distance_2012_i)
   
 }
 
 data_cz_establishments_distance_2012 <- 
   rbind(data_cz_establishments_distance_2012,
         data_cz_area_distance_2012_2)
 
 # 2013 data---------------------------------------------------------------------
 
 data_cz_area_distance_2013 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2013
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count > 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2013_2 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2013
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count == 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2013_2 <- 
   data_cz_area_distance_2013_2 %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::mutate(
     n_firm = dplyr::n()
   )
 
 data_cz_area_distance_2013_2_1 <-
   data_cz_area_distance_2013_2 %>% 
   dplyr::filter(
     n_firm > 1
   ) %>% 
   dplyr::mutate(
     distance_min_r = 0
   ) %>% 
   dplyr::select(
     - n_firm
   )
 
 data_cz_area_distance_2013_2_0 <-
   data_cz_area_distance_2013_2 %>% 
   dplyr::filter(
     n_firm == 1
   ) %>% 
   dplyr::mutate(
     area_code_1 = area_code
   )
 
 data_cz_area_distance_2013_2_0 <- 
   dplyr::left_join(
     data_cz_area_distance_2013_2_0,
     data_area_area_distance_2nd_min,
     by = "area_code_1"
   ) %>% 
   dplyr::mutate(
     distance_min_r = distance_r
   ) %>% 
   dplyr::select(
     - n_firm,
     - area_code_1,
     - distance_r
   )
 
 data_cz_area_distance_2013_2 <- 
   rbind(data_cz_area_distance_2013_2_1,
         data_cz_area_distance_2013_2_0)
 
 data_cz_establishments_distance_2013 <- data.frame()
 
 for (i in unique(data_cz_area_distance_2013$firm_id)) {
   data_cz_area_distance_2013_i <- 
     data_cz_area_distance_2013 %>% 
     dplyr::filter(
       firm_id == i
     ) 
   
   combn_i <- 
     gtools::permutations(
       n = length(unique(data_cz_area_distance_2013_i$area_code)),
       r = 2,
       v = unique(data_cz_area_distance_2013_i$area_code),
       repeats.allowed = F
     ) %>% 
     as.data.frame() %>% 
     dplyr::rename(
       area_code_1 = V1,
       area_code_2 = V2
     ) 
   
   combn_i <-
     dplyr::left_join(
       combn_i,
       data_area_area_distance,
       by = c("area_code_1", "area_code_2")
     ) %>% 
     dplyr::group_by(
       area_code_1
     ) %>% 
     dplyr::mutate(
       distance_min = min(distance)
     ) 
   
   combn_i_unique <- 
     combn_i %>% 
     dplyr::filter(
       distance == distance_min
     ) %>% 
     dplyr::distinct()
   
   data_cz_area_distance_2013_i <- 
     data_cz_area_distance_2013_i %>% 
     dplyr::mutate(
       area_code_1 = area_code
     ) 
   
   data_cz_area_distance_2013_i <- 
     dplyr::left_join(
       data_cz_area_distance_2013_i,
       combn_i_unique,
       by = "area_code_1"
     ) %>% 
     dplyr::select(
       - distance
     )
   
   data_cz_area_distance_2013_i <- 
     data_cz_area_distance_2013_i %>% 
     dplyr::group_by(
       area_code
     ) %>% 
     dplyr::mutate(
       area_code_count = dplyr::n()
     ) %>% 
     dplyr::mutate(
       distance_min_r = ifelse(area_code_count > 1, 0, distance_min)
     ) %>% 
     dplyr::select(
       - area_code_1,
       - area_code_2,
       - distance_min,
       - area_code_count
     )
   
   data_cz_establishments_distance_2013 <- 
     data_cz_establishments_distance_2013 %>% 
     dplyr::bind_rows(data_cz_area_distance_2013_i)
 }
 
 data_cz_establishments_distance_2013 <- 
   rbind(data_cz_establishments_distance_2013,
         data_cz_area_distance_2013_2)
 
 # 2014 data---------------------------------------------------------------------
 
 data_cz_area_distance_2014 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2014
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count > 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2014_2 <- 
   data_cz_area_distance %>% 
   dplyr::filter(
     year == 2014
   ) %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::arrange(
     firm_id
   ) %>% 
   dplyr::mutate(
     area_count = length(unique(area_code))
   ) %>% 
   dplyr::filter(
     area_count == 1
   ) %>% 
   dplyr::ungroup() 
 
 data_cz_area_distance_2014_2 <- 
   data_cz_area_distance_2014_2 %>% 
   dplyr::group_by(
     firm_id
   ) %>% 
   dplyr::mutate(
     n_firm = dplyr::n()
   )
 
 data_cz_area_distance_2014_2_1 <-
   data_cz_area_distance_2014_2 %>% 
   dplyr::filter(
     n_firm > 1
   ) %>% 
   dplyr::mutate(
     distance_min_r = 0
   ) %>% 
   dplyr::select(
     - n_firm
   )
 
 data_cz_area_distance_2014_2_0 <-
   data_cz_area_distance_2014_2 %>% 
   dplyr::filter(
     n_firm == 1
   ) %>% 
   dplyr::mutate(
     area_code_1 = area_code
   )
 
 data_cz_area_distance_2014_2_0 <- 
   dplyr::left_join(
     data_cz_area_distance_2014_2_0,
     data_area_area_distance_2nd_min,
     by = "area_code_1"
   ) %>% 
   dplyr::mutate(
     distance_min_r = distance_r
   ) %>% 
   dplyr::select(
     - n_firm,
     - area_code_1,
     - distance_r
   )
 
 data_cz_area_distance_2014_2 <- 
   rbind(data_cz_area_distance_2014_2_1,
         data_cz_area_distance_2014_2_0)
 
 data_cz_establishments_distance_2014 <- data.frame()
 
 for (i in unique(data_cz_area_distance_2014$firm_id)) {
   data_cz_area_distance_2014_i <- 
     data_cz_area_distance_2014 %>% 
     dplyr::filter(
       firm_id == i
     ) 
   
   combn_i <- 
     gtools::permutations(
       n = length(unique(data_cz_area_distance_2014_i$area_code)),
       r = 2,
       v = unique(data_cz_area_distance_2014_i$area_code),
       repeats.allowed = F
     ) %>% 
     as.data.frame() %>% 
     dplyr::rename(
       area_code_1 = V1,
       area_code_2 = V2
     ) 
   
   combn_i <-
     dplyr::left_join(
       combn_i,
       data_area_area_distance,
       by = c("area_code_1", "area_code_2")
     ) %>% 
     dplyr::group_by(
       area_code_1
     ) %>% 
     dplyr::mutate(
       distance_min = min(distance)
     ) 
   
   combn_i_unique <- 
     combn_i %>% 
     dplyr::filter(
       distance == distance_min
     ) %>% 
     dplyr::distinct()
   
   data_cz_area_distance_2014_i <- 
     data_cz_area_distance_2014_i %>% 
     dplyr::mutate(
       area_code_1 = area_code
     ) 
   
   data_cz_area_distance_2014_i <- 
     dplyr::left_join(
       data_cz_area_distance_2014_i,
       combn_i_unique,
       by = "area_code_1"
     ) %>% 
     dplyr::select(
       - distance
     )
   
   data_cz_area_distance_2014_i <- 
     data_cz_area_distance_2014_i %>% 
     dplyr::group_by(
       area_code
     ) %>% 
     dplyr::mutate(
       area_code_count = dplyr::n()
     ) %>% 
     dplyr::mutate(
       distance_min_r = ifelse(area_code_count > 1, 0, distance_min)
     ) %>% 
     dplyr::select(
       - area_code_1,
       - area_code_2,
       - distance_min,
       - area_code_count
     )
   
   data_cz_establishments_distance_2014 <- 
     data_cz_establishments_distance_2014 %>% 
     dplyr::bind_rows(data_cz_area_distance_2014_i)
 }

 data_cz_establishments_distance_2014 <- 
   rbind(data_cz_establishments_distance_2014,
         data_cz_area_distance_2014_2)
 
 
 # merge all data---------------------------------------------------------------
 
 data_cz_establishments_distance <- 
   rbind(data_cz_establishments_distance_2010,
         data_cz_establishments_distance_2011) %>% 
   rbind(data_cz_establishments_distance_2012) %>% 
   rbind(data_cz_establishments_distance_2013) %>% 
   rbind(data_cz_establishments_distance_2014)
 
 data_cz_establishments_distance <- 
   data_cz_establishments_distance %>% 
   dplyr::group_by(
     year,
     cz
   ) %>% 
   dplyr::mutate(
     distance_min_avr = mean(distance_min_r)
   )
 
 saveRDS(data_cz_establishments_distance, file = "output/data_cz_establishments_distance.rds")
 
 data_cz_establishments_distance <-
   here::here(file = "output/data_cz_establishments_distance.rds") %>% 
   readRDS()
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 