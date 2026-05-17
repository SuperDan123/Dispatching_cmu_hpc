rm(list = ls())
gc()

library("magrittr")


# Use E-stat API
appId <- "873c837f8a4b86e96de7cb9fec7255664b98334c"


# Download the number of firms --------------------------------------------

num_firms_2009 <- 
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003032692", # Keizai Census, Year 2009, Num firms
    cdCat01 = "000" #, # All firms
    )

num_firms_2014 <- 
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003111139",
    cdCat03 = "000"
  )

saveRDS(num_firms_2009, file = "intermediate/num_firms_2009.RDS")
saveRDS(num_firms_2014, file = "intermediate/num_firms_2014.RDS")

# Download labor force ----------------------------------------------------

# year 2010
num_labor_2010 <-
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003052121",
    cdCat01 = "00710",
    cdCat02 = "001"    # Labor force
  )

# year 2015
num_labor_2015 <-
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003174584",
    cdCat01 = "00710",
    cdCat02 = "0010"
  )

saveRDS(num_labor_2010, file = "intermediate/num_labor_2010.RDS")
saveRDS(num_labor_2015, file = "intermediate/num_labor_2015.RDS")

# Download the number of employed people ----------------------------------

# year 2010
# https://www.e-stat.go.jp/dbview?sid=0003067219
num_emp_2010 <-
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003067219",
    cdCat01 = "00710",
    cdCat02 = "000",
    cdCat03 = "001",
    cdCat04 = "000" 
  )

# year 2015
# https://www.e-stat.go.jp/dbview?sid=0003174863
num_emp_2015 <-
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003174863",
    cdCat03 = "0010",
    cdCat05 = "0000"
  )

num_emp_2010 <- 
  num_emp_2010 %>% 
  dplyr::select(
    area_code, 
    value
    ) %>% 
  dplyr::mutate(year = 2010)

num_emp_2015 <- 
  num_emp_2015 %>% 
  dplyr::select(
    area_code, 
    value
    ) %>% 
  dplyr::mutate(year = 2015)

saveRDS(num_emp_2010, file = "intermediate/num_emp_2010.RDS")
saveRDS(num_emp_2015, file = "intermediate/num_emp_2015.RDS")



# Download the number of part-time workers + temp workers -----------------

# year 2010
# https://www.e-stat.go.jp/dbview?sid=0003052127
num_parttemp_2010 <-
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003052127",
    cdCat01 = "00710",
    cdCat02 = "000",   
    cdCat03 = c("005", "006"),   
    cdCat04 = "000"    
  )

# Year 2015
# https://www.e-stat.go.jp/dbview?sid=0003174863
num_parttemp_2015 <- 
  estatapi::estat_getStatsData(
    appId = appId,
    statsDataId = "0003174863",
    cdCat03 = c("0050", "0060"),   
    cdCat05 = "0000"    
  )

num_parttemp_2010 <- 
  num_parttemp_2010 %>% 
  dplyr::select(
    area_code, 
    cat03_code,
    value
    ) %>% 
  dplyr::group_by(
    area_code,
    cat03_code
  ) %>% 
  dplyr::summarise(
    value = sum(value)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    cat03_code = 
      dplyr::case_when(
        cat03_code == "005" ~ "parttime",
        cat03_code == "006" ~ "temp"
      )
  ) %>%
  tidyr::pivot_wider(
    id_cols = area_code,
    names_from = cat03_code,
    values_from = value
  ) %>%
  dplyr::mutate(
    year = 2010
  ) 

num_parttemp_2015 <- 
  num_parttemp_2015 %>% 
  dplyr::select(
    area_code, 
    cat03_code,
    value
    ) %>% 
  dplyr::group_by(
    area_code,
    cat03_code
  ) %>% 
  dplyr::summarise(
    value = sum(value)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    cat03_code = 
      dplyr::case_when(
        cat03_code == "0050" ~ "parttime",
        cat03_code == "0060" ~ "temp"
      )
  ) %>%
  tidyr::pivot_wider(
    id_cols = area_code,
    names_from = cat03_code,
    values_from = value
  ) %>%
  dplyr::mutate(
    year = 2015
  )

num_parttemp_2010 <- num_parttemp_2010[num_parttemp_2010$area_code %in% num_parttemp_2015$area_code, ]

saveRDS(num_parttemp_2010, file = "intermediate/num_parttemp_2010.RDS")
saveRDS(num_parttemp_2015, file = "intermediate/num_parttemp_2015.RDS")
