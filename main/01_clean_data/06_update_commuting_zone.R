
# initialize --------------------------------------------------------------

rm(list = ls())
gc()

library(magrittr)


# read data ---------------------------------------------------------------

mun_history <- readRDS("intermediate/municipal_merger_history.RDS")
commuting_zone <- read.csv("rawdata/CZ_2005_original.csv", stringsAsFactors = F) %>% 
  dplyr::rename(mun = i, cz = cluster)


# clean data --------------------------------------------------------------

mun_history <- 
  mun_history %>% 
  dplyr::select(
    code_old, 
    code_new
    ) %>% 
  dplyr::mutate_all(as.numeric)

commuting_zone <- 
  commuting_zone %>% 
  dplyr::left_join(
    mun_history, 
    by = c("mun" = "code_old")
    )

commuting_zone <- 
  commuting_zone %>% 
  dplyr::mutate(
    mun = dplyr::if_else(!is.na(code_new), code_new, mun)
    ) %>% 
  dplyr::select(
    mun, 
    cz
    ) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(
    mun, 
    cz
    ) %>%
  dplyr::rename(area_code = mun) %>%
  dplyr::distinct(
    area_code, 
    .keep_all = TRUE
  )

saveRDS(commuting_zone, file = "cleaned/data_area.RDS")
