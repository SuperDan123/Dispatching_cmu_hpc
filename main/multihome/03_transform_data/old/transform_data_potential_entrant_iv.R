
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

combn_2010 <- 
  here::here(file = "output/combn_2010.rds") %>% 
  readRDS()

combn_2011 <- 
  here::here(file = "output/combn_2011.rds") %>% 
  readRDS()

combn_2012 <- 
  here::here(file = "output/combn_2012.rds") %>% 
  readRDS()

combn_2013 <- 
  here::here(file = "output/combn_2013.rds") %>% 
  readRDS()

combn_2014 <-
  here::here(file = "output/combn_2014.rds") %>% 
  readRDS()

## for all data-----------------------------------------------------------------

### distanceの単位はm

data_cz_area_distance <- 
  data_cz_establishments 

# 2010 -------------------------------------------------------------------------

data_cz_area_distance_2010 <- 
  data_cz_area_distance %>% 
  dplyr::filter(
    year == 2010
  ) %>% 
  dplyr::group_by(
    area_code
  ) %>% 
  dplyr::ungroup()

## column for iv

data_cz_potential_entrant_2010 <- data.frame()

for (i in data_cz_area_distance_2010$id_unique) {
  
  data_cz_area_distance_2010_i <- 
    data_cz_area_distance_2010 %>% 
    dplyr::filter(
      id_unique == i
    )
  
  areacode_i <- 
    unlist(data_cz_area_distance_2010_i$area_code)
  
  combn_2010_i <- 
    combn_2010 %>% 
    dplyr::filter(
      area_code_1 == areacode_i
    ) %>% 
    dplyr::arrange(
      distance
    ) 
  
  dis_target = 5000
  
  combn_2010_i_target <- 
    combn_2010_i %>% 
    dplyr::filter(
      distance < dis_target
    ) %>% 
    dplyr::filter(
      cz_1 != cz_2
    ) 
  
  id_combn_2010_i_target <- 
    combn_2010_i_target %>% 
    dplyr::select(
      area_code_2
    ) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  data_2010_i <- 
    data_cz_area_distance_2010 %>% 
    dplyr::filter(
      ! firm_id == data_cz_area_distance_2010_i$firm_id
    ) %>% 
    dplyr::filter(
      area_code %in% id_combn_2010_i_target
    ) %>% 
    dplyr::filter(
      ! cz == data_cz_area_distance_2010_i$cz
    ) %>% 
    dplyr::select(
      firm_id
    ) %>% 
    unique()
  
  data_cz_area_distance_2010_i <- 
    data_cz_area_distance_2010_i %>% 
    dplyr::mutate(
      potential_entrant = nrow(data_2010_i)
    )
  
  data_cz_potential_entrant_2010 <- 
    data_cz_potential_entrant_2010 %>% 
    dplyr::bind_rows(data_cz_area_distance_2010_i)
}

# 2011 -------------------------------------------------------------------------

data_cz_area_distance_2011 <- 
  data_cz_area_distance %>% 
  dplyr::filter(
    year == 2011
  ) %>% 
  dplyr::group_by(
    area_code
  ) %>% 
  dplyr::ungroup()

## column for iv

data_cz_potential_entrant_2011 <- data.frame()

for (i in data_cz_area_distance_2011$id_unique) {
  
  data_cz_area_distance_2011_i <- 
    data_cz_area_distance_2011 %>% 
    dplyr::filter(
      id_unique == i
    )
  
  areacode_i <- 
    unlist(data_cz_area_distance_2011_i$area_code)
  
  combn_2011_i <- 
    combn_2011 %>% 
    dplyr::filter(
      area_code_1 == areacode_i
    ) %>% 
    dplyr::arrange(
      distance
    ) 
  
  dis_target = 5000
  
  combn_2011_i_target <- 
    combn_2011_i %>% 
    dplyr::filter(
      distance < dis_target
    ) %>% 
    dplyr::filter(
      cz_1 != cz_2
    ) 
  
  id_combn_2011_i_target <- 
    combn_2011_i_target %>% 
    dplyr::select(
      area_code_2
    ) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  data_2011_i <- 
    data_cz_area_distance_2011 %>% 
    dplyr::filter(
      ! firm_id == data_cz_area_distance_2011_i$firm_id
    ) %>% 
    dplyr::filter(
      area_code %in% id_combn_2011_i_target
    ) %>% 
    dplyr::filter(
      ! cz == data_cz_area_distance_2011_i$cz
    ) %>% 
    dplyr::select(
      firm_id
    ) %>% 
    unique()
  
  data_cz_area_distance_2011_i <- 
    data_cz_area_distance_2011_i %>% 
    dplyr::mutate(
      potential_entrant = nrow(data_2011_i)
    )
  
  data_cz_potential_entrant_2011 <- 
    data_cz_potential_entrant_2011 %>% 
    dplyr::bind_rows(data_cz_area_distance_2011_i)
}

# 2012 -------------------------------------------------------------------------

data_cz_area_distance_2012 <- 
  data_cz_area_distance %>% 
  dplyr::filter(
    year == 2012
  ) %>% 
  dplyr::group_by(
    area_code
  ) %>% 
  dplyr::ungroup()

## column for iv

data_cz_potential_entrant_2012 <- data.frame()

for (i in data_cz_area_distance_2012$id_unique) {
  
  data_cz_area_distance_2012_i <- 
    data_cz_area_distance_2012 %>% 
    dplyr::filter(
      id_unique == i
    )
  
  areacode_i <- 
    unlist(data_cz_area_distance_2012_i$area_code)
  
  combn_2012_i <- 
    combn_2012 %>% 
    dplyr::filter(
      area_code_1 == areacode_i
    ) %>% 
    dplyr::arrange(
      distance
    ) 
  
  dis_target = 5000
  
  combn_2012_i_target <- 
    combn_2012_i %>% 
    dplyr::filter(
      distance < dis_target
    ) %>% 
    dplyr::filter(
      cz_1 != cz_2
    ) 
  
  id_combn_2012_i_target <- 
    combn_2012_i_target %>% 
    dplyr::select(
      area_code_2
    ) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  data_2012_i <- 
    data_cz_area_distance_2012 %>% 
    dplyr::filter(
      ! firm_id == data_cz_area_distance_2012_i$firm_id
    ) %>% 
    dplyr::filter(
      area_code %in% id_combn_2012_i_target
    ) %>% 
    dplyr::filter(
      ! cz == data_cz_area_distance_2012_i$cz
    ) %>% 
    dplyr::select(
      firm_id
    ) %>% 
    unique()
  
  data_cz_area_distance_2012_i <- 
    data_cz_area_distance_2012_i %>% 
    dplyr::mutate(
      potential_entrant = nrow(data_2012_i)
    )
  
  data_cz_potential_entrant_2012 <- 
    data_cz_potential_entrant_2012 %>% 
    dplyr::bind_rows(data_cz_area_distance_2012_i)
}


# 2013 -------------------------------------------------------------------------

data_cz_area_distance_2013 <- 
  data_cz_area_distance %>% 
  dplyr::filter(
    year == 2013
  ) %>% 
  dplyr::group_by(
    area_code
  ) %>% 
  dplyr::ungroup()

## column for iv

data_cz_potential_entrant_2013 <- data.frame()

for (i in data_cz_area_distance_2013$id_unique) {
  
  data_cz_area_distance_2013_i <- 
    data_cz_area_distance_2013 %>% 
    dplyr::filter(
      id_unique == i
    )
  
  areacode_i <- 
    unlist(data_cz_area_distance_2013_i$area_code)
  
  combn_2013_i <- 
    combn_2013 %>% 
    dplyr::filter(
      area_code_1 == areacode_i
    ) %>% 
    dplyr::arrange(
      distance
    ) 
  
  dis_target = 5000
  
  combn_2013_i_target <- 
    combn_2013_i %>% 
    dplyr::filter(
      distance < dis_target
    ) %>% 
    dplyr::filter(
      cz_1 != cz_2
    ) 
  
  id_combn_2013_i_target <- 
    combn_2013_i_target %>% 
    dplyr::select(
      area_code_2
    ) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  data_2013_i <- 
    data_cz_area_distance_2013 %>% 
    dplyr::filter(
      ! firm_id == data_cz_area_distance_2013_i$firm_id
    ) %>% 
    dplyr::filter(
      area_code %in% id_combn_2013_i_target
    ) %>% 
    dplyr::filter(
      ! cz == data_cz_area_distance_2013_i$cz
    ) %>% 
    dplyr::select(
      firm_id
    ) %>% 
    unique()
  
  data_cz_area_distance_2013_i <- 
    data_cz_area_distance_2013_i %>% 
    dplyr::mutate(
      potential_entrant = nrow(data_2013_i)
    )
  
  data_cz_potential_entrant_2013 <- 
    data_cz_potential_entrant_2013 %>% 
    dplyr::bind_rows(data_cz_area_distance_2013_i)
}

# 2014 -------------------------------------------------------------------------

data_cz_area_distance_2014 <- 
  data_cz_area_distance %>% 
  dplyr::filter(
    year == 2014
  ) %>% 
  dplyr::group_by(
    area_code
  ) %>% 
  dplyr::ungroup()

## column for iv

data_cz_potential_entrant_2014 <- data.frame()

for (i in data_cz_area_distance_2014$id_unique) {
  
  data_cz_area_distance_2014_i <- 
    data_cz_area_distance_2014 %>% 
    dplyr::filter(
      id_unique == i
    )
  
  areacode_i <- 
    unlist(data_cz_area_distance_2014_i$area_code)
  
  combn_2014_i <- 
    combn_2014 %>% 
    dplyr::filter(
      area_code_1 == areacode_i
    ) %>% 
    dplyr::arrange(
      distance
    ) 
  
  dis_target = 5000
  
  combn_2014_i_target <- 
    combn_2014_i %>% 
    dplyr::filter(
      distance < dis_target
    ) %>% 
    dplyr::filter(
      cz_1 != cz_2
    ) 
  
  id_combn_2014_i_target <- 
    combn_2014_i_target %>% 
    dplyr::select(
      area_code_2
    ) %>% 
    unique() %>% 
    unlist() %>% 
    as.vector()
  
  data_2014_i <- 
    data_cz_area_distance_2014 %>% 
    dplyr::filter(
      ! firm_id == data_cz_area_distance_2014_i$firm_id
    ) %>% 
    dplyr::filter(
      area_code %in% id_combn_2014_i_target
    ) %>% 
    dplyr::filter(
      ! cz == data_cz_area_distance_2014_i$cz
    ) %>% 
    dplyr::select(
      firm_id
    ) %>% 
    unique()
  
  data_cz_area_distance_2014_i <- 
    data_cz_area_distance_2014_i %>% 
    dplyr::mutate(
      potential_entrant = nrow(data_2014_i)
    )
  
  data_cz_potential_entrant_2014 <- 
    data_cz_potential_entrant_2014 %>% 
    dplyr::bind_rows(data_cz_area_distance_2014_i)
}


data_cz_potential_entrant_5km <- 
  dplyr::bind_rows(
    data_cz_potential_entrant_2010,
    data_cz_potential_entrant_2011,
    data_cz_potential_entrant_2012,
    data_cz_potential_entrant_2013,
    data_cz_potential_entrant_2014
  )

saveRDS(data_cz_potential_entrant_5km, file = "output/data_cz_potential_entrant_5km.rds")

data_cz_potential_entrant_5km <- 
  here::here(file = "output/data_cz_potential_entrant_5km.rds") %>% 
  readRDS()

data_cz_potential_entrant_10km <- 
  here::here(file = "output/data_cz_potential_entrant_10km.rds") %>% 
  readRDS()

data_cz_potential_entrant_20km <- 
  here::here(file = "output/data_cz_potential_entrant_20km.rds") %>% 
  readRDS()

data_cz_potential_entrant_30km <- 
  here::here(file = "output/data_cz_potential_entrant_30km.rds") %>% 
  readRDS()

data_cz_potential_entrant_40km <- 
  here::here(file = "output/data_cz_potential_entrant_40km.rds") %>% 
  readRDS()

data_cz_potential_entrant_50km <- 
  here::here(file = "output/data_cz_potential_entrant_50km.rds") %>% 
  readRDS()


data_cz_potential_entrant_5km_select =
  data_cz_potential_entrant_5km %>% 
  dplyr::select(
    id_unique,
    potential_entrant
  ) %>% 
  dplyr::rename(
    potential_entrant_5km = potential_entrant
  )

data_cz_potential_entrant_10km_select =
  data_cz_potential_entrant_10km %>% 
  dplyr::select(
    id_unique,
    potential_entrant
  ) %>% 
  dplyr::rename(
    potential_entrant_10km = potential_entrant
  )

data_cz_potential_entrant_20km_select =
  data_cz_potential_entrant_20km %>% 
  dplyr::select(
    id_unique,
    potential_entrant
  ) %>% 
  dplyr::rename(
    potential_entrant_20km = potential_entrant
  )


data_cz_potential_entrant_30km_select =
  data_cz_potential_entrant_30km %>% 
  dplyr::select(
    id_unique,
    potential_entrant
  ) %>% 
  dplyr::rename(
    potential_entrant_30km = potential_entrant
  )

data_cz_potential_entrant_40km_select =
  data_cz_potential_entrant_40km %>% 
  dplyr::select(
    id_unique,
    potential_entrant
  ) %>% 
  dplyr::rename(
    potential_entrant_40km = potential_entrant
  )

data_cz_potential_entrant_all =
  dplyr::left_join(
    data_cz_potential_entrant_10km_select,
    data_cz_potential_entrant_20km_select,
    by = "id_unique"
  )

data_cz_potential_entrant_all =
  dplyr::left_join(
    data_cz_potential_entrant_all,
    data_cz_potential_entrant_5km_select,
    by = "id_unique"
  )

data_cz_potential_entrant_all =
  dplyr::left_join(
    data_cz_potential_entrant_all,
    data_cz_potential_entrant_30km_select,
    by = "id_unique"
  )

data_cz_potential_entrant_all =
  dplyr::left_join(
    data_cz_potential_entrant_all,
    data_cz_potential_entrant_40km_select,
    by = "id_unique"
  )

data_cz_potential_entrant_all =
  dplyr::left_join(
    data_cz_potential_entrant_50km,
    data_cz_potential_entrant_all,
    by = "id_unique"
  )

data_cz_potential_entrant_all =
  data_cz_potential_entrant_all %>% 
  dplyr::rename(
    potential_entrant_50km = potential_entrant
  )

saveRDS(data_cz_potential_entrant_all, file = "output/data_cz_potential_entrant_all.rds")


#data_cz_potential_entrant-------

data_cz_potential_entrant_10 <- 
  data_cz_potential_entrant_all %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2010 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2010 = N_jt,
    n_j2010 = dplyr::n(),
    potential_entrant5km_10 = mean(potential_entrant_5km),
    potential_entrant10km_10 = mean(potential_entrant_10km),
    potential_entrant20km_10 = mean(potential_entrant_20km),
    potential_entrant30km_10 = mean(potential_entrant_30km),
    potential_entrant40km_10 = mean(potential_entrant_40km),
    potential_entrant50km_10 = mean(potential_entrant_50km),
    wage_avr_2010 = mean(wage),
    fee_avr_2010 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2010 = log(wage_avr_2010),
    log_fee_avr_2010 = log(fee_avr_2010)
  ) %>% 
  dplyr::select(
    cz,
    n_j2010,
    N_j2010,
    potential_entrant5km_10,
    potential_entrant10km_10,
    potential_entrant20km_10,
    potential_entrant30km_10,
    potential_entrant40km_10,
    potential_entrant50km_10,
    log_wage_avr_2010,
    log_fee_avr_2010
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_11 <- 
  data_cz_potential_entrant_all %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2011 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2011 = N_jt,
    n_j2011 = dplyr::n(),
    potential_entrant5km_11 = mean(potential_entrant_5km),
    potential_entrant10km_11 = mean(potential_entrant_10km),
    potential_entrant20km_11 = mean(potential_entrant_20km),
    potential_entrant30km_11 = mean(potential_entrant_30km),
    potential_entrant40km_11 = mean(potential_entrant_40km),
    potential_entrant50km_11 = mean(potential_entrant_50km),
    wage_avr_2011 = mean(wage),
    fee_avr_2011 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2011 = log(wage_avr_2011),
    log_fee_avr_2011 = log(fee_avr_2011)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2011,
    N_j2011,
    potential_entrant5km_11,
    potential_entrant10km_11,
    potential_entrant20km_11,
    potential_entrant30km_11,
    potential_entrant40km_11,
    potential_entrant50km_11,
    log_wage_avr_2011,
    log_fee_avr_2011
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_12 <- 
  data_cz_potential_entrant_all %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2012 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2012 = N_jt,
    n_j2012 = dplyr::n(),
    potential_entrant5km_12 = mean(potential_entrant_5km),
    potential_entrant10km_12 = mean(potential_entrant_10km),
    potential_entrant20km_12 = mean(potential_entrant_20km),
    potential_entrant30km_12 = mean(potential_entrant_30km),
    potential_entrant40km_12 = mean(potential_entrant_40km),
    potential_entrant50km_12 = mean(potential_entrant_50km),
    wage_avr_2012 = mean(wage),
    fee_avr_2012 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2012 = log(wage_avr_2012),
    log_fee_avr_2012 = log(fee_avr_2012)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2012,
    N_j2012,
    potential_entrant5km_12,
    potential_entrant10km_12,
    potential_entrant20km_12,
    potential_entrant30km_12,
    potential_entrant40km_12,
    potential_entrant50km_12,
    log_wage_avr_2012,
    log_fee_avr_2012
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_13 <- 
  data_cz_potential_entrant_all %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2013 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2013 = N_jt,
    n_j2013 = dplyr::n(),
    potential_entrant5km_13 = mean(potential_entrant_5km),
    potential_entrant10km_13 = mean(potential_entrant_10km),
    potential_entrant20km_13 = mean(potential_entrant_20km),
    potential_entrant30km_13 = mean(potential_entrant_30km),
    potential_entrant40km_13 = mean(potential_entrant_40km),
    potential_entrant50km_13 = mean(potential_entrant_50km),
    wage_avr_2013 = mean(wage),
    fee_avr_2013 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2013 = log(wage_avr_2013),
    log_fee_avr_2013 = log(fee_avr_2013)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2013,
    N_j2013,
    potential_entrant5km_13,
    potential_entrant10km_13,
    potential_entrant20km_13,
    potential_entrant30km_13,
    potential_entrant40km_13,
    potential_entrant50km_13,
    log_wage_avr_2013,
    log_fee_avr_2013
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_14 <- 
  data_cz_potential_entrant_all %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2014 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2014 = N_jt,
    n_j2014 = dplyr::n(),
    potential_entrant5km_14 = mean(potential_entrant_5km),
    potential_entrant10km_14 = mean(potential_entrant_10km),
    potential_entrant20km_14 = mean(potential_entrant_20km),
    potential_entrant30km_14 = mean(potential_entrant_30km),
    potential_entrant40km_14 = mean(potential_entrant_40km),
    potential_entrant50km_14 = mean(potential_entrant_50km),
    wage_avr_2014 = mean(wage),
    fee_avr_2014 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2014 = log(wage_avr_2014),
    log_fee_avr_2014 = log(fee_avr_2014)
  ) %>% 
  dplyr::select(
    cz,
    year,
    N_j2014,
    n_j2014,
    potential_entrant5km_14,
    potential_entrant10km_14,
    potential_entrant20km_14,
    potential_entrant30km_14,
    potential_entrant40km_14,
    potential_entrant50km_14,
    log_wage_avr_2014,
    log_fee_avr_2014
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_11_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_11,
    by = "cz"
  )

data_cz_potential_entrant_11_10 <- 
  data_cz_potential_entrant_11_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2011,
    n_j2010,
    n_j2011,
    log_wage_avr_2010,
    log_wage_avr_2011,
    log_fee_avr_2010,
    log_fee_avr_2011
  ) %>% 
  dplyr::mutate(
    N_j1110 = N_j2011 - N_j2010,
    n_j1110 = n_j2011 - n_j2010,
    log_wage_1110 = log_wage_avr_2011 - log_wage_avr_2010,
    log_fee_1110 = log_fee_avr_2011 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1110,
    log_fee_diff = log_fee_1110,
    Nj_diff = N_j1110
  ) 

data_cz_potential_entrant_na11 <- 
  data_cz_potential_entrant_11_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_11,
    potential_entrant10km_11,
    potential_entrant20km_11,
    potential_entrant30km_11,
    potential_entrant40km_11,
    potential_entrant50km_11,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_11
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_11
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_11
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_11
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_11
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_11
  )


data_cz_potential_entrant_12_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_12,
    by = "cz"
  )

data_cz_potential_entrant_12_10 <- 
  data_cz_potential_entrant_12_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2012,
    n_j2010,
    n_j2012,
    log_wage_avr_2010,
    log_wage_avr_2012,
    log_fee_avr_2010,
    log_fee_avr_2012,
  ) %>% 
  dplyr::mutate(
    N_j1210 = N_j2012 - N_j2010,
    n_j1210 = n_j2012 - n_j2010,
    log_wage_1210 = log_wage_avr_2012 - log_wage_avr_2010,
    log_fee_1210 = log_fee_avr_2012 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1210 / 2,
    log_fee_diff = log_fee_1210 / 2,
    Nj_diff = N_j1210 / 2
  )

data_cz_potential_entrant_na12 <- 
  data_cz_potential_entrant_12_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_12,
    potential_entrant10km_12,
    potential_entrant20km_12,
    potential_entrant30km_12,
    potential_entrant40km_12,
    potential_entrant50km_12,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_12
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_12
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_12
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_12
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_12
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_12
  )

data_cz_potential_entrant_13_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_13,
    by = "cz"
  )

data_cz_potential_entrant_13_10 <- 
  data_cz_potential_entrant_13_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2013,
    n_j2010,
    n_j2013,
    log_wage_avr_2010,
    log_wage_avr_2013,
    log_fee_avr_2010,
    log_fee_avr_2013
  ) %>% 
  dplyr::mutate(
    N_j1310 = N_j2013 - N_j2010,
    n_j1310 = n_j2013 - n_j2010,
    log_wage_1310 = log_wage_avr_2013 - log_wage_avr_2010,
    log_fee_1310 = log_fee_avr_2013 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1310 / 3,
    log_fee_diff = log_fee_1310 / 3,
    Nj_diff = N_j1310 / 3
  )

data_cz_potential_entrant_na13 <- 
  data_cz_potential_entrant_13_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_13,
    potential_entrant10km_13,
    potential_entrant20km_13,
    potential_entrant30km_13,
    potential_entrant40km_13,
    potential_entrant50km_13,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_13,
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_13
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_13
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_13
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_13
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_13
  )

data_cz_potential_entrant_14_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_14,
    by = "cz"
  )

data_cz_potential_entrant_14_10 <- 
  data_cz_potential_entrant_14_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2014,
    n_j2010,
    n_j2014,
    log_wage_avr_2010,
    log_wage_avr_2014,
    log_fee_avr_2010,
    log_fee_avr_2014
  ) %>% 
  dplyr::mutate(
    N_j1410 = N_j2014 - N_j2010,
    n_j1410 = n_j2014 - n_j2010,
    log_wage_1410 = log_wage_avr_2014 - log_wage_avr_2010,
    log_fee_1410 = log_fee_avr_2014 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1410 / 4,
    log_fee_diff = log_fee_1410 / 4,
    Nj_diff = N_j1410 / 4
  )

data_cz_potential_entrant_na14 <- 
  data_cz_potential_entrant_14_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_14,
    potential_entrant10km_14,
    potential_entrant20km_14,
    potential_entrant30km_14,
    potential_entrant40km_14,
    potential_entrant50km_14,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_14
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_14
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_14
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_14
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_14
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_14
  )

data_cz_potential_entrant <- 
  dplyr::bind_rows(
    data_cz_potential_entrant_na11,
    data_cz_potential_entrant_na12,
    data_cz_potential_entrant_na13,
    data_cz_potential_entrant_na14
  )

data_cz_potential_entrant <- 
  data_cz_potential_entrant %>% 
  dplyr::mutate(
    margin = log_fee_diff - log_wage_diff
  )



saveRDS(data_cz_potential_entrant, file = "output/data_cz_potential_entrant.rds")


#data_cz_potential_entrantunder3-----------

data_cz_potential_entrant_under_cz3 =
  data_cz_potential_entrant_all %>% 
  dplyr::group_by(
    year,
    cz
  ) %>% 
  dplyr::mutate(
    cz_count = dplyr::n()
  ) %>% 
  dplyr::filter(
    cz_count <= 3
  )

data_cz_potential_entrant_10 <- 
  data_cz_potential_entrant_under_cz3 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2010 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2010 = N_jt,
    n_j2010 = dplyr::n(),
    potential_entrant5km_10 = mean(potential_entrant_5km),
    potential_entrant10km_10 = mean(potential_entrant_10km),
    potential_entrant20km_10 = mean(potential_entrant_20km),
    potential_entrant30km_10 = mean(potential_entrant_30km),
    potential_entrant40km_10 = mean(potential_entrant_40km),
    potential_entrant50km_10 = mean(potential_entrant_50km),
    wage_avr_2010 = mean(wage),
    fee_avr_2010 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2010 = log(wage_avr_2010),
    log_fee_avr_2010 = log(fee_avr_2010)
  ) %>% 
  dplyr::select(
    cz,
    n_j2010,
    N_j2010,
    potential_entrant5km_10,
    potential_entrant10km_10,
    potential_entrant20km_10,
    potential_entrant30km_10,
    potential_entrant40km_10,
    potential_entrant50km_10,
    log_wage_avr_2010,
    log_fee_avr_2010
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_11 <- 
  data_cz_potential_entrant_under_cz3 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2011 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2011 = N_jt,
    n_j2011 = dplyr::n(),
    potential_entrant5km_11 = mean(potential_entrant_5km),
    potential_entrant10km_11 = mean(potential_entrant_10km),
    potential_entrant20km_11 = mean(potential_entrant_20km),
    potential_entrant30km_11 = mean(potential_entrant_30km),
    potential_entrant40km_11 = mean(potential_entrant_40km),
    potential_entrant50km_11 = mean(potential_entrant_50km),
    wage_avr_2011 = mean(wage),
    fee_avr_2011 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2011 = log(wage_avr_2011),
    log_fee_avr_2011 = log(fee_avr_2011)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2011,
    N_j2011,
    potential_entrant5km_11,
    potential_entrant10km_11,
    potential_entrant20km_11,
    potential_entrant30km_11,
    potential_entrant40km_11,
    potential_entrant50km_11,
    log_wage_avr_2011,
    log_fee_avr_2011
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_12 <- 
  data_cz_potential_entrant_under_cz3 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2012 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2012 = N_jt,
    n_j2012 = dplyr::n(),
    potential_entrant5km_12 = mean(potential_entrant_5km),
    potential_entrant10km_12 = mean(potential_entrant_10km),
    potential_entrant20km_12 = mean(potential_entrant_20km),
    potential_entrant30km_12 = mean(potential_entrant_30km),
    potential_entrant40km_12 = mean(potential_entrant_40km),
    potential_entrant50km_12 = mean(potential_entrant_50km),
    wage_avr_2012 = mean(wage),
    fee_avr_2012 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2012 = log(wage_avr_2012),
    log_fee_avr_2012 = log(fee_avr_2012)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2012,
    N_j2012,
    potential_entrant5km_12,
    potential_entrant10km_12,
    potential_entrant20km_12,
    potential_entrant30km_12,
    potential_entrant40km_12,
    potential_entrant50km_12,
    log_wage_avr_2012,
    log_fee_avr_2012
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_13 <- 
  data_cz_potential_entrant_under_cz3 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2013 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2013 = N_jt,
    n_j2013 = dplyr::n(),
    potential_entrant5km_13 = mean(potential_entrant_5km),
    potential_entrant10km_13 = mean(potential_entrant_10km),
    potential_entrant20km_13 = mean(potential_entrant_20km),
    potential_entrant30km_13 = mean(potential_entrant_30km),
    potential_entrant40km_13 = mean(potential_entrant_40km),
    potential_entrant50km_13 = mean(potential_entrant_50km),
    wage_avr_2013 = mean(wage),
    fee_avr_2013 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2013 = log(wage_avr_2013),
    log_fee_avr_2013 = log(fee_avr_2013)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2013,
    N_j2013,
    potential_entrant5km_13,
    potential_entrant10km_13,
    potential_entrant20km_13,
    potential_entrant30km_13,
    potential_entrant40km_13,
    potential_entrant50km_13,
    log_wage_avr_2013,
    log_fee_avr_2013
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_14 <- 
  data_cz_potential_entrant_under_cz3 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2014 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2014 = N_jt,
    n_j2014 = dplyr::n(),
    potential_entrant5km_14 = mean(potential_entrant_5km),
    potential_entrant10km_14 = mean(potential_entrant_10km),
    potential_entrant20km_14 = mean(potential_entrant_20km),
    potential_entrant30km_14 = mean(potential_entrant_30km),
    potential_entrant40km_14 = mean(potential_entrant_40km),
    potential_entrant50km_14 = mean(potential_entrant_50km),
    wage_avr_2014 = mean(wage),
    fee_avr_2014 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2014 = log(wage_avr_2014),
    log_fee_avr_2014 = log(fee_avr_2014)
  ) %>% 
  dplyr::select(
    cz,
    year,
    N_j2014,
    n_j2014,
    potential_entrant5km_14,
    potential_entrant10km_14,
    potential_entrant20km_14,
    potential_entrant30km_14,
    potential_entrant40km_14,
    potential_entrant50km_14,
    log_wage_avr_2014,
    log_fee_avr_2014
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_11_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_11,
    by = "cz"
  )

data_cz_potential_entrant_11_10 <- 
  data_cz_potential_entrant_11_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2011,
    n_j2010,
    n_j2011,
    log_wage_avr_2010,
    log_wage_avr_2011,
    log_fee_avr_2010,
    log_fee_avr_2011
  ) %>% 
  dplyr::mutate(
    N_j1110 = N_j2011 - N_j2010,
    n_j1110 = n_j2011 - n_j2010,
    log_wage_1110 = log_wage_avr_2011 - log_wage_avr_2010,
    log_fee_1110 = log_fee_avr_2011 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1110,
    log_fee_diff = log_fee_1110,
    Nj_diff = N_j1110
  ) 

data_cz_potential_entrant_na11 <- 
  data_cz_potential_entrant_11_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_11,
    potential_entrant10km_11,
    potential_entrant20km_11,
    potential_entrant30km_11,
    potential_entrant40km_11,
    potential_entrant50km_11,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_11
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_11
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_11
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_11
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_11
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_11
  )


data_cz_potential_entrant_12_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_12,
    by = "cz"
  )

data_cz_potential_entrant_12_10 <- 
  data_cz_potential_entrant_12_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2012,
    n_j2010,
    n_j2012,
    log_wage_avr_2010,
    log_wage_avr_2012,
    log_fee_avr_2010,
    log_fee_avr_2012,
  ) %>% 
  dplyr::mutate(
    N_j1210 = N_j2012 - N_j2010,
    n_j1210 = n_j2012 - n_j2010,
    log_wage_1210 = log_wage_avr_2012 - log_wage_avr_2010,
    log_fee_1210 = log_fee_avr_2012 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1210 / 2,
    log_fee_diff = log_fee_1210 / 2,
    Nj_diff = N_j1210 / 2
  )

data_cz_potential_entrant_na12 <- 
  data_cz_potential_entrant_12_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_12,
    potential_entrant10km_12,
    potential_entrant20km_12,
    potential_entrant30km_12,
    potential_entrant40km_12,
    potential_entrant50km_12,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_12
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_12
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_12
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_12
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_12
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_12
  )

data_cz_potential_entrant_13_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_13,
    by = "cz"
  )

data_cz_potential_entrant_13_10 <- 
  data_cz_potential_entrant_13_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2013,
    n_j2010,
    n_j2013,
    log_wage_avr_2010,
    log_wage_avr_2013,
    log_fee_avr_2010,
    log_fee_avr_2013
  ) %>% 
  dplyr::mutate(
    N_j1310 = N_j2013 - N_j2010,
    n_j1310 = n_j2013 - n_j2010,
    log_wage_1310 = log_wage_avr_2013 - log_wage_avr_2010,
    log_fee_1310 = log_fee_avr_2013 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1310 / 3,
    log_fee_diff = log_fee_1310 / 3,
    Nj_diff = N_j1310 / 3
  )

data_cz_potential_entrant_na13 <- 
  data_cz_potential_entrant_13_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_13,
    potential_entrant10km_13,
    potential_entrant20km_13,
    potential_entrant30km_13,
    potential_entrant40km_13,
    potential_entrant50km_13,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_13,
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_13
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_13
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_13
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_13
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_13
  )

data_cz_potential_entrant_14_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_14,
    by = "cz"
  )

data_cz_potential_entrant_14_10 <- 
  data_cz_potential_entrant_14_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2014,
    n_j2010,
    n_j2014,
    log_wage_avr_2010,
    log_wage_avr_2014,
    log_fee_avr_2010,
    log_fee_avr_2014
  ) %>% 
  dplyr::mutate(
    N_j1410 = N_j2014 - N_j2010,
    n_j1410 = n_j2014 - n_j2010,
    log_wage_1410 = log_wage_avr_2014 - log_wage_avr_2010,
    log_fee_1410 = log_fee_avr_2014 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1410 / 4,
    log_fee_diff = log_fee_1410 / 4,
    Nj_diff = N_j1410 / 4
  )

data_cz_potential_entrant_na14 <- 
  data_cz_potential_entrant_14_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_14,
    potential_entrant10km_14,
    potential_entrant20km_14,
    potential_entrant30km_14,
    potential_entrant40km_14,
    potential_entrant50km_14,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_14
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_14
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_14
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_14
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_14
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_14
  )

data_cz_potential_entrant <- 
  dplyr::bind_rows(
    data_cz_potential_entrant_na11,
    data_cz_potential_entrant_na12,
    data_cz_potential_entrant_na13,
    data_cz_potential_entrant_na14
  )

data_cz_potential_entrant <- 
  data_cz_potential_entrant %>% 
  dplyr::mutate(
    margin = log_fee_diff - log_wage_diff
  )


data_cz_potential_entrantunder3 <- 
  dplyr::bind_rows(
    data_cz_potential_entrant_na11,
    data_cz_potential_entrant_na12,
    data_cz_potential_entrant_na13,
    data_cz_potential_entrant_na14
  )

data_cz_potential_entrantunder3 <- 
  data_cz_potential_entrantunder3 %>% 
  dplyr::mutate(
    margin = log_fee_diff - log_wage_diff
  )

saveRDS(data_cz_potential_entrantunder3, file = "output/data_cz_potential_entrantunder3.rds")


# data_cz_potential_entrantunder5-------

data_cz_potential_entrant_under_cz5 =
  data_cz_potential_entrant_all %>% 
  dplyr::group_by(
    year,
    cz
  ) %>% 
  dplyr::mutate(
    cz_count = dplyr::n()
  ) %>% 
  dplyr::filter(
    cz_count <= 5
  )

data_cz_potential_entrant_10 <- 
  data_cz_potential_entrant_under_cz5 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2010 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2010 = N_jt,
    n_j2010 = dplyr::n(),
    potential_entrant5km_10 = mean(potential_entrant_5km),
    potential_entrant10km_10 = mean(potential_entrant_10km),
    potential_entrant20km_10 = mean(potential_entrant_20km),
    potential_entrant30km_10 = mean(potential_entrant_30km),
    potential_entrant40km_10 = mean(potential_entrant_40km),
    potential_entrant50km_10 = mean(potential_entrant_50km),
    wage_avr_2010 = mean(wage),
    fee_avr_2010 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2010 = log(wage_avr_2010),
    log_fee_avr_2010 = log(fee_avr_2010)
  ) %>% 
  dplyr::select(
    cz,
    n_j2010,
    N_j2010,
    potential_entrant5km_10,
    potential_entrant10km_10,
    potential_entrant20km_10,
    potential_entrant30km_10,
    potential_entrant40km_10,
    potential_entrant50km_10,
    log_wage_avr_2010,
    log_fee_avr_2010
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_11 <- 
  data_cz_potential_entrant_under_cz5 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2011 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2011 = N_jt,
    n_j2011 = dplyr::n(),
    potential_entrant5km_11 = mean(potential_entrant_5km),
    potential_entrant10km_11 = mean(potential_entrant_10km),
    potential_entrant20km_11 = mean(potential_entrant_20km),
    potential_entrant30km_11 = mean(potential_entrant_30km),
    potential_entrant40km_11 = mean(potential_entrant_40km),
    potential_entrant50km_11 = mean(potential_entrant_50km),
    wage_avr_2011 = mean(wage),
    fee_avr_2011 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2011 = log(wage_avr_2011),
    log_fee_avr_2011 = log(fee_avr_2011)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2011,
    N_j2011,
    potential_entrant5km_11,
    potential_entrant10km_11,
    potential_entrant20km_11,
    potential_entrant30km_11,
    potential_entrant40km_11,
    potential_entrant50km_11,
    log_wage_avr_2011,
    log_fee_avr_2011
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_12 <- 
  data_cz_potential_entrant_under_cz5 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2012 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2012 = N_jt,
    n_j2012 = dplyr::n(),
    potential_entrant5km_12 = mean(potential_entrant_5km),
    potential_entrant10km_12 = mean(potential_entrant_10km),
    potential_entrant20km_12 = mean(potential_entrant_20km),
    potential_entrant30km_12 = mean(potential_entrant_30km),
    potential_entrant40km_12 = mean(potential_entrant_40km),
    potential_entrant50km_12 = mean(potential_entrant_50km),
    wage_avr_2012 = mean(wage),
    fee_avr_2012 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2012 = log(wage_avr_2012),
    log_fee_avr_2012 = log(fee_avr_2012)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2012,
    N_j2012,
    potential_entrant5km_12,
    potential_entrant10km_12,
    potential_entrant20km_12,
    potential_entrant30km_12,
    potential_entrant40km_12,
    potential_entrant50km_12,
    log_wage_avr_2012,
    log_fee_avr_2012
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_13 <- 
  data_cz_potential_entrant_under_cz5 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2013 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2013 = N_jt,
    n_j2013 = dplyr::n(),
    potential_entrant5km_13 = mean(potential_entrant_5km),
    potential_entrant10km_13 = mean(potential_entrant_10km),
    potential_entrant20km_13 = mean(potential_entrant_20km),
    potential_entrant30km_13 = mean(potential_entrant_30km),
    potential_entrant40km_13 = mean(potential_entrant_40km),
    potential_entrant50km_13 = mean(potential_entrant_50km),
    wage_avr_2013 = mean(wage),
    fee_avr_2013 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2013 = log(wage_avr_2013),
    log_fee_avr_2013 = log(fee_avr_2013)
  ) %>% 
  dplyr::select(
    cz,
    year,
    n_j2013,
    N_j2013,
    potential_entrant5km_13,
    potential_entrant10km_13,
    potential_entrant20km_13,
    potential_entrant30km_13,
    potential_entrant40km_13,
    potential_entrant50km_13,
    log_wage_avr_2013,
    log_fee_avr_2013
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_14 <- 
  data_cz_potential_entrant_under_cz5 %>% 
  tidyr::drop_na(
    potential_entrant_5km,
    potential_entrant_10km,
    potential_entrant_20km,
    potential_entrant_30km,
    potential_entrant_40km,
    potential_entrant_50km
  ) %>% 
  dplyr::filter(
    year == 2014 
  ) %>% 
  dplyr::group_by(
    cz
  ) %>% 
  dplyr::mutate(
    N_j2014 = N_jt,
    n_j2014 = dplyr::n(),
    potential_entrant5km_14 = mean(potential_entrant_5km),
    potential_entrant10km_14 = mean(potential_entrant_10km),
    potential_entrant20km_14 = mean(potential_entrant_20km),
    potential_entrant30km_14 = mean(potential_entrant_30km),
    potential_entrant40km_14 = mean(potential_entrant_40km),
    potential_entrant50km_14 = mean(potential_entrant_50km),
    wage_avr_2014 = mean(wage),
    fee_avr_2014 = mean(fee)
  ) %>% 
  dplyr::mutate(
    log_wage_avr_2014 = log(wage_avr_2014),
    log_fee_avr_2014 = log(fee_avr_2014)
  ) %>% 
  dplyr::select(
    cz,
    year,
    N_j2014,
    n_j2014,
    potential_entrant5km_14,
    potential_entrant10km_14,
    potential_entrant20km_14,
    potential_entrant30km_14,
    potential_entrant40km_14,
    potential_entrant50km_14,
    log_wage_avr_2014,
    log_fee_avr_2014
  ) %>% 
  dplyr::distinct()

data_cz_potential_entrant_11_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_11,
    by = "cz"
  )

data_cz_potential_entrant_11_10 <- 
  data_cz_potential_entrant_11_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2011,
    n_j2010,
    n_j2011,
    log_wage_avr_2010,
    log_wage_avr_2011,
    log_fee_avr_2010,
    log_fee_avr_2011
  ) %>% 
  dplyr::mutate(
    N_j1110 = N_j2011 - N_j2010,
    n_j1110 = n_j2011 - n_j2010,
    log_wage_1110 = log_wage_avr_2011 - log_wage_avr_2010,
    log_fee_1110 = log_fee_avr_2011 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1110,
    log_fee_diff = log_fee_1110,
    Nj_diff = N_j1110
  ) 

data_cz_potential_entrant_na11 <- 
  data_cz_potential_entrant_11_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_11,
    potential_entrant10km_11,
    potential_entrant20km_11,
    potential_entrant30km_11,
    potential_entrant40km_11,
    potential_entrant50km_11,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_11
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_11
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_11
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_11
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_11
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_11
  )


data_cz_potential_entrant_12_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_12,
    by = "cz"
  )

data_cz_potential_entrant_12_10 <- 
  data_cz_potential_entrant_12_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2012,
    n_j2010,
    n_j2012,
    log_wage_avr_2010,
    log_wage_avr_2012,
    log_fee_avr_2010,
    log_fee_avr_2012,
  ) %>% 
  dplyr::mutate(
    N_j1210 = N_j2012 - N_j2010,
    n_j1210 = n_j2012 - n_j2010,
    log_wage_1210 = log_wage_avr_2012 - log_wage_avr_2010,
    log_fee_1210 = log_fee_avr_2012 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1210 / 2,
    log_fee_diff = log_fee_1210 / 2,
    Nj_diff = N_j1210 / 2
  )

data_cz_potential_entrant_na12 <- 
  data_cz_potential_entrant_12_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_12,
    potential_entrant10km_12,
    potential_entrant20km_12,
    potential_entrant30km_12,
    potential_entrant40km_12,
    potential_entrant50km_12,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_12
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_12
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_12
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_12
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_12
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_12
  )

data_cz_potential_entrant_13_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_13,
    by = "cz"
  )

data_cz_potential_entrant_13_10 <- 
  data_cz_potential_entrant_13_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2013,
    n_j2010,
    n_j2013,
    log_wage_avr_2010,
    log_wage_avr_2013,
    log_fee_avr_2010,
    log_fee_avr_2013
  ) %>% 
  dplyr::mutate(
    N_j1310 = N_j2013 - N_j2010,
    n_j1310 = n_j2013 - n_j2010,
    log_wage_1310 = log_wage_avr_2013 - log_wage_avr_2010,
    log_fee_1310 = log_fee_avr_2013 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1310 / 3,
    log_fee_diff = log_fee_1310 / 3,
    Nj_diff = N_j1310 / 3
  )

data_cz_potential_entrant_na13 <- 
  data_cz_potential_entrant_13_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_13,
    potential_entrant10km_13,
    potential_entrant20km_13,
    potential_entrant30km_13,
    potential_entrant40km_13,
    potential_entrant50km_13,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_13,
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_13
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_13
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_13
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_13
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_13
  )

data_cz_potential_entrant_14_10 <- 
  dplyr::full_join(
    data_cz_potential_entrant_10,
    data_cz_potential_entrant_14,
    by = "cz"
  )

data_cz_potential_entrant_14_10 <- 
  data_cz_potential_entrant_14_10 %>% 
  tidyr::drop_na(
    N_j2010,
    N_j2014,
    n_j2010,
    n_j2014,
    log_wage_avr_2010,
    log_wage_avr_2014,
    log_fee_avr_2010,
    log_fee_avr_2014
  ) %>% 
  dplyr::mutate(
    N_j1410 = N_j2014 - N_j2010,
    n_j1410 = n_j2014 - n_j2010,
    log_wage_1410 = log_wage_avr_2014 - log_wage_avr_2010,
    log_fee_1410 = log_fee_avr_2014 - log_fee_avr_2010
  ) %>% 
  dplyr::mutate(
    log_wage_diff = log_wage_1410 / 4,
    log_fee_diff = log_fee_1410 / 4,
    Nj_diff = N_j1410 / 4
  )

data_cz_potential_entrant_na14 <- 
  data_cz_potential_entrant_14_10 %>% 
  dplyr::select(
    cz,
    year,
    log_wage_diff,
    log_fee_diff,
    Nj_diff,
    potential_entrant5km_14,
    potential_entrant10km_14,
    potential_entrant20km_14,
    potential_entrant30km_14,
    potential_entrant40km_14,
    potential_entrant50km_14,
  ) %>% 
  dplyr::rename(
    potential_entrant5km = potential_entrant5km_14
  ) %>% 
  dplyr::rename(
    potential_entrant10km = potential_entrant10km_14
  ) %>% 
  dplyr::rename(
    potential_entrant20km = potential_entrant20km_14
  ) %>% 
  dplyr::rename(
    potential_entrant30km = potential_entrant30km_14
  ) %>% 
  dplyr::rename(
    potential_entrant40km = potential_entrant40km_14
  ) %>% 
  dplyr::rename(
    potential_entrant50km = potential_entrant50km_14
  )

data_cz_potential_entrant <- 
  dplyr::bind_rows(
    data_cz_potential_entrant_na11,
    data_cz_potential_entrant_na12,
    data_cz_potential_entrant_na13,
    data_cz_potential_entrant_na14
  )

data_cz_potential_entrant <- 
  data_cz_potential_entrant %>% 
  dplyr::mutate(
    margin = log_fee_diff - log_wage_diff
  )


data_cz_potential_entrantunder5 <- 
  dplyr::bind_rows(
    data_cz_potential_entrant_na11,
    data_cz_potential_entrant_na12,
    data_cz_potential_entrant_na13,
    data_cz_potential_entrant_na14
  )

data_cz_potential_entrantunder5 <- 
  data_cz_potential_entrantunder5 %>% 
  dplyr::mutate(
    margin = log_fee_diff - log_wage_diff
  )



saveRDS(data_cz_potential_entrantunder5, file = "output/data_cz_potential_entrantunder5.rds")
