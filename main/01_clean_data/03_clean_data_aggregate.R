rm(list = ls())
gc()

library("magrittr")


# Read data ---------------------------------------------------------------

num_firms_2009_raw <-
  readRDS(file = "intermediate/num_firms_2009.RDS")
num_firms_2014_raw <-
  readRDS(file = "intermediate/num_firms_2014.RDS")
num_labor_2010_raw <-
  readRDS(file = "intermediate/num_labor_2010.RDS")
num_labor_2015_raw <-
  readRDS(file = "intermediate/num_labor_2015.RDS")
num_emp_2010 <- readRDS(file = "intermediate/num_emp_2010.RDS")
num_emp_2015 <- readRDS(file = "intermediate/num_emp_2015.RDS")
num_parttemp_2010 <-
  readRDS(file = "intermediate/num_parttemp_2010.RDS")
num_parttemp_2015 <-
  readRDS(file = "intermediate/num_parttemp_2015.RDS")
partwage_city <-
  readxl::read_excel("rawdata/パート賃金2010-2014/partwage_city.xlsx")
partwage_pref <-
  readxl::read_excel("rawdata/パート賃金2010-2014/partwage_pref.xlsx")
mw <- readxl::read_excel("rawdata/minimum_wage.xls")
cpi <- readr::read_csv("rawdata/Japan_CPI_FRED.csv")
# municipality
zipcode_city <- 
  read.csv(
    file = "rawdata/zenkoku_utf8.csv", 
    stringsAsFactors = F,
    fileEncoding = "utf-8"
  )[, c(2, 3, 5)] %>% 
  dplyr::distinct()


# Clean data --------------------------------------------------------------


## Year-level -------------------------------------------------------------


### CPI -------------------------------------------------------------------


colnames(cpi) <- c("date", "cpi")
cpi <- cpi %>% 
  tidyr::separate(date, c("year", "month", "day"))

cpi <- 
  cpi %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(cpi = mean(cpi)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(year >= 2010, year <= 2014) %>%
  dplyr::mutate(
    cpi = 100 * cpi / sum(cpi * (year == 2010)),
    year = as.numeric(year)
  )

saveRDS(cpi, file = "cleaned/data_year.RDS")


## Area-year level --------------------------------------------------------

### Number of firms --------------------------------------------------------

# 2009
num_firms_2009 <- 
  num_firms_2009_raw %>%
  dplyr::filter(
    tab_code == "040",
    cat02_code %in% c(
      "000",
      # All
      "002",
      # Major classification
      "014",
      "022",
      "030",
      "056",
      "238",
      "249",
      "275",
      "309",
      "389",
      "414",
      "432",
      "467",
      "494",
      "538",
      "566",
      "598",
      "605"
    )
  ) %>%
  dplyr::select(
    cat02_code, # industry
    area_code,
    value
    ) %>%
  tidyr::spread(
    key = cat02_code,
    value = value
    )

colnames(num_firms_2009) <- 
  c(
    "area_code",
    "firms_all",
    "firms_A",
    "firms_B",
    "firms_C",
    "firms_D",
    "firms_E",
    "firms_F",
    "firms_G",
    "firms_H",
    "firms_I",
    "firms_J",
    "firms_K",
    "firms_L",
    "firms_M",
    "firms_N",
    "firms_O",
    "firms_P",
    "firms_Q",
    "firms_R"
  )

num_firms_2009 <- 
  num_firms_2009 %>%
  dplyr::mutate(
    year = 2009,
    firms_AB = firms_A + firms_B
    ) %>%
  dplyr::select(-(firms_A:firms_B))

# 2014
num_firms_2014 <- 
  num_firms_2014_raw %>%
  dplyr::filter(
    tab_code == "174",
    cat01_code == "004",
    cat02_code %in% c(
      "010",
      # All
      "020",
      # Major classification
      "280",
      "370",
      "640",
      "2700",
      "2850",
      "3140",
      "3560",
      "4480",
      "4790",
      "5000",
      "5390",
      "5690",
      "6160",
      "6460",
      "6830",
      "6920"
    )
  ) %>%
  dplyr::select(
    cat02_code, # industry
    area_code,
    value
    ) %>%
  tidyr::spread(
    key = cat02_code,
    value = value
    )

colnames(num_firms_2014) <- 
  c(
    "area_code",
    "firms_all",
    "firms_AB",
    "firms_C",
    "firms_D",
    "firms_E",
    "firms_F",
    "firms_G",
    "firms_H",
    "firms_I",
    "firms_J",
    "firms_K",
    "firms_L",
    "firms_M",
    "firms_N",
    "firms_O",
    "firms_P",
    "firms_Q",
    "firms_R"
  )

num_firms_2014 <- 
  num_firms_2014 %>%
  dplyr::mutate(year = 2014)

num_firms <- dplyr::bind_rows(num_firms_2009, num_firms_2014)

num_firms <-
  num_firms %>%
  dplyr::filter(
    area_code != "00000"
  ) %>%
  dplyr::mutate(
    area_code = as.numeric(area_code)
  )

saveRDS(num_firms, file = "cleaned/data_area_year_num_firms.RDS")


### Number  of establishments ---------------------------------------------

# 2009
num_establishments_2009 <- 
  num_firms_2009_raw %>%
  dplyr::filter(
    tab_code == "044",
    cat02_code %in% c(
      "000",
      # All
      "002",
      # Major classification
      "014",
      "022",
      "030",
      "056",
      "238",
      "249",
      "275",
      "309",
      "389",
      "414",
      "432",
      "467",
      "494",
      "538",
      "566",
      "598",
      "605"
    )
  ) %>%
  dplyr::select(
    cat02_code, # industry
    area_code,
    value
    ) %>%
  tidyr::spread(
    key = cat02_code,
    value = value
    )

colnames(num_establishments_2009) <- 
  c(
    "area_code",
    "establishments_all",
    "establishments_A",
    "establishments_B",
    "establishments_C",
    "establishments_D",
    "establishments_E",
    "establishments_F",
    "establishments_G",
    "establishments_H",
    "establishments_I",
    "establishments_J",
    "establishments_K",
    "establishments_L",
    "establishments_M",
    "establishments_N",
    "establishments_O",
    "establishments_P",
    "establishments_Q",
    "establishments_R"
  )

num_establishments_2009 <- 
  num_establishments_2009 %>%
  dplyr::mutate(
    year = 2009,
    establishments_AB = establishments_A + establishments_B
    ) %>%
  dplyr::select(-(establishments_A:establishments_B))

# 2014
num_establishments_2014 <- 
  num_firms_2014_raw %>%
  dplyr::filter(
    tab_code == "187",
    cat01_code == "004",
    cat02_code %in% c(
      "010",
      # All
      "020",
      # Major classification
      "280",
      "370",
      "640",
      "2700",
      "2850",
      "3140",
      "3560",
      "4480",
      "4790",
      "5000",
      "5390",
      "5690",
      "6160",
      "6460",
      "6830",
      "6920"
    )
  ) %>%
  dplyr::select(
    cat02_code, # industry
    area_code,
    value
    ) %>%
  tidyr::spread(
    key = cat02_code,
    value = value
    )

colnames(num_establishments_2014) <-
  c(
    "area_code",
    "establishments_all",
    "establishments_AB",
    "establishments_C",
    "establishments_D",
    "establishments_E",
    "establishments_F",
    "establishments_G",
    "establishments_H",
    "establishments_I",
    "establishments_J",
    "establishments_K",
    "establishments_L",
    "establishments_M",
    "establishments_N",
    "establishments_O",
    "establishments_P",
    "establishments_Q",
    "establishments_R"
  )

num_establishments_2014 <- 
  num_establishments_2014 %>%
  dplyr::mutate(year = 2014)

num_establishments <-
  dplyr::bind_rows(num_establishments_2009, num_establishments_2014)

num_establishments <-
  num_establishments %>%
  dplyr::mutate(
    area_code = as.numeric(area_code)
  )

saveRDS(num_establishments, file = "cleaned/data_area_year_num_establishments.RDS")



### Number of labor -------------------------------------------------------

# 2010
num_labor_2010 <- 
  num_labor_2010_raw %>%
  dplyr::filter(cat04_code == "000",!(cat03_code %in% c("515", "565", "570", "575"))) %>%
  dplyr::select(
    cat03_code, # age
    area_code,
    value
    ) %>%
  tidyr::spread(
    key = cat03_code,
    value = value
    ) %>%
  dplyr::rename(
    labor_all = "000",
    labor_15 = "203",
    labor_20 = "204",
    labor_25 = "205",
    labor_30 = "206",
    labor_35 = "207",
    labor_40 = "208",
    labor_45 = "209",
    labor_50 = "210",
    labor_55 = "211",
    labor_60 = "212",
    labor_65 = "213",
    labor_70 = "214",
    labor_75 = "215",
    labor_80 = "216",
    labor_85 = "585"
  ) %>%
  dplyr::mutate(year = 2010)



# 2015
num_labor_2015 <- 
  num_labor_2015_raw %>%
  dplyr::filter(cat04_code == "0000",!(cat03_code %in% c("1760", "1770", "1790"))) %>%
  dplyr::select(
    cat03_code, # age
    area_code,
    value
    ) %>%
  tidyr::spread(
    key = cat03_code,
    value = value
    ) %>%
  dplyr::rename(
    labor_all = "0000",
    labor_15 = "1210",
    labor_20 = "1220",
    labor_25 = "1230",
    labor_30 = "1240",
    labor_35 = "1250",
    labor_40 = "1260",
    labor_45 = "1270",
    labor_50 = "1280",
    labor_55 = "1290",
    labor_60 = "1300",
    labor_65 = "1310",
    labor_70 = "1320",
    labor_75 = "1330",
    labor_80 = "1340",
    labor_85 = "1450"
  ) %>%
  dplyr::mutate(year = 2015)

num_labor <- dplyr::bind_rows(num_labor_2010, num_labor_2015)

num_labor <-
  num_labor %>%
  dplyr::mutate(
    area_code = as.numeric(area_code)
  )

saveRDS(num_labor, file = "cleaned/data_area_year_num_labor.RDS")




### Number of employed people ---------------------------------------------

num_emp <- dplyr::bind_rows(num_emp_2010, num_emp_2015)

num_emp <-
  num_emp %>%
  dplyr::mutate(
    area_code = as.numeric(area_code)
  )

saveRDS(num_emp, file = "cleaned/data_area_year_num_emp.RDS")



### Number of part-time or temp workers -----------------------------------


num_parttemp <- dplyr::bind_rows(num_parttemp_2010, num_parttemp_2015)

num_parttemp <-
  num_parttemp %>%
  dplyr::mutate(
    area_code = as.numeric(area_code)
  )

saveRDS(num_parttemp, file = "cleaned/data_area_year_num_parttemp.RDS")


### Part-time wage --------------------------------------------------------

partwage_city <- 
  partwage_city %>%
  dplyr::rename(ptwage = hrp) %>%
  dplyr::mutate(ptwage = ptwage * 8 / 100) %>%
  dplyr::rename(
    area_code = citycode
  ) %>%
  dplyr::mutate(area_code = prefecture * 1000 + area_code) %>%
  dplyr::select(-prefecture)

saveRDS(partwage_city, file = "cleaned/data_area_year_partwage.RDS")


## Prefecture-year level --------------------------------------------------


### Part-time wage --------------------------------------------------------

partwage_pref <- 
  partwage_pref %>%
  dplyr::rename(
    ptwage = hrp,
    pref = prefecture
    ) %>%
  dplyr::mutate(ptwage = ptwage * 8 / 100) 

saveRDS(partwage_pref, file = "cleaned/data_pref_year_partwage.RDS")

### Minimum wage ----------------------------------------------------------

mw <- 
  mw %>%
  dplyr::select(
    PrefectureCode,
    mw2010:mw2014
  ) %>%
  dplyr::rename(pref = PrefectureCode)

mw <- 
  mw %>%
  tidyr::gather(
    key = year,
    value = mw,
    mw2010:mw2014,
    factor_key = F
  ) %>%
  na.omit()

mw$year <- 
  gsub("mw", "", mw$year) %>%
  as.numeric()

mw <- 
  mw %>%
  dplyr::mutate(mw = mw * 8 / 10000) %>%  # Unit: 10K yen per day
  na.omit()

saveRDS(mw, file = "cleaned/data_pref_year_minimum_wage.RDS")


## zipcode level -----------------------------------------------------------

colnames(zipcode_city) <- c("pref_2", "mun", "zipcode")

# In some cases, multiple municipalities correspond to a single zipcode.
# I keep one of them and drop the others.
zipcode_city <- 
  zipcode_city %>% 
  dplyr::rename(area_code = mun) %>%
  dplyr::arrange(zipcode, area_code) %>% 
  dplyr::group_by(zipcode) %>% 
  dplyr::mutate(id = sequence(dplyr::n())) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(id == 1) %>% 
  dplyr::select(-id, -pref_2) %>% 
  dplyr::arrange(area_code, zipcode) %>%
  dplyr::select(zipcode, area_code)
 
saveRDS(zipcode_city, file = "cleaned/data_zipcode.RDS")
