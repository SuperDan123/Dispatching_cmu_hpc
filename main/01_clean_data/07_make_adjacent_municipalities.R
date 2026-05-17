
# initialize --------------------------------------------------------------

rm(list = ls())
gc()

library(magrittr)


# read data ---------------------------------------------------------------

temp1 <- read.csv(file("rawdata/meshdata/01-1.csv", encoding = "cp932"))
temp2 <- read.csv(file("rawdata/meshdata/01-2.csv", encoding = "cp932"))
temp3 <- read.csv(file("rawdata/meshdata/01-3.csv", encoding = "cp932"))
map <-
  sf::st_read(
    "rawdata/shape_files/city_location/P34-14_01.shp",
    options = "ENCODING=CP932",
    stringsAsFactors = F
  )


for (i in 2:47) {
  pref <- as.character(i)
  if (nchar(pref) == 1) {
    filename <-
      paste("rawdata/shape_files/city_location/P34-14_0",
            pref,
            ".shp",
            sep = "")
  } else {
    filename <-
      paste("rawdata/shape_files/city_location/P34-14_",
            pref,
            ".shp",
            sep = "")
  }
  
  temp <- sf::st_read(filename,
                      options = "ENCODING=CP932",
                      stringsAsFactors = F)
  map <- dplyr::bind_rows(map, temp)
  
}


# clean data --------------------------------------------------------------

## make adjacent municipalites --------------------------------------------

meshdata <-  dplyr::bind_rows(temp1, temp2, temp3)

for (i in 2:9) {
  filename <-
    paste("rawdata/meshdata/0", as.character(i), ".csv", sep = "")
  meshdata_i <- read.csv(file(filename, encoding = "cp932"))
  meshdata <- dplyr::bind_rows(meshdata, meshdata_i)
}

for (i in 10:47) {
  filename <-
    paste("rawdata/meshdata/", as.character(i), ".csv", sep = "")
  meshdata_i <- read.csv(file(filename, encoding = "cp932"))
  meshdata <- dplyr::bind_rows(meshdata, meshdata_i)
}

meshdata <- meshdata[, c(1, 3)]
colnames(meshdata) <- c("mun", "meshcode")

mun_vec <- unique(meshdata$mun)
mun1 <- c()
mun2 <- c()

for (i in 1:length(mun_vec)) {
  m <- mun_vec[i]
  temp <- meshdata[meshdata$mun == m, 2]
  adj  <- meshdata[meshdata$meshcode %in% temp, 1] %>%
    unique()
  
  mun1 <- c(mun1, rep(m, length(adj)))
  mun2 <- c(mun2, adj)
  
}

mun_adj <- 
  data.frame(area_code = mun1, adj = mun2) %>%
  dplyr::filter(area_code != adj) %>%
  dplyr::arrange(area_code, adj)

saveRDS(mun_adj, file = "cleaned/data_area_adjacent_municipalities.RDS")


## compute distance between municipalities --------------------------------

colnames(map) <-
  c("mun", "classification", "name", "location", "geometry")

map <- 
  map %>%
  dplyr::filter(classification == 1) %>%
  dplyr::select(mun, geometry) %>%
  dplyr::mutate(mun = as.numeric(mun))


map %>%
  dplyr::pull(geometry) %>%
  lapply("[[", 1) %>%
  unlist() -> lon

map %>%
  dplyr::pull(geometry) %>%
  lapply("[[", 2) %>%
  unlist() -> lat

# Compute distance (in meters)
distance <- 
  data.frame(lon = lon, lat = lat) %>%
  geosphere::distm(fun = geosphere::distGeo) %>%
  as.data.frame()

colnames(distance) <- map$mun
distance$mun1 <- map$mun

distance <- 
  distance %>%
  tidyr::gather(key = mun2, value = distance,-mun1) %>%
  dplyr::mutate(mun2 = as.numeric(mun2)) %>%
  dplyr::rename(
    area_code_1 = mun1,
    area_code_2 = mun2
  )

saveRDS(distance, file = "cleaned/data_area_area_distance.RDS")



