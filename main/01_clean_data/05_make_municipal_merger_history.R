
# initialize --------------------------------------------------------------

rm(list = ls())
gc()

library(magrittr)


# read data ---------------------------------------------------------------

mun_history <-
  readxl::read_excel(
    "rawdata/municipality_code_update_history.xls",
    range = "E5:N1440",
    col_names = FALSE
  )
date2 <-
  readxl::read_excel(
    "rawdata/municipality_code_update_history.xls",
    range = "J5:J1440",
    col_types = c("date"),
    col_names = FALSE
  )

# clean data --------------------------------------------------------------

colnames(mun_history) <- 
  c(
    "pref",
    "code_old",
    "name_old",
    "furigana_old",
    "classification",
    "date",
    "code_new",
    "name_new",
    "furigana_new",
    "reason"
  )

mun_history <- 
  mun_history %>%
  dplyr::select(
    -dplyr::starts_with("furigana"),
    -classification
    )

# A single column on date has two types of data
# 1. H22.01.01 etc., 2. 2010/01/01 etc.

# 1. Convert H22.01.01 -> 2010 1 1
date1 <- 
  mun_history %>%
  dplyr::select(date) %>%
  tidyr::separate(
    date,
    into = c("year", "month", "day"),
    sep = "\\."
    )

date1$year[!grepl("H", date1$year)] <- NA
date1$year <- gsub("H", "", date1$year)
date1$year <- as.numeric(date1$year) + 1988

date1 <- 
  date1 %>%
  dplyr::mutate_all(as.numeric)

# 2. Convert 2010/01/01 -> 2010 1 1
colnames(date2) <- c("date")

date2 <- 
  date2 %>%
  tidyr::separate(
    date,
    into = c("year", "month", "day"),
    sep = "-"
    ) %>%
  dplyr::mutate_all(as.numeric)

date1[is.na(date1)] <- 0
date2[is.na(date2)] <- 0
date <- 
  as.matrix(date1) + as.matrix(date2) %>%
  as.data.frame()
date[date == 0] <- NA

mun_history <- 
  mun_history %>%
  dplyr::select(-date) %>%
  dplyr::bind_cols(date)


# Make group id
pref <- mun_history$pref
id <- 0
group_id <- rep(0, length(pref))
for (i in 1:length(pref)) {
  id <- id+!is.na(pref[i])
  group_id[i] <- id
}
mun_history$id <- group_id
mun_history <- 
  mun_history %>%
  dplyr::select(id, dplyr::everything())



# Fill missing
mun_history[mun_history == "〃"] <- NA
mun_history$name_old[stringr::str_detect(mun_history$name_old, "\\(")] <-
  NA
mun_history$name_new[stringr::str_detect(mun_history$name_new, "\\(")] <-
  NA
mun_history[mun_history == "削除"] <- NA

mun_history <- 
  mun_history %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    reason = dplyr::first(reason),
    reason = gsub("\\n", "", reason)
    ) %>%
  dplyr::ungroup()

mun_history %>%
  dplyr::pull(reason) %>%
  unique() %>%
  print()

mun_history <- 
  mun_history %>%
  dplyr::mutate(
    reason = dplyr::if_else(reason == "政令指定都市へ移行", "政令指定都市", reason),
    reason = dplyr::if_else(reason == "編入合併後は", "編入合併", reason),
    reason = dplyr::if_else(reason == "合併後は", "合併", reason)
  )


mun_history <- 
  mun_history %>%
  tidyr::fill(pref, code_old, name_old, .direction = "down")

mun_history <- 
  mun_history %>%
  dplyr::group_by(id) %>%
  tidyr::fill(year, month, day, .direction = "downup")

mun_history <- 
  mun_history %>%
  dplyr::mutate(
    code_new = dplyr::if_else(code_new == "同左", code_old, code_new),
    name_new = dplyr::if_else(name_new == "同左", name_old, name_new)
  )


mun_history %>%
  dplyr::pull(reason) %>%
  unique() %>%
  print()

temp <- list()

# 市制施行
temp[[1]] <- 
  mun_history %>%
  dplyr::filter(reason == "市制施行") %>%
  dplyr::filter(!is.na(code_new))

# 編入合併
temp[[2]] <- 
  mun_history %>%
  dplyr::filter(reason == "編入合併") %>%
  dplyr::group_by(id) %>%
  tidyr::fill(code_new, name_new, .direction = "downup") %>%
  dplyr::filter(code_old != code_new) %>%
  dplyr::ungroup()

# 政令指定都市
temp[[3]] <- 
  mun_history %>%
  dplyr::filter(reason == "政令指定都市") %>%
  dplyr::filter(!is.na(code_new))

# 北海道における支庁制度改革に伴う所管区域の変更
temp[[4]] <- 
  mun_history %>%
  dplyr::filter(reason == "北海道における支庁制度改革に伴う所管区域の変更") %>%
  dplyr::filter(!is.na(code_new))

# 新設合併
temp[[5]] <- 
  mun_history %>%
  dplyr::filter(reason == "新設合併") %>%
  dplyr::group_by(id) %>%
  tidyr::fill(code_new, name_new, .direction = "downup")

# 名称変更
temp[[6]] <- 
  mun_history %>%
  dplyr::filter(reason == "名称変更") %>%
  dplyr::filter(!is.na(code_new))

# 合併
temp[[7]] <- 
  mun_history %>%
  dplyr::filter(reason == "合併") %>%
  dplyr::group_by(id) %>%
  tidyr::fill(code_new, name_new, .direction = "downup") %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(reason = "新設合併")

# さいたま市区の設置
temp[[8]] <- 
  mun_history %>%
  dplyr::filter(reason == "さいたま市区の設置")

mun_history <- 
  data.table::rbindlist(temp) %>%
  dplyr::arrange(id, code_old, code_new) %>%
  dplyr::mutate_at(
    c("code_old", "code_new"), 
    as.numeric
    ) %>%
  dplyr::mutate(
    code_old = floor(code_old / 10),
    code_new = floor(code_new / 10)
    )

saveRDS(mun_history, file = "intermediate/municipal_merger_history.RDS")
