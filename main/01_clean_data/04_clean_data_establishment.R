rm(list = ls())
gc()

library("magrittr")

# Read data ---------------------------------------------------------------

data <- haven::read_dta("intermediate/data_2010_2014.dta")

# Deal with non-numeric characters ----------------------------------------

# Remove comma
data <- 
  data %>%
  dplyr::mutate_all(
    ~ gsub(",", "", .)
  )

data <-
  data %>%
  dplyr::mutate_all(
    ~ gsub("[[:space:]]", "", .)
  )

data <-
  data %>%
  dplyr::mutate(
    wage =
      wage %>%
      gsub(
        "19535/平均",
        "19535",
        .
      ) %>%
      gsub(
        "9822\\(交通費691円含\\)",
        "9131",
        .
      ) %>%
      gsub(
        "約23300",
        "23300",
        .
      ) %>%
      gsub(
        "6000～8000",
        "7000",
        .
      ) %>%
      gsub(
        "7142\\(921\\)",
        "7142",
        .
      ),
    fee = 
      fee %>%
      gsub(
        "NAME\\?",
        "",
        .
      ) %>%
      gsub(
        "10500～16800",
        "13650",
        .
      ) %>%
      gsub(
        "\\?",
        "",
        .
      ),
    sales = 
      sales %>%
      gsub(
        ".*〓.*",
        "",
        .
      ) %>%
      gsub(
        "万",
        "0000",
        .
      )
  )

data$wage[which(is.na(as.numeric(data$wage)))] %>% unique()
data$fee[which(is.na(as.numeric(data$fee)))] %>% unique()
data$sales[which(is.na(as.numeric(data$sales)))] %>% unique()

data <-
  data %>%
  dplyr::mutate(
    dplyr::across(
      -firm_id,
      ~ gsub(
        ".*〓.*",
        "",
        .
      )      
    )
  )

data <-
  data %>%
  dplyr::mutate_all(
    ~ gsub(
      "~.*",
      "",
      .
    ) %>%
      gsub(
        "～.*",
        "",
        .
      )
  )


# Check non-numeric characters
val <- c()
var <- c()
for (i in 1:dim(data)[2]) {
  if (i != 4) {
    loc <- which(is.na(as.numeric(data[[i]])))
    x <- data[[i]][loc]
    val <- c(val, x)
    var <- c(var, rep(colnames(data)[i], length(x)))
  }
}

nonnum_characters <- 
  data.frame(var = var, val = val) %>%
  dplyr::group_by(var, val) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup()

for (i in 1:dim(data)[2]) {
  if (i != 4) {
    data[[i]] <- as.numeric(data[[i]])
  }
}

data <-
  data %>%
  dplyr::mutate(
    dplyr::across(
      -zipcode,
      as.numeric
    )
  )

# Make unique id ----------------------------------------------------------

data$id_unique <- 1:dim(data)[1]
data <- 
  data %>%
  dplyr::select(
    id_unique, 
    dplyr::everything()
    )


# Deal with missing values ------------------------------------------------

# Replace na with zero for some variables
data <- data  %>%
  tidyr::replace_na(
    list(
      tempdaily_fte = 0,
      tempperm_fte = 0,
      tempfixed_fte = 0,
      tempdaily = 0,
      tempperm = 0,
      tempfixed = 0,
      perm = 0,
      fixed = 0,
      daily = 0,
      trainingnumber = 0,
      shokai = 0,
      oversea = 0,
      cocurrent = 0
    )
  )


# Somehow indicators not taking 2, 3, ...
data <- data %>%
  dplyr::mutate(
    cocurrent = dplyr::if_else(cocurrent > 1, 1, cocurrent),
    oversea = dplyr::if_else(oversea > 1, 1, oversea),
    shokai = dplyr::if_else(shokai > 1, 1, shokai)
  )


# Replace zero with na for wages, fees
data <- data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("wage"), 
      ~ dplyr::na_if(., 0)
      )
    ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("fee"),  
      ~ dplyr::na_if(., 0)
      )
    )



# Change unit -------------------------------------------------------------

data <- 
  data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("wage"), 
      ~ .x / 10000
      )
    ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("fee"),  
      ~ .x / 10000
      )
    ) %>%
  dplyr::mutate(sales = sales / 10000)


# Deal with some variables that are not consistent over time --------------

data <- 
  data %>%
  dplyr::mutate(
    .,
    wage16 = rowMeans(
      dplyr::select(., wage16, wage16_1, wage16_2, wage16_unnamed),
      na.rm = T
    ),
    fee16 = rowMeans(
      dplyr::select(., fee16, fee16_1, fee16_2, fee16_unnamed),
      na.rm = T
    )
  )

data <- 
  data %>%
  dplyr::select(
    -c(
      sales10m,
      sales10m_50m,
      sales50m_100m,
      sales100m_500m,
      sales500m_1b,
      sales1b,
      fee16_1,
      fee16_2,
      fee16_unnamed,
      wage16_1,
      wage16_2,
      wage16_unnamed,
      wage_5_10,
      fee_5_10,
      unnamed1,
      unnamed2
    ),
    # Only H25 data have these variables in addition to the amount of sales-unnamed1,-unnamed2,
    # Only H25 data have these variables. # daily workers + # fixed workers-wage16_1,-wage16_2,-wage16_unnamed,-fee16_1,-fee16_2,-fee16_unnamed,-fee_5_10,
    # Doesn't exist before H24-wage_5_10   # Doesn't exist before H24
  )

saveRDS(data, file = "cleaned/data_establishments.RDS")
