# This code loads the dataset from Google docs

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(here)

file_timestamp <- here::here("data", "updated.rds")
file_raw_data <- here::here("data", "rawdata.rds")


write_ts <- function() {
  timestamp <- lubridate::now()
  write_rds(timestamp, file_timestamp)
}

read_ts <- function() {
  if(file.exists(file_timestamp)) {
    timestamp <- read_rds(file_timestamp) 
  }
  else timestampe <- lubridate::now()
  timestamp
}


read_raw_data <- function() {
  # check age of data
  timestamp <- read_ts()
  now <- lubridate::now()
  data_age <- lubridate::interval(timestamp, now)
  
  if (time_length(data_age, "hours") > 12) {
    sheets_auth()
    fsrc <- sheets_find("nCoV2019_2020_line_list_open")
    
    raw <- sheets_read(fsrc$id)
    write_rds(raw, file_raw_data)
  } else {
    cat(paste("Data age:", as.duration(data_age), "- Using cached version."))
    raw <- read_rds(file_raw_data)
  }
  raw
}

raw <- read_raw_data()

data <- raw %>% select(ID, age, sex, country, date_confirmation, chronic_disease, outcome)
data$sex %>% as.factor() %>% summary()
data$country %>% as.factor() %>% summary()











