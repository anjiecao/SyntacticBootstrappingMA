# cache raw data from google sheets locally

library(tidyverse)
library(googlesheets4) # package for reading data in spreadsheet directly into R
library(here) # pakcage for managing paths

#current version downloaded 2PM 12/29/2020 

MA_DATA_GOOGLE_SHEET_ID <- "1boqZgMNk-BfmHEgiMVNhgWG63nfhAauO4PdcVP3TXRo"
SHEET_NAME <- "Sheet1"
OUTPATH <- here("data/raw/syntactic_bootstrapping_raw_data.csv")


read_raw_data_and_clean <- function(sheet_id, sheet_name, outpath){

  ma_data <- read_sheet(sheet_id, sheet_name,

                        col_types = "ccccccccccccccdccdddccccccccccccccddddccdcdddddddddddcccccccd")
  tidy_es <- ma_data %>%
    mutate(mean_age = str_remove(mean_age, ",")) %>%
    write_csv(OUTPATH)

}

read_raw_data_and_clean(MA_DATA_GOOGLE_SHEET_ID, SHEET_NAME, OUTPATH)









