# cache raw data from google sheets locally

library(tidyverse)
library(googlesheets4) # package for reading data in spreadsheet directly into R
library(here) # pakcage for managing paths


MA_DATA_GOOGLE_SHEET_ID <- "1kSL5lpmHcaw9lOp2dhJ_RH15REFe7oZVwMO1vJc930o"
SHEET_NAME <- "action_matching_paradigm"
#OUTPATH <- here("data/raw/syntactic_bootstrapping_raw_data.csv")
OUTPATH <- here("data/raw/verbextension_raw_data.csv")
#MA_DATA_GOOGLE_SHEET_ID <- "1oD7gRlWjRUIR3Ngqdku11MTEJ92xh5X9xgAe5nxpphM"
#SHEET_NAME <- "double_check_molly"
#OUTPATH <- here("data/raw/syntactic_bootstrapping_raw_data_molly.csv")

read_raw_data_and_clean <- function(sheet_id, sheet_name, outpath){

  ma_data <- read_sheet(sheet_id, sheet_name,
                        col_types = "ccccccccccccccdddddddddccdddcccccccccccccddddccdc")
  #col_types = "cccccccccccccccdddddddddccdddccccccccccccccddddccdc"
                        #col_types = "ccccccccccccccdddddddddccdddccccccccccccccddddccdc")
  tidy_es <- ma_data %>%
    filter(paper_eligibility == "include") %>%
    filter(!is.na(t)| !is.na(d) | (!is.na(x_1) & !is.na(x_2) & !is.na(SD_1))) %>%
    mutate(mean_age = str_remove(mean_age, ",")) %>%
    write_csv(OUTPATH)

}

read_raw_data_and_clean(MA_DATA_GOOGLE_SHEET_ID, SHEET_NAME, OUTPATH)









