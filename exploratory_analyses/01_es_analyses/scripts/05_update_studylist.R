library(tidyverse)


library(tidyverse)
library(googlesheets4) # package for reading data in spreadsheet directly into R
library(here) # pakcage for managing paths

MA_DATA_GOOGLE_SHEET_ID <- "1kSL5lpmHcaw9lOp2dhJ_RH15REFe7oZVwMO1vJc930o"
SHEET_NAME <- "MA data"
OUTPATH <- here("data/raw/reference_list.csv")


read_raw_data_and_clean <- function(sheet_id, sheet_name, outpath){
  
  ma_data <- read_sheet(sheet_id, sheet_name,
                        col_types = "cccccccccccccdddddddddccdddccccccccccccddddccdc")
  tidy_es <- ma_data %>%
    filter(paper_eligibility == "include") %>% select(long_cite) %>% distinct() %>%  write.csv(OUTPATH)
  #         mean_age = as.numeric(mean_age))
  
}

read_raw_data_and_clean(MA_DATA_GOOGLE_SHEET_ID, SHEET_NAME, OUTPATH)
