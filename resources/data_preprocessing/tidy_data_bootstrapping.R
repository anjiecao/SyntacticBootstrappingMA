library(tidyverse)
library(googlesheets4) # package for reading data in spreadsheet directly into R



read_raw_data_and_clean <- function(MA_DATA_GOOGLE_SHEET_ID){
  
  ma_data <- read_sheet(MA_DATA_GOOGLE_SHEET_ID, "MA data",
                        col_types = "ccccccccc?cccdddddddddcc???ccccccccccccddddccdc")
  tidy_es <- ma_data %>%
    filter(paper_eligibility == "include") %>%
    select(-paper_eligibility, -exclusion_reason, -data_source, -long_cite, -note) %>%
    filter(!is.na(t)| !is.na(d) | (!is.na(x_1) & !is.na(x_2) & !is.na(SD_1))) %>%
    mutate(mean_age = str_remove(mean_age, ","),
           mean_age = as.numeric(mean_age))
  write_sheet(tidy_es, MA_DATA_GOOGLE_SHEET_ID, "MA data tidy new")
  
  
}









