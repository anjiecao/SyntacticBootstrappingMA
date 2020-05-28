# calculate ES for syntactic bootstrapping group
library(tidyverse)
library(googlesheets4) # package for reading data in spreadsheet directly into R


MA_DATA_GOOGLE_SHEET_ID <- "1kSL5lpmHcaw9lOp2dhJ_RH15REFe7oZVwMO1vJc930o"
ma_data <- read_sheet(MA_DATA_GOOGLE_SHEET_ID, "MA data tidy new")

# ES function - adopted from compute_es (within-one case)
get_es <- function(df){

  if (!is.na(df$x_1) & !is.na(df$x_2) & !is.na(df$SD_1)) {
    d_calc <- (df$x_1 - df$x_2) / df$SD_1
    es_method  <- "group_means_one"
  } else if (!is.na(df$t)) {
    d_calc <- df$t / sqrt(df$n_1)
    es_method  <- "t_one"
  } else  if (!is.na(df$d)) {
    d_calc <- df$d
    es_method  <- "d_one"
  }
  if (!is.na(d_calc)) {
    d_var_calc <- (1 / df$n_1) + (d_calc ^ 2 / (2 * df$n_1)) # this models what is done in metafor package, escalc(measure="SMCR"() (Viechtbauer, 2010)
  }

  data.frame(d_calc = d_calc,
             d_var_calc = d_var_calc,
             es_method = es_method)
}



write_tidy_sheet <- function(MA_DATA_GOOGLE_SHEET_ID){
  
  ma_data <- read_sheet(MA_DATA_GOOGLE_SHEET_ID, "MA data tidy new")
  tidy_es <- ma_data %>%
    filter(!is.na(t)| (!is.na(x_1) & !is.na(x_2) & !is.na(SD_1)) | (!is.na(d))) %>%
    nest(data = c(n_1, x_1,x_2,x_2_raw,SD_1,SD_2,SD_2_raw,t,d)) %>%
    mutate(es = map(data, get_es)) %>% 
    unnest(cols = c(es,data)) %>% 
    mutate(d_calc = ifelse(d_calc == Inf, NA_real_, d_calc),
           d_var_calc = ifelse(d_var_calc == Inf, NA_real_, d_var_calc))
  write_sheet(tidy_es, MA_DATA_GOOGLE_SHEET_ID, "MA data tidy with ES NEW")

}  