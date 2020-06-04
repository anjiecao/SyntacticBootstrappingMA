# calculate ES for syntactic bootstrapping group
library(tidyverse)
library(here)
library(janitor)


INPATH <- here("data/raw/syntactic_bootstrapping_raw_data.csv")
OUTPATH <- here("data/processed/syntactic_bootstrapping_tidy_data.csv")

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


# read in raw data
ma_data <- read_csv(INPATH)

# add effect sizes
ma_data_with_es <- ma_data %>%
  filter(!is.na(t)| (!is.na(x_1) & !is.na(x_2) & !is.na(SD_1)) | (!is.na(d))) %>%
  nest(data = c(n_1, x_1,x_2,x_2_raw,SD_1,SD_2,SD_2_raw,t,d)) %>%
  mutate(es = map(data, get_es)) %>%
  unnest(cols = c(es,data)) %>%
  mutate(d_calc = ifelse(d_calc == Inf, NA_real_, d_calc),
         d_var_calc = ifelse(d_var_calc == Inf, NA_real_, d_var_calc))

# clean up factor level issues
tidy_es <- ma_data_with_es %>% # it's best practice not to write over existing variables
  clean_names() %>% # tidy column names
  filter(!is.na(d_calc)) %>%  # use line breaks to make code more readable
  mutate(practice_phase = case_when(practice_phase == "NA" ~ "no",
                                    TRUE ~ practice_phase), # case_when is a better version of ifelse (no need to nest ifelse statements)
         character_identification = case_when(character_identification == "NA" ~ "no",
                                              TRUE ~ character_identification),
         presentation_type = case_when(presentation_type == "immediate-after" ~ "immediate_after",
                                       presentation_type == "Immediate-after" ~ "immediate_after",
                                       TRUE ~ presentation_type),
         patient_argument_type = case_when(patient_argument_type == "N/A" ~ "NA",
                                           TRUE ~ patient_argument_type),
         population_type = case_when(population_type == "typical_developing" ~  "typically_developing",
                                     TRUE ~ population_type),
         agent_argument_type = case_when(agent_argument_type == "pronoun_and_noun" | agent_argument_type == "pronoung_and_noun" ~ "noun_and_pronoun",
                                         TRUE ~ agent_argument_type),
         test_type = case_when(test_type == "actor" ~ "agent",
                               TRUE ~ test_type))

write_csv(tidy_es, OUTPATH)

