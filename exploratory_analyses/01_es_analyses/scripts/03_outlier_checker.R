library(tidyverse)
library(here)
DATA_PATH <- here("data/processed/syntactic_bootstrapping_tidy_data.csv") # make all variables (i.e. things that might change) as capital letters at the top of the scripts
OUTPATH <- here("data/processed/outlier.csv")


ma_data <- read_csv(DATA_PATH)   %>%
  filter(language == "English",
         population_type == "typically_developing", 
         !is.na(mean_age))

CONTINUOUS_VARS <- c("n_1", "x_1", "sd_1", "d_calc", "d_var_calc", "mean_age")

long_continuous <- ma_data %>%
  pivot_longer(cols = CONTINUOUS_VARS)

outlier_criteria <- long_continuous %>%
  group_by(name) %>%
  summarize(mean = mean(value),
            sd = sd(value)) %>%
  mutate(outlier_criteria_upper = mean + 3*sd,
         outlier_criteria_lower = mean - 3*sd)


ma_data_outlier <- ma_data %>% 
  filter(n_1 < (outlier_criteria %>% filter(name == "n_1") %>% pull(outlier_criteria_lower)) | 
         n_1 > (outlier_criteria %>% filter(name == "n_1") %>% pull(outlier_criteria_upper)) | 
         x_1 < (outlier_criteria %>% filter(name == "x_1") %>% pull(outlier_criteria_lower)) |
         x_1 > (outlier_criteria %>% filter(name == "x_1") %>% pull(outlier_criteria_upper)) |
         sd_1 < (outlier_criteria %>% filter(name == "sd_1") %>% pull(outlier_criteria_lower)) |
         sd_1 > (outlier_criteria %>% filter(name == "sd_1") %>% pull(outlier_criteria_upper)) |
         d_calc < (outlier_criteria %>% filter(name == "d_calc") %>% pull(outlier_criteria_lower)) |
         d_calc > (outlier_criteria %>% filter(name == "d_calc") %>% pull(outlier_criteria_upper)) |
         mean_age < (outlier_criteria %>% filter(name == "mean_age") %>% pull(outlier_criteria_lower)) |
         mean_age >  (outlier_criteria %>% filter(name == "mean_age") %>% pull(outlier_criteria_upper))) 

write_csv(ma_data_outlier, OUTPATH)
