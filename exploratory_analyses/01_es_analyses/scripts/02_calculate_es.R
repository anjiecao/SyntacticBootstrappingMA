# calculate ES for syntactic bootstrapping group
library(tidyverse)
library(here)
library(janitor)
library(gsubfn)


#INPATH <- here("data/raw/syntactic_bootstrapping_raw_data_molly.csv")
#OUTPATH <- here("data/processed/syntactic_bootstrapping_tidy_data_molly.csv")
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
  if(!is.na(d_calc)) {
    d_var_calc <- (1 / df$n_1) + (d_calc ^ 2 / (2 * df$n_1))# this models what is done in metafor package, escalc(measure="SMCR"() (Viechtbauer, 2010)
  }

  data.frame(d_calc = d_calc,
             d_var_calc = d_var_calc,
             es_method = es_method)
}


# read in raw data
ma_data <- read_csv(INPATH, col_types = "cccccccccccccddddddddddccdddccccccccccccccddddccdc")

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
  clean_names() %>%
  select(-long_cite) %>%
  filter(!is.na(d_calc) & paper_eligibility == "include") %>% # tidy column names
  mutate(id = row_number(),
        practice_phase = case_when(practice_phase == "NA" ~ "no",
                                    is.na(practice_phase) ~ "no",
                                    TRUE ~ practice_phase), # case_when is a better version of ifelse (no need to nest ifelse statements)
         character_identification = case_when(character_identification == "NA" ~ "no",
                                              is.na(character_identification) ~ "no",
                                              TRUE ~ character_identification),
         presentation_type = case_when(presentation_type == "immediate-after" ~ "immediate_after",
                                       presentation_type == "Immediate-after" ~ "immediate_after",
                                       TRUE ~ presentation_type),
         stimuli_actor = case_when(stimuli_actor == "non-person" ~ "non_person",
                                  TRUE ~ stimuli_actor),
         patient_argument_type = case_when(patient_argument_type == "N/A" ~ "NA",
                                           TRUE ~ patient_argument_type),
         population_type = case_when(population_type == "typical_developing" ~  "typically_developing",
                                     TRUE ~ population_type),
         agent_argument_type = case_when(agent_argument_type == "pronoun_and_noun" | agent_argument_type == "pronoung_and_noun" ~ "noun_and_pronoun",
                                         TRUE ~ agent_argument_type),
         test_type = case_when(test_type == "actor" ~ "agent",
                               TRUE ~ test_type),
         unique_infant = case_when(same_infant == "no" ~ "unique_condition",
                                   same_infant == NA ~ "unique_condition",
                                   same_infant == "NA" ~ "unique_condition",
                                   TRUE ~ "not_unique"),
         same_infant = case_when(same_infant == "no" ~ as.character(id),
                                 TRUE ~ same_infant),
         test_method = case_when(
             grepl("point", dependent_measure, fixed = TRUE) ~ "point",
             grepl("look", dependent_measure, fixed = TRUE) ~ "look",
             TRUE ~ dependent_measure),
         agent_argument_type_clean = case_when(
          agent_argument_type == "2noun" ~ "noun_phrase",
          agent_argument_type == "2nouns" ~ "noun_phrase",
          agent_argument_type == "two_nouns" ~ "noun_phrase",
          agent_argument_type == "noun_with_adjectives" ~ "noun_phrase",
          agent_argument_type == "noun_and_pronoun" ~ "varying_agent",
          agent_argument_type == "two_nouns_and_pronoun" ~ "varying_agent",
          TRUE ~ agent_argument_type
        ),
        agent_argument_number = case_when(
          agent_argument_type == "2noun" ~ "2",
          agent_argument_type == "2nouns" ~ "2",
          agent_argument_type == "two_nouns" ~ "2",
          agent_argument_type == "noun_with_adjectives" ~ "1",
          agent_argument_type == "noun_and_pronoun" ~ "varying",
          agent_argument_type == "two_nouns_and_pronoun" ~ "varying",
          TRUE ~ "1"
        ),
        agent_argument_type2 = case_when(
          str_detect(agent_argument_type, "pronoun") ~ "pronoun",
          TRUE ~ "noun"),
        transitive_event_type2 = case_when(transitive_event_type == "direct_caused_action" ~ "direct_caused_action",
                                           TRUE ~ "indirect_caused_action"),
        transitive_event_type = case_when(
          transitive_event_type == "direct_ caused_movement"~ "direct_caused_action",
          transitive_event_type == "direct_caused_movement"~ "direct_caused_action",
          transitive_event_type == "direct_caused_movement"~ "direct_caused_action",
          transitive_event_type == "indirect_cause_action"~ "indirect_caused_action",
          TRUE ~ transitive_event_type
        ),

        patient_argument_type_clean = case_when(
          patient_argument_type ==  "noun_and_dropping" ~ "varying_patient",
          patient_argument_type == "pronoun_and_noun" ~ "varying_patient",
          TRUE ~ patient_argument_type,
        ),

        visual_stimuli_pair = paste(transitive_event_type, intransitive_event_type, sep = '_'),

        adult_participant = case_when(
          is.na(mean_age) ~ "yes",
          TRUE ~ "no"
        ),
        data_source_clean = case_when(
          grepl("text", data_source, fixed = TRUE) ~ "text",
          grepl("author", data_source, fixed = TRUE) ~ "author_contact",
          grepl("table", data_source, fixed = TRUE) ~ "table",
          TRUE ~ "plot"
        ),
        paradigm_type = case_when(
          (transitive_event_type == "AGENT") ~ "agent_matching",
          TRUE ~ "action_matching"
        ),
        publication_year = parse_number(unique_id)
        ) %>%
  mutate(patient_argument_type_clean = if_else (is.na(patient_argument_type),
                                                "intransitive",
                                                patient_argument_type_clean)) %>%
  filter(paradigm_type!= "agent_matching") %>%
  select(-id)
    # use line breaks to make code more readable
  # & (sentence_structure == "intransitive")

write_csv(tidy_es, OUTPATH)


