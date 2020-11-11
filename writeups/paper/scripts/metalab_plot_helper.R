library(tidyverse)
library(metafor)
library(metalabr)
library(here)


fit_model <- function(data, dataset_name){

  if (dataset_name == "Syntactic Bootstrapping"){
    null_model <- rma.mv(d_calc ~ 1,
                         V = d_var_calc,
                         random = ~ 1 | short_cite/same_infant/row_id,
                         method = "REML",
                         data = data)

  }else{

  null_model <- rma.mv(d_calc ~ 1,
                       V = d_var_calc,
                       random = ~ 1 | short_cite/same_infant/unique_row,
                       method = "REML",
                       data = data)
  }

  params <- data.frame(dataset_name =  dataset_name,
                       null_estimate = null_model$b[1],
                       null_estimate.cil = null_model$ci.lb[1],
                       null_estimate.cih = null_model$ci.ub[1],
                       null_z = null_model$zval[1],
                       null_p = null_model$pval[1]
  )

  return(params)

}

get_model_results_younger_than <- function(
  dataset_name, max_age_months){

  ml_dataset_info <- metalabr::get_metalab_dataset_info()

  ME_info <- ml_dataset_info %>%
    filter(name == "Mutual exclusivity")
  SS_info <- ml_dataset_info %>%
    filter(name == "Sound symbolism")
  XS_info <- ml_dataset_info %>%
    filter(name == "Cross-situational word learning")
  GF_info <- ml_dataset_info %>%
    filter(name == "Gaze following")

  ME_data <- metalabr::get_metalab_data(ME_info) %>%
    mutate(dataset = "Mutual exclusivity")
  SS_data <- metalabr::get_metalab_data(SS_info) %>%
    mutate(dataset = "Sound symbolism")
  XS_data <- metalabr::get_metalab_data(XS_info) %>%
    mutate(dataset = "Cross-situational word learning")
  GF_data <- metalabr::get_metalab_data(GF_info) %>%
    mutate(dataset = "Gaze following")

  ALL_data <- bind_rows(ME_data,
                        SS_data,
                        XS_data,
                        GF_data) %>%
    mutate(mean_age_months = mean_age_1 / 30.44)


  mod_data <- ALL_data %>%
    filter(dataset == dataset_name, mean_age_months < max_age_months)

  mod_results <- fit_model(mod_data, dataset_name)

  return (mod_results)


}

tidy_metalab_df <- function(raw_df){
  all_models_younger_tidy <- raw_df %>%
    pivot_longer(
      null_estimate:null_p,
      names_to = "model_name",
      values_to = "value"
    ) %>%
    mutate(
      model_type = case_when(
        grepl("null", model_name) ~ "null",
      ),
      value_type = gsub(".*_", "", model_name)
    ) %>%
    pivot_wider(
      names_from = value_type,
      values_from = value
    ) %>%
    group_by(dataset_name, model_type) %>%
    mutate_at(vars(-group_cols()), function(x) {x[!is.na(x)][1]}) %>%
    distinct() %>%
    select(-model_name)

   return(all_models_younger_tidy)
}

summarize_metalab_age_younger_than <- function(SB_data, max_age_months){

  ml_dataset_info <- metalabr::get_metalab_dataset_info()

  ME_info <- ml_dataset_info %>%
    filter(name == "Mutual exclusivity")
  SS_info <- ml_dataset_info %>%
    filter(name == "Sound symbolism")
  XS_info <- ml_dataset_info %>%
    filter(name == "Cross-situational word learning")
  GF_info <- ml_dataset_info %>%
    filter(name == "Gaze following")

  ME_data <- metalabr::get_metalab_data(ME_info) %>%
    mutate(dataset = "Mutual exclusivity")
  SS_data <- metalabr::get_metalab_data(SS_info) %>%
    mutate(dataset = "Sound symbolism")
  XS_data <- metalabr::get_metalab_data(XS_info) %>%
    mutate(dataset = "Cross-situational word learning")
  GF_data <- metalabr::get_metalab_data(GF_info) %>%
    mutate(dataset = "Gaze following")

  ALL_data <- bind_rows(ME_data,
                        SS_data,
                        XS_data,
                        GF_data)

  age_data_younger <- bind_rows(ME_data,
                                SS_data,
                                XS_data,
                                GF_data) %>%
    select(dataset, mean_age_1) %>%
    mutate(mean_age_months = mean_age_1 / 30.44) %>%
    filter(mean_age_months < max_age_months)

  sb_data_younger <- SB_data %>%
    mutate(mean_age_1 = mean_age,
           unique_row = row_number(),
           dataset = "Syntactic Bootstrapping",
           mean_age_months = mean_age / 30.44) %>%
    select(dataset, mean_age_months, mean_age) %>%
    filter(mean_age_months < max_age_months) %>%
    rename(mean_age_1 = mean_age)

  age_data_younger <- bind_rows(age_data_younger, sb_data_younger)


  summarize_age_younger <- age_data_younger %>%
    group_by(dataset) %>%
    summarise(
      mean_age = mean(mean_age_1),
      mean_age_in_month = mean_age / 30.44,
      sd_age = sd(mean_age_1),
      num_study = n()
    ) %>%
    mutate(
      dataset_name = dataset
    )

  return(summarize_age_younger)


}

generate_metalab_plot <- function(res_with_age){

  res_with_age %>%
    mutate(
      text_color = case_when(
        dataset_name == "Syntactic Bootstrapping" ~ "red",
        TRUE ~  "black"
      ),
      dataset_name = case_when(
        dataset_name == "Cross-situational word learning" ~ "Cross-situational\n word learning",
        dataset_name == "Syntactic Bootstrapping" ~ "Syntactic \n Bootstrapping",
        dataset_name == "Mutual exclusivity" ~ "Mutual \n Exclusivity",
        dataset_name == "Gaze following" ~ "Gaze \n Following",
        dataset_name == "Sound symbolism" ~ "Sound \n Symbolism",
        TRUE ~ dataset_name
      )
    ) %>%
    ggplot(aes(x = mean_age_in_month,
               y = estimate,
               ymin = estimate.cil,
               ymax = estimate.cih,
               size = (num_study * 0.1),
               color = text_color
    )) +
    guides(size = FALSE) +
    xlim(0, 36) +
    coord_flip() +
    geom_point() +
    scale_color_manual(values = c("black", "red")) +
    guides(color = FALSE) +
    geom_text(aes(label = dataset_name), hjust = 0.5, vjust = -.5, size = 3) +
    geom_linerange(size = 0.5) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed")+
    theme(
      axis.line = element_line(size = 1.2),
      axis.ticks = element_line(size = 1),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank()) +
    xlab("Mean Age (Months)") +
    ylab(expression(paste("Effect Size (Cohen's ", italic(d), ")")))

}

get_metalab_es_print <- function(summary_df){

  tidy_print_val <- summary_df %>%
    mutate(
      estimate_print = round(estimate, 2),
      estimate_cil_print = round(estimate.cil, 2),
      estimate_cih_print = round(estimate.cih, 2),
      estimate_print_full = paste0(
        estimate_print,
        " [",
        estimate_cil_print,
        ", ",
        estimate_cih_print,
        "]",
        sep = ""
      ))

  return(tidy_print_val)

}


