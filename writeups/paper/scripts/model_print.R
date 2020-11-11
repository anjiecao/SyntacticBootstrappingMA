library(tidyverse)
library(insight)
library(janitor)
# This script will run single-moderator models and generate a dataframe to be used in results section 

fit_method_model <- function(additional_moderator, df){
  
  formula <- as.formula(paste0("d_calc ~ character_identification + practice_phase + 
                               presentation_type + test_mass_or_distributed + 
                               n_repetitions_sentence + ", additional_moderator, sep = ""))
  
  model <- rma.mv(formula, 
                  V = d_var_calc,
                  random = ~ 1 | short_cite/same_infant/row_id,
                  method = "REML",
                  data = df)
  
  return(model)
  
  
}

print_method_model <- function(model_res){
  
  beta <- model_res$beta %>% as.data.frame() %>% rownames_to_column()
  ci_lb <- model_res$ci.lb %>% as.data.frame() 
  ci_ub <- model_res$ci.ub %>% as.data.frame() 
  z_val <- model_res$zval %>% as.data.frame()
  p_val <- model_res$pval %>% as.data.frame()
  
  
  
  res_df <- bind_cols(beta, ci_lb, ci_ub, z_val, p_val) 
  
  colnames(res_df) <- c("mod_name",
    "beta", "ci_lb", "ci_ub", "z_val", "p_val")
  
  print_res <- res_df %>% 
    mutate(
      mod_name_print = case_when(
        mod_name == "intrcpt" ~ "Intercept",
        mod_name == "character_identificationyes" ~ "Character identification phase (No / Yes)", 
        mod_name == "practice_phaseyes" ~ "Practice phase (No / Yes)", 
        mod_name == "presentation_typesimultaneous" ~ "Stimuli synchronicity (Asynchronous / Simultaneous)",
        mod_name == "test_mass_or_distributedmass" ~ "Testing structure (Distributed / Mass)", 
        mod_name == "agent_argument_typepronoun" ~ "Noun phrase type (Noun / Pronoun)", 
        mod_name == "n_repetitions_sentence" ~ "Number of sentence repetitions",
        mod_name == "sentence_structuretransitive" ~ "Predicate type (Intransitive / Transitive)", 
        mod_name == "productive_vocab_median" ~ "Median productive vocabulary size",
        mod_name == "mean_age_months" ~ "Mean age (months)"
      ), 
      ci_lb_round = round(ci_lb, 2), 
      ci_ub_round = round(ci_ub, 2),
      
      ci_lb_print = case_when(
        ci_lb_round == 0 ~ ">-.001", 
        TRUE ~ as.character(ci_lb_round)
      ), 
      ci_ub_print = case_when(
        ci_ub_round == 0 ~ "<.001", 
        TRUE ~ as.character(ci_ub_round)
      ), 
      
      beta_print_full = case_when(
        round(beta, 2) == 0 ~ paste0("<.001"),
        TRUE ~ paste0(round(beta, 2), 
                      " [", 
                      ci_lb_print, 
                      ", ",
                      ci_ub_print, 
                      "]")
      ),
      z_val_print = round(z_val, 2), 
      p_val_print = case_when(
        p_val < 0.001 ~ "<.001", 
        round(p_val,2) <= 0.05 ~ paste0(as.character(round(p_val,2)), "*"), 
        TRUE ~ as.character(round(p_val, 2))
      )
    ) %>% 
    select(mod_name_print, beta_print_full, z_val_print, p_val_print)
  
  colnames(print_res) <- c("Parameter",
                        "Estimates", "z value", "p value")
  
  return(print_res)
  
  
}



get_MA_params <- function(moderator, df) {
  
  this_data <- df
  n = nrow(this_data)
  
  if (moderator == "NULL"){
    formula <- as.formula(paste0("d_calc ~ 1"))
  }else{
    formula <- as.formula(paste0("d_calc ~ ", moderator, sep = ""))
  }
  
  model <- rma.mv(formula, 
                  V = d_var_calc,
                  random = ~ 1 | short_cite/same_infant/row_id,
                  method = "REML",
                  data = this_data)
  
  this_moderator_estimate <- model$b[2]
  this_moderator_SE <- model$se[2]
  this_moderator_estimate.cil <- model$ci.lb[2]
  this_moderator_estimate.cih <- model$ci.ub[2]
  this_moderator_z <- model$zval[2]
  this_moderator_p <- model$pval[2]
  
  
  params <- data.frame(this_moderator = moderator,
                       n = n,
                       estimate = model$b[1],
                       estimate.cil = model$ci.lb[1],
                       estimate.cih = model$ci.ub[1],
                       z = model$zval[1],
                       p = model$pval[1],
                       
                       mod_estimate = this_moderator_estimate,
                       mod_SE = this_moderator_SE,
                       mod_estimate.cil = this_moderator_estimate.cil, 
                       mod_estimate.cih = this_moderator_estimate.cih,
                       moderator_z = this_moderator_z,
                       moderator_p = this_moderator_p,
                       Q = model$QE,
                       Qp = model$QEp)
}


v <- c( "NULL", "mean_age_months","productive_vocab_median", "sentence_structure", "agent_argument_type", "patient_argument_type", "n_repetitions_sentence", "n_repetitions_video", "stimuli_modality", "stimuli_actor", "transitive_event_type","intransitive_event_type", "visual_stimuli_pair", "test_method","presentation_type","character_identification", "practice_phase", "test_mass_or_distributed", "n_train_test_pair", "n_test_trial_per_pair" )


generate_moderator_df <- function(moderator,data){
  v = moderator 
  ma_data = data
  
  all_models <- map_df(v,ma_data, .f = get_MA_params)
  
  
  mod_print <- all_models %>%
    mutate(moderator = this_moderator, 
           esimate_print =  round(estimate, 2),
           CI_print = paste0(" [", 
                             round(estimate.cil, 2),
                             ", ",
                             round(estimate.cih, 2),
                             "]"),
           estimate_print_full = paste(esimate_print, CI_print),
           z_print = round(z, 2),
           p_print = round(p, 2),
           p_print = ifelse(p_print <.001, "<.001", paste0("= ", p_print)),
           mod_estimate_round_two = round(mod_estimate,2),
           mod_estimate_round_three = round(mod_estimate,3),
           
           mod_estimate_print = case_when(
             mod_estimate_round_two == 0 ~ mod_estimate_round_three, 
             TRUE ~ mod_estimate_round_two),
           
           mod_SE_print = case_when(
             round(mod_SE, 2) == 0 ~ "<.001",
             TRUE ~ as.character(round(mod_SE,2))), 
           
           mod_estimate.cil_round_two = round(mod_estimate.cil, 2),
           mod_estimate.cil_round_three = round(mod_estimate.cil, 3),
           mod_estimate.cih_round_two = round(mod_estimate.cih, 2),
           mod_estimate.cih_round_three = round(mod_estimate.cih, 3),
           
           mod_estimate.cil_print = case_when(
             mod_estimate.cil_round_two == 0 ~ ">-.001", 
             TRUE ~ as.character(mod_estimate.cil_round_two)
           ), 
           mod_estimate.cih_print = case_when(
             mod_estimate.cih_round_two == 0 ~ "<.001", 
             TRUE ~ as.character(mod_estimate.cih_round_two)
           ),
           
           mod_CI_print = paste0(" [", 
                                 mod_estimate.cil_print,
                                 ", ",
                                 mod_estimate.cih_print,
                                 "]"),
           mod_estimate_print_full = case_when(
             mod_estimate_print == 0 ~  "<.001",
             TRUE ~ paste(mod_estimate_print, mod_CI_print),
           ),
           mod_z_print =  round(moderator_z, 2),
           mod_p_print =  round(moderator_p, 2),
           mod_p_print = ifelse(mod_p_print < .001, "<.001", 
                                paste0("= ", mod_p_print)),
           Q_print = round(Q, 2),
           Qp_print = round(Qp, 2),
           Qp_print = ifelse(Qp_print < .001, "<.001", paste0("= ", Qp_print))) %>% 
    select(
      -c(mod_estimate_round_two, mod_estimate_round_three, 
         mod_estimate.cil_round_two, mod_estimate.cil_round_three, 
         mod_estimate.cih_round_two, mod_estimate.cih_round_three)
    )
  
  return(mod_print)
  
  
}

generate_mega_model_df <- function(model){
  
  
  
  mod_estimate <- as.data.frame(model$b) %>% rownames_to_column(var = "moderator_name")
  mod_SE <- as.data.frame(model$se) 
  mod_estimate_cil <- as.data.frame(model$ci.lb)
  mod_estimate_cih <- as.data.frame(model$ci.ub)
  mod_estimate_z <- as.data.frame(model$zval) 
  mod_estimate_p <- as.data.frame(model$pval)
  
  mega_mod <- bind_cols(mod_estimate, mod_SE, mod_estimate_cil, mod_estimate_cih, mod_estimate_z, mod_estimate_p)
  
  mega_mod <- mega_mod %>% 
    janitor::clean_names() %>% 
    mutate(
      mod_estimate = v1, 
      mod_SE = model_se, 
      moderator_z=model_zval, 
      moderator_p = model_pval, 
      Q = model$QE,
      Qp = model$QEp, 
      mod_estimate_print = round(mod_estimate, 2),
      mod_SE_print = case_when(
        round(mod_SE, 2) == 0 ~ "<.001",
        TRUE ~ as.character(round(mod_SE,2))), 
      mod_ci_lb_print = round(model_ci_lb, 2), 
      mod_ci_ub_print = round(model_ci_ub, 2),
      mod_CI_print = paste0(" [", 
                            round(model_ci_lb, 2),
                            ", ",
                            round(model_ci_ub, 2),
                            "]"),
      mod_estimate_print_full = paste(mod_estimate_print, mod_CI_print),
      
      mod_z_print =  round(moderator_z, 2),
      mod_p_print =  round(moderator_p, 2),
      mod_p_print = ifelse(mod_p_print < .001, "<.001", 
                           paste0("= ", mod_p_print)),
      Q_print = round(Q, 2),
      Qp_print = round(Qp, 2),
      Qp_print = ifelse(Qp_print < .001, "<.001", paste0("= ", Qp_print))
    ) %>% 
    mutate(
      mod_estimate_print = case_when(
        mod_estimate_print == 0 ~ round(mod_estimate, 3), 
        TRUE ~ mod_estimate_print
      ),
      mod_ci_lb_print = case_when(
        mod_ci_lb_print == 0 ~ round(model_ci_lb, 3), 
        TRUE ~ mod_ci_lb_print
      ), 
      mod_ci_ub_print = case_when(
        mod_ci_ub_print == 0 ~ round(model_ci_ub, 3), 
        TRUE ~ mod_ci_ub_print
      ), 
      mod_CI_print = paste0(" [", 
                            mod_ci_lb_print,
                            ", ",
                            mod_ci_ub_print,
                            "]"), 
      mod_estimate_print_full = paste(mod_estimate_print, mod_CI_print)
      
    ) %>% 
    select(-v1)
  
  
  
  
  return(mega_mod)
}