library(tidyverse)
library(insight)
library(janitor)
# This script will run single-moderator models and generate a dataframe to be used in results section 
get_MA_params <- function(moderator, df) {
  
  this_data <- df
  n = nrow(this_data)
  
  if (moderator == "mean_age"){
    model <- rma.mv(d_calc ~ log(mean_age), V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  } else if (moderator == "NULL"){
    model <- rma.mv(d_calc, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- NA
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- NA
    this_moderator_estimate.cih <- NA
    this_moderator_z <- NA
    this_moderator_p <- NA
    
  }else if (moderator == "sentence_structure"){
    model <- rma.mv(d_calc~sentence_structure, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
  }else if (moderator == "productive_vocab_median"){
    model <- rma.mv(d_calc~productive_vocab_median, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
  }else if (moderator == "agent_argument_type"){
    model <- rma.mv(d_calc~agent_argument_type, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "patient_argument_type"){
    model <- rma.mv(d_calc~patient_argument_type, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "agent_argument_number"){
    model <- rma.mv(d_calc~agent_argument_number, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "n_repetitions_sentence"){
    model <- rma.mv(d_calc~n_repetitions_sentence, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
  }else if (moderator == "stimuli_modality"){
    model <- rma.mv(d_calc~stimuli_modality, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
    
  }else if (moderator == "n_repetitions_video"){
    model <- rma.mv(d_calc~n_repetitions_video, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
    
  }else if (moderator == "stimuli_actor"){
    model <- rma.mv(d_calc~stimuli_actor, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
  }else if (moderator == "transitive_event_type"){
    model <- rma.mv(d_calc~transitive_event_type, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "intransitive_event_type"){
    model <- rma.mv(d_calc~intransitive_event_type, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "visual_stimuli_pair"){
    model <- rma.mv(d_calc~visual_stimuli_pair, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "test_method"){
    model <- rma.mv(d_calc~test_method, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "presentation_type_collapsed"){
    model <- rma.mv(d_calc~presentation_type_collapsed, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
   
  }else if (moderator == "character_identification"){
    model <- rma.mv(d_calc~character_identification, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "practice_phase"){
    model <- rma.mv(d_calc~practice_phase, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
    
  }else if (moderator == "test_mass_or_distributed"){
    model <- rma.mv(d_calc~test_mass_or_distributed, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "n_train_test_pair"){
    model <- rma.mv(d_calc~n_train_test_pair, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }else if (moderator == "n_test_trial_per_pair"){
    model <- rma.mv(d_calc~n_test_trial_per_pair, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant/x_1,
                    method = "REML",
                    data = this_data)
    
    this_moderator_estimate <- model$b[2]
    this_moderator_SE <- model$se[2]
    this_moderator_estimate.cil <- model$ci.lb[2]
    this_moderator_estimate.cih <- model$ci.ub[2]
    this_moderator_z <- model$zval[2]
    this_moderator_p <- model$pval[2]
    
  }
  
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


v <- c( "NULL", "mean_age","productive_vocab_median", "sentence_structure", "agent_argument_type", "patient_argument_type","agent_argument_number", "n_repetitions_sentence", "n_repetitions_video", "stimuli_modality", "stimuli_actor", "transitive_event_type","intransitive_event_type", "visual_stimuli_pair", "test_method","presentation_type","character_identification", "practice_phase", "test_mass_or_distributed", "n_train_test_pair", "n_test_trial_per_pair" )


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
           mod_estimate_print = round(mod_estimate, 2),
           mod_SE_print = round(mod_SE, 2), 
           mod_CI_print = paste0(" [", 
                                 round(mod_estimate.cil, 2),
                                 ", ",
                                 round(mod_estimate.cih, 2),
                                 "]"),
           mod_estimate_print_full = paste(mod_estimate_print, mod_CI_print),
           
           mod_z_print =  round(moderator_z, 2),
           mod_p_print =  round(moderator_p, 2),
           mod_p_print = ifelse(mod_p_print < .001, "<.001", 
                                paste0("= ", mod_p_print)),
           Q_print = round(Q, 2),
           Qp_print = round(Qp, 2),
           Qp_print = ifelse(Qp_print < .001, "<.001", paste0("= ", Qp_print)))
  
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
      mod_SE_print = round(mod_SE, 2), 
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
      mod_SE_print = case_when(
        mod_SE_print == 0 ~ round(mod_SE, 3), 
        TRUE ~ mod_SE_print
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