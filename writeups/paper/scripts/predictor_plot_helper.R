

generate_predictor_plot <- function(single_model_df, all_model_df, type){
  
  
  
  if (type == "theoretical"){
    MODERATORS <- c("mean_age_months", "sentence_structure", "agent_argument_type")
    
    single_df <- single_model_df %>% 
      filter(this_moderator %in% MODERATORS) %>% 
      select(this_moderator, mod_estimate, mod_estimate.cih, mod_estimate.cil) %>% 
      mutate(model = "single", 
             type = "theoretical", 
             this_moderator = case_when(
               this_moderator == "mean_age_months" ~ "Mean Age (months)", 
               this_moderator == "sentence_structure" ~ "Predicate Type \n (Intransitive / Transitive)", 
               this_moderator == "agent_argument_type" ~ "Agent Argument Type \n (Noun / Pronoun)", 
             ))
    
    
    mega_df <- all_model_df %>% 
      filter(moderator_name != "intrcpt", moderator_name != "productive_vocab_median") %>% 
      mutate(
        this_moderator = case_when(
          moderator_name == "mean_age_months" ~ "Mean Age (months)", 
          moderator_name == "sentence_structuretransitive" ~ "Predicate Type \n (Intransitive / Transitive)", 
          moderator_name == "agent_argument_typepronoun" ~ "Agent Argument Type \n (Noun / Pronoun)", 
        ),
        mod_estimate.cih = model_ci_ub, 
        mod_estimate.cil = model_ci_lb
      )  %>% 
      select(this_moderator, mod_estimate, mod_estimate.cih, mod_estimate.cil) %>% 
      mutate(model = "full", 
             type = "theoretical")
    
  }else if (type == "methodological"){
    MODERATORS <- c("character_identification", "practice_phase", "test_mass_or_distributed","n_repetitions_sentence", "presentation_type")
    
    single_df <- single_model_df %>% 
      filter(this_moderator %in% MODERATORS) %>% 
      select(this_moderator, mod_estimate, mod_estimate.cih, mod_estimate.cil) %>% 
      mutate(model = "single", 
             type = "methodological",
        this_moderator = case_when(
          this_moderator == "n_repetitions_sentence" ~ "Sentence Repetitions ",
          this_moderator == "character_identification" ~ "Character Identification Phase \n (No / Yes)", 
          this_moderator == "practice_phase" ~ "Practice Phase \n (No / Yes)",
          this_moderator == "test_mass_or_distributed" ~ "Testing Procedure Structure \n (Distributed / Mass)",
          this_moderator == "presentation_type" ~ "Synchronicity \n (Asynchronous / Simultaneous)",
        )
      ) 
    
    mega_df <- all_model_df %>% 
      filter(moderator_name != "intrcpt") %>% 
      mutate(
        this_moderator = case_when(
          moderator_name == "n_repetitions_sentence" ~ "Sentence Repetitions ",
          moderator_name == "character_identificationyes" ~ "Character Identification Phase \n (No / Yes)", 
          moderator_name == "practice_phaseyes" ~ "Practice Phase \n (No / Yes)",
          moderator_name == "test_mass_or_distributedmass" ~ "Testing Procedure Structure \n (Distributed / Mass)",
          moderator_name == "presentation_typesimultaneous" ~ "Synchronicity \n (Asynchronous / Simultaneous)",
        ),
        mod_estimate.cih = model_ci_ub, 
        mod_estimate.cil = model_ci_lb
      )  %>% 
      select(this_moderator, mod_estimate, mod_estimate.cih, mod_estimate.cil) %>% 
      mutate(model = "full", 
             type = "methodological")
    
  }
  
  all_df <- bind_rows(mega_df,single_df)

  
  all_df %>%
    ggplot(aes(x = fct_reorder(this_moderator, -mod_estimate), color = model,
               y = mod_estimate,
               ymin = mod_estimate.cil,
               ymax = mod_estimate.cih,
               group = model)) +
    geom_hline(yintercept = 0, color = "black", linetype="dashed")+
    geom_pointrange(size = 1, position = position_dodge(0.5), alpha = 0) +
    scale_color_manual(labels = c("Full model", "Single-predictor model"), values = c("grey", "blue")) +
    scale_color_manual(labels = c("Full model", "Single-predictor model"), 
                       values = c("grey", "blue"), 
                       guide = guide_legend(reverse = TRUE)) +
    # geom_line(#data = theoretical_all_df %>% filter(model == "full"),
    #   aes(x = fct_reorder(this_moderator, -mod_estimate),
    #       y = mod_estimate,
    #       group = model,
    #       color = model,
    #       alpha = model),
    #   position = position_dodge(0.5),
    #   #alpha = 0.5,
    #   #color = "grey",
    #   size = 0.5) +
    geom_pointrange(size = 1, position = position_dodge(0.5)) +
    scale_alpha_manual(values = c(1, 0)) +
    scale_x_discrete(expand = c(0, .2)) +
    guides(alpha = FALSE) +
    labs(color = "Model type: ") +
    coord_flip() +
    theme(
      axis.line = element_line(size = 1.2),
      axis.ticks = element_line(size = 1),
      #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      #panel.background = element_blank(),
      legend.position = "right")+
    xlab("") +
    ylab("Estimate")
  
  
  
}