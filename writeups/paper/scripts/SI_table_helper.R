
pretty_moderator_name <- function(s){
  new_s <- str_to_title(gsub("_", " ",s))
  return(new_s)
}

convert_pretty_print_table <- function(full_df, current_moderator){
  
  full_df <- full_df %>% 
    select(this_moderator, n, 
           estimate_print_full, z_print, p_print, p, 
           mod_estimate_print_full, mod_CI_print, mod_z_print, mod_p_print, moderator_p) %>% 
    mutate(
      intercept = estimate_print_full, 
      moderator = mod_estimate_print_full,
      intercept_z_val = gsub("= ","", z_print), 
      intercept_p_val = case_when(
        p < 0.05 ~ paste0(gsub("= ", "", p_print), "*"), 
        TRUE ~ gsub("= ", "", p_print)
        ),
      mod_z_val = gsub("= ", "", mod_z_print),
      mod_p_val = case_when(
        moderator_p < 0.05 ~ paste0(gsub("= ", "", mod_p_print), "*"), 
        TRUE ~ gsub("= ", "", mod_p_print))
    )
  
  
  estimates <- full_df %>% 
    filter(this_moderator == current_moderator) %>% 
    pivot_longer(cols = c(intercept, moderator), names_to = "Parameter", values_to = "Estimate") %>% 
    select(Parameter, Estimate)
  
  z_val <- full_df %>% 
    filter(this_moderator == current_moderator) %>% 
    pivot_longer(cols = c(intercept_z_val, mod_z_val), names_to = "Parameter", values_to = "z value") %>% 
    select("z value")
  
  p_val <- full_df %>% 
    filter(this_moderator == current_moderator) %>% 
    pivot_longer(cols = c(intercept_p_val, mod_p_val), names_to = "Parameter", values_to = "p value") %>% 
    select("p value")
  
  all_df <- bind_cols(estimates, z_val, p_val) %>% 
    mutate(
      Parameter = case_when(
        Parameter == "moderator" ~ pretty_moderator_name(current_moderator),
        Parameter == "intercept" ~ "Intercept"
      )
    )
  
  return(all_df)
  
}
