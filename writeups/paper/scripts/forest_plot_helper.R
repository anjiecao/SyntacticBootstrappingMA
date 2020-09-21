library(tidyverse)

generate_forest_plot <- function(data){
  individual_data <- ma_data %>% 
    select(short_cite, unique_id,d_calc,d_var_calc, n_1, plot_label,sentence_structure) %>% 
    mutate(cil = d_calc - qnorm(alpha / 2, lower.tail = FALSE) * sqrt(d_var_calc),
           cil = case_when(
             (cil < -8) ~ -8,  # truncate error bar for visibility reason 
             TRUE ~ cil
           ),
           ciu = d_calc +
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(d_var_calc), 
           meta = "no", 
           label_color = "black",
           print_full = paste(round(d_calc,2), " [",round(cil,2),", ",round(ciu,2), "]", sep = "")
    )
  
  cumulative <- mod_print %>% 
    filter(this_moderator == "NULL") %>% 
    select (estimate, estimate.cil, estimate.cih) %>% 
    mutate(short_cite = "Meta-Analytic Effect Size",
           plot_label = "Meta-Analytic Effect Size",
           d_calc = estimate, 
           d_var_calc = NA, 
           n_1 = 99, 
           expt_num = "", 
           expt_condition = "",
           cil = estimate.cil, 
           ciu = estimate.cih, 
           sentence_structure = "cumulative",
           print_full = paste(round(d_calc,2), " [",round(cil,2),", ",round(ciu,2), "]", sep = ""),
           meta = "yes", 
           label_color = "red")
  
  forest_data <- bind_rows(individual_data, cumulative) 
  forest_data$sentence_structure <- as.factor(forest_data$sentence_structure)
  forest_data$meta <- as.factor(forest_data$meta)
  forest_data <- forest_data %>% 
    rowid_to_column() %>% 
    mutate(
      rowid = if_else(rowid == 0, 99, as.double(rowid)) #to always put the MA ES at bottom
    ) %>% 
    group_by(sentence_structure) %>% arrange(-rowid, .by_group = TRUE)
  forest_data$plot_label <- factor(forest_data$plot_label, levels = forest_data$plot_label)
  
  
  
  # set the neighbourhood levels in the order the occur in the data frame
  label_colors <- forest_data$label_color[order(forest_data$plot_label)]
  
  forest_data %>%  # First sort by val. This sort the dataframe but NOT the factor levels
    ggplot(aes(x = plot_label, y = d_calc)) + 
    geom_point(data = forest_data,
               aes(size=n_1, shape = sentence_structure, color = sentence_structure)) + 
    scale_color_manual(breaks = c("cumulative", "intransitive","transitive"),
                       values = c("red", "black", "black"))+ 
    scale_size(guide = 'none') + 
    scale_shape_manual(breaks = c("cumulative", "intransitive","transitive"),
                       values=c(18,16, 17)) +
    #guides(color = guide_legend(override.aes = list(shape = 18, shape = 16, shape = 17))) + 
    geom_linerange(aes(ymin = cil, ymax = ciu, color = sentence_structure), show.legend = FALSE) + 
    geom_segment(aes(x = plot_label, y = d_calc, xend = plot_label, yend = cil),
                 linejoin = "round", 
                 lineend = "round", 
                 size = 0.1,
                 arrow = arrow(length = unit(0.1, "inches")),
                 data = filter(forest_data,cil == -8))+
    geom_hline(aes(yintercept = 0),  color = "gray44",linetype = 2) + 
    geom_hline(aes(yintercept = filter(forest_data, sentence_structure == "cumulative")$d_calc), 
               color = "red", linetype = 2) + 
    geom_text(aes(label = print_full, x = plot_label, y = 8), 
              size = 5, colour = label_colors) + 
    scale_y_continuous(breaks = seq(-10, 5, 1))+ 
    coord_cartesian(clip = 'on') + 
    coord_flip() + 
    ylab("Cohen's d") +
    labs(color  = "Effect Size Type",shape = "Effect Size Type") + # merge two legends 
    theme(text = element_text(size=22),
          legend.position="bottom",   
          plot.margin = unit(c(1,2,16,1), "lines"),
          legend.title = element_blank(),
          panel.background = element_blank(),
          #panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.title.y = element_blank(),
          axis.text.y = element_text(colour = label_colors)) 
  

}