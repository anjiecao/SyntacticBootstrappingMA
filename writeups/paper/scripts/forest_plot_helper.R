library(tidyverse)
library(stringr)

test <- "Arunachalam, Escovar, Hansen, & Waxman, (2013) - 1a"

str_count(test,",")

abbreviate_label <- function(original_label){
  label_vec <- unlist(strsplit(original_label, ","))
  # get the first label 
  first_author_name <- label_vec[[1]]
  year <- gsub("(-).*","",tail(label_vec, n=1))
  number_label <- gsub(".*-","",tail(label_vec, n=1))
  abbreviated_label <- paste0(first_author_name, " et al.,", year, "-", number_label)
  #print(number_label)
  #print(abbreviated_label)
}

#abbreviate_label(test)


generate_forest_plot <- function(data){
  ma_data <- data
  
  individual_data <- ma_data %>% 
    select(short_cite, unique_id,d_calc,d_var_calc, n_1, plot_label,sentence_structure) %>% 
    rowwise() %>% 
    mutate(cil = d_calc - qnorm(alpha / 2, lower.tail = FALSE) * sqrt(d_var_calc),
           cil = case_when(
             (cil < -8) ~ -8, # truncate error bar for visibility reason 
             TRUE ~ cil
           ),
           ciu = d_calc +
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(d_var_calc), 
           ciu = case_when(
             (ciu > 3 ) ~ 3, # truncate error bar for visibility reason 
             TRUE ~ ciu
           ),
           meta = "no", 
           label_color = "black",
           print_full = paste(round(d_calc,2), " [",round(cil,2),", ",round(ciu,2), "]", sep = ""),
           plot_label = case_when(
             str_count(plot_label,",") >= 4 ~ abbreviate_label(plot_label),
             TRUE ~ plot_label
           )
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
  
  
  mm_to_point = 18/6.5
  label_size = 5
  x_axis_title <- expression(paste("Cohen\'s ", italic('d')))
  
  # set the neighbourhood levels in the order the occur in the data frame
  label_colors <- forest_data$label_color[order(forest_data$plot_label)]
  
  forest_data %>%  # First sort by val. This sort the dataframe but NOT the factor levels
    ggplot(aes(x = plot_label, y = d_calc)) + 
    geom_hline(aes(yintercept = 0),  color = "gray44",linetype = 2, size =.3) + 
    geom_hline(aes(yintercept = filter(forest_data, sentence_structure == "cumulative")$d_calc), 
               color = "red", linetype = 2, size = .3) + 
    geom_point(data = forest_data,
               aes(size=(n_1/100), shape = sentence_structure, color = sentence_structure)) + 
    scale_color_manual(breaks = c("cumulative", "intransitive","transitive"),
                       values = c("red", "black", "black"))+ 
    scale_size(guide = 'none', range = c(0.3,3)) + 
    scale_shape_manual(breaks = c("cumulative", "intransitive","transitive"),
                       values=c(18,16, 17)) +
    guides(colour = guide_legend(override.aes = list(size=3))) + 
    #guides(color = guide_legend(override.aes = list(shape = 18, shape = 16, shape = 17))) + 
    geom_linerange(aes(ymin = cil, ymax = ciu, color = sentence_structure), show.legend = FALSE) + 
    geom_segment(aes(x = plot_label, y = d_calc, xend = plot_label, yend = ciu),
                 linejoin = "round", 
                 lineend = "round", 
                 size = 0.2,
                 arrow = arrow(length = unit(0.02, "inches")),
                 data = filter(forest_data,ciu == 3))+
    geom_text(aes(label = print_full, x = plot_label, y = 4.2), 
              size = label_size / mm_to_point, colour = label_colors) + 
    scale_y_continuous(breaks = seq(-10, 7, 1))+ 
    coord_cartesian(clip = 'on') + 
    coord_flip() + 
    ylim(-1.8, 5)+ #doesn't seem to help a lot 
    ylab(x_axis_title) +
    labs(color  = "Effect Size Type",shape = "Effect Size Type") + # merge two legends 
    theme(text = element_text(size=label_size),
          legend.position="bottom",
          legend.text=element_text(size=label_size*1.5),
          plot.margin = unit(c(1,1,1,1), "lines"),
          legend.title = element_blank(),
          panel.background = element_blank(),
          #panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=label_size*1.5),
          axis.text.y = element_text(colour = label_colors, 
                                     size = label_size), 
          axis.text.x = element_text(size = label_size * 2)) 
  

}