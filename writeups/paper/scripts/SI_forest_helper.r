ALTERNATIVE_ES_PATH <- here("writeups/paper/SI/alt_ES.csv")
alt_raw_data <- read_csv(ALTERNATIVE_ES_PATH)
alt_data <- alt_raw_data %>% 
  filter(alternative_calc == "between") %>% 
  mutate(
    pooled_SD = sqrt(((n_1 - 1) * sd_1 ^ 2 + (n_2 - 1) * sd_2 ^ 2) / (n_1 + n_2 - 2)), 
    d_calc = (x_1 - x_2) / pooled_SD, 
    d_var_calc = ((n_1 + n_2) / (n_1 * n_2)) + (d_calc ^ 2 / (2 * (n_1 + n_2)))
  )

DATA_PATH <- here("data/processed/syntactic_bootstrapping_tidy_data.csv") 
RAW_DATA_PATH <- here("data/raw/syntactic_bootstrapping_raw_data.csv")

ma_data <- read_csv(DATA_PATH) 
ma_raw_data <- read_csv(RAW_DATA_PATH)



convert_to_forest_data <- function(raw_data){
  # model for cumulative effect size 
  model <- rma.mv(d_calc, 
                  V = d_var_calc,
                  random = ~ 1 | short_cite/same_infant/row_id,
                  method = "REML",
                  data = raw_data)
  
  this_moderator_estimate <- model$b[1]
  this_moderator_SE <- model$se[1]
  this_moderator_estimate.cil <- model$ci.lb[1]
  this_moderator_estimate.cih <- model$ci.ub[1]
  this_moderator_z <- model$zval[1]
  this_moderator_p <- model$pval[1]
  
  # function to abbreviate label 
  abbreviate_label <- function(original_label){
    label_vec <- unlist(strsplit(original_label, ","))
    # get the first label 
    first_author_name <- label_vec[[1]]
    year <- gsub("(-).*","",tail(label_vec, n=1))
    number_label <- gsub(".*-","",tail(label_vec, n=1))
    abbreviated_label <- paste0(first_author_name, " et al.,", year, "-", number_label)
  }
  
  individual_data <- raw_data %>% 
    select(short_cite, unique_id,d_calc,d_var_calc, n_1, plot_label,calc_type, row_id) %>% 
    rowwise() %>% 
    mutate(cil = d_calc - qnorm(alpha / 2, lower.tail = FALSE) * sqrt(d_var_calc),
           cil = case_when(
             (cil < -8) ~ -8, # truncate error bar for visibility reason 
             TRUE ~ cil
           ),
           ciu = d_calc +
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(d_var_calc), 
           ciu = case_when(
             (ciu > 5 ) ~ 5, # truncate error bar for visibility reason 
             TRUE ~ ciu
           ),
           es_type = "individual",
           meta = "no", 
           label_color = "black",
           print_full = paste(round(d_calc,2), " [",round(cil,2),", ",round(ciu,2), "]", sep = ""),
           plot_label = case_when(
             str_count(plot_label,",") >= 4 ~ abbreviate_label(plot_label),
             TRUE ~ plot_label
           )
    )
  
  cumulative_data <- tibble_row(
    short_cite = "Meta-Analytic Effect Size",
    plot_label = "Meta-Analytic Effect Size",
    d_calc = this_moderator_estimate, 
    d_var_calc = NA, 
    n_1 = 99, 
    cil = this_moderator_estimate.cil, 
    ciu = this_moderator_estimate.cih, 
    es_type = "cumulative",
    print_full = paste(round(d_calc,2), " [",round(cil,2),", ",round(ciu,2), "]", sep = ""), 
    meta = "yes", 
    label_color = "red", 
    row_id = 0, 
    calc_type = unique(individual_data$calc_type)
  )
  
  forest_data <- bind_rows(individual_data, cumulative_data)
  
  return (forest_data)
}


generate_double_forest_plot <- function(double_forest_data){

double_forest_data_withorder <- double_forest_data %>% 
  rowid_to_column() %>% 
  mutate(
    rowid = if_else(es_type == "cumulative", 99, as.double(rowid)), #to always put the MA ES at bottom
    point_shape = if_else(es_type == "cumulative", 18, 16), 
    point_color = case_when(
      es_type == "cumulative" && calc_type == "within" ~ "green",
      es_type == "cumulative" && calc_type == "between" ~ "pink",
      calc_type == "between" ~ "pink", 
      calc_type == "within" ~ "green"
    ), 
    label_color = case_when(
      es_type == "cumulative" ~  "green", 
      es_type == "individual" ~ "pink"
    )
  ) %>% 
  group_by(calc_type) %>% arrange(-rowid, .by_group = FALSE)


mm_to_point = 18/6.5
label_size = 5
x_axis_title <- expression(paste("Cohen\'s ", italic('d')))


pd <- position_dodge(.4)


jittered_x<- ggplot_build(double_forest_data_withorder %>%  # First sort by val. This sort the dataframe but NOT the factor levels
                            ggplot(aes(x = fct_reorder(plot_label, -rowid),
                                       y = d_calc,
                                       ymin = cil, 
                                       ymax = ciu, 
                                       group = calc_type, 
                                       color = calc_type
                            )) + 
                            geom_pointrange(size = .5, 
                                            shape = double_forest_data_withorder$point_shape,
                                            aes(color = double_forest_data_withorder$calc_type),
                                            position = pd, 
                                            alpha = 1, 
                            ) )$data %>% 
  as.data.frame() %>% select(x, y, colour)

double_forest_data_withorder %>%  # First sort by val. This sort the dataframe but NOT the factor levels
  ggplot(aes(x = fct_reorder(plot_label, -rowid),
             y = d_calc,
             ymin = cil, 
             ymax = ciu, 
             group = calc_type, 
             color = calc_type
  )) + 
  geom_hline(aes(yintercept = 0),  color = "gray44",linetype = 2, size =.3) + 
  geom_hline(aes(yintercept = filter(double_forest_data_withorder, es_type == "cumulative", calc_type == "within")$d_calc), 
             color = "#00BFC4", linetype = 2, size = .3) + 
  geom_hline(aes(yintercept = filter(double_forest_data_withorder, es_type == "cumulative", calc_type == "between")$d_calc), 
             color = "#F8766D", linetype = 2, size = .3) +
  geom_pointrange(size = .5, 
                  shape = double_forest_data_withorder$point_shape,
                  aes(color = double_forest_data_withorder$calc_type),
                  position = pd, 
                  alpha = 1, 
  ) + 
  
  geom_pointrange(size = .5, 
                  shape = double_forest_data_withorder$point_shape,
                  aes(color = double_forest_data_withorder$calc_type),
                  position = pd, 
                  alpha = 1, 
  ) +
  scale_color_manual(breaks = c("between", "within"),
                     values = c("#F8766D", "#00BFC4"))+ 
  coord_cartesian(clip = 'on') + 
  coord_flip() + 
  ylim(-1.8, 4)+ 
  ylab(x_axis_title) +
  labs(color  = "Effect Size Type",shape = "Effect Size Type") + # merge two legends 
  theme(text = element_text(size=label_size*1.5),
        legend.position="bottom",
        legend.text=element_text(size=label_size*1.5),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.title = element_blank(),
        panel.background = element_blank(),
        #panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=label_size*1.5),
        axis.text.y = element_text(size = label_size*1.5), 
        axis.text.x = element_text(size = label_size * 2)) 
}


