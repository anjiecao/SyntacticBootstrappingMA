library(tidyverse)

generate_funnel_plot <- function(data){

  CRIT_95 = 1.96

  ma_data <- data

   model <- rma.mv(d_calc ~ 1,  d_var_calc,
                   random = ~ 1 | short_cite/same_infant/row_id,
                   method = "REML",
                   data=ma_data)

  predicted_val <- predict.rma(model)
  intercept_in_model <-  model$beta[1]


ma_funnel <- ma_data %>%
  mutate(
    se = sqrt(d_var_calc),
    es = d_calc,
    center = intercept_in_model,
    lower_lim = max(se) + .05 * max(se),
    type = "non_moderated"
  ) %>%
  select(es, se, center, lower_lim)


funnel_shape_wide <- ma_funnel %>%
  mutate(x1 = (center-lower_lim * CRIT_95)[1],
         x2 = center[1],
         x3 = center[1] + lower_lim[1] * CRIT_95,
         y1 = -lower_lim[1],
         y2 =  0,
         y3 = -lower_lim[1])

funnel95_data_x = funnel_shape_wide  %>%
  select(dplyr::contains("x")) %>%
  gather("coordx", "x", 1:3) %>%
  arrange(coordx) %>%
  select(-coordx)

funnel95_data_Y = funnel_shape_wide  %>%
  select(dplyr::contains("y")) %>%
  gather("coordy", "y", 1:3) %>%
  arrange(coordy) %>%
  select(-coordy)

funnel95.data = bind_cols(funnel95_data_x, funnel95_data_Y)

ggplot(ma_funnel, aes(x = es, y = -se)) +
  xlab(expression(paste("Effect Size (Cohen's ", italic(d), ")")))  +
  ylab("Standard Error\n")  +
  geom_polygon(aes(x = x, y = y),
               data = funnel95.data,
               fill = "grey90") +
  geom_vline(aes(xintercept=intercept_in_model),
             linetype = "dashed", color = "red", size = .8, data = funnel_shape_wide) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey44",  size = .8) +
  scale_y_continuous(labels = function(x){abs(x)}) +
  geom_point(data = ma_funnel, size = 1.5,   alpha = .7) +
  theme(text = element_text(size = 11),
        panel.grid.major = element_line(colour = "white", size = 0.2),
        panel.grid.minor = element_line(colour = "white", size = 0.5),
        strip.text.x = element_text(size = 9),
        strip.background = element_rect(fill="white"))


}
