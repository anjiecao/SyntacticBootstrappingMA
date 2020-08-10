ma_data <- read_csv(DATA_PATH)  %>%
  filter(!is.na(mean_age))

m_age <- rma.mv(d_calc ~ sentence_structure, V = d_var_calc,
                random = ~ 1 | short_cite, data = ma_data)
summary(m_age)

predicted_values <- predict(m_age)  %>%
  as.data.frame()

predicted_values_with_moderators <- bind_cols(list(ma_data, predicted_values))

ggplot(predicted_values_with_moderators) +
  #geom_point(aes(x = mean_age, y = pred)) +
  geom_point(aes(x = mean_age, y = d_calc), color = "red") +
  geom_smooth(method = "lm", aes(x = mean_age, y = pred)) +
  theme_classic()
```
