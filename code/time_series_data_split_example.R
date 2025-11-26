
library(fpp3)

# Välj en serie: Beer production
ts_data <- aus_production %>%
  select(Quarter, Beer) %>%
  filter(!is.na(Beer)) %>%
  mutate(Del = case_when(
    year(Quarter) < 2000 ~ "Träning",
    year(Quarter) < 2006 ~ "Validering",
    TRUE                 ~ "Test"
  ))

# Plot med ggplot2
ts_data %>%
  ggplot(aes(x = Quarter, y = Beer, color = Del)) +
  geom_line(linewidth = 0.75) +
  labs(title = "Ölproduktion i Australien: träning, validering och test",
       x = "Tid", y = "Miljoner liter", color = "Del") +
  scale_color_manual(values = c("Träning" = "blue", "Validering" = "orange", "Test" = "red")) +
  theme_minimal()



