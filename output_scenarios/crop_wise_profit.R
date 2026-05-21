library(dplyr)
library(ggplot2)

profit <- read.csv("crop_profitability.csv")


# Filter out 'Arecanut' and arrange the data
profit_long <- profit %>%
  filter(Crop != "Arecanut") %>%
  # filter(Crop != "Groundnut") %>%
  # filter(Crop != "Chickpea") %>%
  group_by(District, Crop, Year) %>%
  summarise(Profit = mean(Predicted_Yield, na.rm = TRUE), .groups = 'drop')

# Plotting
ggplot(profit_long, aes(x = Year, y = Profit, color = Crop, group = Crop)) +
  geom_line(size = 0.5) +
  facet_wrap(~ District, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white")
  ) +
  labs(
    title = "Crop-wise Profit Over Years by District",
    x = "Year",
    y = "Profit",
    color = "Crop"
  )
