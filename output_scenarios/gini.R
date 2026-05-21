


# Clean the data: Filter for Step = 1 and Liquid Asset only, remove non-finite values
combined_df_step1 <- combined_df %>%
  filter(Step == 1) %>%
  filter(!is.na(Liquid_asset) & is.finite(Liquid_asset)) %>%
  gather(key = "Asset_Type", value = "Value", Liquid_asset)  # Reshape the data

# Mapping Scenario Names
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

# Convert Scenario to Factor with Proper Labels in combined_df_step1
combined_df$Scenario <- factor(combined_df$Scenario, levels = c("01", "02", "03", "04", "05"), labels = scenario_labels)

# Define the plot for the spread of Liquid Asset for Step = 1
plot_spread <- ggplot(combined_df_step1, aes(x = Value)) +
  geom_density(aes(y = ..scaled.., fill = Scenario), adjust = 1, alpha = 0.6, color = "black") +
  facet_wrap(~ Scenario, ncol = 1) +  # 5 facets, one for each scenario
  scale_fill_manual(values = c("green", "blue", "red", "purple", "orange")) +
  labs(
    title = "Spread of Liquid Assets at Step = 1",
    x = "Liquid Asset Value",
    y = "Probability Density",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(min(combined_df_step1$Value), max(combined_df_step1$Value)))  # Fixed x-axis range from negative to positive

# Print the plot
print(plot_spread)


# Save the plot as a PDF
ggsave("Spread_Liquid_Asset_Step_1_v2.pdf", plot_spread, width = 8, height = 12, device = cairo_pdf)



library(ggplot2)
library(dplyr)
library(tidyr)

# Define the steps to plot
steps <- c(1, 10, 20, 30)


# Loop through each step and generate plots
for (step in steps) {
  combined_df_step <- combined_df %>%
    filter(Step == step) %>%
    filter(!is.na(Liquid_asset) & is.finite(Liquid_asset)) %>%
    gather(key = "Asset_Type", value = "Value", Liquid_asset)
  
  plot_spread <- ggplot(combined_df_step, aes(x = Value)) +
    geom_density(aes(y = ..scaled.., fill = Scenario), adjust = 1, alpha = 0.6, color = "black") +
    facet_wrap(~ Scenario, ncol = 3) +
    scale_fill_manual(values = c("green", "blue", "red", "purple", "orange")) +
    labs(
      title = paste("Spread of Liquid Assets at Step =", step),
      x = "Liquid Asset Value",
      y = "Scaled Density",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 6),  # 👈 Very small x-axis text
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 13)
    )
  
  print(plot_spread)
  ggsave(paste0("Spread_Liquid_Asset_Step_", step, "_faceted_v1.pdf"), plot_spread, width = 10, height = 8, device = cairo_pdf)
}





library(ggplot2)
library(dplyr)
library(tidyr)

# Define the steps to plot
steps <- c(1, 10, 20, 30)

# Loop through each step and generate plots for Wealth
for (step in steps) {
  combined_df_step <- combined_df %>%
    filter(Step == step) %>%
    filter(!is.na(Wealth) & is.finite(Wealth)) %>%
    gather(key = "Asset_Type", value = "Value", Wealth)
  
  plot_spread <- ggplot(combined_df_step, aes(x = Value)) +
    geom_density(aes(y = ..scaled.., fill = Scenario), adjust = 1, alpha = 0.6, color = "black") +
    facet_wrap(~ Scenario, ncol = 3) +
    scale_fill_manual(values = c("green", "blue", "red", "purple", "orange")) +
    labs(
      title = paste("Spread of Wealth at Step =", step),
      x = "Wealth Value",
      y = "Scaled Density",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 13)
    )
  
  print(plot_spread)
  ggsave(paste0("Spread_Wealth_Step_", step, "_faceted.pdf"), plot_spread, width = 10, height = 8, device = cairo_pdf)
}
