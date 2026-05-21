# Load required packages
library(dplyr)
library(ggplot2)

# Step 1: Adjust values to remove negatives
combined_df1 <- combined_df %>%
  mutate(
    Wealth_Adjusted = Wealth + 40000000,
    Money_Adjusted = Money + 3000000
  )

# Step 2: Define MLD function (Mean Log Deviation)
calculate_mld <- function(x) {
  x <- x[x > 0]  # Remove non-positive values to avoid log issues
  if (length(x) == 0) return(NA)
  log_mean <- log(mean(x))
  mean_log <- mean(log(x))
  return(log_mean - mean_log)
}

# Step 3: Compute MLD values by Scenario, Step, and Farmer_type
mld_df <- combined_df1 %>%
  group_by(Scenario, Step, Farmer_type) %>%
  summarise(
    MLD_Wealth = calculate_mld(Wealth_Adjusted),
    MLD_Money = calculate_mld(Money_Adjusted),
    .groups = "drop"
  )

# Step 4: Set scenario labels
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

# Step 5: Plot MLD for Wealth
mld_wealth_plot <- ggplot(mld_df, aes(x = Step, y = MLD_Wealth, color = Farmer_type)) +
  geom_line(linewidth = 0.8, linetype = "solid") +
  geom_point(size = 2) +
  facet_wrap(~ Scenario, labeller = labeller(Scenario = scenario_labels)) +
  labs(
    title = "Mean Log Deviation (Wealth) by Farmer Type",
    x = "Step (Time)",
    y = "MLD (Wealth)",
    color = "Farmer Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Step 6: Plot MLD for Money
mld_money_plot <- ggplot(mld_df, aes(x = Step, y = MLD_Money, color = Farmer_type)) +
  geom_line(linewidth = 0.8, linetype = "solid") +
  geom_point(size = 2) +
  facet_wrap(~ Scenario, labeller = labeller(Scenario = scenario_labels)) +
  labs(
    title = "Mean Log Deviation (Money) by Farmer Type",
    x = "Step (Time)",
    y = "MLD (Money)",
    color = "Farmer Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Step 7: Print plots
print(mld_wealth_plot)
print(mld_money_plot)

# Optional: Save plots as PDFs
# ggsave("MLD_Wealth_by_FarmerType.pdf", mld_wealth_plot, width = 8, height = 5, device = cairo_pdf)
# ggsave("MLD_Money_by_FarmerType.pdf", mld_money_plot, width = 8, height = 5, device = cairo_pdf)
