library(dplyr)
library(ggplot2)

# Assume df has columns: farmer_id, year, wealth, income, expenditure

# Set the poverty threshold (example: ₹10,000)
poverty_threshold <- 66000

# Classify whether a farmer is poor at each time point
df <- combined_df1 %>%
  mutate(is_poor = Liquid_asset < poverty_threshold)

# Summarize how many years each farmer is poor
poverty_summary <- df %>%
  group_by(agent_id) %>%
  summarise(years_poor = sum(is_poor),
            total_years = n(),
            trapped = years_poor > 20)  # adjust threshold as needed

head(poverty_summary)

library(broom)

# Calculate slope of wealth over time for each farmer
wealth_trend <- df %>%
  group_by(agent_id) %>%
  do({
    model <- lm(Liquid_asset ~ Step, data = .)
    tibble(slope = coef(model)[["Step"]],
           start_wealth = first(.$Liquid_asset))
  }) %>%
  mutate(trapped = start_wealth < poverty_threshold & slope <= 0)


# Join both summaries
poverty_class <- poverty_summary %>%
  left_join(wealth_trend, by = "agent_id") %>%
  mutate(
    poverty_type = case_when(
      years_poor == 0 ~ "Never Poor",
      years_poor > 20 & slope <= 0 ~ "Structural Trap",
      years_poor <= 20 & slope <= 0 ~ "Growth Failure Trap",
      start_wealth < poverty_threshold & slope > 0 ~ "Escaped Poverty",
      TRUE ~ "Uncategorized"  # fallback
    )
  )

# Merge to main df for plotting
df <- df %>%
  left_join(poverty_class %>% select(agent_id, poverty_type), by = "agent_id")



ggplot(df %>% filter(agent_id %in% sample(unique(agent_id), 16)), 
       aes(x = Step, y = Liquid_asset, group = agent_id)) +
  geom_line(aes(color = poverty_type)) +
  facet_wrap(~ poverty_type, scales = "free_y") +
  labs(title = "Wealth Trajectories by Poverty Trap Type",
       y = "Wealth (₹)", x = "Year") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Structural Trap" = "red",
    "Growth Failure Trap" = "yellow",
    "Escaped Poverty" = "blue",
    "Never Poor" = "darkgreen"
  ))

df <- df %>% filter(poverty_type != "Uncategorized")
df <- df %>%
  mutate(Liquid_asset_million = Liquid_asset / 1e6)

df$poverty_type <- factor(df$poverty_type, levels = c(
  "Never Poor",
  "Escaped Poverty",
  "Growth Failure Trap",
  "Structural Trap"
))


pdf("liquid_asset_trajectories_by_scenario_poverty_type_v6.pdf", width = 14, height = 8)

ggplot(df, 
       aes(x = Step, y = Liquid_asset_million, group = agent_id)) +
  geom_line(aes(color = poverty_type), alpha = 0.6) +
  facet_grid(poverty_type ~ ScenarioName, scales = "free_y") +
  labs(title = "Liquid Asset Trajectories by Scenario and Poverty Trap Type",
       y = "Liquid Asset (Million Rs)", x = "Year") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Structural Trap" = "red",
    "Growth Failure Trap" = "yellow",
    "Escaped Poverty" = "blue",
    "Never Poor" = "darkgreen"
  )) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

dev.off()



df$Farmer_type <- factor(df$Farmer_type, levels = c(
  "Large", "Medium", "Semi-medium", "Small", "Marginal"
))



pdf("liquid_asset_trajectories_by_farmer_and_scenario_v6.pdf", width = 14, height = 10)

ggplot(df, 
       aes(x = Step, y = Liquid_asset_million, group = agent_id)) +
  geom_line(aes(color = poverty_type), alpha = 0.9) +
  facet_grid(Farmer_type ~ ScenarioName, scales = "free_y") +
  labs(title = "Liquid Asset Trajectories by Scenario and Farmer Type",
       y = "Liquid Asset (Million Rs)", x = "Year") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Structural Trap" = "red",
    "Growth Failure Trap" = "yellow",
    "Escaped Poverty" = "blue",
    "Never Poor" = "darkgreen"
  )) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

dev.off()


table(df$Farmer_type, df$poverty_type)
table(df$ScenarioName, df$poverty_type)

cat("Poverty types in data:\n")
print(unique(df$poverty_type))

cat("\nFarmer types in data (ordered):\n")
print(unique(df$Farmer_type))



# Suppose this is your summary table:
my_table <- df %>%
  count(ScenarioName, Farmer_type, poverty_type) %>%
  pivot_wider(names_from = Farmer_type, values_from = n, values_fill = 0)

print(my_table)
# Save it as CSV
write.csv(my_table, "poverty_type_matrix_v1.csv", row.names = FALSE)

