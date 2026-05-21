library(dplyr)
library(tidyr)
library(jsonlite)
library(purrr)

# Step 1: Clean the Crop_Income column
crop_dist_initial <- combined_df %>%
  # Replace `np.float64` with just the numeric value and replace single quotes with double quotes
  mutate(Crop_Income = str_replace_all(Crop_Income, "np.float64\\((.*?)\\)", "\\1")) %>%
  mutate(Crop_Income = str_replace_all(Crop_Income, "'", "\"")) %>%  # Replace single quotes with double quotes
  # Convert the cleaned string into a JSON object
  mutate(Crop_Data = map(Crop_Income, ~ fromJSON(.) %>% as.list())) %>%
  # Remove crops that are missing in the Crop_Income for that row
  mutate(Crop_Data = map(Crop_Data, ~ .[!sapply(., is.null)]))  # Keep only non-NULL crops

# Crop 
crop_count <- crop_dist_initial %>%
  unnest_longer(Crop_Data) %>%
  group_by(Scenario, Step, Farmer_type, Crop_Data_id) %>%
  summarise(Total_Count = sum(as.numeric(Crop_Data), na.rm = TRUE), .groups = "drop") %>%
  rename(Crop = Crop_Data_id) %>%
  
  # Compute total sum of all crops per Scenario, Step, and Farmer_type
  group_by(Scenario, Step, Farmer_type) %>%
  mutate(Total_Crops = sum(Total_Count, na.rm = TRUE)) %>%
  
  # Calculate percentage share of each crop
  mutate(Percent_Share = (Total_Count / Total_Crops) * 100) %>%
  ungroup()



# Compute overall crop counts across all scenarios, steps, and farmer types
overall_crop_count <- crop_dist_initial %>%
  unnest_longer(Crop_Data) %>%
  group_by(Crop_Data_id) %>%
  summarise(Total_Count = sum(as.numeric(Crop_Data), na.rm = TRUE), .groups = "drop") %>%
  rename(Crop = Crop_Data_id)



# Print the results
print(crop_count)
print(overall_crop_count)







library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Fix the factor levels for Step and Farmer_type
filtered_data <- crop_count %>%
  filter(Step %in% c(1, 10, 20, 30)) %>%
  mutate(
    Step = factor(Step, levels = c(1, 10, 20, 30), labels = c("Step = 1", "Step = 10", "Step = 20", "Step = 30")),
    Farmer_type = factor(Farmer_type, levels = c("Large", "Medium", "Semi-medium", "Small", "Marginal"))  # Ensure consistent Farmer types
  )

# Define a distinct color palette for crops
crop_palette <- RColorBrewer::brewer.pal(n = length(unique(filtered_data$Crop)), name = "Set3")

# Mapping Scenario names
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

# Loop over each scenario and generate separate plots
plot_list <- list()

for (scen in unique(filtered_data$Scenario)) {
  scenario_data <- filtered_data %>% filter(Scenario == scen)
  
  p <- ggplot(scenario_data, aes(x = Crop, y = Percent_Share, fill = Crop)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge position
    facet_wrap(~ Step + Farmer_type, scales = "fixed") +  # Fixed scaling for all facets
    scale_fill_manual(values = crop_palette) +  # Custom colors for crops
    labs(
      title = paste("Crop Percentage Share for", scenario_labels[scen]),  # Use mapped scenario labels
      x = NULL,  # Remove x-axis label
      y = "Percentage Share (%)",
      fill = "Crop"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),  # Remove x-axis text
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold"),  # Facet labels
      legend.position = "bottom"
    )
  
  # Save the plot
  file_name <- paste0("Crop_Percentage_Share_v6_", scenario_labels[scen], ".pdf")
  ggsave(filename = file_name, plot = p, width = 10, height = 6)
  
  # Store plot in list
  plot_list[[scen]] <- p
}

# Print all scenario plots
for (scen in names(plot_list)) {
  print(plot_list[[scen]])
}






# Step 1: Randomly select an Agent ID
random_agent_id <- sample(unique(combined_df$AgentID), 1)  # Randomly select one agent

# Step 2: Filter the dataset for that specific Agent ID
agent_crop_data <- crop_dist_initial %>%
  filter(AgentID == random_agent_id) %>%
  unnest_longer(Crop_Data) %>%
  group_by(Scenario, Step, Crop_Data_id) %>%
  summarise(Crop_Count = sum(as.numeric(Crop_Data), na.rm = TRUE), .groups = "drop") %>%
  rename(Crop = Crop_Data_id)

# Step 3: Prepare the data for plotting
agent_crop_data <- agent_crop_data %>%
  group_by(Scenario, Step) %>%
  mutate(Total_Crops = sum(Crop_Count, na.rm = TRUE)) %>%
  mutate(Percent_Share = (Crop_Count / Total_Crops) * 100) %>%
  ungroup()

# Step 4: Plot the crops grown by this agent across steps and scenarios
plot_agent_crops <- ggplot(agent_crop_data, aes(x = Step, y = Percent_Share, color = Crop)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Scenario, ncol = 1) +  # Facet by Scenario
  labs(
    title = paste("Crops Grown by Agent ID", random_agent_id),
    x = "Step (Time)",
    y = "Percentage Share of Crops",
    color = "Crop"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Print the plot
print(plot_agent_crops)
