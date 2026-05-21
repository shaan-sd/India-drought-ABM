# loading required libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)


# set working directory
setwd("C:/Users/dassu/ABM_Paper_Graphs/yield_clim_shock_double/")


# List all CSV files that match the pattern
files <- list.files(pattern = "agent_wealth_debt_over_time_S\\d{2}_\\d{2}\\.csv")
# Function to read each file and extract scenario and file ID
read_data <- function(file) {
  df <- read_csv(file)
  # Extract scenario (e.g., "S01") and file ID (e.g., "01") from filename
  scenario <- str_extract(file, "S\\d{2}") %>% str_remove("S") # Extract scenario number
  file_id <- str_extract(file, "_\\d{2}") %>% str_remove("_")  # Extract file ID
  # Add columns for scenario and file ID
  df <- df %>%
    mutate(Scenario = scenario, File_ID = file_id)
  return(df)
}


# Read and combine all files into one dataframe
combined_df <- bind_rows(lapply(files, read_data))



land_stats <- combined_df %>%
  filter(Step == 1) %>%
  group_by(Scenario, Farmer_type) %>%
  summarise(
    max_farm_size = max(farm_size, na.rm = TRUE),
    min_farm_size = min(farm_size, na.rm = TRUE),
    avg_farm_size = mean(farm_size, na.rm = TRUE),
    avg_agents = n() / 5  # Direct count of farmers in each category
  )


# Define a custom color palette for farmer types (optional)
# Define custom colors
color_palette <- c(
  "Overall" = "black",
  "Large" = "goldenrod2",  # Golden color
  "Medium" = "blue",
  "Semi-medium" = "purple",
  "Small" = "green",
  "Marginal" = "red"  # Red color
)

# Ensure Farmer_type is a factor in land_stats with correct levels
land_stats$Farmer_type <- factor(land_stats$Farmer_type, 
                                 levels = c("Overall", "Large", "Medium", "Semi-medium", "Small", "Marginal"))

# Generate and save separate plots for each scenario
for (scenario in unique(land_stats$Scenario)) {
  
  # Filter data for current scenario
  scenario_data <- land_stats %>% filter(Scenario == scenario)
  
  # Define the file name dynamically for each scenario
  pdf_filename <- paste0("Farmer_size_fixed_v2_", scenario, ".pdf")
  
  # Scaling factor to visually align avg_farm_size with bar height
  scale_factor <- max(scenario_data$avg_agents) / max(scenario_data$avg_farm_size)
  
  # Open a PDF device for this specific scenario
  pdf(pdf_filename, width = 8, height = 5)
  
  # Generate the plot
  p <- ggplot(scenario_data, aes(x = Farmer_type)) +
    
    # Transparent bars for avg_agents
    geom_col(aes(y = avg_agents, fill = Farmer_type), 
             width = 0.6, alpha = 0.4, color = "black") +
    
    # Line plot and points for avg_farm_size (scaled)
    geom_line(aes(y = avg_farm_size * scale_factor, group = 1), 
              color = "blue", size = 1.2) +
    geom_point(aes(y = avg_farm_size * scale_factor), 
               color = "blue", size = 3) +
    
    # Add avg_farm_size labels above points (unscaled)
    geom_text(aes(y = avg_farm_size * scale_factor, 
                  label = round(avg_farm_size, 2)), 
              vjust = -1, size = 3, fontface = "bold", color = "blue") +
    
    # Manual fill colors
    scale_fill_manual(values = color_palette) +
    
    # Primary axis
    scale_y_continuous(
      name = "Mean Count per 1000 Farmers",
      
      # Secondary axis (unscale avg_farm_size)
      sec.axis = sec_axis(~ . / scale_factor, name = "Average Farm Size (hectares)")
    ) +
    
    labs(
      title = paste("Mean Distribution of Farmers by Landholding Category"),
      x = "Farmer Type"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.title.y.right = element_text(size = 10, face = "bold", color = "blue"),
      axis.title.x = element_text(size = 10, face = "bold"),
      legend.position = "none"
    )
  
  # Print the plot to the PDF
  print(p)
  
  # Close the PDF device
  dev.off()
}


combined_df <- combined_df %>%
  mutate(Wealth = `Available Collateral` + Liquid_asset)



# Compute mean and standard deviation for each Scenario, Step, and Farmer_type
mean_data <- combined_df %>%
  mutate(Scenario = as.factor(Scenario)) %>%
  group_by(Scenario, Step, Farmer_type) %>%
  summarize(
    mean_initial_income = mean(Initial_income, na.rm = TRUE),
    sd_initial_income = sd(Initial_income, na.rm = TRUE),
    median_initial_income = median(Initial_income, na.rm = TRUE),
    
    mean_income = mean(Income, na.rm = TRUE), 
    sd_income = sd(Income, na.rm = TRUE),
    median_income = median(Income, na.rm = TRUE),
    
    mean_money = mean(Money, na.rm = TRUE),
    sd_money = sd(Money, na.rm = TRUE),
    median_money = median(Money, na.rm = TRUE),
    
    mean_money_real = mean(Liquid_asset, na.rm = TRUE),
    sd_money_real = sd(Liquid_asset, na.rm = TRUE),
    median_money_real = median(Liquid_asset, na.rm = TRUE),
    
    mean_networth = mean(Networth, na.rm = TRUE),
    sd_networth = sd(Networth, na.rm = TRUE),
    median_networth = median(Networth, na.rm = TRUE),
    
    mean_wealth = mean(Wealth, na.rm = TRUE),
    sd_wealth = sd(Wealth, na.rm = TRUE),
    median_wealth = median(Wealth, na.rm = TRUE),
    
    mean_expenditure = mean(Expenditure, na.rm = TRUE),
    sd_expenditure = sd(Expenditure, na.rm = TRUE),
    median_expenditure = median(Expenditure, na.rm = TRUE),
    
    mean_cultivation = mean(`Total Cultivation Cost`, na.rm = TRUE),
    sd_cultivation = sd(`Total Cultivation Cost`, na.rm = TRUE),
    median_cultivation = median(`Total Cultivation Cost`, na.rm = TRUE),
    
    mean_total_debt = mean(`Total Debt`, na.rm = TRUE),
    sd_total_debt = sd(`Total Debt`, na.rm = TRUE),
    median_total_debt = median(`Total Debt`, na.rm = TRUE),
    
    .groups = "drop"
  )

mean_data_overall <- combined_df %>%
  group_by(Scenario, Step) %>%
  summarize(
    mean_initial_income = mean(Initial_income, na.rm = TRUE),
    sd_initial_income = sd(Initial_income, na.rm = TRUE),
    median_initial_income = median(Initial_income, na.rm = TRUE),
    
    mean_income = mean(Income, na.rm = TRUE), 
    sd_income = sd(Income, na.rm = TRUE),
    median_income = median(Income, na.rm = TRUE),
    
    mean_money = mean(Money, na.rm = TRUE),
    sd_money = sd(Money, na.rm = TRUE),
    median_money = median(Money, na.rm = TRUE),
    
    mean_money_real = mean(Liquid_asset, na.rm = TRUE),
    sd_money_real = sd(Liquid_asset, na.rm = TRUE),
    median_money_real = median(Liquid_asset, na.rm = TRUE),
    
    mean_networth = mean(Networth, na.rm = TRUE),
    sd_networth = sd(Networth, na.rm = TRUE),
    median_networth = median(Networth, na.rm = TRUE),
    
    mean_wealth = mean(Wealth, na.rm = TRUE),
    sd_wealth = sd(Wealth, na.rm = TRUE),
    median_wealth = median(Wealth, na.rm = TRUE),
    
    mean_expenditure = mean(Expenditure, na.rm = TRUE),
    sd_expenditure = sd(Expenditure, na.rm = TRUE),
    median_expenditure = median(Expenditure, na.rm = TRUE),
    
    mean_cultivation = mean(`Total Cultivation Cost`, na.rm = TRUE),
    sd_cultivation = sd(`Total Cultivation Cost`, na.rm = TRUE),
    median_cultivation = median(`Total Cultivation Cost`, na.rm = TRUE),
    
    mean_total_debt = mean(`Total Debt`, na.rm = TRUE),
    sd_total_debt = sd(`Total Debt`, na.rm = TRUE),
    median_total_debt = median(`Total Debt`, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(Farmer_type = "Overall")

# Combine farmer-type and overall data
mean_data <- bind_rows(mean_data, mean_data_overall)

# mean_data <- mean_data %>% filter(Farmer_type != "Large")

# Ensure "Overall" appears first in factor levels
mean_data$Farmer_type <- factor(mean_data$Farmer_type, 
                                levels = c("Overall", "Large", "Medium", "Semi-medium", "Small", "Marginal"))

# combined_df$Farmer_type <- factor(combined_df$Farmer_type, 
#                                 levels = c("Large", "Medium", "Semi-medium", "Small", "Marginal"))


# Define custom colors
color_palette <- c(
  "Overall" = "black",
  "Large" = "goldenrod2",  # Golden color
  "Medium" = "blue",
  "Semi-medium" = "purple",
  "Small" = "green",
  "Marginal" = "red"  # Red color
)


# Mapping Scenario names
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)


### INCOME (Normal Scale)
p_inc <- ggplot(mean_data, aes(x = Step, y = mean_income / 1e6, color = Farmer_type, group = Farmer_type)) +
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = (mean_income - sd_income) / 1e6, 
                  ymax = (mean_income + sd_income) / 1e6, 
                  fill = Farmer_type), 
              alpha = 0.2, color = NA) +  # Adjust alpha for transparency
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  # Mean income line
  geom_line(linewidth = 0.8) +  
  
  # Custom colors for lines
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +  # Match fill colors to line colors
  
  # Y-axis with continuous scale and 0.5 increments
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.5),  # Breaks at 0.5 intervals
    expand = c(0, 0)
  ) +  
  
  # Labels
  labs(
    title = "Mean Income Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Income (₹ in Millions)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario with custom labels
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(size = 10),   
    axis.text.y = element_text(size = 10),   
    axis.title = element_text(size = 10, face = "bold"),                    
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10),  
    legend.position = "bottom",  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p_inc)

library(Cairo)
ggsave("Mean_Income_Plot_sv2.pdf", plot = p_inc, width = 8, height = 5, device = cairo_pdf)


### EXPENDITURE (Thousands Scale)
p_exp <- ggplot(mean_data, aes(x = Step, y = mean_expenditure / 1000, color = Farmer_type, group = Farmer_type)) +
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = (mean_expenditure - sd_expenditure) / 1000, 
                  ymax = (mean_expenditure + sd_expenditure) / 1000, 
                  fill = Farmer_type), 
              alpha = 0.2, color = NA) +  # Adjust alpha for transparency
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  # Mean expenditure line
  geom_line(linewidth = 0.8) +  
  
  # Custom colors for lines
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +  # Match fill colors to line colors
  
  # Y-axis in thousands
  scale_y_continuous(
    breaks = seq(50000, 200000, by = 20000) / 1000,  # Convert to thousands
    labels = scales::label_number(scale = 1, suffix = "k"),  # Display as '50k', '75k', etc.
    expand = c(0, 0)
  ) +  
  
  # Labels
  labs(
    title = "Mean Expenditure Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Expenditure (₹)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario with custom labels
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(size = 10),   
    axis.text.y = element_text(size = 10),   
    axis.title = element_text(size = 10, face = "bold"),                    
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10),  
    legend.position = "bottom",  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p_exp)

ggsave("Mean_Expenditure_Plot_v1.pdf", plot = p_exp, width = 8, height = 5, device = cairo_pdf)


### CULTIVATION (Thousands Scale)
p_cultivation <- ggplot(mean_data, aes(x = Step, y = mean_cultivation / 1000, color = Farmer_type, group = Farmer_type)) +
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = (mean_cultivation - sd_cultivation) / 1000, 
                  ymax = (mean_cultivation + sd_cultivation) / 1000, 
                  fill = Farmer_type), 
              alpha = 0.2, color = NA) +  # Adjust alpha for transparency
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  
  # Mean cultivation line
  geom_line(linewidth = 0.8) +  
  
  # Custom colors for lines
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +  # Match fill colors to line colors
  
  # Y-axis in thousands
  scale_y_continuous(
    breaks = seq(25000, 1250000, by = 100000) / 1000,  # Convert to thousands
    labels = scales::label_number(scale = 1, suffix = "k"),  # Display as '20k', '100k', etc.
    expand = c(0, 0)
  ) +  
  
  # Labels
  labs(
    title = "Mean Cultivation Cost Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Cultivation Cost (₹)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario with custom labels
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  
    axis.text.x = element_text(size = 10),   
    axis.text.y = element_text(size = 10),   
    axis.title = element_text(size = 10, face = "bold"),                    
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10),  
    legend.position = "bottom",  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p_cultivation)

ggsave("Mean_Cultivation_Plot_v6.pdf", plot = p_cultivation, width = 8, height = 5, device = cairo_pdf)


### MEAN MONEY (Million Scale with Better Spacing)

# Calculate quantile range to zoom in on the main data range
q_low <- quantile(mean_data$mean_money_real, 0.03) / 1e6
q_high <- quantile(mean_data$mean_money_real, 0.999) / 1e6

# Generate plot
p_money_real <- ggplot(mean_data, aes(x = Step, y = mean_money_real / 1e6, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(
    ymin = (mean_money_real - sd_money_real) / 1e6,
    ymax = (mean_money_real + sd_money_real) / 1e6,
    fill = Farmer_type
  ),
  alpha = 0.1, color = NA) +
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  
  # Dashed line for mean net worth
  geom_line(linewidth = 0.8, linetype = "dashed") +
  
  # Horizontal reference line at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "solid") +
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Tighter Y-axis based on quantiles
  scale_y_continuous(
    limits = c(q_low, q_high),
    labels = label_number(scale = 1, suffix = "M"),
    expand = c(0.01, 0.01)
  ) +
  
  # Labels and titles
  labs(
    title = "Mean Liquid Networth Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Liquid Networth (₹ in Millions)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by scenario
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Print the plot
print(p_money_real)

ggsave("Mean_Money_Plot_sv2.pdf", plot = p_money_real, width = 8, height = 5, device = cairo_pdf)



money <- mean_data %>%
  filter(Step == 30) %>%
  group_by(Scenario, Farmer_type) %>%
  mutate(mean_money_real = mean_money_real / 1e6, median_money_real = median_money_real / 1e6) %>%
  select(c(Scenario, Farmer_type, mean_money_real, median_money_real))

library(knitr)  # provides the kable() function

money_summary <- mean_data %>%
  filter(Step == 30) %>%
  mutate(
    mean_money_real = round(mean_money_real / 1e6, 2),
    median_money_real = round(median_money_real / 1e6, 2)
  ) %>%
  group_by(Scenario, Farmer_type) %>%
  summarise(
    Mean = mean(mean_money_real, na.rm = TRUE),
    Median = mean(median_money_real, na.rm = TRUE),
    .groups = "drop"
  )


# Step 3: Pivot for LaTeX-friendly format (Scenario-wise columns)
money_table_mean <- money_summary %>%
  select(Scenario, Farmer_type, Mean) %>%
  pivot_wider(names_from = Scenario, values_from = Mean)

money_table_median <- money_summary %>%
  select(Scenario, Farmer_type, Median) %>%
  pivot_wider(names_from = Scenario, values_from = Median)

# Step 4: Print as LaTeX
cat("\\textbf{Mean Liquid Asset (₹ Millions)}\n")
kable(money_table_mean, format = "latex", booktabs = TRUE, digits = 2)

cat("\n\\textbf{Median Liquid Asset (₹ Millions)}\n")
kable(money_table_median, format = "latex", booktabs = TRUE, digits = 2)


## WEALTH
q_low <- quantile(mean_data$mean_wealth, 0.01) / 1e6
q_high <- quantile(mean_data$mean_wealth, 1) / 1e6

p_wealth <- ggplot(mean_data, aes(x = Step, y = mean_wealth / 1e6, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = (mean_wealth - sd_wealth) / 1e6, 
                  ymax = (mean_wealth + sd_wealth) / 1e6, 
                  fill = Farmer_type), 
              alpha = 0.15, color = NA) +  # More transparent band
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  
  # Mean wealth trend (dashed lines)
  geom_line(linewidth = 1, linetype = "dashed") + 
  # Bold black horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "solid") +
  
  
  # Custom colors for lines
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +  
  
  # Y-axis with correct spacing (-3M start, increments of 10M)
  scale_y_continuous(
    limits = c(q_low, q_high),
    # breaks = seq(-100, 100, by = 20),  # Start at -3M, increment by 10M
    labels = scales::label_number(scale = 1, suffix = "M"),  # Display as "-3M", "10M", "20M", etc.
    expand = c(0, 0)
  ) +  
  
  # Labels
  labs(
    title = "Mean Wealth Over Time (with Uncertainty Bands)",
    x = "Step (Time)",
    y = "Mean Wealth (₹M)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario with custom labels
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(p_wealth)


ggsave("Mean_Wealth_Plot_v6.pdf", plot = p_wealth, width = 8, height = 5, device = cairo_pdf)


### MEAN TOTAL DEBT (100k Scale)
p_debt <- ggplot(mean_data, aes(x = Step, y = mean_total_debt + 1, color = Farmer_type, group = Farmer_type)) +
  geom_ribbon(aes(ymin = (mean_total_debt - sd_total_debt + 1), 
                  ymax = (mean_total_debt + sd_total_debt + 1), 
                  fill = Farmer_type), 
              alpha = 0.2, color = NA) +  
  geom_line(linewidth = 1) +  
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +  
  scale_y_log10(
    breaks = c(1, 1000, 10000, 100000, 1000000),  # Set specific intuitive breaks
    labels = c("₹1", "₹1k", "₹10k", "₹100k", "₹1M"),  # Human-readable labels
    expand = c(0, 0)
  ) +  
  labs(
    title = "Mean Total Debt Over Time (Log Scale with Uncertainty Bands)",
    x = "Step (Time)",
    y = "Mean Total Debt (₹, Log Scale)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_debt)



p_debt <- ggplot(mean_data, aes(x = Step, y = mean_total_debt, color = Farmer_type, group = Farmer_type)) +
  
  # # Uncertainty bands (mean ± SD)
  # geom_ribbon(aes(ymin = pmax(mean_total_debt - sd_total_debt, 0),  # Ensure no negative values
  #                 ymax = mean_total_debt + sd_total_debt, 
  #                 fill = Farmer_type), 
  #             alpha = 0.15, color = NA) +  # More transparent band
  # 
  # Mean total debt trend (dashed lines)
  geom_line(linewidth = 1, linetype = "dashed") +  
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  
  # Custom colors for lines
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +  
  
  # Y-axis using pseudo-log scale for better low-value visibility
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),  # Smooth log-like transformation
    breaks = c(0, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 10000000),  
    labels = scales::label_number(scale = 1, prefix = "₹")  # More readable labels
  ) +  
  
  # Labels
  labs(
    title = "Mean Total Debt Over Time (Pseudo-Log Scale)",
    x = "Step (Time)",
    y = "Mean Total Debt (₹, Pseudo-Log Scale)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario with custom labels
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  
  # Theme adjustments
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_debt)
ggsave("Mean_Debt_Plot1_v6.pdf", plot = p_debt, width = 8, height = 5, device = cairo_pdf)



# Ensure Farmer_type is ordered correctly
combined_df$Farmer_type <- factor(combined_df$Farmer_type, 
                                  levels = c("Large", "Medium", "Semi-medium", "Small", "Marginal"))

# Compute mean loans for Scenario 10 and reshape the data
scenario_10_df <- combined_df %>%
  filter(Scenario == "05") %>%
  group_by(Step, Farmer_type) %>%
  summarise(
    mean_neighbour_loan = mean(neighbour_loan, na.rm = TRUE),
    mean_collateral_loan = mean(collateral_loan, na.rm = TRUE),
    mean_jgl_loan = mean(jgl_loan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "Loan_Type",
    values_to = "Mean_Loan"
  ) %>%
  mutate(
    Mean_Loan_Thousand = Mean_Loan / 1e3,  # Convert to thousands (not millions)
    Loan_Type = recode(Loan_Type,  
                       "mean_neighbour_loan" = "Neighbour Loan",
                       "mean_collateral_loan" = "Collateral Loan",
                       "mean_jgl_loan" = "JGL Loan"
    )
  )

# Define colors for loan types
loan_colors <- c("Neighbour Loan" = "blue", "Collateral Loan" = "red", "JGL Loan" = "green")

# Plot with normal scale
p2 <- ggplot(scenario_10_df, aes(x = Step, y = Mean_Loan, color = Loan_Type, group = Loan_Type)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  
  scale_color_manual(values = loan_colors) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),  # Smooth log-like transformation
    breaks = c(0, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 10000000),  
    labels = scales::label_number(scale = 1, prefix = "₹")  # More readable labels
  ) +  
  
  labs(
    title = "Scenario 05 - Mean Loan Breakdown by Farmer Type",
    x = "Step (Time)",
    y = "Mean Loan Amount",
    color = "Loan Type"
  ) +
  facet_wrap(~ Farmer_type, scales = "fixed") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

print(p2)

ggsave("Mean_Debt_Plot_S05_v6.pdf", plot = p2, width = 8, height = 5, device = cairo_pdf)






## POVERTY DURATION
library(purrr)

# Ensure binary integer values
resilience <- combined_df %>%
  select(Step, Scenario, File_ID, AgentID, Farmer_type, poverty_trap) %>%
  arrange(Scenario, File_ID, AgentID, Step) %>%
  mutate(poverty_trap = as.integer(poverty_trap))

# Function to extract durations of 1s before returning to 0 or end
get_poverty_durations <- function(trap_values) {
  durations <- c()
  in_trap <- FALSE
  counter <- 0
  
  for (val in trap_values) {
    if (val == 1) {
      counter <- counter + 1
      in_trap <- TRUE
    } else {
      if (in_trap) {
        durations <- c(durations, counter)
        counter <- 0
        in_trap <- FALSE
      }
    }
  }
  
  # If still in trap at the end
  if (in_trap && counter > 0) {
    durations <- c(durations, counter)
  }
  
  return(durations)
}

# Apply function group-wise
poverty_durations <- resilience %>%
  group_by(Scenario, File_ID, AgentID, Farmer_type) %>%
  summarise(
    durations = list(get_poverty_durations(poverty_trap)),
    .groups = "drop"
  )

# Optional: Count how many agents had more than one episode
agents_multiple_poverty_episodes <- poverty_durations %>%
  mutate(num_episodes = map_int(durations, length)) %>%
  filter(num_episodes > 1)

# View result
head(poverty_durations)


library(stringr)
library(purrr)

poverty_durations_expanded <- poverty_durations %>%
  mutate(
    durations = map(durations, function(x) {
      # Case 1: NULL or empty
      if (is.null(x) || length(x) == 0) return(0)
      
      # Case 2: Already numeric
      if (is.numeric(x)) return(x)
      
      # Case 3: Character string that needs to be parsed
      if (is.character(x)) {
        # Extract all numbers from string, including "c(1, 2)" or 'c("1", "2")'
        nums <- str_extract_all(x, "\\d+")[[1]]
        return(as.numeric(nums))
      }
      
      # Fallback: try to coerce anything else into numeric
      return(as.numeric(x))
    })
  ) %>%
  unnest_longer(durations, values_to = "duration")


library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# Step 1: Per-agent summary
agent_summary <- poverty_durations_expanded %>%
  group_by(Scenario, AgentID, Farmer_type) %>%
  summarise(
    mean_duration = max(duration),
    median_duration = max(duration),
    .groups = "drop"
  )


# Per farmer type in each scenario
type_summary <- agent_summary %>%
  group_by(Scenario, Farmer_type) %>%
  summarise(
    mean_duration = mean(mean_duration),
    median_duration = median(median_duration),
    .groups = "drop"
  )

# Overall (all farmer types) in each scenario
overall_summary <- agent_summary %>%
  group_by(Scenario) %>%
  summarise(
    Farmer_type = "Overall",
    mean_duration = mean(mean_duration),
    median_duration = median(median_duration),
    .groups = "drop"
  )

# Combine both
combined_summary <- bind_rows(type_summary, overall_summary)


# Factor levels for ordering
combined_summary$Farmer_type <- factor(
  combined_summary$Farmer_type,
  levels = c("Overall", "Large", "Medium", "Semi-medium", "Small", "Marginal")
)

# Custom colors
color_palette <- c(
  "Overall" = "black",
  "Large" = "goldenrod2",
  "Medium" = "blue",
  "Semi-medium" = "purple",
  "Small" = "green",
  "Marginal" = "red"
)

combined_summary$Scenario <- factor(
  combined_summary$Scenario,
  levels = c("01", "02", "03", "04", "05"),
  labels = c("No Lending", "Only Neighbour", "Only Banks", "Only JLGs", "Combined Lending")
)

# Final plot with adjusted y-axis breaks and scenario labels
pp <- ggplot(combined_summary, aes(x = Farmer_type, y = mean_duration, fill = Farmer_type)) +
  geom_col(position = "dodge", color = "black", width = 0.5) +  # slimmer bars
  facet_wrap(~Scenario, scales = "fixed", labeller = labeller(Scenario = label_value)) +  # add scenario labels
  scale_fill_manual(values = color_palette) +
  scale_y_continuous(
    limits = c(0, max(combined_summary$mean_duration)),   # Adjust upper limit based on your data
    breaks = seq(0, max(combined_summary$mean_duration) + 5, by = 5)  # Set breaks every 1 step
  ) +
  labs(
    title = "Mean Poverty Duration by Farmer Type Across Scenarios",
    x = NULL,  # remove x-axis label
    y = "Mean Duration (Years)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),  # remove x-axis text
    legend.position = "bottom",     # legend at the bottom
    strip.text = element_text(face = "bold", size = 12),
    legend.title = element_blank()  # remove legend title
  )


ggsave("Mean_Poverty_Duration_v1.pdf", plot = pp, width = 8, height = 5, device = cairo_pdf)








## FINANCIAL RESILIENCE
combined_df <- combined_df %>%
  mutate(
    total_assets = Liquid_asset + `Available Collateral`,
    financial_resilience = if_else(
      total_assets <= 0 | (`Total Debt` / total_assets) >= 0.75,
      1L, 0L
    )
  )

library(purrr)
library(dplyr)
library(tidyr)

# Ensure binary integer values for the target column (financial_resilience or poverty_trap)
resilience_financial <- combined_df %>%
  select(Step, Scenario, File_ID, AgentID, Farmer_type, financial_resilience) %>%
  arrange(Scenario, File_ID, AgentID, Step) %>%
  mutate(financial_resilience = as.integer(financial_resilience))

# Function to count consecutive 1s (for financial resilience or poverty trap)
count_consecutive_ones <- function(values) {
  counts <- c()
  counter <- 0
  
  for (val in values) {
    if (val == 1) {
      counter <- counter + 1  # Increase counter for consecutive 1s
    } else {
      if (counter > 0) {
        counts <- c(counts, counter)  # Store count when a 0 is encountered
        counter <- 0  # Reset counter
      }
    }
  }
  
  # If there are trailing consecutive 1s at the end
  if (counter > 0) {
    counts <- c(counts, counter)
  }
  
  return(counts)
}

# Apply function group-wise to get consecutive 1 counts
consecutive_ones <- resilience_financial %>%
  group_by(Scenario, File_ID, AgentID, Farmer_type) %>%
  summarise(
    consecutive_ones = list(count_consecutive_ones(financial_resilience)),
    .groups = "drop"
  )

expanded_consecutive_ones <- consecutive_ones %>%
  mutate(
    consecutive_ones = map(consecutive_ones, ~ if (length(.) == 0) list(0) else list(.))  # wrap in list()
  ) %>%
  unnest_longer(consecutive_ones, values_to = "consecutive_ones")

# View the result
head(expanded_consecutive_ones)

expanded_consecutive_ones <- expanded_consecutive_ones %>%
  mutate(
    consecutive_ones = map_int(consecutive_ones, ~ max(as.integer(.)))
  )



f1 <- expanded_consecutive_ones %>%
  mutate(n_values = map_int(consecutive_ones, length)) %>%
  filter(n_values > 1) %>%
  select(Scenario, File_ID, AgentID, Farmer_type, consecutive_ones)


library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)

# Step 1: Extract max consecutive ones
agent_max_consecutive <- expanded_consecutive_ones %>%
  mutate(consecutive_ones = map_int(consecutive_ones, ~ max(as.integer(.))))

# Step 2: Flag agents with >3 consecutive ones
agent_flagged <- agent_max_consecutive %>%
  mutate(more_than_3 = consecutive_ones > 3)

# Step 3: Proportions per farmer type and scenario
proportion_by_type <- agent_flagged %>%
  group_by(Scenario, Farmer_type) %>%
  summarise(prop = mean(more_than_3), .groups = "drop")

# Step 4: Overall proportions per scenario
proportion_overall <- agent_flagged %>%
  group_by(Scenario) %>%
  summarise(prop = mean(more_than_3), Farmer_type = "Overall", .groups = "drop")

# Step 5: Combine and factor levels
proportion_combined <- bind_rows(proportion_by_type, proportion_overall)

proportion_combined$Farmer_type <- factor(
  proportion_combined$Farmer_type,
  levels = c("Overall", "Large", "Medium", "Semi-medium", "Small", "Marginal")
)

# Step 6: Scenario labels
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

# Step 7: Custom color palette
color_palette <- c(
  "Overall" = "black",
  "Large" = "goldenrod2",
  "Medium" = "blue",
  "Semi-medium" = "purple",
  "Small" = "green",
  "Marginal" = "red"
)

# Step 8: Plot
pf <- ggplot(proportion_combined, aes(x = Farmer_type, y = prop, fill = Farmer_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels)) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Proportion of Agents with >3 Years of Consecutive Financial Distress",
    x = "Farmer Type",
    y = "Proportion",
    fill = "Farmer Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
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
  

ggsave("Mean_Financial_Resilience_v1.pdf", plot = pf, width = 8, height = 5, device = cairo_pdf)






scenario_05_loaned <- combined_df %>%
  filter(Scenario == "05") %>%
  group_by(Step, Farmer_type) %>%
  summarise(
    mean_loaned = mean(Loaned, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_Loaned_Thousand = mean_loaned / 1e5  # Convert to ₹ Thousands
  )


p3 <- ggplot(scenario_05_loaned, aes(x = Step, y = Mean_Loaned_Thousand)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_vline(xintercept = c(10, 16), color = "grey50", size = 2, alpha = 0.3) +
  
  facet_wrap(~ Farmer_type, scales = "fixed") +
  scale_y_continuous(
    breaks = seq(0, max(scenario_05_loaned$Mean_Loaned_Thousand, na.rm = TRUE), by = 5),
    labels = function(x) paste0(x, "Lacs"),
    expand = c(0, 0)
  ) +
  labs(
    title = "Scenario 05 - Mean Total Loaned Amount by Farmer Type (₹ Lakhs)",
    x = "Step (Time)",
    y = "Mean Loaned Amount (₹ Lakhs)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

ggsave("Mean_Total_Loaned_S05_v1.pdf", plot = p3, width = 8, height = 5, device = cairo_pdf)

