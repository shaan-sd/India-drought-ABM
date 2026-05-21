# loading required libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)
library(ggalluvial)

# set working directory
setwd("C:/Users/dassu/ABM_Paper_Graphs/abm_cluster_fin_and_clim_shock_ubi/")


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


combined_df <- combined_df %>%
  mutate(Wealth = `Available Collateral` + Liquid_asset)


#BPL threshold for base case scenario
threshold <- combined_df %>%
  filter(Scenario == "01") %>%
  summarise(third_quartile = quantile(Liquid_asset, 0.75, na.rm = TRUE)) %>%
  summarise(poverty_line_threshold = mean(third_quartile)) %>%
  pull(poverty_line_threshold)

cat("Poverty line threshold:", format(threshold, big.mark = ",", scientific = FALSE), "\n")



combined_df$poverty <- with(combined_df, ifelse(Liquid_asset <= 0, "EP",
                              ifelse(Liquid_asset <= 66000, "CP", "NP")))


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)

# Ensure ScenarioName is a factor with proper order
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

scenario_order <- scenario_labels[c("01", "02", "03", "04", "05")]

combined_df1 <- combined_df %>%
  mutate(
    time_block = case_when(
      Step == 10 ~ "Before",
      Step == 16 ~ "After",
      Step == 30 ~ "End",
      TRUE ~ NA_character_
    ),
    Scenario_num = sprintf("%02d", as.numeric(Scenario)),
    File_ID_num = sprintf("%02d", as.numeric(File_ID)),
    AgentID_num = sprintf("%04d", AgentID),
    agent_id_str = paste0(AgentID_num, File_ID_num, Scenario_num),
    agent_id = as.numeric(agent_id_str),
    ScenarioName = factor(recode(Scenario, !!!scenario_labels), levels = scenario_order)
  )

# Wide format with Farmer_type
wide_df <- combined_df1 %>%
  filter(!is.na(time_block)) %>%
  select(agent_id, ScenarioName, Farmer_type, time_block, poverty) %>%
  distinct() %>%
  pivot_wider(names_from = time_block, values_from = poverty)

# Long format for plotting
long_df <- wide_df %>%
  pivot_longer(cols = c(Before, After, End),
               names_to = "Time", values_to = "PovertyState") %>%
  mutate(
    Time = factor(Time, levels = c("Before", "After", "End")),
    PovertyState = factor(PovertyState, levels = c("EP", "CP", "NP"))  # EP at bottom
  )






# Ensure ordered factors
long_df1 <- long_df %>%
  mutate(
    Time = factor(Time, levels = c("Before", "After", "End")),
    PovertyState = factor(PovertyState, levels = c("EP", "CP", "NP")),
    Farmer_type = factor(Farmer_type, 
                         levels = c("Large", "Medium", "Semi-medium", "Small", "Marginal"))
  )

# Plotting with facet grid
pdf("poverty_sankey_by_scenario_farmer_type_v6.pdf", width = 14, height = 10)

ggplot(long_df1,
       aes(x = Time, stratum = PovertyState, alluvium = agent_id,
           fill = PovertyState, label = PovertyState)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.6) +
  geom_stratum(width = 0.25, color = "black") +
  scale_fill_manual(
    values = c("EP" = "red", "CP" = "yellow", "NP" = "blue"),
    breaks = c("EP", "CP", "NP")  # Ensure consistent stacking
  ) +
  facet_grid(Farmer_type ~ ScenarioName) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  labs(title = "Poverty State Transitions by Lending Scenario and Farmer Type",
       x = "", y = "Number of Households") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

dev.off()





pdf("poverty_sankey_by_scenario_overall_v6.pdf", width = 10, height = 6)

ggplot(long_df1,
       aes(x = Time, stratum = PovertyState, alluvium = agent_id,
           fill = PovertyState, label = PovertyState)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.6) +
  geom_stratum(width = 0.25, color = "black") +
  scale_fill_manual(
    values = c("EP" = "red", "CP" = "yellow", "NP" = "blue"),
    breaks = c("EP", "CP", "NP")
  ) +
  facet_wrap(~ ScenarioName) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  labs(title = "Poverty State Transitions by Scenario (Overall)",
       x = "", y = "Number of Households") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

dev.off()





# Normalize: count per group to relative frequency
library(dplyr)

long_df1_scaled <- long_df1 %>%
  group_by(Time, ScenarioName, Farmer_type) %>%
  mutate(group_size = n()) %>%
  ungroup()

# Use group_size as weights (ggalluvial doesn’t support weights directly, so we work around it)
# Trick: assign dummy `alluvium` values per group of fixed size
long_df1_scaled <- long_df1_scaled %>%
  group_by(ScenarioName, Farmer_type) %>%
  mutate(agent_id = paste0(ScenarioName, "_", Farmer_type, "_", row_number())) %>%
  ungroup()

# Plot
pdf("poverty_sankey_by_scenario_and_farmer_type_scaled.pdf", width = 14, height = 10)

ggplot(long_df1_scaled,
       aes(x = Time, stratum = PovertyState, alluvium = agent_id,
           fill = PovertyState, label = PovertyState)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.6) +
  geom_stratum(width = 0.25, color = "black") +
  scale_fill_manual(
    values = c("EP" = "red", "CP" = "yellow", "NP" = "blue"),
    breaks = c("EP", "CP", "NP")
  ) +
  facet_grid(Farmer_type ~ ScenarioName, scales = "fixed") +
  theme_minimal() +
  labs(title = "Poverty State Transitions (Scaled to Equal Heights per Farmer Type)",
       x = "", y = "Relative Share of Households") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

dev.off()





# First, assign numeric values to PovertyState
poverty_map <- c("EP" = 0, "CP" = 1, "NP" = 2)

# Add numeric value column
long_df2 <- long_df %>%
  mutate(PovertyCode = poverty_map[PovertyState])

# Create trajectory code: spread, then paste the numeric codes
trajectory_df <- long_df2 %>%
  select(agent_id, ScenarioName, Farmer_type, Time, PovertyCode) %>%
  pivot_wider(names_from = Time, values_from = PovertyCode) %>%
  mutate(trajectory = paste0(Before, After, End))

# View result
head(trajectory_df)


# Summary by trajectory only
summary_by_trajectory <- trajectory_df %>%
  group_by(trajectory) %>%
  summarise(num_households = n())

# Summary by farmer type only
summary_by_farmer_type <- trajectory_df %>%
  group_by(trajectory, Farmer_type) %>%
  summarise(num_households = n())

# Summary by scenario only
summary_by_scenario <- trajectory_df %>%
  group_by(trajectory,ScenarioName) %>%
  summarise(num_households = n())

# Summary by both scenario and farmer type
summary_by_scenario_farmer <- trajectory_df %>%
  group_by(trajectory,ScenarioName, Farmer_type) %>%
  summarise(num_households = n())

# Save each as CSV
write.csv(summary_by_trajectory, "summary_by_trajectory.csv", row.names = FALSE)
write.csv(summary_by_farmer_type, "summary_by_farmer_type.csv", row.names = FALSE)
write.csv(summary_by_scenario, "summary_by_scenario.csv", row.names = FALSE)
write.csv(summary_by_scenario_farmer, "summary_by_scenario_farmer.csv", row.names = FALSE)


