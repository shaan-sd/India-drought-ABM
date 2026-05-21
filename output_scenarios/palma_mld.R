library(dplyr)
library(ggplot2)

# Step 1: Shift values to avoid negatives (you’ve done this already)
combined_df1 <- combined_df %>%
  mutate(
    Wealth_Adjusted = Wealth + 40000000,
    Money_Adjusted = Money + 3000000
  )

# Step 2: Scenario labels
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

# Step 3: Define Palma Ratio Function
calculate_palma <- function(values) {
  sorted <- sort(values, decreasing = TRUE)
  n <- length(sorted)
  top_10 <- sorted[1:ceiling(0.1 * n)]
  bottom_40 <- sorted[(n - floor(0.4 * n) + 1):n]
  
  top_sum <- sum(top_10)
  bottom_sum <- sum(bottom_40)
  
  if (bottom_sum == 0) return(NA)
  return(top_sum / bottom_sum)
}

# Step 4: Define Mean Log Deviation (MLD) Function
calculate_mld <- function(values) {
  values <- values[values > 0]
  if (length(values) == 0) return(NA)
  mean_val <- mean(values)
  return(mean(log(mean_val / values)))
}

# Step 5: Calculate MLD and Palma Ratio by Scenario & Step
inequality_df <- combined_df1 %>%
  group_by(Scenario, Step) %>%
  summarise(
    Palma_Wealth = calculate_palma(Wealth_Adjusted),
    Palma_Money  = calculate_palma(Money_Adjusted),
    MLD_Wealth   = calculate_mld(Wealth_Adjusted),
    MLD_Money    = calculate_mld(Money_Adjusted),
    .groups = "drop"
  )

# Step 6: Replace Scenario with descriptive labels
inequality_df$Scenario_Label <- scenario_labels[inequality_df$Scenario]

# Step 7: Plotting Function
plot_metric <- function(df, metric_col, y_label, title, file_name, y_limits = NULL) {
  p <- ggplot(df, aes(x = Step, y = .data[[metric_col]], color = Scenario_Label, group = Scenario_Label)) +
    geom_line(linewidth = 0.8, linetype = "dashed") +
    geom_point(size = 2) +
    scale_color_manual(values = c("green", "blue", "red", "purple", "orange")) +
    labs(
      title = title,
      x = "Step (Time)",
      y = y_label,
      color = "Scenario"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  # Save plot (uncomment below if needed)
  # ggsave(file_name, plot = p, width = 8, height = 5, device = cairo_pdf)
  
  return(p)
}

# Step 8: Create Plots

# Palma Plots
palma_wealth_plot <- plot_metric(
  inequality_df, "Palma_Wealth", "Palma Ratio (Wealth)",
  "Wealth Palma Ratio Over Time", "Palma_Wealth.pdf", y_limits = c(0, NA)
)

palma_money_plot <- plot_metric(
  inequality_df, "Palma_Money", "Palma Ratio (Money)",
  "Money Palma Ratio Over Time", "Palma_Money.pdf", y_limits = c(0, NA)
)

# MLD Plots
mld_wealth_plot <- plot_metric(
  inequality_df, "MLD_Wealth", "Mean Log Deviation (Wealth)",
  "Wealth MLD Over Time", "MLD_Wealth.pdf", y_limits = c(0, NA)
)

mld_money_plot <- plot_metric(
  inequality_df, "MLD_Money", "Mean Log Deviation (Money)",
  "Money MLD Over Time", "MLD_Money.pdf", y_limits = c(0, NA)
)

# Step 9: Print plots
print(palma_wealth_plot)
print(palma_money_plot)
print(mld_wealth_plot)
print(mld_money_plot)




















library(dplyr)
library(ggplot2)

# Step 1: Shift values to avoid negatives
combined_df1 <- combined_df %>%
  mutate(
    Wealth_Adjusted = Wealth + 40000000,
    Money_Adjusted = Money + 3000000
  )

# Step 2: Scenario labels
scenario_labels <- c(
  "01" = "No Lending",
  "02" = "Only Neighbour",
  "03" = "Only Banks",
  "04" = "Only JLGs",
  "05" = "Combined Lending"
)

# Step 3: Palma Ratio function
calculate_palma <- function(values) {
  sorted <- sort(values, decreasing = TRUE)
  n <- length(sorted)
  top_10 <- sorted[1:ceiling(0.1 * n)]
  bottom_40 <- sorted[(n - floor(0.4 * n) + 1):n]
  top_sum <- sum(top_10)
  bottom_sum <- sum(bottom_40)
  if (bottom_sum == 0) return(NA)
  return(top_sum / bottom_sum)
}

# Step 4: Mean Log Deviation function
calculate_mld <- function(values) {
  values <- values[values > 0]
  if (length(values) == 0) return(NA)
  mean_val <- mean(values)
  return(mean(log(mean_val / values)))
}

# Step 5: Compute Palma and MLD per File_ID, Scenario, Step
inequality_raw <- combined_df1 %>%
  group_by(File_ID, Scenario, Step) %>%
  summarise(
    Palma_Wealth = calculate_palma(Wealth_Adjusted),
    Palma_Money  = calculate_palma(Money_Adjusted),
    MLD_Wealth   = calculate_mld(Wealth_Adjusted),
    MLD_Money    = calculate_mld(Money_Adjusted),
    .groups = "drop"
  )

# Step 6: Aggregate mean and SD across File_IDs for each Scenario and Step
inequality_summary <- inequality_raw %>%
  group_by(Scenario, Step) %>%
  summarise(
    Palma_Wealth_Mean = mean(Palma_Wealth, na.rm = TRUE),
    Palma_Wealth_SD   = sd(Palma_Wealth, na.rm = TRUE),
    
    Palma_Money_Mean  = mean(Palma_Money, na.rm = TRUE),
    Palma_Money_SD    = sd(Palma_Money, na.rm = TRUE),
    
    MLD_Wealth_Mean   = mean(MLD_Wealth, na.rm = TRUE),
    MLD_Wealth_SD     = sd(MLD_Wealth, na.rm = TRUE),
    
    MLD_Money_Mean    = mean(MLD_Money, na.rm = TRUE),
    MLD_Money_SD      = sd(MLD_Money, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Scenario_Label = scenario_labels[Scenario],
    Scenario_Label = factor(Scenario_Label, levels = scenario_labels)  # Fix order here
  )

# Step 7: Plot function with uncertainty bands
plot_metric_with_ribbon <- function(df, metric_mean, metric_sd, y_label, title, file_name, y_limits = NULL) {
  p <- ggplot(df, aes(
    x = Step,
    y = .data[[metric_mean]],
    color = Scenario_Label,
    fill = Scenario_Label,
    group = Scenario_Label
  )) +
    geom_line(linewidth = 0.7, linetype = "dashed") +
    geom_point(size = 2) +
    geom_ribbon(aes(
      ymin = .data[[metric_mean]] - .data[[metric_sd]],
      ymax = .data[[metric_mean]] + .data[[metric_sd]]
    ), alpha = 0.2, linetype = 0) +
    scale_color_manual(values = c("green", "blue", "red", "purple", "orange")) +
    scale_fill_manual(values = c("green", "blue", "red", "purple", "orange")) +
    labs(
      title = title,
      x = "Step (Time)",
      y = y_label,
      color = "Scenario",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  # Save as PDF
  ggsave(filename = file_name, plot = p, width = 8, height = 5)
  
  return(p)
}

# Step 8: Create and save plots

# Palma plots
palma_wealth_plot <- plot_metric_with_ribbon(
  inequality_summary, "Palma_Wealth_Mean", "Palma_Wealth_SD",
  "Palma Ratio (Wealth)", "Wealth Palma Ratio Over Time", "palma_wealth_sd_plot.pdf", y_limits = c(0, NA)
)

palma_money_plot <- plot_metric_with_ribbon(
  inequality_summary, "Palma_Money_Mean", "Palma_Money_SD",
  "Palma Ratio (Liquid Networth)", "Liquid Networth Palma Ratio Over Time", "palma_money_sd_plot.pdf", y_limits = c(0, NA)
)

# MLD plots
mld_wealth_plot <- plot_metric_with_ribbon(
  inequality_summary, "MLD_Wealth_Mean", "MLD_Wealth_SD",
  "Mean Log Deviation (Wealth)", "Wealth MLD Over Time", "mld_wealth_sd_plot.pdf", y_limits = c(0, NA)
)

mld_money_plot <- plot_metric_with_ribbon(
  inequality_summary, "MLD_Money_Mean", "MLD_Money_SD",
  "Mean Log Deviation (Money)", "Money MLD Over Time", "mld_money_sd_plot.pdf", y_limits = c(0, NA)
)

# Step 9: Print plots to screen
print(palma_wealth_plot)
print(palma_money_plot)
print(mld_wealth_plot)
print(mld_money_plot)


# Palma plots
palma_wealth_plot <- plot_metric_with_ribbon(
  inequality_summary, "Palma_Wealth_Mean", "Palma_Wealth_SD",
  "Palma Ratio (Wealth)", "Wealth Palma Ratio Over Time", "palma_wealth_sd_plot.pdf", y_limits = c(0, NA)
)
ggsave("palma_wealth_sd_plot.pdf", plot = palma_wealth_plot, width = 8, height = 5, device = cairo_pdf)

palma_money_plot <- plot_metric_with_ribbon(
  inequality_summary, "Palma_Money_Mean", "Palma_Money_SD",
  "Palma Ratio (Liquid Networth)", "Liquid Networth Palma Ratio Over Time", "palma_money_sd_plot.pdf", y_limits = c(0, NA)
)
ggsave("palma_money_sd_plot.pdf", plot = palma_money_plot, width = 8, height = 5, device = cairo_pdf)

# MLD plots
mld_wealth_plot <- plot_metric_with_ribbon(
  inequality_summary, "MLD_Wealth_Mean", "MLD_Wealth_SD",
  "Mean Log Deviation (Wealth)", "Wealth MLD Over Time", "mld_wealth_sd_plot.pdf", y_limits = c(0, NA)
)
ggsave("mld_wealth_sd_plot.pdf", plot = mld_wealth_plot, width = 8, height = 5, device = cairo_pdf)

mld_money_plot <- plot_metric_with_ribbon(
  inequality_summary, "MLD_Money_Mean", "MLD_Money_SD",
  "Mean Log Deviation (Money)", "Money MLD Over Time", "mld_money_sd_plot.pdf", y_limits = c(0, NA)
)
ggsave("mld_money_sd_plot.pdf", plot = mld_money_plot, width = 8, height = 5, device = cairo_pdf)


