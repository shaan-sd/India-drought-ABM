


# Calculate quantile range based on median_money_real
q_low_median <- quantile(mean_data$median_money_real, 0.01, na.rm = TRUE) / 1e6
q_high_median <- quantile(mean_data$median_money_real, 0.99, na.rm = TRUE) / 1e6

# Generate plot for median
p_median_money_real <- ggplot(mean_data, aes(x = Step, y = median_money_real / 1e6, color = Farmer_type, group = Farmer_type)) +
  
  # Optional: Use SD band from mean if median SD is not available
  geom_ribbon(aes(
    ymin = (median_money_real - sd_money_real) / 1e6,
    ymax = (median_money_real + sd_money_real) / 1e6,
    fill = Farmer_type
  ),
  alpha = 0.1, color = NA) +
  
  # Solid line for median money real
  geom_line(linewidth = 0.8, linetype = "solid") +
  
  # Horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "solid") +
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Y-axis range
  scale_y_continuous(
    limits = c(q_low_median, q_high_median),
    labels = label_number(scale = 1, suffix = "M"),
    expand = c(0.01, 0.01)
  ) +
  
  # Labels and titles
  labs(
    title = "Median Liquid Networth Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Median Liquid Networth (₹ in Millions)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
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

# Print median plot
print(p_median_money_real)



p_money_real <- ggplot(mean_data, aes(x = Step, y = mean_money_real / 1e6, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(
    ymin = (mean_money_real - sd_money_real) / 1e6,
    ymax = (mean_money_real + sd_money_real) / 1e6,
    fill = Farmer_type
  ),
  alpha = 0.1, color = NA) +
  
  # Dashed line for mean net worth
  geom_line(linewidth = 0.8, linetype = "dashed") +
  
  # Horizontal reference at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "solid") +
  
  # Manual colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Y-axis with pseudo-log transform
  scale_y_continuous(
    trans = scales::pseudo_log_trans(sigma = 1e6),
    labels = label_number(scale = 1, suffix = "M"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  # Labels and title
  labs(
    title = "Mean Liquid Networth Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Liquid Networth (₹ in Millions)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by scenario
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
  # Theme tweaks
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





p_money_real <- ggplot(mean_data, aes(x = Step, y = mean_money_real / 1e6, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = (mean_money_real - sd_money_real) / 1e6, 
                  ymax = (mean_money_real + sd_money_real) / 1e6, 
                  fill = Farmer_type), 
              alpha = 0.1, color = NA) +
  
  # Dashed line for mean net worth
  geom_line(linewidth = 0.8, linetype = "dashed") +
  
  # Horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Manual colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Y-axis breaks and labels in millions
  scale_y_continuous(
    labels = scales::label_number(scale = 1, suffix = "M")
  ) +
  
  # Zoom in around -2.5M to 10M
  coord_cartesian(ylim = c(-2.5, 10)) +
  
  # Labels and title
  labs(
    title = "Mean Liquid Networth Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Liquid Networth (₹ in Millions)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario with nicer labels
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
  # Theme tweaks
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

print(p_money_real)








p_money_real <- ggplot(mean_data, aes(x = Step, y = mean_money_real / 1e6, color = Farmer_type, group = Farmer_type)) +
  # Uncertainty bands (mean ± SD) with higher transparency
  geom_ribbon(aes(ymin = (mean_money_real - sd_money_real) / 1e6, 
                  ymax = (mean_money_real + sd_money_real) / 1e6, 
                  fill = Farmer_type), 
              alpha = 0.1, color = NA) +
  
  # Dashed Mean money line
  geom_line(linewidth = 0.8, linetype = "dashed") +  
  
  # Bold black horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Y-axis in millions with custom breaks
  scale_y_continuous(
    breaks = c(-10, -5, 0, 2.5, 5, 10, 20),
    labels = scales::label_number(scale = 1, suffix = "M"),
    expand = c(0, 0)
  ) +
  
  # 🔍 Zoom in to -5M to 10M
  coord_cartesian(ylim = c(-10, 20)) +
  
  # Labels
  labs(
    title = "Mean Liquid Asset Over Time with Uncertainty Bands",
    x = "Step (Time)",
    y = "Mean Liquid Asset (₹ in Millions)",
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

print(p_money_real)







p_wealth <- ggplot(mean_data, aes(x = Step, y = mean_wealth / 1e6, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = (mean_wealth - sd_wealth) / 1e6, 
                  ymax = (mean_wealth + sd_wealth) / 1e6, 
                  fill = Farmer_type), 
              alpha = 0.15, color = NA) +
  
  # Mean wealth trend (dashed lines)
  geom_line(linewidth = 1, linetype = "dashed") + 
  
  # Bold black horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Y-axis with correct spacing
  scale_y_continuous(
    breaks = seq(-20, 170, by = 10),
    labels = scales::label_number(scale = 1, suffix = "M"),
    expand = c(0, 0)
  ) +
  
  # 🔍 Zoom in from -20M to 170M
  coord_cartesian(ylim = c(-20, 120)) +
  
  # Labels
  labs(
    title = "Mean Wealth Over Time (with Uncertainty Bands)",
    x = "Step (Time)",
    y = "Mean Wealth (₹M)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by scenario
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
  # Theme
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


















p_debt <- ggplot(mean_data, aes(x = Step, y = mean_total_debt, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands (mean ± SD)
  geom_ribbon(aes(ymin = pmax(mean_total_debt - sd_total_debt, 0),  # Ensure no negative values
                  ymax = mean_total_debt + sd_total_debt, 
                  fill = Farmer_type), 
              alpha = 0.15, color = NA) +
  
  # Mean total debt trend (dashed lines)
  geom_line(linewidth = 1, linetype = "dashed") +  
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Linear Y-axis
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = scales::label_number(scale = 1, prefix = "₹")
  ) +
  
  # Labels
  labs(
    title = "Mean Total Debt Over Time (Linear Scale with Uncertainty Bands)",
    x = "Step (Time)",
    y = "Mean Total Debt (₹)",  
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") + 
  
  # Theme
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






library(scales)

p_debt <- ggplot(mean_data, aes(x = Step, y = mean_total_debt, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands
  geom_ribbon(aes(
    ymin = pmax(mean_total_debt - sd_total_debt, 0),
    ymax = mean_total_debt + sd_total_debt,
    fill = Farmer_type),
    alpha = 0.15, color = NA
  ) +
  
  # Mean trend line
  geom_line(linewidth = 1, linetype = "dashed") +
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Squished scale to stretch lower values
  scale_y_continuous(
    trans = squish_trans(from = 0, to = 100000),
    breaks = c(0, 10000, 50000, 100000, 500000, 1e6, 5e6, 1e7, 1.5e7),
    labels = label_number(scale = 1, prefix = "₹")
  ) +
  
  # Cut y-axis at 15 million
  coord_cartesian(ylim = c(0, 15000000)) +
  
  # Labels
  labs(
    title = "Mean Total Debt Over Time (Stretched Lower Range)",
    x = "Step (Time)",
    y = "Mean Total Debt (₹)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
  # Theme
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






library(scales)

p_debt <- ggplot(mean_data, aes(x = Step, y = mean_total_debt, color = Farmer_type, group = Farmer_type)) +
  
  # Uncertainty bands
  geom_ribbon(aes(
    ymin = pmax(mean_total_debt - sd_total_debt, 0),
    ymax = mean_total_debt + sd_total_debt,
    fill = Farmer_type),
    alpha = 0.15, color = NA
  ) +
  
  # Mean trend line
  geom_line(linewidth = 1, linetype = "dashed") +
  
  # Custom colors
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) +
  
  # Y-axis scaling with custom breaks and limits
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10),  # Log-like transformation
    breaks = c(0, 10000, 50000, 100000, 500000, 1e6, 5e6, 1e7, 1.5e7),  # Custom breaks
    labels = label_number(scale = 1, prefix = "₹")  # Display labels
  ) +
  
  # Cut y-axis at 15 million
  coord_cartesian(ylim = c(0, 15000000)) +
  
  # Labels
  labs(
    title = "Mean Total Debt Over Time (Pseudo-log scale)",
    x = "Step (Time)",
    y = "Mean Total Debt (₹)",
    color = "Farmer Type",
    fill = "Farmer Type"
  ) +
  
  # Facet by Scenario
  facet_wrap(~ Scenario, labeller = as_labeller(scenario_labels), nrow = 1, scales = "fixed") +
  
  # Theme
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





# Convert Crop_Data from JSON-like string to actual lists
crop_dist_initial <- combined_df %>%
  mutate(
    Crop_Data = str_replace_all(Crop_Data, "'", "\""),  # Ensure proper quote marks
    Crop_Data = map(Crop_Data, ~ tryCatch(
      fromJSON(.) %>% as.list(),   # Convert to list
      error = function(e) NULL     # In case of error, return NULL
    ))
  )
