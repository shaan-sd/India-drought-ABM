import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Step 1: Load the agent data
agent_data = pd.read_csv("agent_data_output.csv")

# Step 2: Filter for Step 1
agent_data_step1 = agent_data[agent_data["Step"] == 1]

# Step 3: Extract position and farmer type
positions = agent_data_step1["Position"].apply(eval)  # Convert string to tuple
farmer_types = agent_data_step1["Farmer_type"]

# Step 4: Map farmer types to colors
type_color_map = {
    "Large": "red",       # Large farmers -> Red
    "Medium": "blue",     # Medium farmers -> Blue
    "Semi-medium": "green",  # Semi-medium farmers -> Green
    "Small": "purple",    # Small farmers -> Purple
    "Marginal": "orange"  # Marginal farmers -> Orange
}

# Step 5: Initialize the plot
fig, ax = plt.subplots(figsize=(10, 10))  # Adjust size as needed

# Step 6: Plot each agent's position on the grid with the corresponding color
for idx, (position, farmer_type) in enumerate(zip(positions, farmer_types)):
    x, y = position
    ax.scatter(x, y, color=type_color_map.get(farmer_type, 'black'), label=farmer_type, alpha=0.7)

# Step 7: Add labels and grid
ax.set_title("Farmer Positioning on Grid (Step 1)", fontsize=14)
ax.set_xlabel("X Position")
ax.set_ylabel("Y Position")
ax.grid(True)

# Step 8: Show legend
handles, labels = ax.get_legend_handles_labels()
# Remove duplicates from the legend
unique_labels = dict(zip(labels, handles))
ax.legend(unique_labels.values(), unique_labels.keys())

# Step 9: Save the plot as a PDF file
plot_filename = "farmer_position_grid_step1.pdf"
plt.savefig(plot_filename, format='pdf')

# Step 10: Optionally show the plot
plt.show()

print(f"Plot saved as {plot_filename}")
