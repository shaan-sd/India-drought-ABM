import pandas as pd
import numpy as np
import itertools
import math
import time
import random
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from collections import defaultdict

from mesa import Model
from mesa.time import BaseScheduler
from mesa.datacollection import DataCollector
from mesa.space import SingleGrid

from agents import Farmer
from objects import Farmland, JGL
from data import ModelParameters, calculate_number_of_crops, get_crop_dict

# Load crop data
crop_df_local = pd.read_csv('../analysis/crops/agg_data.csv', index_col=0)
initial_income_cost_of_living = pd.read_csv("../initial_income_cost_of_living.csv")
initial_income_cost_of_living.reset_index(drop=True, inplace=True)

# Precompute farm sizes and types
precomputed_df = pd.read_csv("../precomputed_farm_sizes_with_types_and_districts.csv")
precomputed_types_and_sizes = list(precomputed_df.itertuples(index=False, name=None))

# Initialize fixed farmer attributes
def initialize_fixed_farmers(precomputed_types_and_sizes):
    fixed_attributes = {}

    income_expenditure_dict = {
        farmer_type: {
            'income_values': initial_income_cost_of_living[
                initial_income_cost_of_living['farmer_size_class'] == farmer_type
            ]['income'].values,
            'expenditure_values': initial_income_cost_of_living[
                initial_income_cost_of_living['farmer_size_class'] == farmer_type
            ]['cost_of_living'].values
        }
        for farmer_type in initial_income_cost_of_living['farmer_size_class'].unique()
    }

    for i, (agent_id, farm_size, farmer_type, district) in enumerate(precomputed_types_and_sizes):
        income_values = income_expenditure_dict[farmer_type]['income_values']
        expenditure_values = income_expenditure_dict[farmer_type]['expenditure_values']

        income = income_values[i % len(income_values)]
        base_expenditure = expenditure_values[i % len(expenditure_values)]

        fixed_attributes[i] = {
            'farm_size': farm_size,
            'farmer_type': farmer_type,
            'district': district,
            'initial_money': income,
            'base_expenditure': base_expenditure
        }

    return fixed_attributes

fixed_farmers = initialize_fixed_farmers(precomputed_types_and_sizes)

# Visualization helper
COLORS = {
    "Marginal": "#1f77b4",
    "Small": "#2ca02c",
    "Semi-medium": "#ff7f0e",
    "Medium": "#d62728",
    "Large": "#9467bd"
}

# def visualize_farmer_grid(grid_map, width, height, filename="farmer_grid_visualization.pdf"):
#     fig, ax = plt.subplots(figsize=(width/5, height/5))
#     ax.set_xlim(0, width)
#     ax.set_ylim(0, height)
#     ax.set_xticks([])
#     ax.set_yticks([])
    
#     for (x, y), ftype in grid_map.items():
#         rect = plt.Rectangle((x, y), 1, 1, color=COLORS.get(ftype, 'grey'))
#         ax.add_patch(rect)

#     # Legend
#     patches = [mpatches.Patch(color=color, label=label) for label, color in COLORS.items()]
#     ax.legend(handles=patches, bbox_to_anchor=(1.05, 1), loc='upper left')
#     ax.set_aspect('equal')
#     plt.tight_layout()
#     plt.savefig(filename)
#     plt.close()

FARMER_TYPE_ORDER = {
    "Large": 0,
    "Medium": 1,
    "Semi-medium": 2,
    "Small": 3,
    "Marginal": 4
}

# Main FarmingModel
class FarmingModel(Model):
    def __init__(self, fixed_farmers):
        super().__init__()
        self.fixed_farmers = fixed_farmers
        self.num_farmers = len(fixed_farmers)
        self.avg_neighbours = 4
        self.year = ModelParameters.initial_year
        self.run_length = ModelParameters.run_length
        self.schedule = BaseScheduler(self)
        self.current_step = 0
        self.minimum_cropable_area = 0.4
        self.crops_per_farmer_coefficient = 2
        self.jgls = []
        self.profit_df = pd.read_csv("../crop_profitability.csv")

        # Sort agents by (district, farmer_type)
        sorted_agents = sorted(
         fixed_farmers.items(),
         key=lambda x: (
         x[1]['district'],
         FARMER_TYPE_ORDER.get(x[1]['farmer_type'], 99)
         )
        )
        self.sorted_agents = sorted_agents

        district_groups = defaultdict(list)
        for i, (idx, attr) in enumerate(sorted_agents):
            key = attr['district']
            district_groups[key].append((idx, attr))

        
        # 1. Precompute agent positions and determine grid size
        agent_positions = {}  # agent_id: (x, y)
        grid_map = {}

        district_list = list(district_groups.keys())
        x_offsets = {}
        farmers_per_row = math.ceil(math.sqrt(self.num_farmers))
        district_spacing = 2

        x_cursor = 0
        for d_index, district in enumerate(district_list):
         x_offsets[district] = x_cursor
         x_cursor += farmers_per_row + district_spacing

        max_x = 0
        max_y = 0
        y_cursor = 0

        for district in district_list:
          x_offset = x_offsets[district]
          col = 0
          row = 0
          district_farmers = district_groups[district]

          for idx, attr in district_farmers:
           x = x_offset + col
           y = y_cursor + row
           agent_positions[idx] = (x, y)
           grid_map[(x, y)] = attr['farmer_type']

           col += 1
           if col >= farmers_per_row:
            col = 0
            row += 1

           max_x = max(max_x, x)
           max_y = max(max_y, y)

          y_cursor = max_y + 2  # Add vertical space between districts

        # 2. Set correct grid size based on final max_x and max_y
        self.width = max_x + 1
        self.height = max_y + 1
        self.grid = SingleGrid(self.width, self.height, torus=False)

        print(f"Grid size: {self.width}x{self.height}")

        # 3. Create agents and place them using safe positions
        for idx, attr in sorted_agents:
         farmland = Farmland(size=attr['farm_size'], district=attr['district'], model=self)
         farmer = Farmer(
          unique_id=self.next_id(),
          model=self,
          type=attr['farmer_type'],
          farmer_size=attr['farm_size'],
          district=attr['district'],
          farmland=farmland,
          initial_money=attr['initial_money'],
          base_expenditure=attr['base_expenditure']
         )
         self.schedule.add(farmer)

         pos = agent_positions[idx]
         self.grid.place_agent(farmer, pos)

        # visualize_farmer_grid(grid_map, self.width, self.height)

        
        # Link neighbors
        total_neighbours = 0  # add this before the loop
        for farmer in self.schedule.agents:
            farmer.neighbours = self.grid.get_neighbors(farmer.pos, moore=True)
            total_neighbours += len(farmer.neighbours)


        # Assign crops to parcels
        avg_parcels = np.mean([farmer.farmland.n_parcels for farmer in self.schedule.agents])
        for farmer in self.schedule.agents:
            farmland = farmer.farmland
            max_crops = max(1, farmland.n_parcels)
            n_crops = min(max_crops, 1 + np.random.poisson(self.crops_per_farmer_coefficient * farmland.n_parcels / avg_parcels))
            crop_dict = get_crop_dict(n_crops=n_crops, n_parcels=farmland.n_parcels)
            flat_crops = itertools.chain.from_iterable([crop] * n for crop, n in crop_dict.items())
            for parcel, crop in zip(farmland.parcels, flat_crops):
                parcel.crop = crop

        self.datacollector = DataCollector(
            model_reporters={"Number of crops": calculate_number_of_crops},
            agent_reporters={
                "Money": "money",
                "Liquid_asset": "real_money",
                "Wealth": "value",
                "Networth": "worth",
                "Initial_income": "initial_money",
                "Income": "income",
                "Expenditure": "cost_of_living",
                "Farmer_type": "type",
                "farm_size": "farmer_size",
                "District": "district",
                "Total Cultivation Cost": "total_cultivation_cost",
                "Crop_Data": lambda a: a.get_previous_crop_data(),
                "Crop_Income": "crop_income",
                "Crop_Profit": "crop_profit",
                "Total Profit": "total_profit",
                "Neighbor_IDs": lambda a: [n.unique_id for n in a.neighbours],
                "Loaned": "loaned",
                "Total Debt": "total_debt",
                "neighbour_loan": "neighbour_loan_due",
                "collateral_loan": "collateral_loan_due",
                "income_financing": "income_financing_loan_due",
                "jgl_loan": "jgl_loan_due",
                "JGL_others_loan": "JGL_others_loan",
                "Years in debt": "years_in_debt",
                "Years in increasing debt": "years_in_increasing_debt",
                "Available Collateral": lambda a: max(0, a.land_value * 0.6 - a.collateral_used),
                "Loan Repaid": lambda a: getattr(a, "loan_repaid", 0)

            }
        )

    def step(self):
        self.schedule.step()
        self.year += 1
        self.current_step += 1
        self.datacollector.collect(self)

    def assign_to_jgl(self, farmer):
        if self.random.random() < ModelParameters.jgl_membership[farmer.type]:
            district_type_jgls = [jgl for jgl in self.jgls if jgl.district == farmer.district and jgl.type == farmer.type]
            if not district_type_jgls or len(district_type_jgls[-1].members) >= district_type_jgls[-1].max_size:
                jgl = JGL(max_size=self.random.randint(4, 10), type=farmer.type, district=farmer.district)
                self.jgls.append(jgl)
            else:
                jgl = district_type_jgls[-1]

            jgl.members.append(farmer)
            farmer.jgl = jgl
            if not hasattr(self, 'jgl_assignments'):
                self.jgl_assignments = {}
            self.jgl_assignments[farmer] = jgl

# Run the model (example run)

# Step 1: Set up the start time for tracking runtime
start_time = time.time()

# Initialize fixed attributes for each agent once to keep it consistent across runs
# fixed_farmers = initialize_fixed_farmers(ModelParameters.num_farmers, precomputed_types_and_sizes, ModelParameters.run_length)
fixed_farmers = initialize_fixed_farmers(precomputed_types_and_sizes)

# Loop to run the model 10 times
for run_number in range(1, 6):  # Run the model 5 times
    # Instantiate the model with fixed farmer attributes
    model = FarmingModel(fixed_farmers=fixed_farmers)
    
    # lookup_table_yield_filename = f"lookup_table_yield_S23_{run_number:02d}.csv"
    # model.lookup_table_yield.to_csv(lookup_table_yield_filename, index=True)
    # print(f"Saved lookup_table_yield as {lookup_table_yield_filename}")

    for step in range(ModelParameters.run_length):
        model.step()
        print(f"Year {model.year}")

    # Get and save Agent Wealth data
    agent_wealth = model.datacollector.get_agent_vars_dataframe().reset_index()
    agent_wealth.rename(columns={
        'Total Debt': 'Total Debt',
        'Years in debt': 'Years in debt',
        'Years in increasing debt': 'Years in increasing debt'
    }, inplace=True)

    wealth_filename = f"agent_wealth_debt_over_time_S05_{run_number:02d}.csv"
    agent_wealth.to_csv(wealth_filename, index=False)
    
    print(f"Run {run_number}: Data saved as {wealth_filename}")

# Step 2: Track and print total runtime
end_time = time.time()
total_time_minutes = (end_time - start_time) / 60
print(f"Total time taken for 10 runs: {total_time_minutes:.2f} minutes")
