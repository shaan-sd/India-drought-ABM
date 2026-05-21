import pandas as pd
import numpy as np
import itertools
import math
import time 
import random
import multiprocessing as mp


from mesa import Model
from mesa.time import BaseScheduler
from mesa.datacollection import DataCollector
from mesa.space import SingleGrid

from agents import Farmer
from objects import Farmland, JGL
from data import ModelParameters, calculate_number_of_crops, get_farm_size, get_crop_dict, classify_size


crop_df_local = pd.read_csv('../analysis/crops/agg_data.csv', index_col=0)

# Load the CSV file
initial_income_cost_of_living = pd.read_csv("../initial_income_cost_of_living.csv")
initial_income_cost_of_living.reset_index(drop=True, inplace=True)

# Assigning same farmer type, initial income and same expenditure values across runs


def precompute_farm_types_and_sizes():
    """Load precomputed farm sizes from CSV and classify farmer types."""
    farm_df = pd.read_csv("../precomputed_farm_sizes.csv")
    farm_df["Farmer_Type"] = farm_df["Farm_Size"].apply(classify_size)
    return list(farm_df.itertuples(index=False, name=None))

precomputed_types_and_sizes = precompute_farm_types_and_sizes()

def initialize_fixed_farmers(num_farmers, precomputed_types_and_sizes):
    """ Initialize farmers with fixed attributes including precomputed cost of living for each step. """
    fixed_attributes = {}

    # Precompute income and expenditure lookup
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

    # Initialize farmers list
    farmers = []

    for i in range(num_farmers):
        _, farm_size, farmer_type = precomputed_types_and_sizes[i]  # Unpack correctly

        # Fetch income and expenditure values
        income_values = income_expenditure_dict[farmer_type]['income_values']
        expenditure_values = income_expenditure_dict[farmer_type]['expenditure_values']

        income = income_values[i % len(income_values)]  # Assign fixed income
        base_expenditure = expenditure_values[i % len(expenditure_values)]  # Assign fixed cost of living

        # Store fixed attributes
        fixed_attributes[i] = {
            'farm_size': farm_size,
            'farmer_type': farmer_type,
            'initial_money': income,
            'base_expenditure': base_expenditure
        }

    return fixed_attributes

# Initialize fixed farmers with precomputed expenditure for each step
fixed_farmers = initialize_fixed_farmers(ModelParameters.num_farmers, precomputed_types_and_sizes)


# Update FarmingModel to use fixed attributes
class FarmingModel(Model):
    def __init__(self, N=ModelParameters.num_farmers, fixed_farmers=None):
        # Ensure fixed_farmers is provided
        if fixed_farmers is None:
            raise ValueError("fixed_farmers cannot be None. Please provide initialized farmer attributes.")
        
        super().__init__()
        self.num_farmers = N
        self.avg_neighbours = 4  # Max 8
        self.year: int = ModelParameters.initial_year
        self.run_length: int = ModelParameters.run_length
        self.start_year = ModelParameters.initial_year
        self.schedule = BaseScheduler(self)  # Use stage scheduler
        self.current_id: int = 0
        self.current_step = 0  # Initialize step tracking

        self.COST_OF_CULTIVATION = {
         'Rice': 92006.24,
         'Sorghum': 60413.47,
         'Maize': 60413.47,
         'Finger millet': 79299.73,
         'Chickpea': 40870.34,
         'Pigeonpea': 45957.39,
         'Groundnut': 67803.33
        }


        self.minimum_cropable_area = 0.4  # in ha (1 Acre)
        self.crops_per_farmer_coefficient = 2  
        self.districts = ModelParameters.districts

        self.initial_money_list = []  # To store initial money for all farmers
        self.cost_of_living_list = []  # To store cost of living for all farmers 

        self.jgls = []  # List to store JLGs
        
        self.profit_df = pd.read_csv("../crop_profitability.csv") 

        # Calculate total grid size
        grid_size = self.num_farmers * 8 // self.avg_neighbours
        district_size = int((grid_size - 4) // 5)  # Approximate district size
        district_dim = int(district_size**0.5)  # District grid dimensions
        self.height = 5 * district_dim + 4  
        self.width = district_dim  
        while (self.height - 4) * self.width < self.num_farmers:
            self.width += 1

        self.grid = SingleGrid(self.width, self.height, torus=False)
        print(f"Created a grid of size {self.width}x{self.height} with {self.num_farmers} farmers")
        
        for i in range(self.num_farmers):
            district = i // (self.num_farmers // 5)  # Assign district based on index
            attributes = fixed_farmers[i]  # Retrieve fixed attributes

            
            farmland = Farmland(size=attributes['farm_size'], district=self.districts[i % 5], model= self)


            farmer = Farmer(
             unique_id=self.next_id(),
             model=self,
             type=attributes['farmer_type'],
             farmer_size=attributes['farm_size'],
             district=self.districts[i % 5],
             farmland=farmland,
             initial_money=attributes['initial_money'],
             base_expenditure=attributes['base_expenditure']  # Pass all precomputed values
            )

            self.schedule.add(farmer)


        # Initialize the base Model class
       
          # Compute district to place the agent
            min_row = district_dim * district + district    # Min row for this district
            max_row = min_row + district_dim - 1            # Max row for this district


            viable_positions = sorted([
            pos for pos in self.grid.empties if min_row <= pos[1] <= max_row])
            pos = viable_positions[i % len(viable_positions)]  # Deterministic choice
            self.grid.place_agent(farmer, pos)
          
            
        total_neighbours = 0
        for farmer in self.schedule.agents:
            farmer.neighbours = self.grid.get_neighbors(farmer.pos, moore=True)
            total_neighbours += len(farmer.neighbours)
        print(f"Average number of neighbours: {total_neighbours / self.num_farmers}")

        average_number_of_farmland_parcels = np.mean([farmer.farmland.n_parcels for farmer in self.schedule.agents])
        for farmer in self.schedule.agents:
            # Determine the number of crops and how many of each crop
            farmland = farmer.farmland
            max_crops = min(1, farmland.n_parcels)
            n_crops = min(max_crops, 1 + np.random.poisson(self.crops_per_farmer_coefficient * farmland.n_parcels / average_number_of_farmland_parcels))
            crop_dict = get_crop_dict(n_crops=n_crops, n_parcels=farmland.n_parcels)
            # Assign crops to parcels
            flat_crops = itertools.chain.from_iterable([crop] * n for crop, n in crop_dict.items())
            for parcel, crop in zip(farmland.parcels, flat_crops):
                parcel.crop = crop
            

        # Add data collector.
        self.datacollector = DataCollector(
          model_reporters={
           "Number of crops": calculate_number_of_crops
          },
        agent_reporters = {
            "Money": "money",
            "Liquid_asset" : "real_money",
            "Wealth":"value",
            "Networth":"worth",
            "Initial_income":"initial_money",
            "Income":"income",
            "Expenditure":"cost_of_living",
            "Farmer_type":"type",
            "farm_size": "farmer_size",
            "District": "district",
            "Total Cultivation Cost": "total_cultivation_cost",
            "Crop_Data": lambda a: a.get_previous_crop_data(),
            "Crop_Income": "crop_income",
            "Crop_Profit":"crop_profit",
            "Total Profit": "total_profit",
            "Neighbor_IDs": lambda a: [neighbor.unique_id for neighbor in a.neighbours],
            "Loaned":"loaned",
            "Total Debt": "total_debt",
            "neighbour_loan": "neighbour_loan_due",
            "collateral_loan": "collateral_loan_due",
            "income_financing": "income_financing_loan_due",
            "jgl_loan": "jgl_loan_due",
            "JGL_others_loan": "JGL_others_loan",
            "Years in debt": "years_in_debt",
            "Years in increasing debt": "years_in_increasing_debt",
            "Available Collateral": lambda a: max(0, a.land_value * 0.6 - a.collateral_used)
          }
        )
        
      
    
    def step(self):
        self.schedule.step()
        self.year += 1
        self.current_step += 1  # Increment the step count
        self.datacollector.collect(self)  # TODO: Only collect data after certain year (warmup)


    def assign_to_jgl(self, farmer):
        # TODO: Improve selection of joining JGL. Base one if collateral is maxed out
        if self.random.random() < ModelParameters.jgl_membership[farmer.type]:
            # Filter JGLs by district and type
            district_type_jgls = [jgl for jgl in self.jgls if jgl.district == farmer.district and jgl.type == farmer.type]
            if not district_type_jgls or len(district_type_jgls[-1].members) >= district_type_jgls[-1].max_size:
                # Create new JGL if none exist or the last one is full
                jgl = JGL(max_size=self.random.randint(4, 10), type=farmer.type, district=farmer.district)
                self.jgls.append(jgl)
            else:
                # Add to the last JGL
                jgl = district_type_jgls[-1]

            # Now jgl is guaranteed to be initialized, so we can add the farmer
            jgl.members.append(farmer)
            farmer.jgl = jgl
    
            # Initialize the dictionary if not already done
            if not hasattr(self, 'jgl_assignments'):
               self.jgl_assignments = {}  # Initialize the tracking dictionary if needed

            # Track the JGL assignment for this farmer
            self.jgl_assignments[farmer] = jgl 


# # Running the model

# Step 1: Set up the start time for tracking runtime
start_time = time.time()

# Initialize fixed attributes for each agent once to keep it consistent across runs
# fixed_farmers = initialize_fixed_farmers(ModelParameters.num_farmers, precomputed_types_and_sizes, ModelParameters.run_length)
fixed_farmers = initialize_fixed_farmers(ModelParameters.num_farmers, precomputed_types_and_sizes)

# Loop to run the model 10 times
for run_number in range(1, 2):  # Run the model 5 times
    # Instantiate the model with fixed farmer attributes
    model = FarmingModel(N=ModelParameters.num_farmers, fixed_farmers=fixed_farmers)
    
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


# Note: S11 - Combined lending with best crop replacement in market

