from dataclasses import dataclass
# from sklearn.linear_model import LinearRegression
# from sklearn.metrics import r2_score
# import statsmodels.api as sm
# import statsmodels.formula.api as smf
import pandas as pd
import numpy as np
# from random import gauss
from scipy.stats import lognorm
from collections import Counter
import random
import pickle 

# Fetching crops for model based on the area grown
crop_df_local = pd.read_csv('../analysis/crops/agg_data.csv', index_col=0)
p = crop_df_local["Area (1000 ha)"].fillna(0)
p = p.sort_values(ascending=False)[:7]  # Keep the 2 highest area crops

farm_df_local = pd.read_csv('../Data/Farmland/farmland_clean.csv', index_col=0)
land_value_df_local = pd.read_csv('../Data/csv_data/land_value.csv', index_col=0)

# for assigning farm area to different farmer groups on an average
farm_df_local = pd.read_csv('../Data/Farmland/farmland_clean.csv', index_col=0) 

# land_value_df_local = pd.read_csv('../Data/csv_data/land_value.csv', index_col=0)

lookup_table_yield = pd.read_pickle('../Data/csv_data/lookup_table_yield.pkl')
residuals = pd.read_pickle('../Data/csv_data/yield_residuals_dict.pkl')

# Assign probabilities of each crop to be planted across
crop_probabilities = p / p.sum()

# Convert crop names to uppercase
upper_crops = p.index.str.upper()

@dataclass
class ModelParameters:
    num_farmers: int = 1000
    initial_year: int = 2017
    run_length: int = 30
    crop_list = p.index.tolist()  # Make sure p is defined elsewhere
    # crop_df = crop_df_local  # Ensure these variables are defined in the appropriate scope
    farm_df = farm_df_local
    # land_value_df = land_value_df_local
    districts = ["Chitradurga", "Bellary", "Davanagere", "Haveri", "Gadag"]
    size_classes = ['Marginal', 'Small', 'Semi-medium', 'Medium', 'Large']
    jgl_membership = {size: membership for size, membership in zip(size_classes, [0.4, 0.4, 0.4, 0.4, 0.1])}

    



def get_weighted_crop_choice():
    # Create a list of crop percentages, to draw from. Do this based on the Area (1000 ha) column, which contains absolute numbers.
    # Generate a weighted random choice:
    random_crop = np.random.choice(crop_probabilities.index, p=crop_probabilities)
    return random_crop

def get_crop_dict(n_crops, n_parcels):
    # Create a dictionary of crops, with the number of parcels of land for each crop.
    crops = np.random.choice(crop_probabilities.index, size=n_crops, replace=False)
    crop_dict = {}
    # Select crops from crop_probabilities, and normalize the probabilities to 1.
    new_crop_probabilities = crop_probabilities[crops] / crop_probabilities[crops].sum()
    for _ in range(n_parcels):
        crop = np.random.choice(crops, p=new_crop_probabilities.values)
        if crop in crop_dict:
            crop_dict[crop] += 1
        else:
            crop_dict[crop] = 1
    return crop_dict

# def get_farm_size(shape=0.92, loc=0, scale=1.25):
#     # Drawing the number from lognormal distribution, capping at 1000, and classifying it.
#     # farm_size = min(lognorm.rvs(shape, scale), 1000)
#     farm_size = min(lognorm.rvs(shape, loc, scale), 20)
#     farm_class = classify_size(farm_size)
#     return farm_size, farm_class

# def get_farm_size(shape=0.92, loc=0, scale=1.25):
#     farm_size = min(lognorm.rvs(shape, loc, scale),1000)  # Generate farm size
#     farm_size = min(farm_size, 15)  # Cap at 20 ha
#     farm_class = classify_size(farm_size)
#     return farm_size, farm_class


# def classify_size(size):
#     # Define the boundaries and the labels
#     boundaries = [0, 1, 2, 4, 10, np.inf]
#     # Get the index of the bin that the farm size falls into
#     bin_index = np.digitize(size, boundaries) - 1
#     # Return the appropriate label
#     return ModelParameters.size_classes[bin_index]


def calculate_number_of_crops(model):
    # Create a Counter with the number of farmers that have a certain number of crops.
    number_of_crops = [len(farmer.farmland.crop_counter) for farmer in model.schedule.agents]
    return Counter(number_of_crops)

# def predicted_crop_prices(model):
#     for district in ModelParameters.districts:
#         crop_prices = pd.read_csv(f"../Data/crops/district_wise_prices/{district}_predicted_prices.csv", index_col=0)
#         model.predicted_crop_prices[district] = crop_prices
        

# Load precomputed farm sizes once when the module is imported
df_farm_sizes = pd.read_csv("../precomputed_farm_sizes.csv")
df_farm_sizes.set_index("AgentID", inplace=True)  # Set AgentID as index for quick lookup

# Define size classification boundaries
SIZE_CLASSES = ["Marginal", "Small", "Semi-medium", "Medium", "Large"]
BOUNDARIES = [0, 1, 2, 4, 10, np.inf]

def classify_size(size):
    """Classify farm size into predefined categories."""
    bin_index = np.digitize(size, BOUNDARIES) - 1
    return SIZE_CLASSES[bin_index]

def get_farm_size(agent_id):
    """Retrieve fixed farm size and farmer type from precomputed data."""
    if agent_id in df_farm_sizes.index:
        farm_size = df_farm_sizes.loc[agent_id, "Farm_Size"]
        farmer_type = classify_size(farm_size)
        return farm_size, farmer_type
    else:
        raise ValueError(f"AgentID {agent_id} not found in precomputed farm size data!")

