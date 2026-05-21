import random
from mesa import Agent
from collections import Counter
import pandas as pd
import numpy as np
from objects import Farmland, Loan, JGL
from data import ModelParameters, get_weighted_crop_choice

# COST_OF_CULTIVATION = {
#     "Rice": 51880.26,
#     "Sorghum": 38950.22,
#     "Maize": 38950.22,
#     "Finger millet": 47083.95,
#     "Chickpea": 22774.72,
#     "Pigeonpea": 27962.05,
#     "Groundnut": 40308.92
# }

COST_OF_CULTIVATION = {
        'Rice': 92006.24,
        'Sorghum': 60413.47,
        'Maize': 60413.47,
        'Finger millet': 79299.73,
        'Chickpea': 40870.34,
        'Pigeonpea': 45957.39,
        'Groundnut': 67803.33
}

# profit_df = pd.read_csv("../crop_profitability.csv")

# Agent description
class Farmer(Agent):
    def __init__(self, unique_id, model, type, district, farmland, farmer_size, initial_money, base_expenditure, jlg=True):

        super().__init__(unique_id, model)        
        self.type = type
        self.district = district
        self.farmland = farmland
        self.farmer_size = farmer_size
        self.land_value = 5000000 * self.farmland.size
        self.money = 200000
        self.real_money = 200000

        self.income = 0
        self.value = 0
        self.worth = 0
        self.initial_money = 200000

        # self.base_expenditure = base_expenditure  # Start with step 0 value
        # self.cost_of_living = base_expenditure  # Initialize cost_of_living
        
        self.cost_of_living = 100000
        
        self.neighbours = []
        self.years_in_increasing_negative_wealth = 0
        self.years_in_negative_wealth = 0
        # self.crop_income = {}
        # self.crop_profit = {}
        self.total_profit = 0

        self.jgl = not None
        self.in_JLG = False
        self.jlg = jlg
        self.loan_start_step = None
        self.previous_total_debt = 0
        self.due = False
        self.loans = []
        self.years_in_debt = 0
        self.years_in_increasing_debt = 0
        self.total_debt = 0

        self.will_lend = True

        self.collateral_used = 0  # Track total collateral currently in use

        # Track loans
        self.neighbour_loan_due = 0
        self.collateral_loan_due = 0
        self.income_financing_loan_due = 0
        self.jgl_loan_due = 0
        self.JGL_others_loan = 0

        self.loaned = 0
        

        self.previous_crop_data = {}

        self.pos = None



    def step(self):
        """ Update cost_of_living at each step using precomputed values. """

        self.cost_of_living = 100000

        # if self.farmer_size in ["Marginal", "Small"]:
        #   self.real_money += 80000
        
        self.crop_data = self.get_crop_data()  
        self.previous_crop_data = self.get_crop_data()  # Save data BEFORE updating crops

        value_last_year = self.value
        self.money -= self.cost_of_living
        
        for loan in self.loans:
            loan.update()

        self.harvest_crops()
        

        # Step 1: Compute cultivation cost BEFORE planting crops
        profit_df = self.model.profit_df
        profit_df = profit_df[(profit_df["Year"] == self.model.year) & (profit_df["District"] == self.district)]

        total_cultivation_cost = 0

        for parcel in self.farmland.parcels:
         if parcel.crop in profit_df["Crop"].values:
          crop_row = profit_df[profit_df["Crop"] == parcel.crop].iloc[0]
          if crop_row["Profit"] >= 0:
            total_cultivation_cost += crop_row["Cost"] * parcel.size
          else:
            total_cultivation_cost += 0  # Optional, explicit for clarity

        self.total_cultivation_cost = total_cultivation_cost



        # Attempt to borrow money
        if self.money < self.cost_of_living and self.money > 0:
         self.borrow_money(amount_to_borrow=self.cost_of_living - self.money)
        elif self.money < 0:
         self.borrow_money(amount_to_borrow=self.cost_of_living)

        if self.money < total_cultivation_cost and self.money > 0:
         self.borrow_money(amount_to_borrow=total_cultivation_cost - self.money)
        elif self.money < 0:
         self.borrow_money(amount_to_borrow=total_cultivation_cost)

        
        self.money -= self.total_cultivation_cost

        # Harvest crops and compute income

        if self.money < self.cost_of_living and self.money > 0:
        # Borrow just enough to cover the difference
          self.borrow_money(amount_to_borrow=self.cost_of_living - self.money)
        elif self.money < 0:
        # Borrow enough to cover both the negative balance and cost of living
           self.borrow_money(amount_to_borrow=self.cost_of_living)

        if self.money < self.total_cultivation_cost and self.money > 0:
        # Borrow just enough to cover the difference
          self.borrow_money(amount_to_borrow=self.total_cultivation_cost - self.money)
        elif self.money < 0:
        # Borrow enough to cover both the negative balance and cost of living
           self.borrow_money(amount_to_borrow=self.total_cultivation_cost)
           

        self.pay_back_loans()

        self.total_debt = self.jgl_loan_due + self.neighbour_loan_due + self.collateral_loan_due + self.income_financing_loan_due

        # Update value by subtracting loans from money, and debt status
        self.value = self.money + self.land_value - self.total_debt

        
        self.real_money += self.income 
        self.real_money -= self.cost_of_living 
        self.real_money -= self.total_cultivation_cost 
        self.real_money -= self.total_debt

        self.worth = self.real_money + (self.land_value * 0.6 - self.collateral_used) - self.total_debt


        # self.real_money = self.money + self.loaned - (2 * self.total_debt)


        if self.value < 0:
            self.years_in_negative_wealth += 1
            if self.value < value_last_year:
                self.years_in_increasing_negative_wealth += 1
            else:
                self.years_in_increasing_negative_wealth = 0
        else:
            self.years_in_negative_wealth = 0

         
        if self.total_debt > 0:
          self.years_in_debt += 1  # Increase debt duration count
          if self.total_debt > self.previous_total_debt:
           self.years_in_increasing_debt += 1  # Track increasing debt
        elif self.total_debt < self.previous_total_debt:
          self.years_in_increasing_debt = 0  # Reset only if debt decreases   
        else:
          self.years_in_debt = 0  # Reset only if fully out of debt
          self.years_in_increasing_debt = 0  # Reset if debt stops increasing

        # Update the previous total debt for comparison in the next step
        self.previous_total_debt = self.total_debt

        self.plant_crops()

    
    @property
    def loan_access(self):
     # Example: Loan access could be a function of money or other attributes
     return max(0, self.value * 0.6) 
    
    @property
    def poverty_trap(self):
     """Check if the farmer is in a poverty trap based on real money."""
     return self.real_money < 120000

    @property
    def financial_resilience(self):
     """Check if total debt exceeds or equals total worth."""
     if self.worth == 0:
        return True  # or True if you treat zero worth as no resilience
     return (self.total_debt / self.worth) >= 0.75


    def harvest_crops(self):
     total_profit, crop_income, crop_profit, income = self.farmland.harvest(year=self.model.year)     
     self.income = income
     self.crop_income = crop_income  # Store the crop income dictionary for this step
     self.crop_profit = crop_profit
     self.total_profit = total_profit
     self.money += income

    def get_previous_crop_data(self):
        return self.previous_crop_data
 
    def get_crop_data(self):
    # Dictionary to store crop data: {crop_name: {"count": X, "planting_years": [Y1, Y2, ...]}}
    
        # Returns a dictionary with the crop name and the number of parcels for each crop
        crop_counts = {}
        for parcel in self.farmland.parcels:
            crop_name = parcel.crop
            if crop_name:
                crop_counts[crop_name] = crop_counts.get(crop_name, 0) + 1
        return crop_counts

    # def plant_crops(self):
    #  """Assign crops to parcels based on profit from crop_profitability.csv."""
    #  current_year = self.model.year  # Get the current simulation year

    #  # Step 1: Read profit data from crop_profitability.csv
    #  try:
    #   profit_df = self.model.profit_df
    #   next_year = current_year + 1  # NEW: Use next year instead
    #   profit_df = profit_df[
    #     (profit_df["Year"] == next_year) & 
    #     (profit_df["District"] == self.district)]
    # #  try:
    # #     profit_df = self.model.profit_df
    # #     # profit_df = pd.read_csv("../crop_profitability.csv")
    # #     profit_df = profit_df[(profit_df["Year"] == current_year) & (profit_df["District"] == self.district)]
    #  except FileNotFoundError:
    #     print("Error: crop_profitability.csv not found.")
    #     return
    #  except Exception as e:
    #     print(f"Error reading crop profitability file: {e}")
    #     return

    #  if profit_df.empty:
    #     return  # No profit data available for this district & year

    #  # Convert to dictionary for quick lookup
    #  profit_dict = dict(zip(profit_df["Crop"], profit_df["Profit"]))

    # # Get current crops on the farmland
    # #  current_crops = list(Counter(parcel.crop for parcel in self.farmland.parcels).keys())

    # #  # Step 1: Identify popular crops among neighbors
    # #  crop_counts = Counter()
    # #  for neighbour in self.neighbours:
    # #     neighbour_crops = set(parcel.crop for parcel in neighbour.farmland.parcels)
    # #     crop_counts.update(neighbour_crops)

    # #  # Potential crops: At least in 1/3 of neighbors' farmland
    # #  potential_crops = [crop for crop, count in crop_counts.items() if count >= len(self.neighbours) / 3]


    #  # Step 2: Identify potential crops (those with positive profitability)
    #  potential_crops = [crop for crop, profit in profit_dict.items() if profit > 0 and crop != "Arecanut"]

    #  if not potential_crops:
    #     return  # No viable crops to plant

    #  # Step 3: Select the most profitable crop
    #  best_crop = max(potential_crops, key=lambda crop: profit_dict[crop])

    #  # Step 4: Identify the worst crop to replace (least profitable among current crops)
    #  current_crops = list(Counter(parcel.crop for parcel in self.farmland.parcels).keys())
    #  worst_crop = min(current_crops, key=lambda crop: profit_dict.get(crop, float("-inf")), default=None)

    #  replace_pref = [worst_crop] if worst_crop else current_crops

    #  # Step 5: Plant the selected crop
    #  self.farmland.plant(best_crop, n_parcels=1, replace_pref=replace_pref)
    


    
    
    def plant_crops(self):
     """Assign crops to parcels based on computed profit using past 3 years' predicted yield and price."""
     current_year = self.model.year

     try:
        df = self.model.profit_df  # This should be the full DataFrame you pasted

        # Step 1: Filter for the last 3 years or fewer (>= 1)
        years_to_include = [y for y in range(current_year - 3 + 1, current_year + 1) if y >= 1]
        df_filtered = df[
            (df["Year"].isin(years_to_include)) &
            (df["District"] == self.district)
        ]

     except Exception as e:
        print(f"Error processing profitability data: {e}")
        return

     if df_filtered.empty:
        return

     # Step 2: Compute average yield, price, and cost for each crop
     crop_profits = {}
     grouped = df_filtered.groupby("Crop")

     for crop, group in grouped:
      avg_revenue = group["Revenue"].mean()
      avg_cost = group["Cost"].mean()

      profit = (2 * avg_revenue) - avg_cost
      crop_profits[crop] = profit
     
    #  crop_profits = {}
    #  grouped = df_filtered.groupby("Crop")

    #  for crop, group in grouped:
    #     if crop not in df.columns:
    #         continue  # Skip if price column is missing

    #     avg_yield = group["Predicted_Yield"].mean()
    #     avg_price = group[crop].mean()
    #     avg_cost = group["Cost"].mean()

    #     revenue = avg_yield * avg_price * 100
    #     profit = (2 * revenue) - avg_cost
    #     crop_profits[crop] = profit

     # Step 3: Select viable crops with positive profit (excluding Arecanut)
     potential_crops = [crop for crop, profit in crop_profits.items() if profit > 0 and crop != "Arecanut"]
     if not potential_crops:
        return

     # Step 4: Choose most profitable crop
     best_crop = max(potential_crops, key=lambda crop: crop_profits[crop])

     # Step 5: Choose worst current crop to replace
     current_crops = list(Counter(parcel.crop for parcel in self.farmland.parcels).keys())
     worst_crop = min(current_crops, key=lambda crop: crop_profits.get(crop, float("-inf")), default=None)
     replace_pref = [worst_crop] if worst_crop else current_crops

     # Step 6: Plant the best crop on 1 parcel
     self.farmland.plant(best_crop, n_parcels=1, replace_pref=replace_pref)



    def check_due(self, current_year):
        # Mark the loan as due if the current year exceeds the loan duration
        if current_year >= self.borrower.year + self.duration:
            self.due = True
    
    def work(self):
        # Let days of work depend on income-expenditure, loans and current money
        pass


    
    def borrow_money(self, amount_to_borrow):
        """ Farmer borrows money based on priority: neighbours, collateral, income financing, and JLG. """
        borrowed = 0
        duration = 1
        
        # First: Borrow from neighbours based on trust
        for neighbour in sorted(self.neighbours, key=lambda x: x.money, reverse=True):
            if neighbour.will_lend and neighbour.money > 0:

                max_willing_to_borrow = max(0, neighbour.money - neighbour.cost_of_living * 1.5)
                amount_to_borrow_now = min(amount_to_borrow, max_willing_to_borrow)
                # amount_to_borrow_now = 0
                
                if amount_to_borrow_now > 0:
                    interest_rate = 0
                    loan = Loan(amount_to_borrow_now, interest_rate, duration, self, loan_type="neighbour")
                    
                    neighbour.money -= amount_to_borrow_now
                    neighbour.loaned += amount_to_borrow_now

                    self.money += amount_to_borrow_now
                    self.loans.append(loan)

                    # self.total_debt += amount_to_borrow_now
                    self.neighbour_loan_due += amount_to_borrow_now
                    borrowed += amount_to_borrow_now
                    
                    if borrowed >= amount_to_borrow:
                        return
        
        # Second: Borrow from bank using collateral
        max_collateral = self.land_value * 0.6
        available_collateral = max_collateral - sum(loan.collateral_used for loan in self.loans if hasattr(loan, 'collateral_used'))
        
        if available_collateral > 0:
            amount_to_borrow_now = min(amount_to_borrow, available_collateral)
            # amount_to_borrow_now = 0

            if amount_to_borrow_now > 0:
                interest_rate = 0.14
                duration = 3
                loan = Loan(amount_to_borrow_now, interest_rate, duration, self, loan_type="collateral", collateral_used=amount_to_borrow_now)
                
                self.money += amount_to_borrow_now
                self.loans.append(loan)

                self.collateral_used += amount_to_borrow_now  # Track total collateral used
                self.collateral_loan_due += amount_to_borrow_now
                borrowed += amount_to_borrow_now

                
                if borrowed >= amount_to_borrow:
                    return
        
        # Third: Borrow from bank using income financing
        income_financing = (self.income - self.cost_of_living) * 0.6
        if income_financing > 0:
            # amount_to_borrow_now = min(amount_to_borrow, income_financing)
            amount_to_borrow_now = 0

            if amount_to_borrow_now > 0:
                interest_rate = 0.18
                duration = 3
                loan = Loan(amount_to_borrow_now, interest_rate, duration, self, loan_type="income_financing_loan")
                
                self.money += amount_to_borrow_now
                self.loans.append(loan)
                self.income_financing_loan_due += amount_to_borrow_now
                borrowed += amount_to_borrow_now


                if borrowed >= amount_to_borrow:
                    return
        
        # Fourth: Borrow from JLG
        if self.jgl and self.jgl_loan_due == 0:
            amount_to_borrow_now = min(amount_to_borrow, 100000)  # Max 1 lakh
            # amount_to_borrow_now = 0

            if amount_to_borrow_now > 0:
                interest_rate = 0.24
                duration = 3
                loan = Loan(amount_to_borrow_now, interest_rate, duration, self, loan_type="jgl")
                
                self.money += amount_to_borrow_now
                self.loans.append(loan)
                self.jgl_loan_due += amount_to_borrow_now
                borrowed += amount_to_borrow_now
                

                if borrowed >= amount_to_borrow:
                    return
        

    def pay_back_loans(self):
     current_year = self.model.current_step

     # Sort loans by highest interest rate first, then smallest loan amount
     loans_to_pay_back = sorted(self.loans, key=lambda x: (-x.current_interest_rate, x.amount))

     total_repaid = 0 

     # Start repaying loans
     while self.money > 0 and loans_to_pay_back:
        loan = loans_to_pay_back.pop(0)
        amount_to_pay_back = min(self.money, loan.amount)

        # Pay back the loan
        loan.pay_back(amount_to_pay_back, current_year)
        self.money -= amount_to_pay_back

        total_repaid += amount_to_pay_back  # <-- Accumulate repayment


        # Reduce the correct loan type due
        if loan.loan_type == "neighbour":
            self.neighbour_loan_due -= amount_to_pay_back
            self.neighbour_loan_due = max(0, self.neighbour_loan_due)

        elif loan.loan_type == "collateral":
            self.collateral_loan_due -= amount_to_pay_back
            self.collateral_loan_due = max(0, self.collateral_loan_due)

        elif loan.loan_type == "income_financing_loan":
            self.income_financing_loan_due -= amount_to_pay_back
            self.income_financing_loan_due = max(0, self.income_financing_loan_due)

        elif loan.loan_type == "jgl":
            self.jgl_loan_due -= amount_to_pay_back
            self.jgl_loan_due = max(0, self.jgl_loan_due)

     self.loan_repaid = total_repaid






# def plant_crops(self):
#     """Assign crops to parcels based on profit and affordability, using 3-year history."""
#     current_year = self.model.year
#     years_to_include = [y for y in range(current_year - 2, current_year + 1) if y >= 1]
    
#     try:
#         df = self.model.profit_df
#         df_filtered = df[(df["Year"].isin(years_to_include)) & (df["District"] == self.district)]
#     except Exception as e:
#         print(f"Error processing profitability data: {e}")
#         return

#     if df_filtered.empty:
#         return

#     # Compute average profit and cost for each crop over the 3 years
#     crop_stats = {}
#     grouped = df_filtered.groupby("Crop")
#     for crop, group in grouped:
#         if crop == "Arecanut":
#             continue  # skip this crop
#         avg_profit = group["Profit"].mean()
#         avg_cost = group["Cost"].mean()
#         if avg_profit > 0:
#             crop_stats[crop] = (avg_profit, avg_cost)

#     if not crop_stats:
#         return  # no viable crop

#     # Step 1: Check if most profitable crop can be planted everywhere
#     best_crop = max(crop_stats.items(), key=lambda item: item[1][0])[0]
#     best_profit, best_cost = crop_stats[best_crop]

#     total_required = sum(parcel.size * best_cost for parcel in self.farmland.parcels)
#     if self.money >= total_required:
#         # Plant most profitable crop on all parcels
#         self.farmland.plant(best_crop, n_parcels=len(self.farmland.parcels), replace_pref=None)
#         self.money -= total_required
#         return

#     # Step 2: Else, sort crops by affordability and profit efficiency
#     crops_sorted = sorted(
#         crop_stats.items(),
#         key=lambda item: (item[1][1], -item[1][0] / item[1][1])  # sort by low cost, then profit-to-cost ratio
#     )

#     available_cash = max(self.money, 0)

#     for parcel in self.farmland.parcels:
#         planted = False
#         for crop, (profit, cost) in crops_sorted:
#             adjusted_cost = cost * parcel.size
#             if available_cash >= adjusted_cost:
#                 self.farmland.plant(crop, n_parcels=1, replace_pref=[parcel.crop])
#                 available_cash -= adjusted_cost
#                 planted = True
#                 break
#         if not planted:
#             # Last resort: plant cheapest viable crop
#             fallback_crop = min(crops_sorted, key=lambda item: item[1][1])[0]
#             self.farmland.plant(fallback_crop, n_parcels=1, replace_pref=[parcel.crop])
