from data import ModelParameters
from collections import Counter
import random
import pandas as pd

# profit_df = pd.read_csv("../crop_profitability.csv")

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


class Farmland:
    def __init__(self, size, district, model):
        self.size = size
        self.district = district
        self.n_parcels = self.determine_parcels(size)
        self.parcels = [Parcel(size / self.n_parcels) for _ in range(self.n_parcels)]
        self.crop_counter = Counter()
        self.model = model

    def determine_parcels(self, size):
        """Determine number of parcels based on farm size"""
        if size < 1:  # Very small farms
            return max(int(size / 0.4), 1)  # Minimum 1 parcel, 0.4 ha each
        
        elif 1 <= size < 3:  # Small farms
            return random.randint(3,3)  # 2-3 parcels
        
        elif 3 <= size < 10:  # Medium farms
            return random.randint(4,4)  # 4-5 parcels
        
        else:  # Very large farms
            return random.randint(5,5)  # 10-15 parcels
        

    def plant(self, new_crop, n_parcels = 1, replace_pref = []):
        # TODO: Simply by only updating one parcel, and one replace_pref value

        # filter for crops in the parcels out of the replace list
        # replace_pref = [crop_type for crop_type in replace_pref if crop_type in self.parcels]
        replace_pref = [crop_type for crop_type in replace_pref if crop_type in {parcel.crop for parcel in self.parcels}]

        replace_pref_index = 0
        while n_parcels > 0:
            current_crops = set(parcel.crop for parcel in self.parcels)
            # Select the first crop out replace_pref that's already planted
            while replace_pref_index < len(replace_pref):
                if replace_pref[replace_pref_index] in current_crops:
                    replace_crop = replace_pref[replace_pref_index]
                    break
                replace_pref_index += 1
            else:
                replace_crop = random.choice(list(current_crops))
            # Select a random parcel with that crop
            parcel = random.choice([parcel for parcel in self.parcels if parcel.crop == replace_crop or parcel.crop == None])
            # Plant the crop
            parcel.crop = new_crop            
            n_parcels -= 1
        self.crop_counter = Counter([parcel.crop for parcel in self.parcels])
    

    def harvest(self, year):
     income = 0
     total_profit = 0
     crop_income = {}  # Dictionary to store income per crop type
     crop_profit = {}

     profit_df = self.model.profit_df

     # Filter the dataset for the given district and year
     district_profit_df = profit_df[
        (profit_df["Year"] == year) &
        (profit_df["District"].str.lower().str.strip() == self.district.lower().strip())
     ]

     for parcel in self.parcels:
        if parcel.crop is not None:
            crop_data = district_profit_df[
                district_profit_df["Crop"].str.lower().str.strip() == parcel.crop.lower().strip()
            ]

            if crop_data.empty:
                continue

            revenue_per_unit = crop_data.iloc[0]["Revenue"]
            profit_per_unit = (crop_data.iloc[0]["Revenue"]*2) - crop_data.iloc[0]["Cost"]

            # profit_per_unit = crop_data.iloc[0]["Profit"]

            parcel_income = 2 * revenue_per_unit * parcel.size
            parcel_profit = profit_per_unit * parcel.size

            # Skip parcels with negative profit
            if parcel_profit < 0:
                continue

            # Accumulate income and profit only if profit is non-negative
            crop_income[parcel.crop] = crop_income.get(parcel.crop, 0) + parcel_income
            crop_profit[parcel.crop] = crop_profit.get(parcel.crop, 0) + parcel_profit

     # Compute total income and total profit from valid parcels
     total_profit = sum(crop_profit.values())
     income = sum(crop_income.values())

     return total_profit, crop_income, crop_profit, income


class Parcel:
    def __init__(self, size):
        self.size = size
        self.crop = None



class JGL:

    _id_counter = 0  # Class variable to generate unique ids

    def __init__(self, max_size, type, district):
        self.members = []
        self.max_size = max_size
        self.type = type
        self.district = district
        self.id = JGL._id_counter  # Assign a unique id to each JGL
        JGL._id_counter += 1       # Increment the id counter for the next JGL



class Loan:
    def __init__(self, amount, interest_rate, duration, borrower, lender=None, interest_rate_after_duration=None, collateral_used=0, jlg=False, loan_type = None):
        self.amount = amount
        self.interest_rate = interest_rate
        self.duration = duration
        self.years = 0
        self.borrower = borrower
        self.lender = lender
        self.interest_rate_after_duration = (
            interest_rate if interest_rate_after_duration is None else interest_rate_after_duration
        )
        self.current_interest_rate = interest_rate
        self.collateral_used = collateral_used
        self.loan_type = loan_type
        self.jlg = jlg
        self.jlg_repaid_by_others = False
        self.due = False  # Initialize as not due


         # Assign loan type based on the lender or the conditions
        if interest_rate == 0:
            self.loan_type = "neighbour"
        elif interest_rate == 0.14:
            self.loan_type = "collateral"
        elif interest_rate == 0.24:  # Assuming high interest rates are for income financing loans
            self.loan_type = "jgl"
        elif interest_rate == 0.18:  # Assuming high interest rates are for income financing loans
            self.loan_type = "income_financing_loan"

        else:
            self.loan_type = "others" # Default case if needed


    def update_due_status(self, current_year):
        # Mark loan as due if the duration has passed
        self.due = (current_year - self.start_year) >= self.duration

    def update(self):
        # TODO: Validate order of steps here, also considering year = 0
        self.years += 1
        if self.years > self.duration:
            self.current_interest_rate = self.current_interest_rate
        self.amount *= (1 + self.current_interest_rate)

        # Update farmer's debt tracking directly
        if self.loan_type == "neighbour":
         self.borrower.neighbour_loan_due = self.amount
        elif self.loan_type == "collateral":
         self.borrower.collateral_loan_due = self.amount
        elif self.loan_type == "income_financing_loan":
         self.borrower.income_financing_loan_due = self.amount
        elif self.loan_type == "jgl":
         self.borrower.jgl_loan_due = self.amount

        self.total_debt = self.amount



    def pay_back(self, pay_amount, current_year):
        # Unified payback method for both regular and JLG loans.
        # Reduce the loan amount by the payback amount
        self.amount -= pay_amount

        if self.amount <= 0:
            self.amount = 0  # Ensure no negative values

        if self.amount <= 0:
            # Loan fully repaid, handle accordingly
            # Remove the loan from the borrower's list
            self.borrower.loans.remove(self)
            
            # Restore collateral if used
            if hasattr(self, 'collateral_used') and self.collateral_used > 0:
              self.borrower.collateral_used -= self.collateral_used
              self.collateral_used = 0  # Reset the collateral on the loan

            # If there's a lender, transfer the remaining amount (if negative) to the lender
            if self.lender is not None:
                # self.lender.money += abs(self.amount)  # In case the repayment exceeded the loan amount
                self.lender.money += pay_amount

                # ✅ If lender is a neighbour, update their "loaned" record
                if self.loan_type == "neighbour":
                 if hasattr(self.lender, 'loaned'):
                  self.lender.loaned -= pay_amount
                  if self.lender.loaned < 0:
                   self.lender.loaned = 0  # Ensure it doesn't go negative

            # Reset the loan amount to 0
            self.amount = 0
            # print(f"Farmer {self.borrower.unique_id} fully repaid the loan.")
            return

        # If the loan isn't fully repaid
        # print(f"Farmer {self.borrower.unique_id} repaid {pay_amount:.2f}. Remaining debt: {self.amount:.2f}")

        # JLG loan repayment handling
        if self.borrower.jgl_loan_due == self and self.due:
            print(f"Farmer {self.borrower.unique_id} couldn't fully repay a JLG loan. Remaining debt: {self.amount:.2f}")

            if self.borrower.jgl is not None:
                jgl = self.borrower.jgl
                jlg_members = [farmer for farmer in self.borrower.jgl.members if farmer != self.borrower]

                if jlg_members:
                    unpaid_amount = self.amount
                    # Remove farmers with no money before distribution
                    able_to_pay = [farmer for farmer in jlg_members if (farmer.money - 1.5 * farmer.cost_of_living) >= 0.10 * unpaid_amount]

                    # Equal distribution round
                    while able_to_pay and unpaid_amount > 0:
                        # Calculate the minimum share each can pay
                        min_share = min(min(farmer.money for farmer in able_to_pay), unpaid_amount / len(able_to_pay))

                        # If the minimum share is greater than 0, proceed
                        if min_share > 0:
                            # Deduct the minimum share from each able member
                            for farmer in able_to_pay:
                                farmer.money -= min_share
                                farmer.JGL_others_loan += min_share
                                unpaid_amount -= min_share  # Deduct from unpaid_amount

                            # Mark that JLG members repaid part of the debt
                            self.borrower.jlg_repaid_by_others = True
                        else:
                            break

                    # After all possible payments
                    if unpaid_amount <= 0:
                        self.amount = 0
                        self.borrower.loans.remove(self)
                        print(f"Unpaid amount was fully repaid by JLG members.")
                    else:
                        # Print the remaining unpaid amount
                        print(f"Warning: Not all of the unpaid amount could be repaid. Remaining debt: {unpaid_amount:.2f}")
                else:
                    print(f"Error: No JGL members available to repay.")
        else:
            # Regular loan, no further actions are needed beyond reducing the amount
            if self.lender is not None:
                self.lender.money += pay_amount
