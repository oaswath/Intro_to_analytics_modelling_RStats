import pandas as pd
from pulp import *
import matplotlib.pyplot as plt

# 1. Load the data directly from the .xls file
file_name = "dietSummer2018.xls"  # Change this to your actual filename
try:
    # Use sheet_name='Sheet1' as requested
    data = pd.read_excel(file_name, sheet_name='Sheet1')
except Exception as e:
    print(f"Error loading file: {e}")
    exit()

# 2. Data Preprocessing
# Based on the diet.xls structure:
# - Foods are in rows 0 to 63
# - Min requirements are in row 65
# - Max requirements are in row 66
food_items = data.iloc[0:64].copy()
min_reqs = data.iloc[65, 3:].astype(float)
max_reqs = data.iloc[66, 3:].astype(float)
nutrients = list(min_reqs.index)

# 3. Define the Problem
prob = LpProblem("Diet_Optimization", LpMinimize)

# 4. Decision Variables
# x_i: Number of servings of food i
food_vars = LpVariable.dicts("Servings", food_items['Foods'], lowBound=0, cat='Continuous')

# 5. Objective Function: Minimize Total Cost
# Z = sum(Price_i * x_i)
prob += lpSum([food_items.loc[i, 'Price/ Serving'] * food_vars[food_items.loc[i, 'Foods']] 
               for i in food_items.index]), "Total_Cost"

# 6. Constraints: Nutritional Limits
for n in nutrients:
    # sum(Nutrient_per_serving_i * x_i)
    total_intake = lpSum([food_items.loc[i, n] * food_vars[food_items.loc[i, 'Foods']] 
                          for i in food_items.index])
    
    prob += total_intake >= min_reqs[n], f"Min_{n.replace(' ', '_')}"
    prob += total_intake <= max_reqs[n], f"Max_{n.replace(' ', '_')}"

# 7. Solve
prob.solve(PULP_CBC_CMD(msg=0))

# 8. Output results
print(f"Status: {LpStatus[prob.status]}")
print(f"Total Daily Cost: ${value(prob.objective):.2f}")
print("-" * 30)

results = []
for v in prob.variables():
    if v.varValue > 0.001:
        food_name = v.name.replace('Servings_', '').replace('_', ' ')
        print(f"{food_name:<25}: {v.varValue:>6.2f} servings")
        results.append({'Food': food_name, 'Servings': v.varValue})

# 9. Visualization
if results:
    res_df = pd.DataFrame(results)
    plt.figure(figsize=(10, 6))
    plt.barh(res_df['Food'], res_df['Servings'], color='skyblue')
    plt.xlabel('Number of Servings')
    plt.title('Optimal Diet Composition (Cheapest Daily Cost)')
    plt.gca().invert_yaxis()
    plt.grid(axis='x', alpha=0.3)
    plt.show()