import pandas as pd
from pulp import *
import matplotlib.pyplot as plt
import re

# 1. LOAD DATA
file_name = "diet_largeSummer2018.xls"
try:
    # Skip the first blank/metadata row, headers are on row 1
    df = pd.read_excel(file_name, sheet_name='Sheet1', engine='xlrd', skiprows=1)
except Exception as e:
    print(f"Error reading file: {e}")
    exit()

# 2. DATA CLEANING (Critical for solving the TypeError)
# Clean column names
df.columns = [str(c).strip() for c in df.columns]

# DROP ROWS where the food name is missing (this removes the units/summary rows at bottom)
df = df.dropna(subset=['Long_Desc'])

# CONFORCE NUMERIC: Convert all nutrient columns to numbers, non-numeric becomes NaN, then 0
nutrient_cols = [
    'Protein', 'Carbohydrate, by difference', 'Energy', 'Calcium, Ca', 
    'Iron, Fe', 'Sodium, Na', 'Cholesterol'
]
for col in nutrient_cols:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors='coerce').fillna(0)

# Reset index to avoid mapping errors during looping
df = df.reset_index(drop=True)
foods = df['Long_Desc'].tolist()

# 3. DEFINE THE MODEL
prob = LpProblem("Minimal_Cholesterol_LargeScale", LpMinimize)

# Decision Variables
x = LpVariable.dicts("Servings", foods, lowBound=0, cat='Continuous')
y = LpVariable.dicts("Selected", foods, cat='Binary')

# 4. OBJECTIVE FUNCTION: Minimize Cholesterol
prob += lpSum([df.loc[i, 'Cholesterol'] * x[foods[i]] for i in range(len(df))])

# 5. CONSTRAINTS
# Standard Nutritional Constraints
constraints = {
    'Energy': (1500, 2500),
    'Protein': (60, 100),
    'Calcium, Ca': (700, 1500),
    'Iron, Fe': (10, 40),
    'Sodium, Na': (0, 2000)
}

for nut, (lo, hi) in constraints.items():
    if nut in df.columns:
        total_nut = lpSum([df.loc[i, nut] * x[foods[i]] for i in range(len(df))])
        prob += total_nut >= lo, f"Min_{nut.replace(', ', '_')}"
        prob += total_nut <= hi, f"Max_{nut.replace(', ', '_')}"

# Logical Constraints
M = 1000 
for f in foods:
    # a. Minimum 0.1 servings if selected
    prob += x[f] >= 0.1 * y[f]
    prob += x[f] <= M * y[f]

# IMPROVED Variety Constraint: Use word boundaries (\b) to avoid partial matches
# This prevents "egg" from matching "eggplant"
protein_keywords = ['chicken', 'beef', 'fish', 'egg', 'pork', 'turkey', 'whale', 'frog', 'snail']
protein_indices = []

for i, f in enumerate(foods):
    # Search for whole words only
    if any(re.search(rf'\b{k}\b', f.lower()) for k in protein_keywords):
        protein_indices.append(i)

prob += lpSum([y[foods[i]] for i in protein_indices]) >= 3

# 6. SOLVE
prob.solve(PULP_CBC_CMD(msg=0))

# 7. EXTRACT RESULTS
results = []
for f in foods:
    if x[f].varValue > 0.001:
        chol = df[df['Long_Desc'] == f]['Cholesterol'].values[0]
        results.append({
            "Food": f, 
            "Servings": round(x[f].varValue, 3),
            "Chol_Contrib": round(x[f].varValue * chol, 2)
        })

res_df = pd.DataFrame(results)
print(f"Status: {LpStatus[prob.status]}")
print(f"Total Daily Cholesterol: {value(prob.objective)} mg")
print(res_df.to_string(index=False))

# 8. PLOT
if not res_df.empty:
    plt.figure(figsize=(10, 6))
    plt.barh(res_df['Food'], res_df['Servings'], color='darkcyan')
    plt.xlabel('Servings')
    plt.title('Optimal Large-Scale Diet (Minimal Cholesterol)')
    plt.gca().invert_yaxis()
    plt.tight_layout()
    plt.show()