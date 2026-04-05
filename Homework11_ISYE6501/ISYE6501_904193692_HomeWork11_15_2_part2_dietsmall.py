import pandas as pd
from pulp import *
import matplotlib.pyplot as plt

# ==========================================
# 1. DATA LOADING & PREPROCESSING
# ==========================================
file_name = "dietSummer2018.xls" # <--- CHANGE THIS to your actual filename (e.g. "diet.xls")

try:
    # Use xlrd engine for .xls files
    data = pd.read_excel(file_name, sheet_name='Sheet1')
except Exception as e:
    print(f"Error: Could not find or read '{file_name}'.")
    print("Ensure the file is in: /Users/aswathoruganti/OMSA-GTech/Intro_to_Analytics_Modelling/Homework11_ISYE6501/")
    exit()

# Extract food data (Rows 0-63)
food_items = data.iloc[0:64].copy()
# Extract Min/Max requirements (Rows 65 and 66)
min_reqs = data.iloc[65, 3:].astype(float)
max_reqs = data.iloc[66, 3:].astype(float)
nutrients = list(min_reqs.index)
foods = food_items['Foods'].tolist()

def solve_diet_problem(part_two=False):
    # ==========================================
    # 2. MODEL FORMULATION
    # ==========================================
    prob = LpProblem(f"Diet_Problem_Part_{'2' if part_two else '1'}", LpMinimize)

    # Continuous variables: servings of each food
    x = LpVariable.dicts("Serv", foods, lowBound=0, cat='Continuous')
    
    # Binary variables (Only for Part 2): 1 if food is chosen, 0 otherwise
    y = LpVariable.dicts("Chosen", foods, cat='Binary')

    # Objective Function: Minimize Total Cost
    prob += lpSum([food_items.loc[i, 'Price/ Serving'] * x[foods[i]] for i in range(64)])

    # ==========================================
    # 3. CONSTRAINTS
    # ==========================================
    
    # Basic Nutritional Constraints (Common to both parts)
    for n in nutrients:
        total_nutrient = lpSum([food_items.loc[i, n] * x[foods[i]] for i in range(64)])
        prob += total_nutrient >= min_reqs[n], f"Min_{n.replace(' ', '_')}"
        prob += total_nutrient <= max_reqs[n], f"Max_{n.replace(' ', '_')}"

    if part_two:
        # a. Min 0.1 servings if selected + Linking Constraints (Big M)
        M = 1000 
        for f in foods:
            prob += x[f] >= 0.1 * y[f], f"Min_Threshold_{f.replace(' ', '_')}"
            prob += x[f] <= M * y[f], f"Link_{f.replace(' ', '_')}"

        # b. Celery vs Broccoli (At most one)
        prob += y['Celery, Raw'] + y['Frozen Broccoli'] <= 1, "Exclusion_Celery_Broccoli"

        # c. Protein Variety (At least 3 meat/poultry/fish/eggs)
        protein_list = [
            'Roasted Chicken', 'Poached Eggs', 'Scrambled Eggs', 'Bologna,Turkey', 
            'Frankfurter, Beef', 'Ham,Sliced,Extralean', 'Kielbasa,Pork', 
            'Hamburger W/Cb', 'Hotdog, Plain', 'Sardines in Oil', 'White Tuna in Water',
            'Pork', 'Chicknoodl Soup', 'Splt Pea&Hamsoup', 'Vegetbeef Soup'
        ]
        prob += lpSum([y[f] for f in protein_list if f in foods]) >= 3, "Protein_Variety"

    # ==========================================
    # 4. SOLVE & RESULTS
    # ==========================================
    prob.solve(PULP_CBC_CMD(msg=0))
    
    print(f"\n--- RESULTS FOR PART {'2' if part_two else '1'} ---")
    print(f"Status: {LpStatus[prob.status]}")
    print(f"Total Daily Cost: ${value(prob.objective):.2f}")
    
    results = []
    for f in foods:
        if x[f].varValue > 0.001:
            results.append({"Food": f, "Servings": round(x[f].varValue, 3)})
    
    res_df = pd.DataFrame(results)
    print(res_df.to_string(index=False))
    return res_df

# Run and Plot
df1 = solve_diet_problem(part_two=False)
df2 = solve_diet_problem(part_two=True)

# Plotting Comparison
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
ax1.barh(df1['Food'], df1['Servings'], color='skyblue')
ax1.set_title("Part 1: Simple LP Solution")
ax2.barh(df2['Food'], df2['Servings'], color='salmon')
ax2.set_title("Part 2: MILP Solution (Constraints Added)")
plt.tight_layout()
plt.show()