# Load necessary libraries
if (!require("FrF2")) install.packages("FrF2")
library(FrF2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# Define the Experimental Design
# nfactors = 10 (Large Yard, Solar Roof, etc.)
# nruns = 16 (The constraint provided by the agent)
set.seed(123) # For reproducibility
house_design <- FrF2(nruns = 16, nfactors = 10, 
                     factor.names = c("Yard", "Solar", "Pool", "Garage", "Basement",  # nolint
                                      "Attic", "AC", "Deck", "Fence", "Security"), # nolint
                     replications = 1, randomize = FALSE)

# Convert the design to numeric -1 and 1 for analysis
design_matrix <- as.data.frame(lapply(house_design, function(x) as.numeric(as.character(x)))) # nolint

# 3. View the set of features for each of the 16 houses
print("The 16 Fictitious House Configurations:")
print(design_matrix)
survey_data <- (design_matrix + 1) / 2 # Convert -1/1 to 0/1 for survey interpretation # nolint
print("Survey Data (0 = No, 1 = Yes):")
print(survey_data)

# Analyze the Aliasing Structure (Design Resolution)
# This shows which main effects are confounded with interactions
design_summary <- summary(house_design)
print(design_summary)

# ====================================================================
# VISUALIZATION & DIAGNOSTICS # nolint
# ====================================================================

# Open Quartz Graphics Device
if (exists("quartz")) {
  quartz(title = "FFDA_Design_Matrix", width = 12, height = 8)
}

# PLOT 1: Design Matrix Heatmap
# Visualizes the distribution of -1 (No) and 1 (Yes) across the 16 houses
design_long <- design_matrix %>% # nolint
  mutate(HouseID = row_number()) %>% # nolint
  pivot_longer(-HouseID, names_to = "Feature", values_to = "Setting")

p1 <- ggplot(design_long, aes(x = Feature, y = factor(HouseID), fill = factor(Setting))) + # nolint
  geom_tile(color = "white") +
  scale_fill_manual(values = c("-1" = "#e74c3c", "1" = "#2ecc71"), 
                    labels = c("Exclude (-1)", "Include (1)")) +
  labs(title = "1. Experimental Design Matrix", 
       subtitle = "16 Houses vs 10 Features",
       y = "House Number", x = "Feature Name", fill = "Decision") +
  theme_minimal()

print(p1)

if (exists("quartz")) {
  quartz(title = "FFDA_Feature_Correlation", width = 12, height = 8)
}

# PLOT 2: Orthogonality Check (Correlation Plot)
# In a good design, features should be uncorrelated (0)
cor_matrix <- as.data.frame(as.table(cor(design_matrix)))
p2 <- ggplot(cor_matrix, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1,1)) + # nolint
  labs(title = "2. Feature Correlation Matrix", 
       subtitle = "Checking for Orthogonality",
       x = "", y = "", fill = "Corr") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# ====================================================================
# SIMULATE THE SURVEY RESULTS
# ====================================================================
# For this example, we assume a base house price of $300,000.
# We define hidden "true" values for the features, then add survey noise.
base_price <- 300000
true_values <- c(Yard=20000, Solar=15000, Pool=25000, Garage=12000, 
                 Basement=8000, Attic=4000, AC=18000, Deck=7500, 
                 Fence=5000, Security=3000)

# Simulate buyer valuations by multiplying features by their true value, plus random survey variance # nolint
set.seed(456)
survey_data$MarketValue <- base_price + as.matrix(survey_data) %*% true_values + rnorm(16, mean=0, sd=1500) # nolint

# ====================================================================
# RUN LINEAR REGRESSION (lm)
# ====================================================================
# The model attempts to guess the true value of each feature based on the 16 survey answers # nolint
pricing_model <- lm(MarketValue ~ ., data = survey_data)

# Print detailed summary
print("=== Linear Regression Output ===")
print(summary(pricing_model))

# ====================================================================
# EXTRACT COEFFICIENTS FOR PLOTTING
# ====================================================================
# We skip the Intercept (Base house price) to focus only on the features
coef_df <- data.frame(
  Feature = names(coef(pricing_model))[-1],
  AddedValue = as.numeric(coef(pricing_model))[-1]
) %>% arrange(desc(AddedValue))  # Sort largest to smallest # nolint

# ====================================================================
# PLOT USING QUARTZ AND GGPLOT
# ====================================================================
# Open Graphics Device
if (exists("quartz")) {
  quartz(title = "Feature Market Value Coefficients", width = 10, height = 7)
}

# Create the Coefficient Bar Chart
p_coef <- ggplot(coef_df, aes(x = reorder(Feature, AddedValue), y = AddedValue, fill = AddedValue)) + # nolint
  geom_col(color = "black") +
  coord_flip() +  # Flips to horizontal bars for readability
  scale_fill_gradient(low = "#3498db", high = "#2c3e50") +
  # Add dollar text labels at the end of each bar
  geom_text(aes(label = paste0("$", format(round(AddedValue), big.mark=","))), 
            hjust = -0.1, size = 4, fontface="bold") +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.15))) + # nolint: line_length_linter.
  labs(title = "Estimated Added Market Value per Feature",
       subtitle = "Extracted via Linear Regression from 16-Run Fractional Factorial Survey", # nolint
       x = "House Feature", y = "Estimated Value Added ($)", fill = "Value") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=11, color="black"))

# Render Plot
print(p_coef)