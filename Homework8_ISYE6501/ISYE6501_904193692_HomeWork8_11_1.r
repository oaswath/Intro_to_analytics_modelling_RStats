# Load necessary libraries
library(tidyr)
library(dplyr)
library(caret)
library(ggplot2)
library(smooth)
library(stringr)
library(glmnet)
#library(gridExtra)

rm(list = ls())

### ISYE 6501 Homework 8 - Stepwise Regression Path Analysis
### Stepwise Regression Path Analysis
### This code performs stepwise regression on the 'uscrime' dataset, captures the AIC values at each step, and visualizes the path taken to reach the optimal model. # nolint
### ------------------------------------

# 1. Load Data and Run Stepwise
data <- read.table("uscrime.txt", header = TRUE)

### ISYE 6501 Homework 8 -  StepWise Regression Analysis
### StepWise Regression Analysis
### This code performs stepwise regression on the 'uscrime' dataset, captures the AIC values at each step, and visualizes the path taken to reach the optimal model. # nolint
### ------------------------------------
full_model <- lm(Crime ~ ., data = data)

# We capture the output of step() to extract the AIC history
step_log <- capture.output(
  step_model <- step(full_model, direction = "both", trace = 1)
)

print(summary(step_model))

# 2. Parse the Stepwise Log to extract AIC and Steps
# This regex looks for lines indicating a change in the model
step_indices <- grep("Step:  AIC=", step_log)
aic_values <- as.numeric(str_extract(step_log[step_indices], "(?<=AIC=)[0-9.]+")) # nolint

# Identify what was added or removed
# The first step is 'Initial', subsequent steps are the variables changed
change_labels <- c("Initial Model")
for (i in 1:(length(step_indices)-1)) {
  # Look at the line immediately following the Step line to see the action
  action_line <- step_log[step_indices[i] + 1]
  change_labels <- c(change_labels, str_trim(action_line))
}

# 3. Create a Data Frame for Plotting
path_df <- data.frame(
  Iteration = 1:length(aic_values), # nolint
  AIC = aic_values,
  Change = change_labels
)

# 4. Open Graphics Device and Plot
if (exists("quartz")) {
  quartz(title = "Stepwise Regression: AIC Path Analysis", width = 10, height = 7) # nolint
}

pw_plot <- ggplot(path_df, aes(x = Iteration, y = AIC)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(aes(color = Iteration == which.min(AIC)), size = 4) +
  geom_text(aes(label = Change), vjust = -1, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("black", "red"), labels = c("Step", "Optimal")) + # nolint
  scale_x_continuous(breaks = 1:nrow(path_df)) + # nolint
  labs(
    title = "Stepwise Regression: AIC Reduction Path",
    subtitle = "Tracking Forward/Backward steps to reach minimum AIC",
    x = "Iteration Number",
    y = "Akaike Information Criterion (AIC)",
    color = "Status"
  ) +
  theme_minimal()

print(pw_plot)


### ISYE 6501 Homework 8 - Lasso Regression Analysis
### Lasso Regression Analysis
### This code performs lasso regression on the 'uscrime' dataset, captures the AIC values at each step, and visualizes the path taken to reach the optimal model. # nolint
### glmnet automatically scales, but for clarity:
### ------------------------------------

x <- as.matrix(data[, 1:15])
y <- data[, 16]
x_scaled <- scale(x)

### Fit Lasso Models
set.seed(123) # Ensure reproducibility for cross-validation
lasso_cv <- cv.glmnet(x_scaled, y, alpha = 1, nfolds = 5)
lasso_model <- glmnet(x_scaled, y, alpha = 1)

# ====================================================================
# PLOT 1: Cross-Validation MSE 
# ====================================================================
cv_data <- data.frame(
  LogLambda = log(lasso_cv$lambda),
  MSE = lasso_cv$cvm,
  MSE_Upper = lasso_cv$cvup,
  MSE_Lower = lasso_cv$cvlo
)

lp1 <- ggplot(cv_data, aes(x = LogLambda, y = MSE)) +
  geom_point(color = "red", size = 2) +
  geom_errorbar(aes(ymin = MSE_Lower, ymax = MSE_Upper), color = "gray50", width = 0.2) + # nolint
  geom_vline(xintercept = log(lasso_cv$lambda.min), linetype = "dashed", color = "blue", linewidth = 1) + # nolint
  geom_vline(xintercept = log(lasso_cv$lambda.1se), linetype = "dotted", color = "darkgreen", linewidth = 1) + # nolint
  labs(title = "1. Lasso CV Error Curve",
       subtitle = "Dashed = Min MSE | Dotted = 1 SE Rule",
       x = "Log(Lambda) - Penalty Strength",
       y = "Mean Squared Error (CV)") +
  theme_minimal()

if (exists("quartz")) quartz(title="MSE Curve") # nolint
  print(lp1) # nolint


# ====================================================================
# PLOT 2: R-Squared (Deviance Ratio) vs. Log(Lambda)
# ====================================================================
# Extract the Deviance Ratio (R-squared equivalent for glmnet)
r2_data <- data.frame(
  LogLambda = log(lasso_model$lambda),
  R2 = lasso_model$dev.ratio
)

lp2 <- ggplot(r2_data, aes(x = LogLambda, y = R2)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_vline(xintercept = log(lasso_cv$lambda.min), linetype = "dashed", color = "black", linewidth = 1) + # nolint
  labs(title = "2. R-Squared vs. Log(Lambda)",
       subtitle = "Proportion of Deviance Explained",
       x = "Log(Lambda) - Increasing Penalty ->",
       y = expression(R^2 ~ "(Deviance Ratio)")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal()

if (exists("quartz")) quartz(title="R-Squared") # nolint
  print(lp2) # nolint

# ====================================================================
# Data Prep for Coefficient Paths
# ====================================================================
beta_matrix <- as.matrix(lasso_model$beta)
coef_data <- as.data.frame(t(beta_matrix))
coef_data$Lambda <- lasso_model$lambda
coef_data$LogLambda <- log(lasso_model$lambda)
coef_data$L1_Norm <- colSums(abs(beta_matrix))

coef_long <- pivot_longer(coef_data, 
                          cols = -c(Lambda, LogLambda, L1_Norm), 
                          names_to = "Variable", 
                          values_to = "Coefficient")

# ====================================================================
# PLOT 3: Coefficient Path vs Log Lambda
# ====================================================================
lp3 <- ggplot(coef_long, aes(x = LogLambda, y = Coefficient, color = Variable)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = log(lasso_cv$lambda.min), linetype = "dashed", color = "black", linewidth = 1) + # nolint
  labs(title = "3. Coefficient Path vs. Log(Lambda)",
       subtitle = "Coefficients squeezed to zero as penalty increases",
       x = "Log(Lambda) - Increasing Penalty ->",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(legend.position = "none")

if (exists("quartz")) quartz(title="Coefficient Path") # nolint
  print(lp3) # nolint

# ====================================================================
# PLOT 4: Coefficient Path vs L1 Norm (T)
# ====================================================================
lp4 <- ggplot(coef_long, aes(x = L1_Norm, y = Coefficient, color = Variable)) +
  geom_line(linewidth = 1) +
  labs(title = "4. Coefficient Path vs. L1 Norm (T)",
       subtitle = "Coefficients grow as budget (T) relaxes",
       x = "L1 Norm (T) - Relaxing Constraint ->",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(legend.position = "right")

if (exists("quartz")) quartz(title="L1 Norm") # nolint
  print(lp4) # nolint

# ====================================================================
# Open Quartz and Render Dashboard
# ====================================================================
#if (exists("quartz")) {
#  quartz(title = "Lasso Regression: Parameter Diagnostics", width = 12, height = 10) # nolint
#}
# Arrange plots in a 2x2 grid
#print(grid.arrange(lp1, lp2, lp3, lp4, ncol = 2))

### ISYE 6501 Homework 8 - ElasticNet Regression Analysis
### ElasticNet Regression Analysis
### This code performs elastic net regression on the 'uscrime' dataset, captures the AIC values at each step, and visualizes the path taken to reach the optimal model. # nolint
### glmnet automatically scales, but for clarity:
### ------------------------------------

x <- as.matrix(data[, 1:15])
y <- data[, 16]
x_scaled <- scale(x)

# Search for the Best Alpha (Mixing Parameter)
alphas <- seq(0, 1, by = 0.1)
results_list <- list()

# We loop through alpha values to find which one yields the lowest CV MSE
for (i in seq_along(alphas)) {
  set.seed(123) # For reproducibility
  cv_fit <- cv.glmnet(x_scaled, y, alpha = alphas[i], nfolds = 5)
  results_list[[i]] <- data.frame(
    alpha = alphas[i], 
    min_mse = min(cv_fit$cvm), 
    lambda_min = cv_fit$lambda.min
  )
}

alpha_search <- do.call(rbind, results_list)
best_alpha <- alpha_search$alpha[which.min(alpha_search$min_mse)]
cat("The optimal Alpha found is:", best_alpha, "\n")

# Fit the Final Best Model
set.seed(123)
best_cv_model <- cv.glmnet(x_scaled, y, alpha = best_alpha, nfolds = 5)
best_full_model <- glmnet(x_scaled, y, alpha = best_alpha)

# ====================================================================
# PREPARE PLOTS
# ====================================================================

# PLOT 1: CV MSE vs Log Lambda
cv_df <- data.frame(LogLambda = log(best_cv_model$lambda), MSE = best_cv_model$cvm,  # nolint
                    Upper = best_cv_model$cvup, Lower = best_cv_model$cvlo)
                     # nolint
ep1 <- ggplot(cv_df, aes(x = LogLambda, y = MSE)) +
  geom_point(color = "red") + geom_errorbar(aes(ymin = Lower, ymax = Upper), color = "gray") + # nolint
  geom_vline(xintercept = log(best_cv_model$lambda.min), linetype = "dashed") +
  labs(title = paste("1. CV MSE (Alpha =", best_alpha, ")"), x = "Log(Lambda)", y = "MSE") + # nolint
  theme_minimal()

if (exists("quartz")) quartz(title="Elastic Net MSE Curve") # nolint
  print(ep1) # nolint

# PLOT 2: R2 (Deviance Ratio) vs Log Lambda
r2_df <- data.frame(LogLambda = log(best_full_model$lambda), R2 = best_full_model$dev.ratio) # nolint

ep2 <- ggplot(r2_df, aes(x = LogLambda, y = R2)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_vline(xintercept = log(best_cv_model$lambda.min), linetype = "dashed") +
  labs(title = "2. R-Squared (Deviance Explained)", x = "Log(Lambda)", y = "R2") + # nolint
  theme_minimal()

if (exists("quartz")) quartz(title="Elastic Net R-Squared") # nolint
  print(ep2) # nolint

# PLOT 3 & 4: Coefficient Paths
beta_matrix <- as.matrix(best_full_model$beta)
coef_df <- as.data.frame(t(beta_matrix)) %>% # nolint
  mutate(LogLambda = log(best_full_model$lambda), L1_Norm = colSums(abs(beta_matrix))) %>% # nolint
  pivot_longer(cols = -c(LogLambda, L1_Norm), names_to = "Variable", values_to = "Coef") # nolint # nolint

ep3 <- ggplot(coef_df, aes(x = LogLambda, y = Coef, color = Variable)) +
  geom_line() + geom_vline(xintercept = log(best_cv_model$lambda.min), linetype = "dashed") + # nolint
  labs(title = "3. Coef Path vs. Log(Lambda)", x = "Log(Lambda)", y = "Coefficient") + # nolint
  theme_minimal() + theme(legend.position = "none")

if (exists("quartz")) quartz(title="Elastic Net Coefficient Paths") # nolint
  print(ep3) # nolint

ep4 <- ggplot(coef_df, aes(x = L1_Norm, y = Coef, color = Variable)) +
  geom_line() + labs(title = "4. Coef Path vs. Norm (T)", x = "L1 Norm (T)", y = "Coefficient") + # nolint
  theme_minimal()

if (exists("quartz")) quartz(title="Elastic Net L1 Norm") # nolint
  print(ep4) # nolint

#if (exists("quartz")) quartz(title = "Elastic Net Analysis", width = 12, height = 9) # nolint
#grid.arrange(ep1, ep2, ep3, ep4, ncol = 2) # nolint

# Output final coefficients
print(coef(best_cv_model, s = "lambda.min"))