# Load necessary libraries
if (!require("FrF2")) install.packages("FrF2")
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# --- 1. PREPARATION AND LOADING ---
library(caret)
library(e1071)
library(dplyr)

# 1. Load and Prepare Data
col_names <- c("ID", "Clump_Thickness", "Uniformity_Cell_Size", "Uniformity_Cell_Shape", # nolint
               "Marginal_Adhesion", "Single_Epithelial_Size", "Bare_Nuclei",
               "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class")

# Ensure the data file is in your working directory
data <- read.csv("breast-cancer-wisconsin.data", header = FALSE, col.names = col_names, na.strings = "?") # nolint
data$Class <- as.factor(data$Class)

# Separate complete and missing rows for Bare_Nuclei
missing_idx <- which(is.na(data$Bare_Nuclei))
complete_df <- data[-missing_idx, ]
missing_df <- data[missing_idx, ]

# ---------------------------------------------------------
# 2. Imputation Methods & Quality Metrics
# ---------------------------------------------------------

# A. Mean Imputation
data_mean <- data
data_mean$Bare_Nuclei[missing_idx] <- mean(complete_df$Bare_Nuclei)

# B. Regression Imputation (Calculating MSE, AIC, BIC)
reg_model <- lm(Bare_Nuclei ~ . - ID - Class, data = complete_df)
imputation_mse <- mean(residuals(reg_model)^2)
imputation_aic <- AIC(reg_model)
imputation_bic <- BIC(reg_model)

cat("\n--- Imputation Regression Model Quality ---\n")
cat("MSE: ", imputation_mse, "\nAIC: ", imputation_aic, "\nBIC: ", imputation_bic, "\n") # nolint

data_reg <- data
data_reg$Bare_Nuclei[missing_idx] <- predict(reg_model, missing_df)

# C. Regression with Perturbation
set.seed(42)
rse <- summary(reg_model)$sigma
data_pert <- data
data_pert$Bare_Nuclei[missing_idx] <- predict(reg_model, missing_df) + rnorm(nrow(missing_df), 0, rse) # nolint

# D. Binary Indicator Method
data_indicator <- data_mean
data_indicator$Missing_Flag <- as.factor(ifelse(is.na(data$Bare_Nuclei), 1, 0))

# E. Dropped Data
data_dropped <- na.omit(data)

# ---------------------------------------------------------
# 3. Model Evaluation (SVM & KNN)
# ---------------------------------------------------------

evaluate_performance <- function(df, name) {
  set.seed(42)
  trainIndex <- createDataPartition(df$Class, p = 0.7, list = FALSE) # nolint
  train <- df[trainIndex, -1] # Remove ID
  test <- df[-trainIndex, -1]
   # nolint
  # Train KNN
  knn_fit <- train(Class ~ ., data = train, method = "knn")
  knn_acc <- confusionMatrix(predict(knn_fit, test), test$Class)$overall['Accuracy'] # nolint
   # nolint
  # Train SVM
  svm_fit <- svm(Class ~ ., data = train)
  svm_acc <- confusionMatrix(predict(svm_fit, test), test$Class)$overall['Accuracy'] # nolint
   # nolint
  return(data.frame(Method = name, Model = c("KNN", "SVM"), Accuracy = c(knn_acc, svm_acc))) # nolint
}

results <- rbind(
  evaluate_performance(data_mean, "Mean"),
  evaluate_performance(data_reg, "Regression"),
  evaluate_performance(data_pert, "Perturbation"),
  evaluate_performance(data_indicator, "Binary Indicator"),
  evaluate_performance(data_dropped, "Dropped Rows")
)

# FIX: Explicitly print the individual accuracies to the console
cat("\n--- Individual Model Accuracies ---\n")
print(results)

# ---------------------------------------------------------
# 4. Visualization with quartz()
# ---------------------------------------------------------
if (exists("quartz")) {
  quartz(title = "Mean Imputation Density", width = 10, height = 7)
}
p_mean <- ggplot(data_mean, aes(x = Bare_Nuclei)) +
  geom_density(fill = "#69b3a2", alpha = 0.6) +
  labs(title = "Density: Mean Imputation", subtitle = "Artificial spike at x=3.5", x = "Bare Nuclei") + theme_minimal() # nolint

print(p_mean)

if (exists("quartz")) {
  quartz(title = "Regression Imputation Density", width = 10, height = 7)
}
p_reg <- ggplot(data_reg, aes(x = Bare_Nuclei)) +
  geom_density(fill = "#404080", alpha = 0.6) +
  labs(title = "Density: Regression Imputation", subtitle = "Predicted values without noise", x = "Bare Nuclei") + theme_minimal() # nolint
print(p_reg)

if (exists("quartz")) {
  quartz(title = "Perturbation Imputation Density", width = 10, height = 7)
}
p_pert <- ggplot(data_pert, aes(x = Bare_Nuclei)) +
  geom_density(fill = "#f0a500", alpha = 0.6) +
  labs(title = "Density: Regression with Perturbation", subtitle = "Maintains natural variance/noise", x = "Bare Nuclei") + theme_minimal() # nolint
print(p_pert)

# Plot 1: Comparison of Imputed Distributions
if (exists("quartz")) {
  quartz(title = "Imputation Density Comparison", width = 10, height = 7)
}
p1 <- ggplot(data_pert, aes(x = Bare_Nuclei)) +
  geom_density(aes(fill = "Perturbation"), alpha = 0.4) +
  geom_density(data = data_reg, aes(fill = "Linear Regression"), alpha = 0.4) +
  geom_density(data = data_mean, aes(fill = "Mean"), alpha = 0.4) +
  labs(title = "Imputation Density Comparison", x = "Value", y = "Density") +
  theme_minimal()
print(p1)

# Plot 2: Model Accuracy Comparison
if (exists("quartz")) {
  quartz(title = "Model Accuracy Comparison", width = 10, height = 7)
}
p2 <- ggplot(results, aes(x = Method, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(ylim = c(0.9, 1.0)) + # Zooms in without removing bars
  scale_y_continuous(labels = scales::label_percent()) + # Updated function call
  labs(title = "Classification Accuracy by Imputation Method", 
       subtitle = "KNN vs SVM Comparison", y = "Accuracy (%)") +
  theme_minimal() + 
  scale_fill_manual(values = c("KNN" = "#377eb8", "SVM" = "#e41a1c"))
print(p2)


