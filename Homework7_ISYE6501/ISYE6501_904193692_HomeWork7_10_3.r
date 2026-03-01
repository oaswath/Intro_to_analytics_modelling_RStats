# Load necessary libraries
library(kernlab)
library(tidyr)
library(dplyr)
library(caret)
library(DAAG)
library(pROC)
library(broom)
library(ggplot2)
library(smooth)

rm(list = ls())
while (!is.null(dev.list())) dev.off()

# 1: LOAD THE DATA
# The file "german.data" has no header and uses spaces as separators
german_data <- read.table("german.data", header = FALSE, sep = " ")

# 2. ASSIGN READABLE COLUMN NAMES
# Mapping based on dataset documentation
colnames(german_data) <- c(
  "CheckingAccount", "Duration", "CreditHistory", "Purpose", "CreditAmount",
  "Savings", "EmploymentSince", "InstallmentRate", "PersonalStatus", "OtherDebtors", # nolint
  "ResidenceSince", "Property", "Age", "OtherInstallments", "Housing",
  "ExistingCredits", "Job", "NumberLiable", "Telephone", "ForeignWorker", "Response" # nolint
)

# 3. Re-code response: 1 (Good) -> 0, 2 (Bad) -> 1
german_data$risk <- ifelse(german_data$Response == 2, 1, 0)

# 4: RUN THE MODEL
# We remove column 21 (V21) because it's the old response variable
logit_model <- glm(risk ~ ., data = german_data[, -21], family = binomial(link = "logit")) # nolint

# Display the software output
summary(logit_model)
print(logit_model)
print(logit_model$coefficients)
print(logit_model$deviance)

# ==========================================
# 5. Plot Quality of Fit (ROC Curve)
# ==========================================
# This plots the Trade-off between Sensitivity and Specificity

prob <- predict(logit_model, type = "response")
roc_obj <- roc(german_data$risk, prob)
roc_value <- auc(roc_obj)

quartz(width=7, height=7)  # nolint

# Use pROC's specific base-plot method to bypass ggplot conflicts
pROC::plot.roc(roc_obj,  # nolint
               main = paste("ROC Curve (AUC =", round(pROC::auc(roc_obj), 3), ")"),  # nolint
               col = "blue",  # nolint
               lwd = 3,  # nolint
               print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "red")


# 6.  Calculate Sensitivity and Specificity at a threshold
# We use 0.5 as the standard threshold for this specific table
# or you can use your custom threshold (0.167)
threshold <- 0.5  # nolint
prob <- predict(logit_model, type = "response")
pred_values <- ifelse(prob > threshold, "Predicted Bad", "Predicted Good")
actual_values <- ifelse(german_data$risk == 1, "Actual Bad", "Actual Good")

# Create the table
conf_matrix <- table(pred_values, actual_values)

# Convert table to a data frame for ggplot
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

# Fix for "depth" error: Clear devices and use explicit sizing
# if (dev.cur() > 1) dev.off()  # nolint
quartz(width = 7, height = 6)

p_conf <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  # Add the frequency numbers in the center of the tiles
  geom_text(aes(label = Freq), size = 8, color = "black") +
  # Use a nice color gradient (Blue for high frequency)
  scale_fill_gradient(low = "white", high = "#3498db") +
  labs(title = paste("Confusion Matrix (Threshold =", threshold, ")"),
       subtitle = "Comparing Predicted Credit Risk vs. Actual Outcome",
       x = "Actual Observation", 
       y = "Model Prediction") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        legend.position = "none") # Remove legend as numbers are on tiles

print(p_conf)

# Create a confusion matrix manually to get Sens and Spec
tab <- table(Predicted = pred_values, Actual = german_data$risk)
print(tab)

# Sensitivity = TP / (TP + FN) # nolint
sens_value <- tab[2,1] / sum(tab[2,])  # nolint

# Specificity = TN / (TN + FP) # nolint
spec_value <- tab[1,2] / sum(tab[1,]) # nolint

# Format and Print the Table
results_table <- data.frame(
  ROC = roc_value,
  Sens = sens_value,
  Spec = spec_value
)

cat("\nModel Performance Results with Threshold =", threshold, ":\n\n")
print(results_table, row.names = FALSE)

# 6.  Calculate Sensitivity and Specificity at a threshold
# We use 0.5 as the standard threshold for this specific table
# or you can use your custom threshold (0.167)
Cfp <- 1
Cfn <- 5
threshold <- Cfp / (Cfp + Cfn)  # This gives us 0.167 as the threshold
prob <- predict(logit_model, type = "response")
pred_values <- ifelse(prob > threshold, "Predicted Bad", "Predicted Good")
actual_values <- ifelse(german_data$risk == 1, "Actual Bad", "Actual Good")

# Create the table
conf_matrix <- table(pred_values, actual_values)

# Convert table to a data frame for ggplot
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

# Fix for "depth" error: Clear devices and use explicit sizing
# if (dev.cur() > 1) dev.off()  # nolint
quartz(width = 7, height = 6)

p_conf <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  # Add the frequency numbers in the center of the tiles
  geom_text(aes(label = Freq), size = 8, color = "black") +
  # Use a nice color gradient (Blue for high frequency)
  scale_fill_gradient(low = "white", high = "#3498db") +
  labs(title = paste("Confusion Matrix (Threshold =", threshold, ")"),
       subtitle = "Comparing Predicted Credit Risk vs. Actual Outcome",
       x = "Actual Observation", 
       y = "Model Prediction") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12, face = "bold"),
        legend.position = "none") # Remove legend as numbers are on tiles

print(p_conf)

# Create a confusion matrix manually to get Sens and Spec
tab <- table(Predicted = pred_values, Actual = german_data$risk)
print(tab)

# Sensitivity = TP / (TP + FN) # nolint
sens_value <- tab[2,1] / sum(tab[2,])  # nolint

# Specificity = TN / (TN + FP) # nolint
spec_value <- tab[1,2] / sum(tab[1,]) # nolint

# Format and Print the Table
results_table <- data.frame(
  ROC = roc_value,
  Sens = sens_value,
  Spec = spec_value
)

cat("\nModel Performance Results with Threshold =", threshold, ":\n\n")
print(results_table, row.names = FALSE)


# ==========================================
# 6. Plot Coefficients & Significance
# ==========================================
# We use tidy() to turn the model summary into a dataframe for ggplot
model_data <- tidy(logit_model)

# 7. Optional: Clean up term names for the plot
# This replaces things like "CheckingAccountA12" with "CheckingAccount: Mid-Balance" # nolint
model_data$term <- gsub("CheckingAccount", "CheckAcc: ", model_data$term)
model_data$term <- gsub("CreditHistory", "History: ", model_data$term)
model_data$term <- gsub("Savings", "Savings: ", model_data$term)

# 8. Plot using quartz()
quartz(width=10, height=8) # nolint
p_coef <- ggplot(model_data, aes(x = reorder(term, estimate), y = estimate)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_point(aes(color = p.value < 0.05), size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) + # nolint
  coord_flip() +
  scale_color_manual(values = c("black", "blue"), labels = c("Not Sig", "Significant")) + # nolint
  labs(title = "Logistic Regression Coefficients", 
       subtitle = "Predictors named by Category",
       x = "Predictor Factors (Named)", y = "Log-Odds Estimate") +
  theme_minimal()

print(p_coef)

# ==========================================
# 9 Define 10-fold Cross Validation
# ==========================================

# 1. Update the re-coding to use text labels instead of 0/1
# We use "Bad" for 1 and "Good" for 0
german_data$risk_factor <- factor(ifelse(german_data$Response == 2, "Bad", "Good"),  # nolint
                                  levels = c("Good", "Bad"))

# 2. Update the caret control
# caret needs these text labels to name the probability columns internally
ctrl <- trainControl(method = "cv",  # nolint
                     number = 10,  # nolint
                     savePredictions = "all",  # nolint
                     classProbs = TRUE,  # nolint
                     summaryFunction = twoClassSummary) # Added for binary classification metrics # nolint

# 3. Update the train function call
# We use risk_factor instead of as.factor(risk)
set.seed(123)
cv_model <- train(risk_factor ~ .,  # nolint
                  data = german_data[, -c(21, 22)], # Remove 'Response' and 'risk' columns # nolint
                  method = "glm",  # nolint
                  family = "binomial",  # nolint
                  metric = "ROC", # Metric for two-class summary
                  trControl = ctrl)

print(cv_model)

# Identify only the predictor columns (1 through 20)
predictors <- german_data[, 1:20]

# Now predict using only those columns
prob_df <- predict(cv_model, newdata = predictors, type = "prob")
prob_bad <- prob_df$Bad

threshold <- 0.5 # Or 0.167 for cost-optimization
pred_values <- ifelse(prob_bad > threshold, "Predicted Bad", "Predicted Good")
actual_values <- ifelse(german_data$risk == 1, "Actual Bad", "Actual Good")


conf_matrix <- table(Predicted = pred_values, Actual = actual_values)
conf_df <- as.data.frame(conf_matrix)

# FIX: Ensure conf_df is not empty before plotting
if(nrow(conf_df) > 0) {
  p_conf <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 10, fontface = "bold") +
    scale_fill_gradient(low = "#e1f5fe", high = "#01579b") +
    labs(title = paste("Confusion Matrix (Threshold =", threshold, ")"),
         x = "Actual Outcome", y = "Model Prediction") +
    theme_minimal()
  
  print(p_conf)
}

threshold <- 0.167 # Or 0.167 for cost-optimization
pred_values <- ifelse(prob_bad > threshold, "Predicted Bad", "Predicted Good")
actual_values <- ifelse(german_data$risk == 1, "Actual Bad", "Actual Good")


conf_matrix <- table(Predicted = pred_values, Actual = actual_values)
conf_df <- as.data.frame(conf_matrix)

# FIX: Ensure conf_df is not empty before plotting
if(nrow(conf_df) > 0) {
  p_conf <- ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 10, fontface = "bold") +
    scale_fill_gradient(low = "#e1f5fe", high = "#01579b") +
    labs(title = paste("Confusion Matrix (Threshold =", threshold, ")"),
         x = "Actual Outcome", y = "Model Prediction") +
    theme_minimal()
  
  print(p_conf)
}