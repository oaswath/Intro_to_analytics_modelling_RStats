# Load necessary libraries
library(kernlab)
library(tidyr)
library(dplyr)
library(caret)
library(DAAG)
library(ggplot2)
library(smooth)

rm(list = ls())
# 1. Load the data
# Assuming uscrime.txt is in your working directory
data <- read.table("uscrime.txt", header = TRUE)

# 2. Build the Full Model
model_full <- lm(Crime ~ ., data = data)

# Display full model summary
print(summary(model_full))

# 3. Predict for the New City
new_city <- data.frame(
  M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
  LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120,
  U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0
)

pred_full <- predict(model_full, new_city)
cat("Predicted Crime (Full Model):", pred_full, "\n")

# 4. Variable Selection (Stepwise or p-value based)
# Using stepwise selection (AIC-based) to find a better model automatically
model_step <- step(model_full, direction = "both", trace = 0)

# Alternatively, a manual reduced model based on significant factors (p < 0.1)
# Factors identified: M, Ed, Po1, U2, Ineq, Prob
model_red <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data)

print(summary(model_red))

# 5. Predict with the Reduced Model
pred_red <- predict(model_red, new_city)
cat("Predicted Crime (Reduced Model):", pred_red, "\n")

# 6. Compute AIC
aic_full <- AIC(model_full)
aic_red  <- AIC(model_red)

# 7. Compute BIC
bic_full <- BIC(model_full)
bic_red  <- BIC(model_red)

# 8. Print the Comparison
cat("AIC Comparison:\n")
cat("Full Model: ", aic_full, "\n")
cat("Reduced Model: ", aic_red, "\n\n")

cat("BIC Comparison:\n")
cat("Full Model: ", bic_full, "\n")
cat("Reduced Model: ", bic_red, "\n")

# 9. Cross-Validation
# Option A: Using the DAAG package for simple CV visualization
cv_full <- cv.lm(data = data, form.lm = model_full, m = 5) # 5-fold CV
cv_red  <- cv.lm(data = data, form.lm = model_red, m = 5)

# Option B: Using Caret for robust MSE calculation
train_control <- trainControl(method = "cv", number = 5)

cv_model_full <- train(Crime ~ ., data = data, method = "lm", trControl = train_control) # nolint
cv_model_red  <- train(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data, method = "lm", trControl = train_control) # nolint

print(cv_model_full)
print(cv_model_red)

# Compare RMSE (Root Mean Squared Error)
cat("Full Model CV RMSE:", cv_model_full$results$RMSE, "\n")
cat("Reduced Model CV RMSE:", cv_model_red$results$RMSE, "\n")

# 10. Compute AIC
cv_aic_full <- AIC(cv_model_full$finalModel)
cv_aic_red  <- AIC(cv_model_red$finalModel)

# 11. Compute BIC
cv_bic_full <- BIC(cv_model_full$finalModel)
cv_bic_red  <- BIC(cv_model_red$finalModel)

# 12. Print the Comparison
cat("AIC Comparison:\n")
cat("Full Model: ", cv_aic_full, "\n")
cat("Reduced Model: ", cv_aic_red, "\n\n")

cat("BIC Comparison:\n")
cat("Full Model: ", cv_bic_full, "\n")
cat("Reduced Model: ", cv_bic_red, "\n")


