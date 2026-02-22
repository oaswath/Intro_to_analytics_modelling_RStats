# Load necessary libraries
library(kernlab)
library(tidyr)
library(dplyr)
library(caret)
library(DAAG)
library(ggplot2)
library(smooth)

rm(list = ls())

# 1. Load data and perform PCA
data <- read.table("uscrime.txt", header = TRUE)
# PCA with scaling (essential for variables with different units)
pca <- prcomp(data[, 1:15], scale. = TRUE)

# 2. Scree Plot to see variance explained
plot(pca, type = "l", main = "Scree Plot")
summary(pca) # 6 components cover ~90% variance
print(summary(pca))

# 3. Build Regression Model using top 6 components
# pca$x contains the scores (coordinates) for each city in the PC space
model_pca <- lm(data$Crime ~ pca$x[, 1:6])
summary(model_pca)
print(summary(model_pca))

# 4. Extract Coefficients and Transform back to original variables
beta_pca <- coef(model_pca)[-1]
print(beta_pca)
alpha_pca <- coef(model_pca)[1]
print(alpha_pca)
V <- pca$rotation[, 1:6]  # The rotation/loadings matrix
print(V) 

# Scaling factors from PCA
s <- pca$scale
m <- pca$center
print(s)
print(m)

# Calculation: beta_orig = V %*% beta_pca / s
beta_orig <- V %*% beta_pca / s
# Calculation: alpha_orig = alpha_pca - sum(beta_orig * m)
intercept_orig <- alpha_pca - sum(beta_orig * m)

# 5. Display the final model equation coefficients
cat("Original Variable Coefficients:\n")
print(beta_orig)
cat("Intercept:", intercept_orig, "\n")

# 6. Prediction for New City
new_city <- c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 
              1.1, 0.120, 3.6, 3200, 20.1, 0.04, 39.0)

prediction <- intercept_orig + sum(beta_orig * new_city)
cat("PCA Model Prediction for New City:", prediction, "\n")
