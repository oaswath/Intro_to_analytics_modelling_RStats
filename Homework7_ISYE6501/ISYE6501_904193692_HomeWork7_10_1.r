# Load necessary libraries
library(kernlab)
library(tidyr)
library(dplyr)
library(caret)
library(DAAG)
library(rpart)
library(randomForest)
library(ggplot2)
library(smooth)

rm(list = ls())

# Load data
data <- read.table("uscrime.txt", header = TRUE)

# ==========================================
# 1. PCA and Scree Plot (ggplot)
# ==========================================
pca <- prcomp(data[, 1:15], scale. = TRUE)

# Prepare data for ggplot scree plot
pca_importance <- as.data.frame(summary(pca)$importance)
scree_data <- data.frame(
  PC = 1:ncol(pca_importance), # nolint
  Variance = as.numeric(pca_importance[2, ])
)

quartz() # Open new window on Mac
p1 <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Scree Plot (PCA Variance Explained)", x = "Principal Component", y = "Proportion of Variance") + # nolint
  theme_minimal()
print(p1)

# ==========================================
# 2. Regression Tree Importance (ggplot)
# ==========================================
tree_model <- rpart(Crime ~ ., data = data, method = "anova")
print(tree_model)
print(summary(tree_model))
importance_scores <- tree_model$variable.importance

# Prepare data for ggplot bar chart
tree_imp_df <- data.frame(
  Variable = names(importance_scores),
  Importance = as.numeric(importance_scores)
) %>% arrange(desc(Importance)) # nolint

quartz() # Open new window on Mac
p2 <- ggplot(tree_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) + # nolint
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  coord_flip() +
  labs(title = "Variable Importance (Regression Tree)", x = "Variables", y = "Importance Score") + # nolint
  theme_minimal()
print(p2)

# Note: rpart.plot does not support ggplot conversion directly, 
# so we use quartz() to host the standard rpart.plot in a new window.
quartz()
rpart.plot(tree_model, main = "Unpruned Regression Tree")

opt_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
pruned_tree <- prune(tree_model, cp = opt_cp)
print(pruned_tree)

quartz()
rpart.plot(pruned_tree, main="Pruned Regression Tree for Crime Rate") # nolint
print(summary(pruned_tree))

# ==========================================
# 3. Random Forest Importance (ggplot)
# ==========================================
set.seed(123)
rf_model <- randomForest(Crime ~ ., data = data, importance = TRUE, ntree = 500)

# Extract Random Forest importance for ggplot
rf_imp_df <- as.data.frame(importance(rf_model))
rf_imp_df$Variable <- rownames(rf_imp_df)

print(rf_model)
print(importance(rf_model))

quartz() # Open new window on Mac
p3 <- ggplot(rf_imp_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) + # nolint
  geom_point(size = 4, color = "darkred") +
  geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = IncNodePurity)) + # nolint
  coord_flip() +
  labs(title = "Variable Importance (Random Forest)", x = "Variables", y = "Increase in Node Purity") + # nolint
  theme_minimal()
print(p3)

# ==========================================
# 4. Final Calculations (Console Output)
# ==========================================
# PCA transformed coefficients
beta_pca <- coef(lm(data$Crime ~ pca$x[, 1:6]))[-1]
V <- pca$rotation[, 1:6]
s <- pca$scale
beta_orig <- V %*% beta_pca / s

cat("PCA Original Variable Coefficients:\n")
print(beta_orig)

# RF Variance Explained
cat("Random Forest Variance Explained:", rf_model$rsq[500] * 100, "%\n")