##### ISYE6501 HW2

# loading packages and datasets
library(kernlab)
library(kknn)
library(caret)

data = read.table("Homework2_ISYE6501/data 3.1/credit_card_data.txt")

# making sure V11 is a factor for classification
data$V11 <- as.factor(data$V11)

set.seed(1105)

##### 3.1 (a) cross validation
# split into two data sets, one (0.8) for cross validation and training,
# another (0.2) for testing
train_index <- createDataPartition(data$V11, p = 0.8, list = FALSE)
data1 <- data[train_index, ]
data2 <- data[-train_index, ]

# Step 2: Set up 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

knn_model <- train(
  V11 ~ ., 
  data = data1,
  method = "kknn",
  trControl = train_control,
  tuneGrid = expand.grid(
    kmax = seq(1, 50, by = 2),
    distance = 2,
    kernel = "optimal"
  )
)

print(knn_model)

svm_model <- train(
  V11 ~ ., 
  data = data1,
  method = "svmRadial", # ksvm's RBF kernel
  trControl = train_control,
  tuneGrid = expand.grid(
    C = c(0.001, 0.01, 0.1, 1, 10, 100),
    sigma = c(0.001, 0.01, 0.1, 1, 10)
    )
)

print(svm_model)

# caret automatically trains on whole data, so knn_model and svm_model are
# already trained on data1. Now we need to test them on data2

# Test KNN on data2
knn_predictions <- predict(knn_model, newdata = data2)
confusionMatrix(knn_predictions, data2$V11)

# Test SVM on data2
svm_predictions <- predict(svm_model, newdata = data2)
confusionMatrix(svm_predictions, data2$V11)


##### 3.1 (b) regular validation
# set.seed(1105) # seed already set on top

# Step 1: Split into 3 sets
n <- nrow(data)

#set train to 60%
train_idx <- sample(1:n, size = 0.6 * n)
train_set <- data[train_idx, ]

# for the rest 40%, split into two equal sets -> validation and test
rem <- data[-train_idx, ]
val_idx <- sample(1:nrow(rem), size = 0.5 * nrow(rem))

val_set <- rem[val_idx, ]
test_set <- rem[-val_idx, ]

# Step 2: Define hyperparameters to test
c_vals <- c(0.001, 0.01, 0.1, 1, 10, 100)
sigma_vals <- c(0.001, 0.01, 0.1, 1, 10)

# Step 3: Store results
results <- data.frame(C = numeric(), sigma = numeric(), accuracy = numeric())

# Step 4: Loop through all combinations
for (c in c_vals) {
  for (sig in sigma_vals) {
    
    # Train on train_set
    model <- ksvm(
      V11 ~ .,
      data = train_set,
      type = "C-svc",
      kernel = "rbfdot",
      C = c,
      kpar = list(sigma = sig),
      scaled = TRUE
    )
    
    # predict on validation set
    pred <- predict(model, val_set)
    
    # calculate accuracy
    acc <- sum(pred == val_set$V11) / nrow(val_set)
    
    # store result
    results <- rbind(results, data.frame(C = c, sigma = sig, accuracy = acc))
  }
}

# Step 5: Find best combination
print(results)
best_comb <- results[which.max(results$accuracy), ] # row that has the best accuracy
print(best_comb)

# Step 6: Train final model with best parameters (on train set only, not validation)
test_model <- ksvm(
  V11 ~ .,
  data = train_set,
  type = "C-svc",
  kernel = "rbfdot",
  C = best_comb$C,
  kpar = list(sigma = best_comb$sigma),
  scaled = TRUE
)

# Step 7: Test on test_set
test_pred <- predict(test_model, test_set)
test_accuracy <- sum(test_pred == test_set$V11) / nrow(test_set)
print(test_accuracy)


##### 4.2
data(iris)

str(iris)
head(iris)

# see how many species there are
unique(iris$Species)

# to find the best combinations, need to find all combinations and test
predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
combination_list <- list()
for (i in 1:length(predictors)) {
  combs <- combn(predictors, i, simplify = FALSE)
  combination_list <- c(combination_list, combs)
}

print(combination_list)

results_kmeans <- data.frame(
  predictors = character(),
  k = numeric(),
  tot_withinss = numeric(),
  accuracy = numeric()
)

# test each combination
set.seed(1105)

for (comb in combination_list) {
  for (i in 2:7) {
    data_used <- iris[, comb, drop = FALSE]
    
    # calculate accuracy by comparing clusters to actual species
    model_kmeans <- kmeans(data_used, centers = i, nstart = 30)
    mat <- table(model_kmeans$cluster, iris$Species) # matrix to see which cluster number represents which species
    accuracy <- sum(apply(mat, 1, max)) / nrow(iris)
    
    # store results to df
    results_kmeans <- rbind(results_kmeans, data.frame(
        predictors = paste(comb, collapse = ", "),
        k = i,
        tot_withinss = model_kmeans$tot.withinss,
        accuracy = accuracy
      )
    )
    
  }
}

print(results_kmeans)

# first, find combinations that yields best accuracy
best_acc <- results_kmeans[which.max(results_kmeans$accuracy), ]
print(best_acc)

# since there are only 3 species, narrow down to k=3
results_k3 <- results_kmeans[results_kmeans$k == 3, ]
print(results_k3)
best_acc_k3 <- results_k3[which.max(results_k3$accuracy), ]
print(best_acc_k3)

# draw graph for both of the results to see the 'elbow'
par(mfrow = c(1, 2))

combo1 <- results_kmeans[results_kmeans$predictors == best_acc$predictors, ]
plot(combo1$k, combo1$tot_withinss,
     type = "b",
     xlab = "k",
     ylab = "total within ss",
     main = best_acc$predictors)

combo2 <- results_kmeans[results_kmeans$predictors == best_acc_k3$predictors, ]
plot(combo2$k, combo2$tot_withinss,
     type = "b",
     xlab = "k",
     ylab = "total within ss",
     main = best_acc_k3$predictors)









