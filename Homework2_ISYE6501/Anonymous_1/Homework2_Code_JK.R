#Clear environment
rm(list = ls())

#Question 3.1a

#Run packages necessary for running the code for Question 3.1(a)
library(kknn)

#Import credit_card_data file
data <- read.table("C:/Users/neojo/OneDrive/바탕 화면/ISYE 6501/Homework2_ISYE6501/data 3.1/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#Setting seed for reproducibility
set.seed(1)

model_cv <- train.kknn(V11 ~ ., data = data, kmax = 20, scale = TRUE)

model_cv

knn_preds <- round(predict(model_cv, data))
knn_preds
knn_accuracy <- sum(knn_preds == data[, 11]) / nrow(data)
knn_accuracy




#Question 3.1b

#Run packages necessary for running the code for Question 3.1(a)
library(kknn)

#Import credit_card_data file
data <- read.table("C:/Users/neojo/OneDrive/바탕 화면/ISYE 6501/Homework2_ISYE6501/data 3.1/credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

#Check data
head(data)

#Setting seed for reproducibility
set.seed(1)

#Split data to train/validation/test 60/20/20
trainData <- sample(1:nrow(data), size = floor(0.6*nrow(data)))
train <- data[trainData,]
remaining <- data[-trainData,]
valData <- sample(1:nrow(remaining), size = floor(0.5*nrow(remaining)))
val <- remaining[valData,]
test <- remaining[-valData,]

#Train dataset using kknn model and validate 
results <- data.frame(k = 1:20, accuracy = NA)
for (i in 1:20){
  model_val <- kknn(V11 ~., train, val, k = i, scale = TRUE)
  pred_val <- round(fitted(model_val))
  results$accuracy[i] <- sum(pred_val == val$V11) / nrow(val)
}

#Find optimal kknn model on validation data
best_k <- which.max(results$accuracy)
cat("The optimal value of k is:", results$k[best_k], "with an accuracy of", results$accuracy[best_k])

#Run test dataset using optimal k value found from validation set
model_test <- kknn(V11 ~., train, test, k = best_k, scale = TRUE)
pred_test <- round(fitted(model_test))
accuracy_test <- sum(pred_test == test$V11) / nrow(test)
cat("Final test accuracy is:", accuracy_test)




#Question 4.2
data <- read.table("C:/Users/neojo/OneDrive/바탕 화면/ISYE 6501/Homework2_ISYE6501/data 4.2/iris.txt", header = TRUE)
head(data)

#Setting seed for reproducibility
set.seed(1)

#Scale data
data_scaled <- as.data.frame(scale(data[, 1:4]))

#Use elbow method to confirm number of clusters
wcss <- c()
for (k in 1:10) {
  model <- kmeans(data_scaled, centers = k, nstart = 20)
  wcss[k] <- model$tot.withinss
}

#Plot the data: number of clusters vs. total wcss
plot(1:10, wcss, type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "Total Within-Cluster Sum of Squares", main = "Elbow Method: Iris")

#Determine the best combination of predictors
predictors <- 1:4
for (i in 1:4) {
  combos <- combn(predictors, i)
  for (j in 1:ncol(combos)) {
    current_cols <- combos[, j]
    combo_names <- paste(colnames(data)[current_cols], collapse = " + ")
    current_combn <- data_scaled[, current_cols]
    model <- kmeans(current_combn, centers = 3, nstart = 20)
    ct_table <- table(model$cluster, data$Species)
    accuracy <- sum(apply(ct_table, 1, max)) / nrow(iris)
    cat(combo_names, "\nAccuracy:", accuracy)
    print(ct_table)
  }
}

#Plot the best pair of predictors
library(ggplot2)
ggplot(data, aes(Petal.Length, Petal.Width, color = model$cluster)) + geom_point()