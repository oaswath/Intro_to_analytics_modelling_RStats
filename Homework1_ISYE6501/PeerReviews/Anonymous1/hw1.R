# Load required libraries
library(kernlab)
library(kknn)

## >>>>>>>> Question 2.2.1 >>>>>>>>>>>>>>>
raw_data <- read.delim("~/Downloads/Homework1_ISYE6501/data 2.2/credit_card_data-headers.txt", header = TRUE)
data <- data.matrix(raw_data)
# call ksvm.  Vanilladot is a simple linear kernel.
model <- ksvm(data[, 1:10], data[, 11], type = "C-svc", kernel = "vanilladot", C = 100, scaled = TRUE)

# calculate a1…am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- -model@b
a0

# see what the model predicts
pred <- predict(model, data[, 1:10])

# check accuracy
sum(pred == data[, 11]) / nrow(data)

# Create a new data frame showing first 10 actual labels next to their predictions
comparison <- data.frame(Actual = data[1:10, 11], Predicted = pred[1:10])
print(comparison)


# trying different C values
c_values <- c(0.0001, 0.01, 1, 100, 10000, 1000000)
accuracies <- c()

for (C_val in c_values) {
  model_temp <- ksvm(data[, 1:10], data[, 11], type = "C-svc", kernel = "vanilladot", C = C_val, scaled = TRUE)
  pred_temp <- predict(model_temp, data[, 1:10])
  acc <- sum(pred_temp == data[, 11]) / nrow(data)
  accuracies <- c(accuracies, acc)
  print(paste("C =", C_val, "Accuracy =", acc))
}

# plot the results
png("svm_c_values.png", width=800, height=600)
plot(c_values, accuracies, type = "b", log = "x", xlab = "C Value", ylab = "Accuracy", main = "SVM Accuracy vs C Value")
dev.off()


# Create the confusion matrix table
conf_matrix <- table(Predicted = pred, Actual = data[, 11])
print(conf_matrix)

# Save confusion matrix plot
png("svm_confusion_matrix.png", width=600, height=600)
fourfoldplot(conf_matrix, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix for SVM (C=100)")
dev.off()

## >>>>>>>> Question 2.2.3 >>>>>>>>>>>>>>>
# k-NN part
# need to use leave-one-out approach like the question says
df_data <- as.data.frame(data)
colnames(df_data) <- c(paste0("A", 1:10), "R1")

# test different k values
k_values <- c(1, 3, 5, 7, 9, 11, 13, 15, 20, 25)
k_accuracies <- c()

for (k_val in k_values) {
  predictions <- rep(0, nrow(df_data))

  # loop through each point
  for (i in 1:nrow(df_data)) {
    # use data[-i,] to exclude point i
    model_knn <- kknn(R1 ~ ., train = df_data[-i,], test = df_data[i,], k = k_val, scale = TRUE)
    predictions[i] <- ifelse(model_knn$fitted.values > 0.5, 1, 0)
  }

  acc <- sum(predictions == df_data$R1) / nrow(df_data)
  k_accuracies <- c(k_accuracies, acc)
  print(paste("k =", k_val, "Accuracy =", acc))
}

# find best k
best_k <- k_values[which.max(k_accuracies)]
print(paste("Best k:", best_k))

# plot
png("knn_k_values.png", width=800, height=600)
plot(k_values, k_accuracies, type = "b", xlab = "k", ylab = "Accuracy", main = "k-NN Accuracy vs k Value")
dev.off()