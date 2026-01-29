
library(kernlab)
library(kknn) # nolint
library(ggplot2)
library(tidyr)

find_svm <- function(dataset, c_list, kernel_type, sigma_list = NULL) {
  accuracies <- c()
  fpr_list <- c()
  fnr_list <- c()

  labels <- as.factor(dataset[,11])

  if (kernel_type == "vanilladot") {
    print("Using Linear Kernel")
    for (val in c_list) {
      print(val)
      # 1. Call ksvm using the new 'df' variable
      model <- ksvm(as.matrix(dataset[,1:10]),
            as.factor(dataset[,11]),  # nolint
            type = "C-svc",  # nolint
            kernel = kernel_type,  # nolint
            C = val,  # nolint
            scaled = TRUE)
      # 2. Calculate a1...am
      a <- colSums(model@xmatrix[[1]] * model@coef[[1]]) # nolint
      # 3. see what the model predicts
      pred <- predict(model, dataset[,1:10])
      # 4. Create Confusion Matrix
      # Rows = Predicted, Cols = Actual
      conf_matrix <- table(Predicted = pred, Actual = labels)
      # 5. Extract Metrics
      # TN is [row 1, col 1] (Pred 0, Actual 0)
      # FP is [row 2, col 1] (Pred 1, Actual 0)
      # We use tryCatch or basic indexing logic:
      tn <- conf_matrix[1, 1]
      fp <- conf_matrix[2, 1]
      fpr <- fp / (fp + tn)
      # FN is [row 1, col 2] (Pred 0, Actual 1)
      # TP is [row 2, col 2] (Pred 1, Actual 1)
      # We use tryCatch or basic indexing logic:
      fn <- conf_matrix[1, 2]
      tp <- conf_matrix[2, 2]
      fnr <- fn / (fn + tp)
      # 6. see what fraction of the model's predictions are correct
      acc <- sum(pred == dataset[,11]) / nrow(dataset)
      accuracies <- c(accuracies, acc)
      fpr_list <- c(fpr_list, fpr)
      fnr_list <- c(fnr_list, fnr)
    }
    return(data.frame(C = c_list, Accuracy = accuracies, FPR = fpr_list, FNR = fnr_list)) # nolint
  } else if (kernel_type == "rbfdot") {
    print("Using RBF Kernel")
    if (is.null(sigma_list)) {
      for (val in c_list) {
        model <- ksvm(as.matrix(dataset[,1:10]),# nolint
                      as.factor(dataset[,11]), # nolint
                      type = "C-svc",
                      kernel = kernel_type,
                      C = val,
                      scaled = TRUE)
        pred <- predict(model, dataset[,1:10])
        conf_matrix <- table(Predicted = pred, Actual = labels)
        tn <- conf_matrix[1, 1]
        fp <- conf_matrix[2, 1]
        fpr <- fp / (fp + tn)
        fn <- conf_matrix[1, 2]
        tp <- conf_matrix[2, 2]
        fnr <- fn / (fn + tp)
        acc <- sum(pred == dataset[,11]) / nrow(dataset)
        accuracies <- c(accuracies, acc)
        fpr_list <- c(fpr_list, fpr)
        fnr_list <- c(fnr_list, fnr)
      }
      return(data.frame(C = c_list, Accuracy = accuracies, FPR = fpr_list, FNR = fnr_list)) # nolint
    } else {
      for (val in c_list) {
        for (s_val in sigma_list) {
          print(paste("C:", val, " Sigma:", s_val))
          model <- ksvm(as.matrix(dataset[,1:10]), 
                        as.factor(dataset[,11]), 
                        type = "C-svc", 
                        kernel = kernel_type, 
                        kpar = list(sigma = s_val), 
                        C = val, 
                        scaled = TRUE)
          pred <- predict(model, dataset[,1:10])
          conf_matrix <- table(Predicted = pred, Actual = labels)
          tn <- conf_matrix[1, 1]
          fp <- conf_matrix[2, 1]
          fpr <- fp / (fp + tn)
          fn <- conf_matrix[1, 2]
          tp <- conf_matrix[2, 2]
          fnr <- fn / (fn + tp)
          acc <- sum(pred == dataset[,11]) / nrow(dataset)
          accuracies <- c(accuracies, acc)
          fpr_list <- c(fpr_list, fpr)
          fnr_list <- c(fnr_list, fnr)
        }
      }
      return(data.frame(C = rep(c_list, each=length(sigma_list)),# nolint
                        Sigma = rep(sigma_list, times=length(c_list)),
                        Accuracy = accuracies,
                        FPR = fpr_list,
                        FNR = fnr_list)) # nolint
    }
  } else {
    stop("Unsupported kernel type")
  }
}

# knn solution 
find_knn <- function(dataset, k_list) {
  k_accuracies <- c()
  results_knn <- data.frame()
  for (k_val in k_list) {
    predictions <- c()
    actuals <- dataset[,11]
    for (i in 1:nrow(dataset)) { # nolint
      # Train on all but row i, test ONLY on row i
      model <- kknn(V11 ~ .,  # nolint
                  train = dataset[-i,],  # nolint
                  test = dataset[i,],  # nolint
                  k = k_val,  # nolint
                  scale = TRUE)
      # kknn returns the probability/fraction in fitted.values
      # Round to 0 or 1 for classification
      pred_val <- as.integer(fitted(model) >= 0.5)
      predictions <- c(predictions, pred_val)
    }
    # Calculate accuracy for this k
    acc <- sum(predictions == dataset[,11]) / nrow(dataset)
    k_accuracies <- c(k_accuracies, acc)
    # Generate Confusion Matrix
    # Rows = Predicted, Cols = Actual
    conf_matrix <- table(Predicted = predictions, Actual = actuals)
    # Extract Confusion Matrix components
    # Note: Actual 0 is col 1, Actual 1 is col 2
    tn <- conf_matrix[1, 1] # Pred 0, Actual 0
    fp <- conf_matrix[2, 1] # Pred 1, Actual 0
    fn <- conf_matrix[1, 2] # Pred 0, Actual 1
    tp <- conf_matrix[2, 2] # Pred 1, Actual 1
    # Calculate Metrics
    fpr <- fp / (fp + tn) # False Positives / All Actual Negatives # nolint
    fnr <- fn / (fn + tp) # False Negatives / All Actual Positives # nolint
    results_knn <- rbind(results_knn, data.frame(k = k_val, 
                                               Accuracy = acc,  # nolint
                                               FPR = fpr,  # nolint
                                               FNR = fnr))
    cat("k =", k_val, " Acc:", round(acc, 3), " FPR:", round(fpr, 3), " FNR:", round(fnr, 3), "\n") # nolint
  }
  best_k_row <- results_knn[which.max(results_knn$Accuracy), ]
  print(paste("Best k:", best_k_row$k, "with Accuracy:", best_k_row$Accuracy))
  return(results_knn) # nolint
}


# Loading the dataset

df <- read.table("/Users/aswathoruganti/OMSA-GTech/Intro_to_Analytics_Modelling/Homework1_ISYE6501/credit_card_data.txt",  # nolint
                 stringsAsFactors = FALSE,  # nolint
                 header = FALSE)  # nolint

# Analysis with SVM for kernel type "vanilladot"
my_c_list <- c(100, 0.1, 1, 10, 100, 200, 300, 500, 1000, 10000)
kernel_type <- "vanilladot" # Radial Basis Function Kernel

results_vanilla <- find_svm(df, my_c_list, kernel_type)
print(results_vanilla)

# Assuming 'results' is the data frame from your loop
# It has columns: C, Accuracy, FPR, FNR
# We "pivot" it to make it ggplot-friendly
plot_data_1 <- pivot_longer(results_vanilla, 
                          cols = c("Accuracy", "FPR", "FNR"), 
                          names_to = "Metric", 
                          values_to = "Value")

vanilla_dot_acc_fpr_fnr_plot <- ggplot(plot_data_1, aes(x = C, y = Value, color = Metric)) + # nolint
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_log10() + # Important for C values like 0.001 to 1000
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.05)) +
  labs(title = "SVM Performance Trade-offs",
       subtitle = "Vanilla Dot Accuracy, False Positive Rate, and False Omission Rate vs Cost", # nolint
       x = "Cost (C) - Log Scale",
       y = "Rate")  +
  theme_minimal() +
  theme(legend.position = "bottom") # Moves legend for better visibility

quartz(title="Vanilla Metrics") # Title helps keep track
print(vanilla_dot_acc_fpr_fnr_plot)
Sys.sleep(1) # Pause to view the plot


# # # Analysis with SVM for kernel type "rbfdot"
my_c_list <- c(100, 0.1, 1, 10, 100, 200, 300, 500, 1000, 10000)
kernel_type <- "rbfdot" # Radial Basis Function Kernel

results_rbf <- find_svm(df, my_c_list, kernel_type)
print(results_rbf)

# # Assuming 'results' is the data frame from your loop
# # It has columns: C, Accuracy, FPR, FNR
# # We "pivot" it to make it ggplot-friendly
plot_data_2 <- pivot_longer(results_rbf, 
                          cols = c("Accuracy", "FPR", "FNR"),  # nolint
                          names_to = "Metric", 
                          values_to = "Value")

rbfdot_acc_fpr_fnr_plot <- ggplot(plot_data_2, aes(x = C, y = Value, color = Metric)) + # nolint
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_log10() + # Important for C values like 0.001 to 1000
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.05)) +
  labs(title = "SVM Performance Trade-offs",
       subtitle = "RBFDOT Accuracy, False Positive Rate, and False Omission Rate vs Cost", # nolint
       x = "Cost (C) - Log Scale",
       y = "Rate")  +
  theme_minimal() +
  theme(legend.position = "bottom") # Moves legend for better visibility

quartz(title="RBF Metrics")
print(rbfdot_acc_fpr_fnr_plot)
Sys.sleep(1)  # Pause to view the plot

# Merge the two dataframes
results_vanilla$Model <- "Vanilla"
results_rbf$Model <- "RBF"
combined_results <- rbind(results_vanilla, results_rbf)

# Plot
combined_plot <- ggplot(combined_results, aes(x = C, y = Accuracy, color = Model, linetype = Model)) + # nolint
  geom_line(size = 1) +
  geom_point() +
  scale_x_log10() +
  scale_y_continuous(limits = c(0.7, 1), breaks = seq(0.7, 1, by = 0.05)) +
  labs(title = "Comparison: Vanilla vs RBF Kernel",
       subtitle = "Accuracy vs Cost (C)",
       x = "Cost (C) - Log Scale",
       y = "Training Accuracy") +
  theme_minimal()

quartz(title="Kernel Comparison")
print(combined_plot)
Sys.sleep(1)  # Pause to view the plot

# KNN Analysis
# Define range of k to test
k_list <- c(1:25)
results_knn <- find_knn(df, k_list)
print(results_knn)

# Plot KNN Results
# Reshape data for plotting
plot_data_knn <- pivot_longer(results_knn, 
                             cols = c("Accuracy", "FPR", "FNR"),  # nolint
                             names_to = "Metric", 
                             values_to = "Value")

# Create Plot
knn_plot <- ggplot(plot_data_knn, aes(x = k, y = Value, color = Metric)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(title = "k-NN Performance Metrics",
       subtitle = "Accuracy, FPR, and FNR across different k-values",
       x = "Number of Neighbors (k)",
       y = "Rate") +
  theme_minimal()

quartz(title="k-NN Metrics")
print(knn_plot)
