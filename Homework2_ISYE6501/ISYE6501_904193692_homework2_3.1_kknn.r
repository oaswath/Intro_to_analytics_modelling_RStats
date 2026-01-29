
library(kernlab)
library(kknn) # nolint
library(ggplot2)
library(tidyr)


run_kknn_analysis <- function(df, current_seed, current_split) {
  # 1. Split the data into Train (60%), Validate (20%), Test (20%)

  set.seed(current_seed)
   # nolint
  # FIX: Ensure the vector has 3 elements before assigning names
  if(length(current_split) == 3) {
    names(current_split) <- c("train", "validate", "test")
  } else {
    stop(paste("Split vector must have 3 elements. Current length is:", length(current_split))) # nolint
  }

  # c(0, current_split) creates the breakpoints at 0, 0.6, 0.8, 1
  # nrow(df)*cumsum(...) scales them to the number of rows in df
  # cut(...) assigns each row to one of the groups based on these breakpoints
  # sample(...) shuffles the group assignments randomly
  g = sample(cut( # nolint
    seq(nrow(df)), # nolint
    nrow(df)*cumsum(c(0, current_split)),
    labels = names(current_split)
  ))

  res = split(df, g) # nolint

  # 2. Define cross-validation function for SVM and kNN
  fold_values <- seq(5, 50, by = 5) # Folds for cross-validation
  test_accuracies <- rep(0, length(fold_values))
  test_fpr <- rep(0, length(fold_values))
  test_fnr <- rep(0, length(fold_values))
  best_k_per_fold <- rep(0, length(fold_values))
  #k_folds <- 10  # Number of folds for cross-validation # nolint
  max_neighbors <- 25  # Maximum number of neighbors for kNN

  # 3. Perform SVM and kNN analysis on training set with cross-validation
  # This finds the best C for SVM and best k for kNN using cross-validation
  train_data <- res$train # Training set (60% of data)
  validate_data <- res$validate # Validation set (20% of data)
  test_data <- res$test # Test set (20% of data) # nolint
  # Combine train and validate for cross-validation
  cv_data <- rbind(train_data, validate_data)

  # kNN Cross-Validation to find best k
  for (i in 1:length(fold_values)) { # nolint
    # nolint
    current_f <- fold_values[i] # nolint
    neighbor_accuracies <- rep(0, max_neighbors)  # Initialize accuracy storage for kNN # nolint
    # nolint
    for (k_neighbors in 1:max_neighbors) { # nolint
      # cv.kknn performs k-fold cross validation internally # nolint
      model_cv <- cv.kknn(V11 ~ ., cv_data, kcv = current_f, k = k_neighbors, scale = TRUE) # nolint
      # Predictions are in the first element, second column (for classification)
      cv_preds <- round(model_cv[[1]][,2])
      neighbor_accuracies[k_neighbors] <- sum(cv_preds == cv_data$V11) / nrow(cv_data) # nolint
    } # nolint

    # Identify the best k based on CV results
    best_k_per_fold[i] <- which.max(neighbor_accuracies)
    cat("Optimal k found via Cross-Validation:", best_k_per_fold[i], "\n")

    # 4. Evaluate on the VALIDATION SET (to confirm the CV results)
    val_model <- kknn(V11 ~ ., res$train, res$validate, k = best_k_per_fold[i], scale = TRUE) # nolint
    val_preds <- round(predict(val_model))
    val_acc <- sum(val_preds == res$validate$V11) / nrow(res$validate) # nolint

    # 5. FINAL EVALUATION on the TEST SET (the "Gold Standard" Accuracy)
    final_model <- kknn(V11 ~ ., res$train, res$test, k = best_k_per_fold[i], scale = TRUE) # nolint
    test_preds <- round(predict(final_model))
    test_accuracies[i] <- sum(test_preds == res$test$V11) / nrow(res$test)
    cat("Test Set Accuracy with k =", best_k_per_fold[i], "is", round(test_accuracies[i], 3), "\n\n") # nolint

    # Force the predictions and actuals to be factors with both levels (0 and 1)
    actuals_factor <- factor(res$test$V11, levels = c(0, 1))
    preds_factor   <- factor(test_preds, levels = c(0, 1))

    # This guarantees a 2x2 table even if a class is missing
    cm <- table(Predicted = preds_factor, Actual = actuals_factor)  
    # nolint
    # Extract components for rates
    # Note: cm[row, col] -> Actual is row, Predicted is column
    TN <- cm[1,1] # nolint
    FP <- cm[1,2] # nolint
    FN <- cm[2,1] # nolint
    TP <- cm[2,2] # nolint
    # nolint
    # Calculate and store metrics
    test_accuracies[i] <- (TP + TN) / sum(cm)
    test_fpr[i] <- FP / (FP + TN)
    test_fnr[i] <- FN / (FN + TP)
    # nolint
    # Optional: Print progress
    cat("Folds:", current_f, "| Best k:", best_k_per_fold[i], "| Acc:", round(test_accuracies[i], 4), "\n") # nolint

  }

  # 4. Create a data frame for ggplot
  plot_data <- data.frame(
    Folds = fold_values, # nolint
    Accuracy = test_accuracies,
    FPR = test_fpr,
    FNR = test_fnr
  )

  # 5. Reshape data from 'wide' to 'long' format
  # This puts all rates in one column so ggplot can color-code them automatically # nolint
  plot_data_long <- pivot_longer(plot_data, 
                               cols = c("Accuracy", "FPR", "FNR"), 
                               names_to = "Metric", 
                               values_to = "Value")

  split_label <- paste(current_split, collapse = "/")
  # 6. Open a high-quality graphics device (MacOS specific)
  # Use windows() on Windows or x11() on Linux if not on a Mac
  if (exists("quartz")) quartz(title="k-NN CV Folds Performance Metrics",width = 8, height = 6) # nolint

  # Add this check right before your ggplot code
  if (is.null(plot_data_long) || nrow(plot_data_long) == 0) {
    stop("Error: plot_data_long is NULL or empty. Check your pivot_longer step.") # nolint
  }

  # 7. Create the ggplot object
  p <- ggplot(plot_data_long, aes(x = Folds, y = Value, color = Metric, shape = Metric)) + # nolint
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Accuracy" = "darkblue", "FPR" = "red", "FNR" = "darkgreen")) + # nolint
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(title = paste("k-NN Performance: Split", split_label, "| Seed:", current_seed), # nolint
       subtitle = "Rates calculated on independent Test Set", # nolint
       x = "Number of Folds (kcv)",
       y = "Rate Value / Percentage") +
    theme_minimal() +
    theme(legend.position = "right")

  # 8. Print the plot to the active device
  print(p)

  # Identify overall best
  best_idx <- which.max(test_accuracies)
  cat("Best k_folds:", fold_values[best_idx], "yielded Test Accuracy:", test_accuracies[best_idx], "\n") # nolint

  # 9. Retrieve the best k and best fold count from your results
  final_best_k <- best_k_per_fold[best_idx]
  final_best_folds <- fold_values[best_idx]

  # 10. Re-train the model using the optimal k on the training set
  # and predict on the test set
  final_model <- kknn(V11 ~ ., 
                    train = res$train,  # nolint
                    test = res$test,  # nolint
                    k = final_best_k,  # nolint
                    scale = TRUE)

  # 11. Generate predictions
  final_predictions <- round(predict(final_model))

  # 12. Create and Print the Confusion Matrix
  final_cm <- table(Predicted = final_predictions, Actual = res$test$V11)

  cat("\n--- Final Model Confusion Matrix ---\n")
  cat("Best k_fold count used:", final_best_folds, "\n")
  cat("Best Neighbors (k) used:", final_best_k, "\n\n")
  print(final_cm)

  # 13. Calculate specific counts for your report
  TN <- final_cm[1,1] # nolint
  FP <- final_cm[1,2] # nolint
  FN <- final_cm[2,1] # nolint
  TP <- final_cm[2,2] # nolint

  cat("\nSummary Statistics:\n")
  cat("True Positives (Correct Approvals):", TP, "\n")
  cat("False Positives (Bad Approvals):", FP, "\n")
  cat("False Negatives (Wrong Denials):", FN, "\n")
  cat("True Negatives (Correct Denials):", TN, "\n")
}

# Loading the dataset

df <- read.table("/Users/aswathoruganti/OMSA-GTech/Intro_to_Analytics_Modelling/Homework1_ISYE6501/credit_card_data.txt",  # nolint
                 stringsAsFactors = FALSE,  # nolint
                 header = FALSE)  # nolint

#1. Manually set aside 20% for the final test set 
seed_list <- c(123, 20260124) # You can change the seed value here

split_list <- list(c(0.6, 0.2, 0.2),
                   c(0.7, 0.15, 0.15))

for (current_split in split_list) {
    for (current_seed in seed_list) { # nolint
   # nolint
    cat("\n==========================================\n")
    cat("ANALYSIS FOR SPLIT RATIO:", paste(current_split, collapse = "/"), "\n")
    cat("==========================================\n")

    cat("\n==========================================\n")
    cat("STARTING ANALYSIS FOR SEED:", current_seed, "\n")
    cat("==========================================\n")

    run_kknn_analysis(df, current_seed, current_split = current_split)
  }
}