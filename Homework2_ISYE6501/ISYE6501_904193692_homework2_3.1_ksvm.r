
library(kernlab)
library(kknn) # nolint
library(ggplot2)
library(tidyr)

run_ksvm_analysis <- function(df, current_seed, current_split) {
  # 1. Split the data into Train, Validate, and Test
  set.seed(current_seed)
   # nolint
  if(length(current_split) == 3) { #nolint  
    names(current_split) <- c("train", "validate", "test")
  } else {
    stop(paste("Split vector must have 3 elements. Current length is:", length(current_split))) # nolint
  }

  g = sample(cut( # nolint
    seq(nrow(df)),  # nolint
    nrow(df)*cumsum(c(0, current_split)), # nolint
    labels = names(current_split)
  ))

  res = split(df, g) # nolint

  # 2. Parameters for SVM
  fold_values <- seq(5, 50, by = 5) 
  c_values    <- c(0.001, 0.01, 0.1, 1, 10, 100) # Range of Cost parameters to tune # nolint
   # nolint
  test_accuracies <- rep(0, length(fold_values))
  test_fpr        <- rep(0, length(fold_values))
  test_fnr        <- rep(0, length(fold_values))
  best_c_per_fold <- rep(0, length(fold_values))

  # Combine train and validate for cross-validation
  cv_data <- rbind(res$train, res$validate)
  cv_data$V11 <- as.factor(cv_data$V11) # ksvm requires factors for classification # nolint

  # 3. SVM Cross-Validation to find best C
  for (i in 1:length(fold_values)) { # nolint
    current_f <- fold_values[i]
    c_accuracies <- rep(0, length(c_values))

    for (j in 1:length(c_values)) { # nolint
      # ksvm built-in cross validation (returns error rate)
      model_cv <- ksvm(V11 ~ ., data = cv_data, 
                       type = "C-svc",  # nolint
                       kernel = "vanilladot",  # nolint
                       C = c_values[j],  # nolint
                       cross = current_f,  # nolint
                       scaled = TRUE)
       # nolint
      # accuracy = 1 - cross validation error
      c_accuracies[j] <- 1 - model_cv@cross
    }

    # Identify the best C based on CV results
    best_c_per_fold[i] <- c_values[which.max(c_accuracies)]
    cat("Optimal C found via Cross-Validation:", best_c_per_fold[i], "at", current_f, "folds\n") # nolint

    # 4. FINAL EVALUATION on the TEST SET
    # Re-train on Training Set using the best C discovered
    final_model <- ksvm(V11 ~ ., data = cv_data,
                        type = "C-svc", 
                        kernel = "vanilladot",  # nolint
                        C = best_c_per_fold[i],  # nolint
                        scaled = TRUE)
     # nolint
    test_preds <- predict(final_model, res$test[, 1:10])
     # nolint
    # Confusion Matrix logic
    actuals_factor <- factor(res$test$V11, levels = c(0, 1))
    preds_factor   <- factor(test_preds, levels = c(0, 1))
    cm <- table(Predicted = preds_factor, Actual = actuals_factor)

    TN <- cm[1,1]; FP <- cm[1,2]; FN <- cm[2,1]; TP <- cm[2,2] # nolint

    # Calculate and store metrics
    test_accuracies[i] <- (TP + TN) / sum(cm)
    test_fpr[i] <- FP / max(1, (FP + TN))
    test_fnr[i] <- FN / max(1, (FN + TP))
     # nolint
    cat("Folds:", current_f, "| Best C:", best_c_per_fold[i], "| Acc:", round(test_accuracies[i], 4), "\n") # nolint
  }

  # 4. Create a data frame for ggplot
  plot_data <- data.frame(
    Folds = fold_values,
    Accuracy = test_accuracies,
    FPR = test_fpr,
    FNR = test_fnr
  )

  plot_data_long <- pivot_longer(plot_data, 
                                 cols = c("Accuracy", "FPR", "FNR"),  # nolint
                                 names_to = "Metric",  # nolint
                                 values_to = "Value")

  if (exists("quartz")) quartz(title="SVM Vanilla-dot Folds Performance Metrics",width = 8, height = 6) # nolint

  # Add this check right before your ggplot code
  if (is.null(plot_data_long) || nrow(plot_data_long) == 0) {
    stop("Error: plot_data_long is NULL or empty. Check your pivot_longer step.") # nolint
  }

  # 5. THE FIX: Open device ONLY if the data is valid # nolint
  p <- ggplot(plot_data_long, aes(x = Folds, y = Value, color = Metric, shape = Metric)) + # nolint
    geom_line(linewidth = 1) + geom_point(size = 3) + # nolint
    scale_color_manual(values = c("Accuracy" = "darkblue", "FPR" = "red", "FNR" = "darkgreen")) + # nolint
    labs(title = paste("SVM Performance | Split:", paste(current_split, collapse="/"), "| Seed:", current_seed), # nolint
         subtitle = "Vanilla Kernel (Linear) | Rates calculated on independent Test Set", # nolint
         x = "Number of Cross-Validation Folds (kcv)",  # nolint
         y = "Rate Value / Percentage") +
    theme_minimal() # nolint
     # nolint
  print(p) # nolint
  # nolint
  # Final Summary Output
  best_idx <- which.max(test_accuracies)
  final_best_c <- best_c_per_fold[best_idx]
  # nolint
  # Re-run final model for matrix printout
  final_svm <- ksvm(V11 ~ ., data = res$train, type = "C-svc", kernel = "vanilladot", C = final_best_c, scaled = TRUE) # nolint
  final_predictions <- predict(final_svm, res$test[, 1:10])
  final_cm <- table(Predicted = final_predictions, Actual = res$test$V11)

  cat("\n--- Final SVM Model Confusion Matrix ---\n")
  cat("Best C used:", final_best_c, "\n\n")
  print(final_cm)
}

# --- Execution ---
df <- read.table("/Users/aswathoruganti/OMSA-GTech/Intro_to_Analytics_Modelling/Homework1_ISYE6501/credit_card_data.txt",  # nolint
                 stringsAsFactors = FALSE, header = FALSE)

seed_list <- c(123, 20260124)
split_list <- list(c(0.6, 0.2, 0.2), c(0.7, 0.15, 0.15))

for (current_split in split_list) {
  for (current_seed in seed_list) {
    cat("\n==========================================\n")
    cat("SVM ANALYSIS FOR SPLIT:", paste(current_split, collapse = "/"), "| SEED:", current_seed, "\n") # nolint
    cat("==========================================\n")
    run_ksvm_analysis(df, current_seed, current_split)
  }
}