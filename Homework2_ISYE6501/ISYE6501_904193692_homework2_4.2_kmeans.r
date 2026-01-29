
library(kernlab)
library(kknn) # nolint
library(ggplot2)
library(tidyr)
library(GGally)


# Load data
data(iris)
p_pairs <- ggpairs(iris, 
                   columns = 1:4, 
                   aes(color = Species, alpha = 0.5),
                   upper = list(continuous = wrap("cor", size = 3)),
                   lower = list(continuous = wrap("points", size = 0.5))) +
           theme_minimal() # nolint

# 2. Open a large Quartz window for better visibility (macOS)
if (exists("quartz")) {
  quartz(title = "Iris Feature Matrix", width = 10, height = 8)
}

# 3. Print the plot
print(p_pairs)

run_kmeans_analysis <- function(df_full, columns_to_use) {

  iris_features <- df_full[, columns_to_use]
  feat_names <- paste(colnames(iris_features), collapse = ", ")
  col_label <- paste(columns_to_use, collapse = ":")

  cat("\n", rep("=", 50), "\n", sep="")
  cat("ANALYSIS FOR PREDICTORS:", col_label, "(", feat_names, ")\n")
  cat(rep("=", 50), "\n", sep="")

  # 1. Elbow Method to find optimal K
  set.seed(123)
  wcss_vector <- sapply(1:10, function(k) {
    model <- kmeans(iris_features, centers = k, nstart = 20)
    return(model$tot.withinss) # nolint
  })

  # 2. THE FIX: Explicitly check for NULLs before making the dataframe
  if (is.null(wcss_vector)) {
    stop("WCSS calculation failed. Check if iris_features exists.")
  }

  # 3. Create the data frame with explicit naming
  elbow_df <- data.frame(Clusters = 1:10, WCSS = wcss_vector)

  # 4. Build and Print
  # Note: Use the new 'elbow_df' name to avoid conflicts with previous code
  p_elbow <- ggplot(elbow_df, aes(x = Clusters, y = WCSS)) + # nolint
    geom_line() + geom_point(size = 3) +
    geom_vline(xintercept = 3, linetype = "dashed", color = "red") +
    labs(title = paste("Elbow Method (Predictors:", col_label, ")"),
         subtitle = "Red line indicates suggested k=3",
         x = "Number of Clusters (k)", y = "Total Within-Cluster SS") +
    theme_minimal()

  # 5. Use Quartz for high-res rendering (MacOS)
  if (exists("quartz")) quartz(title=paste("Elbow Plot -", col_label), width=8, height=6) # nolint
  print(p_elbow)

  # 6. Run Final K-Means with k=3
  best_cluster <- kmeans(iris_features, centers = 3, nstart = 20)
   # nolint
  # 7. Performance Evaluation
  comparison_table <- table(Actual = df_full$Species, Cluster = best_cluster$cluster) # nolint
  print("Confusion Matrix (Species vs Cluster):")
  print(comparison_table)
   # nolint
  # Accuracy Calculation (Handling Label Mismatch)
  accuracy <- sum(apply(comparison_table, 1, max)) / nrow(df_full)
  cat("Clustering Accuracy for", col_label, ":", round(accuracy * 100, 2), "%\n") # nolint
   # nolint
  # 9. Cluster Visualization
  # We always plot against Petal Length/Width for visual comparison of separation # nolint
  plot_df <- df_full
  plot_df$Cluster <- as.factor(best_cluster$cluster)

  p_map <- ggplot(plot_df, aes(x = Petal.Length, y = Petal.Width,  # nolint
                                 color = Cluster, shape = Species)) + # nolint
    geom_point(size = 3, alpha = 0.8) +
    labs(title = paste("K-Means Map (Used Features:", col_label, ")"),
         subtitle = "Visualized on Petal Dimensions",
         x = "Petal Length", y = "Petal Width") +
    theme_minimal()
   # nolint
  if (exists("quartz")) quartz(title=paste("Cluster Map -", col_label), width=8, height=6) # nolint
  print(p_map)

  # 10. Accuracy Check: How well did the 4-predictor model do?
  comparison <- table(Actual = iris$Species, Cluster = best_cluster$cluster)
  cat("\n--- Confusion Matrix (4 Predictors) ---\n")
  print(comparison)

  #11. Calculate purity/accuracy
  accuracy_all <- sum(apply(comparison, 1, max)) / nrow(iris)
  cat("\nOverall Clustering Accuracy:", round(accuracy_all * 100, 2), "%\n")

}

# 1. Define the list of predictor combinations
predictor_list <- list(1:4, 3:4)

# 2. Execute the loop
for (cols in predictor_list) {
  run_kmeans_analysis(iris, cols)
}

