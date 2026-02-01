
library(kernlab)
library(kknn) # nolint
library(ggplot2)
library(tidyr)
library(GGally)
library(outliers)
library(EnvStats)
library(car)


# Load data 
df <- read.table("/Users/aswathoruganti/OMSA-GTech/Intro_to_Analytics_Modelling/Homework3_ISYE6501/uscrime.txt",  # nolint
                 stringsAsFactors = FALSE, header = TRUE)  # nolint

print(df$Crime)
crime_data <- df$Crime

# ---------------------------------------------------------
# METHOD 1: IQR (Boxplot Method)
# ---------------------------------------------------------
# Calculate Quantiles
q1 <- quantile(crime_data, 0.25)
q3 <- quantile(crime_data, 0.75)
iqr_val <- q3 - q1

# Define Bounds
lower_bound <- q1 - 1.5 * iqr_val
upper_bound <- q3 + 1.5 * iqr_val

# Identify Outliers
iqr_outliers <- crime_data[crime_data < lower_bound | crime_data > upper_bound]

cat("--- IQR Method --- \n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")
cat("Outliers detected by IQR:", iqr_outliers, "\n\n")

# OPEN QUARTZ WINDOW 1
if (exists("quartz")) quartz(title="1. IQR Boxplot", width = 8, height = 6)
boxplot(crime_data, main = "Boxplot: IQR Outlier Detection", col = "lightblue", horizontal = TRUE) # nolint


# ---------------------------------------------------------
# METHOD 2: MAD (Median Absolute Deviation)
# ---------------------------------------------------------
# Calculate Median and MAD
med_val <- median(crime_data)
mad_val <- mad(crime_data, constant = 1.4826) # 1.4826 is default for normality

# Calculate Z-scores for MAD (Standard is > 3)
threshold <- 3
mad_scores <- abs(crime_data - med_val) / mad_val
mad_outliers <- crime_data[mad_scores > threshold]

cat("--- MAD Method --- \n")
cat("Median:", med_val, "\n")
cat("MAD Value:", mad_val, "\n")
cat("Outliers detected by MAD (Threshold > 3):", mad_outliers, "\n")

#PLOTTING METHOD : MAD
# ---------------------------------------------------------

# a. Create a Quartz window
if (exists("quartz")) quartz(title="MAD Outlier Detection", width = 8, height = 6) # nolint

# b. Plot the MAD Scores for every data point
plot(mad_scores, 
     main = "MAD Outlier Detection (Threshold = 3)", 
     xlab = "Index (Row Number)", 
     ylab = "MAD Z-Score", 
     pch = 19,            # Solid circle points
     col = ifelse(mad_scores > threshold, "red", "darkgray"), # Highlight outliers in red # nolint
     ylim = c(0, max(mad_scores) + 1))

# c. Add a horizontal threshold line
abline(h = threshold, col = "red", lty = 2, lwd = 2)

# d. Label the threshold line
text(x = 5, y = threshold + 0.2, "Threshold = 3", col = "red", pos = 4)

# e. Label the specific outliers by their row index
outlier_indices <- which(mad_scores > threshold)
text(outlier_indices, mad_scores[outlier_indices], 
     labels = outlier_indices, pos = 3, cex = 0.8, col = "red")

# METHOD 3. Statistical Tests for Normality + Visual test
cat("\n--- Shapiro-Wilk Test for Normality --- \n")

# a. Set your desired confidence (e.g., 99%)
confidence_level <- 0.99
alpha <- 1 - confidence_level  # This equals 0.01

# b. Run the test
s_test <- shapiro.test(crime_data)
p_val <- s_test$p.value

# c. Use the new threshold for your logic
cat("Shapiro-Wilk P-value:", p_val, "\n")
cat("Testing at", confidence_level * 100, "% confidence (alpha =", alpha, ")\n")

if (p_val < alpha) {
  cat("RESULT: Reject Null Hypothesis. Data is NOT normal.\n")
} else {
  cat("RESULT: Fail to Reject Null Hypothesis. Data appears normal.\n")
}

if (exists("quartz")) quartz(title="3. Shapiro-Wilk Confidence Envelope", width = 8, height = 6) # nolint
qqPlot(crime_data, 
       main = paste("Q-Q Plot (p =", p_val, ")"),
       id = list(n = 3),  # Label the top 3 extreme points
       col.lines = "red")


if (exists("quartz")) quartz(title="Shapiro-Wilk Visualization", width = 8, height = 6) # nolint
par(mfrow = c(1, 1)) # 1 row, 2 columns

# --- PLOT : Density Plot vs. Normal Curve ---
# Plot actual data shape
d <- density(crime_data)
plot(d, main = "Density Plot vs. Normal Curve", 
     xlab = "Crime Rate", lwd = 2, col = "blue")
polygon(d, col = rgb(0, 0, 1, 0.2)) # Fill the data area

# Overlay the theoretical Normal Distribution (Bell Curve)
x_seq <- seq(min(crime_data), max(crime_data), length = 100)
y_normal <- dnorm(x_seq, mean = mean(crime_data), sd = sd(crime_data))
lines(x_seq, y_normal, col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("Your Data", "Normal Curve"), 
       col = c("blue", "red"), lty = 1:2, bty = "n")

# Reset layout
par(mfrow = c(1, 1))

# METHOD 4. Grubbs' Test for Outliers
cat("\n--- Grubbs' Test for Outliers --- \n")

# a. Start with your original data
current_data <- df$Crime
outliers_found <- c()
continue_testing <- TRUE

# b. Loop until no more outliers are detected
while(continue_testing) {
  test <- grubbs.test(current_data)
  print(test$p.value)
  if(test$p.value < 0.08) {  # Using 0.08 significance level
    # Extract the outlier value from the test result
    # test$statistics[1] gives the G value; we want the actual value
    # A simple way is to find the value furthest from the mean
    val <- current_data[which.max(abs(current_data - mean(current_data)))]
     # nolint
    outliers_found <- c(outliers_found, val)
     # nolint
    # Remove the outlier for the next round
    current_data <- current_data[current_data != val]
     # nolint
    cat("Detected and removed outlier:", val, "| P-value:", test$p.value, "\n")
    # OPEN A NEW QUARTZ WINDOW FOR EVERY OUTLIER FOUND
    if (exists("quartz")) {
      quartz(title=paste("4. Grubbs Outlier:", val), width = 6, height = 4) # nolint
      plot(current_data, main=paste("Data after removing", val)) # nolint
    }
  } else {
    cat("No more outliers detected. Final P-value:", test$p.value, "\n")
    continue_testing <- FALSE
  }
}

# c. Final Result
cat("All detected outliers:", outliers_found, "\n")


# ---------------------------------------------------------
# METHOD 5: Rosner's Test
# ---------------------------------------------------------
cat("--- Rosner's Test --- \n")
rosner_result <- rosnerTest(crime_data, k = 5)
print(rosner_result)

# OPEN QUARTZ WINDOW 5
if (exists("quartz")) quartz(title="5. Rosner's Test Summary", width = 8, height = 6) # nolint
# Rosner doesn't have a default plot, so we plot the results table 
# or a simple dotplot highlighting the flagged k values
dotchart(crime_data, main="Rosner's Test Candidates", xlab="Crime Rate")
abline(v = rosner_result$all.stats$Value[rosner_result$all.stats$Outlier], col="red", lty=2) # nolint

