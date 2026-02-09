# Load necessary libraries
library(kernlab)
library(tidyr)
library(dplyr)
library(ggplot2)
library(smooth)

rm(list = ls())
# 1. Load the data
data <- read.table("temps.txt", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) # nolint

# 2. Reshape the data into a single time series
# We remove the "DAY" column and flatten the matrix into one vector
# The vector will have 123 days * 20 years = 2460 observations
temp_vector <- as.vector(as.matrix(data[, -1]))

# Create a time series object with a frequency of 123 (days per year)
temp_ts <- ts(temp_vector, start = 1996, frequency = 123)

# 3. Build the Exponential Smoothing Model
# Using HoltWinters (Base R). We use additive seasonality for temperature.
# Note: beta=FALSE can be used if you assume no long-term trend, 
# but we keep it TRUE to see if a trend exists.
hw_model <- HoltWinters(temp_ts)

# 4. Extract Smoothed Values
# HoltWinters fitted values start from the second year (1997)
# xhat is the smoothed estimate of the temperature
smoothed_values <- hw_model$fitted[, "xhat"]

# Reshape the smoothed values back into a matrix (123 days by 19 years)
num_years <- length(smoothed_values) / 123
smoothed_matrix <- matrix(smoothed_values, nrow = 123)

# 5. Identify the "End of Summer" (EoS) each year
# We define EoS as the last day index where temp >= 85
find_eos <- function(year_data, threshold = 85) {
  indices <- which(year_data >= threshold)
  if (length(indices) == 0) return(NA)
  return(max(indices)) # nolint
}

eos_indices <- apply(smoothed_matrix, 2, find_eos, threshold = 85)
years <- 1997:(1997 + num_years - 1)

# 6. Visualize the Results

# a. Prepare the data for ggplot (requires a data frame)
plot_data <- data.frame(
  Year = years,
  EoS_Index = eos_indices
)

# b. Open the Mac-specific Quartz graphics device
quartz(width = 8, height = 6)

# c. Create the ggplot object
p <- ggplot(plot_data, aes(x = Year, y = EoS_Index)) +
  # Add points and a line (equivalent to type="b")
  geom_line(color = "blue", alpha = 0.5) +
  geom_point(color = "blue", size = 3) +
  # Add the linear trend line (equivalent to abline(lm(...)))
  geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 1.2) +
  # Add labels and titles
  labs(
    title = "Unofficial End of Summer in Atlanta using HoltWinters (1997-2015)",
    subtitle = "Metric: Last day with smoothed High Temp >= 85°F",
    x = "Year",
    y = "Day Index (July 1st = 1)"
  ) +
  # Use a clean theme and add grid lines
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# d. Display the plot in the Quartz window
print(p)

# 7. Print the Trend
model_trend <- lm(eos_indices ~ years)
summary(model_trend)
print(summary(model_trend))

# Alternatively Impmentation using the 'smooth' package:
# -----------------------------------------------------
# 3. Build the Exponential Smoothing Model using 'smooth' package
es_model <- es(temp_ts, model="AAM") # Additive Error/Trend, Multiplicative Seasonality # nolint

# 4. Extract Smoothed Values
# es_model$fitted provides the smoothed values (xhat) for the entire series
smoothed_values <- es_model$fitted

# Reshape into a matrix: 123 rows (days) by 20 columns (years 1996-2015)
num_years <- length(smoothed_values) / 123
smoothed_matrix <- matrix(smoothed_values, nrow = 123)

# 5. Identify the "End of Summer" (EoS) each year
find_eos <- function(year_data, threshold = 85) {
  indices <- which(year_data >= threshold)
  if (length(indices) == 0) return(NA)
  return(max(indices)) # nolint
}

# Apply the function to each column (year)
eos_indices <- apply(smoothed_matrix, 2, find_eos, threshold = 85)
years <- 1996:(1996 + num_years - 1)

# 6. Visualize the Results

# a. Prepare the data for ggplot
plot_data_es <- data.frame(
  Year = years,
  EoS_Index = eos_indices
)

# b. Open the Mac-specific Quartz graphics device
quartz(width = 8, height = 6)

# c. Create the ggplot object
p_es <- ggplot(plot_data_es, aes(x = Year, y = EoS_Index)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_point(color = "blue", size = 3) +
  # Add the linear trend line
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE, linewidth = 1.2) + # nolint
  labs(
    title = "Unofficial End of Summer in Atlanta using smooth::es (1996-2015)",
    subtitle = "Model: smooth::es (AAM) | Metric: Last day with smoothed High Temp >= 85°F",
    x = "Year",
    y = "Day Index (July 1st = 1)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# d. Display the plot
print(p_es)

# 7. Print the Trend Summary
model_trend <- lm(eos_indices ~ years)
summary(model_trend)
print(summary(model_trend))