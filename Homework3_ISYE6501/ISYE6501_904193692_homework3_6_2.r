# Load necessary libraries
library(kernlab)
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list = ls())
# 1. Load the data
data <- read.table("temps.txt", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) # nolint
years <- colnames(data)[2:21] # Extract year column names (1996-2015)

# 2. Establish Baseline (July and August across all years)
# We find rows containing "Jul" or "Aug"
summer_rows <- grep("-Jul|-Aug", data$DAY)
summer_matrix <- as.matrix(data[summer_rows, years])
mu_summer <- mean(summer_matrix)
sd_summer <- sd(summer_matrix)

cat("Global Summer Baseline (Mean):", round(mu_summer, 2), "\n")

# ---------------------------------------------------------
# PART 1: Detect End of Summer (Cooling Trend)
# ---------------------------------------------------------
C1 <- 2        # Slack: ignore drops smaller than 2 degrees # nolint
T1 <- 150      # Threshold: cumulative sum needed to signal "Fall" # nolint

results_cooling <- data.frame(Year = character(), EndDay = character(), stringsAsFactors = FALSE) # nolint

for (year in years) {
  temps <- data[[year]]
  s_curr <- 0
  detected_day <- "Not Found"
   # nolint
  for (i in 1:length(temps)) { # nolint
    # CUSUM for decrease: S_t = max(0, S_{t-1} + (Target - Actual - Slack))
    s <- max(0, s_curr + (mu_summer - temps[i] - C1))
    s_curr <- s
     # nolint
    if (s > T1) {
      detected_day <- data$DAY[i]
      break
    }
  }
  results_cooling <- rbind(results_cooling, data.frame(Year = year, EndDay = detected_day)) # nolint
}

print("--- End of Summer Results ---")
print(results_cooling)

# ---------------------------------------------------------
# PLOTTING WITH DYNAMIC LABELS
# ---------------------------------------------------------


# 1. Timeline Plot for End of Summer Dates
if (exists("quartz")) quartz(title=paste0("Timeline T=", T1, " C=", C1), width=10, height=6) # nolint

# 1. Normalize the dates to a single "dummy year" (e.g., 2000) 
# This ensures all years can be plotted on the same Y-axis scale
results_cooling$NormDate <- as.Date(paste(results_cooling$EndDay, "2000"), format="%d-%b %Y") # nolint

# 2. Set the limits using that same dummy year
# 2. Set the limits using that same dummy year
y_min <- as.Date("2000-09-01")
y_max <- as.Date("2000-10-31")


p1 <- ggplot(results_cooling, aes(x = Year, y = NormDate)) +
  geom_point(color = "darkorange", size = 3) +
  geom_line(group = 1, color = "darkorange", alpha = 0.5, linewidth = 1) +
  # 3. Use scale_y_date to set the specific limits and format
  scale_y_date(
    limits = c(y_min, y_max),
    date_labels = "%d-%b",  # Keeps labels looking like "01-Sep"
    date_breaks = "2 weeks" # Optional: adjusts how many labels show up
  ) +
  theme_minimal() +
  labs(title = "End of Unofficial Summer in Atlanta",
       subtitle = paste0("Parameters: Threshold (T) = ", T1, ", Slack (C) = ", C1), # nolint
       x = "Year", 
       y = "Detected Date")

print(p1)

# Plot 2: Combined CUSUM Trace Plot (All Years)
# a. Create a "long" data frame to store CUSUM values for ALL years
all_traces <- data.frame()

for (year in years) {
  temps_yr <- data[[year]]
  s_vec <- numeric(length(temps_yr))
  s_curr <- 0
   # nolint
  for(i in 1:length(temps_yr)) { # nolint
    s_curr <- max(0, s_curr + (mu_summer - temps_yr[i] - C1))
    s_vec[i] <- s_curr
  }
   # nolint
  # Append this year's data to the master data frame
  year_df <- data.frame(
    Day = 1:length(s_vec),  # nolint
    CUSUM = s_vec, 
    Year = year,
    Date = data$DAY
  )
  all_traces <- rbind(all_traces, year_df)
}

if (exists("quartz")) quartz(title=paste0("All years compared against Threshold T = ", T1), width=10, height=6) # nolint

# b. Plot all years together
p2_combined <- ggplot(all_traces, aes(x = Day, y = CUSUM, color = Year)) +
  # Use a lower alpha (transparency) so overlapping lines are visible
  geom_line(linewidth = 0.8, alpha = 0.6) +  # nolint
  geom_hline(yintercept = T1, linetype = "dashed", color = "black", linewidth = 1) + # nolint
  # Use scale_x_continuous to show dates on the bottom
  scale_x_continuous(breaks = seq(1, 123, 15), labels = data$DAY[seq(1, 123, 15)]) + # nolint
  theme_minimal() +
  labs(title = "Overlay: CUSUM Cooling Traces (1996-2015)",
       subtitle = paste0("All years compared against Threshold T = ", T1),
       x = "Date", 
       y = "Cumulative Sum (S_t)") +
  # Optional: Hide legend if 20 colors make it too cluttered
  theme(legend.position = "right")  # nolint

print(p2_combined)

#---------------------------------------------------------
#PART 2: Detect Summer Warming Trend (1996-2015) # nolint
#---------------------------------------------------------
#Calculate the mean temperature for July-August for each year
yearly_summer_means <- colMeans(data[summer_rows, years])

C2 <- 0.5    # Slack: look for shifts larger than 0.5 degrees # nolint
T2 <- 5      # Threshold for detecting a climate shift # nolint

s_warming <- numeric(length(years) + 1)
s_warming[1] <- 0

for (i in 1:length(yearly_summer_means)) { # nolint
  # CUSUM for increase: S_t = max(0, S_{t-1} + (Actual - Target - Slack))
  s_warming[i+1] <- max(0, s_warming[i] + (yearly_summer_means[i] - mu_summer - C2)) # nolint
}

# Create a data frame for the Warming Trend results
warming_df <- data.frame(
  Year = as.numeric(years),
  MeanTemp = yearly_summer_means,
  CUSUM = s_warming[-1]
)

print("--- Warming Trend CUSUM ---")
print(warming_df)

if (exists("quartz")) quartz(title=paste0("CUSUM SUMMER Warming Trend for ATLANTA OVER 1996-2015 (C = ", C2, ", T = ", T2, ")"), width=10, height=6) # nolint

# 3. Visualization of Warming Trend
p3 <- ggplot(warming_df, aes(x = Year, y = CUSUM)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = T2, linetype = "dashed", color = "red") +
  annotate("text", x = 1998, y = T2 + 0.5, label = "Threshold (T=5)", color = "red") + # nolint
  labs(title = "CUSUM for Atlanta Summer Warming Trend (1996-2015)",
       subtitle = "Detection of sustained increase in summer temperatures",
       x = "Year", y = "Cumulative Sum") +
  theme_minimal()

print(p3)