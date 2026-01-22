############################################################
# PM2.5 EXTREME POLLUTION ANALYSIS (2024 DATA)
# Detection and Analysis of Extreme PM2.5 Pollution Events
# Based on Weather Conditions in Sheffield
############################################################

# Clear environment
rm(list = ls())
cat("\n========================================\n")
cat("PM2.5 POLLUTION ANALYSIS (2024)\n")
cat("Sheffield Devonshire Green Station\n")
cat("========================================\n\n")

############################
# 1. PACKAGE INSTALLATION AND LOADING
############################
cat("--- Installing and Loading Required Packages ---\n")

# Function to install and load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Required packages
packages <- c("dplyr", "tidyr", "lubridate", "ggplot2", "stats", "corrplot")

for (pkg in packages) {
  install_and_load(pkg)
}

cat("✓ All packages loaded successfully\n")

############################
# 2. SET WORKING DIRECTORY
############################
cat("\n--- Setting Working Directory ---\n")
# Modify this path to your working directory
setwd("C:/Users/Elangovan/Downloads/PM2.5")
cat("✓ Working directory set\n")

############################
# 3. DATA IMPORT AND PREPROCESSING
############################
cat("\n--- Loading Air Quality Dataset ---\n")

# Load PM2.5 data from OpenAQ
air_quality <- read.csv("data/data.csv", 
                        stringsAsFactors = FALSE)
cat("Loaded:", nrow(air_quality), "raw observations\n")

# Examine structure
str(air_quality)
cat("\nOriginal column names:", paste(names(air_quality), collapse = ", "), "\n")

############################
# 4. DATA CLEANING AND FORMATTING
############################
cat("\n--- Data Cleaning and Formatting ---\n")

# Standardize column names
names(air_quality)[names(air_quality) == "datetimeUtc"] <- "datetime"
names(air_quality)[names(air_quality) == "value"] <- "pm25"

# Convert datetime to POSIXct
air_quality$datetime <- ymd_hms(air_quality$datetime, tz = "UTC")

cat("Date range:", format(min(air_quality$datetime, na.rm = TRUE)), "to",
    format(max(air_quality$datetime, na.rm = TRUE)), "\n")

# Select relevant columns
air_quality <- air_quality %>%
  select(datetime, pm25)

# Remove missing values
air_quality <- na.omit(air_quality)
cat("After removing NA values:", nrow(air_quality), "observations\n")

# Round datetime to hourly intervals for consistency
air_quality$datetime <- as.POSIXct(
  format(air_quality$datetime, "%Y-%m-%d %H:00:00"), 
  tz = "UTC"
)

# Aggregate to hourly means (in case of duplicates)
air_quality <- air_quality %>%
  group_by(datetime) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE)) %>%
  ungroup()

cat("Hourly aggregated data:", nrow(air_quality), "hours\n")

# Add temporal features for analysis
air_quality <- air_quality %>%
  mutate(
    hour = hour(datetime),
    month = month(datetime),
    weekday = wday(datetime, label = TRUE),
    date = as.Date(datetime)
  )

############################
# 5. EXPLORATORY DATA ANALYSIS
############################
cat("\n--- Exploratory Data Analysis ---\n")

# Summary statistics
cat("\nPM2.5 Summary Statistics (µg/m³):\n")
summary_stats <- summary(air_quality$pm25)
print(summary_stats)

cat("\nAdditional statistics:\n")
cat("Standard Deviation:", sprintf("%.2f", sd(air_quality$pm25)), "µg/m³\n")
cat("Coefficient of Variation:", sprintf("%.2f", sd(air_quality$pm25)/mean(air_quality$pm25)), "\n")

############################
# 6. EXTREME EVENT DETECTION
############################
cat("\n--- Identifying Extreme Pollution Events ---\n")

# Calculate 95th percentile threshold
threshold <- quantile(air_quality$pm25, 0.95, na.rm = TRUE)
cat("95th percentile threshold:", sprintf("%.2f", threshold), "µg/m³\n")

# Create extreme event indicator
air_quality <- air_quality %>%
  mutate(extreme_event = ifelse(pm25 >= threshold, "Extreme", "Normal"))

# Count extreme events
extreme_count <- sum(air_quality$extreme_event == "Extreme")
extreme_pct <- 100 * mean(air_quality$extreme_event == "Extreme")

cat("Extreme events:", extreme_count, 
    "(", sprintf("%.1f%%", extreme_pct), ")\n")
cat("Normal events:", sum(air_quality$extreme_event == "Normal"),
    "(", sprintf("%.1f%%", 100 - extreme_pct), ")\n")
cat("Maximum PM2.5 during extreme events:", 
    sprintf("%.2f", max(air_quality$pm25[air_quality$extreme_event == "Extreme"])),
    "µg/m³\n")

############################
# 7. CREATE FIGURES DIRECTORY
############################
if (!dir.exists("figures")) {
  dir.create("figures")
  cat("\n✓ Created 'figures' directory\n")
}

############################
# 8. VISUALIZATION - TIME SERIES
############################
cat("\n--- Generating Visualizations ---\n")

# Plot 1: Complete Time Series with Extreme Events
cat(" • Creating time series plot...\n")

p1 <- ggplot(air_quality, aes(x = datetime, y = pm25)) +
  geom_line(color = "steelblue", linewidth = 0.5, alpha = 0.7) +
  geom_point(data = filter(air_quality, extreme_event == "Extreme"),
             aes(x = datetime, y = pm25),
             color = "red", size = 1.5, alpha = 0.8) +
  geom_hline(yintercept = threshold, color = "red", 
             linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = mean(air_quality$pm25), color = "darkgreen",
             linetype = "dotted", linewidth = 1) +
  labs(
    title = "PM2.5 Hourly Concentrations - Sheffield 2024",
    subtitle = "Red points indicate extreme pollution events (≥95th percentile)",
    x = "Date",
    y = "PM2.5 Concentration (µg/m³)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30")
  )

ggsave("figures/PM25_TimeSeries.png", p1, width = 12, height = 6, dpi = 300)
cat("   ✓ PM25_TimeSeries.png saved\n")

############################
# 9. VISUALIZATION - DISTRIBUTION
############################

# Plot 2: PM2.5 Distribution Histogram
cat(" • Creating distribution histogram...\n")

p2 <- ggplot(air_quality, aes(x = pm25)) +
  geom_histogram(bins = 50, fill = "coral", color = "white", alpha = 0.8) +
  geom_vline(xintercept = threshold, color = "darkred",
             linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = mean(air_quality$pm25), color = "darkgreen",
             linetype = "dotted", linewidth = 1.2) +
  labs(
    title = "PM2.5 Concentration Distribution",
    subtitle = "Sheffield 2024 - Showing positive skewness",
    x = "PM2.5 Concentration (µg/m³)",
    y = "Frequency (hours)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("figures/PM25_Histogram.png", p2, width = 10, height = 6, dpi = 300)
cat("   ✓ PM25_Histogram.png saved\n")

############################
# 10. VISUALIZATION - TEMPORAL PATTERNS
############################

# Plot 3: Hourly Pattern (Diurnal Cycle)
cat(" • Creating hourly pattern plot...\n")

hourly_avg <- air_quality %>%
  group_by(hour) %>%
  summarise(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    se_pm25 = sd(pm25, na.rm = TRUE) / sqrt(n())
  )

p3 <- ggplot(hourly_avg, aes(x = hour, y = mean_pm25)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  geom_ribbon(aes(ymin = mean_pm25 - se_pm25, 
                  ymax = mean_pm25 + se_pm25),
              alpha = 0.2, fill = "darkblue") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(
    title = "Diurnal Pattern of PM2.5 Concentrations",
    subtitle = "Average hourly concentrations with standard error bands",
    x = "Hour of Day",
    y = "Mean PM2.5 (µg/m³)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("figures/PM25_HourlyPattern.png", p3, width = 10, height = 6, dpi = 300)
cat("   ✓ PM25_HourlyPattern.png saved\n")

# Plot 4: Monthly Pattern (Seasonal Cycle)
cat(" • Creating monthly pattern plot...\n")

monthly_avg <- air_quality %>%
  group_by(month) %>%
  summarise(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    median_pm25 = median(pm25, na.rm = TRUE),
    sd_pm25 = sd(pm25, na.rm = TRUE),
    n_obs = n()
  )

month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct")

p4 <- ggplot(monthly_avg, aes(x = factor(month), y = mean_pm25)) +
  geom_col(fill = "skyblue", color = "white", width = 0.8) +
  geom_hline(yintercept = mean(air_quality$pm25), 
             color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_discrete(labels = month_labels[1:nrow(monthly_avg)]) +
  labs(
    title = "Seasonal Pattern of PM2.5 Concentrations",
    subtitle = "Monthly average concentrations",
    x = "Month",
    y = "Mean PM2.5 (µg/m³)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave("figures/PM25_MonthlyPattern.png", p4, width = 10, height = 6, dpi = 300)
cat("   ✓ PM25_MonthlyPattern.png saved\n")

# Plot 5: Monthly Boxplots
cat(" • Creating monthly boxplot...\n")

air_quality_plot <- air_quality %>%
  mutate(month_label = factor(month.abb[month], 
                              levels = month.abb[1:10]))

p5 <- ggplot(air_quality_plot, aes(x = month_label, y = pm25)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", 
               outlier.alpha = 0.5) +
  geom_hline(yintercept = threshold, color = "red",
             linetype = "dashed", linewidth = 1) +
  labs(
    title = "PM2.5 Distribution by Month",
    subtitle = "Boxplots showing median, IQR, and outliers",
    x = "Month",
    y = "PM2.5 Concentration (µg/m³)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

ggsave("figures/PM25_MonthlyBoxplot.png", p5, width = 12, height = 6, dpi = 300)
cat("   ✓ PM25_MonthlyBoxplot.png saved\n")

############################
# 11. EXTREME EVENT ANALYSIS
############################
cat("\n--- Analyzing Extreme Events ---\n")

# Extract extreme events
extreme_events <- air_quality %>%
  filter(extreme_event == "Extreme") %>%
  arrange(desc(pm25))

cat("\nTop 10 Extreme Pollution Events:\n")
print(head(extreme_events %>% 
             select(datetime, pm25, hour, month), 10))

# Temporal distribution of extreme events
extreme_by_month <- extreme_events %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nExtreme Events by Month:\n")
print(extreme_by_month)

extreme_by_hour <- extreme_events %>%
  group_by(hour) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nExtreme Events by Hour of Day:\n")
print(head(extreme_by_hour, 5))

# Plot 6: Extreme Event Frequency by Month
cat(" • Creating extreme events frequency plot...\n")

extreme_monthly <- air_quality %>%
  group_by(month, extreme_event) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(month_label = factor(month.abb[month], levels = month.abb[1:10]))

p6 <- ggplot(extreme_monthly, aes(x = month_label, y = count, 
                                  fill = extreme_event)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Extreme" = "red", "Normal" = "lightgray")) +
  labs(
    title = "Distribution of Normal vs Extreme Events by Month",
    subtitle = "Red bars indicate extreme pollution hours",
    x = "Month",
    y = "Number of Hours",
    fill = "Event Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

ggsave("figures/PM25_ExtremeByMonth.png", p6, width = 10, height = 6, dpi = 300)
cat("   ✓ PM25_ExtremeByMonth.png saved\n")

############################
# 12. SUMMARY STATISTICS OUTPUT
############################
cat("\n========================================\n")
cat("         FINAL SUMMARY REPORT          \n")
cat("========================================\n\n")

cat("DATASET INFORMATION:\n")
cat("-------------------\n")
cat("Monitoring Station: Sheffield Devonshire Green (ID: 2508)\n")
cat("Location: 53.378622°N, 1.478096°W\n")
cat("Parameter: PM2.5\n")
cat("Total Observations:", nrow(air_quality), "hours\n")
cat("Date Range:", format(min(air_quality$datetime), "%Y-%m-%d"), "to",
    format(max(air_quality$datetime), "%Y-%m-%d"), "\n")
cat("Duration:", 
    as.numeric(difftime(max(air_quality$datetime), 
                        min(air_quality$datetime), units = "days")),
    "days\n\n")

cat("PM2.5 CONCENTRATION STATISTICS (µg/m³):\n")
cat("---------------------------------------\n")
cat("Mean:        ", sprintf("%6.2f", mean(air_quality$pm25)), "\n")
cat("Median:      ", sprintf("%6.2f", median(air_quality$pm25)), "\n")
cat("Std Dev:     ", sprintf("%6.2f", sd(air_quality$pm25)), "\n")
cat("Minimum:     ", sprintf("%6.2f", min(air_quality$pm25)), "\n")
cat("Maximum:     ", sprintf("%6.2f", max(air_quality$pm25)), "\n")
cat("25th %ile:   ", sprintf("%6.2f", quantile(air_quality$pm25, 0.25)), "\n")
cat("75th %ile:   ", sprintf("%6.2f", quantile(air_quality$pm25, 0.75)), "\n")
cat("95th %ile:   ", sprintf("%6.2f", threshold), "\n")
cat("CV:          ", sprintf("%6.2f", 
                             sd(air_quality$pm25)/mean(air_quality$pm25)), "\n\n")

cat("EXTREME EVENT SUMMARY:\n")
cat("----------------------\n")
cat("Threshold (95th %ile):", sprintf("%.2f", threshold), "µg/m³\n")
cat("Total Extreme Events: ", extreme_count, "hours\n")
cat("Percentage:           ", sprintf("%.1f%%", extreme_pct), "\n")
cat("Max Concentration:    ", 
    sprintf("%.2f", max(air_quality$pm25[air_quality$extreme_event == "Extreme"])),
    "µg/m³\n\n")

cat("TEMPORAL PATTERNS:\n")
cat("------------------\n")
cat("Peak Hour:    ", sprintf("%02d:00", 
                              hourly_avg$hour[which.max(hourly_avg$mean_pm25)]),
    "(", sprintf("%.2f", max(hourly_avg$mean_pm25)), "µg/m³)\n")
cat("Lowest Hour:  ", sprintf("%02d:00",
                              hourly_avg$hour[which.min(hourly_avg$mean_pm25)]),
    "(", sprintf("%.2f", min(hourly_avg$mean_pm25)), "µg/m³)\n")
cat("Peak Month:   ", month.abb[monthly_avg$month[which.max(monthly_avg$mean_pm25)]],
    "(", sprintf("%.2f", max(monthly_avg$mean_pm25)), "µg/m³)\n")
cat("Lowest Month: ", month.abb[monthly_avg$month[which.min(monthly_avg$mean_pm25)]],
    "(", sprintf("%.2f", min(monthly_avg$mean_pm25)), "µg/m³)\n\n")

cat("WHO GUIDELINES COMPARISON:\n")
cat("--------------------------\n")
cat("WHO 24-hour guideline:    15.0 µg/m³\n")
cat("WHO Annual guideline:      5.0 µg/m³\n")
cat("Sheffield Annual Mean:    ", sprintf("%.2f", mean(air_quality$pm25)), 
    "µg/m³\n")
cat("Exceedances (>15 µg/m³):  ", 
    sum(air_quality$pm25 > 15), "hours",
    "(", sprintf("%.1f%%", 100*mean(air_quality$pm25 > 15)), ")\n\n")

cat("========================================\n")
cat("✓✓✓ ANALYSIS COMPLETE ✓✓✓\n")
cat("========================================\n\n")

cat("All visualizations saved in 'figures/' directory:\n")
cat(" • PM25_TimeSeries.png\n")
cat(" • PM25_Histogram.png\n")
cat(" • PM25_HourlyPattern.png\n")
cat(" • PM25_MonthlyPattern.png\n")
cat(" • PM25_MonthlyBoxplot.png\n")
cat(" • PM25_ExtremeByMonth.png\n\n")

cat("Data exported to 'data/processed/' directory:\n")

# Export processed data
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write.csv(air_quality, "data/processed/pm25_processed.csv", row.names = FALSE)
write.csv(extreme_events, "data/processed/extreme_events.csv", row.names = FALSE)
write.csv(monthly_avg, "data/processed/monthly_summary.csv", row.names = FALSE)
write.csv(hourly_avg, "data/processed/hourly_summary.csv", row.names = FALSE)

cat(" • pm25_processed.csv\n")
cat(" • extreme_events.csv\n")
cat(" • monthly_summary.csv\n")
cat(" • hourly_summary.csv\n\n")

cat("========================================\n")
cat("END OF ANALYSIS\n")
cat("========================================\n")