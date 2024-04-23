library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

library(dplyr)

# Aggregate by year
data_year <- data_processed %>%
  group_by(year) %>%
  summarise(normalized_mean_sentiment = mean(normalized_mean_sentiment))

# Aggregate by month
data_month <- data_processed %>%
  mutate(month = format(as.Date(post_date), "%B")) %>%
  group_by(month) %>%
  summarise(normalized_mean_sentiment = mean(normalized_mean_sentiment))

# Aggregate by day_of_week
data_day_of_week <- data_processed %>%
  mutate(day_of_week = format(as.Date(post_date), "%A")) %>%
  group_by(day_of_week) %>%
  summarise(normalized_mean_sentiment = mean(normalized_mean_sentiment))

# Aggregate by time
data_time <- data_processed %>%
  group_by(time) %>%
  summarise(normalized_mean_sentiment = mean(normalized_mean_sentiment))

# Convert 'post_date' to Date format and extract components if not already done
data_processed <- data_processed %>%
  mutate(post_date = as.Date(post_date),
         month = month(post_date, label = TRUE),
         year = year(post_date),
         day_of_week = wday(post_date, label = TRUE))

# General function to remove outliers within 2 standard deviations
filter_outliers <- function(data, column_name) {
  mean_val <- mean(data[[column_name]], na.rm = TRUE)
  sd_val <- sd(data[[column_name]], na.rm = TRUE)
  data %>%
    filter(between(data[[column_name]], mean_val - 2 * sd_val, mean_val + 2 * sd_val))
}

# Apply outlier filtering to mean sentiment calculations
data_processed <- filter_outliers(data_processed, "mean_sentiment")
data_processed <- filter_outliers(data_processed, "total_sentiment")

# Normalize a vector to a range between 0 and 1
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply normalization to the sentiment scores
data_processed$normalized_mean_sentiment <- normalize(data_processed$mean_sentiment)
data_processed$normalized_total_sentiment <- normalize(data_processed$total_sentiment)

# Plot the normalized total sentiment over time
ggplot(data_processed, aes(x = post_date, y = normalized_total_sentiment)) +
  geom_line() +
  labs(title = "Normalized Total Sentiment Over Time", x = "Date", y = "Normalized Total Sentiment")

# Plot the normalized mean sentiment over time
ggplot(data_processed, aes(x = post_date, y = normalized_mean_sentiment)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Time", x = "Date", y = "Normalized Mean Sentiment")

# Plot normalized mean sentiment by day of the week
ggplot(data_processed, aes(x = day_of_week, y = normalized_mean_sentiment, fill = day_of_week)) +
  geom_boxplot() +
  labs(title = "Normalized Mean Sentiment by Day of the Week", x = "Day of the Week", y = "Normalized Mean Sentiment")

# Plot normalized mean sentiment by month
ggplot(data_processed, aes(x = month, y = normalized_mean_sentiment, fill = month)) +
  geom_boxplot() +
  labs(title = "Normalized Mean Sentiment by Month", x = "Month", y = "Normalized Mean Sentiment")

# Plot normalized mean sentiment by year
# Plot normalized mean sentiment by year
ggplot(data_processed, aes(x = factor(year), y = normalized_mean_sentiment, fill = factor(year))) +
  geom_boxplot() +
  labs(title = "Normalized Mean Sentiment by Year", x = "Year", y = "Normalized Mean Sentiment") +
  theme(legend.position = "none")

# Plot normalized mean sentiment by year
ggplot(data_processed, aes(x = factor(year), y = normalized_mean_sentiment, group = 1)) +
  geom_point() +  # Plot data points
  geom_line() +    # Connect data points with lines
  labs(title = "Normalized Mean Sentiment Over Time",
       x = "Year",
       y = "Normalized Mean Sentiment")



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# Ensure 'post_date' is in the correct date format
data_processed$post_date <- as.Date(data_processed$post_date)

# Plotting
ggplot(data_processed, aes(x = post_date)) +
  geom_bar(aes(y = extreme_positive_ratio, fill = "darkblue"), stat = "identity") +
  geom_bar(aes(y = moderate_positive_ratio, fill = "lightblue"), stat = "identity") +
  geom_bar(aes(y = medium_positive_ratio, fill = "skyblue"), stat = "identity") +
  geom_bar(aes(y = slight_positive_ratio, fill = "blue"), stat = "identity") +
  geom_bar(aes(y = extreme_negative_ratio, fill = "darkred"), stat = "identity") +
  geom_bar(aes(y = moderate_negative_ratio, fill = "red"), stat = "identity") +
  geom_bar(aes(y = medium_negative_ratio, fill = "lightcoral"), stat = "identity") +
  geom_bar(aes(y = slight_negative_ratio, fill = "pink"), stat = "identity") +
  geom_bar(aes(y = neutral_ratio, fill = "green"), stat = "identity") +
  geom_bar(aes(y = buffer_ratio, fill = "yellow"), stat = "identity") +
  labs(title = "Distribution of Sentiment Ratios Over Time",
       x = "Date",
       y = "Ratio",
       fill = "Ratio Type") +
  scale_fill_manual(values = c("darkblue" = "darkblue", "lightblue" = "lightblue",
                               "skyblue" = "skyblue", "blue" = "blue", 
                               "darkred" = "darkred", "red" = "red",
                               "lightcoral" = "lightcoral", "pink" = "pink",
                               "green" = "green", "yellow" = "yellow")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
#############NORMALIZED#############################

library(ggplot2)

ggplot(data_year, aes(x = year, y = normalized_mean_sentiment)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Years",
       x = "Year",
       y = "Normalized Mean Sentiment") +
  scale_x_continuous(breaks = unique(data_year$year))


# Reorder the levels of the month variable
data_month$month <- factor(data_month$month, levels = month.name)

# Plot for data aggregated by month
ggplot(data_month, aes(x = month, y = normalized_mean_sentiment, group = 1)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Months",
       x = "Month",
       y = "Normalized Mean Sentiment")



# Reorder the levels of the day_of_week variable
data_day_of_week$day_of_week <- factor(data_day_of_week$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot for data aggregated by day_of_week
ggplot(data_day_of_week, aes(x = day_of_week, y = normalized_mean_sentiment, group = 1)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Days of Week",
       x = "Day of Week",
       y = "Normalized Mean Sentiment")



# Plot for data aggregated by time
ggplot(data_time, aes(x = time, y = normalized_mean_sentiment)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Time",
       x = "Time",
       y = "Normalized Mean Sentiment")

# Preprocess data to group by hour
data_time_hourly <- data_time %>%
  mutate(hour = format(as.POSIXct(time, format = "%H:%M"), "%H:00")) %>%
  group_by(hour) %>%
  summarise(normalized_mean_sentiment = mean(normalized_mean_sentiment))

# Plot for data aggregated by time (hourly)
ggplot(data_time_hourly, aes(x = hour, y = normalized_mean_sentiment, group = 1)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Time (Hourly)",
       x = "Time",
       y = "Normalized Mean Sentiment")
######################STOP################
# Define a function to categorize time into sections
categorize_time <- function(time) {
  hour <- as.integer(substr(time, 1, 2))
  if (hour >= 6 && hour < 12) {
    return("Morning")
  } else if (hour >= 12 && hour < 18) {
    return("Afternoon")
  } else if (hour >= 18 && hour < 24) {
    return("Evening")
  } else {
    return("Night")
  }
}

# Apply the function to create a new variable representing time sections
data_time$section <- sapply(data_time$time, categorize_time)
# Aggregate mean sentiment for each time section
aggregated_data_time <- data_time %>%
  group_by(section) %>%
  summarise(normalized_mean_sentiment = mean(normalized_mean_sentiment))

# Plot aggregated data for time sections
ggplot(aggregated_data_time, aes(x = section, y = normalized_mean_sentiment)) +
  geom_line() +
  labs(title = "Aggregated Normalized Mean Sentiment Over Time Sections",
       x = "Time Section",
       y = "Aggregated Normalized Mean Sentiment")

# Plot for data aggregated by time section
ggplot(data_time, aes(x = section, y = normalized_mean_sentiment, group = 1)) +
  geom_line() +
  labs(title = "Normalized Mean Sentiment Over Time Sections",
       x = "Time Section",
       y = "Normalized Mean Sentiment") +
  scale_x_discrete(labels = c("Morning", "Afternoon", "Evening", "Night"))
