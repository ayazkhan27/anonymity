library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# New file path
file_path <- "C:/Users/admin/Downloads/tbmics.xls"

# Load the consumer sentiment data from the Excel file
consumer_sentiment <- read_excel(file_path)

# Rename columns
colnames(consumer_sentiment) <- c("Month", "Year", "UMCSENT")

# Filter out rows before August 2010 and beyond March 2024
consumer_sentiment <- consumer_sentiment %>%
  filter((Year > 2010 | (Year == 2010 & Month >= "August")) & 
           (Year < 2024 | (Year == 2024 & Month <= "March")))

# Convert Month and Year to proper date format
consumer_sentiment$Month <- paste0(consumer_sentiment$Month, " ", consumer_sentiment$Year)
consumer_sentiment$Month <- as.Date(paste0("01-", consumer_sentiment$Month), format = "%d-%B %Y")

# Remove Year column
consumer_sentiment <- consumer_sentiment[, -2]

# Convert post_date column in data_processed to Date type
data_processed$post_date <- ymd(data_processed$post_date)

# Merge datasets
merged_data <- inner_join(data_processed, consumer_sentiment, by = c("post_date" = "Month"))

# Convert UMCSENT column to numeric
merged_data$UMCSENT <- as.numeric(merged_data$UMCSENT)

# Calculate correlation
correlation <- cor(merged_data$mean_sentiment, merged_data$UMCSENT, use = "complete.obs")

# Normalize the data
merged_data$mean_sentiment <- (merged_data$mean_sentiment - min(merged_data$mean_sentiment)) / (max(merged_data$mean_sentiment) - min(merged_data$mean_sentiment))
merged_data$UMCSENT <- (merged_data$UMCSENT - min(merged_data$UMCSENT)) / (max(merged_data$UMCSENT) - min(merged_data$UMCSENT))

# Plot the data and the regression line
ggplot(merged_data, aes(x = mean_sentiment, y = UMCSENT)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = paste("Correlation between Mean Sentiment and UMCSENT: ", round(correlation, 2)),
       x = "Mean Sentiment",
       y = "UMCSENT") +
  theme_minimal()
