###MERGERFIX
# Assuming UMCSENT.x is the correct column
data_processed$UMCSENT <- data_processed$UMCSENT.x
data_processed <- data_processed[!names(data_processed) %in% c("UMCSENT.x", "UMCSENT.y", "UMCSENT.x.x", "UMCSENT.y.y")]


###START
# Install and load quantmod if not already installed
if (!require(quantmod)) {
  install.packages("quantmod")
  library(quantmod)
}

# Set the desired date range
start_date <- as.Date("2010-08-28")
end_date <- as.Date("2024-04-16")  # Adjust based on the latest available data

# Fetch the data from FRED
getSymbols("UMCSENT", src = "FRED", from = start_date, to = end_date)

# Access the downloaded data and convert to a data frame
consumer_sentiment <- data.frame(date = index(UMCSENT), UMCSENT = coredata(UMCSENT))

# Convert the date column to Date type if necessary
consumer_sentiment$date <- as.Date(consumer_sentiment$date, format="%Y-%m-%d")

# Convert 'post_date' from character to Date
data_processed$post_date <- as.Date(data_processed$post_date, format="%Y-%m-%d")

# Merge with your main dataset
# Assuming 'data_processed' is your main dataset which has a 'post_date' column
data_processed$month <- as.Date(data_processed$month, format="%Y-%m-%d")
consumer_sentiment$date <- as.Date(consumer_sentiment$date, format="%Y-%m-%d")
data_processed <- data_processed %>%
  left_join(consumer_sentiment, by = c("month" = "date"))


# Remove rows with any missing values
data_processed <- na.omit(data_processed)

# Ensure 'post_date' is in Date format for correct arranging
data_processed$post_date <- as.Date(data_processed$post_date)

# Arrange by date and apply lag to the UMCSENT without grouping
data_processed <- data_processed %>%
  arrange(post_date) %>%
  mutate(lagged_sentiment = lag(UMCSENT, 1))  # Lag of 1 period; adjust as necessary

# Check results
head(data_processed)

data_processed <- na.omit(data_processed)
# Update the random forest model training to include the lagged sentiment
rf_model <- randomForest(
  mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio + UMCSENT + lagged_sentiment,
  data = data_processed,
  ntree = 500,
  mtry = 11
)

# If you have a separate test dataset, make sure to prepare it in the same way

# Evaluate the model again to see the impact on R-squared and other metrics
predictions <- predict(rf_model, newdata = test_data)
results <- postResample(pred = predictions, obs = test_data$mean_sentiment)
cat("Updated R-squared Value: ", results['Rsquared'], "\n")
