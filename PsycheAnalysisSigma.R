library(tidyverse)
library(randomForest)
library(lubridate)
library(caret)
library(timetk)

# Assuming data is loaded into 'data'
# Calculate word counts and sentiment sums for each post
data_processed <- posts_data %>%
  group_by("Post Number", post_date, year, month, day_of_week) %>%
  summarise(
    total_words = n(),
    total_sentiment = sum(sentiment_score),
    neutral_count = sum(sentiment_score == 0),
    positive_count = sum(sentiment_score > 0),
    negative_count = sum(sentiment_score < 0),
    .groups = 'drop'
  ) %>%
  mutate(
    mean_sentiment = total_sentiment / total_words,
    neutral_ratio = neutral_count / total_words,
    positive_ratio = positive_count / total_words,
    negative_ratio = negative_count / total_words
  )

# Determine the mean and standard deviation for sentiment scores
mean_sentiment <- mean(data_processed$mean_sentiment, na.rm = TRUE)
sd_sentiment <- sd(data_processed$mean_sentiment, na.rm = TRUE)

# Define the upper and lower bounds for outliers
upper_bound <- mean_sentiment + (2 * sd_sentiment)
lower_bound <- mean_sentiment - (2 * sd_sentiment)

# Filter the data to exclude outliers
data_filtered <- data_processed %>%
  filter(mean_sentiment >= lower_bound & mean_sentiment <= upper_bound)

# Split the filtered data into training and testing sets based on the date
split_date <- as.Date("2017-01-01")
train_data <- data_filtered %>% filter(post_date < split_date)
test_data <- data_filtered %>% filter(post_date >= split_date)

# Factorize categorical variables
train_data$month <- as.factor(train_data$month)
train_data$day_of_week <- as.factor(train_data$day_of_week)
test_data$month <- as.factor(test_data$month)
test_data$day_of_week <- as.factor(test_data$day_of_week)

# Train the model with additional predictors
rf_model <- randomForest(
  mean_sentiment ~ year + month + day_of_week + neutral_ratio + positive_ratio + negative_ratio,
  data = train_data,
  ntree = 500
)

# Predict using the trained model
predictions <- predict(rf_model, newdata = test_data)

# Add predictions to the test_data, and filter out the outlier predictions
test_data <- test_data %>%
  mutate(predicted_sentiment = predictions) %>%
  filter(predicted_sentiment >= lower_bound & predicted_sentiment <= upper_bound)

# Evaluate the model
results <- postResample(pred = test_data$predicted_sentiment, obs = test_data$mean_sentiment)

# Ensure the data types are correct
test_data$post_date <- as.Date(test_data$post_date)
test_data$mean_sentiment <- as.numeric(test_data$mean_sentiment)
test_data$predicted_sentiment <- as.numeric(test_data$predicted_sentiment)

# Plot the graph with data points to see if the data is plotting correctly
ggplot(test_data, aes(x = post_date)) +
  geom_line(aes(y = mean_sentiment, group = 1, colour = "Actual"), size = 1.2) +
  geom_line(aes(y = predicted_sentiment, group = 1, colour = "Predicted"), size = 1.2) +
  geom_point(aes(y = mean_sentiment, colour = "Actual"), size = 1.5) +
  geom_point(aes(y = predicted_sentiment, colour = "Predicted"), size = 1.5) +
  labs(x = "Date", y = "Mean Sentiment Score", title = "Predicted vs Actual Sentiment Scores Over Time") +
  theme_minimal() +
  scale_color_manual("", labels = c("Actual", "Predicted"), values = c("blue", "red"))

# Print evaluation metrics
cat("R-squared Value: ", results['Rsquared'], "\n")
cat("Mean Absolute Error: ", results['MAE'], "\n")
cat("Root Mean Squared Error: ", results['RMSE'], "\n")

importance_posts_data <- importance(rf_model, type=2)
print(importance_posts_data)

varImpPlot(rf_model)

##################DAY_OF_WEEK###########################

# Assuming 'test_data' has been processed and 'day_of_week' is already a correct factor

# Aggregate mean sentiment scores by day of the week for actual and predicted values
weekly_sentiment <- test_data %>%
  group_by(day_of_week) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert 'day_of_week' to a numeric factor for the polynomial fitting
weekly_sentiment$day_of_week_num <- as.numeric(weekly_sentiment$day_of_week)

# Melt the data for plotting with ggplot2
weekly_sentiment_long <- pivot_longer(weekly_sentiment, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for day of the week
ggplot(weekly_sentiment_long, aes(x = day_of_week_num)) +
  geom_point(aes(y = mean_sentiment, color = type)) +
  geom_smooth(data = weekly_sentiment_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", formula = y ~ poly(x, 4), se = FALSE, color = "red") +
  geom_smooth(data = weekly_sentiment_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", formula = y ~ poly(x, 4), se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Day of the Week",
    x = "Day of the Week", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7, labels = levels(test_data$day_of_week))

######MONTH############

# Assuming 'test_data' has been processed and 'month' is already a correct factor

# Aggregate mean sentiment scores by month for actual and predicted values
monthly_sentiment <- test_data %>%
  group_by(month) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert 'month' to a numeric factor for the polynomial fitting
monthly_sentiment$month_num <- as.numeric(monthly_sentiment$month)

# Melt the data for plotting with ggplot2
monthly_sentiment_long <- pivot_longer(monthly_sentiment, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for month
ggplot(monthly_sentiment_long, aes(x = month_num)) +
  geom_point(aes(y = mean_sentiment, color = type)) +
  geom_smooth(data = monthly_sentiment_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(data = monthly_sentiment_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Month",
    x = "Month", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12, labels = month.name)

###########YEAR###################

# Assuming 'test_data' has been processed and 'year' is correctly set as a numerical value

# Aggregate mean sentiment scores by year for actual and predicted values
yearly_sentiment <- test_data %>%
  group_by(year) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Melt the data for plotting with ggplot2
yearly_sentiment_long <- pivot_longer(yearly_sentiment, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for year
ggplot(yearly_sentiment_long, aes(x = year, y = mean_sentiment, color = type)) +
  geom_point() +
  geom_smooth(data = yearly_sentiment_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "red") +
  geom_smooth(data = yearly_sentiment_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Year",
    x = "Year", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal()

#############METRICS##############

install.packages("Metrics")
library(Metrics)
install.packages("e1071")
library(e1071)

# Current Metrics
rmse_val <- rmse(test_data$mean_sentiment, test_data$predicted_sentiment)
mae_val <- mae(test_data$mean_sentiment, test_data$predicted_sentiment)
r2_val <- cor(test_data$mean_sentiment, test_data$predicted_sentiment)^2

# Adjusted R-squared
n <- nrow(test_data) # Number of observations
p <- length(test_data) - 1 # Number of predictors
adj_r2_val <- 1 - ((1 - r2_val) * (n - 1) / (n - p - 1))

# Print Metrics
cat("RMSE: ", rmse_val, "\n")
cat("MAE: ", mae_val, "\n")
cat("R-squared: ", r2_val, "\n")
cat("Adjusted R-squared: ", adj_r2_val, "\n")

model_summary <- summary(rf_model)
print(model_summary) # Displays the summary of the model including p-values

install.packages("car")
library(car)
# Use diagnostic plots
diagnosticPlots(rf_model, test_data)

varImpPlot(rf_model)
