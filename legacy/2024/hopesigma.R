library(randomForest)
library(dplyr)

# Assuming 'train_data' is your training dataset and has the necessary columns preprocessed
# Calculate interaction terms and new features, adjusting for normal distribution weights in sentiment impact
train_data <- train_data %>%
  mutate(
    negative_impact = 1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio,
    positive_impact = 1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio,
    interaction_term = negative_impact * positive_impact,
    squared_negative_impact = negative_impact^2,
    squared_positive_impact = positive_impact^2
  )
# Scale the ratios to [0, 1] range to normalize their impact
train_data <- train_data %>%
  mutate(
    scaled_buffer_ratio = buffer_ratio / max(buffer_ratio, na.rm = TRUE),
    scaled_neutral_ratio = neutral_ratio / max(neutral_ratio, na.rm = TRUE),
    scaled_positive_ratio = positive_impact / max(positive_impact, na.rm = TRUE),
    scaled_negative_ratio = negative_impact / max(negative_impact, na.rm = TRUE)
  )

# Define a simpler formula for the randomForest model
sentiment_formula <- mean_sentiment ~ scaled_buffer_ratio + scaled_neutral_ratio +
  scaled_positive_ratio + scaled_negative_ratio +
  scaled_buffer_ratio:scaled_neutral_ratio +  # Interaction between buffer and neutral
  scaled_positive_ratio:scaled_negative_ratio  # Interaction between positive and negative

# Train the random forest model using the defined formula
sentiment_model <- randomForest(
  formula = sentiment_formula,
  data = train_data,
  ntree = 500,
  mtry = 3,  # Adjust based on the number of predictors
  importance = TRUE,
  do.trace = 100,
  parallel = TRUE
)

# Predict using the trained model
train_data$predicted_sentiment <- predict(sentiment_model, newdata = train_data)

# Print the model summary and importance
print(summary(sentiment_model))
varImpPlot(sentiment_model)

# Optionally, evaluate the model's performance
# Assuming 'test_data' is your testing dataset and also preprocessed similarly
# Scale the ratios to [0, 1] range to normalize their impact
test_data <- test_data %>%
  mutate(
    negative_impact = 1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio,
    positive_impact = 1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio,
    interaction_term = negative_impact * positive_impact,
    squared_negative_impact = negative_impact^2,
    squared_positive_impact = positive_impact^2
  )

test_data <- test_data %>%
  mutate(
    scaled_buffer_ratio = buffer_ratio / max(buffer_ratio, na.rm = TRUE),
    scaled_neutral_ratio = neutral_ratio / max(neutral_ratio, na.rm = TRUE),
    scaled_positive_ratio = positive_impact / max(positive_impact, na.rm = TRUE),
    scaled_negative_ratio = negative_impact / max(negative_impact, na.rm = TRUE)
  )


test_data$predicted_sentiment <- predict(sentiment_model, newdata = test_data)
results <- postResample(pred = test_data$predicted_sentiment, obs = test_data$mean_sentiment)
print(results)

########RESULTS###############

# Calculate R-squared value for the test set
predicted_sentiment <- predict(sentiment_model, newdata = test_data)
actual_sentiment <- test_data$mean_sentiment
r_squared <- summary(lm(predicted_sentiment ~ actual_sentiment))$r.squared
print(paste("R-squared: ", r_squared))

###############

# Add predicted sentiments back to the test data
test_data$predicted_sentiment <- predicted_sentiment

# Aggregate data by year, month, and day of the week
test_data_summary <- test_data %>%
  mutate(year = lubridate::year(post_date), 
         month = lubridate::month(post_date, label = TRUE), 
         day_of_week = lubridate::wday(post_date, label = TRUE)) %>%
  group_by(year, month, day_of_week) %>%
  summarise(
    mean_actual_sentiment = mean(mean_sentiment),
    mean_predicted_sentiment = mean(predicted_sentiment)
  ) %>%
  ungroup() %>%
  arrange(year, month, day_of_week)
##############
library(ggplot2)

# Plotting mean sentiment scores by time periods
ggplot(test_data_summary, aes(x = month, y = mean_actual_sentiment, group = year)) +
  geom_line(aes(color = as.factor(year)), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, linetype = "Predicted"), size = 1) +
  facet_wrap(~year) +
  labs(title = "Mean Sentiment Score by Month and Year",
       x = "Month",
       y = "Mean Sentiment Score",
       color = "Year",
       linetype = "Legend") +
  theme_minimal()

# Additional plot for day of the week
ggplot(test_data_summary, aes(x = day_of_week, y = mean_actual_sentiment, group = year)) +
  geom_line(aes(color = as.factor(year)), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, linetype = "Predicted"), size = 1) +
  labs(title = "Mean Sentiment Score by Day of Week",
       x = "Day of Week",
       y = "Mean Sentiment Score",
       color = "Year",
       linetype = "Legend") +
  theme_minimal()
