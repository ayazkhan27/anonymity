library(dplyr)
library(randomForest)
library(lubridate)
library(ggplot2)

# Load and preprocess posts data
posts_data <- posts_data %>%
  mutate(
    year = year(as.Date(post_date, format = "%Y-%m-%d")),
    month = month(as.Date(post_date, format = "%Y-%m-%d"), label = TRUE),
    day_of_week = wday(as.Date(post_date, format = "%Y-%m-%d"), label = TRUE)
  )

# Splitting the dataset into training and testing sets
train_posts_data <- posts_data %>%
  filter(year >= 2010 & year <= 2016)
test_posts_data <- posts_data %>%
  filter(year >= 2017 & year <= 2024)

# Define the formula for the dependent variable
dependent_variable_formula <- weighted_mean_sentiment_score ~ year + year*month + month + day_of_week + total_posts

# Function to train, test, and evaluate the model
train_test_evaluate_model <- function(training_data, testing_data, formula) {
  # Prepare training data
  training_data_agg <- training_data %>%
    group_by(year, month, day_of_week) %>%
    summarise(
      weighted_mean_sentiment_score = sum(sentiment_score * n()) / sum(n()),
      total_posts = n(),
      .groups = 'drop'
    )
  
  # Train the Random Forest model
  model <- randomForest(formula, data = training_data_agg)
  
  # Prepare testing data
  testing_data_agg <- testing_data %>%
    group_by(year, month, day_of_week) %>%
    summarise(
      weighted_mean_sentiment_score = sum(sentiment_score * n()) / sum(n()),
      total_posts = n(),
      .groups = 'drop'
    )
  
  # Predict using the testing data
  testing_data_agg$predicted_score <- predict(model, newdata = testing_data_agg)
  
  # Calculate evaluation metrics
  mae <- mean(abs(testing_data_agg$predicted_score - testing_data_agg$weighted_mean_sentiment_score))
  rmse <- sqrt(mean((testing_data_agg$predicted_score - testing_data_agg$weighted_mean_sentiment_score)^2))
  r_squared <- summary(lm(predicted_score ~ weighted_mean_sentiment_score, data = testing_data_agg))$r.squared
  n <- nrow(testing_data_agg)
  p <- length(coefficients(model)) - 1
  adjusted_r_squared = 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
  
  # Return all necessary data including the model
  return(list(
    model = model,
    r_squared = r_squared,
    adjusted_r_squared = adjusted_r_squared,
    mae = mae,
    rmse = rmse,
    testing_data_agg = testing_data_agg
  ))
}

# Now re-run the training/testing process
evaluation_metrics_posts_data <- train_test_evaluate_model(train_posts_data, test_posts_data, dependent_variable_formula)

# Attempt to get variable importance again
importance <- importance(evaluation_metrics_posts_data$model)
print(importance)

# Print evaluation metrics
print("Evaluation Metrics for posts_data model:")
print(paste("R²:", evaluation_metrics_posts_data$r_squared))
print(paste("Adjusted R²:", evaluation_metrics_posts_data$adjusted_r_squared))
print(paste("MAE:", evaluation_metrics_posts_data$mae))
print(paste("RMSE:", evaluation_metrics_posts_data$rmse))

# Plotting the results for the testing dataset
plot_data <- evaluation_metrics_posts_data$testing_data_agg %>%
  filter(year >= 2017 & year <= 2024)

ggplot(plot_data, aes(x = day_of_week, y = weighted_mean_sentiment_score)) +
  geom_line(aes(color = "True")) +
  geom_point(aes(y = predicted_score, color = "Predicted")) +
  ggtitle("Predicted vs Actual Weighted Mean Sentiment Scores for 2017 to 2024") +
  labs(y = "Weighted Mean Sentiment Score", color = "Legend") +
  scale_color_manual(values = c("True" = "blue", "Predicted" = "red")) +
  theme_minimal()
