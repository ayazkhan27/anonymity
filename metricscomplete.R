# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(test_data$predicted_sentiment - test_data$mean_sentiment))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_data$predicted_sentiment - test_data$mean_sentiment)^2))

# Calculate Mean Absolute Percentage Error (MAPE)
# Ensure to handle cases where actual values are zero to avoid division by zero error
mape <- mean(abs((test_data$predicted_sentiment - test_data$mean_sentiment) / test_data$mean_sentiment), na.rm = TRUE) * 100

# Calculate Symmetric Mean Absolute Percentage Error (sMAPE)
smape <- mean(2 * abs(test_data$predicted_sentiment - test_data$mean_sentiment) / (abs(test_data$predicted_sentiment) + abs(test_data$mean_sentiment))) * 100

# Calculate Mean Absolute Scaled Error (MASE)
# Calculate the naive forecast (shift the actual values by one period)
test_data <- test_data %>% 
  arrange(post_date) %>% 
  mutate(y_true_lag1 = dplyr::lag(mean_sentiment, 1))

# Calculate the denominator for MASE (mean absolute error of the naive forecast)
naive_mae <- mean(abs(test_data$mean_sentiment - test_data$y_true_lag1), na.rm = TRUE)

# Calculate MASE
mase <- mean(abs(test_data$predicted_sentiment - test_data$mean_sentiment) / naive_mae, na.rm = TRUE)

# Print the metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
cat("Symmetric Mean Absolute Percentage Error (sMAPE):", smape, "%\n")
cat("Mean Absolute Scaled Error (MASE):", mase, "\n")

# Load necessary library
library(stats)

# Predict sentiment using the model
predicted_sentiment <- predict(sentiment_model, newdata = test_data)
actual_sentiment <- test_data$mean_sentiment

# Fit a linear model to calculate R-squared and adjusted R-squared
fit <- lm(predicted_sentiment ~ actual_sentiment)

# Extract the summary of the linear model
model_summary <- summary(fit)

# Retrieve R-squared and adjusted R-squared values
r_squared <- model_summary$r.squared
adjusted_r_squared <- model_summary$adj.r.squared

# Print R-squared and adjusted R-squared
print(paste("R-squared: ", r_squared))
print(paste("Adjusted R-squared: ", adjusted_r_squared))
