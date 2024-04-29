# Assuming sentiment_model is your trained random forest model and train_data is your dataset
predicted_sentiment <- predict(sentiment_model_2, newdata = train_data)
actual_sentiment <- train_data$adjusted_mean_sentiment
residuals <- actual_sentiment - predicted_sentiment

library(ggplot2)

# Residuals vs Predicted
ggplot(data = NULL, aes(x = predicted_sentiment, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted", x = "Predicted Values", y = "Residuals")

# Density Plot of Residuals
ggplot(data = NULL, aes(x = residuals)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of Residuals", x = "Residuals", y = "Density")

# QQ Plot of Residuals
qqnorm(residuals)
qqline(residuals, col = "steelblue", lwd = 2)

# Scale-Location Plot (Homoscedasticity)
ggplot(data = NULL, aes(x = predicted_sentiment, y = sqrt(abs(residuals)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Scale-Location Plot", x = "Predicted Values", y = "sqrt(|Residuals|)")
