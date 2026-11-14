#########
lm_model <- lm(mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio, data = train_data)

# Assuming 'lm_model' is your current model without the neutral_ratio
summary(lm_model)  # Check the overall model fit and individual predictor significance

# Comparing model performance with and without neutral_ratio
# Fit a model including the neutral_ratio for comparison
lm_model_with_neutral <- lm(mean_sentiment ~ year + month + day_of_week + neutral_ratio + positive_ratio + negative_ratio, data = train_data)

# Compare using an ANOVA between models
anova(lm_model, lm_model_with_neutral)

# Calculate performance metrics if needed (e.g., RMSE, R-squared)
predictions_without_neutral <- predict(lm_model, test_data)
predictions_with_neutral <- predict(lm_model_with_neutral, test_data)

rmse_without_neutral <- sqrt(mean((test_data$mean_sentiment - predictions_without_neutral)^2))
rmse_with_neutral <- sqrt(mean((test_data$mean_sentiment - predictions_with_neutral)^2))

cat("RMSE without neutral_ratio: ", rmse_without_neutral, "\n")
cat("RMSE with neutral_ratio: ", rmse_with_neutral, "\n")
