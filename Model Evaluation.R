#SECTION 10: MODEL EVALUATION

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(daily_data_agg$true_mean_sentiment_score - daily_data_agg$predicted_mean_sentiment_score))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((daily_data_agg$true_mean_sentiment_score - daily_data_agg$predicted_mean_sentiment_score)^2))

# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((daily_data_agg$true_mean_sentiment_score - daily_data_agg$predicted_mean_sentiment_score) / daily_data_agg$true_mean_sentiment_score)) * 100

# Calculate Percentage Accuracy
percentage_accuracy <- 100 - mape

# Calculate R-squared (R^2) Score
r_squared <- cor(daily_data_agg$true_mean_sentiment_score, daily_data_agg$predicted_mean_sentiment_score)^2

# Create a table for model evaluation metrics
evaluation_metrics <- data.frame(
  Metric = c("Mean Absolute Error (MAE)", "Root Mean Squared Error (RMSE)", "Mean Absolute Percentage Error (MAPE)", "Percentage Accuracy", "R-squared (R^2) Score"),
  Value = c(mae, rmse, mape, percentage_accuracy, r_squared)
)

# Color scale function for visualization
color_scale <- scale_fill_gradient(low = "red", high = "green")

# Update color scale function for visualization
color_scale <- scale_fill_manual(values = c("Mean Absolute Error (MAE)" = "red", 
                                            "Root Mean Squared Error (RMSE)" = "blue", 
                                            "Mean Absolute Percentage Error (MAPE)" = "green", 
                                            "Percentage Accuracy" = "orange", 
                                            "R-squared (R^2) Score" = "purple"))

# Create a bar plot for evaluation metrics with updated color scale
evaluation_plot <- ggplot(evaluation_metrics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Value, 2))), vjust = -0.5) +
  labs(title = "Model Evaluation Metrics", y = "Value", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  color_scale

# Print visualization of evaluation metrics
print("Visualization of Evaluation Metrics:")
evaluation_plot


# Load necessary library for table formatting
library(kableExtra)

# Print evaluation metrics table and plot
print("Model Evaluation Metrics:")
kable(evaluation_metrics, format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE) %>%
  column_spec(2, width = "5em") %>%
  as.character() %>%
  cat()

print("Visualization of Evaluation Metrics:")
evaluation_plot

# Detailed explanation of each metric

# 1. Mean Absolute Error (MAE)
# MAE measures the average absolute difference between the true and predicted mean sentiment scores. 
# It indicates the average magnitude of errors made by the model, regardless of their direction.
# Lower MAE values imply better model performance, as it means the model's predictions are closer to the true values on average.

# 2. Root Mean Squared Error (RMSE)
# RMSE calculates the square root of the average squared differences between the true and predicted mean sentiment scores. 
# It penalizes larger errors more heavily than smaller ones, making it sensitive to outliers.
# Lower RMSE values indicate better model performance, as it means the model's predictions are more accurate overall.

# 3. Mean Absolute Percentage Error (MAPE)
# MAPE measures the average percentage difference between the true and predicted mean sentiment scores.
# It provides insight into the relative magnitude of errors compared to the true values.
# Lower MAPE values imply better model performance, as it means the model's predictions are closer to the true values on average, relative to the true values.

# 4. Percentage Accuracy
# Percentage Accuracy represents the percentage of accuracy achieved by the model, calculated as 100 minus the MAPE.
# Higher values indicate higher accuracy of the model's predictions.
# This metric provides a more intuitive understanding of the model's performance for non-statistics readers.

# 5. R-squared (R^2) Score
# R-squared measures the proportion of variance in the dependent variable (true mean sentiment scores) that is explained by the independent variables (predicted mean sentiment scores).
# It ranges from 0 to 1, where 1 indicates a perfect fit.
# Higher R-squared values indicate better model performance in explaining the variability in the true sentiment scores.

# Print evaluation metrics in text format
cat("Evaluation Metrics:\n")
for (i in 1:nrow(evaluation_metrics)) {
  cat(paste0(evaluation_metrics$Metric[i], ": ", round(evaluation_metrics$Value[i], 2), "\n"))
}
