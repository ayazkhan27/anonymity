library(dplyr)
library(ggplot2)

# Assume 'combined_data' is already loaded with appropriate sentiment and emotion word data
# Calculate the total emotion ratio and absolute change in mean sentiment
combined_data <- combined_data %>%
  mutate(
    mean_sentiment_shifted = abs(mean_sentiment) + 0.000000000000001,  # Shift by a small constant to avoid division by zero
    percent_change_mean_sentiment = (abs(mean_sentiment) - abs(lag(mean_sentiment, default = first(mean_sentiment)))) / mean_sentiment_shifted * 100
  ) %>%
  filter(!is.na(percent_change_mean_sentiment))


# Removing any potential infinite or missing values from the ratio for safety
combined_data <- combined_data %>%
  mutate(total_emotion_ratio = ifelse(is.infinite(total_emotion_ratio) | is.na(total_emotion_ratio), 0, total_emotion_ratio))

# Plotting the relationship using ggplot2
ggplot(combined_data, aes(x = scaled_buffer_ratio*scaled_neutral_ratio, y = mean_sentiment_shifted)) +
  geom_point(alpha = 0.5) +  # Plot points with partial transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Add a linear regression line without a confidence envelope
  labs(title = "Relationship Between Absolute Change in Mean Sentiment and Total Emotion Ratio",
       x = "Product of Scaled Positive & Negative Ratio",
       y = "Absolute Change in Mean Sentiment") +
  theme_minimal()  # Use a minimal theme for a clean look

##########################correlation##########

library(dplyr)
library(ggplot2)

# Assuming 'combined_data' is already loaded and contains all the variables
# Calculate the absolute change in mean sentiment (as an example)
combined_data <- combined_data %>%
  mutate(
    mean_sentiment_lag = lag(mean_sentiment, default = first(mean_sentiment)),
    absolute_change_in_sentiment = abs(mean_sentiment - mean_sentiment_lag)
  ) %>%
  filter(!is.na(absolute_change_in_sentiment))

# Create a data frame to store correlation coefficients
correlations <- data.frame(
  variable = character(),
  correlation_with_y = numeric(),
  stringsAsFactors = FALSE
)

# Assuming 'combined_data' has been preprocessed appropriately
variable_names <- setdiff(names(combined_data), c("absolute_change_in_sentiment", "mean_sentiment_lag", "mean_sentiment"))

# Initialize a data frame to store correlation coefficients
correlations <- data.frame(variable = character(), correlation_with_y = numeric(), stringsAsFactors = FALSE)

# Calculate correlation for each numeric variable with 'absolute_change_in_sentiment'
for(var in variable_names) {
  # Check if the variable is numeric
  if(is.numeric(combined_data[[var]])) {
    cor_value <- cor(combined_data[[var]], combined_data$absolute_change_in_sentiment, use = "complete.obs")
    correlations <- rbind(correlations, data.frame(variable = var, correlation_with_y = cor_value))
  } else {
    # Optionally, print a message if the variable is not numeric
    message("Variable '", var, "' is not numeric and will be skipped.")
  }
}

# View the correlations data frame
print(correlations)


# Sort by absolute value of correlation to identify the strongest relationships
correlations <- correlations %>%
  arrange(desc(abs(correlation_with_y)))

# View correlations
print(correlations)

# Now let's create a function to plot correlation graphs for the top n variables with the highest absolute correlation
plot_correlation <- function(data, variable_name, response_variable) {
  ggplot(data, aes_string(x = variable_name, y = response_variable)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    labs(title = paste("Relationship between", response_variable, "and", variable_name),
         x = variable_name,
         y = response_variable) +
    theme_minimal()
}

# Apply the function to plot for the top n variables
# For example, to plot for the top 5 variables
top_n <- 5
plots_list <- lapply(head(correlations$variable, top_n), function(var) {
  plot_correlation(combined_data, var, "absolute_change_in_sentiment")
})

# Display the plots (they will appear in your R plotting window)
for (p in plots_list) {
  print(p)
}
