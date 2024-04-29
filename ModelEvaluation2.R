# Normalize the training_data_agg dataset
train_mean_sentiment_score <- scale(training_data_agg$mean_sentiment_score)
train_total_posts <- scale(training_data_agg$total_posts)

training_data_agg$mean_sentiment_score_normalized <- train_mean_sentiment_score
training_data_agg$total_posts_normalized <- train_total_posts

# Convert month and day_of_week to numerical representations
training_data_agg$month_num <- as.integer(factor(training_data_agg$month, levels = months))
training_data_agg$day_num <- as.integer(factor(training_data_agg$day_of_week, levels = days_of_week))

# Normalize the numerical representations
train_month_num <- scale(training_data_agg$month_num)
train_day_num <- scale(training_data_agg$day_num)

training_data_agg$month_num_normalized <- train_month_num
training_data_agg$day_num_normalized <- train_day_num

# Perform sensitivity analysis on the normalized data
perform_sensitivity_analysis <- function(data) {
  # Initialize a list to store sensitivity analysis results
  sensitivity_results <- list()
  
  # Define different time variables
  time_variables <- c("year", "month_num_normalized", "day_num_normalized")
  
  # Loop over each time variable
  for (time_var in time_variables) {
    # Fit a linear model with the time variable as the predictor
    model <- lm(mean_sentiment_score_normalized ~ get(time_var), data = data)
    
    # Perform correlation test or ANOVA
    if (time_var == "year") {
      # Pearson's correlation test for numeric predictor
      cor_test <- cor.test(data$mean_sentiment_score_normalized, data$year)
      p_value <- cor_test$p.value
    } else {
      # ANOVA for categorical predictors
      anova_test <- anova(model)
      p_value <- anova_test$`Pr(>F)`[1]
    }
    
    # Store the results
    sensitivity_results[[time_var]] <- list(
      p_value = p_value
    )
  }
  
  # Return the sensitivity analysis results
  return(sensitivity_results)
}

# Perform sensitivity analysis on the normalized training data
sensitivity_results <- perform_sensitivity_analysis(training_data_agg)

# Print the results
print(sensitivity_results)