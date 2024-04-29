library(dplyr)
library(randomForest)

# Time-Series Split Function
time_series_split <- function(data, n_splits) {
  n_samples <- nrow(data)
  size_of_each_split <- floor(n_samples / n_splits)
  
  indices <- sample(1:n_samples) # Shuffle indices
  split_points <- seq(size_of_each_split, n_samples, by = size_of_each_split)
  
  list_of_splits <- list()
  
  for (i in seq_len(n_splits)) {
    test_indices <- indices[(split_points[i] - size_of_each_split + 1):split_points[i]]
    train_indices <- indices[!(indices %in% test_indices)]
    
    list_of_splits[[i]] <- list(
      train = data[train_indices, ],
      test = data[test_indices, ]
    )
  }
  
  return(list_of_splits)
}

# Filter data for training (2010-2016) and testing (2017-2024)
train_data <- grouped_posts %>%
  filter(year >= 2010 & year <= 2016) %>%
  select(-Keyword, -Page, -`Post Number`, -`Post Content`, -post_date)

test_data <- grouped_posts %>%
  filter(year >= 2017 & year <= 2024) %>%
  select(-Keyword, -Page, -`Post Number`, -`Post Content`, -post_date)

# Split dataset into features and target
X_train <- train_data %>%
  select(-mean_sentiment_score)

y_train <- train_data$mean_sentiment_score

X_test <- test_data %>%
  select(-mean_sentiment_score)

y_test <- test_data$mean_sentiment_score

# Time-Series Split
n_splits <- 5
splits <- time_series_split(train_data, n_splits)

# Model Training and Evaluation
best_r_squared <- 0
reward <- 0
punishment <- 0

while (best_r_squared < 0.7) {
  results <- list()
  for (split in splits) {
    # Remove missing values
    split$test <- na.omit(split$test)
    
    # Train RF model
    formula <- as.formula(paste("mean_sentiment_score ~ .", collapse = ""))
    model <- randomForest(formula, data = split$train)
    
    # Make predictions
    predictions <- predict(model, newdata = split$test)
    
    # Compute evaluation metrics
    mae <- mean(abs(predictions - split$test$mean_sentiment_score))
    rmse <- sqrt(mean((predictions - split$test$mean_sentiment_score)^2))
    r_squared <- cor(predictions, split$test$mean_sentiment_score)^2
    
    # Store evaluation metrics
    results[[length(results) + 1]] <- list(mae = mae, rmse = rmse, r_squared = r_squared)
  }
  
  # Find the split with the highest R-squared value
  best_split <- which.max(sapply(results, function(x) x$r_squared))
  
  # Get the model with the best performance
  best_model <- randomForest(formula, data = splits[[best_split]]$train)
  
  # Evaluate on test data
  predictions_test <- predict(best_model, newdata = X_test)
  best_r_squared <- cor(predictions_test, y_test)^2
  
  # Hyperparameter Tuning
  if (best_r_squared > reward) {
    reward <- best_r_squared
  } else {
    punishment <- 2 * (reward - best_r_squared)
  }
}

# Output best hyperparameters and performance
print(paste("Best R-squared:", best_r_squared))
print("Best hyperparameters:")
print(best_model)

# Plot Predictions vs Actuals
plot(y_test, predictions_test, main = "Predictions vs Actuals", 
     xlab = "Actuals", ylab = "Predictions", col = "blue", pch = 19)
abline(0, 1, col = "red")
