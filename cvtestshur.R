# Install packages if not already installed
if (!require("doParallel")) install.packages("doParallel")
if (!require("caret")) install.packages("caret")

# Load libraries
library(doParallel)
library(caret)

# Register parallel backend to use multiple cores
registerDoParallel(cores = detectCores() - 1)  # use one less than the total number of cores
# Assuming 'train_data' is your full dataset already prepared
set.seed(123)  # for reproducibility
train_index <- createDataPartition(train_data$adjusted_mean_sentiment, p = 0.8, list = FALSE)
train_set <- train_data[train_index, ]
test_set <- train_data[-train_index, ]

# Define your custom formula
sentiment_formula <- adjusted_mean_sentiment ~ 
  log_emotion_ratio +
  + (scaled_positive_ratio:scaled_negative_ratio):log_emotion_ratio + scaled_buffer_ratio + scaled_neutral_ratio +
  scaled_positive_ratio + scaled_negative_ratio +
  scaled_buffer_ratio:scaled_neutral_ratio +  # Interaction between buffer and neutral
  scaled_positive_ratio:scaled_negative_ratio + decay_factor

# Train the model using your formula
model_rf <- randomForest(sentiment_formula, data = train_set, mtry = 3, ntree = 80)

# Predict on the test set
predictions <- predict(model_rf, test_set)

# Calculate performance metrics
test_rmse <- RMSE(predictions, test_set$adjusted_mean_sentiment)
test_r_squared <- cor(predictions, test_set$adjusted_mean_sentiment)^2

print(paste("Test RMSE: ", test_rmse))
print(paste("Test R-squared: ", test_r_squared))

# Set up 10-fold cross-validation with parallel processing
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  allowParallel = TRUE  # This enables parallel processing
)

# Train the model using cross-validation
model_cv <- train(
  sentiment_formula,
  data = train_data,
  method = "rf",
  trControl = fitControl,
  tuneLength = 5,
  preProcess = "scale"  # only if scaling is needed
)

# Summarize the results
results <- model_cv$results
overall_rmse <- min(results$RMSE)
overall_r_squared <- max(results$Rsquared)

print(paste("Cross-Validated RMSE: ", overall_rmse))
print(paste("Cross-Validated R-squared: ", overall_r_squared))

# Plotting model performance across different tuning parameters
plot(model_cv)

print(model_cv)
#######furhtermetrics#########

# Assuming 'model_rf' is your trained model and 'test_set' is your test data
predictions <- predict(model_rf, test_set)
residuals <- test_set$adjusted_mean_sentiment - predictions

plot(predictions, residuals, main = "Residual vs. Predicted", xlab = "Predicted", ylab = "Residuals")
abline(h = 0, col = "red")

hist(residuals, breaks = 30, main = "Histogram of Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

# Assuming predictions and actual values are stored in test_set
residuals <- test_set$adjusted_mean_sentiment - predictions

# Plot residuals to check for autocorrelation visually
plot(residuals, type = 'l', main = "Residuals Plot", xlab = "Observation", ylab = "Residuals")
abline(h = 0, col = "red")

# Additionally, use ACF to examine autocorrelation
acf(residuals, main = "Autocorrelation Function")

# Calculate residuals
residuals <- test_set$adjusted_mean_sentiment - predictions

# Plot residuals against predicted values
plot(predictions, residuals,
     main = "Residuals vs. Predicted Values",
     xlab = "Predicted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")



mean_residuals <- mean(residuals)
print(paste("Mean of Residuals:", mean_residuals))
