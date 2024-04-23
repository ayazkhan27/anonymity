library(tidyverse)
library(randomForest)
library(lubridate)
library(caret)
library(timetk)
library(doParallel)

# Ensure parallel processing is set up for quicker computation
registerDoParallel(cores = detectCores())

# Assuming data is loaded into 'posts_data' and initial preprocessing is done
# Calculate interaction terms and new features
train_data <- train_data %>%
  mutate(
    negative_impact = extreme_negative_ratio + moderate_negative_ratio + medium_negative_ratio + slight_negative_ratio,
    positive_impact = extreme_positive_ratio + moderate_positive_ratio + medium_positive_ratio + slight_positive_ratio,
    interaction_term = negative_impact * positive_impact,
  )
# Step 1: Predicting Ratios for more detailed sentiment impact
# Train models for each detailed ratio
ratio_models <- list(
  neutral_model = randomForest(neutral_ratio ~ year + month + day_of_week, data = train_data, ntree = 500, parallel = TRUE),
  positive_model = randomForest(positive_impact ~ year + month + day_of_week, data = train_data, ntree = 500, parallel = TRUE),
  buffer_model =  randomForest(buffer_ratio ~ year + month + day_of_week, data = train_data, ntree = 500, parallel = TRUE),
  negative_model = randomForest(negative_impact ~ year + month + day_of_week, data = train_data, ntree = 500, parallel = TRUE)
)

# Predict ratios on the training and testing sets
train_data <- train_data %>%
  mutate(
    predicted_neutral = predict(ratio_models$neutral_model, newdata = .),
    predicted_positive = predict(ratio_models$positive_model, newdata = .),
    predicted_negative = predict(ratio_models$negative_model, newdata = .),
    predicted_buffer = predict(ratio_models$buffer_model, newdata = .)
  )

test_data <- test_data %>%
  mutate(
    predicted_neutral = predict(ratio_models$neutral_model, newdata = .),
    predicted_positive = predict(ratio_models$positive_model, newdata = .),
    predicted_negative = predict(ratio_models$negative_model, newdata = .),
    predicted_buffer = predict(ratio_models$buffer_model, newdata = .)
  )

library(randomForest)
library(dplyr)

# Assuming 'train_data' is your training dataset and has the necessary columns preprocessed
# We will modify the formula to consider that the total number of words never exceeds 55

# Update the formula to include the constraint on the total number of words
# Adding a term that represents the proportion of buffer and neutral words to the total words (which is capped at 55)
sentiment_formula <- as.formula("mean_sentiment ~ log1p(buffer_ratio * 55) + exp(neutral_ratio * 55 - 1) + positive_ratio * negative_ratio + I((buffer_ratio + neutral_ratio) * (positive_ratio + negative_ratio) * 55)")

# Train the random forest model using the defined formula
# Note: Ensure parallel processing is set up if the dataset is large
# Set ntree and mtry according to your dataset's characteristics and computational resources
sentiment_model <- randomForest(
  formula = sentiment_formula,
  data = train_data,
  ntree = 500,
  mtry = 4,  # Adjust based on the number of predictors
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
# Assuming 'test_data' is your testing dataset
test_data$predicted_sentiment <- predict(sentiment_model, newdata = test_data)
results <- postResample(pred = test_data$predicted_sentiment, obs = test_data$mean_sentiment)
print(results)


library(tidyverse)  # for data manipulation
library(car)        # for VIF calculations
library(broom)      # for tidy statistical summaries

# Assuming 'train_data' has all necessary variables and is preprocessed

# Fit an OLS model to see the relationships and initial R-squared
ols_model <- lm(mean_sentiment ~ year + month + day_of_week + predicted_positive + predicted_negative + predicted_neutral, data = train_data)
summary(ols_model)

# Calculate VIF for each predictor in the model
vif_values <- vif(ols_model)
print(vif_values)

# Plotting residuals
plot(ols_model$residuals, type = "p", main = "Residuals Plot")
abline(h = 0, col = "red")

# Leverage plot to check for influential cases
influencePlot(ols_model, id.method = "identify", main = "Influence Plot", sub = "Circle size is proportional to Cook's distance")

# Additional diagnostic plots
par(mfrow = c(2, 2))
plot(ols_model)
