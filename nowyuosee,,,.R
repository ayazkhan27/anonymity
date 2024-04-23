library(tidyverse)
library(randomForest)
library(caret)
library(gbm)
library(xgboost)
library(ranger)
library(recipes)
library(stacks)
library(doParallel)
library(lubridate)  # Adding lubridate for date-time manipulation

# Register parallel backend
cluster <- makeCluster(detectCores())
registerDoParallel(cluster)

# Assuming your data is already sorted by date
# Split the data into training (2010-2016) and testing (2017-2024) sets
train_data <- data_processed[data_processed$year >= 2010 & data_processed$year <= 2016, ]
test_data <- data_processed[data_processed$year >= 2017 & data_processed$year <= 2024, ]

# Define selected columns for feature engineering
selected_cols <- c("year", "month", "day_of_week", "extreme_positive_ratio",  "extreme_negative_ratio",  "moderate_positive_ratio", "moderate_negative_ratio",
"medium_positive_ratio",   "medium_negative_ratio",   "slight_positive_ratio",   "slight_negative_ratio",  
 "neutral_ratio", "buffer_ratio")  # Update with your actual column names

# Assuming 'mean_sentiment' should be included in the modeling
recipe <- recipe(mean_sentiment ~ ., data = train_data) %>%
  update_role(mean_sentiment, new_role = "outcome") %>%
  step_select(all_of(c("mean_sentiment", selected_cols))) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_poly(contains("ratio"), degree = 2)

# Preprocess the recipe
prep_recipe <- prep(recipe, training = train_data)

# Apply preprocessing to training and testing data
train_data_processed <- bake(prep_recipe, new_data = train_data)
test_data_processed <- bake(prep_recipe, new_data = test_data)

# Check again if 'mean_sentiment' is now included
if("mean_sentiment" %in% names(train_data_processed)) {
  print(paste("Length of mean_sentiment:", length(train_data_processed$mean_sentiment)))
} else {
  print("mean_sentiment still not found in train_data_processed")
}
# Ensemble Modeling

# Random Forest Model
complete.cases(train_data_processed)


library(doParallel)
cluster <- makeCluster(detectCores())  # Set up a cluster using all available cores
registerDoParallel(cluster)            # Register the cluster

tr_control_rf <- trainControl(
  method = "cv",            # Cross-validation
  number = 5,               # Number of folds in the cross-validation
  allowParallel = TRUE      # Enable parallel processing
)

rf_model <- train(
  mean_sentiment ~ .,
  data = train_data_processed,
  method = "rf",            # 'rf' for randomForest
  trControl = tr_control_rf,
  tuneGrid = expand.grid(mtry = c(2, 4, 6)),
  ntree = 500               # Number of trees
)



# Gradient Boosting Machine
gbm_model <- train(
  mean_sentiment ~ .,
  data = train_data_processed,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE),
  tuneGrid = expand.grid(n.trees = c(500, 1000, 1500), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1), n.minobsinnode = c(10, 20))
)

# XGBoost
xgb_model <- train(
  mean_sentiment ~ .,
  data = train_data_processed,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE),
  tuneGrid = expand.grid(nrounds = c(500, 1000, 1500), max_depth = c(3, 6, 9), eta = c(0.01, 0.1), gamma = c(0, 0.5), colsample_bytree = c(0.6, 0.8), min_child_weight = c(1, 3, 5), subsample = c(0.5, 0.75))
)

# Random Forest with Ranger
ranger_model <- train(
  mean_sentiment ~ .,
  data = train_data_processed,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE),
  tuneGrid = expand.grid(mtry = c(2, 4, 6), min.node.size = c(1, 5, 10), splitrule = c("gini", "extratrees"))
)

# Ensemble model using stacking
ensemble_model <- stack(
  rf_model,
  gbm_model,
  xgb_model,
  ranger_model,
  metric = "RMSE",
  method = "glmnet",
  control = trainControl(method = "cv", number = 5, allowParallel = TRUE)
)

# Make predictions on the test set
test_predictions <- predict(ensemble_model, newdata = test_data_processed)

# Evaluate the model
test_results <- postResample(test_predictions, test_data$mean_sentiment)
print(test_results)

# Stop the parallel cluster
stopCluster(cluster)
