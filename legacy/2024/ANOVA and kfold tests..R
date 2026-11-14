library(dplyr)
library(randomForest)
library(caret)

# Step 1: Calculate mean sentiment score for each post
posts_data <- posts_data %>%
  group_by(year, month, `day_of_week`) %>%
  summarise(mean_sentiment_score = mean(sentiment_score))

# Step 2: Aggregate data to get mean sentiment score by other variables
training_data_stuff <- posts_data %>%
  group_by(year, month, `day_of_week`) %>%
  summarise(mean_sentiment_score = mean(mean_sentiment_score, na.rm = TRUE),
            total_posts = n())

# Step 3: Prepare dataset for ANOVA
# Select predictor variables and target variable
predictor_vars <- c("year", "month", "day_of_week")
target_var <- "mean_sentiment_score"
training_data_anova <- select(training_data_stuff, predictor_vars, target_var)

# Step 4: Perform ANOVA
model_anova <- aov(mean_sentiment_score ~ ., data = training_data_anova)

# Step 5: Print ANOVA results
anova_results <- summary(model_anova)
print(anova_results)

# Step 6: Prepare dataset for K-fold cross-validation
# Select predictor variables and target variable
predictor_vars <- c("Keyword", "Page", "total_posts")
target_var <- "mean_sentiment_score"
training_data_cv <- select(training_data, predictor_vars, target_var)

# Step 7: Perform K-fold cross-validation
set.seed(123)  # for reproducibility
model_cv <- train(training_data_cv[, -which(names(training_data_cv) == target_var)], 
                  training_data_cv[[target_var]], 
                  method = "rf", 
                  trControl = trainControl(method = "cv", number = 5))  # 5-fold cross-validation

# Step 8: Print K-fold cross-validation results
print(model_cv)

# Step 9: Train the random forest model with a different name
model_rf <- randomForest(training_data_cv[, -which(names(training_data_cv) == target_var)], 
                         training_data_cv[[target_var]])

# View model summary
print(model_rf)
