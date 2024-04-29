library(tidyverse)
library(randomForest)
library(lubridate)
library(caret)
library(timetk)
library(cld2)

# Checking the structure of the detect_language output for a single example
example_output <- cld2::detect_language("This is a sample text to detect language.")

# Print the structure of the output
print(str(example_output))
library(cld2)
library(dplyr)

# Assuming data is loaded into 'posts_data'
# Use cld2 to detect language and filter out non-English posts
english_posts_data <- posts_data %>%
  mutate(language = cld2::detect_language(`Post Content`)) %>%
  filter(language == "en") %>%  # Filter for English language posts based on 'en' code
  select(-language)  # Remove the language column after filtering

library(dplyr)
library(stringr)
library(dplyr)
library(stringr)

# Assuming 'english_posts_data' is your dataframe and 'Post Content' is a column in it
# Calculate metrics per post correctly without wrongly summing up word counts
data_processed <- english_posts_data %>%
  mutate(
    total_words = str_count(`Post Content`, "\\S+"),  # Correctly count words per Post Content entry
    total_sentiment = sum(sentiment_score, na.rm = TRUE),  # Sum of sentiment scores, grouped below
    total_emotion_words = sum(sentiment_score != 0, na.rm = TRUE),  # Count of words with non-zero sentiment, grouped below
    neutral_count = sum(sentiment_score == 0, na.rm = TRUE)  # Count of neutral sentiment words, grouped below
  ) %>%
  group_by(Keyword, Page, `Post Number`, Title, `Post Content`, year, month, day_of_week, time, post_date) %>%
  summarise(
    total_words = first(total_words),  # Ensure total_words is not summed across groups
    total_sentiment = sum(total_sentiment),
    total_emotion_words = sum(total_emotion_words),
    neutral_count = sum(neutral_count),
    extreme_positive_count = sum(sentiment_score >= 4, na.rm = TRUE),
    extreme_negative_count = sum(sentiment_score <= -4, na.rm = TRUE),
    moderate_positive_count = sum(sentiment_score == 2, na.rm = TRUE),
    moderate_negative_count = sum(sentiment_score == -2, na.rm = TRUE),
    medium_positive_count = sum(sentiment_score == 3, na.rm = TRUE),
    medium_negative_count = sum(sentiment_score == -3, na.rm = TRUE),
    slight_positive_count = sum(sentiment_score == 1, na.rm = TRUE),
    slight_negative_count = sum(sentiment_score == -1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    mean_sentiment = if_else(total_emotion_words > 0, total_sentiment / total_emotion_words, 0),  # Compute mean sentiment or zero if no emotion words
    extreme_positive_ratio = extreme_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    extreme_negative_ratio = extreme_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    moderate_positive_ratio = moderate_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    moderate_negative_ratio = moderate_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    medium_positive_ratio = medium_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    medium_negative_ratio = medium_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    slight_positive_ratio = slight_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    slight_negative_ratio = slight_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
    neutral_ratio = neutral_count / first(total_words),  # Using the first function to ensure we use the direct count of words from mutate
    buffer_count = first(total_words) - (total_emotion_words + neutral_count),
    buffer_ratio = buffer_count / first(total_words)
  )

# Print the processed data to check
print(data_processed)


# Print the processed data to check for correctness
print(data_processed)

# Now, each metric should be properly computed for each post

# This processed data now contains properly computed values for each post based on your specifications.


# Ensure correct data types and further processing as needed
# Continue with your existing code to split data, train models, and make predictions...

# Ensure correct data types and further processing as needed
# Continue with your existing code to split data, train models, and make predictions...

# Ensure correct data types and further processing as needed
# Continue with your existing code to split data, train models, and make predictions...

# Determine the mean and standard deviation for sentiment scores
mean_sentiment <- mean(data_processed$mean_sentiment, na.rm = TRUE)
sd_sentiment <- sd(data_processed$mean_sentiment, na.rm = TRUE)

# Define the upper and lower bounds for outliers
upper_bound <- mean_sentiment + (2 * sd_sentiment)
lower_bound <- mean_sentiment - (2 * sd_sentiment)

# Filter the data to exclude outliers
data_filtered <- data_processed %>%
  filter(mean_sentiment >= lower_bound & mean_sentiment <= upper_bound)

# Split the filtered data into training and testing sets based on the date
split_date <- as.Date("2017-01-01")
train_data <- data_filtered %>% filter(post_date < split_date)
test_data <- data_filtered %>% filter(post_date >= split_date)

# Factorize categorical variables
train_data$month <- as.factor(train_data$month)
train_data$day_of_week <- as.factor(train_data$day_of_week)
test_data$month <- as.factor(test_data$month)
test_data$day_of_week <- as.factor(test_data$day_of_week)


# Train the model with additional predictors and specify mtry = 11
rf_model <- randomForest(
  mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio,
  data = train_data,
  ntree = 500
)


# Predict using the trained model
predictions <- predict(rf_model, newdata = test_data)

# Add predictions to the test_data, and filter out the outlier predictions
test_data <- test_data %>%
  mutate(predicted_sentiment = predictions) %>%
  filter(predicted_sentiment >= lower_bound & predicted_sentiment <= upper_bound)

# Evaluate the model
results <- postResample(pred = test_data$predicted_sentiment, obs = test_data$mean_sentiment)

# Ensure the data types are correct
test_data$post_date <- as.Date(test_data$post_date)
test_data$mean_sentiment <- as.numeric(test_data$mean_sentiment)
test_data$predicted_sentiment <- as.numeric(test_data$predicted_sentiment)

# Plot the graph with data points to see if the data is plotting correctly
ggplot(test_data, aes(x = post_date)) +
  geom_line(aes(y = mean_sentiment, group = 1, colour = "Actual"), size = 1.2) +
  geom_line(aes(y = predicted_sentiment, group = 1, colour = "Predicted"), size = 1.2) +
  geom_point(aes(y = mean_sentiment, colour = "Actual"), size = 1.5) +
  geom_point(aes(y = predicted_sentiment, colour = "Predicted"), size = 1.5) +
  labs(x = "Date", y = "Mean Sentiment Score", title = "Predicted vs Actual Sentiment Scores Over Time") +
  theme_minimal() +
  scale_color_manual("", labels = c("Actual", "Predicted"), values = c("blue", "red"))

# Print evaluation metrics
cat("R-squared Value: ", results['Rsquared'], "\n")
cat("Mean Absolute Error: ", results['MAE'], "\n")
cat("Root Mean Squared Error: ", results['RMSE'], "\n")

importance_posts_data <- importance(rf_model, type=2)
print(importance_posts_data)

varImpPlot(rf_model)

##################DAY_OF_WEEK###########################

# Assuming 'test_data' has been processed and 'day_of_week' is already a correct factor

# Aggregate mean sentiment scores by day of the week for actual and predicted values
weekly_sentiment <- test_data %>%
  group_by(day_of_week) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert 'day_of_week' to a numeric factor for the polynomial fitting
weekly_sentiment$day_of_week_num <- as.numeric(weekly_sentiment$day_of_week)

# Melt the data for plotting with ggplot2
weekly_sentiment_long <- pivot_longer(weekly_sentiment, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for day of the week
ggplot(weekly_sentiment_long, aes(x = day_of_week_num)) +
  geom_point(aes(y = mean_sentiment, color = type)) +
  geom_smooth(data = weekly_sentiment_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "red") +
  geom_smooth(data = weekly_sentiment_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Day of the Week",
    x = "Day of the Week", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7, labels = levels(test_data$day_of_week))

######MONTH############

# Assuming 'test_data' has been processed and 'month' is already a correct factor

# Aggregate mean sentiment scores by month for actual and predicted values
monthly_sentiment <- test_data %>%
  group_by(month) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert 'month' to a numeric factor for the polynomial fitting
monthly_sentiment$month_num <- as.numeric(monthly_sentiment$month)

# Melt the data for plotting with ggplot2
monthly_sentiment_long <- pivot_longer(monthly_sentiment, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for month
ggplot(monthly_sentiment_long, aes(x = month_num)) +
  geom_point(aes(y = mean_sentiment, color = type)) +
  geom_smooth(data = monthly_sentiment_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(data = monthly_sentiment_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Month",
    x = "Month", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12, labels = month.name)

###########YEAR###################

# Assuming 'test_data' has been processed and 'year' is correctly set as a numerical value

# Aggregate mean sentiment scores by year for actual and predicted values
yearly_sentiment <- test_data %>%
  group_by(year) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Melt the data for plotting with ggplot2
yearly_sentiment_long <- pivot_longer(yearly_sentiment, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for year
ggplot(yearly_sentiment_long, aes(x = year, y = mean_sentiment, color = type)) +
  geom_point() +
  geom_smooth(data = yearly_sentiment_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "red") +
  geom_smooth(data = yearly_sentiment_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Year",
    x = "Year", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal()

#############METRICS##############

#install.packages("Metrics")
library(Metrics)
#install.packages("e1071")
library(e1071)

# Current Metrics
rmse_val <- rmse(test_data$mean_sentiment, test_data$predicted_sentiment)
mae_val <- mae(test_data$mean_sentiment, test_data$predicted_sentiment)
r2_val <- cor(test_data$mean_sentiment, test_data$predicted_sentiment)^2

# Adjusted R-squared
n <- nrow(test_data) # Number of observations
p <- length(test_data) - 1 # Number of predictors
adj_r2_val <- 1 - ((1 - r2_val) * (n - 1) / (n - p - 1))

# Print Metrics
cat("RMSE: ", rmse_val, "\n")
cat("MAE: ", mae_val, "\n")
cat("R-squared: ", r2_val, "\n")
cat("Adjusted R-squared: ", adj_r2_val, "\n")

model_summary <- summary(rf_model)
print(model_summary) # Displays the summary of the model including p-values

varImpPlot(rf_model)

library(randomForest)
library(caret)

# Define the number of folds for cross-validation
num_folds <- 10

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = num_folds)

# Train the random forest model using k-fold cross-validation
model_cv <- train(mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl)

# Print the cross-validation results
print(model_cv)

# Fit ANOVA model
model_anova <- aov(
  mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio,
  data = train_data)

# Conduct ANOVA test
anova_results <- summary(model_anova)

# Print ANOVA table
print(anova_results)

#######FURTHERMETRICS####################

# Load necessary libraries
library(car)  # for vif function
library(Metrics)  # for rmse, mae
library(randomForest)
library(caret)
library(tidyverse)

# Calculate VIF to check for collinearity
# Load the necessary library for the VIF function
library(car)

# Fit a linear model to your training data
lm_model <- lm(mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio, data = train_data)

# Calculate VIF to check for collinearity
vif_values <- vif(lm_model)

# Print the VIF values
print("VIF Values to check collinearity:")
print(vif_values)


# Sensitivity Analysis: Adjusting random forest parameters
sensitivity_results <- list()

# Vary mtry and ntree parameters
for (m in c(5, 10, 15)) {
  for (n in c(100, 500, 1000)) {
    set.seed(123)  # for reproducibility
    model <- randomForest(mean_sentiment ~ year + month + day_of_week + neutral_ratio + positive_ratio + negative_ratio,
                          data = train_data, ntree = n, mtry = m)
    preds <- predict(model, newdata = test_data)
    rmse_val <- rmse(test_data$mean_sentiment, preds)
    mae_val <- mae(test_data$mean_sentiment, preds)
    r2_val <- cor(test_data$mean_sentiment, preds)^2
    
    # Collect results
    sensitivity_results[[paste("mtry", m, "ntree", n)]] <- c(RMSE = rmse_val, MAE = mae_val, R_squared = r2_val)
  }
}

# Print Sensitivity Analysis Results
print("Sensitivity Analysis Results:")
print(sensitivity_results)
