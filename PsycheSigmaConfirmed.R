library(tidyverse)
library(randomForest)
library(lubridate)
library(caret)
library(timetk)

# Assuming 'posts_data' has columns: Post Number, post_date, year, month, day_of_week, sentiment_score

# Split the raw data first based on the date
train_raw <- posts_data %>% filter(post_date < as.Date("2017-01-01"))
test_raw <- posts_data %>% filter(post_date >= as.Date("2017-01-01"))


# Now compute the sentiment ratios within each split
compute_ratios <- function(data) {
  data %>%
    group_by("Post Number", post_date, year, month, day_of_week) %>%
    summarise(
      total_words = n(),
      total_sentiment = sum(sentiment_score),
      neutral_count = sum(sentiment_score == 0),
      positive_count = sum(sentiment_score > 0),
      negative_count = sum(sentiment_score < 0),
      .groups = 'drop'
    ) %>%
    mutate(
      mean_sentiment = total_sentiment / total_words,
      neutral_ratio = neutral_count / total_words,
      positive_ratio = positive_count / total_words,
      negative_ratio = negative_count / total_words
    )
}


# Apply the function to each subset
train_data_processed <- compute_ratios(train_raw)
test_data_final_processed <- compute_ratios(test_raw)


# Determine the mean and standard deviation for sentiment scores
mean_sentiment <- mean(train_data_processed$mean_sentiment, na.rm = TRUE)
sd_sentiment <- sd(train_data_processed$mean_sentiment, na.rm = TRUE)
upper_bound <- mean_sentiment + (2 * sd_sentiment)
lower_bound <- mean_sentiment - (2 * sd_sentiment)

# Filter both training and testing datasets to exclude outliers
train_data_final <- train_data_processed %>% 
  filter(mean_sentiment >= lower_bound & mean_sentiment <= upper_bound)
test_data_final_final <- test_data_final_processed %>% 
  filter(mean_sentiment >= lower_bound & mean_sentiment <= upper_bound)


# Continue with your existing steps to train the model
rf_model <- randomForest(
  mean_sentiment ~ year + month + day_of_week + positive_ratio + negative_ratio,
  data = train_data_final,
  ntree = 500,
  mtry = 11
)

# Predict using the trained model
predictions <- predict(rf_model, newdata = test_data_final_final)


# Assume 'predictions' and 'test_data_final_final' are already defined and calculated correctly

# Add predictions to the test data and filter out the outliers based on predicted values
test_data_final_final <- test_data_final_final %>%
  mutate(predicted_sentiment = predictions) %>%
  filter(predicted_sentiment >= lower_bound & predicted_sentiment <= upper_bound)

# Evaluate the model using Caret's postResample for predictions vs actuals
results <- postResample(pred = test_data_final_final$predicted_sentiment, obs = test_data_final_final$mean_sentiment)

# Print evaluation metrics
cat("R-squared Value: ", results['Rsquared'], "\n")
cat("Mean Absolute Error: ", results['MAE'], "\n")
cat("Root Mean Squared Error: ", results['RMSE'], "\n")

# Plot the graph with data points to see if the data is plotting correctly
ggplot(test_data_final_final, aes(x = post_date)) +
  geom_line(aes(y = mean_sentiment, group = 1, colour = "Actual"), size = 1.2) +
  geom_line(aes(y = predicted_sentiment, group = 1, colour = "Predicted"), size = 1.2) +
  geom_point(aes(y = mean_sentiment, colour = "Actual"), size = 1.5) +
  geom_point(aes(y = predicted_sentiment, colour = "Predicted"), size = 1.5) +
  labs(x = "Date", y = "Mean Sentiment Score", title = "Predicted vs Actual Sentiment Scores Over Time") +
  theme_minimal() +
  scale_color_manual("", labels = c("Actual", "Predicted"), values = c("blue", "red"))



varImpPlot(rf_model)

##################DAY_OF_WEEK###########################

# Assuming 'test_data_final' has been processed and 'day_of_week' is already a correct factor

# Aggregate mean sentiment scores by day of the week for actual and predicted values
weekly_sentiment_final <- test_data_final_final %>%
  group_by(day_of_week) %>%
  summarise(
    actual_mean_sentiment = mean(mean_sentiment, na.rm = TRUE),
    predicted_mean_sentiment = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup()

# Convert 'day_of_week' to a numeric factor for the polynomial fitting
weekly_sentiment_final$day_of_week_num <- as.numeric(weekly_sentiment_final$day_of_week)

# Melt the data for plotting with ggplot2
weekly_sentiment_final_long <- pivot_longer(weekly_sentiment_final, cols = c(actual_mean_sentiment, predicted_mean_sentiment), names_to = "type", values_to = "mean_sentiment")

# Plotting for day of the week
ggplot(weekly_sentiment_final_long, aes(x = day_of_week_num)) +
  geom_point(aes(y = mean_sentiment, color = type)) +
  geom_smooth(data = weekly_sentiment_final_long %>% filter(type == "actual_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "red") +
  geom_smooth(data = weekly_sentiment_final_long %>% filter(type == "predicted_mean_sentiment"), aes(y = mean_sentiment), method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs Actual Mean Sentiment Score by Day of the Week",
    x = "Day of the Week", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("actual_mean_sentiment" = "red", "predicted_mean_sentiment" = "blue")) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7, labels = levels(test_data_final_final$day_of_week))

######MONTH############

# Assuming 'test_data_final' has been processed and 'month' is already a correct factor

# Aggregate mean sentiment scores by month for actual and predicted values
monthly_sentiment <- test_data_final_final%>%
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

# Assuming 'test_data_final' has been processed and 'year' is correctly set as a numerical value

# Aggregate mean sentiment scores by year for actual and predicted values
yearly_sentiment <- test_data_final_final%>%
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
rmse_val <- rmse(test_data_final_final$mean_sentiment, test_data_final_final$predicted_sentiment)
mae_val <- mae(test_data_final_final$mean_sentiment, test_data_final_final$predicted_sentiment)
r2_val <- cor(test_data_final_final$mean_sentiment, test_data_final_final$predicted_sentiment)^2

# Adjusted R-squared
n <- nrow(test_data_final_final) # Number of observations
p <- length(test_data_final) - 1 # Number of predictors
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
model_cv <- train(mean_sentiment ~ year + month + day_of_week + neutral_ratio + positive_ratio + negative_ratio,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl)

# Print the cross-validation results
print(model_cv)

# Fit ANOVA model
model_anova <- aov(
  mean_sentiment ~ year + month + day_of_week + neutral_ratio + positive_ratio + negative_ratio,
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
    preds <- predict(model, newdata = test_data_final)
    rmse_val <- rmse(test_data_final$mean_sentiment, preds)
    mae_val <- mae(test_data_final$mean_sentiment, preds)
    r2_val <- cor(test_data_final$mean_sentiment, preds)^2
    
    # Collect results
    sensitivity_results[[paste("mtry", m, "ntree", n)]] <- c(RMSE = rmse_val, MAE = mae_val, R_squared = r2_val)
  }
}

# Print Sensitivity Analysis Results
print("Sensitivity Analysis Results:")
print(sensitivity_results)

# Plot results if needed
sensitivity_df <- bind_rows(lapply(sensitivity_results, tibble::enframe), .id = "model") %>%
  mutate(model = factor(model, levels = names(sensitivity_results))) %>%
  pivot_longer(-model, names_to = "metric", values_to = "value")

ggplot(sensitivity_df, aes(x = model, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~metric, scales = "free") +
  theme_minimal() +
  labs(title = "Model Sensitivity Analysis", x = "Model Parameter", y = "Metric Value")

# Conduct ANOVA test on original model to display in context
anova_results <- summary(aov(mean_sentiment ~ year + month + day_of_week + neutral_ratio + positive_ratio + negative_ratio, data = train_data))
print("ANOVA Results:")
print(anova_results)
########################################################################
library(plotly)
library(dplyr)

# Assuming test_data_final_finalincludes 'year', 'month', 'day_of_week', 'mean_sentiment', 'predicted_sentiment'

# Prepare monthly data
monthly_data <- test_data_final_final%>%
  group_by(year, month) %>%
  summarise(actual_mean_sentiment_score = mean(mean_sentiment, na.rm = TRUE),
            predicted_mean_sentiment_score = mean(predicted_sentiment, na.rm = TRUE)) %>%
  ungroup()

# Prepare daily data
daily_data <- test_data_final_final%>%
  group_by(year, day_of_week) %>%
  summarise(actual_mean_sentiment_score = mean(mean_sentiment, na.rm = TRUE),
            predicted_mean_sentiment_score = mean(predicted_sentiment, na.rm = TRUE)) %>%
  ungroup()

# Ensure 'month' and 'day_of_week' are treated as numeric for plotting
monthly_data$month <- as.numeric(factor(monthly_data$month, levels = month.name))
daily_data$day_of_week_num <- as.numeric(factor(daily_data$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# 3D Plot for Monthly Data
plot_monthly <- plot_ly(monthly_data, x = ~year, y = ~month, z = ~actual_mean_sentiment_score, type = 'scatter3d', mode = 'lines+markers',
                        line = list(color = 'red'), name = 'Actual') %>%
  add_trace(z = ~predicted_mean_sentiment_score, type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'blue'), name = 'Predicted') %>%
  layout(title = "Predicted vs Actual Mean Sentiment Score by Month",
         scene = list(xaxis = list(title = 'Year'),
                      yaxis = list(title = 'Month', ticktext = month.name, tickvals = 1:12),
                      zaxis = list(title = 'Mean Sentiment Score')))

# 3D Plot for Daily Data
plot_daily <- plot_ly(daily_data, x = ~year, y = ~day_of_week_num, z = ~actual_mean_sentiment_score, type = 'scatter3d', mode = 'lines+markers',
                      line = list(color = 'red'), name = 'Actual') %>%
  add_trace(z = ~predicted_mean_sentiment_score, type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'blue'), name = 'Predicted') %>%
  layout(title = "Predicted vs Actual Mean Sentiment Score by Day of the Week",
         scene = list(xaxis = list(title = 'Year'),
                      yaxis = list(title = 'Day of the Week', ticktext = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), tickvals = 1:7),
                      zaxis = list(title = 'Mean Sentiment Score')))

# Show plots
plot_monthly
plot_daily
