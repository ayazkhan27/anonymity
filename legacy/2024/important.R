library(randomForest)
library(lubridate)
library(dplyr)

# Assuming 'grouped_posts' is already loaded and has a 'post_date' column

# Convert 'post_date' to proper Date format and extract components
grouped_posts <- grouped_posts %>%
  mutate(
    post_date = as.Date(post_date, format = "%Y-%m-%d"),
    year = year(post_date),
    month = month(post_date),
    day_of_week = wday(post_date, label = TRUE),
    time = hour(post_date) * 60 + minute(post_date)  # Convert time to minutes past midnight
  )

# Create additional transformed features
grouped_posts <- grouped_posts %>%
  mutate(
    year_squared = year^2,
    month_inverse = 1 / month,
    day_of_week_squared = as.numeric(day_of_week)^2,
    time_squared = time^2,
    year_month_interaction = year * month,
    year_time_interaction = year * time
  )

# Define the formula for the model
formula <- mean_sentiment_score ~ year + month + day_of_week + time +
  year_squared + month_inverse + day_of_week_squared + time_squared +
  year_month_interaction + year_time_interaction

# Split data into training and testing sets
set.seed(42)
train_indices <- createDataPartition(grouped_posts$mean_sentiment_score, p = 0.8, list = FALSE)
train_data <- grouped_posts[train_indices, ]
test_data <- grouped_posts[-train_indices, ]

# Train the Random Forest model
set.seed(42)
model <- randomForest(formula, data = train_data, ntree = 500, importance = TRUE)

# Predict on test data
predictions <- predict(model, test_data)

# Evaluate the model
actual <- test_data$mean_sentiment_score
predicted <- predictions
r2_score <- cor(actual, predicted)^2  # Calculate R-squared

# Print R-squared value
print(paste("R-squared for the model: ", r2_score))

# Variable importance plot
importance <- importance(model)
varImpPlot(model)



# Load necessary library
library(randomForest)

# Fit the model
set.seed(42)  # for reproducibility
model <- randomForest(mean_sentiment_score ~ year + month + day_of_week +
                      year_squared + month_inverse + day_of_week_squared +
                      year_month_interaction,
                      data = grouped_posts, importance = TRUE, ntree = 500)

# Check model summary
print(model)

# Optionally, plot importance again to confirm the effectiveness of selected features
varImpPlot(model)
