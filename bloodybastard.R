# Load dplyr
library(dplyr)

# Or load the entire tidyverse
library(tidyverse)


# Calculate interaction terms and new features
train_data <- train_data %>%
  mutate(
    negative_impact = extreme_negative_ratio + moderate_negative_ratio + medium_negative_ratio + slight_negative_ratio,
    positive_impact = extreme_positive_ratio + moderate_positive_ratio + medium_positive_ratio + slight_positive_ratio,
    interaction_term = negative_impact * positive_impact,
    adjusted_neutral_buffer = neutral_ratio + buffer_ratio
  )

# Update random forest model
library(doParallel)
registerDoParallel(cores = detectCores())
rf_model <- randomForest(
  mean_sentiment ~ year + month + day_of_week + interaction_term + adjusted_neutral_buffer,
  data = train_data,
  ntree = 200,
  mtry = 4,
  do.trace = 100,
  importance = TRUE,
  parallel = TRUE  # Enable parallel processing
)
# Load required libraries
library(ggplot2)
library(dplyr)

# Assuming 'test_data' is your dataset

# Fit linear regression model with the best combination of year and month variables
model <- lm(mean_sentiment ~ year*month, data = test_data)

# Make predictions
test_data <- test_data %>%
  mutate(predictions = predict(model))

# Plot the linear regression line
ggplot(test_data, aes(x = post_date, y = mean_sentiment)) +
  geom_point() +
  geom_line(aes(y = predictions), color = "red") +
  labs(x = "Date", y = "Mean Sentiment Score", title = "Linear Regression Model") +
  theme_minimal()

# Calculate R-squared value
r_squared <- summary(model)$r.squared
print(paste("R-squared value:", round(r_squared, 4)))
