# SECTION 1: Sentiment Analysis
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)

# Step 1: Load data and preprocess
file_path <- "C:/Users/admin/AppData/Local/Programs/Python/Python312/posts_data.xlsx"
posts_data <- read_excel(file_path) %>%
  mutate(`Post Content` = as.character(`Post Content`))

# Remove duplicates based on 'Post Content' to avoid processing the same text multiple times
posts_data <- posts_data %>%
  distinct(`Post Content`, .keep_all = TRUE)

# Step 2: Preprocess data
posts_data <- posts_data %>%
  mutate(Post_Content_clean = gsub("[[:punct:]]", "", `Post Content`)) %>%
  mutate(Post_Content_clean = gsub("\\d+", "", Post_Content_clean)) %>%
  unnest_tokens(word, Post_Content_clean) %>%
  anti_join(stop_words)

# Step 3: Remove "uncategorized" and "anonymous"
posts_data <- posts_data %>%
  filter(!word %in% c("uncategorized", "anonymous"))

# Step 4: Calculate sentiment scores for each word
posts_data <- posts_data %>%
  mutate(sentiment_score = get_sentiment(word, method = "afinn"))

# Step 5: Classify sentiments
posts_data <- posts_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"  # Classifying zero sentiment score as Neutral
  ))

# Step 6: Count the sentiment labels
sentiment_counts <- posts_data %>%
  group_by(sentiment_label) %>%
  summarise(
    count = n(),
    mean_sentiment_score = mean(sentiment_score, na.rm = TRUE)
  )

# Print the sentiment counts
print(sentiment_counts)

########PART 2##################

library(dplyr)
library(stringr)
library(lubridate)

# Assuming 'posts_data' is already loaded
posts_data <- posts_data %>%
  mutate(`Post Content` = as.character(`Post Content`),  # Ensure 'Post Content' is character if not already
         post_date = as.Date(post_date)) %>%  # Ensure 'post_date' is in Date format
  arrange(post_date)  # Sort data by 'post_date'

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

# Print the filtered data to check
print(english_posts_data)
library(dplyr)
library(stringr)

# Define a split date
split_date <- as.Date("2017-01-01")  # Adjust this date as necessary for your dataset

# Split data into training and testing sets
train_data <- english_posts_data %>% filter(post_date < split_date)
test_data <- english_posts_data %>% filter(post_date >= split_date)


# Calculate metrics per post correctly
process_data <- function(data) {
  data %>%
    mutate(total_words = str_count(`Post Content`, "\\S+")) %>%
    group_by(Keyword, Page, `Post Number`, Title, `Post Content`, year, month, day_of_week, time, post_date) %>%
    summarise(
      total_words = first(total_words),
      total_sentiment = sum(sentiment_score, na.rm = TRUE),
      total_emotion_words = sum(sentiment_score != 0, na.rm = TRUE),
      neutral_count = sum(sentiment_score == 0, na.rm = TRUE),
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
      mean_sentiment = if_else(total_emotion_words > 0, total_sentiment / total_emotion_words, 0),
      buffer_count = total_words - (total_emotion_words + neutral_count),
      extreme_positive_ratio = extreme_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      extreme_negative_ratio = extreme_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      moderate_positive_ratio = moderate_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      moderate_negative_ratio = moderate_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      medium_positive_ratio = medium_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      medium_negative_ratio = medium_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      slight_positive_ratio = slight_positive_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      slight_negative_ratio = slight_negative_count / if_else(total_emotion_words > 0, total_emotion_words, 1),
      neutral_ratio = neutral_count / if_else(total_words > 0, total_words, 1),
      buffer_ratio = buffer_count / if_else(total_words > 0, total_words, 1)
    )
}

# Apply the function to train and test data
train_data <- process_data(train_data)
test_data <- process_data(test_data)

# Print processed data to check
print(train_data)
print(test_data)

# Factorize categorical variables
train_data$month <- as.factor(train_data$month)
train_data$day_of_week <- as.factor(train_data$day_of_week)
test_data$month <- as.factor(test_data$month)
test_data$day_of_week <- as.factor(test_data$day_of_week)

# Now, each metric should be properly computed for each post
train_data <- train_data %>%
  mutate(
    negative_impact = 1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio,
    positive_impact = 1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio,
    interaction_term = negative_impact * positive_impact,
    squared_negative_impact = negative_impact^2,
    squared_positive_impact = positive_impact^2
  )

test_data <- test_data %>%
  mutate(
    negative_impact = 1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio,
    positive_impact = 1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio,
    interaction_term = negative_impact * positive_impact,
    squared_negative_impact = negative_impact^2,
    squared_positive_impact = positive_impact^2
  )

# First, calculate max values in the training data
max_buffer_ratio <- max(train_data$buffer_ratio, na.rm = TRUE)
max_neutral_ratio <- max(train_data$neutral_ratio, na.rm = TRUE)
max_positive_impact <- max(train_data$positive_impact, na.rm = TRUE)
max_negative_impact <- max(train_data$negative_impact, na.rm = TRUE)

# Then use these max values to scale both training and test data
train_data <- train_data %>%
  mutate(
    scaled_buffer_ratio = buffer_ratio / max_buffer_ratio,
    scaled_neutral_ratio = neutral_ratio / max_neutral_ratio,
    scaled_positive_ratio = positive_impact / max_positive_impact,
    scaled_negative_ratio = negative_impact / max_negative_impact
  )

test_data <- test_data %>%
  mutate(
    scaled_buffer_ratio = buffer_ratio / max_buffer_ratio,
    scaled_neutral_ratio = neutral_ratio / max_neutral_ratio,
    scaled_positive_ratio = positive_impact / max_positive_impact,
    scaled_negative_ratio = negative_impact / max_negative_impact
  )

########PART 3##########################

library(randomForest)
library(dplyr)

# Assuming 'train_data' is your training dataset and has the necessary columns preprocessed
# Define a simpler formula for the randomForest model
sentiment_formula <- mean_sentiment ~ scaled_buffer_ratio + scaled_neutral_ratio +
  scaled_positive_ratio + scaled_negative_ratio +
  scaled_buffer_ratio:scaled_neutral_ratio +  # Interaction between buffer and neutral
  scaled_positive_ratio:scaled_negative_ratio  # Interaction between positive and negative

# Train the random forest model using the defined formula
sentiment_model <- randomForest(
  formula = sentiment_formula,
  data = train_data,
  ntree = 500,
  mtry = 3,  # Adjust based on the number of predictors
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
# Assuming 'test_data' is your testing dataset and also preprocessed similarly
# Scale the ratios to [0, 1] range to normalize their impact

test_data$predicted_sentiment <- predict(sentiment_model, newdata = test_data)
results <- postResample(pred = test_data$predicted_sentiment, obs = test_data$mean_sentiment)
print(results)

########RESULTS###############

# Calculate R-squared value for the test set
predicted_sentiment <- predict(sentiment_model, newdata = test_data)
actual_sentiment <- test_data$mean_sentiment
r_squared <- summary(lm(predicted_sentiment ~ actual_sentiment))$r.squared
print(paste("R-squared: ", r_squared))

###############

# Add predicted sentiments back to the test data
test_data$predicted_sentiment <- predicted_sentiment

# Aggregate data by year, month, and day of the week
test_data_summary <- test_data %>%
  mutate(year = lubridate::year(post_date), 
         month = lubridate::month(post_date, label = TRUE), 
         day_of_week = lubridate::wday(post_date, label = TRUE)) %>%
  group_by(year, month, day_of_week) %>%
  summarise(
    mean_actual_sentiment = mean(mean_sentiment),
    mean_predicted_sentiment = mean(predicted_sentiment)
  ) %>%
  ungroup() %>%
  arrange(year, month, day_of_week)
##############
library(ggplot2)

# Plotting mean sentiment scores by time periods
ggplot(test_data_summary, aes(x = month, y = mean_actual_sentiment, group = year)) +
  geom_line(aes(color = as.factor(year)), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, linetype = "Predicted"), size = 1) +
  facet_wrap(~year) +
  labs(title = "Mean Sentiment Score by Month and Year",
       x = "Month",
       y = "Mean Sentiment Score",
       color = "Year",
       linetype = "Legend") +
  theme_minimal()

# Additional plot for day of the week
ggplot(test_data_summary, aes(x = day_of_week, y = mean_actual_sentiment, group = year)) +
  geom_line(aes(color = as.factor(year)), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, linetype = "Predicted"), size = 1) +
  labs(title = "Mean Sentiment Score by Day of Week",
       x = "Day of Week",
       y = "Mean Sentiment Score",
       color = "Year",
       linetype = "Legend") +
  theme_minimal()
###########TESTS#################################
library(caret)
library(lubridate)

# Assuming 'data_processed' is ordered chronologically
set.seed(123)  # for reproducibility

# Creating time slices manually
initial <- floor(0.8 * nrow(data_processed))
horizon <- nrow(data_processed) - initial
slices <- createTimeSlices(1:nrow(data_processed),
                           initialWindow = initial,
                           horizon = horizon,
                           fixedWindow = TRUE,
                           skip = 0)

# Setup trainControl with manual indices
fitControl <- trainControl(
  method = "timeslice",
  index = slices$train,
  indexOut = slices$test,
  savePredictions = "final",
  verboseIter = TRUE
)

# Define the formula and data
sentiment_formula <- mean_sentiment ~ scaled_buffer_ratio + scaled_neutral_ratio +
  scaled_positive_ratio + scaled_negative_ratio +
  scaled_buffer_ratio:scaled_neutral_ratio +
  scaled_positive_ratio:scaled_negative_ratio

data_processed <- data_processed %>%
  mutate(
    negative_impact = 1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio,
    positive_impact = 1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio,
    interaction_term = negative_impact * positive_impact,
    squared_negative_impact = negative_impact^2,
    squared_positive_impact = positive_impact^2
  )


# First, calculate max values in the training data
max_buffer_ratio <- max(data_processed$buffer_ratio, na.rm = TRUE)
max_neutral_ratio <- max(data_processed$neutral_ratio, na.rm = TRUE)
max_positive_impact <- max(data_processed$positive_impact, na.rm = TRUE)
max_negative_impact <- max(data_processed$negative_impact, na.rm = TRUE)

# Then use these max values to scale both training and test data
data_processed <- data_processed %>%
  mutate(
    scaled_buffer_ratio = buffer_ratio / max_buffer_ratio,
    scaled_neutral_ratio = neutral_ratio / max_neutral_ratio,
    scaled_positive_ratio = positive_impact / max_positive_impact,
    scaled_negative_ratio = negative_impact / max_negative_impact
  )

# Train the model using caret
model <- train(sentiment_formula, data=data_processed,
               method="rf",  # Random forest
               trControl=fitControl,
               tuneLength=5)  # Number of different tuning parameters to try

# Print the model summary
print(model)
############graph#######

library(dplyr)
library(ggplot2)
library(lubridate)

# Assuming 'test_data' already includes predictions named 'predicted_sentiment'
# Add date-related information
test_data <- test_data %>%
  mutate(
    year = year(post_date),
    month = month(post_date, label = TRUE),  # label=TRUE gives month names
    day_of_week = wday(post_date, label = TRUE)  # label=TRUE gives day names
  )

# Aggregate data to calculate means by time units
data_summary <- test_data %>%
  group_by(year, month, day_of_week) %>%
  summarise(
    mean_actual = mean(mean_sentiment, na.rm = TRUE),
    mean_predicted = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year, month, day_of_week)
