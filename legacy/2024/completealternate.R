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
    adjusted_mean_sentiment_score = mean(sentiment_score, na.rm = TRUE)
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

# Revised function to calculate metrics per post correctly

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
      .groups = 'drop'  # Ensures the group data is dropped after summarisation
    ) %>%
    mutate(
      adjusted_mean_sentiment = if_else(total_emotion_words > 0, total_sentiment / total_emotion_words, 0),
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
      buffer_ratio = buffer_count / if_else(total_words > 0, total_words, 1),
      log_emotion_ratio = log1p(total_emotion_words / total_words),
      exp_buffer_neutral_ratio = exp(min(neutral_count + buffer_count, 10) / total_words),
      decay_factor = exp(-(neutral_count + buffer_count) / total_words),
      mean_sentiment = total_sentiment / total_emotion_words,
      abs_adjusted_mean_sentiment = abs(adjusted_mean_sentiment), 
      adjusted_mean_sentiment_sign = sign(adjusted_mean_sentiment),
      adjusted_mean_sentiment = if_else(total_emotion_words > 0, abs_adjusted_mean_sentiment * decay_factor * adjusted_mean_sentiment_sign,
                                        0)  # Apply decay factor to adjusted mean sentiment
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
max_interaction_term <- max(train_data$interaction_term, na.rm = TRUE)

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
library(caret)

# Update the model formula to include the new features and interactions

sentiment_formula <- adjusted_mean_sentiment ~ 
  log_emotion_ratio +
  + (scaled_positive_ratio:scaled_negative_ratio):log_emotion_ratio + scaled_buffer_ratio + scaled_neutral_ratio +
  scaled_positive_ratio + scaled_negative_ratio +
  scaled_buffer_ratio:scaled_neutral_ratio +  # Interaction between buffer and neutral
  scaled_positive_ratio:scaled_negative_ratio + decay_factor

# Train the random forest model using the defined formula
sentiment_model_2 <- randomForest(
  formula = sentiment_formula,
  data = train_data,
  ntree = 80,
  mtry = 3,  # Adjust based on the number of predictors
  importance = TRUE,
  do.trace = 10,
  parallel = TRUE
)

# Predict using the trained model
train_data$predicted_sentiment <- predict(sentiment_model_2, newdata = train_data)


# Print the model summary and importance
print(summary(sentiment_model_2))
varImpPlot(sentiment_model_2)

# Evaluate the model's performance
library(caret)

test_data$predicted_sentiment <- predict(sentiment_model_2, newdata = test_data)
results <- postResample(pred = test_data$predicted_sentiment, obs = test_data$adjusted_mean_sentiment)
print(results)


########RESULTS###############

# Calculate R-squared value for the test set
predicted_sentiment <- predict(sentiment_model_2, newdata = test_data)
actual_sentiment <- test_data$adjusted_mean_sentiment
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
    mean_actual_sentiment = mean(adjusted_mean_sentiment),
    mean_predicted_sentiment = mean(predicted_sentiment)
  ) %>%
  ungroup() %>%
  arrange(year, month, day_of_week)
##############
library(ggplot2)

# Plotting mean sentiment scores by time periods
library(showtext)

# Initialize showtext to automatically use added fonts
showtext_auto(enable = TRUE)

# Load Arial font from your system, ensure it's available
font_add(family = "Arial", regular = "arial.ttf")  # Check the actual path if not found

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
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Arial"),  # Apply Arial with a larger base size
        axis.title = element_text(size = 24, family = "Arial"),
        plot.title = element_text(size = 28, face = "bold", family = "Arial", hjust = 0.5),
        legend.title = element_text(size = 24, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial"))



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

# Plotting mean sentiment scores by years
ggplot(test_data_summary, aes(x = as.factor(year), y = mean_actual_sentiment, group = year)) +
  geom_line(aes(color = as.factor(year)), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, linetype = "Predicted"), size = 1) +
  labs(title = "Mean Sentiment Score by Year",
       x = "Year",
       y = "Mean Sentiment Score",
       color = "Year",
       linetype = "Legend") +
  theme_minimal()

# Convert time column to POSIXct format
test_data$time <- as.POSIXct(test_data$time, format = "%H:%M")

# Aggregate data by hourly timestamp
test_data_hourly <- test_data %>%
  mutate(hour = lubridate::hour(time)) %>%
  group_by(hour) %>%
  summarise(
    mean_actual_sentiment = mean(adjusted_mean_sentiment),
    mean_predicted_sentiment = mean(predicted_sentiment)
  ) %>%
  ungroup() %>%
  arrange(hour)

# Plotting mean sentiment scores by hourly timestamp with time labels in AM/PM format
ggplot(test_data_hourly, aes(x = hour, y = mean_actual_sentiment)) +
  geom_line(aes(color = "Actual"), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, color = "Predicted"), linetype = "dashed", size = 1) +
  labs(title = "Mean Sentiment Score by Hour of Day",
       x = "Hour of Day",
       y = "Mean Sentiment Score",
       color = "Legend",
       linetype = "Legend") +
  scale_x_continuous(breaks = seq(0, 23, by = 1),
                     labels = function(x) {
                       ifelse(x < 12, paste0(x, " AM"), ifelse(x == 12, "12 PM", paste0(x - 12, " PM")))
                     }) +
  theme_minimal()


###############bindinGFORTESTING################
# Combining the data frames
combined_data <- rbind(train_data, test_data)
#######PERFORM 5 OR 10 FOLD CROSS VALIDATION##############


###########TESTS#################################
library(caret)
library(lubridate)

# Assuming 'data_processed' is ordered chronologically
set.seed(123)  # for reproducibility

# Recreating time slices based on actual size of train_data
# Create time slices based on combined_data
initial <- floor(0.8 * nrow(combined_data))
horizon <- nrow(combined_data) - initial
slices <- createTimeSlices(1:nrow(combined_data),
                           initialWindow = initial,
                           horizon = horizon,
                           fixedWindow = TRUE,
                           skip = 0)

# Setup trainControl with the full combined_data indices
fitControl <- trainControl(
  method = "timeslice",
  index = slices$train,
  indexOut = slices$test,
  savePredictions = "final",
  verboseIter = TRUE
)

# Train the model on the combined_data
model <- train(sentiment_formula, data = combined_data,
               method = "rf",  # Random forest
               trControl = fitControl,
               tuneLength = 5)  # Tune across different mtry values

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
    mean_actual = mean(adjusted_mean_sentiment, na.rm = TRUE),
    mean_predicted = mean(predicted_sentiment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year, month, day_of_week)

library(dplyr)

# Summarizing data to get average sentiments per year
yearly_sentiments <- combined_data %>%
  group_by(year) %>%
  summarise(mean_actual_sentiment = mean(adjusted_mean_sentiment, na.rm = TRUE),
            mean_predicted_sentiment = mean(predicted_sentiment, na.rm = TRUE))

library(ggplot2)

# Enhanced Plotting
ggplot(yearly_sentiments, aes(x = year)) +
  geom_line(aes(y = mean_actual_sentiment, color = "Actual Sentiment"), linewidth = 1) +
  geom_point(aes(y = mean_actual_sentiment, color = "Actual Sentiment")) +
  geom_line(aes(y = mean_predicted_sentiment, color = "Predicted Sentiment"), size = 1, linetype = "dashed") +
  geom_point(aes(y = mean_predicted_sentiment, color = "Predicted Sentiment")) +
  scale_color_manual(values = c("Actual Sentiment" = "blue", "Predicted Sentiment" = "red")) +
  labs(title = "Mean and Predicted Sentiment Scores by Year",
       x = "Year",
       y = "Sentiment Score",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot variable importance
varImpPlot(sentiment_model_2)

# Get variable importance
var_importance <- importance(sentiment_model_2)

# Print the variable importance values
print(var_importance)


######modelplot###
library(randomForest)
library(extrafont)

# Plot the random forest model
plot(sentiment_model_2, main = "Sentiment Gummy Worm (SGW) Model",
     cex.main = 2.5,  # Increase the main title size
     cex.lab = 2.2,   # Increase the axis labels size
     cex.axis = 2.1,  # Increase the axis text size
     family = "Arial")  # Ensure Arial is used


# Save the model to an RDS file
saveRDS(sentiment_model_2, "sentiment_model_2.rds")

# Optionally, you can also save it using `save`, which saves an R object under its name in an .RData file
save(sentiment_model_2, file = "sentiment_model_2.RData")
