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

# SECTION 2: Word Frequency Analysis

library(dplyr)

# Assuming posts_data has already been loaded and includes necessary columns
# Group data by 'Keyword', 'Page', 'Post Number', and calculate the mean sentiment score, while preserving the unique "Post Content" and other details
grouped_posts <- posts_data %>%
  group_by(Keyword, Page, `Post Number`) %>%
  summarise(
    "Post Content" = first(`Post Content`),  # Assuming all entries in a group share the same Post Content
    year = first(year),
    month = first(month),
    day_of_week = first(day_of_week),
    time = first(time),
    post_date = first(post_date),
    mean_sentiment_score = mean(sentiment_score, na.rm = TRUE),
    .groups = 'drop'
  )

# Split the grouped data into happy, sad, and neutral posts based on mean sentiment score
happy_posts <- grouped_posts %>%
  filter(mean_sentiment_score > 0)

sad_posts <- grouped_posts %>%
  filter(mean_sentiment_score < 0)

neutral_posts <- grouped_posts %>%
  filter(mean_sentiment_score == 0)

# Print the total counts for each category
cat("Total Happy Posts:", nrow(happy_posts), "\n")
cat("Total Sad Posts:", nrow(sad_posts), "\n")
cat("Total Neutral Posts:", nrow(neutral_posts), "\n")

# Optionally print the first few rows of each dataframe to verify
print(head(happy_posts))
print(head(sad_posts))
print(head(neutral_posts))


##revision
# SECTION 3: Word Clouds

# Step 1: Prepare data for word clouds
library(wordcloud)

# Preprocess data to remove "uncategorized" and "anonymous"
filtered_happy_posts <- happy_posts %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE))

filtered_sad_posts <- sad_posts %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from happy posts
##filtered_happy_posts <- filtered_happy_posts %>%
#mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from sad posts
#filtered_sad_posts <- filtered_sad_posts %>%
#mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Step 2: Create word clouds for happy and sad posts
happy_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_happy_posts$`Post Content`, "\\s+")),
                                        max.words = 100, colors = brewer.pal(8, "Set2"),
                                        scale = c(2.75, 0.5), random.order = FALSE, rot.per = 0.45)

sad_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_sad_posts$`Post Content`, "\\s+")),
                                      max.words = 100, colors = brewer.pal(8, "Dark2"),
                                      scale = c(2.75, 2), random.order = FALSE, rot.per = 0.45)


# SECTION 5: Emotion Intensity Analysis

# Step 1: Load required libraries
library(tidytext)
library(dplyr)

# Step 2: Load Emotion Lexicon
data("nrc")
lexicon <- get_sentiments("nrc")

# Step 3: Tokenize Post Content and filter out words containing "March" or "August"
post_words <- posts_data %>%
  unnest_tokens(word, 'Post Content') %>%
  filter(!grepl("March", word, ignore.case = TRUE) & 
           !grepl("August", word, ignore.case = TRUE))

# Step 4: Join Emotion Lexicon with Tokenized Words
post_words_emotion <- post_words %>%
  inner_join(lexicon, by = "word")

# Step 5: Calculate Proportion of Words Related to Each Emotion
emotion_prop <- post_words_emotion %>%
  count(sentiment) %>%
  mutate(proportion = n / sum(n))

library(extrafont)
font_import()  # This might take a few minutes
loadfonts(device = "win")  # Use device = "win" for Windows; for Mac, use device = "quartz"; for Linux, use device = "cairo"


# Step 6: Visualize Emotion Intensity
library(ggplot2)
library(RColorBrewer)

library(showtext)

# Load Arial font from your system
font_add(family = "Arial", regular = "arial.ttf")  # Ensure you have arial.ttf on your system, adjust if using Helvetica

library(ggplot2)
library(RColorBrewer)

# Create a color palette
emotion_colors <- brewer.pal(n = length(unique(emotion_prop$sentiment)), name = "Set3")

# Plot with customized aesthetics using Arial
ggplot(emotion_prop, aes(x = sentiment, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = emotion_colors) +
  labs(title = "Proportion of Words Related to Each Emotion",
       x = "Emotion", y = "Proportion") +
  theme_minimal() +
  theme(text = element_text(size = 44, family = "Arial"),  # Apply Arial
        axis.title = element_text(size = 46, face = "bold", family = "Arial"),
        plot.title = element_text(size = 50, face = "bold", hjust = 0.5, family = "Arial"),
        legend.title = element_text(size = 46, family = "Arial"),
        legend.text = element_text(size = 44, family = "Arial"))




# Step 7: Visualize Words Contribution to Sentiment Using NRC Lexicon
word_contribution <- post_words_emotion %>%
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ggplot(word_contribution, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Words Contributing to Sentiment Using NRC Lexicon",
       x = "Word", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))


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

library(tidytext)
library(randomForest)
library(dplyr)
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)

predict_mean_sentiment_2 <- function(paragraph, sentiment_model_2) {
  # Clean the paragraph
  paragraph_clean <- gsub("[[:punct:]]", "", paragraph) %>%
    gsub("\\d+", "", .) %>%
    tolower()
  
  # Tokenize the paragraph
  paragraph_df <- data.frame(paragraph_clean = paragraph_clean)
  paragraph_tokenized <- paragraph_df %>%
    unnest_tokens(word, paragraph_clean)
  
  # Check if the tokenization results in an empty data frame
  if (nrow(paragraph_tokenized) == 0) {
    return(data.frame(paragraph = paragraph, predicted_mean_sentiment = NA, stringsAsFactors = FALSE))
  }
  
  # Calculate sentiment scores
  paragraph_sentiment <- paragraph_tokenized %>%
    mutate(sentiment_score = get_sentiment(word, method = "afinn"))
  
  # Aggregate sentiment scores
  paragraph_aggregated <- paragraph_sentiment %>%
    summarise(
      total_words = n(),
      total_sentiment = sum(sentiment_score, na.rm = TRUE),
      total_emotion_words = sum(sentiment_score != 0, na.rm = TRUE),
      neutral_count = sum(sentiment_score == 0, na.rm = TRUE)
    )
  
  # Calculate decay factor
  decay_factor <- exp(-(paragraph_aggregated$neutral_count) / paragraph_aggregated$total_words)
  
  # Adjust decay factor based on sentiment score sign
  if (paragraph_aggregated$total_sentiment < 0) {
    decay_factor <- 1 / decay_factor
  }
  
  # Apply decay factor to adjusted sentiment score
  adjusted_sentiment_score <- paragraph_aggregated$total_sentiment * decay_factor
  
  # Predict sentiment based on the adjusted sentiment score
  predicted_sentiment <- predict(sentiment_model_2, newdata = data.frame(adjusted_sentiment_score))
  
  # Return a data frame with the original paragraph and predicted sentiment
  result <- data.frame(paragraph = paragraph, predicted_mean_sentiment = predicted_sentiment)
  return(result)
}



predict_mean_sentiment <- function(paragraph, sentiment_model_2) {
  # Clean the paragraph
  paragraph_clean <- gsub("[[:punct:]]", "", paragraph) %>%
    gsub("\\d+", "", .) %>%
    tolower()
  
  # Tokenize the paragraph
  paragraph_df <- data.frame(paragraph_clean = paragraph_clean)
  paragraph_tokenized <- paragraph_df %>%
    unnest_tokens(word, paragraph_clean)
  
  # Check if the tokenization results in an empty data frame
  if (nrow(paragraph_tokenized) == 0) {
    return(data.frame(paragraph = paragraph, predicted_mean_sentiment = NA, stringsAsFactors = FALSE))
  }
  
  # Calculate sentiment scores
  paragraph_sentiment <- paragraph_tokenized %>%
    mutate(sentiment_score = get_sentiment(word, method = "afinn"))
  
  # Aggregate sentiment scores
  paragraph_aggregated <- paragraph_sentiment %>%
    summarise(
      total_words = n(),
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
      slight_negative_count = sum(sentiment_score == -1, na.rm = TRUE)
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
      negative_impact = 1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio,
      positive_impact = 1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio,
      interaction_term = negative_impact * positive_impact,
      squared_negative_impact = negative_impact^2,
      squared_positive_impact = positive_impact^2,
      buffer_ratio = buffer_count / if_else(total_words > 0, total_words, 1),
      log_emotion_ratio = log1p(total_emotion_words / total_words),
      exp_buffer_neutral_ratio = exp(min(neutral_count + buffer_count, 10) / total_words),
      decay_factor = exp(-(neutral_count + buffer_count) / total_words),
      abs_adjusted_mean_sentiment = abs(adjusted_mean_sentiment), 
      adjusted_mean_sentiment_sign = sign(adjusted_mean_sentiment),
      adjusted_mean_sentiment = if_else(total_emotion_words > 0, abs_adjusted_mean_sentiment * decay_factor * adjusted_mean_sentiment_sign,
                                        0)  # Apply decay factor to adjusted mean sentiment
    )
  
  # Calculate max values from the paragraph itself
  max_buffer_ratio <- max(paragraph_aggregated$buffer_ratio, na.rm = TRUE)
  max_neutral_ratio <- max(paragraph_aggregated$neutral_ratio, na.rm = TRUE)
  max_positive_impact <- max(1.5 * paragraph_aggregated$slight_positive_ratio + 2 * paragraph_aggregated$medium_positive_ratio + 2.5 * paragraph_aggregated$moderate_positive_ratio + 3 * paragraph_aggregated$extreme_positive_ratio, na.rm = TRUE)
  max_negative_impact <- max(1.5 * paragraph_aggregated$slight_negative_ratio + 2 * paragraph_aggregated$medium_negative_ratio + 2.5 * paragraph_aggregated$moderate_negative_ratio + 3 * paragraph_aggregated$extreme_negative_ratio, na.rm = TRUE)
  
  
  # Scale features based on their calculated maximums
  max_buffer_ratio <- max(paragraph_aggregated$buffer_ratio, na.rm = TRUE)
  max_neutral_ratio <- max(paragraph_aggregated$neutral_ratio, na.rm = TRUE)
  max_interaction_term <- max(train_data$interaction_term, na.rm = TRUE)
  
  # Calculate the maximum positive and negative impact
  max_positive_impact <- max(1.5 * paragraph_aggregated$slight_positive_ratio +
                               2 * paragraph_aggregated$medium_positive_ratio +
                               2.5 * paragraph_aggregated$moderate_positive_ratio +
                               3 * paragraph_aggregated$extreme_positive_ratio -
                               (1.5 * paragraph_aggregated$slight_negative_ratio +
                                  2 * paragraph_aggregated$medium_negative_ratio +
                                  2.5 * paragraph_aggregated$moderate_negative_ratio +
                                  3 * paragraph_aggregated$extreme_negative_ratio), na.rm = TRUE)
  
  max_negative_impact <- max(1.5 * paragraph_aggregated$slight_negative_ratio +
                               2 * paragraph_aggregated$medium_negative_ratio +
                               2.5 * paragraph_aggregated$moderate_negative_ratio +
                               3 * paragraph_aggregated$extreme_negative_ratio -
                               (1.5 * paragraph_aggregated$slight_positive_ratio +
                                  2 * paragraph_aggregated$medium_positive_ratio +
                                  2.5 * paragraph_aggregated$moderate_positive_ratio +
                                  3 * paragraph_aggregated$extreme_positive_ratio), na.rm = TRUE)
  
  # Check if max buffer ratio is 0 and if so, set it to max neutral ratio
  if (max_buffer_ratio == 0) {
    max_buffer_ratio <- max_neutral_ratio
  } else if (max_neutral_ratio == 0) { # Check if max neutral ratio is 0 and if so, set it to max buffer ratio
    max_neutral_ratio <- max_buffer_ratio
  }
  
  # Check if both max positive impact and max negative impact are 0
  if (max_positive_impact == 0 && max_negative_impact == 0) {
    return(data.frame(paragraph = paragraph, predicted_mean_sentiment = 0, stringsAsFactors = FALSE))
  }
  
  # Check if max positive impact is 0, set scaled positive ratio to 0
  if (max_positive_impact == 0) {
    paragraph_aggregated$scaled_positive_ratio <- 0
  }
  
  # Check if max negative impact is 0, set scaled negative ratio to 0
  if (max_negative_impact == 0) {
    paragraph_aggregated$scaled_negative_ratio <- 0
  }
  
  # Ensure max values are not zero or NA
  max_values <- c(max_buffer_ratio, max_neutral_ratio, max_positive_impact, max_negative_impact)
  if (any(max_values == 0 | is.na(max_values))) {
    warning("One or more max scaling factors are zero or NA.")
    return(data.frame(paragraph = paragraph, predicted_mean_sentiment = NA, stringsAsFactors = FALSE))
  }
  
  # Apply scaling
  paragraph_aggregated <- paragraph_aggregated %>%
    mutate(
      scaled_buffer_ratio = buffer_ratio / max_buffer_ratio,
      scaled_neutral_ratio = neutral_ratio / max_neutral_ratio,
      scaled_positive_ratio = if_else(max_positive_impact == 0, -0.5 * max_negative_impact,
                                      (1.5 * paragraph_aggregated$slight_positive_ratio + 2 * paragraph_aggregated$medium_positive_ratio +
                                         2.5 * paragraph_aggregated$moderate_positive_ratio + 3 * paragraph_aggregated$extreme_positive_ratio) / max_positive_impact),
      scaled_negative_ratio = if_else(max_negative_impact == 0, -0.5 * max_positive_impact,
                                      (1.5 * paragraph_aggregated$slight_negative_ratio + 2 * paragraph_aggregated$medium_negative_ratio +
                                         2.5 * paragraph_aggregated$moderate_negative_ratio + 3 * paragraph_aggregated$extreme_negative_ratio) / max_negative_impact)
    )
  
  # Predict mean sentiment if not already set to 0
  if (max_positive_impact != 0 || max_negative_impact != 0) {
    predicted_sentiment <- predict(sentiment_model_2, newdata = paragraph_aggregated)
  } else {
    predicted_sentiment <- 0
  }
  
  # Return a data frame with the original paragraph and predicted sentiment
  result <- data.frame(paragraph = paragraph, predicted_mean_sentiment = predicted_sentiment)
  return(result)
}

# Example usage
paragraph <- "Today was a good day, took the kids to the mall, went up the ladder, construction work, did some errands here and there, but you know what? Today was a goshdarn terrible, miserable day."
predicted_sentiment <- predict_mean_sentiment(paragraph, sentiment_model_2)
print(predicted_sentiment)


#########BOOKTEST############################


#Today was a good day, took the kids to the mall, went up the ladder, construction work, did some errands here and there, but you know what? Today was a goshdarn good day.


Sys.setenv(RETICULATE_PYTHON = "C:/Users/admin/anaconda3/envs/your_conda_environment/python.exe")
library(reticulate)

py_discover_config()  # This can help to see what Python environments are discoverable

# Specify the Python executable directly
use_python("C:/Users/admin/anaconda3/envs/your_conda_environment/python.exe", required = TRUE)



prepare_data <- function(data) {
  # Extract relevant columns and remove rows with any missing values
  data <- data[, c("Post Content", "mean_sentiment")]
  na.omit(data)  # Remove rows with NA values
}

# Example of loading and preparing data
# train_data <- read.csv("path_to_train_data.csv")
# test_data <- read.csv("path_to_test_data.csv")
# train_data <- prepare_data(train_data)
# test_data <- prepare_data(test_data)
library(reticulate)
use_condaenv("your_conda_environment", required = TRUE)

# Assuming the Python environment has transformers installed
transformers <- import('transformers')
torch <- import('torch')

bert_tokenizer <- function() {
  transformers$BertTokenizer$from_pretrained('bert-base-uncased')
}

encode_text <- function(tokenizer, texts) {
  tokenizer$batch_encode_plus(texts, return_tensors = 'pt', padding = TRUE, truncation = TRUE)
}

as.tensor <- function(x) {
  torch$tensor(x)
}

fine_tune_bert <- function(train_data, d_model = 768, num_heads = 12) {
  tokenizer <- bert_tokenizer()
  train_encodings <- encode_text(tokenizer, train_data$`Post Content`)
  
  config <- list(d_model = d_model, num_heads = num_heads)
  bert_model <- transformer(x = array(0, dim = c(1, 10, config$d_model)), 
                            d_model = config$d_model,
                            num_heads = config$num_heads)
  
  train_labels <- as.tensor(train_data$mean_sentiment)
  train_dataset <- list(input_ids = train_encodings$input_ids,
                        attention_mask = train_encodings$attention_mask,
                        labels = train_labels)
  
  # Define training parameters
  train_batch_size <- 8
  num_epochs <- 3
  
  # Assuming the transformer function has a training method
  for (epoch in 1:num_epochs) {
    for (i in seq_len(length(train_dataset$input_ids))) {
      input_batch <- list(input_ids = train_dataset$input_ids[[i]],
                          attention_mask = train_dataset$attention_mask[[i]],
                          labels = train_dataset$labels[[i]])
      # This is a placeholder for the actual training step
      loss <- bert_model$train_step(input_batch)
    }
  }
  
  return(bert_model)
}

evaluate_bert <- function(bert_model, test_data) {
  tokenizer <- bert_tokenizer()
  test_encodings <- encode_text(tokenizer, test_data$`Post Content`)
  
  test_dataset <- list(input_ids = test_encodings$input_ids,
                       attention_mask = test_encodings$attention_mask)
  
  predictions <- list()
  for (i in seq_len(length(test_dataset$input_ids))) {
    input_batch <- list(input_ids = test_dataset$input_ids[[i]],
                        attention_mask = test_dataset$attention_mask[[i]])
    # Placeholder for model prediction
    output <- bert_model$predict(input_batch)
    predictions <- c(predictions, output)
  }
  
  predicted_sentiment <- unlist(predictions)
  actual_sentiment <- test_data$mean_sentiment
  r_squared <- cor(predicted_sentiment, actual_sentiment)^2
  
  return(r_squared)
}


print(paste("BERT R-squared:", r_squared))

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
