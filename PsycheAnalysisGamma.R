###PREBOOT:
# Assuming your dataset is named testing_data
# Replace 'day_of_week' with the actual column name in your dataset
# Create a new column 'day_count' to store the total occurrences for each day of the week
testing_data$day_count <- NA

# Loop through each year from 2017 to 2024
for (year in 2017:2024) {
  # Calculate the count for each day of the week in the current year
  for (day in c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) {
    day_count <- sum(testing_data$year == year & testing_data$day_of_week == day, na.rm = TRUE)
    # Assign the count to the corresponding rows in the 'day_count' column
    testing_data$day_count[testing_data$year == year & testing_data$day_of_week == day] <- day_count
  }
}

# Print the updated dataset
head(testing_data)

# Assuming your dataset is named testing_data
# Replace 'month' with the actual column name in your dataset
# Create a new column 'month_count' to store the total occurrences for each month
testing_data$month_count <- NA

# Loop through each year from 2017 to 2024
for (year in 2017:2024) {
  # Calculate the count for each month in the current year
  for (month in c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) {
    month_count <- sum(testing_data$year == year & testing_data$month == month, na.rm = TRUE)
    # Assign the count to the corresponding rows in the 'month_count' column
    testing_data$month_count[testing_data$year == year & testing_data$month == month] <- month_count
  }
}

# Print the updated dataset
head(testing_data)



# SECTION 1: Sentiment Analysis
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)

# Step 1: Load data and preprocess
file_path <- "C:/Users/admin/AppData/Local/Programs/Python/Python312/posts_data.xlsx"

# Read data from all sheets
posts_data <- lapply(excel_sheets(file_path), function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    mutate_all(as.character)
}) %>% 
  bind_rows() %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE)) # Remove "uncategorized" and "anonymous"

# Step 2: Preprocess data
posts_data <- posts_data %>%
  mutate(Post_Content_clean = gsub("[[:punct:]]", "", `Post Content`)) %>%
  mutate(Post_Content_clean = gsub("\\d+", "", Post_Content_clean)) %>%
  unnest_tokens(word, Post_Content_clean) %>%
  anti_join(stop_words)

# Step 3: Remove "uncategorized" and "anonymous"
posts_data <- posts_data %>%
  filter(!word %in% c("uncategorized", "anonymous"))

# Step 4: Calculate sentiment scores
posts_data <- posts_data %>%
  mutate(sentiment_score = get_sentiment(word, method = "afinn"))

# Step 5: Remove repeated keywords
posts_data <- posts_data %>%
  group_by(Keyword, word) %>%
  mutate(word_count = n()) %>%
  filter(word_count < 3) %>%
  ungroup() %>%
  select(-word_count)

# Step 6: Filter out introductory website text
introductory_text <- c("Love","Work","Family","Friends","Games","Kids", "Life", "Home", "About", "Search", "Write", "Categories", "Login", "Write what you feel...", "Please enter a keyword, term, or post number to search for:")
posts_data <- posts_data %>%
  filter(!word %in% introductory_text)

# Step 7: Remove neutral sentiments and sentiment scores of 0
posts_data <- posts_data %>%
  filter(sentiment_score != 0) %>%
  filter(sentiment_score != "Neutral")

# Step 8: Classify posts as positive or negative
posts_data <- posts_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative"
  ))

# Step 9: Aggregate sentiment scores and count posts
aggregate_sentiment <- posts_data %>%
  group_by(sentiment_label) %>%
  summarise(mean_sentiment_score = mean(sentiment_score),
            total_posts = n())

# Print the aggregated sentiment scores
print(aggregate_sentiment)

# SECTION 2: Word Frequency Analysis

# Step 1: Define Happy and Sad Keywords
happy_keywords <- c("joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive")

sad_keywords <- c("death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast")

# Step 2: Count Word Frequency for Happy and Sad Keywords
count_keywords <- function(text, keywords) {
  keyword_count <- sum(grepl(paste0(keywords, collapse = "|"), text, ignore.case = TRUE))
  return(keyword_count)
}

# Define Happy and Sad Keywords
happy_keywords <- c("joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive")

sad_keywords <- c("death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast")

# Count word occurrences for happy and sad keywords
posts_data$happy_keyword_count <- sapply(posts_data$`Post Content`, count_keywords, keywords = happy_keywords)
posts_data$sad_keyword_count <- sapply(posts_data$`Post Content`, count_keywords, keywords = sad_keywords)

# Step 3: Filter out posts with no happy keywords or only sad keywords
happy_posts <- posts_data %>%
  filter(happy_keyword_count > 0 & sad_keyword_count == 0) # Exclude posts with sad keywords

sad_posts <- posts_data %>%
  filter(sad_keyword_count > 0 & happy_keyword_count == 0) # Exclude posts with happy keywords

# Step 4: Aggregate keyword counts and count posts
aggregate_keywords <- posts_data %>%
  summarise(total_happy_posts = sum(happy_keyword_count),
            total_sad_posts = sum(sad_keyword_count))

# Print the aggregated keyword counts
print(aggregate_keywords)
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
filtered_happy_posts <- filtered_happy_posts %>%
  mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from sad posts
filtered_sad_posts <- filtered_sad_posts %>%
  mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Step 2: Create word clouds for happy and sad posts
happy_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_happy_posts$`Post Content`, "\\s+")),
                                        max.words = 100, colors = brewer.pal(8, "Dark2"),
                                        scale = c(2, 0.7), random.order = FALSE, rot.per = 0.35)

sad_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_sad_posts$`Post Content`, "\\s+")),
                                      max.words = 100, colors = brewer.pal(8, "Dark2"),
                                      scale = c(2, 0.7), random.order = FALSE, rot.per = 0.35)

###DEADSECTION
###SECTION 4 = FAILURE###

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

# Step 6: Visualize Emotion Intensity
library(ggplot2)
ggplot(emotion_prop, aes(x = sentiment, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Proportion of Words Related to Each Emotion",
       x = "Emotion", y = "Proportion") +
  theme_minimal()

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

#SECTION 6: TEMPORAL ANALYSIS

# Load necessary libraries
library(dplyr)
library(randomForest)
library(ggplot2)

# Data Preparation
years <- 2009:2024
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
days_of_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Function to extract date from Post Content
extract_date <- function(content) {
  date_pattern <- "\\b\\w+ \\d{1,2}, \\d{4}\\b"
  date_match <- regmatches(content, regexpr(date_pattern, content))
  date <- as.Date(date_match, format = "%B %d, %Y")
  return(date)
}

# Data Preparation
posts_data$post_date <- as.Date(gsub(paste0(".*\\b(", paste0(years, collapse = "|"), ")\\b.*"), "\\1", posts_data$`Post Content`), format = "%Y")
posts_data$year <- lubridate::year(posts_data$post_date)
# Function to extract date from Post Content
extract_date <- function(content) {
  date_pattern <- "\\b\\w+ \\d{1,2}, \\d{4}\\b"
  date_match <- regmatches(content, regexpr(date_pattern, content))
  date <- as.Date(date_match, format = "%B %d, %Y")
  return(date)
}

# Function to determine day of the week
determine_day_of_week <- function(date) {
  return(weekdays(date))
}

# Data Preparation
posts_data$post_date <- as.Date(gsub(paste0(".*\\b(", paste0(years, collapse = "|"), ")\\b.*"), "\\1", posts_data$`Post Content`), format = "%Y")
posts_data$month <- NA
for (i in 1:length(months)) {
  posts_data$month[grepl(months[i], posts_data$`Post Content`, ignore.case = TRUE)] <- months[i]
}
posts_data$day_of_week <- NA
for (i in 1:length(days_of_week)) {
  posts_data$day_of_week[grepl(days_of_week[i], posts_data$`Post Content`, ignore.case = TRUE)] <- days_of_week[i]
}

# Remove rows with missing or NA values in the year column
posts_data <- posts_data[complete.cases(posts_data$year), ]

# Subset Data for Training and Testing
training_data <- subset(posts_data, year(post_date) %in% 2009:2016)
testing_data <- subset(posts_data, year(post_date) %in% 2017:2024)

# Apply functions to extract dates and determine day of the week
training_data$extracted_date <- sapply(training_data$`Post Content`, extract_date)
training_data$day_of_week[is.na(training_data$day_of_week)] <- sapply(training_data$extracted_date[is.na(training_data$day_of_week)], determine_day_of_week)
training_data$extracted_date <- NULL

# Feature Extraction
training_data$mean_sentiment_score <- training_data$sentiment_score
testing_data$mean_sentiment_score <- testing_data$sentiment_score

# Aggregate Sentiment Score by Year, Month, and Day of the Week
training_data_agg <- training_data %>%
  group_by(year, month, day_of_week) %>%
  summarise(mean_sentiment_score = mean(mean_sentiment_score, na.rm = TRUE),
            total_posts = n())

# Train Random Forest Model for the dataset
model <- randomForest(mean_sentiment_score ~ year + month + day_of_week + total_posts, data = training_data_agg)


###SECTION 7: VISUALIZATION
# Visualization
# Temporal Trends in Mean Sentiment Score by Year
plot_year <- ggplot(training_data_agg, aes(x = factor(year), y = mean_sentiment_score, size = total_posts)) +
  geom_point() +
  labs(title = "Temporal Trends in Mean Sentiment Score by Year (Training Data)",
       x = "Year", y = "Mean Sentiment Score") +
  scale_size_continuous(name = "Total Posts")

# Temporal Trends in Mean Sentiment Score by Month
plot_month <- ggplot(training_data_agg, aes(x = factor(month, levels = months), y = mean_sentiment_score, size = total_posts)) +
  geom_point() +
  labs(title = "Mean Sentiment Score by Month (Training Data)",
       x = "Month", y = "Mean Sentiment Score") +
  scale_size_continuous(name = "Total Posts")

# Temporal Trends in Mean Sentiment Score by Day of the Week
plot_day <- ggplot(training_data_agg, aes(x = factor(day_of_week, levels = days_of_week), y = mean_sentiment_score, size = total_posts)) +
  geom_point() +
  labs(title = "Mean Sentiment Score by Day of the Week (Training Data)",
       x = "Day of the Week", y = "Mean Sentiment Score") +
  scale_size_continuous(name = "Total Posts")

# Display the plots
print(plot_year)
print(plot_month)
print(plot_day)

#SECTION 9: TESTING

# Prepare testing data to match training format

# Extract date, month, and day of the week from Post Content
testing_data$post_date <- as.Date(gsub(paste0(".*\\b(", paste0(years, collapse = "|"), ")\\b.*"), "\\1", testing_data$`Post Content`), format = "%Y")
testing_data$month <- NA
for (i in 1:length(months)) {
  testing_data$month[grepl(months[i], testing_data$`Post Content`, ignore.case = TRUE)] <- months[i]
}
testing_data$day_of_week <- NA
for (i in 1:length(days_of_week)) {
  testing_data$day_of_week[grepl(days_of_week[i], testing_data$`Post Content`, ignore.case = TRUE)] <- days_of_week[i]
}

# Remove rows with missing or NA values in the year column
testing_data <- testing_data[complete.cases(testing_data$year), ]

# Aggregate Sentiment Score by Year, Month, and Day of the Week for testing data
testing_data_agg <- testing_data %>%
  group_by(year, month, day_of_week) %>%
  summarise(true_mean_sentiment_score = mean(mean_sentiment_score, na.rm = TRUE),
            total_posts = n())

# Remove rows with missing or NA values in testing_data_agg
testing_data_agg <- testing_data_agg[complete.cases(testing_data_agg), ]

# Predict sentiment scores for testing data using the trained model
testing_data_agg$predicted_mean_sentiment_score <- predict(model, newdata = testing_data_agg)

# Aggregate Sentiment Score by Year, Month, and Day of the Week for testing data
testing_data_agg <- testing_data_agg %>%
  ungroup() %>%
  group_by(year, month, day_of_week) %>%
  summarise(true_mean_sentiment_score = sum(true_mean_sentiment_score * total_posts) / sum(total_posts),
            predicted_mean_sentiment_score = sum(predicted_mean_sentiment_score * total_posts) / sum(total_posts),
            total_posts = sum(total_posts))

# Create a single data point per year
yearly_data <- testing_data_agg %>%
  group_by(year) %>%
  summarise(true_mean_sentiment_score = mean(true_mean_sentiment_score),
            predicted_mean_sentiment_score = mean(predicted_mean_sentiment_score))

# Create a single data point per month
monthly_data <- testing_data_agg %>%
  group_by(year, month) %>%
  summarise(true_mean_sentiment_score = mean(true_mean_sentiment_score),
            predicted_mean_sentiment_score = mean(predicted_mean_sentiment_score))

# Create a single data point per day of the week
daily_data <- testing_data_agg %>%
  group_by(year, day_of_week) %>%
  summarise(true_mean_sentiment_score = mean(true_mean_sentiment_score),
            predicted_mean_sentiment_score = mean(predicted_mean_sentiment_score))

# Calculate Mean Absolute Error (MAE) for each graph
mae_year <- mean(abs(yearly_data$true_mean_sentiment_score - yearly_data$predicted_mean_sentiment_score))
mae_month <- mean(abs(monthly_data$true_mean_sentiment_score - monthly_data$predicted_mean_sentiment_score))
mae_day <- mean(abs(daily_data$true_mean_sentiment_score - daily_data$predicted_mean_sentiment_score))

# Calculate accuracy percentage for each graph
accuracy_percentage_year <- 100 * (1 - mae_year / mean(yearly_data$true_mean_sentiment_score))
accuracy_percentage_month <- 100 * (1 - mae_month / mean(monthly_data$true_mean_sentiment_score))
accuracy_percentage_day <- 100 * (1 - mae_day / mean(daily_data$true_mean_sentiment_score))

library(ggplot2)

# Convert year, month, and day_of_week to factors
testing_data_agg$year <- as.factor(testing_data_agg$year)
testing_data_agg$month <- factor(testing_data_agg$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
testing_data_agg$day_of_week <- factor(testing_data_agg$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Convert year, month, and day_of_week to factors
testing_data_agg$year <- as.factor(testing_data_agg$year)
testing_data_agg$month <- factor(testing_data_agg$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
testing_data_agg$day_of_week <- factor(testing_data_agg$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

library(ggplot2)

# Visualization of predicted mean sentiment scores by year
ggplot(yearly_data, aes(x = year)) +
  geom_point(aes(y = true_mean_sentiment_score, color = "True")) +
  geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_point(aes(y = predicted_mean_sentiment_score, color = "Predicted")) +
  geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(title = "Predicted vs True Mean Sentiment Score by Year (Testing Data)",
       x = "Year", y = "Mean Sentiment Score") +
  scale_color_manual(values = c("True" = "red", "Predicted" = "blue")) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Aggregate sentiment scores by month
monthly_data_agg <- testing_data_agg %>%
  group_by(month) %>%
  summarise(true_mean_sentiment_score = mean(true_mean_sentiment_score),
            predicted_mean_sentiment_score = mean(predicted_mean_sentiment_score))

# Aggregate sentiment scores by day of the week
daily_data_agg <- testing_data_agg %>%
  group_by(day_of_week) %>%
  summarise(true_mean_sentiment_score = mean(true_mean_sentiment_score),
            predicted_mean_sentiment_score = mean(predicted_mean_sentiment_score))

# Plotting for month
ggplot(monthly_data_agg, aes(x = month)) +
  geom_point(aes(y = true_mean_sentiment_score, color = "True")) +
  geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_point(aes(y = predicted_mean_sentiment_score, color = "Predicted")) +
  geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(title = "Predicted vs True Mean Sentiment Score by Month (Testing Data)",
       x = "Month", y = "Mean Sentiment Score") +
  scale_color_manual(values = c("True" = "red", "Predicted" = "blue")) +
  theme_minimal()


library(ggplot2)
library(dplyr)

# Calculate Root Mean Squared Error (RMSE)
rmse_day <- sqrt(mean((daily_data_agg$true_mean_sentiment_score - daily_data_agg$predicted_mean_sentiment_score)^2))


##WINNERWINNER
# Plotting for day of the week
ggplot(daily_data_agg, aes(x = day_of_week)) +
  geom_point(aes(y = true_mean_sentiment_score, color = "True")) +
  geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_point(aes(y = predicted_mean_sentiment_score, color = "Predicted")) +
  geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(
    title = "Predicted vs True Mean Sentiment Score by Day of the Week (Testing Data)",
    subtitle = paste0("Root Mean Squared Error (RMSE): ", round(rmse_day, 2)),
    x = "Day of the Week", y = "Mean Sentiment Score"
  ) +
  scale_color_manual(values = c("True" = "red", "Predicted" = "blue")) +
  theme_minimal() +
  # Add separate polynomial lines of best fit
  geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue")


library(dplyr)
library(modelr)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(daily_data_agg$true_mean_sentiment_score - daily_data_agg$predicted_mean_sentiment_score))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((daily_data_agg$true_mean_sentiment_score - daily_data_agg$predicted_mean_sentiment_score)^2))

# Calculate R-squared (R^2) Score
r_squared <- cor(daily_data_agg$true_mean_sentiment_score, daily_data_agg$predicted_mean_sentiment_score)^2

# Print the metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R^2) Score:", r_squared, "\n")

library(plotly)
library(dplyr)

# Define a function to filter outliers based on standard deviation
filter_outliers <- function(data, variable, sd_threshold = 3) {
  mean_val <- mean(data[[variable]])
  sd_val <- sd(data[[variable]])
  filtered_data <- data %>%
    filter(abs(.data[[variable]] - mean_val) <= sd_threshold * sd_val)
  return(filtered_data)
}

# Filter out outliers from monthly_data
monthly_data_filtered <- filter_outliers(monthly_data, "true_mean_sentiment_score", sd_threshold = 3)
monthly_data_filtered <- filter_outliers(monthly_data_filtered, "predicted_mean_sentiment_score", sd_threshold = 3)

# Filter out outliers from daily_data
daily_data_filtered <- filter_outliers(daily_data, "true_mean_sentiment_score", sd_threshold = 3)
daily_data_filtered <- filter_outliers(daily_data_filtered, "predicted_mean_sentiment_score", sd_threshold = 3)

# Convert month and day_of_week to factors
monthly_data_filtered$month <- factor(monthly_data_filtered$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
daily_data_filtered$day_of_week <- factor(daily_data_filtered$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create 3D plot for monthly data with curves of best fit
plot_monthly <- plot_ly(monthly_data_filtered, x = ~year, y = ~month, z = ~true_mean_sentiment_score, type = "scatter3d", mode = "lines", line = list(color = "red"), name = "True") %>%
  add_trace(y = ~month, z = ~predicted_mean_sentiment_score, type = "scatter3d", mode = "lines", line = list(color = "blue"), name = "Predicted") %>%
  layout(scene = list(xaxis = list(title = "Year"), yaxis = list(title = "Month"), zaxis = list(title = "Mean Sentiment Score")), 
         title = "Predicted vs True Mean Sentiment Score by Month (Testing Data)")

# Create 3D plot for daily data with curves of best fit
plot_daily <- plot_ly(daily_data_filtered, x = ~year, y = ~day_of_week, z = ~true_mean_sentiment_score, type = "scatter3d", mode = "lines", line = list(color = "red"), name = "True") %>%
  add_trace(y = ~day_of_week, z = ~predicted_mean_sentiment_score, type = "scatter3d", mode = "lines", line = list(color = "blue"), name = "Predicted") %>%
  layout(scene = list(xaxis = list(title = "Year"), yaxis = list(title = "Day of the Week"), zaxis = list(title = "Mean Sentiment Score")), 
         title = "Predicted vs True Mean Sentiment Score by Day of the Week (Testing Data)")

# Show plots
plot_monthly
plot_daily

