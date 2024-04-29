# SECTION: 1 - Sentimental Analysis

# Step 1: Load necessary packages
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)

# Step 2: Read data from all sheets of the Excel file
file_path <- "C:/Users/admin/AppData/Local/Programs/Python/Python312/posts_data.xlsx"

# Read data from each sheet and combine into one dataframe
posts_data <- lapply(excel_sheets(file_path), function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    mutate_all(as.character) # Convert all columns to character type
}) %>% bind_rows()

# List column names to confirm if they match what we're seeing
print(colnames(posts_data))

# Step 3: Preprocess the text data
posts_data <- posts_data %>%
  mutate(Post_Content_clean = gsub("[[:punct:]]", "", `Post Content`)) %>%
  mutate(Post_Content_clean = gsub("\\d+", "", Post_Content_clean)) %>%
  unnest_tokens(word, Post_Content_clean) %>%
  anti_join(stop_words)

# Step 4: Calculate sentiment scores
posts_data <- posts_data %>%
  mutate(sentiment_score = get_sentiment(word, method = "afinn"))

# Step 5: Classify posts as positive, negative, or neutral
posts_data <- posts_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Step 6: Aggregate sentiment scores and count posts
aggregate_sentiment <- posts_data %>%
  group_by(sentiment_label) %>%
  summarise(mean_sentiment_score = mean(sentiment_score),
            total_posts = n())

# Print the aggregated sentiment scores
print(aggregate_sentiment)
