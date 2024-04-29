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

# Step 6: Filter out repeated keywords
posts_data <- posts_data %>%
  group_by(Keyword, word) %>%
  mutate(word_count = n()) %>%
  filter(word_count < 3) %>%
  ungroup() %>%
  select(-word_count)

# Step 7: Filter out introductory website text
introductory_text <- c("Home", "About", "Search", "Write", "Categories", "Login", "Write what you feel...", "Please enter a keyword, term, or post number to search for:")
posts_data <- posts_data %>%
  filter(!word %in% introductory_text)

# Step 8: Remove neutral sentiments and sentiment scores of 0
posts_data <- posts_data %>%
  filter(sentiment_score != 0) %>%
  filter(sentiment_score != "Neutral")

# Step 9: Classify posts as positive or negative
posts_data <- posts_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative"
  ))

# Step 10: Aggregate sentiment scores and count posts
aggregate_sentiment <- posts_data %>%
  group_by(sentiment_label) %>%
  summarise(mean_sentiment_score = mean(sentiment_score),
            total_posts = n())

# Print the aggregated sentiment scores
print(aggregate_sentiment)

# SECTION 2: Word Frequency Analysis

# Step 1: Define Happy and Sad Keywords
happy_keywords <- c("joy", "happy", "excited")
sad_keywords <- c("sad", "depressed", "miserable")

# Step 2: Count Word Frequency for Happy and Sad Keywords
count_keywords <- function(text, keywords) {
  keyword_count <- sum(grepl(paste0(keywords, collapse = "|"), text, ignore.case = TRUE))
  return(keyword_count)
}

# Define Happy and Sad Keywords
happy_keywords <- c("joy", "happy", "excited")
sad_keywords <- c("sad", "depressed", "miserable")

# Count word occurrences for happy and sad keywords
posts_data$happy_keyword_count <- sapply(posts_data$`Post Content`, count_keywords, keywords = happy_keywords)
posts_data$sad_keyword_count <- sapply(posts_data$`Post Content`, count_keywords, keywords = sad_keywords)

# Step 3: Filter out posts with no happy or sad keywords
posts_data <- posts_data %>%
  filter(happy_keyword_count > 0 | sad_keyword_count > 0)

# Step 4: Aggregate keyword counts and count posts
aggregate_keywords <- posts_data %>%
  summarise(total_happy_posts = sum(happy_keyword_count),
            total_sad_posts = sum(sad_keyword_count))

# Print the aggregated keyword counts
print(aggregate_keywords)
