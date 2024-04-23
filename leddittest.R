# Install required packages if not already installed
install.packages(c("readr", "dplyr"))

# Load required packages
library(readr)
library(dplyr)

# Read the Reddit data from the CSV file
reddit_data <- read_csv("C:\\Users\\admin\\Documents\\one-million-reddit-confessions.csv")

# View the first few rows of the data
head(reddit_data)
# Load required packages
library(dplyr)
library(stringr)

# Combine "title" and "selftext" columns
reddit_data <- reddit_data %>%
  mutate(combined_text = paste(title, selftext, sep = " ")) %>%
  select(combined_text)

# Remove rows with "[removed]" or "[deleted]" in "selftext"
reddit_data <- reddit_data %>%
  filter(!str_detect(combined_text, "\\[removed\\]|\\[deleted\\]"))

# Preprocess the combined text data
reddit_data <- reddit_data %>%
  mutate(text_clean = gsub("[[:punct:]]", "", combined_text)) %>%
  mutate(text_clean = gsub("\\d+", "", text_clean)) %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words)

# Calculate sentiment scores for each word
reddit_data <- reddit_data %>%
  mutate(sentiment_score = get_sentiment(word, method = "afinn"))

# Classify sentiments
reddit_data <- reddit_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Aggregate sentiment scores at the document level
reddit_data_aggregated <- reddit_data %>%
  group_by(row_number()) %>% # Use row_number() to group by each row (document)
  summarise(mean_sentiment = mean(sentiment_score, na.rm = TRUE))

# Make sure the features are calculated in the same way as your original data
reddit_data_processed <- process_data(reddit_data_aggregated)

# Predict mean sentiment using your trained model
reddit_data_processed$predicted_sentiment <- predict(sentiment_model, newdata = reddit_data_processed)

# Evaluate model performance
results <- postResample(pred = reddit_data_processed$predicted_sentiment,
                        obs = reddit_data_processed$mean_sentiment)
print(results)