predict_mean_sentiment <- function(paragraph, loaded_model) {
  # Preprocess paragraph
  paragraph_clean <- gsub("[[:punct:]]", "", paragraph) %>%
    gsub("\\d+", "", .) %>%
    tolower()  # Convert to lowercase for consistency
  
  # Tokenize paragraph
  paragraph_df <- data.frame(paragraph_clean = paragraph_clean)
  paragraph_tokenized <- paragraph_df %>%
    unnest_tokens(word, paragraph_clean)
  
  # Calculate sentiment scores for each word
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
  
  # Scale the aggregated features
  max_buffer_ratio <- 0.9818181818181818181818181818181818181818181818181818181818181818181818181818181818  # Update with actual max values
  max_neutral_ratio <- 1  # Update with actual max values
  max_positive_impact <- 3  # Update with actual max values
  max_negative_impact <- 3  # Update with actual max values
  
  paragraph_scaled <- paragraph_aggregated %>%
    mutate(
      scaled_buffer_ratio = buffer_ratio / max_buffer_ratio,
      scaled_neutral_ratio = neutral_ratio / max_neutral_ratio,
      scaled_positive_ratio = (1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio) / max_positive_impact,
      scaled_negative_ratio = (1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio) / max_negative_impact
    )
  
  # Predict mean sentiment
  predicted_sentiment <- predict(loaded_model, newdata = paragraph_scaled)
  
  # Create a data frame with the paragraph and predicted mean sentiment score
  result <- data.frame(paragraph = paragraph, predicted_mean_sentiment = predicted_sentiment)
  
  return(result)
}

# Example usage:
paragraph <- "My day is ruined and my life is over."
# Load the saved model
loaded_model <- readRDS("sentiment_model.rds")

# Predict sentiment using the loaded model
predicted_sentiment <- predict_mean_sentiment(paragraph, loaded_model)
print(predicted_sentiment)
