predict_mean_sentiment <- function(sentences, loaded_model) {
  # Preprocess sentences
  sentences_clean <- gsub("[[:punct:]]", "", sentences) %>%
    gsub("\\d+", "", .) %>%
    tolower()  # Convert to lowercase for consistency
  
  # Create a data frame with the sentences
  sentences_df <- data.frame(sentences_clean = sentences_clean)
  
  # Tokenize sentences
  sentences_tokenized <- sentences_df %>%
    unnest_tokens(word, sentences_clean)
  
  # Calculate sentiment scores for each word
  sentences_sentiment <- sentences_tokenized %>%
    mutate(sentiment_score = get_sentiment(word, method = "afinn"))
  
  # Aggregate sentiment scores to sentence level
  sentences_aggregated <- sentences_sentiment %>%
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
  
  # Scale the aggregated features using max values from training data
  max_buffer_ratio <- 0.98181818181818  # Update with actual max values
  max_neutral_ratio <- 1  # Update with actual max values
  max_positive_impact <- 3  # Update with actual max values
  max_negative_impact <- 3  # Update with actual max values
  
  sentences_scaled <- sentences_aggregated %>%
    mutate(
      scaled_buffer_ratio = buffer_ratio / max_buffer_ratio,
      scaled_neutral_ratio = neutral_ratio / max_neutral_ratio,
      scaled_positive_ratio = (1.5 * slight_positive_ratio + 2 * medium_positive_ratio + 2.5 * moderate_positive_ratio + 3 * extreme_positive_ratio) / max_positive_impact,
      scaled_negative_ratio = (1.5 * slight_negative_ratio + 2 * medium_negative_ratio + 2.5 * moderate_negative_ratio + 3 * extreme_negative_ratio) / max_negative_impact
    )
  
  # Predict mean sentiment using the loaded model
  predicted_sentiments <- predict(loaded_model, newdata = sentences_scaled)
  
  # Create a dataframe with sentences and predicted mean sentiment scores
  result <- data.frame(sentence = sentences, predicted_mean_sentiment = predicted_sentiments)
  
  return(result)
}

# Example usage:
sentences <- c(
  'This is a terrible and ridiculous idea.',
  'They told me the book is great and very entertaining, but I found the book disappointing...',
  'This is not really a good movie...',
  'First there were riots in the streets, then the killings, and not long after that the genocide started',
  "The food in Antonioni's Pizza is not bad at all."
)

# Load the saved model from the file
loaded_model <- readRDS("sentiment_model.rds")

# Predict sentiment using the loaded model
predicted_sentiments <- predict_mean_sentiment(sentences, loaded_model)
print(predicted_sentiments)