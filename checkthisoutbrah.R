predict_mean_sentiment <- function(paragraph, loaded_model) {
  # Clean the paragraph
  paragraph_clean <- gsub("[[:punct:]]", "", paragraph) %>%
    gsub("\\d+", "", .) %>%
    tolower()
  
  # Tokenize the paragraph
  paragraph_df <- data.frame(paragraph_clean = paragraph_clean)
  paragraph_tokenized <- paragraph_df %>%
    unnest_tokens(word, paragraph_clean)
  
  # Check if the tokenization results in an empty data frame
  if(nrow(paragraph_tokenized) == 0) {
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
      slight_negative_count = sum(sentiment_score == -1, na.rm = TRUE),
      neutral_ratio = neutral_count / if_else(total_words > 0, total_words, 1)
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
      buffer_ratio = if_else(buffer_count == 0, neutral_ratio, buffer_count / if_else(total_words > 0, total_words, 1)),
      neutral_ratio = if_else(neutral_count == 0, buffer_ratio, neutral_count / if_else(total_words > 0, total_words, 1))
    )
  
  # Print the calculated ratios
  print(paragraph_aggregated)
  
  # Scale features based on their calculated maximums
  max_buffer_ratio <- max(paragraph_aggregated$buffer_ratio, na.rm = TRUE)
  max_neutral_ratio <- max(paragraph_aggregated$neutral_ratio, na.rm = TRUE)
  # Calculate the maximum positive impact
  max_positive_impact <- max(1.5 * paragraph_aggregated$slight_positive_ratio +
                               2 * paragraph_aggregated$medium_positive_ratio +
                               2.5 * paragraph_aggregated$moderate_positive_ratio +
                               3 * paragraph_aggregated$extreme_positive_ratio -
                               (1.5 * paragraph_aggregated$slight_negative_ratio +
                                  2 * paragraph_aggregated$medium_negative_ratio +
                                  2.5 * paragraph_aggregated$moderate_negative_ratio +
                                  3 * paragraph_aggregated$extreme_negative_ratio), na.rm = TRUE)
  
  # Calculate the maximum negative impact
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
  max_values <- c(max_buffer_ratio, max_neutral_ratio, max_positive_impact, max_negative_impact) # Define max_values
  print(max_values) # Print max_values
  if(any(max_values == 0 | is.na(max_values))) {
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
  
  
  # Print the scaled ratios
  print(paragraph_aggregated)
  
  # Predict mean sentiment if not already set to 0
  if (max_positive_impact != 0 || max_negative_impact != 0) {
    predicted_sentiment <- predict(loaded_model, newdata = paragraph_aggregated)
  } else {
    predicted_sentiment <- 0
  }
  
  # Return a data frame with the original paragraph and predicted sentiment
  result <- data.frame(paragraph = paragraph, predicted_mean_sentiment = predicted_sentiment)
  return(result)
}

# Example usage
paragraph <- "Yesterday started off great. The sun was shining, birds chirping, and I felt on top of the world. However, as the day progressed, things took a turn for the worse. I got stuck in traffic on my way to work, which made me late for an important meeting. Then, to top it off, I spilled coffee all over my favorite shirt. Despite these setbacks, I managed to salvage the day by treating myself to a delicious dinner with friends in the evening. Overall, it was a rollercoaster of emotions, but I'm ending the day with a positive outlook."
loaded_model <- readRDS("sentiment_model.rds")
predicted_sentiment <- predict_mean_sentiment(paragraph, loaded_model)
print(predicted_sentiment)
