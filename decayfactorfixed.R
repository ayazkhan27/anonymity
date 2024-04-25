library(tidytext)
library(randomForest)

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
  
  # Set placeholder value for buffer and neutral ratios
  buffer_placeholder <- 0.001
  neutral_placeholder <- 0.001
  
  # Check if max buffer ratio is 0 and if so, set it to max neutral ratio or placeholder value
  if (max_buffer_ratio == 0) {
    if (max_neutral_ratio == 0) {
      max_buffer_ratio <- buffer_placeholder
    } else {
      max_buffer_ratio <- max_neutral_ratio
    }
  }
  
  # Check if max neutral ratio is 0 and if so, set it to max buffer ratio or placeholder value
  if (max_neutral_ratio == 0) {
    if (max_buffer_ratio == 0) {
      max_neutral_ratio <- neutral_placeholder
    } else {
      max_neutral_ratio <- max_buffer_ratio
    }
  }
  
  
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
paragraph <- "hell hell hell hell hell hell hell hell hi"
loaded_model_2 <- readRDS("sentiment_model_2.rds") # Replace with the actual path to your saved model
predicted_sentiment <- predict_mean_sentiment(paragraph, loaded_model_2)
print(predicted_sentiment)


#########BOOKTEST############################


