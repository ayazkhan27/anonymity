library(ggplot2)

# Scatter plot for total_words vs adjusted_mean_sentiment
ggplot(test_data, aes(x = total_words, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Total Words", y = "Mean Sentiment")

# Scatter plot for total_emotion_words vs adjusted_mean_sentiment
ggplot(test_data, aes(x = total_emotion_words, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Total Emotion Words", y = "Mean Sentiment")

# Scatter plot for neutral_ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = neutral_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Neutral Ratio", y = "Mean Sentiment")

# Scatter plot for buffer_ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = buffer_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Buffer Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_neutral_ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = scaled_neutral_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Neutral Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_buffer_ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = scaled_buffer_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Buffer Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_positive_ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = scaled_positive_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Positive Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_negative_ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = scaled_negative_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Negative Ratio", y = "Mean Sentiment")



# Scatter plot for log emotion ratio vs adjusted_mean_sentiment
ggplot(test_data, aes(x = log_emotion_ratio, y = adjusted_mean_sentiment)) + 
  geom_point() +
  labs(x = "Log Emotion Ratio", y = "Mean Sentiment")

