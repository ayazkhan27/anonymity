library(ggplot2)

# Scatter plot for total_words vs mean_sentiment
ggplot(test_data, aes(x = total_words, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Total Words", y = "Mean Sentiment")

# Scatter plot for total_emotion_words vs mean_sentiment
ggplot(test_data, aes(x = total_emotion_words, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Total Emotion Words", y = "Mean Sentiment")

# Scatter plot for neutral_ratio vs mean_sentiment
ggplot(test_data, aes(x = neutral_ratio, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Neutral Ratio", y = "Mean Sentiment")

# Scatter plot for buffer_ratio vs mean_sentiment
ggplot(test_data, aes(x = buffer_ratio, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Buffer Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_neutral_ratio vs mean_sentiment
ggplot(test_data, aes(x = scaled_neutral_ratio, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Neutral Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_buffer_ratio vs mean_sentiment
ggplot(test_data, aes(x = scaled_buffer_ratio, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Buffer Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_positive_ratio vs mean_sentiment
ggplot(test_data, aes(x = scaled_positive_ratio, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Positive Ratio", y = "Mean Sentiment")

# Scatter plot for scaled_negative_ratio vs mean_sentiment
ggplot(test_data, aes(x = scaled_negative_ratio, y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Scaled Negative Ratio", y = "Mean Sentiment")



# Scatter plot for log(scaled_positive_ratio + 1) vs mean_sentiment
ggplot(test_data, aes(x = log(scaled_positive_ratio + 1), y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Log(Scaled Positive Ratio + 1)", y = "Mean Sentiment")

# Scatter plot for log(scaled_negative_ratio + 1) vs mean_sentiment
ggplot(test_data, aes(x = log(scaled_negative_ratio + 1), y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Log(Scaled Negative Ratio + 1)", y = "Mean Sentiment")

# Scatter plot for exp(scaled_buffer_ratio) vs mean_sentiment
ggplot(test_data, aes(x = exp(scaled_buffer_ratio), y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Exp(Scaled Buffer Ratio)", y = "Mean Sentiment")

# Scatter plot for exp(scaled_neutral_ratio) vs mean_sentiment
ggplot(test_data, aes(x = exp(scaled_neutral_ratio), y = mean_sentiment)) + 
  geom_point() +
  labs(x = "Exp(Scaled Neutral Ratio)", y = "Mean Sentiment")
