# Visualization of ALL the data included so far

# SECTION 1: Sentiment Analysis
# Visualize aggregated sentiment scores
print(aggregate_sentiment)

# SECTION 2: Word Frequency Analysis
# Visualize aggregated keyword counts
print(aggregate_keywords)

# SECTION 3: Word Clouds
# Visualize word clouds for happy and sad posts
library(wordcloud)
happy_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_happy_posts$`Post Content`, "\\s+")),
                                        max.words = 100, colors = brewer.pal(8, "Dark2"),
                                        scale = c(2, 0.7), random.order = FALSE, rot.per = 0.35)

sad_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_sad_posts$`Post Content`, "\\s+")),
                                      max.words = 100, colors = brewer.pal(8, "Dark2"),
                                      scale = c(2, 0.7), random.order = FALSE, rot.per = 0.35)

# SECTION 5: Emotion Intensity Analysis

# Step 7: Visualize Words Contribution to Sentiment Using NRC Lexicon
library(ggplot2)

word_contribution <- post_words_emotion %>%
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

# Visualize proportion of words related to each emotion
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

# SECTION 6: Temporal Analysis
# Visualize temporal trends in mean sentiment score
print(plot_year)
print(plot_month)
print(plot_day)

# SECTION 9: Testing
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


# Visualize predicted vs true mean sentiment score by year, month, and day of the week
print(ggplot(yearly_data, aes(x = year)) +
        geom_point(aes(y = true_mean_sentiment_score, color = "True")) +
        geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
        geom_point(aes(y = predicted_mean_sentiment_score, color = "Predicted")) +
        geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
        labs(title = "Predicted vs True Mean Sentiment Score by Year (Testing Data)",
             x = "Year", y = "Mean Sentiment Score") +
        scale_color_manual(values = c("True" = "red", "Predicted" = "blue")) +
        theme_minimal())

print(ggplot(monthly_data_agg, aes(x = month)) +
        geom_point(aes(y = true_mean_sentiment_score, color = "True")) +
        geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
        geom_point(aes(y = predicted_mean_sentiment_score, color = "Predicted")) +
        geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
        labs(title = "Predicted vs True Mean Sentiment Score by Month (Testing Data)",
             x = "Month", y = "Mean Sentiment Score") +
        scale_color_manual(values = c("True" = "red", "Predicted" = "blue")) +
        theme_minimal())

print(ggplot(daily_data_agg, aes(x = day_of_week)) +
        geom_point(aes(y = true_mean_sentiment_score, color = "True")) +
        geom_smooth(aes(y = true_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
        geom_point(aes(y = predicted_mean_sentiment_score, color = "Predicted")) +
        geom_smooth(aes(y = predicted_mean_sentiment_score), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
        labs(title = "Predicted vs True Mean Sentiment Score by Day of the Week (Testing Data)",
             subtitle = paste0("Root Mean Squared Error (RMSE): ", round(rmse_day, 2)),
             x = "Day of the Week", y = "Mean Sentiment Score") +
        scale_color_manual(values = c("True" = "red", "Predicted" = "blue")) +
        theme_minimal())

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