library(syuzhet)

# Define the paragraph
paragraph <- "My day is ruined and my life is over."
# Calculate sentiment scores for each sentence
sentiment_scores <- get_sentiment(paragraph, method = "afinn")

# Calculate the mean sentiment score
mean_sentiment_score <- mean(sentiment_scores, na.rm = TRUE)

print(mean_sentiment_score)
