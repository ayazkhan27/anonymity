
# Load necessary libraries
library(ggplot2)

# Assuming you have the data available with these columns: scaled_positive_ratio, scaled_negative_ratio, scaled_buffer_ratio, scaled_neutral_ratio

# Create a data frame with these variables
data_bar <- data.frame(
  Variable = c("Positive Ratio", "Negative Ratio", "Buffer Ratio", "Neutral Ratio"),
  Value = c(mean(train_data$scaled_positive_ratio),
            mean(train_data$scaled_negative_ratio),
            mean(train_data$scaled_buffer_ratio),
            mean(train_data$scaled_neutral_ratio))
)

# Plotting bar chart
ggplot(data_bar, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Scaled Ratio Comparison",
       x = "Ratio Type",
       y = "Scaled Value") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend as it's not necessary for this graph

library(ggplot2)
library(dplyr)

# Assuming you have the posts data with sentiment scores
# Assuming the data frame is named 'posts_data' with columns 'Post Content' and 'sentiment_score'
library(ggplot2)
library(dplyr)

# Assuming you have the posts data with sentiment scores
# Assuming the data frame is named 'posts_data' with columns 'Post Content' and 'sentiment_score'

# Calculate the length of each post
posts_data <- posts_data %>%
  mutate(post_length = nchar(`Post Content`))

# Filter outlier values for post length
posts_data <- posts_data %>%
  filter(post_length <= 2000)  # Filter posts with length up to 2,000 characters

# Aggregate data by post length and calculate mean sentiment score
sentiment_by_length <- posts_data %>%
  group_by(post_length) %>%
  summarise(mean_sentiment_score = mean(sentiment_score, na.rm = TRUE))

# Visualize mean sentiment score against post length
ggplot(sentiment_by_length, aes(x = mean_sentiment_score, y = post_length)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(title = "Mean Post Length by Sentiment Score (up to 2,000 characters)",
       x = "Mean Sentiment Score",
       y = "Post Length")

library(ggplot2)
library(dplyr)

# Assuming the data frame is named 'sentiment_by_length' with columns 'post_length' and 'mean_sentiment_score'
# Calculate mean and standard deviation of post lengths
mean_length <- mean(sentiment_by_length$post_length)
sd_length <- sd(sentiment_by_length$post_length)

# Generate x values for the bell curve based on the range of mean sentiment scores
x <- seq(min(sentiment_by_length$mean_sentiment_score), max(sentiment_by_length$mean_sentiment_score), length.out = 1000)

# Calculate the midpoint of x
midpoint_x <- mean(x)

# Adjustments for the overlay
shift_x <- 0.5  # Shift to the right
shift_y <- -50   # Shift down
spikiness <- .65  # Adjust spikiness

# Calculate y values for the bell curve (normal distribution) representing post length
y <- dnorm(x, mean = midpoint_x + shift_x, sd = sd(x) * spikiness) * max(sentiment_by_length$post_length) + shift_y

# Plot the scatter plot of mean sentiment score against post length
plot <- ggplot(sentiment_by_length, aes(x = mean_sentiment_score, y = post_length)) +
  geom_point() +
  labs(title = "Mean Post Length by Sentiment Score (up to 2,000 characters)",
       x = "Mean Sentiment Score",
       y = "Post Length")

# Overlay the bell curve on the plot
plot + geom_line(data = data.frame(x = x, y = y), aes(x = x, y = y), color = "red")

# Calculate the number of data points falling within certain standard deviations from the mean
within_1_sd <- sum(abs(sentiment_by_length$mean_sentiment_score - mean(sentiment_by_length$mean_sentiment_score)) < sd(sentiment_by_length$mean_sentiment_score))
within_2_sd <- sum(abs(sentiment_by_length$mean_sentiment_score - mean(sentiment_by_length$mean_sentiment_score)) < 2 * sd(sentiment_by_length$mean_sentiment_score))
within_3_sd <- sum(abs(sentiment_by_length$mean_sentiment_score - mean(sentiment_by_length$mean_sentiment_score)) < 3 * sd(sentiment_by_length$mean_sentiment_score))

# Calculate the total number of data points
total_data_points <- nrow(sentiment_by_length)

# Calculate the expected proportions from a normal distribution
expected_within_1_sd <- pnorm(1) - pnorm(-1)
expected_within_2_sd <- pnorm(2) - pnorm(-2)
expected_within_3_sd <- pnorm(3) - pnorm(-3)

# Calculate the actual proportions
actual_proportion_1_sd <- within_1_sd / total_data_points
actual_proportion_2_sd <- within_2_sd / total_data_points
actual_proportion_3_sd <- within_3_sd / total_data_points

# Print the results
cat("Proportion of data points within 1 standard deviation from the mean:", actual_proportion_1_sd, "(Expected:", expected_within_1_sd, ")\n")
cat("Proportion of data points within 2 standard deviations from the mean:", actual_proportion_2_sd, "(Expected:", expected_within_2_sd, ")\n")
cat("Proportion of data points within 3 standard deviations from the mean:", actual_proportion_3_sd, "(Expected:", expected_within_3_sd, ")\n")

