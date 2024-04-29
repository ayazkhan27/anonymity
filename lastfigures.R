# Plotting mean sentiment scores by time periods
library(showtext)

# Initialize showtext to automatically use added fonts
showtext_auto(enable = TRUE)

# Load Arial font from your system, ensure it's available
font_add(family = "Arial", regular = "arial.ttf")  # Check the actual path if not found

library(ggplot2)

# Plotting mean sentiment scores by time periods
ggplot(test_data_summary, aes(x = month, y = mean_actual_sentiment, group = year)) +
  geom_line(aes(color = as.factor(year)), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, linetype = "Predicted"), size = 1) +
  facet_wrap(~year) +
  labs(title = "Mean Sentiment Score by Month and Year",
       x = "Month",
       y = "Mean Sentiment Score",
       color = "Year",
       linetype = "Legend") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Arial"),  # Apply Arial with a larger base size
        axis.title = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, face = "bold", family = "Arial", hjust = 0.5),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial"))

library(showtext)

# Enable automatic use of showtext for fonts
showtext_auto(enable = TRUE)

# Load Arial font from your system
font_add(family = "Arial", regular = "arial.ttf")  # Check the actual path if not found

library(ggplot2)

# Assuming model_cv is a model object that can be plotted directly
plot(model_cv, main = "Model Cross-Validation Results",
     xlab = "Number of Randomly Selected Predictors", ylab = "RMSE",
     family = "Arial", cex.main = 5, cex.lab = 5.5, cex.axis = 5.2)

library(showtext)

# Initialize showtext to automatically use added fonts
showtext_auto(enable = TRUE)

# Load Arial font from your system
font_add(family = "Arial", regular = "arial.ttf")  # Ensure Arial is available

library(ggplot2)

# Plotting mean sentiment scores by hourly timestamp with time labels in AM/PM format
ggplot(test_data_hourly, aes(x = hour, y = mean_actual_sentiment)) +
  geom_line(aes(color = "Actual"), size = 1) +
  geom_line(aes(y = mean_predicted_sentiment, color = "Predicted"), linetype = "dashed", size = 1) +
  labs(title = "Mean Sentiment Score by Hour of Day",
       x = "Hour of Day",
       y = "Mean Sentiment Score",
       color = "Legend",
       linetype = "Legend") +
  scale_x_continuous(breaks = seq(0, 23, by = 1),
                     labels = function(x) {
                       ifelse(x < 12, paste0(x, " AM"), ifelse(x == 12, "12 PM", paste0(x - 12, " PM")))
                     }) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Arial"),  # Apply Arial with a larger base size
        axis.title = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, face = "bold", family = "Arial", hjust = 0.5),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 20, family = "Arial"))
##########################################################################

library(ggplot2)

# Plot the scatter plot of mean sentiment score against post length
plot <- ggplot(sentiment_by_length, aes(x = mean_sentiment_score, y = post_length)) +
  geom_point() +
  labs(title = "Mean Post Length by Sentiment Score",
       x = "Mean Sentiment Score",
       y = "Post Length") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Arial"),  # Apply Arial with a larger base size
        axis.title = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, face = "bold", family = "Arial", hjust = 0.5),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 24, family = "Arial"))

# Overlay the bell curve on the plot
plot + geom_line(data = data.frame(x = x, y = y), aes(x = x, y = y), color = "red")

##

library(ggplot2)

# Density Plot of Residuals
ggplot(data = NULL, aes(x = residuals)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of Residuals", x = "Residuals", y = "Density") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Arial"),  # Apply Arial with a larger base size
        axis.title = element_text(size = 20, family = "Arial"),
        plot.title = element_text(size = 20, face = "bold", family = "Arial", hjust = 0.5),
        legend.title = element_text(size = 20, family = "Arial"),
        legend.text = element_text(size = 24, family = "Arial"))


