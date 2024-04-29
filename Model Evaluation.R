library(plotly)
library(dplyr)

# Assuming test_data includes 'year', 'month', 'day_of_week', 'mean_sentiment', 'predicted_sentiment'

# Prepare monthly data
monthly_data <- test_data %>%
  group_by(year, month) %>%
  summarise(actual_mean_sentiment_score = mean(mean_sentiment, na.rm = TRUE),
            predicted_mean_sentiment_score = mean(predicted_sentiment, na.rm = TRUE)) %>%
  ungroup()

# Prepare daily data
daily_data <- test_data %>%
  group_by(year, day_of_week) %>%
  summarise(actual_mean_sentiment_score = mean(mean_sentiment, na.rm = TRUE),
            predicted_mean_sentiment_score = mean(predicted_sentiment, na.rm = TRUE)) %>%
  ungroup()

# Ensure 'month' and 'day_of_week' are treated as numeric for plotting
monthly_data$month <- as.numeric(factor(monthly_data$month, levels = month.name))
daily_data$day_of_week_num <- as.numeric(factor(daily_data$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# 3D Plot for Monthly Data
plot_monthly <- plot_ly(monthly_data, x = ~year, y = ~month, z = ~actual_mean_sentiment_score, type = 'scatter3d', mode = 'lines',
                        line = list(color = 'red', width = 2), name = 'Actual') %>%
  add_trace(z = ~predicted_mean_sentiment_score, type = 'scatter3d', mode = 'lines',
            line = list(color = 'blue', width = 2), name = 'Predicted') %>%
  layout(title = "Predicted vs Actual Mean Sentiment Score by Month",
         scene = list(xaxis = list(title = 'Year'),
                      yaxis = list(title = 'Month', ticktext = month.name, tickvals = 1:12),
                      zaxis = list(title = 'Mean Sentiment Score')))

# 3D Plot for Daily Data
plot_daily <- plot_ly(daily_data, x = ~year, y = ~day_of_week_num, z = ~actual_mean_sentiment_score, type = 'scatter3d', mode = 'lines',
                      line = list(color = 'red', width = 2), name = 'Actual') %>%
  add_trace(z = ~predicted_mean_sentiment_score, type = 'scatter3d', mode = 'lines',
            line = list(color = 'blue', width = 2), name = 'Predicted') %>%
  layout(title = "Predicted vs Actual Mean Sentiment Score by Day of the Week",
         scene = list(xaxis = list(title = 'Year'),
                      yaxis = list(title = 'Day of the Week', ticktext = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), tickvals = 1:7),
                      zaxis = list(title = 'Mean Sentiment Score')))

# Show plots
plot_monthly
plot_daily
