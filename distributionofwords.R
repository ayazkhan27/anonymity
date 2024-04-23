library(ggplot2)
library(dplyr)
library(lubridate)

# Assuming 'test_data' already includes scaled ratios and date-related information
# Aggregate data to calculate means by time units
data_summary <- test_data %>%
  group_by(year, month) %>%
  summarise(
    mean_scaled_positive = mean(scaled_positive_ratio, na.rm = TRUE),
    mean_scaled_negative = mean(scaled_negative_ratio, na.rm = TRUE),
    mean_scaled_buffer = mean(scaled_buffer_ratio, na.rm = TRUE),
    mean_scaled_neutral = mean(scaled_neutral_ratio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year, month)

# Convert month to ordered factor to ensure correct ordering in the plot
data_summary$month <- factor(data_summary$month, levels = month.name)

# Plotting
ggplot(data_summary, aes(x = year)) +
  geom_line(aes(y = mean_scaled_positive, color = "Scaled Positive"), size = 1, linetype = "solid") +
  geom_line(aes(y = mean_scaled_negative, color = "Scaled Negative"), size = 1, linetype = "solid") +
  geom_line(aes(y = mean_scaled_buffer, color = "Scaled Buffer"), size = 1, linetype = "solid") +
  geom_line(aes(y = mean_scaled_neutral, color = "Scaled Neutral"), size = 1, linetype = "solid") +
  labs(title = "Distribution of Scaled Ratios Over Time",
       x = "Year",
       y = "Mean Scaled Ratio",
       color = "Ratio Type") +
  scale_x_continuous(breaks = unique(data_summary$year), labels = unique(data_summary$year)) +
  theme_minimal() +
  theme(legend.position = "right")
