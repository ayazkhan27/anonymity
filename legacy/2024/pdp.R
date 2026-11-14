# Load necessary libraries
library(randomForest)
library(pdp)
library(ggplot2)

# Assuming you have already trained a Random Forest model (sentiment_model)
# and you have a dataset with relevant predictor variables (train_data)

# Define the features for which you want to create partial dependence plots
features_of_interest <- c("scaled_buffer_ratio", "scaled_neutral_ratio", 
                          "scaled_positive_ratio", "scaled_negative_ratio")

# Generate partial dependence plots for each feature
for (feature in features_of_interest) {
  pdp_data <- partial(sentiment_model, pred.var = feature, train_data, 
                      grid.resolution = 100)  # Adjust grid.resolution as needed
  
  # Plot partial dependence
  ggplot(pdp_data, aes_string(x = feature, y = "yhat")) +
    geom_line() +
    labs(title = paste("Partial Dependence Plot for", feature),
         x = feature,
         y = "Mean Sentiment Score")
}
