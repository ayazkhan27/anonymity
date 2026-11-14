
Sys.setenv(RETICULATE_PYTHON = "C:/Users/admin/anaconda3/envs/your_conda_environment/python.exe")
library(reticulate)

py_discover_config()  # This can help to see what Python environments are discoverable

# Specify the Python executable directly
use_python("C:/Users/admin/anaconda3/envs/your_conda_environment/python.exe", required = TRUE)



prepare_data <- function(data) {
  # Extract relevant columns and remove rows with any missing values
  data <- data[, c("Post Content", "mean_sentiment")]
  na.omit(data)  # Remove rows with NA values
}

# Example of loading and preparing data
# train_data <- read.csv("path_to_train_data.csv")
# test_data <- read.csv("path_to_test_data.csv")
# train_data <- prepare_data(train_data)
# test_data <- prepare_data(test_data)
library(reticulate)
use_condaenv("your_conda_environment", required = TRUE)

# Assuming the Python environment has transformers installed
transformers <- import('transformers')
torch <- import('torch')

bert_tokenizer <- function() {
  transformers$BertTokenizer$from_pretrained('bert-base-uncased')
}

encode_text <- function(tokenizer, texts) {
  tokenizer$batch_encode_plus(texts, return_tensors = 'pt', padding = TRUE, truncation = TRUE)
}

as.tensor <- function(x) {
  torch$tensor(x)
}

fine_tune_bert <- function(train_data, d_model = 768, num_heads = 12) {
  tokenizer <- bert_tokenizer()
  train_encodings <- encode_text(tokenizer, train_data$`Post Content`)
  
  config <- list(d_model = d_model, num_heads = num_heads)
  bert_model <- transformer(x = array(0, dim = c(1, 10, config$d_model)), 
                            d_model = config$d_model,
                            num_heads = config$num_heads)
  
  train_labels <- as.tensor(train_data$mean_sentiment)
  train_dataset <- list(input_ids = train_encodings$input_ids,
                        attention_mask = train_encodings$attention_mask,
                        labels = train_labels)
  
  # Define training parameters
  train_batch_size <- 8
  num_epochs <- 3
  
  # Assuming the transformer function has a training method
  for (epoch in 1:num_epochs) {
    for (i in seq_len(length(train_dataset$input_ids))) {
      input_batch <- list(input_ids = train_dataset$input_ids[[i]],
                          attention_mask = train_dataset$attention_mask[[i]],
                          labels = train_dataset$labels[[i]])
      # This is a placeholder for the actual training step
      loss <- bert_model$train_step(input_batch)
    }
  }
  
  return(bert_model)
}

evaluate_bert <- function(bert_model, test_data) {
  tokenizer <- bert_tokenizer()
  test_encodings <- encode_text(tokenizer, test_data$`Post Content`)
  
  test_dataset <- list(input_ids = test_encodings$input_ids,
                       attention_mask = test_encodings$attention_mask)
  
  predictions <- list()
  for (i in seq_len(length(test_dataset$input_ids))) {
    input_batch <- list(input_ids = test_dataset$input_ids[[i]],
                        attention_mask = test_dataset$attention_mask[[i]])
    # Placeholder for model prediction
    output <- bert_model$predict(input_batch)
    predictions <- c(predictions, output)
  }
  
  predicted_sentiment <- unlist(predictions)
  actual_sentiment <- test_data$mean_sentiment
  r_squared <- cor(predicted_sentiment, actual_sentiment)^2
  
  return(r_squared)
}


print(paste("BERT R-squared:", r_squared))
