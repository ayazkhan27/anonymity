library(reticulate)

# Set the Python environment to use (ensure this is done before loading any Python libraries)
Sys.setenv(RETICULATE_PYTHON = "C:/Users/admin/anaconda3/envs/your_conda_environment/python.exe")

# Use the specific Python executable if necessary
use_python("C:/Users/admin/anaconda3/envs/your_conda_environment/python.exe", required = TRUE)

# Import the required Python modules using reticulate
transformers <- import('transformers')

# Load the tokenizer and model
tokenizer <- transformers$AutoTokenizer$from_pretrained("sbcBI/sentiment_analysis")
model <- transformers$AutoModelForSequenceClassification$from_pretrained("sbcBI/sentiment_analysis")

# Function to classify sentiment of a text
classify_sentiment <- function(text) {
  encoding <- tokenizer$encode_plus(
    text,
    add_special_tokens = TRUE,
    return_tensors = "pt"
  )
  
  # Perform prediction
  output <- model(encoding$input_ids, attention_mask = encoding$attention_mask)
  
  # Extract logits and compute softmax to get probabilities
  logits <- output$logits
  softmax <- torch$nn$functional$softmax(logits, dim=1)
  
  # Convert logits to probabilities and return
  probabilities <- softmax$detach()$numpy()
  probabilities <- py_to_r(probabilities)
  
  return(probabilities)
}

# Test the function with a sample text
text <- "I really enjoyed this movie!"
sentiment_probabilities <- classify_sentiment(text)
print(sentiment_probabilities)
