library(reticulate)

# Use the correct path to your Python environment
use_python("C:\\Users\\admin\\AppData\\Local\\Programs\\Python\\Python312")


# Load the BERT model from Hugging Face
transformers <- import("transformers")

# Load the BERT tokenizer
tokenizer <- transformers$BertTokenizer.from_pretrained("bert-base-uncased")

# Load the BERT model
model <- transformers$BertModel.from_pretrained("bert-base-uncased")

# Define a function to clean up text using BERT
clean_text_with_bert <- function(text) {
  tokenized_text <- tokenizer$encode_plus(text, max_length = 512, pad_to_max_length = TRUE)
  input_ids <- tokenized_text$input_ids
  attention_mask <- tokenized_text$attention_mask
  
  # Convert R matrices to Python numpy arrays
  input_ids <- py$torch$LongTensor(input_ids)
  attention_mask <- py$torch$LongTensor(attention_mask)
  
  # Forward pass through the BERT model
  with_no_grad({
    outputs <- model(input_ids, attention_mask)
  })
  
  # Get the final hidden states
  hidden_states <- outputs[["last_hidden_state"]]
  
  # Convert Python tensor to R matrix
  hidden_states <- as.array(hidden_states)
  
  # Perform text cleaning using BERT embeddings (you may need to adjust this part)
  cleaned_text <- text
  
  return(cleaned_text)
}

# Load the data from the xlsx file
file_path <- "C:/Users/admin/Documents/posts_data.xlsx"
data <- read.xlsx(file_path)

# Clean up the "Post Content" column using BERT
data$Post_Content <- sapply(data$Post_Content, clean_text_with_bert)

# Save the cleaned data back to the xlsx file
write.xlsx(data, file_path, overwrite = TRUE)
