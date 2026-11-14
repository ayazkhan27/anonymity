import torch
from transformers import BertTokenizer, BertModel
import pandas as pd
import re
import random

# Load the BERT tokenizer
tokenizer = BertTokenizer.from_pretrained("bert-base-uncased")

# Load the BERT model
model = BertModel.from_pretrained("bert-base-uncased")

# Define a function to clean up text using BERT
def clean_text_with_bert(text):
    # Tokenize the text
    tokenized_text = tokenizer.encode_plus(text, max_length=512, pad_to_max_length=True, return_tensors="pt")

    # Forward pass through the BERT model
    with torch.no_grad():
        outputs = model(**tokenized_text)

    # Get the final hidden states
    hidden_states = outputs.last_hidden_state

    # Convert PyTorch tensor to NumPy array
    hidden_states = hidden_states.numpy()

    # Define patterns to identify non-human-generated content
    patterns = [
        r'\|\s*on\s+\w+\s+\d+,\s+\d+\s*-\s*\d+:\d+\s+\w+\s*-\s*by\s*[^|]+',  # Timestamp and author
        r'Page \d+ of \d+',  # Page number
        r'Tweet',  # Tweet button
        r'Donations Help.*?Bitcoin',  # Donation section
        r'Recent Comments.*?© \d{4}-\d{4} Somewhere To Write',  # Footer section
        r'Tags.*?work',  # Tags section
        r'You searched for.*?Please enter a keyword, term, or post number to search for:',  # Search bar and message
        r'Tuesday \d+(st|nd|rd|th) \w+ \d{4}',  # Date
        r'Somewhere To Write.*?Love',  # Footer section
        r'STW#\d+'  # Post ID
    ]

    # Remove non-human-generated content using regular expressions
    cleaned_text = text
    for pattern in patterns:
        cleaned_text = re.sub(pattern, '', cleaned_text, flags=re.DOTALL)

    return cleaned_text.strip()

# Load the data from the xlsx file
file_path = "C:/Users/admin/Documents/posts_data.xlsx"
xls = pd.ExcelFile(file_path)

# Initialize a dictionary to store the original and cleaned data for each sheet
original_data = {}
cleaned_data = {}

# Iterate over each sheet in the Excel file
for sheet_name in xls.sheet_names:
    # Read data from the current sheet
    data = pd.read_excel(xls, sheet_name)
    
    # Skip processing if the number of rows is 0
    if len(data) == 0:
        print("Skipping sheet", sheet_name, "as it has no rows.")
        continue
    
    # Store the original data for this sheet
    original_data[sheet_name] = data.copy()
    
    # Print the sheet name and total number of rows in the DataFrame
    print("Processing sheet:", sheet_name)
    print("Total number of rows:", len(data))
    
    # Clean up the "Post Content" column using BERT and the defined cleaning function
    print("Processing rows...")
    for idx, row in data.iterrows():
        # If the length of rows exceeds 512, only clean up to row 512
        if idx >= 512:
            break
        
        data.at[idx, "Post Content"] = clean_text_with_bert(row["Post Content"])
        print("Processed row", idx + 1, "of", min(len(data), 512))
    
    # Store the cleaned data for this sheet
    cleaned_data[sheet_name] = data
    
    # Print a before-and-after comparison of the cleaned Post Content column for a random row
    random_row_idx = random.randint(0, min(len(data)-1, 512))  # Random row index, limited to 512 rows
    original_text = original_data[sheet_name].at[random_row_idx, "Post Content"]
    cleaned_text = clean_text_with_bert(original_text)
    print("\nBefore cleaning (row", random_row_idx + 1, "):\n", original_text)
    print("\nAfter cleaning (row", random_row_idx + 1, "):\n", cleaned_text)
    print("\n---\n")

# Save the cleaned data back to the xlsx file
with pd.ExcelWriter(file_path) as writer:
    for sheet_name, df in cleaned_data.items():
        df.to_excel(writer, sheet_name=sheet_name, index=False)

print("All sheets processed.")
