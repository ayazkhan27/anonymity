import torch
from transformers import BertTokenizer, BertForSequenceClassification, AdamW
from torch.utils.data import TensorDataset, DataLoader
import numpy as np

def fine_tune_bert(train_texts, train_labels, test_texts, test_labels):
    # Load the pre-trained BERT tokenizer
    tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')

    # Tokenize and encode the input texts
    train_encodings = tokenizer(list(train_texts), truncation=True, padding=True)
    test_encodings = tokenizer(list(test_texts), truncation=True, padding=True)

    # Convert the data to PyTorch tensors
    train_dataset = TensorDataset(torch.tensor(train_encodings['input_ids']),
                                  torch.tensor(train_encodings['attention_mask']),
                                  torch.tensor(train_labels))  # Remove .values

    test_dataset = TensorDataset(torch.tensor(test_encodings['input_ids']),
                                 torch.tensor(test_encodings['attention_mask']),
                                 torch.tensor(test_labels))  # Remove .values

    # Define the data loaders
    train_loader = DataLoader(train_dataset, batch_size=8, shuffle=True)
    test_loader = DataLoader(test_dataset, batch_size=8, shuffle=False)

    # Load the pre-trained BERT model for sequence classification
    model = BertForSequenceClassification.from_pretrained('bert-base-uncased', num_labels=1)

    # Set up the optimizer and learning rate scheduler
    optimizer = AdamW(model.parameters(), lr=2e-5)

    # Fine-tune the model
    epochs = 3
    for epoch in range(epochs):
        model.train()
        total_loss = 0
        for batch in train_loader:
            input_ids, attention_mask, labels = batch
            outputs = model(input_ids, attention_mask=attention_mask, labels=labels)
            loss = outputs.loss
            total_loss += loss.item()
            loss.backward()
            optimizer.step()
            optimizer.zero_grad()
        print(f'Epoch {epoch + 1}, Loss: {total_loss / len(train_loader)}')

    # Evaluate the fine-tuned model
    model.eval()
    predictions = []
    true_values = []
    with torch.no_grad():
        for batch in test_loader:
            input_ids, attention_mask, labels = batch
            outputs = model(input_ids, attention_mask=attention_mask)
            logits = outputs.logits
            predictions.extend(logits.squeeze().tolist())
            true_values.extend(labels.tolist())

    # Calculate evaluation metrics
    true_values = np.array(true_values)
    predictions = np.array(predictions)
    mse = np.mean((predictions - true_values) ** 2)
    rmse = np.sqrt(mse)
    r_squared = np.corrcoef(predictions, true_values)[0, 1] ** 2

    return mse, rmse, r_squared
