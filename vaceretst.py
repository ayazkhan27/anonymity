import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

# Load the data
file_path = "C:/Users/admin/AppData/Local/Programs/Python/Python312/posts_data.xlsx"
posts_data = pd.read_excel(file_path, engine='openpyxl')

# Remove duplicates
posts_data = posts_data.drop_duplicates(subset=['Post Content'])

# Convert all entries in 'Post Content' to strings to avoid TypeError
posts_data['Post Content'] = posts_data['Post Content'].astype(str)

# Ensure 'post_date' is a datetime object
posts_data['post_date'] = pd.to_datetime(posts_data['post_date'])

# Initialize VADER sentiment analyzer
analyzer = SentimentIntensityAnalyzer()

# Apply VADER to each post and store the compound scores
posts_data['vader_scores'] = posts_data['Post Content'].apply(lambda x: analyzer.polarity_scores(x)['compound'])

# Extract year from 'post_date'
posts_data['year'] = posts_data['post_date'].dt.year

# Filter years from 2009 to 2024
filtered_data = posts_data[(posts_data['year'] >= 2009) & (posts_data['year'] <= 2024)]

# Calculate mean sentiment score for each year
mean_scores_by_year = filtered_data.groupby('year')['vader_scores'].mean().reset_index()

# Print the results
print(mean_scores_by_year)
