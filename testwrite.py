import pandas as pd
import requests
from bs4 import BeautifulSoup

# Function to convert HTML to plain text
def html_to_text(url):
    response = requests.get(url)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        # Extract text from HTML content
        text = soup.get_text(separator='\n')
        return text
    else:
        print("Failed to retrieve content from the URL.")
        return None

# Function to filter out header and filler text and extract individual posts
def filter_and_extract_posts(text, keywords):
    # Split the text using the separator "Be the First to comment"
    posts = text.split("Be the First to Comment")
    # Filter out empty strings and whitespace
    posts = [post.strip() for post in posts if post.strip()]
    # Filter out header and filler text, and potential bot posts
    filtered_posts = []
    for post in posts:
        # Check if the post contains any keywords and doesn't contain "http" or "https"
        if any(keyword in post.lower() for keyword in keywords) and "http" not in post.lower():
            filtered_posts.append(post)
    return filtered_posts

# Function to scrape posts for a given keyword and page number
def scrape_posts_for_keyword(keyword, page):
    url = f"https://www.somewheretowrite.com/page/{page}?s={keyword}"
    text = html_to_text(url)
    return text

# Define the happy keywords to search for
happy_keywords = ["joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive"]

# Define the sad keywords to search for
sad_keywords = ["death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast"]

# Function to scrape posts and compile into a CSV file
def scrape_and_compile_csv(keywords, filename):
    data = {'Keyword': [], 'Page': [], 'Post Number': [], 'Post Content': []}
    for keyword in keywords:
        print(f"Scraping posts for keyword '{keyword}':")
        page = 1
        while True:
            text = scrape_posts_for_keyword(keyword, page)
            if text and "Be the First to Comment" in text:
                # Filter and extract individual posts
                posts = filter_and_extract_posts(text, keywords)
                if posts:
                    # Append posts to data dictionary
                    for i, post in enumerate(posts, start=1):
                        data['Keyword'].append(keyword)
                        data['Page'].append(page)
                        data['Post Number'].append(i)
                        data['Post Content'].append(post)
                    page += 1
                else:
                    print(f"No relevant posts found for keyword '{keyword}' on page {page}.")
                    break
            else:
                print(f"No content to analyze for keyword '{keyword}' on page {page}.")
                break

    # Convert data dictionary to DataFrame
    df = pd.DataFrame(data)
    # Write DataFrame to CSV file with separate sheets for happy and sad keywords
    with pd.ExcelWriter(filename, engine='xlsxwriter') as writer:
        for keyword in keywords:
            df_keyword = df[df['Keyword'] == keyword]
            df_keyword.to_excel(writer, sheet_name=keyword, index=False)

# Scrape posts and compile into CSV file
scrape_and_compile_csv(happy_keywords + sad_keywords, 'posts_data.xlsx')
