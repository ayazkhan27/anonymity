import requests
from bs4 import BeautifulSoup

# Function to scrape posts for a given keyword
def scrape_posts_for_keyword(keyword):
    url = f"https://www.somewheretowrite.com/?s={keyword}"
    response = requests.get(url)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        # Extract text from HTML content
        text = soup.get_text(separator='\n')
        return text
    else:
        print(f"Failed to retrieve content for the keyword '{keyword}'.")
        return None

# Predefined lists of happy and sad words along with synonyms
happy_words = ["joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive"]
sad_words = ["death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast"]

# Initialize counters for happy and sad posts
happy_post_count = 0
sad_post_count = 0

# Iterate over each keyword and scrape posts
for keyword in happy_words + sad_words:
    text = scrape_posts_for_keyword(keyword)
    if text:
        # Perform further analysis here (e.g., count happy and sad posts)
        if keyword in happy_words:
            happy_post_count += text.lower().count(keyword)
        if keyword in sad_words:
            sad_post_count += text.lower().count(keyword)

# Print the counts
print("Number of happy posts:", happy_post_count)
print("Number of sad posts:", sad_post_count)
