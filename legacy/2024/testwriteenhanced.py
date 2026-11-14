import pandas as pd
import requests
from bs4 import BeautifulSoup
from datetime import datetime
import re

def fetch_html(url):
    response = requests.get(url)
    if response.status_code == 200:
        return BeautifulSoup(response.content, 'html.parser')
    else:
        print("Failed to retrieve content from the URL:", url)
        return None

def extract_posts(soup, keywords, post_limit=5):
    posts = soup.find_all('div', class_='post')
    bot_indicators = ["download for free", "survey", "http://", "https://", "www."]
    special_chars = set("ÜÄÝÞßþÙ")  # Add more special characters as needed
    data = []
    post_count = 0
    
    for post in posts:
        if post_count >= post_limit:
            break

        title_tag = post.find('h1').find('a')
        title = title_tag.text.strip() if title_tag else "No Title"
        content_tag = post.find('p')
        content = content_tag.text.strip() if content_tag else "No Content"
        
        # Clean the content of any trailing text like "[..more..]"
        content = content.replace("[..more..]", "").strip()
        
        # Skip bot-like content and check for special characters
        if any(indicator in content.lower() for indicator in bot_indicators) or any(char in content for char in special_chars):
            continue
        
        # Extracting the date-time using a regular expression
        details = post.find('div', class_='postdetails').get_text()
        date_time_match = re.search(r'(\w+ \d{1,2}, \d{4} - \d{1,2}:\d{2} [ap]m)', details)
        if date_time_match:
            date_time_str = date_time_match.group(1)
            date_time = datetime.strptime(date_time_str, '%B %d, %Y - %I:%M %p')
            post_data = {
                'Title': title,
                'Post Content': content,
                'Year': date_time.year,
                'Month': date_time.strftime('%B'),
                'Day of Week': date_time.strftime('%A'),
                'Time Stamp': date_time.strftime('%Y-%m-%d %H:%M:%S')
            }
            data.append(post_data)
            post_count += 1
            # Print the extracted post content for debugging, but only the first 5
            print("Extracted post:", post_data['Post Content'])

    return data

def scrape_all_posts(keywords, base_url):
    page = 1
    all_posts = []
    for keyword in keywords:
        print(f"Scraping for keyword '{keyword}'...")
        while True:
            url = f"{base_url}/search/{keyword}/page/{page}"
            soup = fetch_html(url)
            if soup is None:
                print("Failed to fetch page:", page)
                break
            print(f"Scraping page {page} for keyword '{keyword}'...")
            posts = extract_posts(soup, keywords)
            if not posts:
                print(f"No more relevant posts found for keyword '{keyword}' at page {page}. Stopping.")
                break
            all_posts.extend(posts)
            page += 1
    return all_posts

base_url = "https://www.somewheretowrite.com"
keywords = [
    # Positive Sentiment Keywords
    "joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic",
    "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled",
    "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant",
    "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate",
    "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive", "enchanted",
    "charmed", "overjoyed", "pleased", "satisfied", "rewarded", "fulfilled",
    "accomplished", "proud", "inspired", "motivated", "enthusiastic", "optimistic",
    "hopeful", "sanguine", "contented", "peaceful", "rejuvenated", "refreshed",
    "relieved", "serene", "tranquil", "secure", "loved", "adored", "cherished",
    "valued", "respected", "praised", "esteemed", "admired", "welcomed", "celebrated",
    "approved", "applauded", "honored", "empathetic", "compassionate", "supportive",
    "caring", "kind", "generous", "affectionate", "passionate", "fond", "loving",
    "playful", "funny", "entertaining", "amusing", "lighthearted", "joyful",
    "spirited", "energetic", "lively", "invigorated", "stimulated", "keen",
    "eager", "fervent", "enthusiastic", "involved", "engaged", "interested",
    "attracted", "intrigued", "fascinated", "enthralled", "captivated",
    "charismatic", "dynamic", "vibrant", "sparkling", "dazzling", "shining",
    "glowing", "flourishing", "thriving", "prospering", "successful", "winning",
    "leading", "prominent", "eminent", "reputable", "influential", "powerful",
    "strong", "healthy", "robust", "vital", "vigorous", "fit", "wholesome",
    "hearty", "blooming", "sufficient", "ample", "plentiful", "bountiful",

    # Negative Sentiment Keywords
    "death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic",
    "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken",
    "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate",
    "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful",
    "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast",
    "upset", "disturbed", "displeased", "annoyed", "agitated", "frustrated",
    "irritated", "angry", "furious", "enraged", "incensed", "outraged", "wrathful",
    "offended", "insulted", "neglected", "rejected", "excluded", "isolated",
    "abandoned", "lonely", "alone", "lost", "confused", "perplexed", "indignant",
    "embittered", "sour", "disgruntled", "discontented", "disappointed", "dissatisfied",
    "defeated", "helpless", "powerless", "hopeless", "demoralized", "discouraged",
    "uninspired", "dull", "bored", "tired", "fatigued", "weary", "burned out",
    "exhausted", "strained", "stressed", "pressured", "troubled", "tormented",
    "tortured", "afflicted", "harassed", "bothered", "worried", "alarmed", "frightened",
    "scared", "terrified", "horrified", "apprehensive", "panicked", "hysterical",
    "shocked", "stunned", "flustered", "rattled", "disoriented", "unsettled",
    "uncomfortable", "insecure", "vulnerable", "exposed", "threatened", "menaced",
    "endangered", "imperiled", "doomed", "defective", "flawed", "imperfect",
    "inadequate", "insufficient", "lacking", "wanting", "needing", "craving",
    "desiring", "longing", "yearning", "pining", "nostalgic", "homesick", "remorseful",
    "guilty", "ashamed", "chagrined", "humiliated", "embarrassed", "awkward",
    "clumsy", "inept", "incompetent", "ineffective", "useless", "unproductive",
    "futile", "pointless", "senseless", "absurd", "ridiculous", "laughable",
    "silly", "foolish", "stupid", "idiotic", "crazy", "insane", "mad", "deranged",
    "manic", "wild", "uncontrolled", "chaotic", "disordered", "confused", "disarrayed",
    "disorganized", "untidy", "messy", "muddled", "scattered", "dispersed", "squandered",
    "wasted"
]


all_posts = scrape_all_posts(keywords, base_url)
df = pd.DataFrame(all_posts)
df.to_excel('C:/Users/admin/Documents/anonymitysentiment/posts_data.xlsx', index=False)
print("All data has been compiled and saved successfully.")
