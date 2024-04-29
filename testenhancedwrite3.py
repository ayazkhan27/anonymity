import pandas as pd
import requests
from bs4 import BeautifulSoup
from datetime import datetime
import re

def fetch_html(url):
    """Fetch the HTML content of a given URL."""
    response = requests.get(url)
    if response.status_code == 200:
        return BeautifulSoup(response.content, 'html.parser')
    else:
        print(f"Failed to retrieve content from the URL: {url}")
        return None

def get_full_post_content(more_url):
    """Retrieve the full post content from the detailed post page."""
    soup = fetch_html(more_url)
    if soup:
        content_section = soup.find('span', id='the-content')
        return content_section.text.strip() if content_section else "No extended content found"
    return "Failed to fetch extended content"

def extract_posts(soup, keyword, page_number, show_debug):
    """Extract posts from the page, following links for full content if necessary."""
    posts = soup.find_all('div', class_='post')
    bot_indicators = ["hack", "hack tool", "http", "torrent", "download for free", "survey", "http://", "https://", "www.", "http", "hack", "hack tool", "torrent"]
    special_chars = set("ÜÄÝÞßþÙ")  # Additional special characters can be added as needed
    data = []
    post_number = 1  # Initialize post number for each page
    
    for post in posts:
        title_tag = post.find('h1').find('a')
        title = title_tag.text.strip() if title_tag else "No Title"
        content_tag = post.find('p')
        content = content_tag.text.strip() if content_tag else "No Content"

        # Clean the content of any trailing text like "[..more..]"
        content = content.replace("[..more..]", "").strip()

        # Skip bot-like content in both title and content and check for special characters
        if (any(indicator in title.lower() for indicator in bot_indicators) or 
            any(indicator in content.lower() for indicator in bot_indicators) or
            any(char in content for char in special_chars)):
            continue

        more_link = post.find('a', text=re.compile(r'\[\.\.\.more\.\.\.\]'))
        if more_link and more_link.has_attr('href'):
            full_content_url = f"https://www.somewheretowrite.com{more_link['href']}"
            content = get_full_post_content(full_content_url)  # Fetch the full content using the hyperlink
        else:
            content_tag = post.find('p')  # Initial content tag
            content = content_tag.text.strip() if content_tag else "No Content"
        
        details = post.find('div', class_='postdetails').get_text()
        date_match = re.search(r'on (\w+ \d{1,2}, \d{4} - \d{1,2}:\d{2} [ap]m)', details)
        date_time = datetime.strptime(date_match.group(1), '%B %d, %Y - %I:%M %p') if date_match else None

        if date_time:
            post_data = {
                'Keyword': keyword,
                'Page': page_number,
                'Post Number': post_number,
                'Title': title,
                'Post Content': content,
                'year': date_time.year,
                'month': date_time.strftime('%B'),
                'day_of_week': date_time.strftime('%A'),
                'Time': date_time.strftime('%H:%M')
            }
            data.append(post_data)
            post_number += 1  # Increment post number
            
            if show_debug:
                print(f"Debug - Keyword '{keyword}': Page {page_number}, Post {post_number}")
                print(f"  Title: {title}, Date: {date_time.strftime('%Y-%m-%d')}, Time: {date_time.strftime('%H:%M')}")
                show_debug = False  # Print debug info for the first post only

    return data

def scrape_all_posts(keywords, base_url):
    """Scrape posts for each keyword."""
    all_posts = []
    for keyword in keywords:
        page = 1
        show_debug = True
        while True:
            url = f"{base_url}/search/{keyword}/page/{page}"
            soup = fetch_html(url)
            if soup is None:
                print(f"Failed to fetch page {page} for keyword '{keyword}'.")
                break
            posts = extract_posts(soup, keyword, page, show_debug)
            if not posts:
                print(f"No relevant posts found for keyword '{keyword}' at page {page}.")
                break
            all_posts.extend(posts)
            page += 1  # Increment page number for pagination
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
