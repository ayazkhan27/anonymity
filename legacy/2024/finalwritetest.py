import pandas as pd
import requests
from bs4 import BeautifulSoup
from datetime import datetime
import re
from urllib.parse import urljoin

def fetch_html(url):
    """Fetch the HTML content of a given URL."""
    response = requests.get(url)
    if response.status_code == 200:
        return BeautifulSoup(response.content, 'html.parser')
    else:
        print(f"Failed to retrieve content from the URL: {url}")
        return None

def extract_post_content(post_html):
    """Extract the post content from the HTML."""
    content_tag = post_html.find('p')
    if content_tag:
        content = content_tag.get_text(strip=True)
        return content
    return "No Content"

def extract_posts(soup, keyword, page_number, base_url):
    """Extract posts from the page."""
    posts = soup.find_all('div', class_='post')
    bot_indicators = ["download for free", "survey", "http", "https", "www.", "torrent", "hack tool", "hack"]
    special_chars = set("ÜÄÝÞßþÙ")
    data = []

    for post in posts:
        title_tag = post.find('h1').find('a')
        title = title_tag.text.strip() if title_tag else "No Title"
        content_html = str(post.find('p'))

        # Skip bot-like content and check for special characters
        if (any(indicator in title.lower() for indicator in bot_indicators) or
                any(indicator in content_html.lower() for indicator in bot_indicators) or
                any(char in content_html for char in special_chars)):
            continue

        more_link = post.find('a', href=re.compile(r'/\d+'))
        if more_link:
            full_content_url = urljoin(base_url, more_link['href'])
            full_content_soup = fetch_html(full_content_url)
            content = extract_post_content(full_content_soup) if full_content_soup else "No Content"
        else:
            content = extract_post_content(post)

        details = post.find('div', class_='postdetails').get_text()
        date_time_match = re.search(r'on (\w+ \d{1,2}, \d{4} - \d{1,2}:\d{2} [ap]m)', details)
        date_time = datetime.strptime(date_time_match.group(1), '%B %d, %Y - %I:%M %p') if date_time_match else None

        if date_time:
            post_data = {
                'Keyword': keyword,
                'Page': page_number,
                'Title': title,
                'Post Content': content,
                'year': date_time.year,
                'month': date_time.strftime('%B'),
                'day_of_week': date_time.strftime('%A'),
                'Time': date_time.strftime('%H:%M')
            }
            data.append(post_data)

    return data

def scrape_all_posts(keywords, base_url):
    all_posts = []
    for keyword in keywords:
        page = 1
        while True:
            url = f"{base_url}/search/{keyword}/page/{page}"
            soup = fetch_html(url)
            if soup is None:
                print(f"Failed to fetch page {page} for keyword '{keyword}'.")
                break
            posts = extract_posts(soup, keyword, page, base_url)
            if not posts:
                print(f"No more relevant posts found for keyword '{keyword}' at page {page}. Stopping.")
                break
            all_posts.extend(posts)
            page += 1
    return all_posts

# Configuration for scraping
base_url = "https://www.somewheretowrite.com"
happy_keywords = ["joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive"]
sad_keywords = ["death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast"]
keywords = happy_keywords + sad_keywords

# Start scraping process
all_posts = scrape_all_posts(keywords, base_url)

# Save data to DataFrame
df = pd.DataFrame(all_posts)
df.to_excel('posts_data.xlsx', index=False)
print("All data has been compiled and saved successfully.")
