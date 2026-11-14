import pandas as pd
import requests
from bs4 import BeautifulSoup
from datetime import datetime
import re

def fetch_html(url):
    try:
        response = requests.get(url)
        response.raise_for_status()
        return BeautifulSoup(response.content, 'html.parser')
    except requests.RequestException as e:
        print(f"Failed to retrieve content from {url}: {str(e)}")
        return None

def extract_posts(soup, keyword, page_number, show_debug):
    # Bot indicators and special characters
    bot_indicators = ["download for free", "survey", "http", "https", "www.", "torrent", "hack tool", "hack"]
    special_chars = set("ÜÄÝÞßþÙ")

    posts = soup.find_all('div', class_='post')
    print(f"Found {len(posts)} posts for keyword '{keyword}'.")  # Debug: Number of posts found
    data = []
    post_number = 1  # Initialize post counter
    debug_shown = False
    
    for post in posts:
        try:
            title_tag = post.find('h1').find('a')
            title = title_tag.text.strip() if title_tag else "No Title"
            content_tag = post.find('p')
            content = content_tag.text.strip() if content_tag else "No Content"
            content = content.replace("[..more..]", "").strip()

            # Skip bot-like content and check for special characters
            if any(indicator in title.lower() or indicator in content.lower() for indicator in bot_indicators) or any(char in content for char in special_chars):
                continue

            date_time = extract_date_time(post)
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
                post_number += 1  # Increment post counter

                if show_debug and not debug_shown:
                    print(f"Post extracted: {post_data}")
                    debug_shown = True  # Set flag to prevent further debug messages

        except Exception as e:
            print(f"Error processing post: {str(e)}")

    return data

def extract_date_time(post):
    details = post.find('div', class_='postdetails').get_text()
    date_time_match = re.search(r'on (\w+ \d{1,2}, \d{4} - \d{1,2}:\d{2} [ap]m)', details)
    return datetime.strptime(date_time_match.group(1), '%B %d, %Y - %I:%M %p') if date_time_match else None


def extract_date_time(post):
    details = post.find('div', class_='postdetails').get_text()
    date_time_match = re.search(r'(\w+ \d{1,2}, \d{4} - \d{1,2}:\d{2} [ap]m)', details)
    if date_time_match:
        return datetime.strptime(date_time_match.group(1), '%B %d, %Y - %I:%M %p')
    return None

def scrape_all_posts(keywords, base_url):
    all_posts = []
    for keyword in keywords:
        page = 1
        show_debug = True  # Set to True to enable debugging output
        while True:
            # Construct the URL based on page number
            if page == 1:
                url = f"{base_url}/?s={keyword}"
            else:
                url = f"{base_url}/page/{page}?s={keyword}"
            
            soup = fetch_html(url)
            if soup is None:
                print(f"Failed to fetch page {page} for keyword '{keyword}'. Stopping pagination.")
                break
            
            # Pass the correct arguments to extract_posts
            posts = extract_posts(soup, keyword, page, show_debug)
            if not posts:
                print(f"No more posts found for keyword '{keyword}' at page {page}. Ending scraping for this keyword.")
                break
            
            all_posts.extend(posts)
            page += 1
            show_debug = False  # Optionally disable debugging after the first page

    return all_posts

base_url = "https://www.somewheretowrite.com"
keywords = ["joyous", "celebration", "wonderful", "death", "sad", "depressed"]
all_posts = scrape_all_posts(keywords, base_url)
df = pd.DataFrame(all_posts)
df.to_excel('C:/Users/admin/Documents/anonymitysentiment/posts_data.xlsx', index=False)
print("All data has been compiled and saved successfully.")
