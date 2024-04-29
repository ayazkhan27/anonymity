import tweepy
import pandas as pd

# Twitter API credentials
consumer_key = "rmMkUzWmeCRS9QeDVHASVWo5Z"
consumer_secret = "YotrKkZSBNGj1mBpdp842fKoyQchekIFWxrYm6CPt1yEtYAVCp"
access_token = "1642044832561393665-aMPjru2lh242JqTIpEK5Zzidpcv5Fe"
access_token_secret = "9KSiPq3kDcQ2En4FujEoV1UmVyoV1iAAHPjXDoVvl3qys"
bearer_token = "AAAAAAAAAAAAAAAAAAAAAHT7tAEAAAAAZns56aVxdVxLU2aWYTFNx4ATqVU%3D4CeSlqR3bGkE4BUPFoYbq4Wc1CMxIU1756pLGQekZG3LEfxiZe"

# OAuth 1.0a User Context Authentication
auth1 = tweepy.OAuth1UserHandler(
   consumer_key, consumer_secret,
   access_token, access_token_secret
)

# OAuth 2.0 Bearer Token (App-Only) Authentication
auth2 = tweepy.OAuth2BearerHandler(bearer_token)

# Initialize Tweepy Client with the appropriate authentication
client = tweepy.Client(auth1)  # Choose either auth1 or auth2 here

def fetch_tweets(keyword):
    try:
        tweets = client.search_recent_tweets(query=keyword, tweet_fields=["text"], max_results=100)
        tweet_texts = [tweet.text for tweet in tweets]
        return tweet_texts
    except tweepy.errors.TweepError as e:
        print("Error occurred:", e)
        return []

# Keywords
happy_keywords = ["joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive"]

sad_keywords = ["death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast"]

# Fetch tweets for happy and sad keywords
happy_tweets = fetch_tweets(happy_keywords)
sad_tweets = fetch_tweets(sad_keywords)

# Write data to Excel file
with pd.ExcelWriter('tweets_data.xlsx', engine='xlsxwriter') as writer:
    pd.DataFrame({'Happy Keywords': happy_tweets}).to_excel(writer, sheet_name='Happy Keywords', index=False)
    pd.DataFrame({'Sad Keywords': sad_tweets}).to_excel(writer, sheet_name='Sad Keywords', index=False)
