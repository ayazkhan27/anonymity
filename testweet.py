import tweepy

# Twitter API credentials
bearer_token = "AAAAAAAAAAAAAAAAAAAAAHT7tAEAAAAAZns56aVxdVxLU2aWYTFNx4ATqVU%3D4CeSlqR3bGkE4BUPFoYbq4Wc1CMxIU1756pLGQekZG3LEfxiZe"

# Initialize Tweepy Client with OAuth2BearerHandler instance
client = tweepy.Client(bearer_token=bearer_token)

def fetch_tweets(tweet_ids):
    try:
        # Fetch tweets based on specified tweet IDs and specify additional fields
        tweets = client.search_recent_tweets(
            query=f"ids:{','.join(tweet_ids)}", 
            tweet_fields=["created_at"], 
            expansions=["author_id"], 
            user_fields=["created_at"]
        )
        return tweets
    except tweepy.TweepError as e:
        print("Error occurred:", e)
        return []

# List of tweet IDs
tweet_ids = ["1228393702244134912", "1227640996038684673", "1199786642791452673"]

# Fetch tweets
tweets_response = fetch_tweets(tweet_ids)

# Print the fetched tweets
for tweet in tweets_response.data:
    print("Tweet ID:", tweet.id)
    print("Author ID:", tweet.author_id)
    print("Created At:", tweet.created_at)
    print("Text:", tweet.text)
    print()

# Print the included users
if tweets_response.includes is not None and 'users' in tweets_response.includes:
    for user in tweets_response.includes['users']:
        print("User ID:", user.id)
        print("Name:", user.name)
        print("Username:", user.username)
        print("Created At:", user.created_at)
        print()
