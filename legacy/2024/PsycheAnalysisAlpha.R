# SECTION 1: Sentiment Analysis
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)

# Step 1: Load data and preprocess
file_path <- "C:/Users/admin/AppData/Local/Programs/Python/Python312/posts_data.xlsx"

# Read data from all sheets
posts_data <- lapply(excel_sheets(file_path), function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    mutate_all(as.character)
}) %>% 
  bind_rows() %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE)) # Remove "uncategorized" and "anonymous"

# Step 2: Preprocess data
posts_data <- posts_data %>%
  mutate(Post_Content_clean = gsub("[[:punct:]]", "", `Post Content`)) %>%
  mutate(Post_Content_clean = gsub("\\d+", "", Post_Content_clean)) %>%
  unnest_tokens(word, Post_Content_clean) %>%
  anti_join(stop_words)

# Step 3: Remove "uncategorized" and "anonymous"
posts_data <- posts_data %>%
  filter(!word %in% c("uncategorized", "anonymous"))

# Step 4: Calculate sentiment scores
posts_data <- posts_data %>%
  mutate(sentiment_score = get_sentiment(word, method = "afinn"))

# Step 5: Remove repeated keywords
posts_data <- posts_data %>%
  group_by(Keyword, word) %>%
  mutate(word_count = n()) %>%
  filter(word_count < 3) %>%
  ungroup() %>%
  select(-word_count)

# Step 6: Filter out introductory website text
introductory_text <- c("Love","Work","Family","Friends","Games","Kids", "Life", "Home", "About", "Search", "Write", "Categories", "Login", "Write what you feel...", "Please enter a keyword, term, or post number to search for:")
posts_data <- posts_data %>%
  filter(!word %in% introductory_text)

# Step 7: Remove neutral sentiments and sentiment scores of 0
posts_data <- posts_data %>%
  filter(sentiment_score != 0) %>%
  filter(sentiment_score != "Neutral")

# Step 8: Classify posts as positive or negative
posts_data <- posts_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative"
  ))

# Step 9: Aggregate sentiment scores and count posts
aggregate_sentiment <- posts_data %>%
  group_by(sentiment_label) %>%
  summarise(mean_sentiment_score = mean(sentiment_score),
            total_posts = n())

# Print the aggregated sentiment scores
print(aggregate_sentiment)

# SECTION 2: Word Frequency Analysis

# Step 1: Define Happy and Sad Keywords
happy_keywords <- c("joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive")

sad_keywords <- c("death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast")

# Step 2: Count Word Frequency for Happy and Sad Keywords
count_keywords <- function(text, keywords) {
  keyword_count <- sum(grepl(paste0(keywords, collapse = "|"), text, ignore.case = TRUE))
  return(keyword_count)
}

# Define Happy and Sad Keywords
happy_keywords <- c("joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive")

sad_keywords <- c("death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast")

# Count word occurrences for happy and sad keywords
posts_data$happy_keyword_count <- sapply(posts_data$`Post Content`, count_keywords, keywords = happy_keywords)
posts_data$sad_keyword_count <- sapply(posts_data$`Post Content`, count_keywords, keywords = sad_keywords)

# Step 3: Filter out posts with no happy keywords or only sad keywords
happy_posts <- posts_data %>%
  filter(happy_keyword_count > 0 & sad_keyword_count == 0) # Exclude posts with sad keywords

sad_posts <- posts_data %>%
  filter(sad_keyword_count > 0 & happy_keyword_count == 0) # Exclude posts with happy keywords

# Step 4: Aggregate keyword counts and count posts
aggregate_keywords <- posts_data %>%
  summarise(total_happy_posts = sum(happy_keyword_count),
            total_sad_posts = sum(sad_keyword_count))

# Print the aggregated keyword counts
print(aggregate_keywords)

# SECTION 3: Word Clouds

# Step 1: Prepare data for word clouds
library(wordcloud)

# Preprocess data to remove "uncategorized" and "anonymous"
filtered_happy_posts <- happy_posts %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE))

filtered_sad_posts <- sad_posts %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from happy posts
filtered_happy_posts <- filtered_happy_posts %>%
  mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from sad posts
filtered_sad_posts <- filtered_sad_posts %>%
  mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Step 2: Create word clouds for happy and sad posts
happy_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_happy_posts$`Post Content`, "\\s+")),
                                        max.words = 100, colors = brewer.pal(8, "Dark2"),
                                        scale = c(2, 0.7), random.order = FALSE, rot.per = 0.35)

sad_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_sad_posts$`Post Content`, "\\s+")),
                                      max.words = 100, colors = brewer.pal(8, "Dark2"),
                                      scale = c(2, 0.7), random.order = FALSE, rot.per = 0.35)

###DEAD SECTION: FAILURE###
# SECTION 4: Topic Modeling

# Step 1: Create Document-Term Matrix for Happy and Sad Posts
happy_corpus <- Corpus(VectorSource(filtered_happy_posts$`Post Content`))
sad_corpus <- Corpus(VectorSource(filtered_sad_posts$`Post Content`))

happy_dtm <- DocumentTermMatrix(happy_corpus)
sad_dtm <- DocumentTermMatrix(sad_corpus)

# Step 2: Apply Topic Modeling Techniques
library(topicmodels)

# Example using Latent Dirichlet Allocation (LDA)
happy_lda <- LDA(happy_dtm, k = 5)  # Assuming 5 topics for happy posts
sad_lda <- LDA(sad_dtm, k = 5)      # Assuming 5 topics for sad posts

# Step 3: Visualize Topics
happy_topics <- terms(happy_lda, 10)  # Display top terms for each topic in happy posts
sad_topics <- terms(sad_lda, 10)      # Display top terms for each topic in sad posts

# Print the top terms for happy posts
print("Top terms for happy posts:")
print(happy_topics)

# Print the top terms for sad posts
print("Top terms for sad posts:")
print(sad_topics)
###SECTION 4 = FAILURE###

# SECTION 5: Emotion Intensity Analysis

# Step 1: Load required libraries
library(tidytext)
library(dplyr)

# Step 2: Load Emotion Lexicon
data("nrc")
lexicon <- get_sentiments("nrc")

# Step 3: Tokenize Post Content
post_words <- posts_data %>%
  unnest_tokens(word, 'Post Content')

# Step 4: Join Emotion Lexicon with Tokenized Words
post_words_emotion <- post_words %>%
  inner_join(lexicon, by = "word")

# Step 5: Calculate Proportion of Words Related to Each Emotion
emotion_prop <- post_words_emotion %>%
  count(sentiment) %>%
  mutate(proportion = n / sum(n))

# Step 6: Visualize Emotion Intensity
library(ggplot2)
ggplot(emotion_prop, aes(x = sentiment, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Proportion of Words Related to Each Emotion",
       x = "Emotion", y = "Proportion") +
  theme_minimal()

# Step 7: Visualize Words Contribution to Sentiment Using NRC Lexicon
word_contribution <- post_words_emotion %>%
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ggplot(word_contribution, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Words Contributing to Sentiment Using NRC Lexicon",
       x = "Word", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels and adjust spacing

###SECTION 6: ANALYSIS

# Extract year from post_date
posts_data$year <- lubridate::year(posts_data$post_date)

# Extract month from Post Content
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
month_pattern <- paste0(months, collapse = "|")

posts_data$month <- NA

for (i in 1:length(months)) {
  posts_data$month[grepl(months[i], posts_data$`Post Content`, ignore.case = TRUE)] <- months[i]
}

# Extract day of the week from Post Content
days_of_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_of_week_pattern <- paste0(days_of_week, collapse = "|")

posts_data$day_of_week <- NA

for (i in 1:length(days_of_week)) {
  posts_data$day_of_week[grepl(days_of_week[i], posts_data$`Post Content`, ignore.case = TRUE)] <- days_of_week[i]
}

# Subset Data for Training and Testing
training_data <- subset(posts_data, year(post_date) %in% 2009:2016)
testing_data <- subset(posts_data, year(post_date) %in% 2017:2024)

# Prepare Sentiment Label for Modeling
# Convert sentiment_label to numeric
#training_data$sentiment_label_num <- ifelse(training_data$sentiment_label == "Positive", 1, 0)
#testing_data$sentiment_label_num <- ifelse(testing_data$sentiment_label == "Positive", 1, 0)

# Define Keyword Categories Function
categorize_keywords <- function(posts) {
  # Initialize categories vector
  categories <- character(length(posts))
  
  # Define happy and sad keywords
  happy_keywords <- c("joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive")
  
  sad_keywords <- c("death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast")
  
  # Categorize posts based on keywords
  for (i in seq_along(posts)) {
    if (any(grepl(paste(happy_keywords, collapse = "|"), posts[i], ignore.case = TRUE))) {
      categories[i] <- "Happy"
    } else if (any(grepl(paste(sad_keywords, collapse = "|"), posts[i], ignore.case = TRUE))) {
      categories[i] <- "Sad"
    } else {
      categories[i] <- "Neutral"
    }
  }
  return(categories)
}

# Keyword Category
training_data$keyword_category <- categorize_keywords(training_data$`Post Content`)
testing_data$keyword_category <- categorize_keywords(testing_data$`Post Content`)

# Function to extract date from Post Content
extract_date <- function(content) {
  # Define the date pattern
  date_pattern <- "\\b\\w+ \\d{1,2}, \\d{4}\\b"
  
  # Extract date using regex
  date_match <- regmatches(content, regexpr(date_pattern, content))
  
  # Convert date to Date object
  date <- as.Date(date_match, format = "%B %d, %Y")
  
  return(date)
}

# Apply the function to extract dates
training_data$extracted_date <- sapply(training_data$`Post Content`, extract_date)

# Function to determine day of the week
determine_day_of_week <- function(date) {
  return(weekdays(date))
}

# Replace NA values in day_of_week column with determined day of the week
training_data$day_of_week[is.na(training_data$day_of_week)] <- 
  sapply(training_data$extracted_date[is.na(training_data$day_of_week)], determine_day_of_week)

# Remove the extracted_date column
training_data$extracted_date <- NULL

# Subset Data for Training and Testing
training_data <- subset(posts_data, year(post_date) %in% 2009:2016)
testing_data <- subset(posts_data, year(post_date) %in% 2017:2024)

# Prepare Sentiment Label for Modeling
# Convert sentiment_label to numeric
#training_data$sentiment_label_num <- ifelse(training_data$sentiment_label == "Positive", 1, 0)
#testing_data$sentiment_label_num <- ifelse(testing_data$sentiment_label == "Positive", 1, 0)

# Define Keyword Categories Function
categorize_keywords <- function(posts) {
  # Initialize categories vector
  categories <- character(length(posts))
  
  # Define happy and sad keywords
  happy_keywords <- c("joyous", "celebration", "wonderful", "amazing", "delightful", "ecstatic", "blissful", "cheerful", "exuberant", "jubilant", "euphoric", "thrilled", "content", "elated", "gleeful", "grateful", "happy", "merry", "radiant", "sunny", "upbeat", "victorious", "vivacious", "zestful", "blessed", "fortunate", "lucky", "jolly", "smiling", "joy", "happiness", "excited", "positive")
  
  sad_keywords <- c("death", "sad", "depressed", "worst", "miserable", "hate", "unhappy", "tragic", "grief", "heartbroken", "sorrow", "melancholy", "gloomy", "grief-stricken", "despair", "disheartened", "tearful", "unfortunate", "bleak", "desolate", "forlorn", "dejected", "woeful", "anguish", "dismal", "unbearable", "painful", "distraught", "regretful", "bereaved", "pain", "suffering", "negative", "downcast")
  
  # Categorize posts based on keywords
  for (i in seq_along(posts)) {
    if (any(grepl(paste(happy_keywords, collapse = "|"), posts[i], ignore.case = TRUE))) {
      categories[i] <- "Happy"
    } else if (any(grepl(paste(sad_keywords, collapse = "|"), posts[i], ignore.case = TRUE))) {
      categories[i] <- "Sad"
    } else {
      categories[i] <- "Neutral"
    }
  }
  return(categories)
}

# Keyword Category
training_data$keyword_category <- categorize_keywords(training_data$`Post Content`)
testing_data$keyword_category <- categorize_keywords(testing_data$`Post Content`)

# Create separate datasets for year, month, and day of the week
training_data_year <- training_data %>%
  group_by(year, keyword_category) %>%
  summarise(mean_sentiment_score = mean(sentiment_score, na.rm = TRUE),
            total_posts = n())

training_data_month <- training_data %>%
  group_by(year, month, keyword_category) %>%
  summarise(mean_sentiment_score = mean(sentiment_score, na.rm = TRUE),
            total_posts = n())

training_data_day <- training_data %>%
  group_by(year, day_of_week, keyword_category) %>%
  summarise(mean_sentiment_score = mean(sentiment_score, na.rm = TRUE),
            total_posts = n())

# Remove rows with missing values
training_data_year <- na.omit(training_data_year)
training_data_month <- na.omit(training_data_month)
training_data_day <- na.omit(training_data_day)

# Train Random Forest Model for each dataset
model_year <- randomForest(mean_sentiment_score ~ keyword_category + year + total_posts, data = training_data_year)
model_month <- randomForest(mean_sentiment_score ~ keyword_category + month + year + total_posts, data = training_data_month)
model_day <- randomForest(mean_sentiment_score ~ keyword_category + day_of_week + year + total_posts, data = training_data_day)

# Visualization
# Temporal Trends in Mean Sentiment Score
plot_year <- ggplot(training_data_year, aes(x = factor(year), y = mean_sentiment_score)) +
  geom_boxplot() +
  labs(title = "Temporal Trends in Mean Sentiment Score by Year (Training Data)",
       x = "Year", y = "Mean Sentiment Score")

plot_month <- ggplot(training_data_month, aes(x = factor(month, levels = months), y = mean_sentiment_score)) +
  geom_boxplot() +
  labs(title = "Mean Sentiment Score by Month (Training Data)",
       x = "Month", y = "Mean Sentiment Score")

plot_day <- ggplot(training_data_day, aes(x = factor(day_of_week, levels = days_of_week), y = mean_sentiment_score)) +
  geom_boxplot() +
  labs(title = "Mean Sentiment Score by Day of the Week (Training Data)",
       x = "Day of the Week", y = "Mean Sentiment Score")

# Display the plots
print(plot_year)
print(plot_month)
print(plot_day)

# Additional steps for evaluation, statistical analysis, etc. can be added here.