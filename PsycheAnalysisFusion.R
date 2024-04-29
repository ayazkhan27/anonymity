# SECTION 1: Sentiment Analysis
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)

# Step 1: Load data and preprocess
file_path <- "C:/Users/admin/AppData/Local/Programs/Python/Python312/posts_data.xlsx"
posts_data <- read_excel(file_path) %>%
  mutate(`Post Content` = as.character(`Post Content`))

# Remove duplicates based on 'Post Content' to avoid processing the same text multiple times
posts_data <- posts_data %>%
  distinct(`Post Content`, .keep_all = TRUE)

# Step 2: Preprocess data
posts_data <- posts_data %>%
  mutate(Post_Content_clean = gsub("[[:punct:]]", "", `Post Content`)) %>%
  mutate(Post_Content_clean = gsub("\\d+", "", Post_Content_clean)) %>%
  unnest_tokens(word, Post_Content_clean) %>%
  anti_join(stop_words)

# Step 3: Remove "uncategorized" and "anonymous"
posts_data <- posts_data %>%
  filter(!word %in% c("uncategorized", "anonymous"))

# Step 4: Calculate sentiment scores for each word
posts_data <- posts_data %>%
  mutate(sentiment_score = get_sentiment(word, method = "afinn"))

# Step 5: Classify sentiments
posts_data <- posts_data %>%
  mutate(sentiment_label = case_when(
    sentiment_score > 0 ~ "Positive",
    sentiment_score < 0 ~ "Negative",
    TRUE ~ "Neutral"  # Classifying zero sentiment score as Neutral
  ))

# Step 6: Count the sentiment labels
sentiment_counts <- posts_data %>%
  group_by(sentiment_label) %>%
  summarise(
    count = n(),
    mean_sentiment_score = mean(sentiment_score, na.rm = TRUE)
  )

# Print the sentiment counts
print(sentiment_counts)

# SECTION 2: Word Frequency Analysis

library(dplyr)

# Assuming posts_data has already been loaded and includes necessary columns
# Group data by 'Keyword', 'Page', 'Post Number', and calculate the mean sentiment score, while preserving the unique "Post Content" and other details
grouped_posts <- posts_data %>%
  group_by(Keyword, Page, `Post Number`) %>%
  summarise(
    "Post Content" = first(`Post Content`),  # Assuming all entries in a group share the same Post Content
    year = first(year),
    month = first(month),
    day_of_week = first(day_of_week),
    time = first(time),
    post_date = first(post_date),
    mean_sentiment_score = mean(sentiment_score, na.rm = TRUE),
    .groups = 'drop'
  )

# Split the grouped data into happy, sad, and neutral posts based on mean sentiment score
happy_posts <- grouped_posts %>%
  filter(mean_sentiment_score > 0)

sad_posts <- grouped_posts %>%
  filter(mean_sentiment_score < 0)

neutral_posts <- grouped_posts %>%
  filter(mean_sentiment_score == 0)

# Print the total counts for each category
cat("Total Happy Posts:", nrow(happy_posts), "\n")
cat("Total Sad Posts:", nrow(sad_posts), "\n")
cat("Total Neutral Posts:", nrow(neutral_posts), "\n")

# Optionally print the first few rows of each dataframe to verify
print(head(happy_posts))
print(head(sad_posts))
print(head(neutral_posts))


##revision
# SECTION 3: Word Clouds

# Step 1: Prepare data for word clouds
library(wordcloud)

# Preprocess data to remove "uncategorized" and "anonymous"
filtered_happy_posts <- happy_posts %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE))

filtered_sad_posts <- sad_posts %>%
  mutate(`Post Content` = gsub("\\b(uncategorized|anonymous)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from happy posts
##filtered_happy_posts <- filtered_happy_posts %>%
  #mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Filter out months, years, and additional non-human generated content from sad posts
#filtered_sad_posts <- filtered_sad_posts %>%
  #mutate(`Post Content` = gsub("\\b(Page|Categories|Term|Somewhere|Searched|Keyword|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|Comment|Comments|Donations|Login|January|February|March|April|May|June|July|August|September|October|November|December|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024|Home|About|Search|Write|Categories|Login|Love|Work|Family|Friends|Games|Kids|Life|Write what you feel)\\b", "", `Post Content`, ignore.case = TRUE))

# Step 2: Create word clouds for happy and sad posts
happy_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_happy_posts$`Post Content`, "\\s+")),
                                        max.words = 100, colors = brewer.pal(8, "Set2"),
                                        scale = c(2.75, 0.5), random.order = FALSE, rot.per = 0.45)

sad_wordcloud <- wordcloud::wordcloud(words = unlist(strsplit(filtered_sad_posts$`Post Content`, "\\s+")),
                                      max.words = 100, colors = brewer.pal(8, "Dark2"),
                                      scale = c(2.75, 1), random.order = FALSE, rot.per = 0.45)


# SECTION 5: Emotion Intensity Analysis

# Step 1: Load required libraries
library(tidytext)
library(dplyr)

# Step 2: Load Emotion Lexicon
data("nrc")
lexicon <- get_sentiments("nrc")

# Step 3: Tokenize Post Content and filter out words containing "March" or "August"
post_words <- posts_data %>%
  unnest_tokens(word, 'Post Content') %>%
  filter(!grepl("March", word, ignore.case = TRUE) & 
           !grepl("August", word, ignore.case = TRUE))

# Step 4: Join Emotion Lexicon with Tokenized Words
post_words_emotion <- post_words %>%
  inner_join(lexicon, by = "word")

# Step 5: Calculate Proportion of Words Related to Each Emotion
emotion_prop <- post_words_emotion %>%
  count(sentiment) %>%
  mutate(proportion = n / sum(n))

library(extrafont)
font_import()  # This might take a few minutes
loadfonts(device = "win")  # Use device = "win" for Windows; for Mac, use device = "quartz"; for Linux, use device = "cairo"


# Step 6: Visualize Emotion Intensity
library(ggplot2)
library(RColorBrewer)

library(showtext)

# Load Arial font from your system
font_add(family = "Arial", regular = "arial.ttf")  # Ensure you have arial.ttf on your system, adjust if using Helvetica

library(ggplot2)
library(RColorBrewer)

# Create a color palette
emotion_colors <- brewer.pal(n = length(unique(emotion_prop$sentiment)), name = "Set3")

# Plot with customized aesthetics using Arial
ggplot(emotion_prop, aes(x = sentiment, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = emotion_colors) +
  labs(title = "Proportion of Words Related to Each Emotion",
       x = "Emotion", y = "Proportion") +
  theme_minimal() +
  theme(text = element_text(size = 14, family = "Arial"),  # Apply Arial
        axis.title = element_text(size = 16, face = "bold", family = "Arial"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Arial"),
        legend.title = element_text(size = 16, family = "Arial"),
        legend.text = element_text(size = 14, family = "Arial"))




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
        axis.text.x = element_text(angle = 45, hjust = 1))

