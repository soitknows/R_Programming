library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)

df <- read.csv("../../python/dt_tweets.csv")
df <- df[df$is_retweet...realDonaldTrump.s.Tweets. == "false",]
df <- df[,c(2,4,11)]
names(df) <- c("created_at", "id", "tweet")
df$created_at <- as.Date(df$created_at, "%m/%d/%Y")
df$tweet <- as.character(df$tweet)
tweets <- df[,"tweet"]
tweets <- data_frame(line = 1:length(tweets), text = tweets)
tidy_tweets <- tweets %>% unnest_tokens(word, text)
data("stop_words")
tidy_tweets <- tidy_tweets %>% filter(str_detect(word, "^[a-z]"))
tweet_stops <- data_frame(line = 1:3, word = c('t.co', 'https', 'amp'))
tidy_tweets <- tidy_tweets %>% anti_join(stop_words)
tidy_tweets <- tidy_tweets %>% anti_join(tweet_stops, by = "word")
tidy_tweets %>% count(word, sort = TRUE)

tidy_tweets %>% 
    count(word, sort = TRUE) %>%
    filter(n > 100) %>%
    mutate(word = reorder(word, n )) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

nrc_joy <- get_sentiments("nrc") %>%
    filter(sentiment == "joy")

tidy_tweets %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE)