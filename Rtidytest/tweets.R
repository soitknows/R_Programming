library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(readr)

GetTweets <- function(file) {
    df <- readr::read_csv(file, 
                          col_types = cols_only("created_at" = "c", 
                                                "text"       = "c", 
                                                "is_retweet" = "c"))
    # Remove retweets.
    df <- df[df$is_retweet == "false",]
    # Remove retweet column.
    df <- df[,c("created_at", "text")]
    # Get news source name from csv file name.
    source <- stringr::str_extract(file, "[a-z]{1,}_")
    source <- stringr::str_replace(source, "_", "")
    source <- dplyr::tibble( source = rep(source, length(df$text)))
    # Bind news source column to data frame.
    df <- dplyr::bind_cols(df, source)
    # Cast created_at as Date instead of char.
    df$created_at <- as.Date(df$created_at, "%m/%d/%Y")
    df
}

files <- list.files(pattern = "*.csv")
tweets <- dplyr::tibble(created_at = as.Date(character()), 
                 text = character(),
                 source = character())

for (i in 1:length(files)) {
    df <- GetTweets(files[i])
    tweets <- dplyr::bind_rows(tweets,df)
}


news_tweets <- tweets[tweets$source != 'trump' & tweets$source != 'cspan',]

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://t.co/[A-Za-z\\d]+|&amp;|&lt;|&gt;|'"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
news_tweets <- news_tweets %>% 
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))


    
