library(topicmodels)
library(tidyverse)
library(stopwords)
library(tidytext)
library(schrute)

data <- schrute::theoffice

setwd('/home/schalk/kantar/sicss/R')

source("utils.R")

df <- data.frame(episode = data$episode_name, air_date = data$air_date, documents = data$text, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())

df <- aggregate(documents ~ air_date, data = df, FUN = paste, collapse = " ")

df$documents <- df$documents %>% as.character()

freq <- mutate(df, group = 1:n()) %>%
               unnest_tokens(input = documents, output = word, token = "words", to_lower = TRUE) %>%
               count(group, word) %>% 
               filter(word %ni% stopwords()) 

cnt <- freq %>% count(word)

cnt <- cnt[order(-cnt$n),]

dtm <- freq %>% 
        spread(key = word, value = n, fill = 0)

        