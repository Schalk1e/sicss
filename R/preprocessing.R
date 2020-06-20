library(topicmodels)
library(ggcorrplot)
library(stopwords)
library(heatmaply)
library(tidyverse)
library(tidytext)
library(forecast)
library(mFilter)
library(tseries)
library(schrute)
library(plotly)

setwd('/home/schalk/kantar/sicss/R')

source("utils.R")

data <- schrute::theoffice

#Exploration
#--------------------------------------
# colnames(data)
# summary(data$imdb_rating)
# unique(data$imdb_rating)
# head(data$text_w_direction)
# head(data$text)
# unique(data$episode)
# unique(data$air_date)
# unique(data$episode_name)
# scores <- select(data, c('episode_name', 'imdb_rating'))
# nrow(unique(scores))==length(unique(data$episode_name))
#TRUE can group by episode / imdb_rating. 

ts <- select(data, c('air_date', 'imdb_rating')) %>% unique()

colnames(ts) <- c('date','imdb_rating')

#ratings over time.
series.plot(ts)

#very interesting underlying cyclic structure. How can we visualise? Let's try to smooth...

###########################
#       FILTERING         #
###########################

smth <- ses(ts$imdb_rating)

ts$ses_imdb_rating <- smth$fitted #Uses window function and doesn't coincide with contemporary spikes in raw data. Try different approach. 

series.plot(ts)

hp <- hpfilter(ts$imdb_rating,freq=100,type=c("lambda"),drift=FALSE)

ts$hp_imdb_rating <- as.numeric(hp$trend)

series.plot(ts)

ts$hpc_imdb_rating <- hp$cycle

select(ts, -c('ses_imdb_rating')) %>% series.plot()

###########################
#       CHARACTERS        #
###########################

#Perhaps a particular character can be associated with more / less popular episodes? 
main <- readRDS('data/characters')

#Change name spelling errors.
data$character <- as.character(data$character)

data[c(which(data$character=='Michel')),]$character <- "Michael"

data[c(which(data$character=='JIm')),]$character <- "Jim"

#We note there is more progress to be made by refning this logic. 
char <- data[c(which(data$character %in% main)),] %>% 
           group_by(air_date, episode_name, character) %>% 
           count()

char <- char %>% spread(key = character, value = n, fill = 0)

char <- as.data.frame(char)

ts <- cbind(ts, select(char, -c('episode_name', 'air_date')))

series.plot(ts[,-c(2,3,4,5)]) #Interesting, can get a sense of which characters speak the most. Let's try to get a sense of the underlying correlation structure in this data...

corr <- round(cor(ts[,-c(1,2,3,4,5)]), 1)

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(ts[,-c(1,2,3,4,5)])

# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
  )
ggplotly(corr.plot)

#Smooth each... A lot of variation. 
smth <- apply(ts[,-c(1,2,3,4,5)], 2, function(x){
        tmp <- hpfilter(x,freq=100,type=c("lambda"),drift=FALSE)$trend 
}) %>% as.data.frame()

smth$date <- ts$date

series.plot(smth)

#What is the relationship between michael's absence and ratings? 
#First step would be visual inspection - however, we cannot directly compare these two series as they are conflicting measures. We can remedy this by standardising both and displaying on the same axes. 

std <- apply(cbind(ts$hp_imdb_rating, smth$Michael), 2, function(x){
        tmp <- standardise(x)
}) %>% as.data.frame()

std$date <- ts$date

colnames(std) <- c('imdb', 'Michael','date')

series.plot(std)

std <- cbind(std, apply(select(smth, -"date"), 2, function(x){
        tmp <- standardise(x)
}))

series.plot(select(std, c("date", "imdb", "Michael", "Dwight", "Jim", "Pam")))

ts <- select(ts, -c('ses_imdb_rating', 'hpc_imdb_rating'))



##########################
#         TEXT?          #
##########################

#Need to process the text associated with each episode in some meaningful way! 
#World is your oyster here. 
#From tokenisation to topic extraction - any number of approaches could yield meaningful results. And the more time you spend the greater the insights. For now - a simple approach. 

#Word Frequency... 

#most_freq_words <- c('Free', 'New', 'Limited', 'Offer')

df <- data.frame(episode = data$episode_name, air_date = data$air_date, documents = data$text, row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = default.stringsAsFactors())

df <- aggregate(documents ~ air_date, data = df, FUN = paste, collapse = " ")

df$documents <- df$documents %>% as.character()

#Remove stopwords, punctuation, and stem if poss

freq <- mutate(df, group = 1:n()) %>%
               unnest_tokens(input = documents, output = word, token = "words", to_lower = TRUE) %>%
               count(group, word) %>% 
               filter(word %ni% stopwords()) #Need to filter out words here...

cnt <- freq %>% count(word)
cnt <- cnt[order(-cnt$n),]

#threshold <- 0.1

#most_freq_words <- cnt[1:round(threshold*nrow(cnt)),]$word

dtm <- freq %>% 
        #filter(word %in% most_freq_words) %>%
        spread(key = word, value = n, fill = 0)


