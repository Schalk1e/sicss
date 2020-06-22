library(ggcorrplot)
library(heatmaply)
library(tidyverse)
library(forecast)
library(mFilter)
library(tseries)
library(schrute)
library(plotly)

setwd('/home/schalk/kantar/sicss/R')

source("utils.R")

data <- schrute::theoffice

ts <- select(data, c('air_date', 'imdb_rating')) %>% unique()

colnames(ts) <- c('date','imdb_rating')

series.plot(ts)

smth <- ses(ts$imdb_rating)

ts$ses_imdb_rating <- smth$fitted 

series.plot(ts)

smth <- ses(ts$imdb_rating, alpha = 0.08)

ts$ses2_imdb_rating <- smth$fitted

series.plot(ts)

hp <- hpfilter(ts$imdb_rating,freq=100,type=c("lambda"),drift=FALSE)

ts$hp_imdb_rating <- as.numeric(hp$trend)

series.plot(ts)

ts$hpc_imdb_rating <- hp$cycle

ts <- select(ts, -c(ses_imdb_rating, ses2_imdb_rating))

series.plot(ts)

ts$hp1600_imdb_rating <- hpfilter(ts$imdb_rating,freq=1600,type=c("lambda"),drift=FALSE)$trend %>% as.numeric()

ts$hp50_imdb_rating <- hpfilter(ts$imdb_rating,freq=50,type=c("lambda"),drift=FALSE)$trend %>% as.numeric()

series.plot(select(ts, -c(hpc_imdb_rating)))

ts <- select(ts, -c(hp50_imdb_rating, hp1600_imdb_rating))

main <- readRDS('../data/characters') 

data$character <- as.character(data$character)

data[c(which(data$character=='Michel')),]$character <- "Michael"

data[c(which(data$character=='JIm')),]$character <- "Jim"

char <- data %>% 
           group_by(air_date, episode_name, character) %>% 
           count()

char <- char %>% spread(key = character, value = n, fill = 0)

char <- as.data.frame(char)

ts <- cbind(ts, select(char, c(main)))

series.plot(ts[,-c(2,3,4)]) 

smth <- apply(ts[,-c(1,2,3,4)], 2, function(x){
        tmp <- hpfilter(x,freq=100,type=c("lambda"),drift=FALSE)$trend 
}) %>% as.data.frame()

smth$date <- ts$date

series.plot(smth)

cormat(ts[,-c(1,2,3,4)])

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

ts <- select(ts, -c('hpc_imdb_rating'))

cc <- corvec(ts$imdb_rating, select(ts, -c(date, imdb_rating, hp_imdb_rating)))
cc

df <- cbind(select(ts, c(date, imdb_rating, hp_imdb_rating)), select(char, -c(air_date, episode_name)))

saveRDS(df, '../data/model')