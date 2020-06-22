library(tidyverse)
library(starnet)

setwd('/home/schalk/kantar/sicss/R')

source("utils.R")

data <- readRDS('../data/model')

df <- select(data, -c('date', 'imdb_rating', 'hp_imdb_rating')) %>% crop() 

df <- cbind(df, select(data, c('date', 'imdb_rating')))

adf <- adfdf(select(df, -c('date')))

