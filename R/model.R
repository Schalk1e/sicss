library(tidyverse)
library(glmnet)
library(plotly)

setwd('/home/schalk/kantar/sicss/R')

source("utils.R")

data <- readRDS('../data/model')

df <- select(data, -c('date', 'imdb_rating', 'hp_imdb_rating')) %>% crop() 

df <- cbind(df, select(data, c('date', 'imdb_rating')))

dfs <- select(df, -c('date'))

dfs[dfs == 0] <- 0.1*exp(-100)

dfs <- log(dfs)

adf <- adfdf(select(df, -c('date')))

dfs <- diff(dfs, adf)

dfs <- na.omit(dfs)

adf <- adfdf(dfs)

dfs[,c(which(adf==0))]

dfs <- dfs[,-c(which(adf==0))]

dfs <- df.std(dfs) %>% as.data.frame()

dfs$date <- df$date[2:186]

cv <- cv.glmnet(as.matrix(select(dfs, -c(date, imdb_rating))), y = dfs$imdb_rating, type.measure = "mse", nfolds = 10, alpha = 0.05)

coef <- coef(cv, s='lambda.min') %>% as.matrix() %>% as.data.frame()

coef <- tibble::rownames_to_column(coef,'characters')

colnames(coef) <- c('characters', 'coefficients')

coef <- coef[ order(-coef$coefficients),]

xform <- list(categoryorder = "array",
              categoryarray = coef$characters)

plot_ly(coef, x = ~characters, 
  y = ~coefficients, 
  type = 'bar',
  #opacity = .5,
  marker = list(color = 'rgb(218,165,32)',
                      line = list(color = 'rgb(100,100,100)',
                                  width = 1.5))) %>% layout(xaxis = xform)

y_hat <- predict(cv, newx = as.matrix(select(dfs, -c(date, imdb_rating))), s = 'lambda.min', gamma = 'gamma.min')

dfs$y_hat <- y_hat

series.plot(select(dfs, c(date, imdb_rating, y_hat)))