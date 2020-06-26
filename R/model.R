library(tidyverse)
library(glmnet)
library(plotly)

setwd('/home/schalk/kantar/sicss/R')

source("utils.R")

data <- readRDS('../data/model')

df <- select(data, -c('date', 'imdb_rating', 'hp_imdb_rating')) %>% crop() 

df <- cbind(df, select(data, c('date', 'imdb_rating')))

dfs <- select(df, -c('date'))

#Assign small positive real number to 0 entries to allow log transform. 
dfs[dfs == 0] <- 0.1*exp(-100)

#Take log transform. 
dfs <- log(dfs)

#Return adf stationarity indicator vector. 
adf <- adfdf(select(df, -c('date')))

#Take first difference for necessary series. 
dfs <- diff(dfs, adf)

#Omit NA's from lag columns in previous step.
dfs <- na.omit(dfs)

#Second adf run. 
adf <- adfdf(dfs)

#Remove series that are still non-stationary. 
dfs[,c(which(adf==0))]

dfs <- dfs[,-c(which(adf==0))]

dfs <- df.std(dfs) %>% as.data.frame()

dfs$date <- df$date[2:186]

#cross-validation from elastic net at alpha=0.01. Try grid search for optimal alpha. 
cv <- cv.glmnet(as.matrix(select(dfs, -c(date, imdb_rating))), y = dfs$imdb_rating, type.measure = "mse", nfolds = 10, alpha = 0.01)

#Retrieve coefficient values from cv object. 
coef <- coef(cv, s='lambda.min') %>% as.matrix() %>% as.data.frame()

coef <- tibble::rownames_to_column(coef,'characters')

colnames(coef) <- c('characters', 'coefficients')

coef <- coef[ order(-coef$coefficients),]

#Order coefficient values by size. 
xform <- list(categoryorder = "array",
              categoryarray = coef$characters)

plot_ly(coef, x = ~characters, 
  y = ~coefficients, 
  type = 'bar',
  #opacity = .5,
  marker = list(color = 'rgb(218,165,32)',
                      line = list(color = 'rgb(100,100,100)',
                                  width = 1.5))) %>% layout(xaxis = xform)

#Retrieve fitted values. 
y_hat <- predict(cv, newx = as.matrix(select(dfs, -c(date, imdb_rating))), s = 'lambda.min', gamma = 'gamma.min')

dfs$y_hat <- y_hat

#Plot fitted against actuals. 
series.plot(select(dfs, c(date, imdb_rating, y_hat)))