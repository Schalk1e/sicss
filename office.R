install.packages('schrute', repos='http://cran.r-project.org')
install.packages('plotly', repos='http://cran.r-project.org')
install.packages('forecast', repos='http://cran.r-project.org')
install.packages('mFilter', repos='http://cran.r-project.org')

library(forecast)
library(mFilter)
library(schrute)
library(plotly)
library(dplyr)


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

#Distribution of ratings.
p <- plot_ly(ts, x = ~imdb_rating, type='histogram', histnorm = 'probability')
p

#ratings over time.
p <- plot_ly(ts, x = ~air_date, y = ~imdb_rating, type='scatter', mode='lines')
p

#very interesting underlying cyclic structure. How can we visualise? Let's try to smooth...
smth <- ses(ts$imdb_rating)
ts$ses_imdb_rating <- smth$fitted #Uses window function and doesn't coincide with contemporary spikes in raw data. Try different approach. 

p <- plot_ly(ts) %>% 
  add_lines(x = ~air_date, y = ~imdb_rating, type='scatter', mode='lines') %>%
  add_lines(x = ~air_date, y = ts$ses_imdb_rating)
p

hp <- hpfilter(ts$imdb_rating,freq=100,type=c("lambda"),drift=FALSE)

ts$hp_imdb_rating <- as.numeric(hp$trend)

p <- plot_ly(ts) %>% 
  add_lines(x = ~air_date, y = ~imdb_rating, type='scatter', mode='lines') %>%
  add_lines(x = ~air_date, y = ts$ses_imdb_rating) %>% 
  add_lines(x=~air_date, y=~hp_imdb_rating, type='scatter', mode='lines')
p



#Graph distribution of imdb_ratings.
standardise <- function(x){
  s <- (x-mean(x))/sd(x)
  return(s)
}

data$std_imdb_rating <- standardise(data$imdb_rating)

#--------------------------------------





