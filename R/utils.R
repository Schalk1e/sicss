'%ni%' <- Negate('%in%')

standardise <- function(x){
  s <- (x-mean(x))/sd(x)
  return(s)
}

series.plot <- function(dataframe){
  df <- dataframe
  p <- plot_ly()
  palette <- c('rgb(218,165,32)', 'rgb(0,0,0)', 'rgb(100,100,100)', 'rgb(137,207,240)', 'rgb(128,128,128)') 
  colNames <- colnames(df[, !(colnames(df) %in% "date")])
  i=1
  for (trace in colNames){
    p <- p %>% add_trace(data=df, x = ~date, y = as.formula(paste0("~`", trace, "`")), opacity=0.6, name = trace, type = 'scatter', mode = 'lines', line = list(color = palette[i], width = 4))
    i=i+1
  }
  return(p)
}

hist.plot <- function(dataframe){
  df <- dataframe
  p <- plot_ly()
  colNames <- colnames(df[, !(colnames(df) %in% "date")])
  i=1
  for (trace in colNames){
    p <- p %>% add_histogram(data=df, x = as.formula(paste0("~`", trace, "`")), opacity=0.6, name = trace)
    i=i+1
  }
  p <- p %>% layout(barmode = "overlay")
  return(p)
}

