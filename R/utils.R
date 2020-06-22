'%ni%' <- Negate('%in%')

standardise <- function(x){
  s <- (x-mean(x))/sd(x)
  return(s)
}

df.std <- function(df){
  apply(df, 2, function(x){
    z <- as.numeric(x)
    standardise(z)
    }
  )
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

cormat <- function(x){
  corr <- round(cor(x), 1)
  p.mat <- cor_pmat(x)
  corr.plot <- ggcorrplot(
        corr, hc.order = TRUE, type = "lower", outline.col = "white",
        p.mat = p.mat
        )
  obj <- ggplotly(corr.plot)
  return(obj)
}

corvec <- function(x, y){
  cc <- apply(y, 2, function(i){
    obj <- cor.test(x, i, method = 'pearson', conf.level=0.9)
    mean <- obj$estimate
    int <- obj$estimate - obj$conf.int[1]
    sig <- ifelse(obj$p.value<0.05, 1, 0)
    c(mean, int, sig)
  })
  cc <- t(cc)
  colnames(cc) <- c("coefficient", "interval", "sig")
  p <- plot_ly(data = as.data.frame(cc), x = row.names(cc), y = ~coefficient, type = 'scatter', mode = 'markers', error_y = ~list(array = interval, color = '#000000'), 
  opacity = 0.5,
  marker = list(
      color = ~I(1-sig),
      size = 50,
      line = list(
        color = 'rgb(0,0,0)',
        width = 5
      )
    ))
return(p)
}

adfdf <- function(df){
  pv <- apply(df, 2, function(x){
    tseries::adf.test(x)$p.value
      }
    )
  ind <- ifelse(pv<0.05 & is.na(pv)==FALSE, 1, 0) %>% as.vector
  return(ind)
}

crop <- function(df){
  cs <- colSums(df) %>% as.vector()
  ind <- which(cs < 0.1*mean(cs))
  return(df[,-ind])
}

diff <- function(df, adf){
  ind <- which(adf==0)
  temp <- df[,c(ind)]
  temp <- apply(temp, 2, function(x){
    x-lag(x)
   }
  )
  df <- df[,-c(ind)]
  df <- cbind(df, temp)
  return(df)
}

elnet.best <- function(caret_fit) {
  best <- which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result <- caret_fit$results[best, ]
  rownames(best_result) <- NULL
  return(best_result)
}