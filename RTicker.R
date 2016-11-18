library(httr)
library(dplyr)
source('DataFetch.R')

Sys.sleep(75*60)

res <- get.data()
sdata <- as.vector(res[[1]])
names <- res[[3]]
date <- res[[2]]
stockdata.ask <- data.frame(names,sdata)
colnames(stockdata.ask) <- c("Names",date)

sdata <- as.vector(res[[4]])
names <- res[[3]]
date <- res[[2]]
stockdata.real.time.ask <- data.frame(names,sdata)
colnames(stockdata.real.time.ask) <- c("Names",date)

ticker <- function(rest.time){
  res <- get.data()
  sdata <- as.vector(res[[1]])
  names <- res[[3]]
  date <- res[[2]]
  cols <- colnames(stockdata.ask)
  stockdata.ask <- cbind(stockdata.ask,sdata)
  colnames(stockdata.ask) <- c(cols,date)
  
  sdata <- as.vector(res[[4]])
  names <- res[[3]]
  date <- res[[2]]
  cols <- colnames(stockdata.real.time.ask)
  stockdata.real.time.ask <- cbind(stockdata.real.time.ask,sdata)
  colnames(stockdata.real.time.ask) <- c(cols,date)

  Sys.sleep(rest.time)
}

for(i in 1:80){
 ticker(5*60)  
}
save(stockdata.ask,stockdata.real.time.ask,file = "RealTimeData.rda")
