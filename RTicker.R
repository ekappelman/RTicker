library(httr)
library(dplyr)
library(jsonlite)
source('DataFetch.R')

Sys.sleep(110*60)

frame.list <- list()

ticker <- function(rest.time){
  df <- data.frame(get.data(),Date = date())
  frame.list <- c(list(df),frame.list)
  Sys.sleep(rest.time)
}

for(i in 1:80){
 ticker(5*60)  
}
save(frame.list,file = "RealTimeData.rda")
