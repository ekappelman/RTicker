library(httr)
library(dplyr)
library(jsonlite)
source('DataFetch.R')

Sys.sleep(55*60)

frame.list <- list()

ticker <- function(rest.time){
  ptm <- proc.time()
  df <- data.frame(get.data(),Date = date())
  timer.time <- proc.time() - ptm
  Sys.sleep(as.numeric(rest.time - timer.time[3]))
  return(list(df))
}

for(i in 1:80){
  frame.list <- c(suppressWarnings(ticker(5 * 60)),frame.list)
}
save(frame.list,file = "RealTimeData.rda")
