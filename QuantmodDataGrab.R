library(quantmod)
syms <- read.table("NYSE.txt",header = 2, sep = "\t")
smb <- grep("[A-Z]{4}",syms$Symbol,perl = F, value = T)
getSymbols(smb)
mat <- c()
stocks <- c()
stockList <- list()
names <- c()
for(i in 1:length(smb)){
  temp <- get(smb[i])
  names <- c(names,smb[i])
  stockList[[i]] <- as.numeric(getPrice(temp))
  len <- length(attributes(temp)$index)
  if(len < 1001) next
  stocks <- c(stocks,smb[i])
  temp2 <- temp[(len - 1000):len]
  vex <- as.numeric(getPrice(temp2))
  mat <- rbind(mat,vex)
}

names(stockList) <- names
stock.mat <- as.matrix(mat)
row.names(stock.mat) <- stocks
colnames(stock.mat) <- as.character(index(temp2))
save(stock.mat,stockList,file = "StockData.rda")
rm(list = ls())