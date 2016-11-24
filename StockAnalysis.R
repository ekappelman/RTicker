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

library(mclust)
library(vars)
library(ggplot2)
load("StockData.rda")

cl <- Mclust(stock.mat,G = 1:9)
stock.mat <- cbind(stock.mat,cl$classification)
table(cl$classification)


cluster <- stock.mat[stock.mat[,1002] == 6,1:1001]
ts <- ts(t(cluster))
fit <- VAR(ts[1:(1001-10),],p = 10)
preds <- predict(fit, n.ahead = 10)
forecast <- preds$fcst$TEVA
plot.ts(ts[950:1001,8],ylim=c(36,54))
lines(y = forecast[,1], x = (50 - 9):50,col = "blue")
lines(y = forecast[,2], x = (50 - 9):50,col = "red",lty=2)
lines(y = forecast[,3], x = (50 - 9):50,col = "red",lty=2)

for(i in 1:8){
  assign(paste0("cluster.",i),stock.mat[stock.mat[,1002] == i,1:1001])
  assign(paste0("ts.",i),ts(t(get(paste0("cluster.",i)))))
  temp <- get(paste0("ts.",i))
  assign(paste0("fit.",i),VAR(temp, p = 10))
  assign(paste0("preds.",i),predict(get(paste0("fit.",i)),n.ahead = 10))
}

stock.mat <- cbind(stock.mat,0)
for(j in 1:8){
  pred.vec <- c()
  temp <- get(paste0("preds.",j))
  for(i in temp$fcst){
    cast <- temp$fcst[1]
    cast <- cast[[1]]
    cast <- cast[10,]
    pred.vec <- c(pred.vec,cast[1])
  }
  stock.mat[stock.mat[,1002] == j,1003] <- pred.vec
}

stock.mat <- stock.mat[stock.mat[,1002] != 9,]
stock.mat[,1004] <- (stock.mat[,1003] - stock.mat[,1001]) / stock.mat[,1001] * 100
stock.mat <- stock.mat[order(-stock.mat[,1004]),]
stock.mat[1:10,1004]

library(forecast)
forecasts <- c()
names <- c()
for(i in 1:length(stockList)){
  mod <- auto.arima(stockList[[i]])
  cast <- forecast(mod)
  cast <- cast$mean[10]
  temp <- c(as.numeric(stockList[[i]][length(stockList[[i]])]), as.numeric(cast))
  forecasts <- rbind(forecasts,temp)
  names <- c(names,names(stockList[i]))
}
forecasts <- matrix(ncol = 2,forecasts)
forecasts <- cbind(forecasts,(forecasts[,2] - forecasts[,1])/forecasts[,1] * 100)
colnames(forecasts) <- c("Price","Forecast","% Change")
row.names(forecasts) <- names
forecasts <- forecasts[order(-forecasts[,3]),]
