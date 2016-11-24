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
rm(list = ls())