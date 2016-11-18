library(quantmod)
syms <- read.table("NYSE.txt",header = 2, sep = "\t")
smb <- grep("[A-Z]{4}",syms$Symbol,perl = F, value = T)
getSymbols(smb)
mat <- c()
stocks <- c()
for(i in 1:length(smb)){
  temp <- get(smb[i])
  len <- length(attributes(temp)$index)
  if(len < 1001) next
  stocks <- c(stocks,smb[i])
  temp2 <- temp[(len - 1000):len]
  vex <- as.numeric(getPrice(temp2))
  mat <- rbind(mat,vex)
}
quant
stock.mat <- as.matrix(mat)
row.names(stock.mat) <- stocks
colnames(stock.mat) <- as.character(index(temp2))
save(stock.mat,file = "StockData.rda")
rm(list = ls())
library(mclust)
library(vars)

cl <- Mclust(stock.mat,G = 1:20)
stock.mat <- cbind(stock.mat,cl$classification)




library(ggplot2)
load("StockData.rda")
cost_df <- data.frame()
for(i in 1:100){
  kmeans<- kmeans(x=stock.mat, centers=i, iter.max=100)
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
ggplot(data=cost_df, aes(x=cluster, y=cost)) +
  geom_line(colour = "darkgreen") +
  ggtitle("Reduction In Cost For Values of 'k'n") +
  xlab("nClusters") +
  ylab("Within-Cluster Sum of Squares n")

kmeans<- kmeans(x=stock.mat, centers=50, iter.max=100)
stock.mat <- cbind(stock.mat,kmeans$cluster)
table(kmeans$cluster)
cluster.eleven <- stock.mat[stock.mat[,1002] == 11,1:1001]
ts.eleven <- ts(t(cluster.eleven))
fit <- VAR(ts.eleven[1:(1001 - 56),], p = 56)
preds <- predict(fit,n.ahead = 56)
forecast <- preds$fcst$FSCE
plot.ts(ts.eleven[900:1001,1],ylim = c(20,30))
lines(y = forecast[,1], x = (100 - 55):100,col = "blue")
lines(y = forecast[,2], x = (100 - 55):100,col = "red",lty=2)
lines(y = forecast[,3], x = (100 - 55):100,col = "red",lty=2)

fit <- VAR(ts.eleven[1:(1001 - 14),], p = 14)
preds <- predict(fit,n.ahead = 14)
forecast <- preds$fcst$FSCE
plot.ts(ts.eleven[950:1001,1],ylim = c(24,27))
lines(y = forecast[,1], x = (50 - 13):50,col = "blue")
lines(y = forecast[,2], x = (50 - 13):50,col = "red",lty=2)
lines(y = forecast[,3], x = (50 - 13):50,col = "red",lty=2)


