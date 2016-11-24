load("StockData.rda")
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
rm(list = ls())