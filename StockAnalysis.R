library(quantmod)
syms <- read.table("NYSE.txt",header = 2, sep = "\t")
smb <- grep("[A-Z]{4}",syms$Symbol,perl = F, value = T)
getSymbols(smb[1:10])
mat <- c()
stocks
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
col.names(stock.mat) <- index(temp2)

