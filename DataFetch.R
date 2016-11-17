get.data <- function(){
syms <- read.table("NYSE.txt",header = 2, sep = "\t")
base.URL <- "http://finance.yahoo.com/d/quotes.csv?s="
sb <- grep("[A-Z]{4}|[A-Z]{3}",syms$Symbol,perl = F, value = T)
symbol.string <- syms$Symbol[1]
for(i in 2:length(syms$Symbol)){
  symbol.string <- paste(symbol.string,syms$Symbol[i],sep="+")
}
URL <- paste(base.URL,symbol.string,"&f=nab2",sep="")

data <- GET(URL)
now <- date()
bin <- content(data, "raw")
writeBin(bin, "data.csv")
price.data <- read.csv("data.csv",header = FALSE,col.names = c("Name","Ask","Real.Time.Ask"))
file.remove("data.csv")
price.data <- price.data[price.data$Name != "N/A",]
return(list(price.data$Ask,now,price.data$Name,price.data$Real.Time.Ask))
}