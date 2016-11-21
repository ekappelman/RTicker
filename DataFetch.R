parse.data <- function(symbols,range){
  base.URL <- "http://finance.google.com/finance/info?client=ig&q="
  start = min(range)
  end = max(range)
  symbol.string <- paste0("NYSE:",symbols[start],",")
  for(i in (start+1):end){
    temp <- paste0("NYSE:",symbols[i],",")
    symbol.string <- paste(symbol.string,temp,sep="")
  }
  URL <- paste(base.URL,symbol.string,sep="")
  
  data <- GET(URL)
  now <- date()
  bin <- content(data, "raw")
  writeBin(bin, "data.txt")
  
  conn <- file("data.txt",open="r")
  linn <-readLines(conn)
  jstring <- "["
  for (i in 3:length(linn)){
    jstring <- paste0(jstring,linn[i])
  }
  close(conn)
  file.remove("data.txt")
  obj <- fromJSON(jstring)
  
  return(data.frame(Symbol = obj$t,Price = as.numeric(obj$l)))
}

get.data <- function(){
syms <- read.csv("NYSE.txt",header = 2, sep = "\t")
sb <- grep("[A-Z]{4}|[A-Z]{3}",syms$Symbol,perl = F, value = T)
result <- c()
in.list <- list()
list.seq <- seq(1,2901,100)
  for(i in 1:(length(list.seq)-1)){
    range <- list.seq[i]:list.seq[i+1]
    result <- rbind(result,parse.data(sb,range))
  }
return(droplevels.data.frame(na.omit(result)))
}

