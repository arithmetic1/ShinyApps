#!/usr/bin/Rscript
rm(list=ls())
library(tis)

f.get.hopey.quote.data <- function(sym, date, exchange, write.data=FALSE, dir="~/StrategyResearch/LETF/Data/Hopey/") {
  if (exchange=="AMEX") {
    exch <- 'A'
  } else if (exchange=="NYSE") {
    exch <- 'N'
  } else if (exchange=="NASDAQ") {
    exch <- 'O'
  } else {
    cat("Unknown Exchange")
    return 
  }
  link <- paste("http://hopey.netfonds.no/posdump.php?date=",date,"&paper=",sym, ".",exch, "&csv_format=csv", sep="")
  data<-read.csv(link)
  idx<-strptime(as.character(data$time),format="%Y%m%dT%H%M%S")
  idx<-as.POSIXct(idx, tz="Europe/Oslo")
  attr(idx, "tzone") <- "America/New_York"
  out <- xts(data[,-1], idx)
  
  if(write.data) {
    fname <- paste(sym, "_", date, ".csv", sep="")
    path <- paste(dir, fname, sep="")
    
    out.df <- data.frame(datetime=as.character(index(out)), coredate(out))
    write.csv(out.df, path, row.names=F)
  }
  
  out
}

f.get.LETF.data <- function(ticker1, ticker2, lev1, lev2, date) {
  t1<-f.get.hopey.quote.data(ticker1, date, "AMEX")
  t1$mid <- (t1$bid + t1$offer) / 2
  
  t2<-f.get.hopey.quote.data(ticker2, date, "AMEX")
  t2$mid <- (t2$bid + t2$offer) / 2
  
  getSymbols(c(ticker1, ticker2))
  
  date.dt <- strptime(as.character(date), '%Y%m%d')
  pbdate <- strptime(previousBusinessDay(date), '%Y%m%d')
  
  t1.close <- as.numeric(Ad(get(ticker1)[pbdate]))
  t2.close <- as.numeric(Ad(get(ticker2)[pbdate]))
  
  date.use <- paste(date.dt, "T09:30:00/", date.dt, "T16:00:00", sep="")
  data <- na.locf(merge((t1$mid/t1.close - 1)/lev1, (t2$mid/t2.close-1)/lev2))[date.use]
  colnames(data) <- c(ticker1, ticker2)
  data
} 