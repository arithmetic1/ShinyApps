#!/usr/bin/Rscript
source("~/StrategyResearch/LETF/f.get.hopey.data.R")
library("PerformanceAnalytics")

date <- as.numeric(strftime(Sys.Date(), '%Y%m%d'))

d<-f.get.LETF.data("SPY", "UPRO", 1, 3, date)
err.xts <- d$UPRO - d$SPY
colnames(err.xts) <- "h.noise"
chart.TimeSeries(err.xts)

#Create trade region as a funciton of position size

x <- c(1000, 0, -1000) #Shares 

bid.y<-c(mean(err.xts)-3*sd(err.xts), mean(err.xts)-2*sd(err.xts), mean(err.xts)-sd(err.xts)) #Define Bid Region
mod.bid <- lm(bid.y~x) 

ask.y<-c(mean(err.xts)+sd(err.xts), mean(err.xts)+2*sd(err.xts), mean(err.xts)+3*sd(err.xts)) #Define Ask Region
mod.ask <- lm(ask.y~x)