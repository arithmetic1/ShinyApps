rm(list=ls())
library("mvtnorm")
library("tseries")
library("quantmod")
library("PerformanceAnalytics")
library("fUnitRoots")
library("tseries")
library("R.utils")

#### Random NOISE ####
N = 100
e = rmvnorm(N, mean = 0) # qqline(e) # shows it's random noise: 

tickers <- c("VALE3", "NEE")
#symbol.path.A <- paste("http://ichart.finance.yahoo.com/table.csv?s=", "&ignore=.csv", sep=tickers[1])
#VALE.path <- read.csv(symbol.path.A, stringsAsFactors=F)
#VALE.dates <- as.Date(VALE.path[,1])
#VALE.price <- xts(VALE.path[,7], VALE.dates)
#VALE.volume <- xts(VALE.path[,6], VALE.dates)
#Axts <- merge.xts(VALE.price, VALE.volume)

# SE
symbol.path <- paste("http://ichart.finance.yahoo.com/table.csv?s=", "&ignore=.csv", sep=tickers[2])
SE.path <- read.csv(symbol.path, stringsAsFactors=F)
SE.dates <- as.Date(SE.path[,1])
SE.price <- xts(SE.path[,7], SE.dates)
SE.volume <- xts(SE.path[,6], SE.dates)
Bxts <- merge.xts(SE.price, SE.volume)


path <- paste("Tickers/", tickers[1], ".csv", sep = "")
A.df <- read.csv(path, header = FALSE, stringsAsFactors=T)
Axts <- xts(A.df[,4], as.Date(A.df[,1], format = "%m/%d/%Y"))

Axts.r <- (Axts[,1] - lag(Axts[,1],1))/lag(Axts[,1],1)
Bxts.r <- (Bxts[,1] - lag(Bxts[,1],1))/lag(Bxts[,1],1)

data.prices <- merge.xts(Axts[,1], Bxts[,1])
data.r <- merge.xts(Axts.r, Bxts.r)
#data.r[is.na(data.r)] <- 0
#data.prices[is.na(data.prices)] <- 0
data.prices <- na.omit(data.prices)
data.r <- na.omit(data.r)

colnames(data.prices) <- c(tickers[1], tickers[2])

x <- data.prices
rets <- data.r

estimate.OU <- function(data){
  fit = lm(data[,1] ~ data[,2])
  tstat = coef(summary(fit))[1,3]
  e = lm(data[,1] ~ data[,2])$residuals
  X_t = cumsum(e)
  f = lm(X_t[2:length(X_t)] ~ X_t[1:length(X_t)-1])
  
  
  return( c(coef(f)[1], coef(f)[2], var(f$residuals), tstat))
}

OU.parameters <- function (data.r){
  
  data.r <- data.volume
  
  data.c <- rollapply(data.r[625:nrow(data.volume)], roll.window, estimate.OU, by.column = FALSE)
  colnames(data.c) <- c('a', 'b', 'Var', 'tstat')
  data.r[is.na(data.r)] <- 0
  
  a = data.c$a[roll.window:nrow(data.c$a)]
  b = data.c$b[roll.window:nrow(data.c$b)]
  var.a = data.c$Var[roll.window:nrow(data.c$Var)]
  
  deltaT = 1/252
  kappa = -log(b)/deltaT
  m = a/(1-b)
  sigma = sqrt((var.a*2*kappa)/(1-b^2))
  sigma.eq = sqrt(var.a/(1-b^2))
  alpha <- data.c[,1]*(1/deltaT) 
  
  # coef.nam = coef(lm(data.volume[,1]~data.volume[,2]))
  #coef.nam = coef(fit)
  #resid <- data.volume[,1] - coef.nam[2]*data.volume[,2] - coef.nam[1]
  
  s =   -m/sigma.eq
  s.mod = s - (alpha/(kappa*sigma.eq))
  s.mod.new1 = (-a*sqrt(1-b^2))/((1-b)*sqrt(var.a))+mean(m)*(sqrt((1-b^2)/var.a))
  s.mod.new = s.mod.new1 - (alpha/(kappa*sigma.eq))
  
  par.output <- merge.xts(kappa, alpha, m, s, sigma, sigma.eq, s.mod, s.mod.new, data.c$tstat)
}

w.wo.drift <- function(window, data.v)
{
  data.v <- data.volume
  
  coef.nam <- rollapply(data.volume[625:nrow(data.volume)], roll.window, function(x) coef(lm(x[,1] ~ x[,2])), by.column = FALSE)
  resid <- data.volume[,1] - coef.nam[,2]*data.volume[,2] - coef.nam[,1]
  spread <- cumsum(na.omit(resid))
  
  s.so <- 2
  s.bc <- 0.75
  s.sc <- 0.50
  
  # with drift
  buy.open.drift = ou.parameters$s.mod < -s.so
  sell.open.drift = ou.parameters$s.mod > s.so
  close.short.drift = ou.parameters$s.mod < s.bc
  close.long.drift = ou.parameters$s.mod > -s.sc
  
  sell.open.drift[is.na(sell.open.drift)] <- 0
  close.long.drift[is.na(close.long.drift)] <- 0
  close.short.drift[is.na(close.short.drift)] <- 0
  buy.open.drift[is.na(buy.open.drift)] <- 0
  
  #without drift
  
  buy.open.wo = ou.parameters$s.mod.new < -s.so
  sell.open.wo = ou.parameters$s.mod.new > s.so
  close.short.wo = ou.parameters$s.mod.new < s.bc
  close.long.wo = ou.parameters$s.mod.new > -s.sc
  
  sell.open.wo[is.na(sell.open.wo)] <- 0
  close.long.wo[is.na(close.long.wo)] <- 0
  close.short.wo[is.na(close.short.wo)] <- 0
  buy.open.wo[is.na(buy.open.wo)] <- 0
  
  cappedCumSum <- function(x, y,max_value,min_value) max(min(x + y, max_value), min_value)
  shortPositionFunc <- function(x,y) { cappedCumSum(x,y,0,-1) }
  longPositionFunc <- function(x,y) { cappedCumSum(x,y,1,0) }
  
  # w/ drift
  shortPositions.drift <- Reduce(shortPositionFunc, -1*matrix(sell.open.drift) + matrix(close.short.drift), accumulate=TRUE)
  longPositions.drift <- Reduce(longPositionFunc,matrix(buy.open.drift)-matrix(close.long.drift), accumulate=TRUE)
  
  # wo/ drift
  shortPositions.wo <- Reduce(shortPositionFunc, -1*matrix(sell.open.wo) + matrix(close.short.wo), accumulate=TRUE)
  longPositions.wo <- Reduce(longPositionFunc,matrix(buy.open.wo)-matrix(close.long.wo), accumulate=TRUE)
  
  conservative.possible.pnl <- (lag(data.prices[,1],2) - lag(data.prices[,1],1)) - coef.nam[,2]*(lag(data.prices[,2],2) - lag(data.prices[,2],1))
  
  short.val.drift <- lag(conservative.possible.pnl,0)*shortPositions.drift
  short.val.drift[is.na(short.val.drift)] <- 0
  long.val.drift <- lag(conservative.possible.pnl,1)*longPositions.drift
  long.val.drift[is.na(long.val.drift)] <- 0
  trade.vals.drift <- (long.val.drift + short.val.drift)
  trade.vals.drift[is.na(trade.vals.drift)] <- 0
  
  short.val.wo <- lag(conservative.possible.pnl,1)*shortPositions.wo
  short.val.wo[is.na(short.val.wo)] <- 0
  long.val.wo <- lag(conservative.possible.pnl,1)*longPositions.wo
  long.val.wo[is.na(long.val.wo)] <- 0
  trade.vals.wo <- (long.val.wo + short.val.wo)
  trade.vals.wo[is.na(trade.vals.wo)] <- 0
  
  output = list('drift' = trade.vals.drift, 'none' = trade.vals.wo)
  return(output) 
}

f.get.volume <- function(x, roll.window){
  
  VolumeCsv <- read.csv("Cleanup/DailyReturns_IBX__CSHTRD - Trading Volume - Daily.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
  
  A.vol <- xts(VolumeCsv[tickers[1]], as.Date(VolumeCsv$Date, format = "%m/%d/%Y"))
  B.vol <- Bxts[,2]
  rets <- data.r
  
  start.idx = max(which(!is.na(Bxts[,2]))[1], which(!is.na(A.vol))[1])
  
  VOL = merge.xts(A.vol[start.idx:nrow(A.vol)],B.vol[start.idx:nrow(B.vol)])
  
  bad.idx = which(as.logical(rowSums((is.na(VOL)))))
  keep.idx = seq(nrow(VOL))[ !(seq(nrow(VOL)) %in% bad.idx) ]
  VOL = VOL[keep.idx,]
  
  VOL.mean = rollapply(VOL, roll.window, mean)
  
  as.numeric(x[nrow(x[,1]),]) / as.numeric(x[1,]) - 1
  (cumprod(1+rets)-1)[nrow(rets),]
  
  mult.of.avg.daily.volume = VOL.mean / VOL   
  mod.rets = rets * mult.of.avg.daily.volume
  data.volume <- mod.rets
  data.volume <- na.omit(data.volume)
}

roll.window = 20

data.volume <- f.get.volume(x, roll.window)

ou.parameters <- OU.parameters(data.volume)
colnames(ou.parameters) <- c('kappa', 'alpha', 'm', 's', 'sigma', 'sigma.eq', 's.mod', 's.mod.new', 'tstat')

trade.vals.20 <- w.wo.drift(roll.window, data.volume)
chart.TimeSeries(cumsum(trade.vals.20$none))
chart.TimeSeries(cumsum(trade.vals.20$drift))


hedge.ratio <- rollapply(data.prices, roll.window, function(x) coef(lm(x[,2] ~ x[,1] + 0)), by.column = FALSE)
hedge.ratio[is.na(hedge.ratio)] <- 0
bt.spread <- data.prices[,1] - hedge.ratio * data.prices[,2]
z.score.og <- rollapply(bt.spread, roll.window, function(x) (tail(x, 1) - mean(x))/stdev(x))
z.score.og[is.na(z.score.og)] <- 0
z.score.og.signal <- ifelse(z.score.og < -1.25, 1, 0) + ifelse(z.score.og > 1.25, -1, 0)
conservative.pnl <- (lag(data.prices[,1],2) - lag(data.prices[,1],1)) - hedge.ratio*(lag(data.prices[,2],2) - lag(data.prices[,2],1))
conservative.pnl[is.na(conservative.pnl)] <- 0
trade.vals1 <- cumsum(conservative.pnl*z.score.og.signal)
chart.TimeSeries(trade.vals1)

#all.output <- na.omit(merge.xts(data.prices[,1], data.prices[,2], data.r[,1], data.r[,2], A.vol, Bxts[,2]))
#colnames(all.output) <- c('VALE3', 'SE', 'VALE.RETURNS', 'SE.RETURNS', 'VALE.VOLUME', 'SE.VOLUME')
#backtest <- write.zoo(all.output, file = "Unittest-SE-VALE3.csv", sep = ",")
