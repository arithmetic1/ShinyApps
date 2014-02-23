# http://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf
get_yhoo_data <- function(ticker){
  symbol.path <- paste("http://ichart.finance.yahoo.com/table.csv?s=", "&ignore=.csv", sep=ticker)
  data <- read.csv(symbol.path, stringsAsFactors=F)
  dates <- as.Date(data[,1])
  data.zoo <- zoo(data[,2:7], dates)  # drop dates
  adj = data.zoo$Adj.Close / data.zoo$Close
  data.zoo$Adj.Open = data.zoo$Open * adj
  ## 
  data.zoo$day.ret = (data.zoo$Adj.Close - data.zoo$Adj.Open) / data.zoo$Adj.Open
  data.zoo$ovn.ret = (data.zoo$Adj.Open - lag(data.zoo$Adj.Close,-1)) / lag(data.zoo$Adj.Close,-1)
  data.zoo$prior.ret = (data.zoo$Adj.Open - lag(data.zoo$Adj.Close,-1)) / lag(data.zoo$Adj.Close,-1)
  # forward.ret
  return(data.zoo)
}

get_prior_day_return <- function(data.zoo){
  returns.notl <- data.zoo - lag(data.zoo,-1)
  returns.prct <- returns.notl / lag(data.zoo,-1)
  return(returns.prct)
}

# plot_autocorrel(arima.sim(list(order=c(5,0,0), ar=coef(arima(-sso-sds,order=c(5,0,0)))[1:5]),sd=0.0003, n=1000)+0.0001)
# http://people.duke.edu/~rnau/arimrule.htm
# http://www.le.ac.uk/users/dsgp1/COURSES/TSERIES/8IDNTIFY.PDF
# http://www.stat.pitt.edu/stoffer/tsa2/Rissues.htm
# http://people.duke.edu/~rnau/411arim3.htm
# http://www.albany.edu/faculty/faugere/PhDcourse/meanreversion1.pdf

sds = get_prior_day_return(get_yhoo_data('SDS')$Adj)
sso = get_prior_day_return(get_yhoo_data('SSO')$Adj)



window = 200
cd = coredata(-sso-sds)
N = length(cd)
foo = matrix()
for (idx in seq(window,N,1)){
  foo[idx] = predict(arima(cd[(idx-window+1):idx],order=c(2,0,0)),1)$pred
}

strategy.return = sign(foo[window:(N-1)]) * cd[(window+1):N]
summary(lm(matrix(foo[window:(N-1)]) ~ matrix(cd[(window+1):N])))

# risk = apply(matrix(seq(100)),1,FUN=function(idx) t(weight[idx,]) %*% V %*% weight[idx,])

# http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture17_TS4.pdf
# http://www.stat.pitt.edu/stoffer/tsa2/Rissues.htm
# To match pred
ar = arima(cd[1:10],order=c(2,0,0))
cf = coef(ar)
a = cf[3]*(1-cf[1]-cf[2]) + cf[1]*cd[10] + cf[2]*cd[9]
b = predict(ar)$pred
a - b

#### 
vix = get_yhoo_data('^VIX')$Adj
spy = get_yhoo_data('SSO')$Adj



sds = get_prior_day_return(get_yhoo_data('SDS')$Adj)
sso = get_prior_day_return(get_yhoo_data('SSO')$Adj)
letf = cumprod(1-sso-sds)-1

d = merge(spy, vix, letf,all=FALSE)
colnames(d) = c('spy','vix','letf')

plot.zoo(d)

