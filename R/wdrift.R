# Code from: https://www.rmetrics.org/files/Meielisalp2007/Presentations/Pfaff.pdf
# http://wps.ablongman.com/wps/media/objects/2829/2897573/ch18.pdf
# http://www.statmethods.net/advgraphs/probability.html
# 

# Plot data 
plot_autocorrel <- function(dat){
  # http://stackoverflow.com/questions/7309411/how-to-calculate-autocorrelation-in-r-zoo-object
  # Include histogram and fit
  mu = mean(dat)
  std = as.numeric( sqrt(var(dat)) )
  layout(matrix(1:4, ncol = 2, nrow = 2))
  #cumulative.ret = cumsum(dat)
  cumulative.ret = cumprod(1+dat)-1
  plot.zoo(cumulative.ret, xlab = "Date", ylab = "CumSum", main='Data')
  plot.zoo((dat-mu)/std, xlab = "obs", ylab = "std", main=paste('mean/std =',round(mu,3),'/',round(std,5)))
  abline(h = mu, col = "blue")
  abline(h = mu+std, col = "red")
  abline(h = mu-std, col = "red")
  acf(coredata(dat), main = "ACF")
  # qqnorm(data,)
  pacf(coredata(dat), main = "PACF")
  # return(object)
}

# Estimate Data
estimate_arima <- function(data, order, include.mean=FALSE){
  # TODO: o Implement
  arma20 <- arima(data, order=order, include.mean = include.mean)
  result <- matrix(cbind(arma20$coef, sqrt(diag(arma20$var.coef))), nrow = 2)
  #rownames(result) <- c("ar1", "ar2")
  #colnames(result) <- c("estimate", "s.e.")
  return(result)
}
check_spurious_regression <- function(y1,y2){
  #'''
  ## NOTE: DW Test Stat Just is: e = spurious.reg$residuals; T = length(e)
  # sum((e[2:T]-e[1:(T-1)])^2) / sum(e^2) # no .^ ?
  # Spurious Regression Rule-of-thumb: Be cautious when R2 is greater than DW statistic.  
  # for the null hypothesis that the errors on a regression model follow a process 
  # with a unit root against the alternative hypothesis that the errors follow a 
  # stationary first order autoregression (Sargan and Bhargava, 1983)
  #'''
  library(lmtest)
  
  spurious.reg <- lm(y1 ~ y2)
  sumry = summary(spurious.reg)
  dw <- dwtest(spurious.reg) 
  check = sumry$r.squared > as.numeric(dw[[1]])
  if (check){
    warning('R-Squared is greater than DW Statistic. Could be Spurious Regression') 
  }
  output = list(sumry, dw, check)
  names(output) = c('summary','dw','check')
  return(output)
}

# Generate Data
# TODO: o Detect & Include seasonality and trend
get_reverting_spread <- function( ar.order=c(0.3,0), n=1000, intercept = 0, time.drift = 0){
  # Get AR errors
  u.ar <- arima.sim(list(ar = ar.order), n=n) # Mean 0 Unit Var?
  TD <- intercept + time.drift * seq(n) # time drift
  y <- cumsum(u.ar)         # Will this go through 0 infinitely often? Would random noise?
  y.td <- y + TD
  output = list(u.ar = u.ar, y=y, TD=TD) # y.td = y.td)
  return(output)
}

# First mean(diff(y.td)) estimates TD
# 



# Test function
set.seed(12345)
gwn <- rnorm(100) # guassian white noise
plot_autocorrel(gwn)


# Create Mean Reverting Spread
# Learn how to read these better
u.ar <- arima.sim(n = 500, list(ar = c(0.3, -0.3)))
plot_autocorrel(u.ar)
r = estimate_arima(u.ar)


# y.tsar2 <- 5 + 0.5 * seq(250) + u.ar; # arima.sim(list(ar = c(0.8, -0.2)), n = 250)


### Difference Stationary I(1) with Drift
set.seed(12345)
layout(matrix(1:2, nrow = 2, ncol = 1))
plot.ts(y1, main = "I(1) process without drift", ylab="", xlab = "")
plot.ts(y1.d, main = "I(1) process with drift", ylab="", xlab = "")
abline(a=5, b=0.7, col = "red")

plot_autocorrel(y1)
plot_autocorrel(diff(y1))


# Spurious correlation Monte Carlo
# 81% Spurious Y
# No Spurious e and expected number of spurious betas
N = 100
k = 500
spur_E = 0
spur_SigP = 0; p = 0.05
spur_Y = 0 
for(x in seq(N)){
  e1 <- rnorm(k)
  e2 <- rnorm(k)
  y1 <- cumsum(e1)
  y2 <- cumsum(e2)

  e.reg = check_spurious_regression(e1,e2)  
  y.reg = check_spurious_regression(y1,y2)
  
  spur_E = spur_E + e.reg$check
  spur_Y = spur_Y + y.reg$check
  spur_SigP = spur_SigP + (e.reg$summary$coefficients[2,4] < p)
}



