estimate.OU <- function(data){
  fit = lm(data[,1] ~ data[,2])
  tstat = coef(summary(fit))[1,3]
  e = lm(data[,1] ~ data[,2])$residuals
  X_t = cumsum(e)
  f = lm(X_t[2:length(X_t)] ~ X_t[1:length(X_t)-1])
  
  
  return( c(coef(f)[1], coef(f)[2], var(f$residuals), tstat))
}
