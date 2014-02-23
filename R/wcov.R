wcov <- function(R,h){
  #function [C,sda,CC,e] = wcov(R,h);
  #% [C,sda,CC,e]  = wcov(R,h);
  #%    produces a weighted covariance matrix using a specified half-life
  #%    also produces standard deviations, correlations and expected values
  #%    variables:
  #  %          R: matrix of s observations (states) on n variables
  #%          h: half-life (0 for equal weights)
  #%          C: n*n covariance matrix 
  #%        sda: n*1 vector of standard deviations
  #%         CC: n*n matrix of correlation coefficients
  #%          e: n*1 vector of expected returns  
  #%          
  #%  weight for observation t is 2^(t/h), scaled to sum to 1.0  
  
  #%  copyright 1995, William F. Sharpe
  #%  wfsharpe@leland.stanford.edu
  #%  this version Nov. 2, 1995
  
  # % if matrix has more rows than columns, transpose it
  #if size(R,1) > size(R,2)
  #R = R';
  #end;
  sz = dim(as.matrix(R))
  if (sz[1] > sz[2]){
    R = t(R)
  }
  
  #% get dimensions
  #[n,s] = size(R);
  sz = dim(as.matrix(R)) # PETE: Get again in case it changed
  n = sz[1]
  s = sz[2]
  
  
  #% set up weight vector
  #if h == 0
  # x = zeros(1,s);
  #else
  #  x = (1:s)/h;
  #end;
  if (h==0){
    x = matrix(0,nrow=1,ncol=s)
  }else{
    x = matrix(seq(s),nrow=1,ncol=s)/h
  }
  
  #w = (2.^x);
  #p = w/sum(w);
  w = 2^x
  p = w/sum(w)
  
  #% compute expected values
  #e = R*p';
  # PETE: Skip this to get non-demeaned values
  e = R %*% t(p)
  
  #    % compute matrix of deviations
  #        d = R - e*ones(1,s);
  d = R - matrix(e,nrow=n,ncol=s)
  
  #    % compute weighted covariances
  #        C = d*diag(p)*d';
  diag.p = matrix(0,nrow=s,ncol=s)
  for (idx in seq(s)){
    diag.p[idx,idx] = p[idx]
  }  
  C = d %*% diag.p %*% t(d)
  
  #% compute standard deviations
  #sda = sqrt(diag(C));
  sda = sqrt(diag(C))
    
  #% compute correlations (if sda = 0, corr = 0)
  # z = sda*sda';
  #       z = z + (z==0);
  #       CC = C./z;
  z = sda %*% t(sda)
  z = z + (z==0)
  CC = C/z
  
  # [C,sda,CC,e] 
  output = list(C=C,sda=sda,CC=CC,e=e)
  return(output)
}
        