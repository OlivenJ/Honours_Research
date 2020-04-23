library(mvtnorm)
library(tidyverse)
library(broom)

set.seed(19970628)

t <- 120
#the time periods, i.e. the observations amount: we mimic the real stock's monthely
#return, 12 months a year and 10 years, so we will generate 120 different returns 
n <- 500
#How many independent cross-section observation we have. Since in the real data scenario
#we will use S&P 500, so we set the sample size to 500 to mimic it.
k <- 30
# the number of factors
constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
#Assume the constant term ( the alpha term from CAPM model) follows uniform distribution

error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
#Assume the error term follows a standard normal distribution

alpha <- c( rep(0.55, k),1)
#the factor strength, with the last term as market factor strength, which equals to 1
#represents that market factor influence all the strocks, the rest 30 factors have 
#strength 0.55, which is pretty low.


Sigma <- diag(1, k+1)
factor <- matrix(rep(rmvnorm(1,rep(0, nrow(Sigma)), Sigma), t),ncol = t)
market_factor <- factor[length(factor)]
#random generate factor list, the last one is the market facotr which by definition
#equals to the difference between makret return and risk free return

sig_count <- as.integer((n)^alpha)
#Indicates under the factor strength setting, how many factor laodings are significnatly
#different from 0. We only take the integer part. From the result we can see that 
#when the factor strengh is 0.55, 30 amongh 500 stocks will be significant from 0,
# and when the factor strengh is 1, all 500 loading will be significant

mu_var = 0.71
mu <- matrix(runif(n*k+1,mu_var-0.2, mu_var + 0.2), ncol = k+1, nrow = n)


loading_assign <- function(origin, count){
  pos = 1
  for(i in count){
    if(i < n){
      while(i < n){
        origin[i+1,pos] = 0 
        i = i+1}
    }
    else{
      next
    }
    pos = pos+1}
  return(origin)
  }

loading <- loading_assign(mu, sig_count)

return = constant + loading %*% factor + error

estimation <- for(i in 1:nrow(factor)){
  for(j in 1:nrow(return))
  lm(return[j,]~factor[i,])
}



