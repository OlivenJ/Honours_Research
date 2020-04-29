library(tidyverse)
library(broom)
library(mvtnorm)
library(modeerndive)

set.seed(19890604)

t <- 12
#the time periods, i.e. the observations amount: we mimic the real stock's monthely
#return, 12 months a year and 10 years, so we will generate 120 different returns 
n <- 5
#How many independent cross-section observation we have. Since in the real data scenario
#we will use S&P 500, so we set the sample size to 500 to mimic it.
k <- 5
# the number of factors
constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
#Assume the constant term ( the alpha term from CAPM model) follows uniform distribution

error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
#Assume the error term follows a standard normal distribution

alpha <- c( rep(0.55, k),1)
#the factor strength, with the last term as market factor strength, which equals to 1
#represents that market factor influence all the strocks, the rest 30 factors have 
#strength 0.55, which is pretty low.
critical_value <- qnorm(1-0.025/(2*n^1.96))
  


Sigma <- diag(1,k+1)


factor <- t(rmvnorm(t,mean = rep(0, k+1), Sigma))

market_factor <- matrix(factor[nrow(factor),])
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
#Function help assign significant loading, and change the non-significant to 0
loading <- loading_assign(mu, sig_count)

return <-  constant + loading%*%factor + error
colnames(return) <- paste0("m", 1:t)
return <- as_tibble(return) %>% mutate(stock = 1:n) %>% select(stock, everything())

return <- return %>%
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "return")

colnames(factor) <- paste0("m", 1:t)
factor <- as_tibble(factor) %>% mutate(factor = 1:(k+1)) %>% select(factor,everything())
factor <- factor %>% 
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "factor_val")

combine_table <- right_join(return, factor)

factor_group <- combine_table %>% group_by(factor,stock) %>% nest()

factor_regress <-function(data){
  lm(return ~ factor_val, data)
}

factor_group <- factor_group %>% 
  mutate(model = map(data, factor_regress))
factor_group %>% filter(factor == 1) 


temp <- map(factor_group$data, factor_regress)
temp[2]


(factor_group %>% filter(stock == 1 & factor == 1) %>% select(model))

result <- map(factor_group$data, factor_regress)

