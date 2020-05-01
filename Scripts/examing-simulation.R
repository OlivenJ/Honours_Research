library(tidyverse)
library(broom)
library(mvtnorm)
library(modeerndive)

set.seed(19890604)

t <- 120
#the time periods, i.e. the observations amount: we mimic the real stock's monthely
#return, 12 months a year and 10 years, so we will generate 120 different returns 
n <- 500
#How many independent cross-section observation we have. Since in the real data scenario
#we will use S&P 500, so we set the sample size to 500 to mimic it.
k <- 50
# the number of factors
constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
#Assume the constant term ( the alpha term from CAPM model) follows uniform distribution

error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
#Assume the error term follows a standard normal distribution
alpha = 0.55
strength <- c( rep(alpha, k),1)
#the factor strength, with the last term as market factor strength, which equals to 1
#represents that market factor influence all the strocks, the rest 30 factors have 
#strength 0.55, which is pretty low.
critical_value <- qnorm(1-0.025/(2*n^1.96))

Sigma <- diag(1,k+1)

factor <- t(rmvnorm(t,mean = rep(0, k+1), Sigma))



market_factor <- t(matrix(factor[nrow(factor),]))
colnames(market_factor) <- paste0("m", 1:t)
market_factor <- as_tibble(market_factor)%>% 
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "market_factor_val")
#random generate factor list, the last one is the market facotr which by definition
#equals to the difference between makret return and risk free return

sig_count <- as.integer((n)^strength)
#Indicates under the factor strength setting, how many factor laodings are significnatly
#different from 0. We only take the integer part. From the result we can see that 
#when the factor strengh is 0.55, 30 amongh 500 stocks will be significant from 0,
# and when the factor strengh is 1, all 500 loading will be significant
adj_stat <- pnorm(1-(0.025/(2*n^1.68)))


mu_var = 0.71

loading_generate<- function(zero_amount){
  loading <- matrix(0, n, k+1)
  for(i in 1:(k+1)){
  col <- runif(n,mu_var-0.2, mu_var + 0.2)
  ind <- which(col %in% sample(col,n-zero_amount[i]))
  col[ind] <- 0
  loading[,i] <- col
  }
  return(loading)
}
loading <- loading_generate(sig_count)
##Package the generate factor and return into tibble
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
#==============+==============+==============+==============+==============+==============+==============+#
##The setting of only strong factor.
factor_group <- combine_table %>% group_by(factor,stock) %>% nest()

single_market_regress <-function(data){
  tidy(lm(return ~ factor_val, data))
}

single_market_factor <- factor_group %>% 
  filter(factor == k+1) %>% 
  mutate(coefficient = map(data, single_market_regress)) %>% 
  unnest(coefficient)

single_market_result <- single_market_factor %>%
  select(-data) %>% filter(term != "(Intercept)") %>% 
  group_by(factor) %>% filter(statistic > adj_stat) %>% 
  summarize(alpha_hat =(log(n())/log(n))) 

##The result is consistent with the inital setting, all the factors loading among 500 assets are significant,
##The corresponding strength is 1, indicates the market factor influence across all assets.
#==============+==============+==============+==============+==============+==============+==============+#
##The setting of strong factor and one other factor.
two_factors <- combine_table %>% 
  filter(factor != k+1) %>% 
  right_join(market_factor, by = 'month') %>% 
  select(stock, factor,return, market_factor_val, factor_val)

two_factor_group<- two_factors %>% 
  group_by(stock,factor) %>% nest()

two_factor_regress <-function(data){
  tidy(lm(return ~ factor_val + market_factor_val, data))
}

two_factor <- two_factor_group %>% 
  mutate(coefficient = map(data, two_factor_regress)) %>% 
  unnest(coefficient)

  two_factor_result <- two_factor %>% 
  select(-data) %>% filter(term == "factor_val" ) %>% 
    group_by(factor) %>% filter(statistic > adj_stat) %>% 
    summarize(alpha_hat =(log(n())/log(n))) 

  two_factor_result %>% arrange(alpha_hat)
  summary(two_factor_result)
  
  two_factor %>% select(-data) %>% 
    filter(term == "market_factor_val") %>% 
    group_by(factor) %>% 
    filter(statistic > adj_stat) %>% 
    summarize(alpha_hat =(log(n())/log(n))) %>% 
    arrange(alpha_hat)