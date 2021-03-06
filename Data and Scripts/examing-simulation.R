library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)
set.seed(19890604)

t <- 120
#the time periods, i.e. the observations amount: we mimic the real stock's monthely
#return, 12 months a year and 10 years, so we will generate 120 different returns 
n <- 500
#How many independent cross-section observation we have. Since in the real data scenario
#we will use S&P 500, so we set the sample size to 500 to mimic it.
k <- 2
# the number of factors
alpha = 1

constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
#Assume the constant term ( the alpha term from CAPM model) follows uniform distribution
error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
#Assume the error term follows a standard normal distribution
strength <- c( rep(alpha, k),1)
#the factor strength, with the last term as market factor strength, which equals to 1
#represents that market factor influence all the strocks, the rest 30 factors have 
#strength 0.55, which is pretty low.
sig_count <- as.integer((n)^strength)

Sigma <- diag(1,k+1)
factor <- t(rmvnorm(t,mean = rep(0, k+1), Sigma))

#Generate the factor vector with k factor and one market factor.
#Each facotr foolows a multinomial distribution with zeor mean and Sigma vairance


market_factor <- t(matrix(factor[nrow(factor),]))
colnames(market_factor) <- paste0("m", 1:t)
market_factor <- as_tibble(market_factor)%>% 
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "market_factor_val")
#Take out the market factor, and pack it into a tibble format.


#Indicates under the factor strength setting, how many factor laodings are significnatly
#different from 0. We only take the integer part. From the result we can see that 

adj_stat <- pnorm(1-(0.05/(2*n^1.96)))

mu_var = 0.71
loading_generate<- function(nonzero_amount){
  loading <- matrix(0, n, k+1)
  for(i in 1:(k+1)){
  col <- runif(n,mu_var-0.2,  mu_var + 0.2)
  ind <- which(col %in% sample(col,n-nonzero_amount[i]))
  col[ind] <- 0
  loading[,i] <- col
  }
  return(loading)
}
loading <- loading_generate(sig_count)
#==============+==============+==============+==============+==============+==============+==============+#
##Wrap the generate factor and return into tibble
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
regression <- function(data, equation){
  data %>% 
    mutate(coefficient = map(data, equation)) %>% 
    unnest(coefficient)
}

strength_calc <- function(regress_result){
  regress_result$sig_indi <-  if_else(regress_result$statistic > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    select(-data) %>% 
    filter(term == "factor_val") %>% 
    group_by(factor) %>% 
    summarize(pi_hat =sum(sig_indi)/n) 
  
regress_result <- regress_result %>% 
  mutate(alpha_hat = 1 +log(pi_hat)/log(n)) %>% 
  mutate(alpha_hat = replace(alpha_hat, alpha_hat == -Inf, 0))
  return(regress_result)
}
#==============+==============+==============+==============+==============+==============+==============+#
##The setting of only strong factor.
factor_group <- combine_table %>% group_by(factor,stock) %>% nest()

single_market_regress <-function(data){
  tidy(lm(return ~ factor_val, data))
}
factor_group %>% regression(single_market_regress)

single_market_factor <- factor_group %>% 
  filter(factor == k+1) %>% 
  regression(single_market_regress)

single_market_result <- single_market_factor %>%
  select(-data) %>% filter(term != "(Intercept)") %>% 
  group_by(factor) %>% filter(statistic > adj_stat) %>% 
  summarize(alpha_hat =(log(n())/log(n)), diff = alpha_hat - 1) 

##The result is consistent with the inital setting, all the factors loading among 500 assets are significant,
##The corresponding strength is 1, indicates the market factor influence across all assets.
#==============+==============+==============+==============+==============+==============+==============+#
#The setting of only one factor, including both strong and weak
one_factor_group <- combine_table %>% 
  group_by(stock,factor) %>% nest()

one_factor_regress <- function(data){
  tidy(lm(return ~ factor_val, data))
}

one_factor <- one_factor_group %>% 
  regression(one_factor_regress)

one_factor_result <- one_factor %>% 
  strength_calc()

one_factor_result %>% 
  mutate(diff = alpha_hat - alpha) %>% 
  summarize(mean(diff^2))
#==============+==============+==============+==============+==============+==============+==============+#
##The setting of market factor and one other factor.

#add market factor as a separate variable of the data frame.
two_factor_group <- combine_table %>% 
  filter(factor != k+1) %>% 
  right_join(market_factor, by = 'month') %>% 
  select(stock, factor,return, market_factor_val, factor_val) %>% 
  group_by(stock,factor) %>% nest()


#Set the regression function
two_factor_regress <-function(data){
  tidy(lm(return ~ factor_val + market_factor_val, data))
}
#Run the regression and unnest the results
two_factor <- two_factor_group %>% 
  regression(two_factor_regress)

##Caculate the factor strength
two_factor_result <- two_factor %>% 
  strength_calc()

two_factor_result %>% 
  mutate(diff = alpha_hat - alpha) %>% 
  summarize(mean(diff^2))
